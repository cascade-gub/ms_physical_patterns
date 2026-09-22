#!/usr/bin/env python
"""
grid_modis_gpp_gee.py
=====================

Extract MODIS MOD17A3HGF (v061) annual GPP, 2001-2023, for the CONUS reference
grid used in "Stability of Headwater Streamflow Under Four Decades of Climate
Change", via the Google Earth Engine Python API.

Replaces the Landsat-derived data_raw/grid_csvs/gpp.csv (archived as
gpp_landsat.csv) with a MODIS product of the same schema so that
data_vis/data_groups.R reads it unchanged (it full_joins the grid CSVs on
FID + year and selects only FID, year, GPP, geometry).

Per grid point:
  footprint  = ee.Geometry.Point([lon, lat]).buffer(500).bounds()  (~1 km^2, 100 ha)
  collection = MODIS/061/MOD17A3HGF, band 'Gpp' (uint16, scale 0.0001,
               valid 0-65500, fill 65535 for urban / barren / water land cover)
  masking    = raw Gpp > 65500 masked BEFORE scaling; then * 0.0001 -> kgC m-2 yr-1
  reduction  = mean and count of valid 500 m pixels over the square
               (ee.Reducer.mean().combine(ee.Reducer.count(), sharedInputs=True))
               at scale = 500 m in the image's native (sinusoidal) projection.

Strategy: all 23 annual images are stacked into one image (ee.Image.cat), and
reduceRegions() is called once per chunk of <= --chunk points with getInfo()
(no Drive exports). If a chunk's single call fails after retries, the script
falls back to one call per year for that chunk. Every finished chunk is
appended to a checkpoint CSV next to the output, and a restart skips FIDs
already present there.

Output columns (exactly): FID, year, GPP, n_valid, geometry
  GPP      mean valid-pixel GPP, kgC m-2 yr-1; empty where n_valid == 0
  n_valid  number of 500 m MODIS pixels intersecting the square with valid GPP
  geometry the ppt.csv geometry string for that FID ("c(lon, lat)")
Rows sorted by FID, year. 5,212 FIDs x 23 years = 119,876 rows.

Usage:
  python src/grid_modis_gpp_gee.py --project <gcloud-project-id>
  (or set env var EE_PROJECT). Requires prior `earthengine authenticate`.

Options:
  --out    output CSV (default data_raw/grid_csvs/gpp.csv, relative to repo root)
  --chunk  points per reduceRegions call (default 1000)
  --limit  only process the first N FIDs (smoke test; use with a scratch --out)
  --no-verify  skip the post-run verification summary
"""

from __future__ import annotations

import argparse
import os
import re
import sys
import time
from datetime import datetime
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
PPT_CSV = ROOT / "data_raw" / "grid_csvs" / "ppt.csv"
LANDSAT_CSV = ROOT / "data_raw" / "grid_csvs" / "gpp_landsat.csv"
DEFAULT_OUT = ROOT / "data_raw" / "grid_csvs" / "gpp.csv"

COLLECTION = "MODIS/061/MOD17A3HGF"
BAND = "Gpp"
SCALE_FACTOR = 0.0001
VALID_MAX = 65500          # raw values above this are fill (65535)
YEARS = list(range(2001, 2024))   # 2001..2023 inclusive
PIXEL_SCALE = 500          # metres
BUFFER_M = 500             # half-side of the square footprint, metres
OUT_COLS = ["FID", "year", "GPP", "n_valid", "geometry"]

GEOM_RE = re.compile(r"c\(\s*([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?)\s*,\s*([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?)\s*\)")


def log(msg: str) -> None:
    print(f"[{datetime.now():%H:%M:%S}] {msg}", flush=True)


# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

def parse_geometry(s: str) -> tuple[float, float]:
    """'c(-81.63, 24.65)' -> (lon, lat)."""
    m = GEOM_RE.fullmatch(str(s).strip())
    if not m:
        raise ValueError(f"unparseable geometry string: {s!r}")
    return float(m.group(1)), float(m.group(2))


def load_points(ppt_csv: Path = PPT_CSV) -> pd.DataFrame:
    """Distinct FID -> geometry from ppt.csv.

    ppt.csv carries two geometry strings per FID that differ only in the last
    printed digit (~1e-12 deg; the 1995-1996 rows). We keep the modal string per
    FID, which is the one used for every 2001-2023 row.
    """
    ppt = pd.read_csv(ppt_csv, usecols=["FID", "geometry"], dtype={"FID": "int64", "geometry": "string"})
    counts = ppt.groupby(["FID", "geometry"], sort=False).size().reset_index(name="n")
    counts = counts.sort_values(["FID", "n"], ascending=[True, False])
    pts = counts.drop_duplicates("FID", keep="first")[["FID", "geometry"]].reset_index(drop=True)
    lonlat = pts["geometry"].map(parse_geometry)
    pts["lon"] = [ll[0] for ll in lonlat]
    pts["lat"] = [ll[1] for ll in lonlat]
    if pts["FID"].duplicated().any():
        raise RuntimeError("duplicate FIDs after de-duplication -- should not happen")
    if not pts["lon"].between(-180, 180).all() or not pts["lat"].between(-90, 90).all():
        raise RuntimeError("lon/lat out of range in ppt.csv geometry strings")
    return pts


# ---------------------------------------------------------------------------
# Earth Engine
# ---------------------------------------------------------------------------

def ee_init(project: str | None):
    import ee  # imported here so --help works without the package

    try:
        if project:
            ee.Initialize(project=project)
        else:
            ee.Initialize()
    except Exception as exc:  # noqa: BLE001
        sys.stderr.write(
            "\nERROR: ee.Initialize failed.\n"
            f"  {type(exc).__name__}: {str(exc).strip()}\n\n"
            "To fix (run these yourself; the first one is interactive):\n"
            '  1. python -c "import ee; ee.Authenticate()"        # or: earthengine authenticate\n'
            "  2. Register / pick a Google Cloud project with Earth Engine enabled\n"
            "     (https://code.earthengine.google.com/register) and pass it:\n"
            "       python src/grid_modis_gpp_gee.py --project <project-id>\n"
            "     or set the environment variable EE_PROJECT.\n"
        )
        sys.exit(2)
    return ee


def build_stack(ee):
    """One image with bands y2001..y2023 = scaled, fill-masked annual GPP."""
    col = ee.ImageCollection(COLLECTION).filterDate("2001-01-01", "2023-12-31").select(BAND)

    def annual(year: int):
        img = ee.Image(col.filterDate(f"{year}-01-01", f"{year}-12-31").first())
        # Mask fill BEFORE scaling; 65535 is the documented fill value.
        return img.updateMask(img.lte(VALID_MAX)).multiply(SCALE_FACTOR).rename(f"y{year}")

    stack = ee.Image.cat([annual(y) for y in YEARS])
    names = stack.bandNames().getInfo()
    expected = [f"y{y}" for y in YEARS]
    if names != expected:
        raise RuntimeError(f"unexpected band names from {COLLECTION}: {names}")
    return stack


def reducer(ee):
    return ee.Reducer.mean().combine(ee.Reducer.count(), sharedInputs=True)


def make_fc(ee, chunk: pd.DataFrame):
    feats = [
        ee.Feature(
            ee.Geometry.Point([float(r.lon), float(r.lat)]).buffer(BUFFER_M).bounds(),
            {"FID": int(r.FID)},
        )
        for r in chunk.itertuples(index=False)
    ]
    return ee.FeatureCollection(feats)


TRANSIENT_MARKERS = (
    "429", "500", "502", "503", "504", "Too Many Requests", "timed out", "timeout",
    "Deadline", "deadline", "Connection", "connection", "RemoteDisconnected",
    "Computation timed out", "memory", "capacity", "Payload", "payload", "too large",
    "Internal error", "internal error", "unavailable", "Unavailable",
)


def with_retry(fn, what: str, tries: int = 4, base_sleep: float = 10.0):
    """Call fn(); retry on Earth Engine / HTTP errors with exponential backoff."""
    last = None
    for attempt in range(1, tries + 1):
        try:
            return fn()
        except Exception as exc:  # noqa: BLE001
            last = exc
            msg = f"{type(exc).__name__}: {str(exc)[:300]}"
            if attempt == tries:
                log(f"  {what}: attempt {attempt}/{tries} failed -- giving up ({msg})")
                break
            sleep = base_sleep * (2 ** (attempt - 1))
            log(f"  {what}: attempt {attempt}/{tries} failed ({msg}); retrying in {sleep:.0f}s")
            time.sleep(sleep)
    raise last  # type: ignore[misc]


def _features_to_rows(features: list[dict], years: list[int]) -> list[dict]:
    rows = []
    for f in features:
        p = f.get("properties", {})
        fid = int(p["FID"])
        for y in years:
            mean = p.get(f"y{y}_mean")
            cnt = p.get(f"y{y}_count")
            cnt = int(cnt) if cnt is not None else 0
            if cnt == 0 or mean is None:
                mean = None
            rows.append({"FID": fid, "year": y, "GPP": mean, "n_valid": cnt})
    return rows


def reduce_chunk_all_years(ee, stack, fc, tile_scale: int = 1) -> list[dict]:
    """One reduceRegions call for all 23 bands over the chunk."""
    result = (
        stack.reduceRegions(collection=fc, reducer=reducer(ee), scale=PIXEL_SCALE, tileScale=tile_scale)
        .select(["FID", ".*_mean", ".*_count"], None, False)   # drop geometry from payload
    )
    info = result.getInfo()
    return _features_to_rows(info["features"], YEARS)


def reduce_chunk_per_year(ee, stack, fc) -> list[dict]:
    """Fallback: one reduceRegions call per year for the chunk (smaller payloads)."""
    by_fid: dict[int, dict] = {}
    for y in YEARS:
        band = stack.select(f"y{y}")

        def call(band=band):
            return (
                band.reduceRegions(collection=fc, reducer=reducer(ee), scale=PIXEL_SCALE, tileScale=4)
                .select(["FID", ".*_mean", ".*_count"], None, False)
                .getInfo()
            )

        info = with_retry(call, what=f"year {y}", tries=5)
        for f in info["features"]:
            p = f.get("properties", {})
            by_fid.setdefault(int(p["FID"]), {})[y] = (p.get(f"y{y}_mean"), p.get(f"y{y}_count"))
    rows = []
    for fid, yd in by_fid.items():
        for y in YEARS:
            mean, cnt = yd.get(y, (None, 0))
            cnt = int(cnt) if cnt is not None else 0
            rows.append({"FID": fid, "year": y, "GPP": (mean if cnt > 0 else None), "n_valid": cnt})
    return rows


# ---------------------------------------------------------------------------
# Checkpointing / assembly
# ---------------------------------------------------------------------------

def read_partial(partial: Path) -> pd.DataFrame:
    if partial.exists() and partial.stat().st_size > 0:
        df = pd.read_csv(partial, dtype={"FID": "int64", "year": "int64", "n_valid": "int64"})
        return df[["FID", "year", "GPP", "n_valid"]]
    return pd.DataFrame(columns=["FID", "year", "GPP", "n_valid"])


def append_partial(partial: Path, rows: list[dict]) -> None:
    df = pd.DataFrame(rows, columns=["FID", "year", "GPP", "n_valid"])
    header = not (partial.exists() and partial.stat().st_size > 0)
    df.to_csv(partial, mode="a", header=header, index=False, lineterminator="\n")


def assemble(partial_df: pd.DataFrame, pts: pd.DataFrame) -> pd.DataFrame:
    """Full FID x year frame with GPP / n_valid merged in and geometry attached."""
    full = pd.MultiIndex.from_product([pts["FID"].tolist(), YEARS], names=["FID", "year"]).to_frame(index=False)
    dat = partial_df.drop_duplicates(["FID", "year"], keep="last")
    out = full.merge(dat, on=["FID", "year"], how="left")
    out["n_valid"] = out["n_valid"].fillna(0).astype("int64")
    out.loc[out["n_valid"] == 0, "GPP"] = pd.NA
    out["GPP"] = pd.to_numeric(out["GPP"], errors="coerce")
    out = out.merge(pts[["FID", "geometry"]], on="FID", how="left")
    out = out.sort_values(["FID", "year"]).reset_index(drop=True)
    return out[OUT_COLS]


# ---------------------------------------------------------------------------
# Verification
# ---------------------------------------------------------------------------

def verify(out: pd.DataFrame, pts: pd.DataFrame, landsat_csv: Path = LANDSAT_CSV) -> None:
    print("\n=== verification ===")
    n_fid = out["FID"].nunique()
    per_fid = out.groupby("FID")["year"].agg(["count", "nunique", "min", "max"])
    print(f"rows: {len(out):,}  (expected {len(pts) * len(YEARS):,})")
    print(f"FIDs: {n_fid:,}  (expected {len(pts):,});  years per FID: "
          f"min {per_fid['nunique'].min()}, max {per_fid['nunique'].max()};  "
          f"year range {out['year'].min()}-{out['year'].max()}")
    print(f"duplicate FID+year rows: {out.duplicated(['FID', 'year']).sum()}")
    gpp = out["GPP"]
    print(f"GPP: min {gpp.min():.4f}, median {gpp.median():.4f}, max {gpp.max():.4f}  "
          f"(within 0-6.5: {bool(gpp.dropna().between(0, 6.5).all())})")
    na_rows = int(gpp.isna().sum())
    allna = out.groupby("FID")["GPP"].apply(lambda s: s.isna().all())
    print(f"NA rows: {na_rows:,} ({na_rows / len(out):.2%});  all-NA FIDs: {int(allna.sum()):,};  "
          f"partially-NA FIDs: {int(((out.groupby('FID')['GPP'].apply(lambda s: s.isna().any())) & ~allna).sum()):,}")
    print(f"n_valid: min {out['n_valid'].min()}, median {out['n_valid'].median():.0f}, max {out['n_valid'].max()}")
    print(f"geometry matches ppt.csv modal string for every FID: "
          f"{bool((out['geometry'] == out['FID'].map(pts.set_index('FID')['geometry'])).all())}")

    if not landsat_csv.exists():
        print(f"(no {landsat_csv.name}; skipping Landsat spot-check)")
        return
    ls = pd.read_csv(landsat_csv, usecols=["FID", "year", "GPP"])
    ls = ls[ls["year"].between(2001, 2021)].groupby("FID")["GPP"].mean().rename("landsat")
    mo = out[out["year"].between(2001, 2021)].groupby("FID")["GPP"].mean().rename("modis")
    both = pd.concat([mo, ls], axis=1, join="inner").dropna()
    both = both[both["landsat"] > 0]
    both["ratio"] = both["modis"] / both["landsat"]
    print(f"\nLandsat spot-check, per-FID mean GPP 2001-2021 (n = {len(both):,} FIDs with both):")
    print(f"  MODIS/Landsat ratio: median {both['ratio'].median():.3f}, "
          f"IQR {both['ratio'].quantile(0.25):.3f}-{both['ratio'].quantile(0.75):.3f}, "
          f"within +/-50%: {both['ratio'].between(0.5, 1.5).mean():.1%}")
    sample = both.sample(n=min(3, len(both)), random_state=42).sort_index()
    for fid, r in sample.iterrows():
        print(f"  FID {fid:>5}: MODIS {r['modis']:.3f}  Landsat {r['landsat']:.3f}  ratio {r['ratio']:.3f}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--project", default=os.environ.get("EE_PROJECT"),
                    help="Google Cloud project id for ee.Initialize (default: env EE_PROJECT)")
    ap.add_argument("--out", type=Path, default=DEFAULT_OUT, help=f"output CSV (default {DEFAULT_OUT})")
    ap.add_argument("--chunk", type=int, default=1000, help="points per reduceRegions call (default 1000)")
    ap.add_argument("--limit", type=int, default=None, help="process only the first N FIDs (smoke test)")
    ap.add_argument("--no-verify", action="store_true", help="skip the verification summary")
    args = ap.parse_args(argv)

    t0 = time.time()
    out_path: Path = args.out if args.out.is_absolute() else (Path.cwd() / args.out)
    partial = out_path.parent / (out_path.stem + ".partial.csv")
    out_path.parent.mkdir(parents=True, exist_ok=True)

    pts = load_points()
    if args.limit:
        pts = pts.head(args.limit).reset_index(drop=True)
    log(f"{len(pts):,} grid points from {PPT_CSV.name}; output -> {out_path}")

    done = read_partial(partial)
    done_fids = set(done["FID"].unique()) if len(done) else set()
    todo = pts[~pts["FID"].isin(done_fids)].reset_index(drop=True)
    if done_fids:
        log(f"resuming: {len(done_fids):,} FIDs already in {partial.name}; {len(todo):,} to do")

    if len(todo):
        ee = ee_init(args.project)
        log(f"Earth Engine initialised (project={args.project or 'default'}); building {COLLECTION} stack")
        stack = build_stack(ee)

        n_chunks = (len(todo) + args.chunk - 1) // args.chunk
        for i in range(n_chunks):
            chunk = todo.iloc[i * args.chunk:(i + 1) * args.chunk]
            tc = time.time()
            log(f"chunk {i + 1}/{n_chunks}: FIDs {chunk['FID'].iloc[0]}-{chunk['FID'].iloc[-1]} (n={len(chunk)})")
            fc = make_fc(ee, chunk)
            try:
                rows = with_retry(lambda: reduce_chunk_all_years(ee, stack, fc), what="all-years call", tries=3)
            except Exception as exc:  # noqa: BLE001
                log(f"  all-years call failed ({type(exc).__name__}); falling back to per-year calls")
                rows = reduce_chunk_per_year(ee, stack, fc)

            got = {r["FID"] for r in rows}
            missing = set(chunk["FID"]) - got
            if missing:
                raise RuntimeError(f"chunk {i + 1}: {len(missing)} FIDs missing from EE result, e.g. {sorted(missing)[:5]}")
            if len(rows) != len(chunk) * len(YEARS):
                raise RuntimeError(f"chunk {i + 1}: expected {len(chunk) * len(YEARS)} rows, got {len(rows)}")
            append_partial(partial, rows)
            n_na = sum(r["GPP"] is None for r in rows)
            log(f"  done in {time.time() - tc:.0f}s; {len(rows):,} rows, {n_na:,} NA; checkpointed")

    partial_df = read_partial(partial)
    out = assemble(partial_df, pts)
    out.to_csv(out_path, index=False, lineterminator="\n")
    log(f"wrote {out_path} ({len(out):,} rows) in {(time.time() - t0) / 60:.1f} min total")

    if not args.no_verify:
        verify(out, pts)
    return 0


if __name__ == "__main__":
    sys.exit(main())
