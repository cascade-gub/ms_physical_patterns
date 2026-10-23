#!/usr/bin/env python
"""
modis_gpp_wateryear_gee.py
==========================

Water-year (October-September) MODIS GPP from the 8-day MOD17A2HGF (v061)
product, for (a) the MacroSheds watersheds and (b) the CONUS reference grid,
via the Google Earth Engine Python API. Answers reviewer comment #16: the
annual MOD17A3HGF value MacroSheds distributes is a calendar-year total, which
overlaps the water year used for streamflow by only nine months.

Per water year y (1 Oct y-1 to 30 Sep y) the 8-day composites whose start date
falls in that span are summed per pixel (the gap-filled product has no
systematic gaps; a per-pixel count of composites is kept to check this).
Calendar-year sums are produced alongside as a check against MOD17A3HGF.

  collection = MODIS/061/MOD17A2HGF, band 'Gpp' (int16, scale 0.0001 kg C m-2 per
               8 days, valid 0-30000, fill 32761-32767 for urban / barren / water)
  masking    = raw Gpp > 30000 masked BEFORE scaling
  watersheds = median and count of valid 500 m pixels inside each MacroSheds
               boundary (same statistic as MacroSheds' gpp_global_500m_median)
  grid       = mean and count of valid pixels in the 1 km2 square footprint
               (same statistic as src/grid_modis_gpp_gee.py)

Outputs
  data_raw/ms_add_ons/modis_gpp_wateryear.csv
      site_code, water_year, gpp_wy_median, n_valid_wy, n_composites_wy,
      gpp_cy_median, n_valid_cy
  data_raw/grid_csvs/gpp_wy.csv   (same schema as gpp.csv; `year` = water year)
      FID, year, GPP, n_valid, geometry

Usage
  python src/modis_gpp_wateryear_gee.py --project <gcloud-project-id> [--sites] [--grid]
  (or set env var EE_PROJECT; both targets run when neither flag is given)
Requires prior `earthengine authenticate` and
  Rscript src/export_ws_boundaries.R   (writes data_working/ms_ws_boundaries.geojson)
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
from grid_modis_gpp_gee import (  # noqa: E402
    ROOT, YEARS, PIXEL_SCALE, OUT_COLS,
    log, load_points, ee_init, make_fc, with_retry, read_partial, append_partial, assemble,
)

COLLECTION = "MODIS/061/MOD17A2HGF"
BAND = "Gpp"
SCALE_FACTOR = 0.0001
VALID_MAX = 30000
BOUNDARIES = ROOT / "data_working" / "ms_ws_boundaries.geojson"
SITES_OUT = ROOT / "data_raw" / "ms_add_ons" / "modis_gpp_wateryear.csv"
GRID_OUT = ROOT / "data_raw" / "grid_csvs" / "gpp_wy.csv"


# ---------------------------------------------------------------------------
# Earth Engine images
# ---------------------------------------------------------------------------

def scaled_collection(ee):
    col = ee.ImageCollection(COLLECTION).filterDate("2000-01-01", "2024-01-01").select(BAND)

    def prep(img):
        valid = img.lte(VALID_MAX)
        return (img.updateMask(valid).multiply(SCALE_FACTOR)
                .addBands(valid.rename("n"))
                .copyProperties(img, ["system:time_start"]))

    return col.map(prep)


def period_sum(ee, col, start: str, end: str, name: str):
    """Sum of scaled GPP over composites starting in [start, end), plus the
    per-pixel count of composites contributing (band <name>_n)."""
    sub = col.filterDate(start, end)
    gpp = sub.select(BAND).sum().rename(name)
    n = sub.select("n").sum().rename(f"{name}_n")
    return gpp.addBands(n)


def build_stack(ee, calendar: bool):
    col = scaled_collection(ee)
    imgs = []
    for y in YEARS:
        imgs.append(period_sum(ee, col, f"{y - 1}-10-01", f"{y}-10-01", f"wy{y}"))
        if calendar:
            imgs.append(period_sum(ee, col, f"{y}-01-01", f"{y + 1}-01-01", f"cy{y}"))
    return ee.Image.cat(imgs)


# ---------------------------------------------------------------------------
# Watersheds
# ---------------------------------------------------------------------------

def load_boundaries(ee):
    if not BOUNDARIES.exists():
        sys.exit(f"{BOUNDARIES} not found; run Rscript src/export_ws_boundaries.R first")
    gj = json.loads(BOUNDARIES.read_text(encoding="utf-8"))
    feats = []
    for f in gj["features"]:
        geom = ee.Geometry(f["geometry"], "EPSG:4326", False)
        feats.append(ee.Feature(geom, {"site_code": f["properties"]["site_code"]}))
    log(f"{len(feats)} watershed boundaries loaded")
    return ee.FeatureCollection(feats), [f["properties"]["site_code"] for f in gj["features"]]


def run_sites(ee, out: Path = SITES_OUT) -> pd.DataFrame:
    fc, codes = load_boundaries(ee)
    stack = build_stack(ee, calendar=True)
    red = ee.Reducer.median().combine(ee.Reducer.count(), sharedInputs=True)
    # count only the GPP bands for n_valid; composites-per-pixel is summarised by its median
    gpp_bands = [f"wy{y}" for y in YEARS] + [f"cy{y}" for y in YEARS]
    n_bands = [f"wy{y}_n" for y in YEARS]

    def call():
        res = stack.select(gpp_bands).reduceRegions(collection=fc, reducer=red, scale=PIXEL_SCALE, tileScale=8)
        res = res.select(["site_code", ".*_median", ".*_count"], None, False)
        return res.getInfo()

    log("reducing watersheds (median, count) for 23 water years + 23 calendar years ...")
    info = with_retry(call, what="watersheds", tries=4, base_sleep=20)

    def call_n():
        res = stack.select(n_bands).reduceRegions(collection=fc, reducer=ee.Reducer.median(), scale=PIXEL_SCALE, tileScale=8)
        return res.select(["site_code", ".*_n.*"], None, False).getInfo()

    log("reducing composite counts ...")
    info_n = with_retry(call_n, what="composite counts", tries=4, base_sleep=20)
    ncomp = {f["properties"]["site_code"]: f["properties"] for f in info_n["features"]}

    rows = []
    for f in info["features"]:
        p = f["properties"]
        sc = p["site_code"]
        for y in YEARS:
            c_wy = int(p.get(f"wy{y}_count") or 0)
            c_cy = int(p.get(f"cy{y}_count") or 0)
            rows.append({
                "site_code": sc, "water_year": y,
                "gpp_wy_median": p.get(f"wy{y}_median") if c_wy > 0 else None,
                "n_valid_wy": c_wy,
                "n_composites_wy": ncomp.get(sc, {}).get(f"wy{y}_n"),
                "gpp_cy_median": p.get(f"cy{y}_median") if c_cy > 0 else None,
                "n_valid_cy": c_cy,
            })
    df = pd.DataFrame(rows).sort_values(["site_code", "water_year"]).reset_index(drop=True)
    out.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out, index=False, lineterminator="\n")
    log(f"wrote {out} ({len(df):,} rows, {df['site_code'].nunique()} sites)")
    missing = sorted(set(codes) - set(df["site_code"]))
    if missing:
        log(f"  no result for {len(missing)} sites: {', '.join(missing)}")
    return df


# ---------------------------------------------------------------------------
# Grid
# ---------------------------------------------------------------------------

def run_grid(ee, out: Path = GRID_OUT, chunk_size: int = 500, limit: int | None = None) -> pd.DataFrame:
    pts = load_points()
    if limit:
        pts = pts.head(limit)
    stack = build_stack(ee, calendar=False).select([f"wy{y}" for y in YEARS])
    stack = stack.rename([f"y{y}" for y in YEARS])        # reuse the grid script's row parser names
    red = ee.Reducer.mean().combine(ee.Reducer.count(), sharedInputs=True)
    partial = out.with_name(out.stem + ".partial.csv")
    done = read_partial(partial)
    done_fids = set(done["FID"].tolist())
    todo = pts[~pts["FID"].isin(done_fids)]
    log(f"grid: {len(pts)} points, {len(done_fids)} already done, {len(todo)} to do")
    for start in range(0, len(todo), chunk_size):
        chunk = todo.iloc[start:start + chunk_size]
        fc = make_fc(ee, chunk)

        def call(fc=fc):
            res = stack.reduceRegions(collection=fc, reducer=red, scale=PIXEL_SCALE, tileScale=4)
            return res.select(["FID", ".*_mean", ".*_count"], None, False).getInfo()

        info = with_retry(call, what=f"grid chunk {start // chunk_size + 1}", tries=4, base_sleep=20)
        rows = []
        for f in info["features"]:
            p = f.get("properties", {})
            fid = int(p["FID"])
            for y in YEARS:
                cnt = int(p.get(f"y{y}_count") or 0)
                mean = p.get(f"y{y}_mean") if cnt > 0 else None
                rows.append({"FID": fid, "year": y, "GPP": mean, "n_valid": cnt})
        append_partial(partial, rows)
        log(f"  chunk {start // chunk_size + 1}: {len(chunk)} points done")
    full = assemble(read_partial(partial), pts)
    full.to_csv(out, index=False, lineterminator="\n")
    log(f"wrote {out} ({len(full):,} rows)")
    return full


# ---------------------------------------------------------------------------

def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--project", default=os.environ.get("EE_PROJECT"))
    ap.add_argument("--sites", action="store_true", help="only the MacroSheds watersheds")
    ap.add_argument("--grid", action="store_true", help="only the reference grid")
    ap.add_argument("--limit", type=int, default=None, help="grid smoke test: first N points")
    args = ap.parse_args(argv)
    ee = ee_init(args.project)
    do_sites = args.sites or not args.grid
    do_grid = args.grid or not args.sites
    if do_sites:
        df = run_sites(ee)
        ok = df.dropna(subset=["gpp_wy_median"])
        log(f"sites: {ok['site_code'].nunique()} with data; median composites per water year "
            f"{ok['n_composites_wy'].median():.1f}; wy/cy median ratio "
            f"{(ok['gpp_wy_median'] / ok['gpp_cy_median']).median():.3f}")
    if do_grid:
        run_grid(ee, limit=args.limit)


if __name__ == "__main__":
    main()
