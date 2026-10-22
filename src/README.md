# `src/` - analysis scripts

Scripts for two manuscripts share this folder:

- **Streamflow paper** - *Stability of Headwater Streamflow Under Four Decades of Climate Change* (MacroSheds). Scripts: `setup.R`, `metrics.R`, `stream_order_ms.R`, `mega_zipper_data.R`, `grid_modis_gpp_gee.py`, `p_pet_aridity.R`, `patchwork_figures.R`, plus the plotting scripts in `../data_vis/`.
- **Nitrogen manuscript** (Heili Lowman) - `nitrogen_trends.R`, `nitrogen_regression.R`, `nitrogen_figures.R`. These consume the discharge metrics and trend tables produced by the streamflow pipeline.

Conventions that apply to every script here:

- Every R script begins with `library(here); source(here('src', 'setup.R'))`. `setup.R` loads packages, sets paths and constants, and defines the shared helper functions.
- Run from the repo root. Most paths use `here()`, but several scripts (the nitrogen VWM block of `metrics.R`, all three `nitrogen_*.R`, `setup.R` line 46) use bare relative paths such as `data_working/...`, so the working directory must be the repository root.
- `data_raw/`, `data_working/`, `figures/` and `log/` are gitignored. Every collaborator regenerates them locally; nothing in them is shared through git. The MacroSheds v2 dataset must be downloaded into `data_raw/ms/` first (see the comment near the top of `setup.R`).
- R is not on PATH on the development machine; call `"C:/Program Files/R/R-4.6.1/bin/Rscript.exe"` directly.
- `data_working/trends/` must exist before `mega_zipper_data.R` runs; nothing creates it.

## Run order (streamflow paper)

| Step | Script | Reads | Writes |
|---|---|---|---|
| 1 | `src/metrics.R` | MacroSheds discharge, stream chemistry, `data_raw/ms/v2/spatial_timeseries_{climate,vegetation}.feather` | `data_working/discharge_metrics_siteyear.rds` and `_nTest.rds` (identical), `clim_summaries.rds`, `all_possible_good_siteyears.csv`, the `N_VWM_*.rds` / `N_CQ_*.rds` nitrogen files |
| 2 | `src/stream_order_ms.R` | MacroSheds site list, NHDPlus web service | `data_working/ms_stream_order.csv` |
| 3 | `src/mega_zipper_data.R` | `discharge_metrics_siteyear_nTest.rds` | `data_working/trends/full_prisim_climate.csv`, `data_working/trends/best_run_prisim.csv` |
| 4 | `src/grid_modis_gpp_gee.py` | `data_raw/grid_csvs/ppt.csv` (grid geometry), Earth Engine `MODIS/061/MOD17A3HGF` | `data_raw/grid_csvs/gpp.csv` |
| 5 | `data_vis/data_groups.R` | trend CSVs, `data_raw/grid_csvs/*.csv` | `data_working/site_groupings_by_prsim_trend.csv`, `data_working/grid_groups.csv`, `figures/map_*_trends.png` |
| 6 | `data_vis/grouping_histograms.R` | groupings, `_nTest.rds` | aridity histograms |
| 7 | `src/p_pet_aridity.R` | groupings, `_nTest.rds`, MacroSheds Daymet/climate products, `data_raw/ms_add_ons/macrosheds_et2.csv` | `figures/ei_obs_vs_modeled.png`, `pq_coupling.png`, `ei_range_by_site.png`, `budyko_plot.png` |
| 8 | `src/patchwork_figures.R` | everything above | `figures/Figure_1.png` ... `Figure_4.png` |

Nitrogen manuscript, after steps 1 and 3: `nitrogen_trends.R` then `nitrogen_figures.R`; `nitrogen_regression.R` needs only step 1.

## Shared settings in `setup.R`

**Trend window.** All streamflow-paper trends are fit on water years `analysis_start_year` to `analysis_end_year` (2001-2023, the MODIS GPP era), defined near line 67 of `setup.R`. `mega_zipper_data.R` filters on these before the best-run search and Sen's slope; the `data_vis/` scripts and `patchwork_figures.R` use them for the aridity climatology and axis labels. To fit trends on a different window, change these two values and re-run `mega_zipper_data.R`; do not commit the change unless the whole paper is moving. MODIS GPP only exists for 2001-2023, so GPP trends will not lengthen.

**Discharge completeness constants** (`frequency_check()`): a site-water year is kept if at least `good_weeks_to_year_master` (51) weeks have at least `minimum_per_week_sampling_frequency_master` (4) discharge observations. `start_year_master` and `minimum_continuous_record_length_master` are defined but not used by any live script.

**Satellite era markers**: `prisim_year` 1980, `landsat_year` 1984, `modis_year` 2000 (reference lines in Figure 2).

**Helper functions**

| Function | What it does |
|---|---|
| `set_logger()` | Points the `logger` package at `log/<script>_<datetime>.txt`. |
| `frequency_check(data_in)` | Returns the site-water years passing the completeness rule above. |
| `reduce_to_longest_site_runs(data_in, metric)` | Keeps each site's longest run of consecutive years with non-NA `metric`. Used for the climate trends. |
| `reduce_to_best_range(data_in, metric, max_missing = 0.4)` | Keeps each site's longest span with at most 40 % missing years of `metric`; sites with fewer than 10 rows get an NA row. Used for the discharge trends (`metric = 'q_mean'`). |
| `gen_reduce_to_best_range(data_in, solute, pair_data, aggregation, max_missing)` | Same idea on `analyte`/`timestep` columns; used by the nitrogen scripts. |
| `detect_trends(df_in)` | Sen's slope with confidence limits (`zyp.sen` + `confint.zyp`) per site x variable; needs more than 9 years, otherwise `code = 'under_10'`. Output columns: `start`, `end`, `n`, `trend`, `trend_upper`, `trend_lower`, intercept terms. |
| `add_flags(data_in)` | Flags `increasing` / `decreasing` / `non-significant` from the sign of the confidence limits. |

`setup.R` also loads `ms_site_data` (`ms_load_sites()`) and `ms_ws_attr` (`data_raw/ms/v2/watershed_summaries.feather`), which the other scripts use without reloading.

## Script reference

### Streamflow paper

**`metrics.R`** - Per site-water year discharge metrics: the Archfield et al. (2014) "magnificent seven", Richards-Baker flashiness, flow quantiles and their day-of-water-year exceedances, high-flow days; annual/seasonal/monthly climate, stream temperature and MODIS GPP summaries; and the nitrogen volume-weighted means and CQ slopes.

- `q_freq_check = T` (line 13) switches the completeness check on. Discharge is deduplicated by site and date, restricted to `ms_interp == 0`, converted to mm/d with watershed area, and assigned USGS water years (Oct-Sep).
- MODIS fill values are masked: `gpp_global_500m_median >= 6.55` becomes NA. Annual MODIS rows carry calendar year as `water_year`.
- The nitrogen block (NO3, NH3, TDN/TKN harmonisation, DIN, VWM, CQ) uses discharge from *before* the completeness check, so `q_freq_check` does not change the `N_VWM_*` / `N_CQ_*` outputs. It drops `NO3_NO2_N` at BIGC, BLDE, COMO, TECR and stevenson_creek.
- Writes the discharge metrics under two names, `discharge_metrics_siteyear.rds` (read by `nitrogen_figures.R`) and `discharge_metrics_siteyear_nTest.rds` (read by `mega_zipper_data.R`, `p_pet_aridity.R`, `patchwork_figures.R`, `data_vis/`). Same object.

**`stream_order_ms.R`** - NHDPlus stream order for each MacroSheds gauge in the lower 48 plus Hawaii via `nhdplusTools`; writes only sites with stream order < 2 to `data_working/ms_stream_order.csv`. Needs internet. No live script currently reads the output.

**`mega_zipper_data.R`** - Trend detection. (a) Climate run: `temp_mean`, `precip_mean`, `gpp_global_500m_median`, annual, within the window, `reduce_to_longest_site_runs()` on `temp_mean`, then `detect_trends()`; written to `full_prisim_climate.csv`. (b) Discharge run: climate, hydrologic (`q_mean`, `q_ar1`, `q_rbi`, `q_q50_dowy_exceed`, `runoff_ratio`), stream temperature and `gpp_modis` (renamed from `gpp_global_500m_median`), within the window, after a seasonal-site completeness rule (sites whose median annual observation count is 90-179 are "seasonal" and need n > 89 per year; others need n > 305), then `reduce_to_best_range()` on `q_mean` and `detect_trends()`; written to `best_run_prisim.csv`. Note the GPP variable has a different name in each CSV.

**`grid_modis_gpp_gee.py`** - Earth Engine extraction of MODIS MOD17A3HGF v061 annual GPP (2001-2023, hard-coded) for the 5,212-point CONUS reference grid, in the same schema as the Landsat file it replaced (`gpp_landsat.csv`). Mean of 500 m pixels within a 500 m buffer around each point; fill values (> 65500) masked. Requires `earthengine authenticate` and `--project <gcp-project>` (or `EE_PROJECT`). Options: `--out`, `--chunk` (1000), `--limit N` (smoke test), `--no-verify`. Writes a `gpp.partial.csv` checkpoint and resumes from it; delete it to force a fresh run.

**`p_pet_aridity.R`** - Annual aridity index (PET/P from Daymet forcings) and evaporative index (modeled from ET, and observed as (P - Q)/P), P-Q coupling, Budyko plot. Reads `site_groupings_by_prsim_trend.csv` (needs the `q_flag` column) and `macrosheds_et2.csv`. Four saved figures; everything after the Budyko plot is interactive exploration of individual sites.

**`patchwork_figures.R`** - Assembles manuscript Figures 1-4 with `patchwork`. Figure 1: trend maps (PNG grobs from `data_groups.R`) plus grid-vs-MacroSheds group densities. Figure 2: the "mega zipper" of discharge coverage and trend tiles. Figure 3: T trend vs P trend scatter coloured by GPP and Q trend (per decade). Figure 4: aridity-index histograms by climate quadrant. Group codes: wetting W/D/-, warming H/C/-, greening G/B/-.

### Nitrogen manuscript

**`nitrogen_trends.R`** - Annual Sen's-slope trends for NO3_N, NH3_N, TDN (from `N_VWM_*.rds`), N deposition (`N_flux_mean`, from the climate feather) and greenness (`nirv`, from `data_raw/ms_landsat_greenness_annual.csv`, supplied by Mike Vlah and not produced in this repo). Nitrogen data are trimmed to water year > 1979, years with 10 or more observations and at least 1 observation in 8 or more months, then `gen_reduce_to_best_range()`. Also summarises the `q_mean` trends from `best_run_prisim.csv` for non-experimental sites. Writes `data_working/{no3,nh3,tdn,ndep,green}_trends_annual.rds` and the matching `*_data_for_trends_annual.rds`. The "Old Seasonal Data Exploration" section at the end references an object (`n_monthly`) that nothing defines and will not run; all exports happen before it.

**`nitrogen_regression.R`** - lm / gls / lme (random intercept by hand-assigned ecoregion) models of 2010-2020 mean annual VWM for NO3_N, NH3_N and TDN against watershed attributes, non-experimental sites only. Console output only. One NH3 row is dropped by position (row 45), so check that if the input changes.

**`nitrogen_figures.R`** - Figures for Heili's 2024 AGU talk, 2025 GRC poster and the N manuscript. Live outputs: `figures/sfs_figHB.jpeg`, `N3panel_intfig.jpeg`, `peak_months_N_bw.jpeg`, `trends_no3_nh3_tdn_1980onwards.jpeg`, `monthly_vwm_old_v_new_decade.jpeg`; the other `ggsave` calls are commented out. Reads the `N_VWM_*` and `N_CQ_*` files, the `nitrogen_trends.R` outputs, `full_prisim_climate.csv` and `discharge_metrics_siteyear.rds`. Two caveats: it reads `data_working/N_CQ_annual.rds`, which no script writes (comment the line out or create the file); and the AGU / Monthly / Drivers sections near the top depend on legacy objects whose loads are commented out, so only the `#### Manuscript Figures ####` section runs against current inputs.

### Exploratory

- `camels_trend_exploration.R` - trends and warming/wetting/greening groups for the CAMELS remote-sensing yearly data (`data_raw/camels_rs/*.csv`); outputs commented out.
- `chem_sandbox.R` - scratch script for testing `gen_reduce_to_best_range()` on the N VWM data.
- `grid_trend_exploration.R` - empty.

### `archived/` and `defunct - remove before pub/`

| File | Status |
|---|---|
| `archived/nitrogen_vwm.R` | Original N VWM calculation; its own header says `metrics.R` supersedes it. Writes commented out. |
| `archived/nitrogen_filters.R` | Coverage filters for the old VWM files above. Writes commented out. |
| `defunct.../trends.R` | Pre-2024 trend loop over three start years; calls `detect_trends()` with an argument that no longer exists. Wrote the old `longest_site_run_*.csv` files. |
| `defunct.../arima.R`, `t_testing.R`, `winter warming exploration.R` | Early explorations; reference undefined objects or the removed `q_metrics.R`. |
| `defunct.../stream_order.R`, `stream_order2.R` | Earlier NHDPlus lookups, replaced by `stream_order_ms.R`. |

The defunct scripts read `discharge_metrics_siteyear.RDS` (upper case); the live pipeline writes `.rds`. They are kept for history only and are not expected to run.
