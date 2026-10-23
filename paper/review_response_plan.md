# Plan for addressing Stephanie Kampf's review of `draft_modis 1.docx`

Review dated 2026-10-05: 44 comments and 20 tracked changes. Comments are numbered here in document order (#1–#44) with Word's comment id in parentheses so each can be found in the docx. Numbers quoted below were recomputed from the current pipeline outputs on 2026-10-22 (`data_working/trends/*.csv`, `macrosheds::ms_load_sites()`).

The plan has four parts: (A) decisions that change numbers or figures and need a call before editing; (B) figure fixes; (C) the comment-by-comment plan with proposed wording; (D) mechanics and order of operations.

---

## A. Decisions needed before editing

### D1. The "first- and zero-order" claim is not implemented, and comment #37 will expose it

The abstract says "166 first- and zero-order watersheds" and the Streamflow analysis section says "we limited our analysis to first or zero order streams." No live script applies that filter: nothing reads `data_working/ms_stream_order.csv` (confirmed by grep; `src/README.md` says the same). The 166-site climate pool therefore contains:

| Site | Domain | Area (km²) | Usable Q? |
|---|---|---|---|
| TOMB | NEON | 47,129 | no |
| BLWA | NEON | 16,178 | no |
| FLNT | NEON | 14,996 | no |
| ARIK | NEON | 2,630 | no |
| BLUE | NEON | 322 | no |
| SYCA | NEON | 281 | no |
| BC_SW_4 | Boulder | 266 | yes |
| GFCP | Baltimore | 163 | yes |
| SCO | Shale Hills | 123 | no |
| trout_river | Trout Lake | 113 | yes, one of the 4 "increasing" sites |
| W-5 | Sleepers | 111 | yes |

Area distribution of the 166 sites (km²): min 0.014 (Bigelow), 5th percentile 0.08, Q1 0.50, median 3.07, Q3 11.98, 95th percentile 121, max 47,129. The 86 sites with a usable streamflow record span 0.06–266 km².

**NHDPlus rerun (2026-10-22).** `src/stream_order_ms.R` was rewritten to be non-interactive and to keep every site with its matched flowline, order, NHDPlus drainage area and snap distance (`data_working/ms_stream_order.csv`). All 166 climate-pool sites resolved. NHDPlus has no order 0, so "zero-order" has to go regardless.

Raw Strahler order of the 166: 89 order 1, 44 order 2, 21 order 3, 6 order 4, 3 order 5, 1 order 6, 2 order 7. The raw order overstates the smallest catchments: NHDPlus flowlines stop at roughly 1 km² of drainage, so a gauge on a smaller catchment snaps to the nearest mapped (larger) segment. 81 sites have an NHD drainage area more than twice their MacroSheds area; 27 of those carry a raw order of 2 or more (the three Calhoun weirs, 0.05–0.07 km², land on the order-5 Tyger River). Treating those 81 as order 1 gives an adjusted distribution of 116 order 1, 26 order 2, 16 order 3, 5 order 4, 1 order 6, 2 order 7. The adjustment threshold (ratio > 2) is a judgment call; the ratio column is in the CSV so it can be changed.

Significant non-experimental sites: RS02, North Creek and Stevenson Creek are order 1; AB00 (23 km²) and Allequash Creek (41 km²) are genuine order 2 (area ratio 1.06); Trout River (113 km²) is order 3. All four significant experimental sites are order 1.

Nine sites snapped suspiciously (snap distance over 1 km, or NHD area under half the MS area) and should be checked by hand if the filter is by order: REDB (5.0 km to Emigration Creek), weir_2/3/4 (Calhoun), POSE, Bigelow, WS-7, RedondoMeadow, black_earth_creek (23 km² gauge snapped to a 1.2 km² tributary). Under the adjustment all nine fall in order 1, but REDB, POSE and black_earth_creek could really be order 2–3.

Filter scenarios (pool / non-exp / non-exp with usable Q / significant non-exp kept / largest site):

| Filter | Pool | Non-exp | Usable Q | Significant kept | Max area |
|---|---|---|---|---|---|
| none (current) | 166 | 126 | 46 | 6 | 47,129 km² |
| adjusted order 1 | 116 | 79 | 33 | 3 (RS02, North, Stevenson) | 24 km² |
| adjusted order ≤ 2 | 142 | 103 | 37 | 5 (loses Trout River) | 85 km² |
| adjusted order ≤ 3 | 158 | 118 | 45 | 6 | 163 km² |
| raw order 1 (old file) | 89 | 64 | 26 | 3 | 24 km² |
| area ≤ 100 km² | 155 | 115 | 42 | 5 (loses Trout River) | 95 km² |

**Decision (Nic, 2026-10-22): adjusted order ≤ 2.** It keeps 142 of 166 sites and 5 of 6 significant non-experimental sites, caps the pool at 85 km², removes all six NEON rivers over 250 km², and matches the title. The filter is being wired into `setup.R` / `mega_zipper_data.R` and the pipeline rerun (with D2 and D8); every count in the abstract, results, conclusions, discussion and figure captions is refreshed from that run.

Methods wording, replacing the "first or zero order" sentence in the Streamflow analysis paragraph: "We restricted the analysis to headwater streams, defined as first- and second-order streams (Strahler, 1957; Meyer et al, 2007). Stream order was taken from the NHDPlus flowline nearest each gauge using the 'nhdplusTools' package in R (Blodgett, 2023). Because NHDPlus flowlines omit much of the headwater channel network (Fritz et al, 2013; Elmore et al, 2013), a gauge on a catchment smaller than about half the drainage area of its nearest flowline was treated as first-order; by this rule the climate pool contains [N] sites." Citation basis: Meyer et al. (2007, JAWRA 43:86–103) define headwaters as first- and second-order streams; Fritz et al. (2013, JAWRA 49:867–882) found seven of nine forests had more than 200 % more channel length than the high-resolution NHD depicts, and Elmore et al. (2013, PLoS ONE 8:e74819) found the NHD underestimates stream density by up to 250 %; Strahler (1957, Trans. AGU 38:913–920) is the ordering scheme. Reference entries for all four, plus Sen (1968) for #18, were appended to `paper/new_references.docx` on 2026-10-22.

### D2. Grid n = 4,857 depends on variables that play no role

`data_groups.R` requires complete trends in ET, PDSI and SPEI-30d (`na.omit()` across all six variables) before a grid cell enters the NWD comparison, but none of the three is used in the grouping. This is the root of comments #21–#23. Dropping them changes the count from 4,857 to 4,859 (2 cells), so the reported percentages will not move at the stated precision. Recommendation: drop the three variables from the completeness rule and from the text, rerun `data_groups.R` and `patchwork_figures.R`, and recheck the 40.3/52.4/4.1/3.3 % split and the "159 decreasing cells" breakdown.

### D3. Aridity index convention (#30)

The code and text use PET/P (CAMELS and Budyko convention); the reviewer notes the UNEP convention is P/PET. Flipping would change the Figure 4 and s4 axes, the supplement paragraph, and about a dozen numbers (2.9, 2.5, 0.66–0.67, 1.24, 1.35, 0.32, 0.59, 0.51, 1.16, 0.96, 1.01, 0.014, 0.44). Recommendation: keep PET/P, but define it at first use as the Budyko dryness (aridity) index and say explicitly that it is the reciprocal of the P/PET convention. Wording-only.

### D4. Define NWD properly (#2, #34, #36, #41)

The Methods define NWD only by hotter/drier vs cooler/wetter, but the code classifies sites by the three trend flags (temperature H/C/–, precipitation W/D/–, GPP G/B/–) and maps the 27 combinations to four coarse groups: (+), (–), no change, variable. The reviewer's "what is browning?" and "precipitation doesn't affect atmospheric demand" both follow from this gap. Plan: add a Table 2 listing the code combinations and their coarse group, and rewrite the NWD paragraph so "net" is explicit (demand terms: warming as a proxy for vapor pressure deficit, greening as vegetation water use; supply term: precipitation). Optional: PRISM provides daily VPD; adding a VPD trend would answer the "use humidity" point directly but is a new extraction for 166 sites and the grid.

### D5. GPP by water year (#16)

MacroSheds distributes only the annual (calendar-year) MOD17A3HGF value. Water-year totals could be built from the 8-day MOD17A2HGF product in Earth Engine using `grid_modis_gpp_gee.py` as a template plus the MacroSheds watershed boundaries. Recommendation: not now; reword the sentence so it no longer reads as "we could not", and ask Mike Vlah which MOD17 collection MacroSheds used so the "not documented" sentence can be replaced with a fact.

### D5 follow-up (2026-10-22, 17:50): water-year GPP is being recomputed after all

Nic decided to answer comment #16 by recomputing GPP on water years. MacroSheds has no 8-day MODIS GPP (checked in the processing code and the `ms_load_variables` catalog: the only MODIS GPP is the annual MOD17A3HGF; the 16-day products are Landsat GPP and a 250 m NPP), so `src/modis_gpp_wateryear_gee.py` sums the 8-day MOD17A2HGF product over October–September in Earth Engine for the 166 watershed boundaries (`src/export_ws_boundaries.R`) and the 5,212 grid points, with calendar-year sums alongside as a check. `gpp_source <- 'water_year'` in `setup.R` switches the pipeline; `metrics.R` and `data_groups.R` substitute the files. After the extraction: rerun `metrics.R` → `patchwork_figures.R`, `paper_numbers.R`, `anomaly_analysis.R`; refresh every GPP-dependent number in the draft (GPP flags, NWD shares, quadrant GPP counts, decreasing-cell causes, Figure 5 and the anomaly paragraphs); rewrite the MOD17 calendar-year paragraph in Data sourcing and the GPP row of Table 1; reply to #16. Table S1 and figure s3 follow the pipeline (the sensitivity script reads the substituted variable), so their MODIS rows become water-year sums; figure s2 stays on the annual product.

Extraction done 17:52 (2.5 min): 166 watersheds (165 with valid pixels; GFGL has none), 5,212 grid points; median 46 composites per water year; calendar-year sums from the same composites against the MacroSheds annual values r = 0.993, median ratio 1.000; water-year vs calendar-year r = 0.998. Consequence to state in the paper: the fill is masked before the watershed median, so six sites that MacroSheds' unmasked median had turned into all-fill (DRKR, DV01, GFGB, MAWI, saw_mill_brook, upper_ipswich) now have GPP records; only GFGL is excluded.

Rerun on water-year GPP done 2026-10-22 18:02–18:38 (all ten scripts exit 0; `log/rerun_wateryear_gpp.log`). What moved: GPP usable 135 → 141 of 142 (GFGL only excluded); GPP flags 24/14/97 → 25/15/101 (increasing/decreasing/non-significant); NWD MS (+)/no change/variable/(−) 47.2/42.3/9.9/0.7 % → 47.9/40.8/9.9/1.4 % (the two (−) sites are now SM01 and SM04, browning alone; TECR is no longer (−)); grid 40.3/52.4/4.1/3.3 % → 41.1/51.5/4.2/3.2 % of 4,859, decreasing cells 159 → 157 (93 wetting without warming, 58 browning alone, 6 cooling); quadrant GPP flags HW 17/0/29 → 18/0/31, HD 3/13/32 → 3/14/34. Streamflow, temperature and precipitation counts unchanged (83/49 usable, 10 significant, 65 warming, 2 precipitation). Validation on the water-year series and the 142-site pool: Landsat vs MODIS GPP n = 139, r = 0.63, RMA slope 0.74; Landsat GPP vs MODIS EVI n = 140, r = 0.55, slope 0.65; sensor effect 59 of 73 (81 %) Landsat-greening sites become non-significant, 12 remain; MODIS 2001–2021 flags 14/112/15, MODIS 2001–2023 25/101/15 (1 insufficient). Anomaly analysis (rerun by the other session at 18:37 with new figure panels): GPP site-years 676 → 722 at 43 sites; the other session is sending the exact E3 text edits.

Text mappings for the water-year pass (numbers marked [N] come from the rerun):
- 236D422A: "we used the MODIS MOD17A3HGF annual GPP product (500 m resolution) as distributed by MacroSheds (variable gpp_global_500m_median), which reports the median annual GPP of the MODIS pixels within each watershed boundary for 2001–2023 (Running et al, 2004; Running & Zhao, 2021)." → "we summed the 8-day MODIS MOD17A2HGF product (Collection 6.1, 500 m; Running et al, 2004; Running & Zhao, 2021) over each water year (October–September) in Google Earth Engine and took the median of the pixels within each watershed boundary for 2001–2023." (Nic, 18:04: the draft must read as the published method, with no mention of the calendar-year product or of the change; the validation against the annual product stays in the plan and the response letter only. Also drop "annual" before GPP in the Abstract and the Figure 1 caption, and re-point figure s2's comparison script at the water-year series so no part of the paper still uses the annual product.)
- 31900D3A: whole paragraph → "MOD17 writes a fill value in place of GPP wherever land cover is classed as urban, barren, or water. We masked these pixels before taking the watershed median, so a watershed has a GPP record wherever it contains vegetated pixels; one site (GFGL, in the Baltimore domain) has none and is excluded from GPP trend analysis, leaving [N] sites with a usable GPP record."
- 7E584298: "Second, MOD17A3HGF reports a calendar-year total. ... derived from MOD17A3HGF Collection 6.1." → "Second, the 8-day composites start on fixed days of the year, so a water-year sum comprises the composites that begin between 1 October and 30 September; the few days of overlap at each end cancel, and GPP is aligned with the water-year streamflow and climate metrics. The grid GPP described below was summed the same way."
- 3E207CBB: "MODIS annual GPP from MOD17A3HGF Collection 6.1 (2001–2023)" → "water-year MODIS GPP summed from the 8-day MOD17A2HGF composites (Collection 6.1, 2001–2023)"; "(MOD17 stores GPP as integers; multiplying by 0.0001 gives kg C m⁻² yr⁻¹)" → "(MOD17 stores GPP as integers; multiplying by 0.0001 gives kg C m⁻² per composite)".
- Table 1 caption: "calendar years for the grid and for MODIS GPP" → "calendar years for the grid climate"; GPP rows: source "MODIS MOD17A2HGF Collection 6.1 (8-day), water-year sums in Google Earth Engine; watershed median" / "... mean of pixels in the 1 km² footprint", time step "8-day, summed to water years", aggregation "water-year total (kg C m⁻² yr⁻¹); fill pixels masked".
- 16C9F0A2 (anomaly Methods): delete "Because annual MODIS GPP is a calendar-year value assigned to the same-numbered water year, the GPP anomaly overlaps the streamflow year by nine months."
- 334A7E09: "GPP trends for 135 of them; the remaining seven sites fall in urban land cover that the MODIS product reports as fill values" → "GPP trends for [N] of them; the remaining site (GFGL) has no vegetated MODIS pixel".
- 1922EA2C: "MOD17A3HGF is an annual product at 500 m, so a single pixel" → "MODIS GPP has a 500 m pixel, so a single pixel"; "Seven sites (four in Baltimore, one at Santa Barbara, and two at Plum Island) return the MOD17 urban/barren fill value and were masked, so they have no GPP trend." → "One site (GFGL, Baltimore) has no vegetated MODIS pixel and therefore no GPP trend."; delete "MODIS GPP is also a calendar-year total whereas streamflow is aggregated by water year, so the two annual series are offset by a season."
- 1CA89AB8 (fig s3) and the Table S1 caption: "MOD17A3HGF (annual, 500 m) with the urban/barren/water fill value masked" → "the water-year sum of MOD17A2HGF (8-day, 500 m) with the urban/barren/water fill pixels masked"; refresh the MODIS counts.
- Numbers: GPP flags (now [N] of [N]), NWD shares (MS and grid), quadrant GPP counts, decreasing-cell causes, Table S1 MODIS rows, 44BEAB20, 2817434F, 1458A930, Figure 5 paragraphs and caption n, abstract GPP clause if it changes qualitatively.
- Reply to #16: "Recomputed: GPP is now the October–September sum of the 8-day MOD17A2HGF product for every watershed and grid point, so GPP, streamflow and climate share the same water year; calendar-year sums from the same composites reproduce the MacroSheds annual values (r = 0.99)." Reference entry for the 8-day product added to new_references.docx (needs an a/b suffix against the annual entry).

### D6. Move GPP product validation to the supplement with a table (#33)

Agree. Keep a three-sentence summary in Methods (why MODIS; Landsat greening is mostly a sensor effect; see Table S1 and figures s2–s3). Build Table S1 from the counts already in the figure s3 caption (increasing / non-significant / decreasing for Landsat 1986–2022, Landsat 2001–2021, MODIS 2001–2021, MODIS 2001–2023, plus the 33/113 and 64/85 transitions). Trim the same numbers from the Discussion paragraph "A Landsat-based view…" to one sentence pointing to the table.

### D7. Add a variable table (#31)

Facts confirmed 2026-10-22 from the MacroSheds processing code (github.com/MacroSHEDS/data_processing): the MacroSheds GPP variable is Earth Engine `MODIS/061/MOD17A3HGF` (Collection 6.1, band Gpp, watershed median of 500 m pixels); the MacroSheds climate series are `OREGONSTATE/PRISM/AN81d` (daily, 4 km, watershed median); `pet(mm)` in the CAMELS-compliant Daymet forcings is Priestley–Taylor with the gridded alpha of Aschonitis et al. (2017, ESSD 9:615–638). Still unconfirmed: which PRISM product the grid files were exported from (no script in this repo; ask Heili). MODIS EVI product for figure s2 also unconfirmed.

Draft Table 1 for Methods (fill the "confirm" cells before use):

| Variable | Source / product | Native resolution | Time step | Aggregation used for trends | Sites | Window | Used for |
|---|---|---|---|---|---|---|---|
| Streamflow (Q) | MacroSheds discharge (Vlah et al, 2023) | gauge | daily | water-year mean daily flow, mm d⁻¹ (`q_mean`), after completeness check | MS | 2001–2023, longest span with ≤ 40 % missing years | RQ1 trends, Fig. 2–4 |
| Air temperature | PRISM, watershed median distributed by MacroSheds | PRISM grid (confirm 800 m vs 4 km) | daily | water-year mean, °C | MS | 2001–2023 | NWD, Fig. 1–4 |
| Air temperature | PRISM via Earth Engine | 4 km | annual (confirm source script; none in repo) | calendar-year mean, °C | grid | 2001–2023 | NWD, Fig. 1 |
| Precipitation | PRISM, watershed median via MacroSheds | as above | daily | water-year mean daily, mm d⁻¹ | MS | 2001–2023 | NWD, Fig. 1–4 |
| Precipitation | PRISM via Earth Engine | 4 km | annual (confirm) | calendar-year total, mm | grid | 2001–2023 | NWD, Fig. 1 |
| GPP | MODIS MOD17A3HGF (collection: ask MacroSheds), watershed median | 500 m | annual (calendar) | annual total, kg C m⁻² yr⁻¹; fill ≥ 6.55 masked | MS | 2001–2023 | NWD, Fig. 1–3 |
| GPP | MOD17A3HGF v061 via Earth Engine, mean of pixels in 1 km² footprint | 500 m | annual | as above | grid | 2001–2023 | NWD, Fig. 1 |
| PET and P (Daymet) | MacroSheds "CAMELS Daymet forcings" (Addor et al, 2017; Thornton et al, 2022) | 1 km | daily | water-year sums; PET/P averaged 2001–2023 | MS | 2001–2023 | aridity classes, Fig. 4 |
| Landsat GPP | NTSG product via MacroSheds (USGS, 2025) | 30 m | 16-day | annual | MS | 1986–2022 | validation only (Fig. s2–s3) |
| MODIS EVI | confirm product (MOD13?) | 250/500 m | 16-day | annual | MS | 2001–2023 | validation only (Fig. s2) |
| ET (MOD16A2), PDSI, SPEI-30d | grid only | — | — | — | grid | — | drop (D2) |

Note the MS vs grid mismatch this table makes visible: MS precipitation trends are in mean daily precipitation by water year, grid trends are annual totals by calendar year rescaled by 1/365 in `data_groups.R`. Figures 1 and 3 both label them "mean annual"; fix the labels once the table settles the definitions (also answers #19, #24–#27, #39–#40).

### D8. Seasonal sites are not "added back" as the Methods say (raised by Nic via the parallel session, 2026-10-22)

The Streamflow analysis paragraph says sites with seasonal sampling, such as the Niwot Ridge gauges, "were manually flagged and added back into the analysis." No script does that. `mega_zipper_data.R` (lines 42–57) has a seasonal rule (median 90–179 discharge days per water year → keep years with more than 89 days), but since `q_freq_check = T` was enabled in `metrics.R` (2026-08-06) the 51-week × 4-observation check runs first and leaves only year-round site-years, so the seasonal rule flags 0 of 228 sites. With the check off it flags 23 sites: Niwot GREEN4, GREEN5, MARTINELLI, NAVAJO, SADDLE, como; Loch Vale icy_brook; NEON FLNT, GUIL, HOPB, CARI, MCDI, TECR; and the Bonanza and Arctic sites, which are outside CONUS. Today GREEN5, MARTINELLI, NAVAJO, SADDLE and como have zero passing years, GREEN4 has 9, and ALBION is the only Niwot site with a streamflow trend.

Nic wants the code to match the sentence. Recommended implementation: in `metrics.R`, classify sites by their median raw discharge count per water year before `frequency_check()`, and pass seasonal sites (median 90–179 days) on the relaxed rule (more than 89 observations in the water year) instead of the 51-week rule; year-round sites keep the strict rule.

**Implemented 2026-10-22 (switch `seasonal_relaxed_check` in `setup.R`, classification in `data_working/seasonal_sites.csv`) and then checked against the raw records. The rule as implemented produces artifacts and should not be used as is.** With it on, three Niwot gauges acquire significant mean-flow trends (GREEN4 decreasing, slope −0.11 mm d⁻¹ yr⁻¹ over 2001–2022; MARTINELLI increasing, +0.58, 2001–2019; NAVAJO increasing, +0.43, 2001–2014). The raw daily records show why: the annual mean is taken over whichever days were sampled, and the sampling window changes between years.
- GREEN4 was sampled for 128–179 days in 2001–2008 (melt season) and 365 days from 2009 on; mean flow over the sampled days drops from 100–165 L s⁻¹ in the seasonal years to 78–110 L s⁻¹ once winter low flows are included. The "decrease" is the change in window.
- NAVAJO starts on day 1 of the water year in 2001–2003 (winter included, means 43–59 L s⁻¹) and on days 228–258 from 2004 (melt season only, means 46–80 L s⁻¹). The "increase" is the same artifact in the opposite direction.
- MARTINELLI is sampled from day 205–267 to day 320–366 in most years but from day 1 in five of them; its means (2–17 L s⁻¹) are noisy and the trend is not trustworthy either.
Options: (i) keep the relaxed admission rule but, for seasonal sites, compute the trend variable over a fixed window present in every retained year (e.g. the summer (JJA) mean already produced by `metrics.R` as `agg_code = 'Summer'`, with its own completeness check of ≥ 80 days), and label those sites in Figures 2–4; (ii) add a window-consistency test (first and last sampled day within ±15 days of the site median) so years with a different window are dropped; (iii) turn `seasonal_relaxed_check` off, keep seasonal sites out, and rewrite the Methods sentence to say that seasonal gauges were excluded because their annual means are not comparable across years. Recommendation: (iii) for this revision unless Nic wants Niwot in, in which case (i). Either way the Methods sentence "manually flagged and added back" must go.

**Decision (Nic, 2026-10-22): try option (i), with the rule applying only to sites that cannot get in through the standard gate.** Implemented as follows (switch `seasonal_summer_path` in `setup.R`, with `summer_months <- c(6, 7, 8)` and `seasonal_min_summer_days <- 80`):
- `metrics.R` runs the standard `frequency_check()` for every site (the relaxed rule is gone, `seasonal_sites.csv` is no longer written) and, independently, saves the mean discharge over June–August for every site-year with at least 80 summer observation days to `data_working/q_summer_siteyear.rds`.
- `mega_zipper_data.R` fits the standard trends first. Headwater sites that do not reach a usable `q_mean` trend that way (fewer than 10 passing years) are then fitted on the summer-window series with the same best-range rule (≥ 10 years, ≤ 40 % missing); their cut-to-Q climate trends use the same years. A site enters `best_run_prisim.csv` through exactly one path; the new column `q_window` is `'water_year'` or `'JJA'`. `data_working/seasonal_path_sites.csv` lists every candidate and whether it got in.
- Draft Methods sentence to replace "Sites with limited or seasonal sampling ... were manually flagged and added back into the analysis.": "Gauges that operate only in the snow-free season (for example the Niwot Ridge sites) cannot meet this criterion. For sites that did not reach a usable record under it, we instead used mean streamflow over a fixed June–August window, retaining water years with at least 80 observation days in that window and applying the same minimum of 10 years with no more than 40% missing; [N] sites entered the analysis this way and are marked in figures 2–4."
- Rerun result (2026-10-22, 14:47): 14 of 65 gate-failing headwater sites reached a usable trend on the summer window: Niwot GREEN4, GREEN5, MARTINELLI, NAVAJO, SADDLE; Santa Barbara BC02, HO00, SP02; Catalina-Jemez HistoryGrove, UpperJaramillo; Plum Island cart_creek, saw_mill_brook; KREW P300 and Santee WS78 (experimental). Significant: BC02 and HO00 decreasing, GREEN5, MARTINELLI and NAVAJO increasing; GREEN4 is non-significant once the window is fixed. Usable Q 83 (49 non-exp); significant non-exp 10 of 49 (6 inc, 4 dec); aridity with/without Q 0.52 vs 1.20, 20 % vs 70 % arid, p = 2.1 × 10⁻⁴.
- **Decision (Nic, 2026-10-22): keep them, call them "summer-only sites", mark them in figures, and add a supplementary table with a reason per site.** Done: `patchwork_figures.R` marks them (Figure 2 light-purple rows showing the summer-window years, Figure 3 squares, Figure 4 rug ticks); Methods sentence written; Table S2 (site, domain, status, area, why the standard criterion was not met, summer-window years, summer-flow trend) placed at the end of the supplement. Reasons come from the raw records: the four seasonal Niwot gauges have no winter observations (median 120–139 days per water year); GREEN4 was seasonal through 2008 and year-round after (9 complete years); BC02, HO00, SP02, P300 and WS78 have 8–9 complete water years, one or two short of ten, with BC02, SP02 and WS78 showing zero flow on 8–20 % of days; HistoryGrove and UpperJaramillo have winter gaps (2 and 5 complete years); cart_creek and saw_mill_brook have scattered gaps (6 and 2). Caveat kept in the Results: the Niwot increases are marginal (lower CI bounds just above zero) and Santa Barbara summer trends are dry-season flow. That reuses the thresholds already in `mega_zipper_data.R`, removes the dead seasonal branch there, and needs no hand-maintained site list. Then rewrite the Methods sentence to describe the rule rather than a manual flag. Expected effect: the six Niwot gauges, icy_brook and up to four NEON sites (FLNT is order 7 and HOPB/MCDI are order 2–3, so D1 may remove some) can gain a usable streamflow record if they have 10 or more passing years; the climate pool of 166 does not change, but the usable-Q counts (46 non-experimental, 79 overall), the aridity comparison of sites with and without records, and Figures 2–4 do. Fold into the same rerun as D1 and D2.

---

## B. Figures (#38, #44)

Resolution is not the problem: the PNGs embedded in the docx are the full 300-dpi source files (Figure 1 is 4200 × 3000 px, Figure 3 is 4800 × 2100 px). The problem is design size. Every main figure is drawn at 10–16 in wide and displayed at 6.5 in, so fonts shrink to 40–65 % of their nominal size, and Figure 1 rasterizes three 10 × 7 in map PNGs into ~7 × 5 in panels through `rasterGrob`.

- **Status 2026-10-22, 15:35:** implemented. `data_groups.R` now saves the three map ggplot objects as RDS and takes grid coordinates from `ppt.csv` (the previous lookup used `et2.csv` geometry at the minimum year, 1980, where every value is NA, so the geom_tile maps had been drawing no grid cells at all since commit ed85ef9; this is the "lost heatmap"). `patchwork_figures.R` lays Figure 1 out from those objects, and Figures 1–4 are exported at 7.5 in width (400 dpi PNG plus PDF) with text sizes set for print. Comments #16, #19, #29, #31, #38 and #44 are written up for Nic to answer in `paper/comment_handoff_for_nic.md`.
- **Figure 1.** Build the maps as ggplot objects inside `patchwork_figures.R` (source the map code from `data_groups.R` or have it `saveRDS()` the three map objects) instead of loading PNGs. Design at the print width (7–7.5 in for a two-column figure), `base_size` 10–12, legend text readable at that size; consider giving the density panel a full row so the three maps can be larger. Export PDF for submission and PNG at 400 dpi for the docx.
- **Figure 3.** Drawn at 16 × 7 in; the quadrant annotations are `size = 3.5` (about 10 pt), which becomes about 4 pt in Word. Redesign at 7.5 in wide (stack the two panels vertically or shrink to two 3.6-in panels), `base_size` 10, annotation size ≥ 3.5 at that width, and move the n labels out of the corners if they still collide with points.
- **Figures 2 and 4** have the same scaling issue (12 × 10 and 10 × 9 in) and should be set to print width in the same pass.
- **Figure s1** (flowchart) is only 864 × 1056 px; re-export it at 300 dpi.
- Replace images in the docx from Word (or `update_image`) after the rebuild.

---

## C. Comment-by-comment plan

Format: **#n (id)** — anchored text → comment. Then the plan or proposed wording. "Numbers" means the item waits for the D1/D2 rerun.

### Introduction – Background

**#1 (982843393)** — equation 1 → "usually S represented as + or −". Change the equation to Q = P − E − T − ΔS and define ΔS in the preceding sentence as the change in storage over the period (positive when storage increases). Equation is OMML; edit in Word, not with docx-mcp.

**#2 (1040970997)** — "Temperatures are increasing across most of the CONUS, increasing evaporation" → evaporation is not a direct function of temperature. Proposed: "Temperatures are increasing across most of the CONUS (Ji et al, 2014). Warming raises atmospheric water demand because higher air temperature is generally accompanied by a higher vapor pressure deficit, so potential evaporation increases; actual evapotranspiration rises only where water is available to meet that demand." Check that Ji et al (2014) is a temperature-trend source (the rewrite attaches it to the temperature sentence only); if it was cited for the evaporation claim, move or replace it.

**#3 (777513837) and #4 (434487546)** — "productivity" → plant productivity? be explicit about CO₂. Proposed: "Concurrently, plant productivity (gross primary productivity, GPP) has been increasing across most of the CONUS, in part because elevated atmospheric CO₂ stimulates photosynthesis (Zeng et al, 2018; Wang et al, 2021; Yang et al, 2023)." Check that one of the three citations supports the CO₂ fertilization clause; if not, add one to the references doc. This also defines GPP at first use, which resolves #14.

**#5 (539070697)** — "leading to increased transpiration as temperatures continue to rise" → in places with available water? Append "where soil water is available".

**#6 (13418329)** — "essential." → essential for what? Proposed: "becomes essential for anticipating water availability and for understanding how watersheds are responding."

**#7 (2029486498)** — "inputs" → precipitation is the only input. Proposed: "Given all this change in the terms of the water budget, it is reasonable to expect change in the output, namely…" (streamflow is the output; E and T are losses, P the input).

**#8 (947126173)** — "productivity" (single-sensor record of productivity) → needs a qualifier. Change to "ecosystem productivity (GPP)".

Tracked change in the same paragraph: Stephanie deleted the Lins & Slack (1999) clause. Accept it, then check whether Lins & Slack is cited anywhere else; if not, remove it from the reference list (separate references doc per the usual workflow).

### Introduction – Research questions

**#9 (1274505262)** — RQ1 "streamflow" → which type? The only reported trend variable is water-year mean daily streamflow. Change RQ1 (and its copy in Conclusions) to "What are the trends in mean annual streamflow…", and say in Methods that other annual metrics (Magnificent 7, flashiness, etc.) were computed but only mean flow is reported (or drop the list).

**#10 (408170488)** — streamflow vs discharge. Standardize on "streamflow" in prose and Q as the symbol; keep "discharge" only as the MacroSheds product name. Nine body paragraphs use "discharge": 19F9B568, 334A7E09, 64506AA6, 40810FFA, 1458A930, 6C63D381, 4CB5DE8B, 1922EA2C, 61B7BBC5. Also the Figure 2 caption ("discharge data").

**#11 (321989700)** — "abundant" → quantify. Proposed: "We expect significant streamflow trends at most sites that show a significant trend in temperature or precipitation, because small watersheds…"

**#12 (1456164771)** — "changes in water balance" (RQ1A) → changes in climate? Yes: "to changes in climate". Update the RQ1A copy in Conclusions (26B51A94) too.

**#13 (769680520)** — "watershed studies" (RQ2) → studies or sites? Proposed: "How well does the existing network of small-watershed study sites capture…". Update the Conclusions copy (1F7E048B) and the Results opening sentence ("monitoring efforts across watershed science").

### Methods – Data sourcing

**#14 (1106614526)** — "GPP," define. Resolved by #3; also expand at first Methods use: "gross primary productivity (GPP)".

**#15 (278268551)** — "65535, or 6.5535" units? Proposed: "(65535 in the stored integer, or 6.5535 kg C m⁻² yr⁻¹ after applying the product's 0.0001 scale factor)". Make the Figure 3 legend units match (kg C m⁻² decade⁻¹).

**#16 (1359991307)** — "is not documented" → could you not get water-year values? See D5. Proposed replacement for the last two sentences: "MOD17A3HGF reports a calendar-year total. We used the annual product as distributed and assigned each value to the water year ending in the same calendar year (a nine-month overlap); this offset does not affect the trend estimates but means annual GPP and streamflow are not aligned to the same twelve months. [Collection X was used for the MacroSheds variable; the grid GPP was extracted from Collection 6.1.]"

Also add the site size range here (needed for #37), numbers after D1.

### Methods – Streamflow analysis

**#17 (389712045)** — "trends in daily streamflow data" → why daily? Restructure the paragraph: daily records → completeness screening → annual (water-year) metrics → trends on the annual series. Opening sentence: "we computed annual metrics from daily streamflow records at MS sites and analyzed trends in the annual series."

Also in this paragraph: fix or remove the "first or zero order" sentence per D1.

**#18 (374292661)** — "to" (calculate trends) → is this Mann–Kendall? Add to the Sen's slope paragraph (4DB21A59): "Significance was judged from the 95 % confidence interval of the Sen slope, which `zyp` derives from the variance of Kendall's S (Sen, 1968); a series was classed as increasing or decreasing when the interval excluded zero and non-significant otherwise. No prewhitening was applied." (Verified against `zyp::confint.zyp`: it uses `Kendall()` varS, no Yue–Pilon.) Add Sen (1968) to the references doc.

### Methods – Gridded sampling

**#19 (1569590748)** — "temperature and precipitation" time step? State it: daily PRISM for MS sites, annual PRISM values for grid sites (confirm how `ppt.csv`/`temperature.csv` were produced; there is no extraction script in the repo). Table 1 carries the detail.

**#20 (1196937058)** — "scale factor" what is this? Fold into #15's definition and reword: "after masking the fill value and applying the 0.0001 scale factor" (the script masks, scales, then averages; order does not matter for a mean).

**#21 (805742360)** — "evapotranspiration" actual or potential? Under D2 the sentence is deleted. If kept: "actual evapotranspiration from the MODIS MOD16A2 8-day product".

**#22 (50687076)** — "drought indices" mentioned before? Under D2, delete. If kept, name them (PDSI and 30-day SPEI) and their source.

**#23 (238446062)** — "enter the NWD comparison" unclear. Proposed (after D2): "Of the 5,212 points, [4,859] had complete temperature, precipitation, and GPP trends and were used in the comparison with MS sites."

### Methods – Climate analysis

**#24 (1889915785), #25 (1448091359)** — "annually": summary statistics by year? water or calendar year? The text also lists "mean, maximum, minimum, and variance", which `metrics.R` does not compute. Proposed rewrite of the first two sentences: "From the daily PRISM series we computed, for each site and water year (October–September), mean daily temperature and mean daily precipitation; these annual series are the basis of all temperature and precipitation trends reported here. Grid sites use annual PRISM values by calendar year." Grid trends are per calendar year (confirm with #19).

**#26 (1144407105)** — "cumulative precipitation" how is this different from precipitation? In the code it is a timing metric: the day of the water year by which half the annual precipitation has fallen. **#27 (1535967953)** — "precipitation intensity" over what period? In the code: annual total divided by the number of days with more than 5 mm (the code comment says 1 mm; fix one or the other). Neither metric is reported anywhere in the paper. Recommendation: delete the clause "along with cumulative precipitation and precipitation intensity" and the monthly/seasonal mention, so the Methods describe only what the Results use.

**#28 (1772405568)** — "(the CAMELS-style Daymet product)" unclear. Proposed: "the MacroSheds 'CAMELS Daymet forcings' product, which reproduces the CAMELS forcing variables (Addor et al, 2017) from Daymet (Thornton et al, 2022) for each MacroSheds watershed".

**#29 (656684849)** — "daily PET" method? Not stated in the MacroSheds package docs (searched); CAMELS forcings use Priestley–Taylor (Newman et al, 2015). Confirm with MacroSheds documentation or Mike Vlah, then state the method and add the citation to the references doc.

**#30 (1951603216)** — AI convention. See D3. Proposed: "an aridity index for each site, defined as in the Budyko framework as the ratio of potential evapotranspiration to precipitation (PET/P; the reciprocal of the P/PET convention used in climatology)".

**#31 (676443099)** — table of variables. See D7; place Table 1 at the start of Data sourcing and reference it from each data paragraph.

### Methods – GPP product validation

**#32 (1460682386)** — "related" → "correlated". Accept.

**#33 (2108557058)** — too many numbers; table and move to supplement. See D6.

### Methods – Expected response groupings

**#34 (381676394)** — "combining changes in vegetation" → PET does not account for vegetation. Proposed rewrite of the first two sentences: "Potential evapotranspiration (PET) is the usual measure of atmospheric water demand; it integrates temperature, radiation, and humidity (via vapor pressure deficit) (Zeng et al, 2018), but it describes demand on a reference surface and does not respond to changes in the vegetation that actually transpires, and reference-crop formulations translate poorly to natural vegetation (Heerwaarden et al, 2010)."

**#35 (1976979258)** — "to smaller, less anthropogenically affected systems" → "to natural vegetation". Accept (included above).

**#36 (585394592)** — "drier (less precipitation)" → precipitation does not affect atmospheric demand. See D4. Proposed replacement for the paragraph's first two sentences: "NWD is a qualitative balance between atmospheric demand for water and its supply. Demand rises with warming (a proxy for rising vapor pressure deficit) and with increasing vegetation productivity (greening, a significant increase in GPP); supply falls with declining precipitation. A watershed with significant warming, drying, or greening and no opposing trend is classed as increasing NWD (+); one with significant cooling, wetting, or browning (a significant decrease in GPP) and no opposing trend is decreasing NWD (−); a watershed with no significant trend in any driver is 'no change'; mixed combinations are 'variable' (Table 2)." Add Table 2 from the `coarse_grouping` lookup in `data_groups.R`. Optional sentence acknowledging that PRISM VPD would be a more direct demand measure.

**#37 (1939410491)** — "As sites are small," → give size range; why would small size mean static storage? Two fixes. (i) Site description: add the area range to the Data sourcing paragraph (numbers after D1). (ii) Replace the storage sentence: "Over multi-year windows we treat the change in storage (ΔS) as small relative to the other terms, the standard closure assumption for long-term water-balance analysis; this is an assumption about the averaging period, not catchment size, and a persistent trend in groundwater storage would violate it."

### Results – Study coverage of NWD

Tracked changes by Stephanie in 44278C49 ("monitoring", capitalisation, semicolons): accept.

**#38 (757868304)** — Figure 1 maps too small, lower-right text blurry. See Part B.

**#39 (601305499), #40 (1659345861)** — caption "temperature", "precipitation" → mean annual? Proposed: "Trends in mean annual temperature (top left), annual precipitation (PPT, bottom left) and annual gross primary productivity (GPP, top right)…". Confirm the map units in `data_groups.R` when rebuilding (grid precipitation trend is in mm yr⁻¹ of annual total unless rescaled).

**#41 (1414464526)** — "browning" what is this? Defined in Methods under #36; at this first Results use write "browning (a significant decrease in GPP) alone".

### Results – Data availability

Tracked change in 334A7E09 ("F"): accept.

**#42 (1982586400)** — "very few sites show corresponding changes in discharge" → where shown? Change to "…changes in streamflow (figure 3)". Figure 2's bar panels show only T, P and GPP flags; consider adding a Q flag bar so Figure 2 supports the sentence on its own.

### Results – Trends in drivers and outputs

**#43 (2018682351)** — "it" (where GPP changed it increased) → streamflow? The "it" is GPP; reword: "and GPP, where it changed, increased (22 greening, 0 browning, 37 non-significant)".

**#44 (1943811490)** — Figure 3 blurry, corner text unreadable. See Part B.

---

## D. Mechanics and order of operations

1. **Decide D1 and D2** (and D3–D7 are wording/structure; defaults above). Wire the site filter into the pipeline, drop ET/PDSI/SPEI from the grid completeness rule, rerun from `mega_zipper_data.R` through `patchwork_figures.R`, and regenerate every count. The accuracy-review notes from 2026-09-22 still apply: take all numbers from the rerun, not from memory.
2. **Accept Stephanie's 20 tracked changes** (all copy edits; the only substantive one is the Lins & Slack deletion). After accepting, fix the punctuation her deletion leaves behind in the Background paragraph: "wetter but less extreme, Some of those changes" needs a period before "Some". Then make the wording edits in Part C as tracked changes under the author's name so co-authors can see them.
3. **Structure:** add Table 1 (variables) and Table 2 (NWD codes) to Methods; move the validation numbers to Table S1 in the supplement; trim the Discussion duplicate.
4. **Figures:** rebuild at print width (Part B), replace in the docx.
5. **Numbers pass:** update abstract, results, conclusions, discussion and captions from the rerun; re-read the aridity, site-count and percentage sentences together so they agree.
6. **Consistency pass:** streamflow vs discharge; GPP/greening/browning defined before use; RQ wording identical in Introduction and Conclusions; "first-order"/"zero-order" removed everywhere; title says "Two Decades" while `CLAUDE.md` and `src/README.md` still say "Four Decades" (update the repo text to match the paper). Also close out the two pending items from the 2026-08-06 accuracy review: the "less than 1%" arithmetic error (#1) no longer appears anywhere in the body text, and the independent climate/streamflow trend windows (#2) are now described correctly in the Climate analysis paragraph and the Figure 2 caption; verify both once more and mark them resolved.
7. **References:** new entries for Sen (1968), Newman et al (2015) if the PET method is stated, and any CO₂ fertilization source; drop Lins & Slack (1999) if orphaned. Entries go in the separate references Word doc, in-text citations only in the draft. Strahler (1957), Meyer et al (2007), Fritz et al (2013), Elmore et al (2013) and Sen (1968) were appended to `paper/new_references.docx` on 2026-10-22. Deleting the grid ET sentence (D2) orphans Mu et al (2011), which that file also lists; drop it from the reference manager unless it is cited elsewhere.

---

## F. Numbers pass checklist (after the D1/D2/D8 rerun)

**Status 2026-10-22, 15:10: done.** Both passes are applied to `draft_modis_r1.docx` as tracked changes (climate side first, streamflow side after the summer-window rerun), Tables S1 and S2 are in the supplement, and Figures 1–4, S3 and S4 in the docx are the regenerated files. The rows below are kept as the audit trail. Also done 15:15: Table 2 (NWD code lookup, built from the `coarse_grouping` rule in `data_groups.R`) inserted after the NWD paragraph in Methods, and equation 1 changed to Q = P − (E + T + ΔS) with ΔS defined in the preceding sentence (comment #1 answered). Note for the authors: the lookup groups HDB (warming, drying, browning) with increasing NWD although browning points the other way; it does not occur in the data, but ratify or move it to "variable" (this is the accuracy-review item #4 that was never signed off). Still open: Table 1 (variables; needs the three confirmations in D7), the MOD17 collection and PET-method facts, the grid PRISM time step, and the figure print-width redesign (#38, #44).

Every count below lives in `draft_modis_r1.docx` and must be replaced from `data_working/paper_numbers.md`. Paragraph ids are Word paraIds. Work top to bottom; the same number often appears in four places (Results, Conclusions, Discussion, caption), so change all of them together.

| Paragraph | Section | Numbers to refresh |
|---|---|---|
| 14730FB1 | Abstract | 166 sites; 5,212 grid points; warming 71 of 166; precipitation 2 sites; 126 non-exp, 46 usable, 6 significant (4 inc Wisconsin, 2 dec California) |
| 31900D3A | Data sourcing | 8 fill-masked watersheds and their domains; 158 with GPP |
| 7E584298 | Data sourcing | 166 sites; median 3.07 km², IQR 0.5–11.98; 24/60/79 smaller than 1/4/10 pixels; add the size range (#37) |
| 2E4C6B69 | Streamflow analysis | 166 = 40 experimental + 126 non-experimental |
| 614BA6D7 | Streamflow analysis | rewrite the last sentence once D8 is implemented (describe the seasonal rule; "manually flagged" goes) |
| 3E207CBB | Gridded sampling | 4,857 complete grid cells |
| 4F2E137E | Climate analysis | median record 23 years |
| 35AEA2C3, 0B4443A3, 36AFC4BF | Climate analysis, Supplement, Fig s4 | r = 0.999, median shift 0.014, max 0.44, 1 of 166 (BLUE 0.96 → 1.01) |
| 3BA0B113, 1CA89AB8, 26D1D5F4, 44BEAB20 | GPP validation, Fig s3, Fig s2, Discussion | all Landsat/MODIS flag counts (113/40/10, 85/57/21, 16/123/19, 29/112/17, 33 of 113, 64 of 85, 115, 26, 25), n = 113 r = 0.52 slope 0.75, n = 120 r = 0.58 slope 0.70; these move to Table S1 (D6) |
| 44278C49 | Results, coverage | 71 of 166 warming; 1 and 1 precipitation; MS 46.4/42.8/8.4/2.4 %; grid 40.3/52.4/4.1/3.3 % of 4,857 |
| 26854DAE | Figure 1 caption | 5,212; 4,857 |
| 6301C7ED | Results, coverage | 159 decreasing cells (3.3 %); regions 51/50/36/12/7/3; 98 wetting, 52 browning; the four MS decreasing sites |
| 334A7E09 | Results, data availability | 166; 23 years; 158; eight fill sites; 166 → 79; 126 → 46; 19 years; end year 2020; seven non-CONUS; AI 0.51 (22 %) vs 1.16 (64 %); Wilcoxon p; NEON 23, Catalina-Jemez 13, Santa Barbara 13, East River 12 |
| 4BF88462 | Results, trends | 126; 46; 6 (13 %); 4 increasing (names); 2 decreasing (names); experimental sites with trends |
| 17FB9007 | Results, trends | 166; 1 and 1; quadrants 64/60/1/1; HW flags 30/4/30 and 22/0/37; HD flags 10/2/48 and 3/16/38; 158; 29/17/112 |
| 766C0C7A, 582CE36A | Fig 3, Fig 4 captions | n_ne per quadrant (in the figures themselves) |
| 2A27A918 | Results, aridity | 1 of 166 |
| 4D1D3E96, 0CC97842, 60AB69E7 | Results, Conclusions, Discussion | AI of significant sites: 2.9, 2.5, 0.66–0.67, 1.24, 1.35, 0.32, 0.59; "four" and "six" site counts |
| 14EEC2D5, 2817434F, 1458A930 | Conclusions, Discussion | 46; 6 (13 %); 4 and 2; 71 of 166; 2 of 166; 29 greening / 17 browning of 158 |
| 40810FFA | Conclusions | 46/43/8/2 vs 40/52/4/3 %; 0.51 (22 %) vs 1.16 (64 %); p |
| 6C63D381 | Discussion | 126 → 46; 0.51 (10 of 46 arid) vs 1.16 (51 of 80); p; 61 of 80 lost sites in four domains |
| 4CB5DE8B | Discussion | 23-year; 19 years; 2020; 2 of 166; under 6 % of grid cells; 40 of 166 over 1981–2024 |
| 1922EA2C | Discussion | 3.07 km², IQR; 24/60/79 of 166; eight fill sites (five Baltimore, one Santa Barbara, two Plum Island) |
| 44BEAB20 | Discussion | 113 of 166; 33 of 113 (29 %); 64 of 85 (75 %); 29 of 158; 17; 22, 0, 16, 3 |

Also regenerate Figures 1–4 and s1–s4 from the rerun before replacing the images, and rebuild Table S1 from the new validation counts. If Trout River drops out under the order ≤ 2 filter, the "four increasing Trout Lake sites" language becomes three everywhere it appears (Abstract, 4BF88462, 4D1D3E96, 14EEC2D5, 0CC97842, 60AB69E7, 1458A930).

---

## E. Stephanie's follow-up feedback (received 2026-10-22, to tackle after the comments)

She sent three further points by message. Nic's instruction: finish the comment responses first, then take these up.

**E1. Which variables carry the trends?** "Are you conducting trends on mean annual temperature, annual total precipitation, and annual total GPP? ... at one point the text mentions trend analysis on daily data; another part mentions calculating max, min, median." This is the same confusion behind #17 and #24–#27 and is resolved by the rewrites there plus Table 1. Answer for the response letter: trends are fit to one value per site and water year: mean daily temperature, mean daily precipitation (MS) or annual total (grid), annual MOD17 GPP, and mean daily streamflow (`q_mean`). Make sure no sentence still mentions daily trends or max/min/variance once the edits are in.

**E2. Try other streamflow metrics.** "You could try other streamflow metrics (peak flows, low flows, timing, seasonal values, etc) to see if any others show more change." The pipeline already computes these per site-year (`metrics.R`: Magnificent 7, Richards-Baker flashiness, flow quantiles and their day-of-water-year timing, high-flow days, monthly and seasonal means), and `mega_zipper_data.R` already fits trends to `q_mean`, `q_ar1`, `q_rbi`, `q_q50_dowy_exceed` and `runoff_ratio` in `best_run_prisim.csv`; only `q_mean` is reported. Plan: after the rerun, tabulate the count of significant increasing / decreasing / non-significant trends for every annual, seasonal and monthly streamflow metric (non-experimental sites, same window and completeness rule), and report it as a supplementary table with a sentence in Results. If any metric shows materially more change than mean flow (low flows and timing are the likely candidates), bring it into the main text and Figure 3/4.

**E3 status (2026-10-22, 17:20): done and integrated.** The parallel session built `src/anomaly_analysis.R` (within-site anomalies, Spearman summaries, lme4 mixed models; numbers in `data_working/anomaly_numbers.md`, figure `figures/anomaly_draft.png`, draft text in `paper/anomaly_results_draft.md`). Nic approved the within-site version only; the Budyko-space variant (`src/budyko_anomaly.R`) is on hold. Integrated into `draft_modis_r1.docx`: a Methods paragraph after the Sen's-slope paragraph (cites Bates et al, 2015, added to `new_references.docx`), a new Results subsection "Interannual response of streamflow to its drivers" (two paragraphs) with Figure 5 and its caption after the Budyko paragraph, a trimmed Discussion paragraph after the biotic-buffering paragraph in "Why so little change?", one sentence in the Abstract and one in the RQ1A conclusion, and Table S3 (other streamflow metrics) at the end of the supplement. Paragraph, table and image insertions are untracked by nature; the Abstract and Conclusions sentences are tracked. Response-letter text for Stephanie's point is in `paper/anomaly_results_draft.md`. One correction to the handed-over text: the anomaly script's secondary "JJA-window" pool (13 sites with at least six complete summers but fewer than six complete water years: GGL, GREEN5, HistoryGrove, LaJaraSouthSpring, LowerLaJara, MARTINELLI, NAVAJO, SADDLE, UpperJaramillo, WALK, como, icy_brook, saw_mill_brook) is not the paper's 14 summer-only sites, so the Methods and Results sentences now describe it by that definition rather than as "summer-only sites".

**E3. Record length and a non-trend analysis.** "Since the macrosheds data are so limited in length, I am concerned that they're not super well suited to trend analysis. You could evaluate the data in different ways that do not rely on trends. For example, do years with higher than normal GPP tend to have lower than normal Q?" Plan: an anomaly analysis on the same site-years. For each site compute standardized annual anomalies (z-scores within site, or percent of site mean) of Q, P, T and GPP; then (a) within-site Spearman correlations of Q anomaly against GPP, P and T anomalies, summarized across sites and split by aridity class and NWD group; (b) a pooled mixed model of Q anomaly on P, T and GPP anomalies with site as a random intercept, which uses all site-years regardless of record length; (c) optionally the same in Budyko space (evaporative index anomaly vs aridity). This reframes the paper's conclusion from "no trends" to "how Q responds to interannual variation in drivers", which the 19-year records can support. Needs a decision on whether it becomes a Results section or a supplement, and a new figure.
