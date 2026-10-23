# Comments left for Nic to answer in `draft_modis_r1.docx`

Six of Stephanie Kampf's comments have no reply in the draft yet. For each: where it is, what she asked, what is now known, and a suggested reply you can paste or rework. Reply in Word on the comment thread (Review pane).

**Update 15:45:** the text changes listed under #16, #19 and #29 are already in the draft as tracked changes, Table 1 is inserted after the first Data sourcing paragraph (the grid PRISM product cell just says "PRISM via Google Earth Engine"), and the Aschonitis reference is in `new_references.docx`. Only the replies remain for those three. #38 and #44 wait for you to look at the rebuilt figures.

Facts found on 2026-10-22 in the MacroSheds processing code (github.com/MacroSHEDS/data_processing, `src/global/general_kernels.R` and `eml/eml_templates/methods.md`):

- MacroSheds `gpp_global_500m_median` is Earth Engine `MODIS/061/MOD17A3HGF`, band `Gpp`, 500 m, the median of pixels within each watershed boundary (an `_sd` companion is also produced). So the MacroSheds variable and our grid extraction are the same collection, 6.1.
- MacroSheds `precip_median` and `temp_median` are Earth Engine `OREGONSTATE/PRISM/AN81d`, the daily 4 km PRISM product, as the median of grid cells within each watershed.
- `pet(mm)` in the CAMELS-compliant Daymet forcings is Priestley–Taylor with the gridded alpha coefficient of Aschonitis et al. (2017), not a calibrated alpha as in CAMELS. Reference: Aschonitis, V. G., et al. (2017). High-resolution global grids of revised Priestley-Taylor and Hargreaves-Samani coefficients for assessing ASCE-standardized reference crop evapotranspiration and solar radiation. Earth System Science Data, 9(2), 615–638. https://doi.org/10.5194/essd-9-615-2017 (add to the references doc if you cite it).
- Not found anywhere in the repo: how our own grid PRISM files (`data_raw/grid_csvs/ppt.csv`, `temperature.csv`: one value per point and calendar year, 1981–2024) were extracted. There is no script for them in this repository, so the dataset (AN81m monthly summed to annual, or AN81d) has to come from whoever ran the Earth Engine export, probably Heili.

---

## Comment #16 — superseded (17:55): GPP is being recomputed on water years from the 8-day MOD17A2HGF product (`src/modis_gpp_wateryear_gee.py`); the text below is the earlier plan and no longer applies. The reply will be written once the rerun is in.

## Comment #16 (id 1359991307) — Methods, Data sourcing, paragraph beginning "Two properties of the MODIS record"

**Anchor:** "is not documented" in the last sentence, "The MOD17A3HGF collection underlying the MacroSheds variable is not documented; the grid GPP described below was extracted from Collection 6.1."

**She asked:** "implying you could not adjust the calculations to get water year values?"

**Status:** The water-year part of the paragraph was already reworded (we used the annual product as distributed; the nine-month offset does not affect the trend estimates). The collection sentence is now answerable: both are Collection 6.1.

**Text change to make (tracked):** replace "The MOD17A3HGF collection underlying the MacroSheds variable is not documented; the grid GPP described below was extracted from Collection 6.1." with "Both the MacroSheds variable and the grid GPP described below are derived from MOD17A3HGF Collection 6.1."

**Suggested reply:** "Water-year totals would require re-aggregating the 8-day MOD17A2HGF product rather than the annual product MacroSheds distributes; because the trend is fit to one value per year, the nine-month offset between calendar-year GPP and water-year streamflow does not change the slope, so we kept the annual product and say so. The collection is now stated (6.1 for both the MacroSheds variable and the grid)."

## Comment #19 (id 1569590748) — Methods, Gridded sampling

**Anchor:** "temperature and precipitation" in "For each footprint we extracted PRISM temperature and precipitation (1981–2024; PRISM Group, 2025)".

**She asked:** "time step?"

**Status:** MS sites use daily PRISM (AN81d, watershed median) aggregated to water years. The grid files hold one value per point and calendar year (annual mean temperature, annual total precipitation); the Climate analysis paragraph already says this. The source product for the grid export is the one open fact.

**Text change to make (tracked), once the product is confirmed:** "we extracted annual PRISM temperature (mean) and precipitation (total) for calendar years 1981–2024 [from the AN81m monthly product] (PRISM Group, 2025)".

**Suggested reply:** "Annual values per grid point (calendar-year mean temperature and total precipitation), extracted in Google Earth Engine; now stated here and in Table 1. MacroSheds sites use the daily PRISM product aggregated to water years."

## Comment #29 (id 656684849) — Methods, Climate analysis

**Anchor:** "daily PET" in the aridity-index sentence.

**She asked:** "calculated with what method?"

**Text change to make (tracked):** after "water-year sums of daily PET divided by water-year sums of daily precipitation" add ", with PET computed by the Priestley–Taylor method using the gridded alpha coefficients of Aschonitis et al (2017)". Add the Aschonitis reference to the references doc.

**Suggested reply:** "Priestley–Taylor, using the gridded alpha product of Aschonitis et al. (2017), as implemented in the MacroSheds CAMELS-compliant forcings; added to the text."

## Comment #31 (id 676443099) — Methods, Climate analysis

**Anchor:** "Trends and confidence intervals for all climate metrics".

**She asked:** "I think it would be helpful to have a table of all the variables considered."

**Status:** Table 1 is drafted in `paper/review_response_plan.md` (section D7) with the facts above filled in; the one open cell is the grid PRISM product. It is not in the docx yet. Insert it at the start of Data sourcing and cite it from each data paragraph.

**Suggested reply:** "Added Table 1 listing every variable with its source, resolution, time step, aggregation, window and the analysis it feeds."

## Comment #38 (id 757868304) — Figure 1

**She asked:** "maps too small to see, text in lower right is blurry."

**Status:** Fixed. Figure 1 is now built from the map objects at 7.5 in width (vector panels, 400 dpi PNG plus PDF), the grid heatmap is back (it had been missing since the geom_tile maps were introduced: the coordinate lookup returned NA for every cell), and Figures 2–4 are also exported at print width. The new images are in the draft; look at them before replying.

**Suggested reply (after the new figure is in):** "Figure 1 was rebuilt at print size with the maps drawn directly rather than embedded as images; the density panel text is now legible."

## Comment #44 (id 1943811490) — Figure 3

**She asked:** "blurry, can't read text in corners of graphs."

**Status:** Same fix: Figure 3 redrawn at print width with larger quadrant labels.

**Suggested reply (after the new figure is in):** "Figure 3 was redrawn at print width; the quadrant counts are now readable."

---

## Not a comment, but needs your sign-off

Table 2 (Methods) documents the NWD grouping rule. One combination, HDB (warming, drying, browning), is grouped with increasing NWD although browning points the other way. It does not occur in the data. Decide whether to keep it there or move it to "variable" (and change `coarse_grouping` in `data_vis/data_groups.R` to match).
