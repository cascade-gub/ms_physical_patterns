# Conventions

- Every R script starts `library(here); source(here('src', 'setup.R'))`, then loads any extra packages it needs itself (patchwork, sf, lme4, nhdplusTools, ...). Paths go through `here()`; a few legacy bare relative paths assume the repo-root working directory.
- tidyverse style: snake_case names, `<-`, `%>%` pipelines (`|>` is not used anywhere); figures built with ggplot2 and saved with `ggsave` to `figures/` (print figures 7.5 in wide, PNG 400 dpi plus PDF).
- Shared constants and switches live only in `setup.R` (analysis window, completeness thresholds, `max_stream_order`, `nhd_area_ratio_cutoff`, `gpp_source`, `seasonal_summer_path`); scripts read them and never redefine them.
- Trend flags `increasing` / `decreasing` / `non-significant` come from the Sen's slope CI (`detect_trends` then `add_flags`). Group codes: wetting W/D/-, warming H/C/-, greening G/B/-.
- Site-year tables are keyed by `site_code` + `water_year` (Oct–Sep); climate and MODIS GPP series are on water years too.
- Logging via `logger` into `log/`; `set_logger()` is guarded so sourcing `setup.R` interactively opens no log.
- Outputs go to `data_working/` (rds and csv side by side); data files are never committed.
