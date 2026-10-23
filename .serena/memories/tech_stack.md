# Tech stack

- R 4.6.1 at `C:/Program Files/R/R-4.6.1/bin/` — NOT on PATH. No renv/DESCRIPTION; packages live in the user library. Key packages: tidyverse, here, logger, feather, sf, zyp (Sen's slope + CI), trend, lfstat, e1071, macrosheds (MacroSheds v2 loaders), nhdplusTools (NHDPlus stream order), ggplot2 + patchwork/ggtext/ggrepel/viridis (figures), lme4/lmerTest (anomaly mixed models).
- Python 3.14 (system install) for the two Earth Engine scripts in `src/` (`earthengine-api`, pandas); no requirements file.
- Data: MacroSheds v2 under `data_raw/ms/v2/` (feather files); PRISM and MODIS reference-grid CSVs under `data_raw/grid_csvs/`; add-on extracts under `data_raw/ms_add_ons/`.
- Serena: R language server (CRAN `languageserver` package); symbol cache in `.serena/cache/r` (gitignored); `.serena/project.yml` is tracked.
