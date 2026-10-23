# Commands (Windows; run from the repo root)

- Run an R script: `"C:/Program Files/R/R-4.6.1/bin/Rscript.exe" src/metrics.R`. R is not on PATH. `Rscript -e` with pipes fails in Git Bash; put code in a file instead.
- Pipeline order and per-step I/O: table in `src/README.md` (metrics → stream_order_ms → mega_zipper_data → GEE GPP → data_vis/data_groups → grouping_histograms → p_pet_aridity → patchwork_figures → anomaly_analysis; then `data_vis/paper_numbers.R`).
- Earth Engine scripts: `python src/modis_gpp_wateryear_gee.py --project <gcp-project-id>` (needs a prior `earthengine authenticate`; `EE_PROJECT` env var also accepted). Same flags for `src/grid_modis_gpp_gee.py`.
- Re-index Serena after large code changes (Git Bash): `PATH="/c/Program Files/R/R-4.6.1/bin/x64:$PATH" "/c/Users/nic/.local/bin/serena.exe" project index .` (about 75 s for 31 files). Changes to `.serena/project.yml` need an MCP server restart.
- Git Bash heredocs mangle backslashes on this machine; keep helper scripts as files and pass paths as arguments.
