# ms_physical_patterns — core

R analysis pipeline for the headwater streamflow manuscript (MacroSheds data: "Stability of Headwater Streamflow Under ... Climate Change"). The `src/nitrogen_*.R` scripts belong to a separate (nitrogen) manuscript and share the folder.

Source map:
- `src/` — pipeline scripts plus `setup.R` (packages, paths, constants, shared helpers `frequency_check`, `reduce_to_best_range`, `detect_trends`, `add_flags`, `headwater_sites`). `src/README.md` is the authoritative pipeline doc: run order, each script's inputs/outputs, every setup.R constant. Read it before touching the pipeline.
- `data_vis/` — figure and numbers scripts: `data_groups.R` (NWD groupings, trend maps), `grouping_histograms.R`, `paper_numbers.R` (writes `data_working/paper_numbers.md`, the source of every count quoted in the paper), GPP validation scripts.
- `paper/` — manuscript .docx files, review-response plan and handoff notes; edited with docx tooling, not from R.
- `data_raw/`, `data_working/`, `figures/`, `log/` — gitignored and regenerated locally; invisible to Serena (`ignore_all_files_in_gitignore`), use shell/grep for them.
- `src/archived/`, `*/defunct - remove before pub/` — dead code, excluded from the Serena index on purpose.

Invariants:
- All trends are fit on water years `analysis_start_year`..`analysis_end_year` (2001–2023) from `setup.R`.
- Every script sources `src/setup.R` and must run from the repo root.
- Numbers in the draft come from `data_working/paper_numbers.md`; regenerate it after any pipeline change.

Further: `mem:tech_stack` (R/Python versions, key packages, data layout), `mem:suggested_commands` (R executable path, GEE scripts, Serena re-index), `mem:conventions` (script skeleton, naming, where constants live), `mem:task_completion` (what to run before calling a change done).
