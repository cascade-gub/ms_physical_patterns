# Task completion

No test suite, linter or formatter exists.

A change is done when:
1. The affected pipeline step(s) rerun with Rscript (see `mem:suggested_commands`), exit code 0, expected outputs present in `data_working/` / `figures/`.
2. If any site count, flag count or statistic can change: downstream steps rerun through `data_vis/paper_numbers.R`, and `data_working/paper_numbers.md` compared against the numbers in the draft.
3. `src/README.md` updated for new scripts, inputs/outputs or `setup.R` constants.
4. Nothing committed — the user commits.
