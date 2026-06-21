# EER Replication Package Notes

## Required Software

- Stata 16 or newer. Verified locally with StataNow 19.5 BE.
- Stata packages: `reghdfe`, `estout`, `texdoc`, `coefplot`, `rdrobust`, `distinct`.
- R 4.6 or newer.
- R packages: `haven`, `dplyr`, `ggplot2`.
- LaTeX via MiKTeX or another TeX Live distribution.

## Main Scripts

- `../biasi2025/analysis/eer_preflight.do`: checks Stata packages and input files.
- `../biasi2025/analysis/eer_state_sample_audit.do`: creates the state sample audit.
- `../biasi2025/analysis/eer_state_mechanisms.do`: creates descriptive state mechanism tables and figures.
- `../biasi2025/analysis/eer_bandwidth_sensitivity.do`: estimates full-sample and restricted-margin robustness checks.
- `../biasi2025/analysis/eer_driver.do`: runs the full EER analysis workflow.
- `../biasi2025/R/eer_state_mechanism_plots.R`: regenerates mechanism plots from the Stata output.

## Manuscript Build

From `docs/Biasi_Rep_draft_EER`:

```powershell
latexmk -pdf -interaction=nonstopmode -halt-on-error Biasi-Rep-Article-EER.tex
latexmk -pdf -interaction=nonstopmode -halt-on-error eer_cover_letter.tex
```

## Output Locations

- New EER tables: `../biasi2025/tables/eer/`
- New EER figures: `../biasi2025/figures/eer/`
- Intermediate EER datasets: `../biasi2025/datasets/eer_*.dta`
