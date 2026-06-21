# Ohio Taxation Pending Work Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Finish, verify, and package the active `ohio_taxation` research work without mixing it with generated artifacts, sibling-project changes, or archival drafts.

**Architecture:** Treat the repository as five workstreams: road-quality data pipelines, streetview detector calibration, budget-spending mechanism data, paper/draft integration, and repository hygiene. Each workstream must leave behind explicit outputs and a verification readout before it is staged or used in the paper.

**Tech Stack:** Python, R 4.6.0, Stata project data, LaTeX/MiKTeX, Ultralytics YOLO, HuggingFace/torch, rdrobust/fixest/tidyverse.

---

## Current Evidence

- Git root is `C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects`, so repo commands from `ohio_taxation` can see sibling-project changes. Scope commands to `ohio_taxation` unless the task explicitly crosses projects.
- `data/roads/satellite_images` has 55,562 NAIP JPGs and both prediction CSVs have 55,562 rows.
- NAIP merged outputs exist: 7,378 rows each in `naip_road_quality_panel_convnext.csv` and `naip_road_quality_panel_yolo.csv`, 2,628 rows in `naip_road_quality_collapsed.csv`, and 4,790 rows in `naip_road_quality_collapsed_delayed.csv`.
- Stacked road-quality outputs exist from the previous run, but the current `code/3.14_stacked_event_road_quality_analysis.R` now writes `road_quality_event_sample_*_analysis.csv`; those new files are missing, so the script has not been rerun after the filename and RD coefficient-index changes.
- Streetview detector prepared dataset exists at `data/roads/N-RDD2024Road damage and defects - v5/Training and Validation Dataset/USA_yolo_detection_with_clean_negatives` with 4,898 manifest rows.
- Streetview prediction folders each have 12,512 label files and 12,512 summary rows. The latest visible run is `predict_ohio_conf10_annotated_no_negs`, paired with weights in `streetview_det_yolo26l_colab_no_negs_v1`.
- Streetview county PCR calibration has weak validation: learned in-sample Pearson 0.147, learned LOOCV Pearson -0.196; threshold-grid Pearson tops out around 0.060.
- Python syntax check passed for `code/1.2_pcr_streetview_images.py`, `code/1.72_import_satellite_images_osip.py`, `code/4.5_predict_naip_satellite_images.py`, `code/4.51_predict_osip3_satellite_images.py`, `code/4.6_prep_streetview_model_data.py`, `code/4.7_train_streetview_model.py`, and `code/4.8_predict_ohio_streetview.py`.
- R parse check passed using `C:/Program Files/R/R-4.6.0/bin/Rscript.exe` for `code/2.10_merge_naip_predictions.R`, `code/2.11_merge_satellite_predictions_stacked_event.R`, `code/3.13_naip_road_quality_analysis.R`, and `code/3.14_stacked_event_road_quality_analysis.R`.
- `docs/JMP_draft/JMP-Article.pdf` builds, but the log still has overfull boxes and two float-too-large warnings. `docs/pub_capital_dep_draft/Article_v2.pdf` also builds with layout warnings.

## Task 1: Repository Scope And Artifact Hygiene

**Files:**
- Inspect: `.vscode/settings.json`
- Inspect: `Rplots.pdf`
- Inspect: `bw_sensitivity_of_means.png`
- Inspect: `yolo11n-cls.pt`
- Inspect: `code/__pycache__/`
- Inspect: `runs/detect/`
- Inspect: `docs/Biasi_Rep_draft/JAE cover letter/`
- Inspect: `docs/pub_capital_dep_draft/`

- [ ] **Step 1: Capture scoped status**

Run:
```powershell
git status --short -- ohio_taxation
git diff --name-status -- ohio_taxation
```

Expected: Output limited to `ohio_taxation` paths. Sibling changes such as `ohio_employment` are ignored for this workstream.

- [ ] **Step 2: Classify every dirty path**

Create a keep/delete/generated/archive classification for:
```text
.vscode/settings.json
Rplots.pdf
bw_sensitivity_of_means.png
code/__pycache__/
runs/detect/
yolo11n-cls.pt
docs/Biasi_Rep_draft/JAE cover letter/
docs/pub_capital_dep_draft/
```

Expected: No generated artifact is staged unless it is a deliberate paper output.

- [ ] **Step 3: Update ignore rules only after classification**

If generated model/cache files should stay untracked, update `.gitignore` at the Git root with scoped patterns such as:
```gitignore
ohio_taxation/code/__pycache__/
ohio_taxation/runs/
ohio_taxation/*.pt
ohio_taxation/Rplots.pdf
```

Expected: `git status --short -- ohio_taxation` becomes easier to read without hiding source files or paper deliverables.

## Task 2: Finish NAIP/OSIP3 Stacked Road-Quality Pipeline

**Files:**
- Modify if needed: `code/2.11_merge_satellite_predictions_stacked_event.R`
- Modify if needed: `code/3.14_stacked_event_road_quality_analysis.R`
- Verify: `data/roads/stacked_event_road_quality/`
- Verify: `data/outputs/tables/road_quality_*.tex`
- Verify: `data/outputs/plots/road_quality_*.png`

- [ ] **Step 1: Rerun the merge script**

Run:
```powershell
& 'C:/Program Files/R/R-4.6.0/bin/Rscript.exe' code/2.11_merge_satellite_predictions_stacked_event.R
```

Expected: The script writes fresh `road_quality_image_predictions.csv`, `road_quality_subdivision_year_panel.csv`, `road_quality_event_time_panel.csv`, and event-sample CSVs in `data/roads/stacked_event_road_quality/`.

- [ ] **Step 2: Rerun the stacked RD analysis**

Run:
```powershell
& 'C:/Program Files/R/R-4.6.0/bin/Rscript.exe' code/3.14_stacked_event_road_quality_analysis.R
```

Expected: The new output names introduced in the current diff exist:
```text
data/roads/stacked_event_road_quality/road_quality_event_sample_convnext_analysis.csv
data/roads/stacked_event_road_quality/road_quality_event_sample_yolo_analysis.csv
```

- [ ] **Step 3: Verify coefficient indexing**

Check `road_quality_table5_rolling_windows.csv` and `road_quality_dynamic_rd.csv` after rerun:
```powershell
Import-Csv data/roads/stacked_event_road_quality/road_quality_table5_rolling_windows.csv | Select-Object -First 10
Import-Csv data/roads/stacked_event_road_quality/road_quality_dynamic_rd.csv | Format-Table
```

Expected: Estimates use the robust/local-linear coefficient consistently with the `se`, `pval`, and confidence interval columns from `rdrobust`.

## Task 3: Finish Streetview Detector And PCR Calibration

**Files:**
- Modify: `code/4.6_prep_streetview_model_data.py`
- Modify: `code/4.7_train_streetview_model.py`
- Modify: `code/4.8_predict_ohio_streetview.py`
- Modify: `code/1.2_pcr_streetview_images.py`
- Verify: `data/roads/runs_ohio/yolo11_rdd2024_streetview_detector/`

- [ ] **Step 1: Standardize current run names**

Make the scripts or a small config file point to the current best observed run:
```text
training run: streetview_det_yolo26l_colab_no_negs_v1
prediction run: predict_ohio_conf10_annotated_no_negs
```

Expected: `code/4.8_predict_ohio_streetview.py` no longer defaults to a missing `streetview_det_yolo11m` run when the current weights are in `streetview_det_yolo26l_colab_no_negs_v1`.

- [ ] **Step 2: Reconcile the stale TODO in PCR scoring**

In `code/1.2_pcr_streetview_images.py`, update the comment that says grid search "DOES NOT WORK YET" because grid search and learned calibration now write outputs. Add a short result note that current county-level validation is weak.

Expected: The code comments match the actual outputs and do not overstate calibration quality.

- [ ] **Step 3: Decide whether county-level calibration is publishable**

Use the current evidence:
```text
learned_in_sample Pearson: 0.147
learned_loocv Pearson: -0.196
best threshold-grid Pearson: 0.060
```

Expected: Either improve the match by moving below county-level aggregation or present this as a limitation rather than a main validation result.

- [ ] **Step 4: Run the streetview scoring script after any changes**

Run:
```powershell
python code/1.2_pcr_streetview_images.py
```

Expected: It rewrites `pcr_detections_long.csv`, `threshold_grid_search_results.csv`, `county_pcr_compare_best_thresholds.csv`, `learned_calibration_metrics.csv`, and `county_pcr_compare_learned_calibration.csv` under the selected prediction run folder.

## Task 4: Finish Auditor Report Budget Mechanism Data

**Files:**
- Modify: `code/1.0_download_spending_reports.py`
- Modify: `code/1.1_parse_spending_reports.py`
- Modify: `code/2.6_local_govt_road_spending.R`
- Modify if needed: `code/3.9_road_new_levy_and_reports_analysis.R`
- Modify if needed: `code/3.9_road_renewal_levy_and_reports_analysis.r`
- Integrate: `docs/JMP_draft/results.tex`

- [ ] **Step 1: Resolve downloader remaining cases**

Start from the existing note in `code/1.0_download_spending_reports.py` about failed PDF-link clicking. Produce a small CSV of failed municipality-year report downloads.

Expected: The missing-report set is explicit and reproducible.

- [ ] **Step 2: Parse road-spending line items**

Extend `code/1.1_parse_spending_reports.py` so the parser emits at least:
```text
subdivision identifier
fiscal year
report source file
road maintenance line item text
road maintenance amount
total road-related spending amount
parser confidence flag
manual review flag
```

Expected: The parser output can support the TAFT/JMP mechanism claim about how much road spending declines after failed renewals.

- [ ] **Step 3: Merge budget data into the event/RD panel**

Update `code/2.6_local_govt_road_spending.R` or a new narrowly named merge script only if the current file is already too broad.

Expected: The paper can report budget trajectories at `t`, `t+3`, `t+5`, and `t+10` or explain where coverage is insufficient.

## Task 5: Paper Draft Integration And Layout QA

**Files:**
- Modify: `docs/JMP_draft/data.tex`
- Modify: `docs/JMP_draft/results.tex`
- Modify: `docs/JMP_draft/appendix.tex`
- Modify if needed: `docs/JMP_draft/JMP-Article.tex`
- Verify: `docs/JMP_draft/JMP-Article.pdf`

- [ ] **Step 1: Verify significance stars**

For every manually inserted star in `results.tex` and `appendix.tex`, confirm it matches the reported p-value.

Expected:
```text
* for p < 0.10
** for p < 0.05
*** for p < 0.01
no star for p >= 0.10
```

- [ ] **Step 2: Fix layout warnings that affect readability**

Prioritize the current log warnings:
```text
Float too large for page by 36.46622pt
Float too large for page by 57.21585pt
Overfull hbox by 81.14139pt
Overfull hbox by 213.62677pt
```

Expected: No large overfull boxes or too-large floats remain in the final log.

- [ ] **Step 3: Rebuild the paper**

Run from `docs/JMP_draft`:
```powershell
latexmk -pdf -interaction=nonstopmode -halt-on-error JMP-Article.tex
```

Expected: `JMP-Article.pdf` is rebuilt successfully and the final log has no errors or unresolved references.

## Task 6: Decide Scope Of DGE Model Improvements

**Files:**
- Inspect: `code/5.1_roads_model_dge_new.jl`
- Inspect: `code/5.2_roads_model_dge_house.py`
- Inspect: `docs/JMP_draft/model.tex`

- [ ] **Step 1: Resolve or explicitly defer the intertemporal-capital TODO**

The current Julia file notes that richer transitional dynamics require Euler-enforced capital accumulation.

Expected: Either implement the richer model and regenerate figures, or add a clear modeling-scope note explaining that the current version keeps dynamics driven by road quality.

- [ ] **Step 2: Verify model figure provenance**

Confirm that all model figures included by `docs/JMP_draft/model.tex` are generated by the current model scripts.

Expected: Each included model figure can be traced to a script and command.

## Task 7: Triage Parallel Drafts And Archival Material

**Files:**
- Inspect: `docs/Biasi_Rep_draft/JAE cover letter/`
- Inspect: `docs/pub_capital_dep_draft/`
- Inspect: `docs/AEJEP_draft/`
- Inspect: `docs/EJ_draft/`

- [ ] **Step 1: Decide whether Biasi cover-letter rename is intentional**

The old tracked `docs/Biasi_Rep_draft/cover letter/` paths are deleted and `docs/Biasi_Rep_draft/JAE cover letter/` is untracked.

Expected: Either commit the rename intentionally or keep it out of this workstream.

- [ ] **Step 2: Decide whether `pub_capital_dep_draft` belongs in this repo**

This new untracked draft has a built `Article_v2.pdf` and layout warnings.

Expected: Either add it as a separate paper subproject with its own plan, or move it out of the active `ohio_taxation` commit scope.

## Task 8: Final Reproducibility Package

**Files:**
- Create or modify: `README.md`
- Create or modify: `docs/plans/README.md` or `docs/superpowers/plans/README.md`
- Create if useful: `code/run_road_quality_pipeline.ps1`

- [ ] **Step 1: Write a minimal runbook**

Document the exact order:
```text
4.5 -> 2.10 -> 3.13 for NAIP-only analysis
1.7/1.71/4.51 -> 2.11 -> 3.14 for stacked NAIP/OSIP3 analysis
4.6 -> 4.7 -> 4.8 -> 1.2 for streetview detector and PCR calibration
1.0 -> 1.1 -> 2.6/3.9 for auditor report budget mechanism
latexmk JMP-Article.tex for paper build
```

Expected: A future worker can reproduce the active outputs without reverse-engineering script order from filenames.

- [ ] **Step 2: Run final verification**

Run:
```powershell
python -m py_compile code\1.2_pcr_streetview_images.py code\1.72_import_satellite_images_osip.py code\4.5_predict_naip_satellite_images.py code\4.51_predict_osip3_satellite_images.py code\4.6_prep_streetview_model_data.py code\4.7_train_streetview_model.py code\4.8_predict_ohio_streetview.py
& 'C:/Program Files/R/R-4.6.0/bin/Rscript.exe' -e "files <- c('code/2.10_merge_naip_predictions.R','code/2.11_merge_satellite_predictions_stacked_event.R','code/3.13_naip_road_quality_analysis.R','code/3.14_stacked_event_road_quality_analysis.R'); for (f in files) { parse(file=f); cat(f, 'OK\n') }"
```

Expected: Python syntax and R parse checks pass before any commit.

