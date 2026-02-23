# NAIP Road Quality Prediction & Analysis Pipeline

**Date:** 2026-02-22
**Author:** Saani Rawat (with Claude Code)

## Overview

Use fine-tuned AI vision models (ConvNeXt v2 and YOLOv11) to predict road quality from NAIP satellite images collected via Google Earth Engine. Merge predictions with the RDD dataset to estimate the causal effect of road maintenance tax cuts on road quality using Difference-in-Differences, event-study, and Regression Discontinuity designs.

## Decisions

- **Models:** Both ConvNeXt v2 and YOLOv11 (robustness check)
- **Group assignment:** Merge manifest with `roads_and_census.dta` on `COSBIDFP00 == tendigit_fips`
- **Time structure:** Panel (subdivision x NAIP year) for event-study, plus collapsed pre/post for simple DID
- **Scoring:** Both existing formula (`((pred_id + confidence) / 3) * 99 + 1`) and expected value (`p0*0 + p1*1 + p2*2`)
- **Constraint:** No original data files are modified or overwritten

## Data Flow

```
satellite_images/          4.5_predict_naip_*.py        2.10_merge_naip_*.R         3.13_naip_road_quality_*.R
  *.jpg + manifest.csv  ->  ConvNeXt v2 + YOLOv11    ->  Join roads_and_census.dta ->  RDD + Event Study + DID
                            |                            |                            |
                         naip_preds_convnext.csv      naip_road_quality_panel.csv   Tables, plots, rdrobust
                         naip_preds_yolo.csv
```

## New Files

| File | Type | Purpose |
|------|------|---------|
| `code/4.5_predict_naip_satellite_images.py` | Python | Run ConvNeXt v2 + YOLOv11 on NAIP images |
| `code/2.10_merge_naip_predictions.R` | R | Merge predictions with RDD data, create panel |
| `code/3.13_naip_road_quality_analysis.R` | R | DID, event-study, RDD analysis + plots |

---

## Script 1: `4.5_predict_naip_satellite_images.py`

### Inputs
- Images: `data/roads/satellite_images/*.jpg`
- Manifest: `data/roads/satellite_images/satellite_images_manifest.csv` (columns: `cosbidfp, namelsad, roadname, year, lat, lon, filename`)
- ConvNeXt v2: `data/roads/hf_finetuned_convnextv2/` (model.safetensors, config.json, preprocessor_config.json; image_size=224, ImageNet normalization)
- YOLOv11: `data/roads/runs_ohio/yolo11_finetune_satellite_images/yolo11n_cls_roads/weights/best.pt` (224px, 3 classes)

### Processing
- Load manifest CSV to get image list
- For each model, iterate through all images, resize to 224x224, predict class (0/1/2) + probabilities (p0, p1, p2)
- ConvNeXt: HuggingFace `transformers` + `ConvNextV2ForImageClassification` with saved preprocessor config
- YOLOv11: `ultralytics` YOLO classifier (same pattern as existing 4.4)
- Batch processing with progress logging, resume capability (skip images already in output CSV)
- GPU if available, fallback to CPU
- Environment: `geoai-mf` conda env

### Outputs (saved to `data/roads/satellite_images/`)
- `naip_preds_convnext.csv` -- columns: `filename, cosbidfp, year, lat, lon, roadname, pred_id, pred_label, max_prob, p0, p1, p2`
- `naip_preds_yolo.csv` -- same columns

---

## Script 2: `2.10_merge_naip_predictions.R`

### Inputs
- `naip_preds_convnext.csv` and `naip_preds_yolo.csv` (from 4.5)
- `roads_and_census.dta` (read-only; contains `tendigit_fips`, `year`, `votes_pct_against`, `treated`, `pop`, covariates)

### Processing
1. Load both prediction CSVs
2. Compute road quality scores using both methods:
   - Method A (3.11 formula): `score = ((pred_id + max_prob) / 3) * 99 + 1`
   - Method B (expected value): `score_ev = p0*0 + p1*1 + p2*2`
3. Aggregate to subdivision-year level: mean/median scores per `cosbidfp x year`
4. Join to `roads_and_census.dta` on `cosbidfp == tendigit_fips` to get `election_year`, `votes_pct_against`, `treated`, `pop`
5. Create temporal variables:
   - `post_election_flag`: 1 if NAIP year > election_year
   - `event_time`: NAIP year - election_year
   - `did`: `post_election_flag * treated`
6. Filter: keep subdivisions with at least 1 pre and 1 post-election NAIP year

### Outputs (saved to `data/roads/satellite_images/`)
- `naip_road_quality_panel_convnext.csv` -- subdivision x NAIP year panel
- `naip_road_quality_panel_yolo.csv` -- same for YOLOv11
- `naip_road_quality_collapsed.csv` -- pre/post averages for simple DID

---

## Script 3: `3.13_naip_road_quality_analysis.R`

### Inputs
- Panel and collapsed CSVs from 2.10

### Analyses

1. **Summary statistics**: Group means table (treatment x pre/post) for all scoring methods, both models
2. **Simple DID** (collapsed): `road_quality ~ post_election_flag * treated`, clustered by `tendigit_fips`, using `fixest::feols()`
3. **Event-study** (panel): `road_quality ~ i(event_time, treated, ref = -1) | tendigit_fips`, clustered, with `fixest::iplot()` for the coefficient plot
4. **RDD** (post-election only): `rdrobust(y = road_quality, x = votes_pct_against, c = 50)` with kernel, MSERD bandwidth, pop covariate, clustering
5. **Model comparison**: Side-by-side table (ConvNeXt v2 vs YOLOv11)

### Outputs (saved to `data/outputs/`)
- `tables/naip_road_quality_did.tex`
- `tables/naip_road_quality_rd.tex`
- `tables/naip_road_quality_model_comparison.tex`
- `plots/naip_event_study_road_quality.png`
- `plots/naip_rd_plot_road_quality.png`
- `plots/naip_road_quality_lineplot.png`
- `plots/naip_road_quality_barplot.png`
