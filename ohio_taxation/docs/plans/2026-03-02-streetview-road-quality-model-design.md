# Streetview Road Quality Classification: PCR-Grounded Model Design

**Date**: 2026-03-02
**Author**: Saani Rawat (with Claude Code)
**Status**: Approved

## Goal

Build an accurate road quality classification model grounded in ODOT's Pavement Condition Rating (PCR) methodology, trained on RDD2024 labeled streetview images, and deploy it to predict road quality on 12,868 Ohio Google Street View photos. Benchmark predictions against actual ODOT PCR data (27,920 road segments).

## Problem Statement

The current satellite image models (ConvNeXt v2: 83.4% test accuracy, ViT: 83.9%) struggle with medium-quality roads (29% recall on test set) due to:
1. Extreme class imbalance in RoadRunner training data (88.6% high quality, 10.1% medium, 1.3% low)
2. No training recipe optimization (no class-weighted loss, no augmentation, no LR scheduling)
3. Ad-hoc quality class definitions not grounded in any engineering standard
4. RDD2024 labeled streetview data (4,800 USA images with damage annotations) is unused

## Approach: Fix the Data and Training, Not the Architecture

Keep ConvNeXt v2 architecture but overhaul everything else:
- Ground quality classes in ODOT's PCR methodology
- Convert RDD2024 bounding-box damage annotations into PCR-like scores
- Fix class imbalance with weighted loss + oversampling
- Optimize training recipe (augmentation, LR scheduling, balanced accuracy)
- Benchmark against actual ODOT PCR data

## Design

### 1. RDD2024 to PCR Score Conversion

#### 1.1 Damage Class Mapping

Map RDD2024's 10 bounding-box damage classes to ODOT's flexible pavement distress categories:

| RDD2024 Class | Description | ODOT Distress | Weight |
|---|---|---|---|
| D00 | Longitudinal crack | Longitudinal Cracking | 5 |
| D10 | Transverse crack | Block & Transverse Cracking | 10 |
| D20 | Alligator crack | Wheel Track Cracking | 15 |
| D30 | Concrete bleed | Bleeding | 5 |
| D40 | Exposed aggregate | Raveling | 10 |
| D50 | Spalling/joint deterioration | Debonding | 5 |
| D60 | Corner break/durability cracking | Block & Transverse Cracking | 10 |
| D70 | Long. & transverse crack (concrete) | Longitudinal + Block & Trans. | 5+10 |
| D80 | Patching | Patching | 5 |
| D90 | Unraveling | Raveling | 10 |

#### 1.2 Severity Estimation (from bounding box area)

Since RDD2024 lacks explicit severity labels, proxy severity from bbox area relative to image area:

| Bbox Area / Image Area | Severity | Weight (from ODOT table) |
|---|---|---|
| < 2% | Low | 0.3 - 0.4 (distress-specific) |
| 2% - 8% | Medium | 0.6 - 0.7 (distress-specific) |
| > 8% | High | 1.0 |

**Note**: These thresholds are configurable and will be tuned during benchmarking.

#### 1.3 Extent Estimation (from bbox count per damage type per image)

Multiple instances of the same damage type in one image indicate higher extent:

| Count of Same Damage Type | Extent | Weight (from ODOT table) |
|---|---|---|
| 1 | Occasional | 0.5 |
| 2-3 | Frequent | 0.7 |
| 4+ | Extensive | 1.0 |

**Note**: These thresholds are configurable and will be tuned during benchmarking.

#### 1.4 PCR Score Computation

For each image:
```
For each damage annotation:
    deduct = distress_weight × severity_weight × extent_weight
Total_deduct = sum of all deduct points
PCR = max(0, 100 - total_deduct)
```

Images with zero damage annotations receive PCR = 100.

#### 1.5 Three-Class Mapping

| Class | Label | PCR Range | Interpretation |
|---|---|---|---|
| 0 | Low quality | PCR < 50 | Poor condition, multiple/severe defects |
| 1 | Medium quality | 50 ≤ PCR < 75 | Fair condition, moderate defects |
| 2 | High quality | PCR ≥ 75 | Good condition, minimal defects |

**Note**: These cutoffs are configurable. Will be calibrated against the ODOT PCR distribution in the benchmark data (where the actual distribution of scores across road segments is known).

### 2. Model Training Pipeline

#### 2.1 Architecture

- **Model**: ConvNeXt v2 (facebook/convnextv2-base-22k-224 or current fine-tuned weights)
- **Framework**: HuggingFace Transformers
- **Input**: 224×224 RGB streetview images
- **Output**: 3-class probabilities (low/medium/high quality)
- **Training environment**: Google Colab Pro (T4/A100 GPU)

#### 2.2 Training Data

- **Source**: RDD2024 USA streetview images (~4,800 images)
- **Labels**: 3-class quality labels derived from PCR score computation (Section 1)
- **Split**: 70/15/15 train/val/test, stratified by class
- **External validation**: 483 manually-labeled Ohio images (above/below directories) — never included in training

#### 2.3 Class Imbalance Handling

- **Weighted cross-entropy loss**: Weights inversely proportional to class frequency
  - Example: if class distribution is [200, 1500, 3100], weights ≈ [15.5, 2.1, 1.0]
- **Optional oversampling**: If class 0 has very few samples, duplicate minority class images with different augmentations

#### 2.4 Data Augmentation (albumentations)

```python
augmentation_pipeline = A.Compose([
    A.RandomResizedCrop(224, 224, scale=(0.8, 1.0)),
    A.HorizontalFlip(p=0.5),
    A.ColorJitter(brightness=0.3, contrast=0.3, saturation=0.2, hue=0.1),
    A.GaussianBlur(blur_limit=3, p=0.1),
    A.Rotate(limit=15, p=0.3),
    A.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225]),
    ToTensorV2(),
])
```

#### 2.5 Training Hyperparameters

- **Optimizer**: AdamW
- **Learning rate**: 2e-5 (with warmup for 5% of steps, cosine decay to 0)
- **Weight decay**: 0.01
- **Batch size**: 16-32 (Colab-dependent)
- **Epochs**: Up to 30, with early stopping
- **Early stopping metric**: Balanced accuracy on validation set (NOT overall accuracy)
- **Gradient accumulation**: As needed for effective batch size

### 3. Ohio Streetview Prediction

#### 3.1 Target Data

- **Source**: 12,868 Google Street View photos in `data/roads/ohio/google streetview photos/`
- **Format**: 640×640 JPG images
- **Metadata in filenames**: `{FIPS}_{ROAD}_{TOWNSHIP}_{DATE}_{LAT}_{LON}_{HEADING}_{PITCH}_{FOV}.jpg`

#### 3.2 Output

Per-image predictions CSV (`ohio_streetview_preds.csv`):
```
filename, tendigit_fips, road_name, township, date, lat, lon,
pred_class, pred_label, pcr_score_pred, max_prob, p0, p1, p2
```

Where `pcr_score_pred` is a continuous 0-100 score derived from class probabilities:
```
pcr_score_pred = p0 × 25 + p1 × 62.5 + p2 × 87.5
```
(midpoints of each PCR class range)

#### 3.3 Aggregation

Roll up to `(tendigit_fips, date)` level:
- `n_images`: count of images
- `mean_pred_class`, `median_pred_class`: raw classification
- `mean_pcr_score`, `median_pcr_score`: continuous PCR score
- `mean_p0`, `mean_p1`, `mean_p2`: probability distributions

Then merge with RDD election data on `tendigit_fips`, following the same logic as `2.10_merge_naip_predictions.R`.

### 4. PCR Benchmarking

#### 4.1 Data

- **ODOT PCR data**: 27,920 road segments with 0-100 PCR scores (`data/roads/PCR/PCR.csv`)
  - Includes county codes, route types, route numbers
  - Year: 2023-2024 ratings
  - Covers County Roads (8,370), State Routes (8,062), Municipal Roads (6,754), US Routes (2,581), Township Roads (1,337)

#### 4.2 Matching Strategy

Join predicted PCR scores (from Ohio streetview images) to actual ODOT PCR scores:
- Match on geography: county code + route name/number
- Match on time: closest-date streetview image to PCR rating date
- Accept fuzzy matches where exact route matching isn't possible

#### 4.3 Validation Metrics

1. **Pearson/Spearman correlation**: predicted PCR vs. actual PCR
2. **MAE/RMSE**: absolute prediction error
3. **3-class confusion matrix**: predicted quality class vs. actual quality class (using same PCR thresholds)
4. **Scatter plot**: predicted vs. actual PCR scores with regression line

#### 4.4 Iteration Loop

If correlation is weak:
1. Adjust bbox → severity thresholds
2. Adjust bbox count → extent thresholds
3. Adjust PCR class cutoffs
4. Add stronger augmentation or adjust model hyperparameters
5. Retrain and re-evaluate

### 5. New Code Files

Following existing numbering convention:

| File | Description |
|---|---|
| `code/4.6_rdd2024_to_pcr_labels.py` | Convert RDD2024 bounding-box annotations to PCR-grounded 3-class labels |
| `code/4.7_train_streetview_model.py` | Fine-tune ConvNeXt v2 on labeled streetview data (Colab-ready) |
| `code/4.8_predict_ohio_streetview.py` | Run inference on 12,868 Ohio streetview images |
| `code/4.9_benchmark_pcr.R` | Benchmark predictions against ODOT PCR data |
| `code/2.11_merge_streetview_predictions.R` | Merge streetview predictions with RDD panel data |

### 6. Data Flow

```
RDD2024 bounding-box annotations (USA, ~4,800 images)
        │
        ▼
  4.6: PCR Score Computation ──── ODOT PCR methodology
  (bbox → severity/extent            (Flexible-Pavement-Forms.pdf)
   → deduct points → PCR)
        │
        ▼
  3-class labels (0/1/2) ◄──── Configurable thresholds (PCR < 50 / 50-74 / ≥ 75)
        │
        ▼
  4.7: Fine-tune ConvNeXt v2 ◄── Class-weighted loss, augmentation, LR schedule
  (Google Colab Pro)                 Early stopping on balanced accuracy
        │
        ▼
  4.8: Predict Ohio Streetview ──► ohio_streetview_preds.csv
  (12,868 images)
        │
        ├──► 4.9: Benchmark vs ODOT PCR (27,920 segments)
        │         → correlation, MAE, confusion matrix
        │         → iterate if weak
        │
        └──► 2.11: Merge with RDD election data
                    → panel dataset
                    → causal analysis (RDD/DID/Event Study)
```

### 7. Success Criteria

1. **Test accuracy ≥ 88%** (up from 83%) with **medium-quality recall ≥ 60%** (up from 29%)
2. **Pearson correlation ≥ 0.5** between predicted PCR and actual ODOT PCR scores
3. Predictions pass sanity checks: known bad roads score lower, known good roads score higher
4. RDD analysis with streetview predictions shows cleaner treatment effects than current models

### 8. Key Design Decisions

- **Streetview only**: No mixing of satellite and streetview images in training data. Clean domain separation.
- **PCR-grounded labels**: Quality classes defined by ODOT's PCR methodology, not ad-hoc thresholds.
- **Configurable thresholds**: All severity, extent, and class boundary thresholds are parameterized for iteration.
- **External benchmark**: ODOT PCR data provides independent validation, publishable as a benchmarking exercise.
- **ConvNeXt v2 retained**: Architecture is adequate; the bottleneck is data quality and training recipe.
