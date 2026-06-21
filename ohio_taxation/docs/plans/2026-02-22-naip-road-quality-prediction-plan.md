# NAIP Road Quality Prediction Pipeline - Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Run fine-tuned ConvNeXt v2 and YOLOv11 vision models on NAIP satellite images of Ohio roads, merge predictions with the RDD dataset, and run causal analyses (DID, event-study, RDD) to estimate the effect of road maintenance tax cuts on road quality.

**Architecture:** Three modular scripts following the project's numbering convention: 4.5 (Python prediction), 2.10 (R data merge), 3.13 (R analysis). Each script reads from the previous step's output. No original data files are modified.

**Tech Stack:** Python (transformers, ultralytics, torch, PIL, pandas), R (fixest, rdrobust, haven, tidyverse, ggplot2)

---

## Task 1: Create `4.5_predict_naip_satellite_images.py` - ConvNeXt v2 inference

**Files:**
- Create: `code/4.5_predict_naip_satellite_images.py`

**Context:**
- The fine-tuned ConvNeXt v2 model lives at `data/roads/hf_finetuned_convnextv2/` with `model.safetensors`, `config.json`, `preprocessor_config.json`
- The model expects 224x224 RGB images, ImageNet normalization (mean=[0.485,0.456,0.406], std=[0.229,0.224,0.225])
- It outputs 3 classes: 0 (poor), 1 (decent), 2 (high quality)
- Reference inference code is in `code/4.1_roads_ai_model_oh.jl` lines 63-173 (Python embedded in Julia via PyCall)
- The NAIP images are at `data/roads/satellite_images/*.jpg` with a manifest at `data/roads/satellite_images/satellite_images_manifest.csv`
- Manifest columns: `cosbidfp, namelsad, roadname, year, lat, lon, filename`
- Environment: `C:/Users/rawatsa/AppData/Local/miniforge3/envs/geoai-mf/python.exe`

**Step 1: Write the script header, imports, and configuration**

```python
#==========================================================================#
# File: 4.5_predict_naip_satellite_images.py
# Author: Saani Rawat
# Date: 22 Feb 2026
# Description: Run ConvNeXt v2 and YOLOv11 inference on NAIP satellite
#              images to predict road quality (0=poor, 1=decent, 2=high).
#
# Dependencies:
#   - transformers, torch, ultralytics, PIL, pandas
#   - Fine-tuned ConvNeXt v2: data/roads/hf_finetuned_convnextv2/
#   - Fine-tuned YOLOv11: data/roads/runs_ohio/.../weights/best.pt
#   - NAIP images + manifest: data/roads/satellite_images/
#
# Usage:
#   python 4.5_predict_naip_satellite_images.py
#   Uses geoai-mf conda env.
#==========================================================================#

from __future__ import annotations
from pathlib import Path
import csv
import os
import sys
import time
import logging
from collections import Counter

import numpy as np
import torch
from PIL import Image
from transformers import AutoImageProcessor, AutoModelForImageClassification
from ultralytics import YOLO

# ---- Configuration --------------------------------------------------------

ROOT = Path("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation")
DATA_DIR = ROOT / "data" / "roads"
SAT_DIR = ROOT / "data" / "roads" / "satellite_images"
MANIFEST_CSV = SAT_DIR / "satellite_images_manifest.csv"

# ConvNeXt v2 model
CONVNEXT_MODEL_DIR = DATA_DIR / "hf_finetuned_convnextv2"

# YOLOv11 model
YOLO_WEIGHTS = DATA_DIR / "runs_ohio" / "yolo11_finetune_satellite_images" / "yolo11n_cls_roads" / "weights" / "best.pt"

# Output prediction CSVs (saved alongside images)
CONVNEXT_OUT_CSV = SAT_DIR / "naip_preds_convnext.csv"
YOLO_OUT_CSV = SAT_DIR / "naip_preds_yolo.csv"

# Class mapping
CLASS_NAMES = {0: "low_quality", 1: "medium_quality", 2: "high_quality"}

# Inference settings
BATCH_SIZE = 32
IMGSZ = 224
PRINT_EVERY = 200

DEVICE = "cuda" if torch.cuda.is_available() else "cpu"

# Logging
LOG_PATH = SAT_DIR / "prediction_run.log"
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.FileHandler(LOG_PATH),
        logging.StreamHandler(sys.stdout),
    ],
)
```

**Step 2: Write manifest loading and image listing functions**

```python
# ---- Load manifest --------------------------------------------------------

def load_manifest(manifest_path: Path) -> list[dict]:
    """Load the satellite images manifest CSV. Returns list of row dicts."""
    rows = []
    with open(manifest_path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            img_path = SAT_DIR / row["filename"]
            if img_path.exists():
                row["img_path"] = str(img_path)
                rows.append(row)
    logging.info(f"Loaded {len(rows)} images from manifest (with existing files).")
    return rows


def load_already_predicted(out_csv: Path) -> set[str]:
    """Load filenames already predicted (for resume capability)."""
    if not out_csv.exists():
        return set()
    done = set()
    with open(out_csv, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            done.add(row["filename"])
    logging.info(f"  Resume: {len(done)} images already predicted in {out_csv.name}")
    return done
```

**Step 3: Write ConvNeXt v2 inference function**

This follows the same pattern as `code/4.1_roads_ai_model_oh.jl` lines 80-169 but in pure Python.

```python
# ---- ConvNeXt v2 inference ------------------------------------------------

def run_convnext_inference(manifest_rows: list[dict], out_csv: Path):
    """
    Run ConvNeXt v2 classification on all NAIP images.
    Writes results to out_csv with columns:
      filename, cosbidfp, year, lat, lon, roadname, pred_id, pred_label, max_prob, p0, p1, p2
    """
    logging.info("=" * 70)
    logging.info("ConvNeXt v2 INFERENCE")
    logging.info("=" * 70)

    # Load model and processor from saved fine-tuned weights
    processor = AutoImageProcessor.from_pretrained(str(CONVNEXT_MODEL_DIR))
    model = AutoModelForImageClassification.from_pretrained(str(CONVNEXT_MODEL_DIR)).to(DEVICE)
    model.eval()
    logging.info(f"  Loaded ConvNeXt v2 from {CONVNEXT_MODEL_DIR}")
    logging.info(f"  Device: {DEVICE}")

    # Resume: skip already-predicted images
    done_filenames = load_already_predicted(out_csv)
    todo_rows = [r for r in manifest_rows if r["filename"] not in done_filenames]
    logging.info(f"  Images to predict: {len(todo_rows)} (skipping {len(done_filenames)} already done)")

    if not todo_rows:
        logging.info("  Nothing to do.")
        return

    # Open CSV in append mode
    write_header = not out_csv.exists() or out_csv.stat().st_size == 0
    counts = Counter()
    n_done = 0

    with open(out_csv, "a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        if write_header:
            writer.writerow(["filename", "cosbidfp", "year", "lat", "lon",
                             "roadname", "pred_id", "pred_label", "max_prob",
                             "p0", "p1", "p2"])

        # Process in batches
        for batch_start in range(0, len(todo_rows), BATCH_SIZE):
            batch_rows = todo_rows[batch_start : batch_start + BATCH_SIZE]

            # Load and preprocess images
            images = []
            valid_rows = []
            for row in batch_rows:
                try:
                    img = Image.open(row["img_path"]).convert("RGB")
                    images.append(img)
                    valid_rows.append(row)
                except Exception as e:
                    logging.warning(f"  Could not open {row['filename']}: {e}")

            if not images:
                continue

            # Run inference
            inputs = processor(images=images, return_tensors="pt").to(DEVICE)
            with torch.no_grad():
                logits = model(**inputs).logits
                probs = torch.softmax(logits, dim=-1).cpu().numpy()

            preds = probs.argmax(axis=1)
            max_probs = probs.max(axis=1)

            for i, row in enumerate(valid_rows):
                pred_id = int(preds[i])
                pred_label = CLASS_NAMES.get(pred_id, str(pred_id))
                writer.writerow([
                    row["filename"], row["cosbidfp"], row["year"],
                    row["lat"], row["lon"], row["roadname"],
                    pred_id, pred_label, f"{float(max_probs[i]):.6f}",
                    f"{float(probs[i, 0]):.6f}",
                    f"{float(probs[i, 1]):.6f}",
                    f"{float(probs[i, 2]):.6f}",
                ])
                counts[pred_label] += 1

            n_done += len(valid_rows)
            if n_done % PRINT_EVERY < BATCH_SIZE:
                logging.info(f"  [{n_done}/{len(todo_rows)}] processed")

        # Flush
        f.flush()

    logging.info(f"  ConvNeXt v2 done: {n_done} images predicted")
    logging.info(f"  Class distribution: {dict(counts)}")
    logging.info(f"  Output: {out_csv}")
```

**Step 4: Write YOLOv11 inference function**

This follows the same pattern as `code/4.4_yolo11_predict_satellite_images_ohio.py`.

```python
# ---- YOLOv11 inference ----------------------------------------------------

def probs_to_numpy(probs_obj) -> np.ndarray:
    """Convert Ultralytics probs object to a 1D numpy array."""
    data = getattr(probs_obj, "data", None)
    if data is None:
        if hasattr(probs_obj, "numpy"):
            return np.array(probs_obj.numpy(), dtype=float).reshape(-1)
        if hasattr(probs_obj, "tolist"):
            return np.array(probs_obj.tolist(), dtype=float).reshape(-1)
        raise TypeError("Could not extract probabilities from result.probs")
    if hasattr(data, "detach"):
        data = data.detach()
    if hasattr(data, "cpu"):
        data = data.cpu()
    if hasattr(data, "numpy"):
        return np.array(data.numpy(), dtype=float).reshape(-1)
    return np.array(data, dtype=float).reshape(-1)


def run_yolo_inference(manifest_rows: list[dict], out_csv: Path):
    """
    Run YOLOv11 classification on all NAIP images.
    Same output format as ConvNeXt.
    """
    logging.info("=" * 70)
    logging.info("YOLOv11 INFERENCE")
    logging.info("=" * 70)

    if not YOLO_WEIGHTS.exists():
        logging.error(f"  YOLO weights not found: {YOLO_WEIGHTS}")
        return

    model = YOLO(str(YOLO_WEIGHTS))
    logging.info(f"  Loaded YOLOv11 from {YOLO_WEIGHTS}")

    # Resume
    done_filenames = load_already_predicted(out_csv)
    todo_rows = [r for r in manifest_rows if r["filename"] not in done_filenames]
    logging.info(f"  Images to predict: {len(todo_rows)} (skipping {len(done_filenames)} already done)")

    if not todo_rows:
        logging.info("  Nothing to do.")
        return

    write_header = not out_csv.exists() or out_csv.stat().st_size == 0
    counts = Counter()
    n_done = 0

    # Build path list and row lookup
    img_paths = [r["img_path"] for r in todo_rows]

    with open(out_csv, "a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        if write_header:
            writer.writerow(["filename", "cosbidfp", "year", "lat", "lon",
                             "roadname", "pred_id", "pred_label", "max_prob",
                             "p0", "p1", "p2"])

        # YOLO predict with streaming
        preds = model.predict(
            source=img_paths,
            imgsz=IMGSZ,
            batch=BATCH_SIZE,
            device=DEVICE,
            stream=True,
            verbose=False,
        )

        for idx, r in enumerate(preds):
            row = todo_rows[idx]
            probs = probs_to_numpy(r.probs)
            pred_id = int(getattr(r.probs, "top1", int(np.argmax(probs))))
            max_prob = float(getattr(r.probs, "top1conf", float(np.max(probs))))
            pred_label = CLASS_NAMES.get(pred_id, str(pred_id))

            p0 = float(probs[0]) if probs.shape[0] == 3 else 0.0
            p1 = float(probs[1]) if probs.shape[0] == 3 else 0.0
            p2 = float(probs[2]) if probs.shape[0] == 3 else 0.0

            writer.writerow([
                row["filename"], row["cosbidfp"], row["year"],
                row["lat"], row["lon"], row["roadname"],
                pred_id, pred_label, f"{max_prob:.6f}",
                f"{p0:.6f}", f"{p1:.6f}", f"{p2:.6f}",
            ])
            counts[pred_label] += 1
            n_done += 1

            if n_done % PRINT_EVERY == 0:
                logging.info(f"  [{n_done}/{len(todo_rows)}] processed")

        f.flush()

    logging.info(f"  YOLOv11 done: {n_done} images predicted")
    logging.info(f"  Class distribution: {dict(counts)}")
    logging.info(f"  Output: {out_csv}")
```

**Step 5: Write main entry point**

```python
# ---- Main -----------------------------------------------------------------

def main():
    logging.info("=" * 70)
    logging.info("NAIP SATELLITE IMAGE ROAD QUALITY PREDICTION")
    logging.info("=" * 70)

    start_time = time.time()

    # Load manifest
    manifest_rows = load_manifest(MANIFEST_CSV)
    if not manifest_rows:
        logging.error("No images found in manifest. Exiting.")
        sys.exit(1)

    # Run ConvNeXt v2
    run_convnext_inference(manifest_rows, CONVNEXT_OUT_CSV)

    # Run YOLOv11
    run_yolo_inference(manifest_rows, YOLO_OUT_CSV)

    elapsed = time.time() - start_time
    logging.info(f"All predictions complete in {elapsed:.1f}s")
    logging.info(f"ConvNeXt output: {CONVNEXT_OUT_CSV}")
    logging.info(f"YOLO output: {YOLO_OUT_CSV}")


if __name__ == "__main__":
    main()
```

**Step 6: Verify the script runs without errors**

Run (from conda geoai-mf):
```bash
cd "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
python code/4.5_predict_naip_satellite_images.py
```

Expected: Both CSVs created in `data/roads/satellite_images/` with one row per image. Progress logged to console and `prediction_run.log`.

**Step 7: Commit**

```bash
git add code/4.5_predict_naip_satellite_images.py
git commit -m "feat: add NAIP satellite image prediction script (ConvNeXt v2 + YOLOv11)"
```

---

## Task 2: Create `2.10_merge_naip_predictions.R` - Data merge and panel construction

**Files:**
- Create: `code/2.10_merge_naip_predictions.R`

**Context:**
- Prediction CSVs from Task 1: `data/roads/satellite_images/naip_preds_convnext.csv` and `naip_preds_yolo.csv`
- Main RDD dataset: `data/roads_and_census.dta` (read-only) -- has `tendigit_fips`, `year` (election year), `votes_pct_against`, `treated`, `pop`
- The merge key is `cosbidfp == tendigit_fips` (both are 10-digit FIPS codes)
- Reference for variable construction: `code/3.11_road_quality_analysis.R` lines 150-268

**Step 1: Write the script**

```r
#================================================================================================================#
# Purpose : Merge NAIP satellite image predictions with RDD data to create analysis-ready panel
# Name    : Saani Rawat
# Date    : 2026-02-22
# Input   : naip_preds_convnext.csv, naip_preds_yolo.csv, roads_and_census.dta
# Output  : naip_road_quality_panel_convnext.csv, naip_road_quality_panel_yolo.csv,
#           naip_road_quality_collapsed.csv
# Note    : No original data files are modified.
#================================================================================================================#

library(tidyverse)
library(haven)

root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root, "/data")
sat_dir <- paste0(data, "/roads/satellite_images")

#==========================================================================================================#
# 1. Load data
#==========================================================================================================#

# Load the RDD dataset (read-only)
roads_and_census <- read_dta(paste0(data, "/roads_and_census.dta")) %>%
  dplyr::select(tendigit_fips, year, votes_pct_against, treated, pop) %>%
  rename(election_year = year)

# Load prediction CSVs
preds_convnext <- read_csv(paste0(sat_dir, "/naip_preds_convnext.csv"))
preds_yolo <- read_csv(paste0(sat_dir, "/naip_preds_yolo.csv"))

cat("ConvNeXt predictions:", nrow(preds_convnext), "rows\n")
cat("YOLO predictions:", nrow(preds_yolo), "rows\n")
cat("RDD dataset:", nrow(roads_and_census), "rows\n")

#==========================================================================================================#
# 2. Compute road quality scores (both methods, applied to each model)
#==========================================================================================================#

compute_scores <- function(df) {
  df %>%
    mutate(
      # Method A: existing 3.11 formula
      rq_score_a = round(((pred_id + max_prob) / 3) * 99 + 1, 2),
      # Method B: expected value using full probability distribution
      rq_score_ev = round(p0 * 0 + p1 * 1 + p2 * 2, 4),
      # Ensure cosbidfp is numeric for merge
      cosbidfp = as.numeric(cosbidfp),
      year = as.integer(year)
    )
}

preds_convnext <- compute_scores(preds_convnext)
preds_yolo <- compute_scores(preds_yolo)

#==========================================================================================================#
# 3. Aggregate to subdivision x NAIP year
#==========================================================================================================#

aggregate_to_panel <- function(df, model_name) {
  df %>%
    group_by(cosbidfp, year) %>%
    summarize(
      n_images = n(),
      mean_pred_id = mean(pred_id, na.rm = TRUE),
      median_pred_id = median(pred_id, na.rm = TRUE),
      mean_rq_score_a = mean(rq_score_a, na.rm = TRUE),
      median_rq_score_a = median(rq_score_a, na.rm = TRUE),
      mean_rq_score_ev = mean(rq_score_ev, na.rm = TRUE),
      median_rq_score_ev = median(rq_score_ev, na.rm = TRUE),
      mean_max_prob = mean(max_prob, na.rm = TRUE),
      mean_p0 = mean(p0, na.rm = TRUE),
      mean_p1 = mean(p1, na.rm = TRUE),
      mean_p2 = mean(p2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(model = model_name)
}

panel_convnext <- aggregate_to_panel(preds_convnext, "convnext_v2")
panel_yolo <- aggregate_to_panel(preds_yolo, "yolo11")

cat("ConvNeXt panel:", nrow(panel_convnext), "subdivision-year obs\n")
cat("YOLO panel:", nrow(panel_yolo), "subdivision-year obs\n")

#==========================================================================================================#
# 4. Merge with RDD data
#==========================================================================================================#

merge_with_rdd <- function(panel_df) {
  panel_df %>%
    rename(naip_year = year) %>%
    inner_join(roads_and_census, by = c("cosbidfp" = "tendigit_fips")) %>%
    mutate(
      post_election_flag = as.integer(naip_year > election_year),
      event_time = naip_year - election_year,
      did = post_election_flag * treated
    ) %>%
    # Keep only subdivisions with at least 1 pre and 1 post-election image
    group_by(cosbidfp, election_year) %>%
    filter(any(post_election_flag == 0) & any(post_election_flag == 1)) %>%
    ungroup() %>%
    arrange(cosbidfp, election_year, naip_year)
}

panel_convnext_rdd <- merge_with_rdd(panel_convnext)
panel_yolo_rdd <- merge_with_rdd(panel_yolo)

cat("ConvNeXt panel (merged, filtered):", nrow(panel_convnext_rdd), "obs\n")
cat("YOLO panel (merged, filtered):", nrow(panel_yolo_rdd), "obs\n")
cat("Unique subdivisions (ConvNeXt):", n_distinct(panel_convnext_rdd$cosbidfp), "\n")

#==========================================================================================================#
# 5. Create collapsed pre/post dataset
#==========================================================================================================#

collapse_pre_post <- function(panel_df, model_name) {
  panel_df %>%
    group_by(cosbidfp, election_year, treated, votes_pct_against, pop, post_election_flag) %>%
    summarize(
      n_images = sum(n_images),
      n_years = n(),
      mean_pred_id = mean(mean_pred_id, na.rm = TRUE),
      mean_rq_score_a = mean(mean_rq_score_a, na.rm = TRUE),
      mean_rq_score_ev = mean(mean_rq_score_ev, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      did = post_election_flag * treated,
      model = model_name
    )
}

collapsed_convnext <- collapse_pre_post(panel_convnext_rdd, "convnext_v2")
collapsed_yolo <- collapse_pre_post(panel_yolo_rdd, "yolo11")
collapsed_all <- bind_rows(collapsed_convnext, collapsed_yolo)

#==========================================================================================================#
# 6. Save outputs
#==========================================================================================================#

write_csv(panel_convnext_rdd, paste0(sat_dir, "/naip_road_quality_panel_convnext.csv"))
write_csv(panel_yolo_rdd, paste0(sat_dir, "/naip_road_quality_panel_yolo.csv"))
write_csv(collapsed_all, paste0(sat_dir, "/naip_road_quality_collapsed.csv"))

cat("\nOutputs saved to", sat_dir, ":\n")
cat("  naip_road_quality_panel_convnext.csv\n")
cat("  naip_road_quality_panel_yolo.csv\n")
cat("  naip_road_quality_collapsed.csv\n")
```

**Step 2: Run the script in R**

```r
source("code/2.10_merge_naip_predictions.R")
```

Expected: Three CSVs created. Console prints row counts and unique subdivisions.

**Step 3: Commit**

```bash
git add code/2.10_merge_naip_predictions.R
git commit -m "feat: add NAIP predictions merge and panel construction script"
```

---

## Task 3: Create `3.13_naip_road_quality_analysis.R` - Causal analysis

**Files:**
- Create: `code/3.13_naip_road_quality_analysis.R`

**Context:**
- Panel CSVs from Task 2
- Reference analysis code: `code/3.11_road_quality_analysis.R` (DID, RDD with rdrobust, ggplot2 plots)
- Key R packages: `fixest` (feols, iplot), `rdrobust` (rdrobust, rdplot), `ggplot2`
- Cutoff for RDD: 50 (votes_pct_against)

**Step 1: Write the script - setup and data loading**

```r
#================================================================================================================#
# Purpose : Road Quality Analysis using NAIP satellite images + AI vision model predictions
# Name    : Saani Rawat
# Date    : 2026-02-22
# Input   : naip_road_quality_panel_{convnext,yolo}.csv, naip_road_quality_collapsed.csv
# Output  : Tables (LaTeX) and plots (PNG) to data/outputs/{tables,plots}/
#================================================================================================================#

library(fixest)
library(rdrobust)
library(tidyverse)

root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root, "/data")
sat_dir <- paste0(data, "/roads/satellite_images")
tables <- paste0(data, "/outputs/tables")
plots <- paste0(data, "/outputs/plots")

cutoff <- 50  # RDD cutoff: 50% votes against

# Load panel data
panel_cx <- read_csv(paste0(sat_dir, "/naip_road_quality_panel_convnext.csv"))
panel_yl <- read_csv(paste0(sat_dir, "/naip_road_quality_panel_yolo.csv"))
collapsed <- read_csv(paste0(sat_dir, "/naip_road_quality_collapsed.csv"))

cat("ConvNeXt panel:", nrow(panel_cx), "obs,", n_distinct(panel_cx$cosbidfp), "subdivisions\n")
cat("YOLO panel:", nrow(panel_yl), "obs,", n_distinct(panel_yl$cosbidfp), "subdivisions\n")
```

**Step 2: Summary statistics (2x2 DID table)**

```r
#==========================================================================================================#
# 1. Summary Statistics: Treatment x Pre/Post
#==========================================================================================================#

summary_table <- function(df, model_name) {
  df %>%
    group_by(treated, post_election_flag) %>%
    summarize(
      n = n(),
      mean_pred_id = mean(mean_pred_id, na.rm = TRUE),
      sd_pred_id = sd(mean_pred_id, na.rm = TRUE),
      mean_rq_a = mean(mean_rq_score_a, na.rm = TRUE),
      sd_rq_a = sd(mean_rq_score_a, na.rm = TRUE),
      mean_rq_ev = mean(mean_rq_score_ev, na.rm = TRUE),
      sd_rq_ev = sd(mean_rq_score_ev, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(model = model_name)
}

summary_cx <- summary_table(panel_cx, "ConvNeXt v2")
summary_yl <- summary_table(panel_yl, "YOLOv11")
print(bind_rows(summary_cx, summary_yl), width = Inf)
```

**Step 3: Simple DID regressions (collapsed pre/post)**

```r
#==========================================================================================================#
# 2. Difference-in-Differences (collapsed pre/post)
#==========================================================================================================#

# ConvNeXt v2
collapsed_cx <- collapsed %>% filter(model == "convnext_v2")
collapsed_yl <- collapsed %>% filter(model == "yolo11")

did_cx_pred <- feols(mean_pred_id ~ post_election_flag * treated,
                     data = collapsed_cx, cluster = ~cosbidfp)
did_cx_ev <- feols(mean_rq_score_ev ~ post_election_flag * treated,
                   data = collapsed_cx, cluster = ~cosbidfp)

did_yl_pred <- feols(mean_pred_id ~ post_election_flag * treated,
                     data = collapsed_yl, cluster = ~cosbidfp)
did_yl_ev <- feols(mean_rq_score_ev ~ post_election_flag * treated,
                   data = collapsed_yl, cluster = ~cosbidfp)

# Combined DID table
etable(did_cx_pred, did_cx_ev, did_yl_pred, did_yl_ev,
       headers = c("CX: pred_id", "CX: EV score", "YOLO: pred_id", "YOLO: EV score"),
       tex = TRUE,
       file = paste0(tables, "/naip_road_quality_did.tex"))

etable(did_cx_pred, did_cx_ev, did_yl_pred, did_yl_ev,
       headers = c("CX: pred_id", "CX: EV score", "YOLO: pred_id", "YOLO: EV score"))
```

**Step 4: Event-study analysis (panel)**

```r
#==========================================================================================================#
# 3. Event Study (panel: subdivision x NAIP year)
#==========================================================================================================#

# ConvNeXt v2 event study
es_cx <- feols(mean_rq_score_ev ~ i(event_time, treated, ref = -1) | cosbidfp,
               data = panel_cx, cluster = ~cosbidfp)

png(paste0(plots, "/naip_event_study_road_quality.png"), width = 10, height = 6, units = "in", res = 300)
iplot(es_cx,
      main = "Event Study: Road Quality Around Tax Levy Elections (ConvNeXt v2)",
      xlab = "Years Relative to Election",
      ylab = "Road Quality Score (Expected Value)")
abline(v = 0, lty = 2, col = "gray50")
dev.off()

# YOLO event study
es_yl <- feols(mean_rq_score_ev ~ i(event_time, treated, ref = -1) | cosbidfp,
               data = panel_yl, cluster = ~cosbidfp)

summary(es_cx)
summary(es_yl)
```

**Step 5: RDD analysis (post-election only)**

```r
#==========================================================================================================#
# 4. Regression Discontinuity (post-election observations only)
#==========================================================================================================#

panel_cx_post <- panel_cx %>% filter(post_election_flag == 1)
panel_yl_post <- panel_yl %>% filter(post_election_flag == 1)

# ConvNeXt v2 RDD
rd_cx <- rdrobust(
  y = panel_cx_post$mean_rq_score_ev,
  x = panel_cx_post$votes_pct_against,
  c = cutoff,
  covs = panel_cx_post$pop,
  all = TRUE, kernel = "uniform", bwselect = "mserd", p = 1, q = 2,
  h = max(abs(panel_cx_post$votes_pct_against - cutoff), na.rm = TRUE),
  cluster = panel_cx_post$cosbidfp
)
summary(rd_cx)

# YOLOv11 RDD
rd_yl <- rdrobust(
  y = panel_yl_post$mean_rq_score_ev,
  x = panel_yl_post$votes_pct_against,
  c = cutoff,
  covs = panel_yl_post$pop,
  all = TRUE, kernel = "uniform", bwselect = "mserd", p = 1, q = 2,
  h = max(abs(panel_yl_post$votes_pct_against - cutoff), na.rm = TRUE),
  cluster = panel_yl_post$cosbidfp
)
summary(rd_yl)

# RD Plot (ConvNeXt v2)
rd_plot_cx <- rdplot(
  y = panel_cx_post$mean_rq_score_ev,
  x = panel_cx_post$votes_pct_against,
  c = cutoff,
  p = 1, kernel = "uniform",
  h = max(abs(panel_cx_post$votes_pct_against - cutoff), na.rm = TRUE),
  nbins = 4, binselect = "esmv",
  title = "Road Quality vs Vote Share Against Tax Renewal (NAIP + ConvNeXt v2)",
  x.label = "Vote Share Against Tax Renewal (%)",
  y.label = "Road Quality Score (Expected Value)"
)
ggsave(paste0(plots, "/naip_rd_plot_road_quality.png"),
       plot = rd_plot_cx$rdplot, width = 10, height = 6, dpi = 300)
```

**Step 6: Pre/post line plot and bar plot (same style as 3.11)**

```r
#==========================================================================================================#
# 5. Visualization: Line plot and bar plot (matching 3.11 style)
#==========================================================================================================#

# Prepare summary for plotting
plot_summary <- collapsed %>%
  group_by(model, treated, post_election_flag) %>%
  summarize(mean_rq = mean(mean_rq_score_ev, na.rm = TRUE),
            sd_rq = sd(mean_rq_score_ev, na.rm = TRUE),
            n = n(), .groups = "drop")

# Line plot: ConvNeXt v2
plot_cx <- plot_summary %>% filter(model == "convnext_v2")
lp <- ggplot(plot_cx, aes(x = post_election_flag, y = mean_rq, color = factor(treated))) +
  geom_line(aes(group = treated), linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_color_manual(values = c("0" = "#2c3e50", "1" = "#e74c3c"),
                     labels = c("Control", "Treated"), name = "Group") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Pre-Election", "Post-Election")) +
  labs(x = "Election Period", y = "Mean Road Quality Score (EV)",
       title = "NAIP Road Quality Before and After Elections (ConvNeXt v2)",
       subtitle = "Treatment vs Control Groups") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"))
ggsave(paste0(plots, "/naip_road_quality_lineplot.png"), plot = lp, width = 8, height = 6, dpi = 300)

# Bar plot
bp <- ggplot(plot_cx, aes(x = factor(treated), y = mean_rq, fill = factor(post_election_flag))) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#2c3e50", "1" = "#e74c3c"),
                    labels = c("Pre-Election", "Post-Election"), name = "Election Period") +
  scale_x_discrete(labels = c("0" = "Control", "1" = "Treated")) +
  labs(x = "Group", y = "Mean Road Quality Score (EV)",
       title = "NAIP Road Quality by Treatment and Election Period (ConvNeXt v2)") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave(paste0(plots, "/naip_road_quality_barplot.png"), plot = bp, width = 8, height = 6, dpi = 300)

cat("\nAll outputs saved.\n")
```

**Step 7: Run the analysis script**

```r
source("code/3.13_naip_road_quality_analysis.R")
```

Expected: Summary tables printed to console, LaTeX table saved, plots saved to `data/outputs/plots/`.

**Step 8: Commit**

```bash
git add code/3.13_naip_road_quality_analysis.R
git commit -m "feat: add NAIP road quality causal analysis (DID, event-study, RDD)"
```

---

## Task 4: Final commit with design doc

**Step 1: Commit the design documents**

```bash
git add docs/plans/2026-02-22-naip-road-quality-prediction-design.md
git add docs/plans/2026-02-22-naip-road-quality-prediction-plan.md
git commit -m "docs: add design doc and implementation plan for NAIP road quality pipeline"
```
