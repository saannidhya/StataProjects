#==========================================================================#
# File: 4.8_predict_ohio_streetview.py
# Author: Saani Rawat
# Date: 04 Mar 2026
# Description: Run ConvNeXt v2 inference on Ohio Google Street View images
#              to predict road quality (0=low, 1=medium, 2=high).
#              Outputs per-image predictions and PCR scores.
#
# Dependencies:
#   - transformers, torch, PIL, pandas
#   - Fine-tuned model from 4.7: data/roads/hf_finetuned_convnextv2_streetview/
#   - Ohio streetview images: data/roads/ohio/google streetview photos/
#
# Usage:
#   python 4.8_predict_ohio_streetview.py
#==========================================================================#

from __future__ import annotations

import csv
import logging
import sys
import time
from collections import Counter
from pathlib import Path

import numpy as np
import torch
from PIL import Image
from transformers import AutoImageProcessor, AutoModelForImageClassification

# ---- Configuration --------------------------------------------------------

ROOT = Path(
    "C:/Users/rawatsa/OneDrive - University of Cincinnati/"
    "StataProjects/ohio_taxation"
)
DATA_DIR = ROOT / "data" / "roads"
SV_DIR = DATA_DIR / "ohio" / "google streetview photos"

# Model directory (from 4.7 training)
MODEL_DIR = DATA_DIR / "hf_finetuned_convnextv2_streetview"

# Output
OUT_CSV = SV_DIR / "ohio_streetview_preds.csv"

# Class mapping
CLASS_NAMES = {0: "low_quality", 1: "medium_quality", 2: "high_quality"}

# Inference settings
BATCH_SIZE = 32
PRINT_EVERY = 200

DEVICE = "cuda" if torch.cuda.is_available() else "cpu"

# Logging
LOG_PATH = SV_DIR / "streetview_prediction_run.log"
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.FileHandler(LOG_PATH),
        logging.StreamHandler(sys.stdout),
    ],
)


# ---- Filename parser -------------------------------------------------------

def parse_streetview_filename(filename: str) -> dict:
    """Parse metadata from streetview image filename.

    Format (from 1.2_import_streetview_images_new.py):
        {cosbidfp}_{stname}_{namelsad}_{date}_{lat}_{lon}_h{heading}_p{pitch}_f{fov}.jpg

    where stname and namelsad have spaces replaced with underscores.
    namelsad typically ends with "township" (e.g., "Orange_township").

    Examples:
        3900558562_Columbus_St_Orange_township_2019-07_40.919..._-82.281..._h359..._p-15_f60.jpg
        3913775206_Water_St_Sugar_Creek_township_2014-08_40.864..._-84.140..._h159..._p-15_f60.jpg
    """
    stem = Path(filename).stem
    parts = stem.split("_")

    # Parse from the end (fixed format)
    fov = parts[-1]        # f60
    pitch = parts[-2]      # p-15
    heading = parts[-3]    # h359.530...
    lon = parts[-4]        # -82.281...
    lat = parts[-5]        # 40.919...
    date = parts[-6]       # 2019-07

    # First part is always the 10-digit FIPS
    tendigit_fips = parts[0]

    # Middle parts: street name + subdivision name (e.g., "Orange_township")
    middle = parts[1:-6]

    # Find "township" keyword to split road name from subdivision name.
    # The subdivision name (namelsad) ends with "township".
    # Everything before the township name is the street name.
    twp_idx = None
    for i, p in enumerate(middle):
        if p.lower() == "township":
            twp_idx = i
            break

    if twp_idx is not None and twp_idx > 0:
        # The township name starts at the word before "township"
        # BUT it could be multi-word like "Sugar Creek township" or "E Saybrook township"
        # Heuristic: common township prefixes are 1-3 words. Since the road name
        # typically ends with a road suffix (St, Rd, Ave, Dr, etc.), find the last
        # road suffix to determine where road ends and township begins.
        road_suffixes = {"st", "rd", "ave", "dr", "ln", "ct", "pl", "blvd",
                         "way", "pike", "hwy", "nw", "ne", "sw", "se",
                         "n", "s", "e", "w"}

        # Find the last road suffix in the middle parts (before "township")
        last_suffix_idx = -1
        for i in range(twp_idx):
            if middle[i].lower() in road_suffixes:
                last_suffix_idx = i

        if last_suffix_idx >= 0:
            road_parts = middle[:last_suffix_idx + 1]
            township_parts = middle[last_suffix_idx + 1:twp_idx + 1]
        else:
            # No recognized suffix — assume everything up to 1 before "township" is road
            road_parts = middle[:max(1, twp_idx - 1)]
            township_parts = middle[max(1, twp_idx - 1):twp_idx + 1]
    else:
        # No "township" found — treat everything as road name
        road_parts = middle
        township_parts = []

    return {
        "tendigit_fips": tendigit_fips,
        "road_name": " ".join(road_parts),
        "township": " ".join(township_parts),
        "date": date,
        "lat": lat,
        "lon": lon,
    }


# ---- Resume capability -----------------------------------------------------

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


# ---- Main -------------------------------------------------------------------

def main():
    logging.info("=" * 70)
    logging.info("OHIO STREETVIEW ROAD QUALITY PREDICTION")
    logging.info("=" * 70)

    # Check model exists
    if not MODEL_DIR.exists():
        logging.error(f"Model not found at {MODEL_DIR}")
        logging.error("Run 4.7_train_streetview_model.py first (on Colab GPU).")
        sys.exit(1)

    # Collect image files
    image_files = sorted(SV_DIR.glob("*.jpg"))
    logging.info(f"  Found {len(image_files)} JPG images in {SV_DIR}")

    if not image_files:
        logging.error("No images found!")
        sys.exit(1)

    # Load model
    logging.info(f"  Loading model from {MODEL_DIR}")
    processor = AutoImageProcessor.from_pretrained(str(MODEL_DIR))
    model = AutoModelForImageClassification.from_pretrained(str(MODEL_DIR)).to(DEVICE)
    model.eval()
    logging.info(f"  Device: {DEVICE}")

    # Resume: skip already-predicted images
    done_filenames = load_already_predicted(OUT_CSV)
    todo_files = [f for f in image_files if f.name not in done_filenames]
    logging.info(f"  Images to predict: {len(todo_files)} "
                 f"(skipping {len(done_filenames)} already done)")

    if not todo_files:
        logging.info("  Nothing to do.")
        return

    # Open CSV in append mode
    write_header = not OUT_CSV.exists() or OUT_CSV.stat().st_size == 0
    counts = Counter()
    n_done = 0
    t0 = time.time()

    with open(OUT_CSV, "a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        if write_header:
            writer.writerow([
                "filename", "tendigit_fips", "road_name", "township",
                "date", "lat", "lon", "pred_class", "pred_label",
                "pcr_score_pred", "max_prob", "p0", "p1", "p2",
            ])

        # Process in batches
        for batch_start in range(0, len(todo_files), BATCH_SIZE):
            batch_files = todo_files[batch_start:batch_start + BATCH_SIZE]

            # Load images
            images = []
            valid_files = []
            for img_path in batch_files:
                try:
                    img = Image.open(img_path).convert("RGB")
                    images.append(img)
                    valid_files.append(img_path)
                except Exception as e:
                    logging.warning(f"  Could not open {img_path.name}: {e}")

            if not images:
                continue

            # Run inference
            inputs = processor(images=images, return_tensors="pt").to(DEVICE)
            with torch.no_grad():
                logits = model(**inputs).logits
                probs = torch.softmax(logits, dim=-1).cpu().numpy()

            preds = probs.argmax(axis=1)
            max_probs = probs.max(axis=1)

            for i, img_path in enumerate(valid_files):
                meta = parse_streetview_filename(img_path.name)
                pred_class = int(preds[i])
                pred_label = CLASS_NAMES.get(pred_class, str(pred_class))
                p0, p1, p2 = float(probs[i, 0]), float(probs[i, 1]), float(probs[i, 2])

                # PCR score from probabilities (midpoints of each class's PCR range)
                # Class 0 (low): PCR ~47-85, midpoint ~66
                # Class 1 (med): PCR ~85-93, midpoint ~89
                # Class 2 (high): PCR ~93-98, midpoint ~95.5
                pcr_score_pred = p0 * 66.0 + p1 * 89.0 + p2 * 95.5

                writer.writerow([
                    img_path.name,
                    meta["tendigit_fips"],
                    meta["road_name"],
                    meta["township"],
                    meta["date"],
                    meta["lat"],
                    meta["lon"],
                    pred_class,
                    pred_label,
                    f"{pcr_score_pred:.4f}",
                    f"{float(max_probs[i]):.6f}",
                    f"{p0:.6f}",
                    f"{p1:.6f}",
                    f"{p2:.6f}",
                ])
                counts[pred_label] += 1

            n_done += len(valid_files)
            if n_done % PRINT_EVERY < BATCH_SIZE:
                elapsed = time.time() - t0
                rate = n_done / elapsed if elapsed > 0 else 0
                logging.info(f"  [{n_done}/{len(todo_files)}] processed "
                             f"({rate:.1f} img/s)")

        f.flush()

    elapsed = time.time() - t0
    logging.info(f"\n  Done: {n_done} images predicted in {elapsed:.1f}s")
    logging.info(f"  Class distribution: {dict(counts)}")
    logging.info(f"  Output: {OUT_CSV}")

    # ------------------------------------------------------------------
    # ODOT calibration: reclassify to match ODOT distribution (14%/17%/69%)
    # ------------------------------------------------------------------
    logging.info("\n  ODOT calibration ...")
    import pandas as pd

    df = pd.read_csv(OUT_CSV)
    n = len(df)

    # ODOT proportions: 13.87% poor/very poor, 16.63% fair, 69.50% good/very good
    low_pct = 0.1387
    med_pct = 0.1663
    # high_pct = 0.6950

    # Use pcr_score_pred percentiles to find recalibration thresholds
    low_thresh = df["pcr_score_pred"].quantile(low_pct)
    med_thresh = df["pcr_score_pred"].quantile(low_pct + med_pct)

    def odot_class(score):
        if score < low_thresh:
            return 0
        elif score < med_thresh:
            return 1
        else:
            return 2

    df["odot_pred_class"] = df["pcr_score_pred"].apply(odot_class)
    df["odot_pred_label"] = df["odot_pred_class"].map(CLASS_NAMES)

    # Save calibrated output
    OUT_CALIBRATED = SV_DIR / "ohio_streetview_preds_odot_calibrated.csv"
    df.to_csv(OUT_CALIBRATED, index=False)

    odot_counts = df["odot_pred_label"].value_counts().to_dict()
    logging.info(f"  ODOT calibration thresholds: low < {low_thresh:.2f}, "
                 f"med < {med_thresh:.2f}")
    logging.info(f"  ODOT-calibrated distribution: {odot_counts}")
    logging.info(f"  ODOT-calibrated output: {OUT_CALIBRATED}")


if __name__ == "__main__":
    main()
