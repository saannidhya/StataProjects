#==========================================================================#
# File: 4.51_predict_osip3_satellite_images.py
# Author: Saani Rawat
# Date: 09 Mar 2026
# Description: Run ConvNeXt v2 and YOLOv11 inference on OSIP3 satellite
#              images to predict road quality (0=poor, 1=decent, 2=high).
#
# Dependencies:
#   - transformers, torch, ultralytics, PIL, pandas
#   - Fine-tuned ConvNeXt v2: data/roads/hf_finetuned_convnextv2/
#   - Fine-tuned YOLOv11: data/roads/runs_ohio/.../weights/best.pt
#   - OSIP3 images + manifest: data/roads/satellite_images_osip3/
#
# Usage:
#   python 4.51_predict_osip3_satellite_images.py
#   Uses geoai-mf conda env.
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
from ultralytics import YOLO

# ---- Configuration --------------------------------------------------------#

ROOT = Path("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation")
DATA_DIR = ROOT / "data" / "roads"
SAT_DIR = DATA_DIR / "satellite_images_osip3"
MANIFEST_CSV = SAT_DIR / "satellite_images_osip3_manifest.csv"

# Fine-tuned ConvNeXt v2 model
CONVNEXT_MODEL_DIR = DATA_DIR / "hf_finetuned_convnextv2"

# Fine-tuned YOLOv11 classification model
YOLO_WEIGHTS = (
    DATA_DIR
    / "runs_ohio"
    / "yolo11_finetune_satellite_images"
    / "yolo11n_cls_roads"
    / "weights"
    / "best.pt"
)

# Output prediction CSVs (saved alongside OSIP3 images)
CONVNEXT_OUT_CSV = SAT_DIR / "osip3_preds_convnext.csv"
YOLO_OUT_CSV = SAT_DIR / "osip3_preds_yolo.csv"

# Class mapping
CLASS_NAMES = {0: "low_quality", 1: "medium_quality", 2: "high_quality"}

# Output schema: keep the core 4.5 columns first, then append OSIP-specific metadata.
OUTPUT_COLUMNS = [
    "filename",
    "cosbidfp",
    "year",
    "lat",
    "lon",
    "roadname",
    "namelsad",
    "product",
    "folder",
    "tile",
    "sha256",
    "pred_id",
    "pred_label",
    "max_prob",
    "p0",
    "p1",
    "p2",
    "source_url",
]

# Inference settings
BATCH_SIZE = 32
IMGSZ = 224
PRINT_EVERY = 200

DEVICE = "cuda" if torch.cuda.is_available() else "cpu"

# Logging
LOG_PATH = SAT_DIR / "prediction_run_osip3.log"
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.FileHandler(LOG_PATH),
        logging.StreamHandler(sys.stdout),
    ],
)


# ---- Load manifest --------------------------------------------------------#

def load_manifest(manifest_path: Path) -> list[dict]:
    """Load the OSIP3 manifest CSV and keep rows with existing JPG files."""
    rows = []
    with open(manifest_path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            img_path = SAT_DIR / row["filename"]
            if img_path.exists():
                row["img_path"] = str(img_path)
                rows.append(row)
    logging.info(f"Loaded {len(rows)} OSIP3 images from manifest (with existing files).")
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


def build_prediction_row(row: dict, pred_id: int, pred_label: str, max_prob: float, probs: np.ndarray) -> list:
    """Create one output row in a stable schema for either model."""
    p0 = float(probs[0]) if probs.shape[0] >= 1 else 0.0
    p1 = float(probs[1]) if probs.shape[0] >= 2 else 0.0
    p2 = float(probs[2]) if probs.shape[0] >= 3 else 0.0

    return [
        row["filename"],
        row.get("cosbidfp", ""),
        row.get("year", ""),
        row.get("lat", ""),
        row.get("lon", ""),
        row.get("roadname", ""),
        row.get("namelsad", ""),
        row.get("product", ""),
        row.get("folder", ""),
        row.get("tile", ""),
        row.get("sha256", ""),
        pred_id,
        pred_label,
        f"{float(max_prob):.6f}",
        f"{p0:.6f}",
        f"{p1:.6f}",
        f"{p2:.6f}",
        row.get("source_url", ""),
    ]


# ---- ConvNeXt v2 inference ------------------------------------------------#

def run_convnext_inference(manifest_rows: list[dict], out_csv: Path):
    """
    Run ConvNeXt v2 classification on all OSIP3 images.
    """
    logging.info("=" * 70)
    logging.info("ConvNeXt v2 INFERENCE")
    logging.info("=" * 70)

    if not CONVNEXT_MODEL_DIR.exists():
        logging.error(f"  ConvNeXt model not found: {CONVNEXT_MODEL_DIR}")
        return

    processor = AutoImageProcessor.from_pretrained(str(CONVNEXT_MODEL_DIR))
    model = AutoModelForImageClassification.from_pretrained(str(CONVNEXT_MODEL_DIR)).to(DEVICE)
    model.eval()
    logging.info(f"  Loaded ConvNeXt v2 from {CONVNEXT_MODEL_DIR}")
    logging.info(f"  Device: {DEVICE}")

    done_filenames = load_already_predicted(out_csv)
    todo_rows = [r for r in manifest_rows if r["filename"] not in done_filenames]
    logging.info(f"  Images to predict: {len(todo_rows)} (skipping {len(done_filenames)} already done)")

    if not todo_rows:
        logging.info("  Nothing to do.")
        return

    write_header = not out_csv.exists() or out_csv.stat().st_size == 0
    counts = Counter()
    n_done = 0

    with open(out_csv, "a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        if write_header:
            writer.writerow(OUTPUT_COLUMNS)

        for batch_start in range(0, len(todo_rows), BATCH_SIZE):
            batch_rows = todo_rows[batch_start: batch_start + BATCH_SIZE]

            images = []
            valid_rows = []
            for row in batch_rows:
                try:
                    img = Image.open(row["img_path"]).convert("RGB")
                    images.append(img)
                    valid_rows.append(row)
                except Exception as exc:  # noqa: BLE001
                    logging.warning(f"  Could not open {row['filename']}: {exc}")

            if not images:
                continue

            try:
                inputs = processor(images=images, return_tensors="pt").to(DEVICE)
                with torch.no_grad():
                    logits = model(**inputs).logits
                    probs = torch.softmax(logits, dim=-1).cpu().numpy()

                preds = probs.argmax(axis=1)
                max_probs = probs.max(axis=1)
            finally:
                for img in images:
                    img.close()

            for idx, row in enumerate(valid_rows):
                pred_id = int(preds[idx])
                pred_label = CLASS_NAMES.get(pred_id, str(pred_id))
                writer.writerow(
                    build_prediction_row(
                        row=row,
                        pred_id=pred_id,
                        pred_label=pred_label,
                        max_prob=float(max_probs[idx]),
                        probs=probs[idx],
                    )
                )
                counts[pred_label] += 1

            n_done += len(valid_rows)
            if n_done % PRINT_EVERY < BATCH_SIZE:
                logging.info(f"  [{n_done}/{len(todo_rows)}] processed")

        f.flush()

    logging.info(f"  ConvNeXt v2 done: {n_done} OSIP3 images predicted")
    logging.info(f"  Class distribution: {dict(counts)}")
    logging.info(f"  Output: {out_csv}")


# ---- YOLOv11 inference ----------------------------------------------------#

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
    Run YOLOv11 classification on all OSIP3 images.
    """
    logging.info("=" * 70)
    logging.info("YOLOv11 INFERENCE")
    logging.info("=" * 70)

    if not YOLO_WEIGHTS.exists():
        logging.error(f"  YOLO weights not found: {YOLO_WEIGHTS}")
        return

    model = YOLO(str(YOLO_WEIGHTS))
    logging.info(f"  Loaded YOLOv11 from {YOLO_WEIGHTS}")

    done_filenames = load_already_predicted(out_csv)
    todo_rows = [r for r in manifest_rows if r["filename"] not in done_filenames]
    logging.info(f"  Images to predict: {len(todo_rows)} (skipping {len(done_filenames)} already done)")

    if not todo_rows:
        logging.info("  Nothing to do.")
        return

    write_header = not out_csv.exists() or out_csv.stat().st_size == 0
    counts = Counter()
    n_done = 0

    with open(out_csv, "a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        if write_header:
            writer.writerow(OUTPUT_COLUMNS)

        for batch_start in range(0, len(todo_rows), BATCH_SIZE):
            batch_rows = todo_rows[batch_start: batch_start + BATCH_SIZE]
            batch_paths = [r["img_path"] for r in batch_rows]

            results = model.predict(
                source=batch_paths,
                imgsz=IMGSZ,
                batch=BATCH_SIZE,
                device=DEVICE,
                verbose=False,
            )

            for idx, result in enumerate(results):
                row = batch_rows[idx]
                probs = probs_to_numpy(result.probs)
                pred_id = int(getattr(result.probs, "top1", int(np.argmax(probs))))
                max_prob = float(getattr(result.probs, "top1conf", float(np.max(probs))))
                pred_label = CLASS_NAMES.get(pred_id, str(pred_id))

                writer.writerow(
                    build_prediction_row(
                        row=row,
                        pred_id=pred_id,
                        pred_label=pred_label,
                        max_prob=max_prob,
                        probs=probs,
                    )
                )
                counts[pred_label] += 1
                n_done += 1

            if n_done % PRINT_EVERY < BATCH_SIZE:
                logging.info(f"  [{n_done}/{len(todo_rows)}] processed")

        f.flush()

    logging.info(f"  YOLOv11 done: {n_done} OSIP3 images predicted")
    logging.info(f"  Class distribution: {dict(counts)}")
    logging.info(f"  Output: {out_csv}")


# ---- Main ----------------------------------------------------------------#

def main():
    logging.info("=" * 70)
    logging.info("OSIP3 SATELLITE IMAGE ROAD QUALITY PREDICTION")
    logging.info("=" * 70)

    start_time = time.time()

    if not MANIFEST_CSV.exists():
        logging.error(f"Manifest not found: {MANIFEST_CSV}")
        sys.exit(1)

    manifest_rows = load_manifest(MANIFEST_CSV)
    if not manifest_rows:
        logging.error("No OSIP3 images found in manifest. Exiting.")
        sys.exit(1)

    run_convnext_inference(manifest_rows, CONVNEXT_OUT_CSV)
    run_yolo_inference(manifest_rows, YOLO_OUT_CSV)

    elapsed = time.time() - start_time
    logging.info(f"All OSIP3 predictions complete in {elapsed:.1f}s")
    logging.info(f"ConvNeXt output: {CONVNEXT_OUT_CSV}")
    logging.info(f"YOLO output: {YOLO_OUT_CSV}")


if __name__ == "__main__":
    main()
