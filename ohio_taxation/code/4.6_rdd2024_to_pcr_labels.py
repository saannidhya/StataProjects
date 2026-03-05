#==========================================================================#
# File: 4.6_rdd2024_to_pcr_labels.py
# Author: Saani Rawat
# Date: 03 Mar 2026
# Description: Convert RDD2024 COCO-format bounding-box road damage
#              annotations into ODOT PCR-grounded 3-class road quality
#              labels (0=low, 1=medium, 2=high).
#
#              Each annotation independently contributes a deduction
#              (distress_weight * severity_weight) matching the per-
#              annotation approach in pcr_score.jl.  PCR = 100 - sum(deducts).
#              Images are assigned to 3 quality classes using fixed deduction
#              thresholds (7/15).  Output is split into stratified
#              train/val/test sets (70/15/15).
#
# Dependencies:
#   - pandas, numpy, sklearn
#   - RDD2024 USA COCO annotations:
#       data/roads/N-RDD2024.../USA_coco/train/_annotations.coco.json
#       data/roads/N-RDD2024.../USA_coco/valid/_annotations.coco.json
#
# Usage:
#   python 4.6_rdd2024_to_pcr_labels.py
#==========================================================================#

from __future__ import annotations

import json
from collections import defaultdict
from pathlib import Path

import pandas as pd
from sklearn.model_selection import train_test_split

# ---- Configuration --------------------------------------------------------

ROOT = Path(
    "C:/Users/rawatsa/OneDrive - University of Cincinnati/"
    "StataProjects/ohio_taxation"
)
DATA_DIR = ROOT / "data" / "roads"
RDD_DIR = (
    DATA_DIR
    / "N-RDD2024Road damage and defects - v5"
    / "Training and Validation Dataset"
    / "USA_coco"
)

TRAIN_JSON = RDD_DIR / "train" / "_annotations.coco.json"
VALID_JSON = RDD_DIR / "valid" / "_annotations.coco.json"

OUTPUT_ALL   = DATA_DIR / "rdd2024_pcr_labels.csv"
OUTPUT_TRAIN = DATA_DIR / "rdd2024_pcr_train.csv"
OUTPUT_VAL   = DATA_DIR / "rdd2024_pcr_val.csv"
OUTPUT_TEST  = DATA_DIR / "rdd2024_pcr_test.csv"

# ---------------------------------------------------------------------------
# All configurable thresholds are collected here.
# ---------------------------------------------------------------------------
CONFIG = {
    # Image dimensions (RDD2024 USA images are 640x640)
    "IMAGE_AREA": 640 * 640,

    # bbox area / image area thresholds for severity proxy
    # ratio < L -> "L";  L <= ratio < M -> "M";  ratio >= M -> "H"
    # M threshold = 0.05 (matching pcr_score.jl line 105)
    "SEVERITY_AREA_THRESHOLDS": {"L": 0.02, "M": 0.05},

    # Fixed deduction thresholds for class assignment
    # deduct >= low_min  -> class 0 (low quality)
    # deduct >= high_max -> class 1 (medium quality)
    # deduct <  high_max -> class 2 (high quality)
    "DEDUCT_THRESHOLDS": {
        "low_min": 15,   # deduct >= 15 -> class 0 (low quality)
        "high_max": 7,   # deduct < 7 -> class 2 (high quality)
    },

    # Stratified split proportions and seed
    "TEST_SIZE": 0.30,       # first split: 70/30
    "VAL_TEST_SIZE": 0.50,   # second split of the 30: 50/50 -> 15/15
    "RANDOM_STATE": 42,
}

# ---------------------------------------------------------------------------
# RDD2024 category_id -> (ODOT distress type, distress weight)
# category_id 0 ("roaddamage" parent) is intentionally excluded.
# ---------------------------------------------------------------------------
DAMAGE_TO_ODOT = {
    1:  ("longitudinal_cracking",       5),    # D00
    2:  ("block_transverse_cracking",  10),    # D10
    3:  ("wheel_track_cracking",       15),    # D20
    4:  ("bleeding",                    5),    # D30
    5:  ("raveling",                   10),    # D40
    6:  ("debonding",                   5),    # D50
    7:  ("block_transverse_cracking",  10),    # D60
    8:  ("longitudinal_cracking",      10),    # D70
    9:  ("patching",                    5),    # D80
    10: ("raveling",                   10),    # D90
}

# ---------------------------------------------------------------------------
# ODOT severity weights (from Flexible-Pavement-Forms.pdf p.7)
# Keys: L = Low, M = Medium, H = High
# ---------------------------------------------------------------------------
SEVERITY_WEIGHTS = {
    "raveling":                  {"L": 0.3, "M": 0.6, "H": 1.0},
    "bleeding":                  {"L": 0.8, "M": 0.8, "H": 1.0},
    "patching":                  {"L": 0.3, "M": 0.6, "H": 1.0},
    "debonding":                 {"L": 0.4, "M": 0.7, "H": 1.0},
    "wheel_track_cracking":      {"L": 0.4, "M": 0.7, "H": 1.0},
    "block_transverse_cracking": {"L": 0.4, "M": 0.7, "H": 1.0},
    "longitudinal_cracking":     {"L": 0.4, "M": 0.7, "H": 1.0},
}



# ---- Helper functions -----------------------------------------------------

def load_coco_json(json_path: Path) -> dict:
    """Load a COCO-format JSON annotation file."""
    with open(json_path, "r", encoding="utf-8") as f:
        data = json.load(f)
    return data


def severity_from_area(bbox_area: float) -> str:
    """Map bbox area ratio (bbox_area / image_area) to severity level."""
    image_area = CONFIG["IMAGE_AREA"]
    ratio = bbox_area / image_area
    thresholds = CONFIG["SEVERITY_AREA_THRESHOLDS"]
    if ratio < thresholds["L"]:
        return "L"
    elif ratio < thresholds["M"]:
        return "M"
    else:
        return "H"


def pcr_class(total_deduct: float, thresholds: dict) -> int:
    """Map total deduction to a 3-class integer label using fixed thresholds."""
    if total_deduct >= thresholds["low_min"]:
        return 0   # low quality
    elif total_deduct >= thresholds["high_max"]:
        return 1   # medium quality
    else:
        return 2   # high quality


# ---- Core PCR computation -------------------------------------------------

def compute_pcr_for_images(
    coco_data: dict,
    image_dir: Path,
    original_split: str,
) -> list[dict]:
    """
    Compute a PCR-like score for every image in a COCO annotation file.

    Parameters
    ----------
    coco_data : dict
        Parsed COCO JSON with keys "images", "annotations", "categories".
    image_dir : Path
        Directory containing the image files (same dir as the JSON).
    original_split : str
        Label for the original COCO split ("train" or "valid").

    Returns
    -------
    list[dict]
        One dict per image with keys: image_id, filename, pcr_score,
        int_label, n_annotations, total_deduct, image_path, original_split.
    """
    # Build lookup: image_id -> image metadata
    image_lookup = {img["id"]: img for img in coco_data["images"]}

    # Group annotations by image_id, filtering out category 0 (parent)
    annots_by_image: dict[int, list[dict]] = defaultdict(list)
    for ann in coco_data["annotations"]:
        cat_id = ann["category_id"]
        if cat_id == 0:
            continue
        if cat_id not in DAMAGE_TO_ODOT:
            continue
        annots_by_image[ann["image_id"]].append(ann)

    results = []

    for img_id, img_meta in image_lookup.items():
        filename = img_meta["file_name"]
        image_path = image_dir / filename
        annotations = annots_by_image.get(img_id, [])
        n_annotations = len(annotations)

        if n_annotations == 0:
            # No damage detected -> perfect road (int_label assigned in main)
            results.append({
                "image_id": img_id,
                "filename": filename,
                "pcr_score": 100.0,
                "int_label": -1,
                "n_annotations": 0,
                "total_deduct": 0.0,
                "image_path": str(image_path),
                "original_split": original_split,
            })
            continue

        # Per-annotation deductions (matching pcr_score.jl lines 88-121)
        # Each bbox independently contributes distress_weight * severity_weight
        total_deduct = 0.0
        for ann in annotations:
            cat_id = ann["category_id"]
            odot_type, d_weight = DAMAGE_TO_ODOT[cat_id]
            sev = severity_from_area(ann["area"])
            s_weight = SEVERITY_WEIGHTS[odot_type][sev]
            total_deduct += d_weight * s_weight

        pcr_score = max(0.0, 100.0 - total_deduct)

        # int_label assigned in main() using deduction thresholds
        results.append({
            "image_id": img_id,
            "filename": filename,
            "pcr_score": round(pcr_score, 4),
            "int_label": -1,
            "n_annotations": n_annotations,
            "total_deduct": round(total_deduct, 4),
            "image_path": str(image_path),
            "original_split": original_split,
        })

    return results


# ---- Stratified split ------------------------------------------------------

def stratified_split(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """
    Split dataframe into train/val/test (70/15/15) stratified by int_label.
    """
    rs = CONFIG["RANDOM_STATE"]

    # First split: 70% train, 30% temp
    df_train, df_temp = train_test_split(
        df,
        test_size=CONFIG["TEST_SIZE"],
        stratify=df["int_label"],
        random_state=rs,
    )

    # Second split: 50/50 of the 30% -> 15% val, 15% test
    df_val, df_test = train_test_split(
        df_temp,
        test_size=CONFIG["VAL_TEST_SIZE"],
        stratify=df_temp["int_label"],
        random_state=rs,
    )

    # Add split column
    df_train = df_train.copy()
    df_val = df_val.copy()
    df_test = df_test.copy()
    df_train["split"] = "train"
    df_val["split"] = "val"
    df_test["split"] = "test"

    return df_train, df_val, df_test


# ---- Diagnostics -----------------------------------------------------------

def print_diagnostics(df: pd.DataFrame, label: str) -> None:
    """Print class distribution and PCR summary stats for a dataframe."""
    n = len(df)
    class_counts = df["int_label"].value_counts().sort_index()
    class_names = {0: "low", 1: "medium", 2: "high"}

    print(f"\n  {label} (n={n})")
    print(f"  {'Class':<10} {'Count':>6} {'Pct':>8}")
    print(f"  {'-'*26}")
    for cls_id in sorted(class_counts.index):
        cnt = class_counts[cls_id]
        pct = 100.0 * cnt / n if n > 0 else 0.0
        name = class_names.get(cls_id, str(cls_id))
        print(f"  {cls_id} ({name:<6}) {cnt:>6} {pct:>7.1f}%")

    pcr = df["pcr_score"]
    print(f"\n  PCR score stats:")
    print(f"    mean   = {pcr.mean():.2f}")
    print(f"    median = {pcr.median():.2f}")
    print(f"    std    = {pcr.std():.2f}")
    print(f"    min    = {pcr.min():.2f}")
    print(f"    max    = {pcr.max():.2f}")

    # Per-class annotation count stats (visual separability diagnostic)
    if "n_annotations" in df.columns:
        print(f"\n  Per-class annotation counts:")
        print(f"  {'Class':<10} {'Mean':>8} {'Median':>8}")
        print(f"  {'-'*28}")
        for cls_id in sorted(class_counts.index):
            name = class_names.get(cls_id, str(cls_id))
            subset = df.loc[df["int_label"] == cls_id, "n_annotations"]
            print(f"  {cls_id} ({name:<6}) {subset.mean():>8.1f} {subset.median():>8.1f}")

    # Warn if any class has fewer than 100 samples
    for cls_id in sorted(class_counts.index):
        if class_counts[cls_id] < 100:
            name = class_names.get(cls_id, str(cls_id))
            print(
                f"\n  WARNING: class {cls_id} ({name}) has only "
                f"{class_counts[cls_id]} samples (< 100)"
            )


# ---- Main ------------------------------------------------------------------

def main():
    print("=" * 70)
    print("RDD2024 -> ODOT PCR-GROUNDED 3-CLASS LABELS")
    print("=" * 70)

    # ------------------------------------------------------------------
    # 1. Load COCO annotations
    # ------------------------------------------------------------------
    print("\n[1/6] Loading COCO annotations ...")

    train_coco = load_coco_json(TRAIN_JSON)
    valid_coco = load_coco_json(VALID_JSON)

    n_train_imgs = len(train_coco["images"])
    n_valid_imgs = len(valid_coco["images"])
    n_train_anns = len(train_coco["annotations"])
    n_valid_anns = len(valid_coco["annotations"])

    print(f"  Train: {n_train_imgs} images, {n_train_anns} annotations")
    print(f"  Valid: {n_valid_imgs} images, {n_valid_anns} annotations")

    # ------------------------------------------------------------------
    # 2. Compute PCR scores for each image
    # ------------------------------------------------------------------
    print("\n[2/6] Computing PCR scores ...")

    train_dir = TRAIN_JSON.parent
    valid_dir = VALID_JSON.parent

    train_results = compute_pcr_for_images(train_coco, train_dir, "train")
    valid_results = compute_pcr_for_images(valid_coco, valid_dir, "valid")

    all_results = train_results + valid_results
    df = pd.DataFrame(all_results)

    print(f"  Total images processed: {len(df)}")
    print(f"    from train split: {len(train_results)}")
    print(f"    from valid split: {len(valid_results)}")

    # ------------------------------------------------------------------
    # 3. Assign class labels using fixed deduction thresholds
    # ------------------------------------------------------------------
    print("\n[3/6] Assigning class labels via fixed deduction thresholds ...")

    thresholds = CONFIG["DEDUCT_THRESHOLDS"]
    print(f"  Deduction thresholds:")
    print(f"    deduct >= {thresholds['low_min']} -> class 0 (low quality)")
    print(f"    {thresholds['high_max']} <= deduct < {thresholds['low_min']} -> class 1 (medium quality)")
    print(f"    deduct < {thresholds['high_max']} -> class 2 (high quality)")

    pcr_scores = df["pcr_score"].values
    deducts = df["total_deduct"].values
    print(f"  PCR score range: [{pcr_scores.min():.2f}, {pcr_scores.max():.2f}]")
    print(f"  Deduction range: [{deducts.min():.2f}, {deducts.max():.2f}]")

    # Assign class labels using fixed deduction thresholds
    df["int_label"] = df["total_deduct"].apply(lambda x: pcr_class(x, thresholds))

    # ------------------------------------------------------------------
    # 4. Save full labelled dataset
    # ------------------------------------------------------------------
    print("\n[4/6] Saving full labelled dataset ...")

    column_order = [
        "image_id", "filename", "pcr_score", "int_label",
        "n_annotations", "total_deduct", "image_path", "original_split",
    ]
    df = df[column_order]
    df.to_csv(OUTPUT_ALL, index=False)
    print(f"  Saved: {OUTPUT_ALL}")
    print(f"  Rows:  {len(df)}")

    # ------------------------------------------------------------------
    # 5. Stratified train/val/test split
    # ------------------------------------------------------------------
    print("\n[5/6] Creating stratified train/val/test splits (70/15/15) ...")

    df_train, df_val, df_test = stratified_split(df)

    # Add split column to column order for split files
    split_cols = column_order + ["split"]

    df_train[split_cols].to_csv(OUTPUT_TRAIN, index=False)
    df_val[split_cols].to_csv(OUTPUT_VAL, index=False)
    df_test[split_cols].to_csv(OUTPUT_TEST, index=False)

    print(f"  Train: {len(df_train)} rows -> {OUTPUT_TRAIN}")
    print(f"  Val:   {len(df_val)} rows -> {OUTPUT_VAL}")
    print(f"  Test:  {len(df_test)} rows -> {OUTPUT_TEST}")

    # ------------------------------------------------------------------
    # 6. Diagnostics
    # ------------------------------------------------------------------
    print("\n[6/6] Diagnostics")
    print("=" * 70)

    print_diagnostics(df, "ALL DATA")
    print_diagnostics(df_train, "TRAIN")
    print_diagnostics(df_val, "VALIDATION")
    print_diagnostics(df_test, "TEST")

    print("\n" + "=" * 70)
    print("Done.")
    print("=" * 70)


if __name__ == "__main__":
    main()
