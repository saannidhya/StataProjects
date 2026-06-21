"""
File: 4.6_prep_streetview_model_data.py
Author: Saani Rawat
Date: 11 Mar 2026
Purpose:
    Prepare a YOLO detection dataset for streetview road-damage fine-tuning.

    Legacy filename retained for numbering continuity. This script no longer
    collapses USA RDD2024 defect boxes into heuristic PCR image labels.
    Instead, it:

    1. Reads the USA COCO annotations from RDD2024.
    2. Converts the original defect boxes into YOLO detection labels.
    3. Splits the original USA train split into detector train/val.
    4. Keeps the original USA valid split as the detector test split.
    5. Adds manually reviewed Ohio clean-road negatives as empty-label images.
    6. Writes a self-contained YOLO dataset with manifests and summary files.

Usage:
    python 4.6_prep_streetview_model_data.py --overwrite
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import random
import shutil
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path
from PIL import Image


ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = ROOT / "data" / "roads"


def resolve_training_dir(data_dir: Path) -> Path:
    candidates = [
        data_dir / "N-RDD2024Road damage and defects" / "Training and Validation Dataset",
        data_dir / "N-RDD2024Road damage and defects - v5" / "Training and Validation Dataset",
    ]
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return candidates[0]


TRAINING_DIR = resolve_training_dir(DATA_DIR)
USA_COCO_DIR = TRAINING_DIR / "USA_coco"

TRAIN_JSON = USA_COCO_DIR / "train" / "_annotations.coco.json"
VALID_JSON = USA_COCO_DIR / "valid" / "_annotations.coco.json"

NEGATIVE_REVIEW_DIR = (
    TRAINING_DIR
    / "manual_negative_review_ohio_streetview_seed42_n200_2026-03-11"
    / "candidates"
)

DEFAULT_OUTPUT_ROOT = TRAINING_DIR / "USA_yolo_detection_with_clean_negatives"

CLASS_NAMES = {
    0: "D00",
    1: "D10",
    2: "D20",
    3: "D30",
    4: "D40",
    5: "D50",
    6: "D60",
    7: "D70",
    8: "D80",
    9: "D90",
}

COCO_TO_YOLO = {
    1: 0,
    2: 1,
    3: 2,
    4: 3,
    5: 4,
    6: 5,
    7: 6,
    8: 7,
    9: 8,
    10: 9,
}


@dataclass(frozen=True)
class ImageRecord:
    filename: str
    source_path: Path
    label_lines: tuple[str, ...]
    class_ids: tuple[int, ...]
    source_dataset: str
    source_split: str
    is_negative: bool

    @property
    def n_boxes(self) -> int:
        return len(self.label_lines)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Prepare a YOLO detection dataset from USA RDD2024 COCO + clean-road negatives."
    )
    parser.add_argument(
        "--output_root",
        type=str,
        default=str(DEFAULT_OUTPUT_ROOT),
        help="Directory where the prepared YOLO dataset will be written.",
    )
    parser.add_argument(
        "--negatives_dir",
        type=str,
        default=str(NEGATIVE_REVIEW_DIR),
        help="Directory containing manually reviewed clean-road negative JPGs.",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed for split generation.",
    )
    parser.add_argument(
        "--val_frac",
        type=float,
        default=0.15,
        help="Fraction of original USA train images reserved for detector validation.",
    )
    parser.add_argument(
        "--neg_train_frac",
        type=float,
        default=0.70,
        help="Fraction of curated negatives assigned to train.",
    )
    parser.add_argument(
        "--neg_val_frac",
        type=float,
        default=0.15,
        help="Fraction of curated negatives assigned to val.",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Delete output_root first if it already exists.",
    )
    return parser.parse_args()


def load_coco_json(path: Path) -> dict:
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def is_valid_image(path: Path) -> bool:
    try:
        with Image.open(path) as img:
            img.verify()
        return True
    except Exception:
        return False


def coco_bbox_to_yolo_line(
    coco_bbox: list[float],
    width: float,
    height: float,
    class_id: int,
) -> str:
    x, y, w, h = coco_bbox
    x_center = (x + (w / 2.0)) / width
    y_center = (y + (h / 2.0)) / height
    w_norm = w / width
    h_norm = h / height

    # Clip to valid YOLO ranges. Roboflow export should already be valid, but
    # clipping avoids broken labels when bbox edges land exactly on boundaries.
    x_center = min(max(x_center, 0.0), 1.0)
    y_center = min(max(y_center, 0.0), 1.0)
    w_norm = min(max(w_norm, 0.0), 1.0)
    h_norm = min(max(h_norm, 0.0), 1.0)

    return f"{class_id} {x_center:.6f} {y_center:.6f} {w_norm:.6f} {h_norm:.6f}"


def load_positive_records(json_path: Path, source_split: str) -> tuple[list[ImageRecord], dict]:
    coco = load_coco_json(json_path)
    image_dir = json_path.parent

    anns_by_image: dict[int, list[dict]] = defaultdict(list)
    for ann in coco["annotations"]:
        coco_class = int(ann["category_id"])
        if coco_class not in COCO_TO_YOLO:
            continue
        anns_by_image[int(ann["image_id"])].append(ann)

    records: list[ImageRecord] = []
    skipped_images: list[str] = []
    box_counts = Counter()
    image_class_counts = Counter()

    for img in coco["images"]:
        image_id = int(img["id"])
        filename = str(img["file_name"])
        width = float(img["width"])
        height = float(img["height"])
        source_path = image_dir / filename

        if not source_path.exists():
            skipped_images.append(filename)
            continue
        if not is_valid_image(source_path):
            skipped_images.append(filename)
            continue

        label_lines: list[str] = []
        class_ids: list[int] = []
        for ann in anns_by_image.get(image_id, []):
            yolo_class = COCO_TO_YOLO[int(ann["category_id"])]
            label_lines.append(
                coco_bbox_to_yolo_line(ann["bbox"], width, height, yolo_class)
            )
            class_ids.append(yolo_class)
            box_counts[yolo_class] += 1

        if not label_lines:
            skipped_images.append(filename)
            continue

        uniq_class_ids = tuple(sorted(set(class_ids)))
        for class_id in uniq_class_ids:
            image_class_counts[class_id] += 1

        records.append(
            ImageRecord(
                filename=filename,
                source_path=source_path,
                label_lines=tuple(label_lines),
                class_ids=uniq_class_ids,
                source_dataset="rdd2024_usa",
                source_split=source_split,
                is_negative=False,
            )
        )

    diag = {
        "n_images_kept": len(records),
        "n_images_skipped": len(skipped_images),
        "skipped_examples": skipped_images[:10],
        "box_counts": dict(sorted(box_counts.items())),
        "image_class_counts": dict(sorted(image_class_counts.items())),
    }
    return records, diag


def choose_validation_filenames(
    records: list[ImageRecord],
    val_frac: float,
    seed: int,
) -> set[str]:
    if not records:
        return set()

    rng = random.Random(seed)
    target_val = max(len(CLASS_NAMES), round(len(records) * val_frac))
    target_val = min(target_val, len(records))

    by_class: dict[int, list[ImageRecord]] = defaultdict(list)
    for record in records:
        for class_id in record.class_ids:
            by_class[class_id].append(record)

    val_filenames: set[str] = set()

    # Seed validation with at least one image per class, prioritizing rare classes.
    class_order = sorted(CLASS_NAMES, key=lambda cls: len(by_class[cls]))
    for class_id in class_order:
        candidates = [r for r in by_class[class_id] if r.filename not in val_filenames]
        if not candidates or len(val_filenames) >= target_val:
            continue
        val_filenames.add(rng.choice(candidates).filename)

    remaining = [r.filename for r in records if r.filename not in val_filenames]
    rng.shuffle(remaining)
    needed = max(0, target_val - len(val_filenames))
    val_filenames.update(remaining[:needed])
    return val_filenames


def split_negative_records(
    negatives_dir: Path,
    seed: int,
    train_frac: float,
    val_frac: float,
) -> dict[str, list[ImageRecord]]:
    if not negatives_dir.exists():
        raise FileNotFoundError(f"Negative review directory not found: {negatives_dir}")

    negative_paths = [p for p in sorted(negatives_dir.glob("*.jpg")) if is_valid_image(p)]
    if not negative_paths:
        raise FileNotFoundError(
            f"No curated negative JPGs found in: {negatives_dir}"
        )

    rng = random.Random(seed)
    shuffled = negative_paths[:]
    rng.shuffle(shuffled)

    n_total = len(shuffled)
    n_train = int(round(n_total * train_frac))
    n_val = int(round(n_total * val_frac))
    n_train = min(n_train, n_total)
    n_val = min(n_val, max(0, n_total - n_train))
    n_test = n_total - n_train - n_val

    split_paths = {
        "train": shuffled[:n_train],
        "val": shuffled[n_train:n_train + n_val],
        "test": shuffled[n_train + n_val:n_train + n_val + n_test],
    }

    split_records: dict[str, list[ImageRecord]] = {}
    for split_name, paths in split_paths.items():
        split_records[split_name] = [
            ImageRecord(
                filename=path.name,
                source_path=path,
                label_lines=tuple(),
                class_ids=tuple(),
                source_dataset="ohio_clean_negative",
                source_split="manual_review",
                is_negative=True,
            )
            for path in paths
        ]
    return split_records


def ensure_clean_output(output_root: Path, overwrite: bool) -> None:
    if output_root.exists():
        if not overwrite:
            raise FileExistsError(
                f"Output directory already exists: {output_root}\n"
                "Re-run with --overwrite to rebuild it."
            )
        shutil.rmtree(output_root)

    for split_name in ("train", "val", "test"):
        (output_root / "images" / split_name).mkdir(parents=True, exist_ok=True)
        (output_root / "labels" / split_name).mkdir(parents=True, exist_ok=True)
    (output_root / "manifests").mkdir(parents=True, exist_ok=True)


def copy_record_to_split(
    record: ImageRecord,
    split_name: str,
    output_root: Path,
) -> tuple[Path, Path]:
    image_dst = output_root / "images" / split_name / record.filename
    label_dst = output_root / "labels" / split_name / f"{Path(record.filename).stem}.txt"

    try:
        os.link(record.source_path, image_dst)
    except OSError:
        shutil.copyfile(record.source_path, image_dst)
    with open(label_dst, "w", encoding="utf-8", newline="\n") as f:
        if record.label_lines:
            f.write("\n".join(record.label_lines))
            f.write("\n")

    return image_dst, label_dst


def build_split_manifests(
    positive_train: list[ImageRecord],
    positive_val: list[ImageRecord],
    positive_test: list[ImageRecord],
    negative_splits: dict[str, list[ImageRecord]],
) -> dict[str, list[ImageRecord]]:
    split_records = {
        "train": positive_train + negative_splits["train"],
        "val": positive_val + negative_splits["val"],
        "test": positive_test + negative_splits["test"],
    }
    return split_records


def write_dataset_yaml(output_root: Path) -> Path:
    yaml_path = output_root / "streetview_det_dataset.yaml"
    yaml_lines = [
        "path: .",
        "train: images/train",
        "val: images/val",
        "test: images/test",
        "names:",
    ]
    for class_id, class_name in CLASS_NAMES.items():
        yaml_lines.append(f"  {class_id}: {class_name}")

    with open(yaml_path, "w", encoding="utf-8", newline="\n") as f:
        f.write("\n".join(yaml_lines))
        f.write("\n")
    return yaml_path


def summarize_split(records: list[ImageRecord]) -> dict:
    box_counts = Counter()
    image_class_counts = Counter()
    n_negative = 0

    for record in records:
        if record.is_negative:
            n_negative += 1
            continue
        for class_id in record.class_ids:
            image_class_counts[class_id] += 1
        for line in record.label_lines:
            class_id = int(line.split()[0])
            box_counts[class_id] += 1

    return {
        "n_images": len(records),
        "n_negative_images": n_negative,
        "n_positive_images": len(records) - n_negative,
        "n_boxes": int(sum(box_counts.values())),
        "box_counts": {CLASS_NAMES[k]: int(v) for k, v in sorted(box_counts.items())},
        "image_class_counts": {
            CLASS_NAMES[k]: int(v) for k, v in sorted(image_class_counts.items())
        },
    }


def main() -> None:
    args = parse_args()
    output_root = Path(args.output_root)
    negatives_dir = Path(args.negatives_dir)

    if args.neg_train_frac < 0 or args.neg_val_frac < 0:
        raise ValueError("Negative split fractions must be non-negative.")
    if args.neg_train_frac + args.neg_val_frac > 1.0:
        raise ValueError("neg_train_frac + neg_val_frac must be <= 1.0.")

    print("=" * 78)
    print("RDD2024 STREETVIEW DETECTION DATASET PREP")
    print("=" * 78)
    print(f"  Output root:    {output_root}")
    print(f"  Negatives dir:  {negatives_dir}")
    print(f"  Seed:           {args.seed}")
    print(f"  Val fraction:   {args.val_frac}")
    print(f"  Neg split:      train={args.neg_train_frac:.2f}, val={args.neg_val_frac:.2f}, "
          f"test={1.0 - args.neg_train_frac - args.neg_val_frac:.2f}")

    ensure_clean_output(output_root, overwrite=args.overwrite)

    usa_train_records, train_diag = load_positive_records(TRAIN_JSON, "train")
    usa_test_records, test_diag = load_positive_records(VALID_JSON, "valid")

    val_filenames = choose_validation_filenames(
        usa_train_records,
        val_frac=args.val_frac,
        seed=args.seed,
    )

    positive_train = [r for r in usa_train_records if r.filename not in val_filenames]
    positive_val = [r for r in usa_train_records if r.filename in val_filenames]
    positive_test = usa_test_records

    negative_splits = split_negative_records(
        negatives_dir=negatives_dir,
        seed=args.seed,
        train_frac=args.neg_train_frac,
        val_frac=args.neg_val_frac,
    )

    split_records = build_split_manifests(
        positive_train=positive_train,
        positive_val=positive_val,
        positive_test=positive_test,
        negative_splits=negative_splits,
    )

    manifest_rows: list[dict] = []
    for split_name, records in split_records.items():
        print(f"\n[{split_name}] copying {len(records)} images ...")
        for record in records:
            image_dst, label_dst = copy_record_to_split(record, split_name, output_root)
            manifest_rows.append(
                {
                    "split": split_name,
                    "filename": record.filename,
                    "source_path": str(record.source_path),
                    "image_path": str(image_dst),
                    "image_rel_path": image_dst.relative_to(output_root).as_posix(),
                    "label_path": str(label_dst),
                    "label_rel_path": label_dst.relative_to(output_root).as_posix(),
                    "source_dataset": record.source_dataset,
                    "source_split": record.source_split,
                    "is_negative": int(record.is_negative),
                    "n_boxes": record.n_boxes,
                    "classes": "|".join(CLASS_NAMES[c] for c in record.class_ids),
                }
            )

    manifest_path = output_root / "manifests" / "dataset_manifest.csv"
    with open(manifest_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(manifest_rows[0].keys()))
        writer.writeheader()
        writer.writerows(manifest_rows)

    summary = {
        "seed": args.seed,
        "output_root": str(output_root),
        "yaml_path": str(write_dataset_yaml(output_root)),
        "train_source_diagnostics": train_diag,
        "test_source_diagnostics": test_diag,
        "splits": {
            split_name: summarize_split(records)
            for split_name, records in split_records.items()
        },
    }

    summary_path = output_root / "manifests" / "dataset_summary.json"
    with open(summary_path, "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2)

    print("\nSummary")
    print("-" * 78)
    for split_name in ("train", "val", "test"):
        split_summary = summary["splits"][split_name]
        print(
            f"  {split_name:5s}: {split_summary['n_images']:4d} images | "
            f"{split_summary['n_positive_images']:4d} positives | "
            f"{split_summary['n_negative_images']:3d} negatives | "
            f"{split_summary['n_boxes']:5d} boxes"
        )

    print("\nArtifacts")
    print("-" * 78)
    print(f"  YAML:     {summary['yaml_path']}")
    print(f"  Manifest: {manifest_path}")
    print(f"  Summary:  {summary_path}")
    print("\nDone.")


if __name__ == "__main__":
    main()
