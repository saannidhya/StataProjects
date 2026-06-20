"""
File: 4.7_train_streetview_model.py
Author: Saani Rawat
Date: 11 Mar 2026
Purpose:
    Fine-tune a YOLO detector on the USA RDD2024 streetview defect dataset,
    augmented with manually curated clean-road negatives from Ohio.

    This replaces the earlier image-classification workflow. The model now
    learns the two tasks that matter for downstream streetview scoring:

    1. Locate each road defect.
    2. Classify each located defect into the original RDD2024 defect class.

Usage:
    python 4.7_train_streetview_model.py
    python 4.7_train_streetview_model.py --model yolo11l.pt --epochs 150 --device 0
    python 4.7_train_streetview_model.py --resume --run_name streetview_det_yolo11m
    python 4.7_train_streetview_model.py --eval_only --weights path/to/best.pt
"""

from __future__ import annotations

import argparse
import csv
import json
import math
from collections import Counter
from pathlib import Path

from ultralytics import YOLO
import yaml
from PIL import Image


def resolve_root() -> Path:
    file_root = Path(__file__).resolve().parents[1]
    drive_root = Path("/content/drive/MyDrive/ohio_taxation")
    if drive_root.exists():
        return drive_root
    return file_root


def resolve_training_dir(data_dir: Path) -> Path:
    candidates = [
        data_dir / "N-RDD2024Road damage and defects" / "Training and Validation Dataset",
        data_dir / "N-RDD2024Road damage and defects - v5" / "Training and Validation Dataset",
    ]
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return candidates[0]


ROOT = resolve_root()
DATA_DIR = ROOT / "data" / "roads"
TRAINING_DIR = resolve_training_dir(DATA_DIR)
PREPARED_DATA_ROOT = TRAINING_DIR / "USA_yolo_detection_with_clean_negatives"
DEFAULT_DATA_YAML = PREPARED_DATA_ROOT / "streetview_det_dataset.yaml"
DEFAULT_MANIFEST = PREPARED_DATA_ROOT / "manifests" / "dataset_manifest.csv"

DEFAULT_PROJECT = DATA_DIR / "runs_ohio" / "yolo11_rdd2024_streetview_detector"
DEFAULT_RUN_NAME = "streetview_det_yolo11m"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Fine-tune a YOLO streetview defect detector on RDD2024 USA + clean negatives."
    )
    parser.add_argument(
        "--data",
        type=str,
        default=str(DEFAULT_DATA_YAML),
        help="Path to the YOLO dataset YAML created by 4.6.",
    )
    parser.add_argument(
        "--manifest",
        type=str,
        default=str(DEFAULT_MANIFEST),
        help="Dataset manifest created by 4.6 (used for negative holdout evaluation).",
    )
    parser.add_argument(
        "--model",
        type=str,
        default="yolo11m.pt",
        help="Pretrained YOLO detection checkpoint to fine-tune.",
    )
    parser.add_argument(
        "--project",
        type=str,
        default=str(DEFAULT_PROJECT),
        help="Ultralytics project directory for training runs.",
    )
    parser.add_argument(
        "--run_name",
        type=str,
        default=DEFAULT_RUN_NAME,
        help="Ultralytics run name.",
    )
    parser.add_argument(
        "--epochs",
        type=int,
        default=100,
        help="Training epochs.",
    )
    parser.add_argument(
        "--imgsz",
        type=int,
        default=640,
        help="Training and evaluation image size.",
    )
    parser.add_argument(
        "--batch",
        type=int,
        default=16,
        help="Batch size.",
    )
    parser.add_argument(
        "--device",
        type=str,
        default="cpu",
        help="Ultralytics device string, e.g. 'cpu', '0', '0,1'.",
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=4,
        help="DataLoader workers.",
    )
    parser.add_argument(
        "--patience",
        type=int,
        default=20,
        help="Early stopping patience.",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed.",
    )
    parser.add_argument(
        "--conf",
        type=float,
        default=0.001,
        help="Confidence threshold for detector mAP evaluation.",
    )
    parser.add_argument(
        "--iou",
        type=float,
        default=0.60,
        help="IoU threshold for negative-only prediction diagnostics.",
    )
    parser.add_argument(
        "--neg_conf",
        type=float,
        default=0.25,
        help="Confidence threshold for negative-only false-positive diagnostics.",
    )
    parser.add_argument(
        "--exist_ok",
        action="store_true",
        help="Reuse an existing Ultralytics run directory.",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Resume training from a saved last.pt checkpoint in the run directory.",
    )
    parser.add_argument(
        "--resume_from",
        type=str,
        default="",
        help="Optional explicit last.pt checkpoint to resume from.",
    )
    parser.add_argument(
        "--eval_only",
        action="store_true",
        help="Skip training and only run validation/test/negative-holdout evaluation.",
    )
    parser.add_argument(
        "--weights",
        type=str,
        default="",
        help="Checkpoint to evaluate when --eval_only is set. Defaults to best.pt in the run directory.",
    )
    parser.add_argument(
        "--no_oversample",
        action="store_true",
        help="Disable rare-class oversampling for the training split.",
    )
    parser.add_argument(
        "--oversample_target_images",
        type=int,
        default=300,
        help="Target minimum image count per class when building the oversampled train list.",
    )
    parser.add_argument(
        "--oversample_max_repeat",
        type=int,
        default=8,
        help="Maximum repeat factor for any single training image in the oversampled train list.",
    )
    return parser.parse_args()


def metrics_payload(metrics) -> dict:
    payload: dict[str, object] = {}
    box = getattr(metrics, "box", None)
    if box is not None:
        payload["precision"] = float(box.mp)
        payload["recall"] = float(box.mr)
        payload["map50"] = float(box.map50)
        payload["map75"] = float(box.map75)
        payload["map50_95"] = float(box.map)
    payload["results_dict"] = {
        str(k): float(v) for k, v in getattr(metrics, "results_dict", {}).items()
    }
    payload["speed_ms"] = {
        str(k): float(v) for k, v in getattr(metrics, "speed", {}).items()
    }
    try:
        payload["per_class"] = make_jsonable(metrics.summary(normalize=True, decimals=6))
    except Exception:
        payload["per_class"] = []
    return payload


def write_per_class_csv(metrics, out_csv: Path) -> None:
    try:
        rows = metrics.summary(normalize=True, decimals=6)
    except Exception:
        rows = []
    if not rows:
        return
    with open(out_csv, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def lookup_label(names, class_id: int) -> str:
    if isinstance(names, dict):
        return str(names.get(class_id, class_id))
    if isinstance(names, list):
        if 0 <= class_id < len(names):
            return str(names[class_id])
    return str(class_id)


def make_jsonable(obj):
    if isinstance(obj, dict):
        return {str(k): make_jsonable(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [make_jsonable(v) for v in obj]
    if isinstance(obj, Path):
        return str(obj)
    if hasattr(obj, "item"):
        try:
            return obj.item()
        except Exception:
            pass
    if hasattr(obj, "tolist") and not isinstance(obj, (str, bytes)):
        try:
            return obj.tolist()
        except Exception:
            pass
    return obj


def is_valid_image(path: Path) -> bool:
    try:
        with Image.open(path) as img:
            img.verify()
        return True
    except Exception:
        return False


def materialize_runtime_data_yaml(data_yaml: Path) -> Path:
    with open(data_yaml, "r", encoding="utf-8") as f:
        cfg = yaml.safe_load(f)

    if not isinstance(cfg, dict):
        return data_yaml

    data_root = data_yaml.parent.resolve()
    path_value = cfg.get("path", ".")
    if path_value in (None, "", "."):
        cfg["path"] = str(data_root)
    else:
        path_obj = Path(str(path_value))
        cfg["path"] = str(path_obj if path_obj.is_absolute() else (data_root / path_obj).resolve())

    runtime_stem = data_yaml.stem
    if not runtime_stem.endswith("_runtime_abs"):
        runtime_stem = f"{runtime_stem}_runtime_abs"
    runtime_yaml = data_yaml.with_name(f"{runtime_stem}.yaml")
    with open(runtime_yaml, "w", encoding="utf-8", newline="\n") as f:
        yaml.safe_dump(cfg, f, sort_keys=False)

    return runtime_yaml


def build_oversampled_train_file(
    manifest_path: Path,
    dataset_root: Path,
    target_images: int,
    max_repeat: int,
) -> tuple[Path, dict]:
    rows: list[dict] = []
    class_image_counts = Counter()
    invalid_images: list[str] = []

    with open(manifest_path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            if row.get("split") != "train":
                continue

            rel_path = (row.get("image_rel_path") or "").strip()
            abs_path = (row.get("image_path") or "").strip()
            candidates: list[Path] = []
            if rel_path:
                candidates.append(dataset_root / Path(rel_path))
            if abs_path:
                candidates.append(Path(abs_path))

            image_path = None
            for candidate in candidates:
                if candidate.exists():
                    image_path = candidate.resolve()
                    break
            if image_path is None:
                continue
            if not is_valid_image(image_path):
                invalid_images.append(str(image_path))
                continue

            is_negative = row.get("is_negative") == "1"
            class_names = [c for c in row.get("classes", "").split("|") if c]
            if not is_negative:
                for class_name in set(class_names):
                    class_image_counts[class_name] += 1

            rows.append(
                {
                    "image_path": image_path,
                    "classes": class_names,
                    "is_negative": is_negative,
                }
            )

    repeat_by_class: dict[str, int] = {}
    for class_name, count in class_image_counts.items():
        if count <= 0:
            repeat_by_class[class_name] = 1
            continue
        repeat_by_class[class_name] = min(
            max_repeat,
            max(1, math.ceil(target_images / count)),
        )

    train_list_path = dataset_root / "train_oversampled.txt"
    repeat_histogram = Counter()
    train_entries: list[str] = []
    for row in rows:
        if row["is_negative"] or not row["classes"]:
            repeat = 1
        else:
            repeat = max(repeat_by_class.get(class_name, 1) for class_name in row["classes"])
        repeat_histogram[repeat] += 1
        train_entries.extend([str(row["image_path"])] * repeat)

    with open(train_list_path, "w", encoding="utf-8", newline="\n") as f:
        for image_path in train_entries:
            f.write(f"{image_path}\n")

    info = {
        "train_list_path": str(train_list_path),
        "n_unique_train_images": len(rows),
        "n_invalid_train_images": len(invalid_images),
        "invalid_train_examples": invalid_images[:10],
        "n_train_entries": len(train_entries),
        "repeat_by_class": dict(sorted(repeat_by_class.items())),
        "repeat_histogram": dict(sorted(repeat_histogram.items())),
    }
    return train_list_path, info


def materialize_training_data_yaml(
    runtime_data_yaml: Path,
    manifest_path: Path,
    enable_oversample: bool,
    target_images: int,
    max_repeat: int,
) -> tuple[Path, dict | None]:
    if not enable_oversample:
        return runtime_data_yaml, None

    with open(runtime_data_yaml, "r", encoding="utf-8") as f:
        cfg = yaml.safe_load(f)
    if not isinstance(cfg, dict):
        return runtime_data_yaml, None

    dataset_root = runtime_data_yaml.parent.resolve()
    train_list_path, info = build_oversampled_train_file(
        manifest_path=manifest_path,
        dataset_root=dataset_root,
        target_images=target_images,
        max_repeat=max_repeat,
    )

    cfg["train"] = str(train_list_path)
    training_yaml = runtime_data_yaml.with_name(f"{runtime_data_yaml.stem}_oversampled.yaml")
    with open(training_yaml, "w", encoding="utf-8", newline="\n") as f:
        yaml.safe_dump(cfg, f, sort_keys=False)

    return training_yaml, info


def read_negative_test_paths(manifest_path: Path) -> list[Path]:
    if not manifest_path.exists():
        return []
    dataset_root = manifest_path.parent.parent
    negatives: list[Path] = []
    with open(manifest_path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            if row.get("split") != "test":
                continue
            if row.get("is_negative") != "1":
                continue
            rel_path = (row.get("image_rel_path") or "").strip()
            abs_path = (row.get("image_path") or "").strip()
            candidates: list[Path] = []
            if rel_path:
                candidates.append(dataset_root / Path(rel_path))
            if abs_path:
                candidates.append(Path(abs_path))
            for candidate in candidates:
                if candidate.exists():
                    negatives.append(candidate)
                    break
    return negatives


def resolve_resume_checkpoint(run_dir: Path, resume_from: str) -> Path:
    if resume_from:
        checkpoint = Path(resume_from)
    else:
        checkpoint = run_dir / "weights" / "last.pt"
    if not checkpoint.exists():
        raise FileNotFoundError(
            f"Resume checkpoint not found: {checkpoint}\n"
            "Pass --resume_from path/to/last.pt or point --run_name at an existing run."
        )
    return checkpoint


def resolve_eval_checkpoint(run_dir: Path, weights_arg: str) -> Path:
    candidates = []
    if weights_arg:
        candidates.append(Path(weights_arg))
    else:
        candidates.extend(
            [
                run_dir / "weights" / "best.pt",
                run_dir / "weights" / "last.pt",
            ]
        )
    for candidate in candidates:
        if candidate.exists():
            return candidate
    raise FileNotFoundError(
        f"No evaluation checkpoint found. Checked: {', '.join(str(p) for p in candidates)}"
    )


def infer_run_dir(weights_path: Path, fallback_run_dir: Path) -> Path:
    if weights_path.parent.name == "weights":
        return weights_path.parent.parent
    return fallback_run_dir


def evaluate_negative_holdout(
    model: YOLO,
    image_paths: list[Path],
    imgsz: int,
    device: str,
    conf: float,
    iou: float,
    out_csv: Path,
) -> dict:
    if not image_paths:
        return {
            "n_images": 0,
            "n_flagged_images": 0,
            "clean_image_rate": None,
            "avg_detections_per_image": None,
        }

    rows: list[dict] = []
    flagged = 0
    total_detections = 0

    preds = model.predict(
        source=[str(p) for p in image_paths],
        imgsz=imgsz,
        conf=conf,
        iou=iou,
        device=device,
        stream=True,
        verbose=False,
        save=False,
    )

    for result in preds:
        boxes = result.boxes
        n_det = 0 if boxes is None else len(boxes)
        total_detections += n_det
        if n_det > 0:
            flagged += 1
            confs = boxes.conf.detach().cpu().tolist()
            clses = boxes.cls.detach().cpu().tolist()
            best_idx = max(range(n_det), key=lambda idx: confs[idx])
            max_conf = float(confs[best_idx])
            top_class = int(clses[best_idx])
            top_label = lookup_label(model.names, top_class)
        else:
            max_conf = 0.0
            top_class = None
            top_label = ""

        rows.append(
            {
                "image_path": str(Path(result.path)),
                "n_detections": n_det,
                "max_conf": max_conf,
                "top_class": top_class,
                "top_label": top_label,
            }
        )

    with open(out_csv, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)

    n_images = len(image_paths)
    return {
        "n_images": n_images,
        "n_flagged_images": flagged,
        "n_clean_images": n_images - flagged,
        "flagged_image_rate": flagged / n_images,
        "clean_image_rate": (n_images - flagged) / n_images,
        "avg_detections_per_image": total_detections / n_images,
        "confidence_threshold": conf,
        "iou_threshold": iou,
        "details_csv": str(out_csv),
    }


def run_evaluation(
    weights_path: Path,
    run_dir: Path,
    data_yaml: Path,
    manifest_path: Path,
    args: argparse.Namespace,
) -> None:
    run_dir.mkdir(parents=True, exist_ok=True)
    best_model = YOLO(str(weights_path))

    print("\n[2/4] Validation metrics ...")
    val_metrics = best_model.val(
        data=str(data_yaml),
        split="val",
        imgsz=args.imgsz,
        batch=args.batch,
        device=args.device,
        conf=args.conf,
        save_json=False,
        verbose=False,
    )

    print("\n[3/4] Test metrics ...")
    test_metrics = best_model.val(
        data=str(data_yaml),
        split="test",
        imgsz=args.imgsz,
        batch=args.batch,
        device=args.device,
        conf=args.conf,
        save_json=False,
        verbose=False,
    )

    print("\n[4/4] Negative-only holdout diagnostics ...")
    negatives = read_negative_test_paths(manifest_path)
    negative_eval = evaluate_negative_holdout(
        model=best_model,
        image_paths=negatives,
        imgsz=args.imgsz,
        device=args.device,
        conf=args.neg_conf,
        iou=args.iou,
        out_csv=run_dir / "negative_test_predictions.csv",
    )

    write_per_class_csv(val_metrics, run_dir / "val_metrics_per_class.csv")
    write_per_class_csv(test_metrics, run_dir / "test_metrics_per_class.csv")

    summary = {
        "weights_path": str(weights_path),
        "data_yaml": str(data_yaml),
        "manifest_path": str(manifest_path),
        "model": args.model,
        "train_args": vars(args),
        "validation": metrics_payload(val_metrics),
        "test": metrics_payload(test_metrics),
        "negative_test": negative_eval,
    }
    summary = make_jsonable(summary)

    summary_path = run_dir / "metrics_summary.json"
    with open(summary_path, "w", encoding="utf-8") as f:
        json.dump(summary, f, indent=2)

    print("\nKey test metrics")
    print("-" * 78)
    print(f"  mAP50:     {summary['test'].get('map50')}")
    print(f"  mAP50-95:  {summary['test'].get('map50_95')}")
    print(f"  Precision: {summary['test'].get('precision')}")
    print(f"  Recall:    {summary['test'].get('recall')}")
    print(f"  Neg clean: {summary['negative_test'].get('clean_image_rate')}")

    print("\nArtifacts")
    print("-" * 78)
    print(f"  Run dir:     {run_dir}")
    print(f"  Weights:     {weights_path}")
    print(f"  Summary:     {summary_path}")
    print(f"  Val detail:  {run_dir / 'val_metrics_per_class.csv'}")
    print(f"  Test detail: {run_dir / 'test_metrics_per_class.csv'}")
    print(f"  Neg detail:  {run_dir / 'negative_test_predictions.csv'}")


def main() -> None:
    args = parse_args()

    data_yaml = Path(args.data)
    manifest_path = Path(args.manifest)
    project_dir = Path(args.project)
    run_dir = project_dir / args.run_name

    if not data_yaml.exists():
        raise FileNotFoundError(
            f"Missing dataset YAML: {data_yaml}\n"
            "Run 4.6_prep_streetview_model_data.py first."
        )
    if args.resume and args.eval_only:
        raise ValueError("--resume and --eval_only cannot be used together.")

    runtime_data_yaml = materialize_runtime_data_yaml(data_yaml)
    training_data_yaml, oversample_info = materialize_training_data_yaml(
        runtime_data_yaml=runtime_data_yaml,
        manifest_path=manifest_path,
        enable_oversample=not args.no_oversample and not args.eval_only,
        target_images=args.oversample_target_images,
        max_repeat=args.oversample_max_repeat,
    )

    print("=" * 78)
    print("YOLO STREETVIEW DEFECT DETECTOR TRAINING")
    print("=" * 78)
    print(f"  Data YAML:      {data_yaml}")
    print(f"  Runtime YAML:   {runtime_data_yaml}")
    if not args.eval_only:
        print(f"  Train YAML:     {training_data_yaml}")
    print(f"  Manifest:       {manifest_path}")
    print(f"  Model:          {args.model}")
    print(f"  Project:        {project_dir}")
    print(f"  Run name:       {args.run_name}")
    print(f"  Device:         {args.device}")
    print(f"  Epochs:         {args.epochs}")
    print(f"  Image size:     {args.imgsz}")
    print(f"  Batch size:     {args.batch}")
    print(f"  Patience:       {args.patience}")
    print(f"  Resume mode:    {args.resume}")
    print(f"  Eval only:      {args.eval_only}")
    print(f"  Root:           {ROOT}")
    print(f"  Data dir:       {DATA_DIR}")
    print(f"  Prepared data:  {PREPARED_DATA_ROOT}")
    if oversample_info is not None:
        print(f"  Oversample:     enabled")
        print(f"  Train entries:  {oversample_info['n_train_entries']} from {oversample_info['n_unique_train_images']} unique images")
        print(f"  Invalid train:  {oversample_info['n_invalid_train_images']}")
        if oversample_info["n_invalid_train_images"] > 0:
            print(f"  Invalid ex.:   {oversample_info['invalid_train_examples']}")
        print(f"  Repeat by cls:  {oversample_info['repeat_by_class']}")
        print(f"  Repeat hist:    {oversample_info['repeat_histogram']}")
    else:
        print(f"  Oversample:     disabled")

    if args.eval_only:
        weights_path = resolve_eval_checkpoint(run_dir, args.weights)
        run_dir = infer_run_dir(weights_path, run_dir)
        print(f"\n[1/4] Skipping training. Evaluating: {weights_path}")
        run_evaluation(weights_path, run_dir, runtime_data_yaml, manifest_path, args)
        return

    if args.resume:
        resume_ckpt = resolve_resume_checkpoint(run_dir, args.resume_from)
        run_dir = infer_run_dir(resume_ckpt, run_dir)
        print(f"\n[1/4] Resuming detector training from {resume_ckpt} ...")
        model = YOLO(str(resume_ckpt))
        train_kwargs = {
            "data": str(training_data_yaml),
            "imgsz": args.imgsz,
            "batch": args.batch,
            "device": args.device,
            "workers": args.workers,
            "patience": args.patience,
            "plots": True,
            "resume": True,
        }
    else:
        model = YOLO(args.model)
        train_kwargs = {
            "data": str(training_data_yaml),
            "epochs": args.epochs,
            "imgsz": args.imgsz,
            "batch": args.batch,
            "device": args.device,
            "workers": args.workers,
            "project": str(project_dir),
            "name": args.run_name,
            "patience": args.patience,
            "seed": args.seed,
            "exist_ok": args.exist_ok,
            "optimizer": "AdamW",
            "cos_lr": True,
            "plots": True,
            "save": True,
            "fliplr": 0.5,
            "flipud": 0.0,
            "degrees": 0.0,
            "translate": 0.03,
            "scale": 0.15,
            "shear": 0.0,
            "perspective": 0.0,
            "mosaic": 0.15,
            "close_mosaic": 15,
            "mixup": 0.0,
            "copy_paste": 0.0,
            "auto_augment": None,
            "erasing": 0.0,
            "hsv_h": 0.005,
            "hsv_s": 0.15,
            "hsv_v": 0.15,
        }
        print("\n[1/4] Training detector ...")

    train_results = model.train(**train_kwargs)

    actual_save_dir = getattr(train_results, "save_dir", None)
    if actual_save_dir is None:
        actual_save_dir = getattr(getattr(model, "trainer", None), "save_dir", None)
    if actual_save_dir is not None:
        run_dir = Path(actual_save_dir)

    best_weights = run_dir / "weights" / "best.pt"
    last_weights = run_dir / "weights" / "last.pt"
    weights_path = best_weights if best_weights.exists() else last_weights
    if not weights_path.exists():
        raise FileNotFoundError(
            f"Training finished but no weights found in: {run_dir / 'weights'}"
        )

    run_evaluation(weights_path, run_dir, runtime_data_yaml, manifest_path, args)


if __name__ == "__main__":
    main()
