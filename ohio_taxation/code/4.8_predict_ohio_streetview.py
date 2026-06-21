"""
File: 4.8_predict_ohio_streetview.py
Author: Saani Rawat
Date: 11 Mar 2026
Purpose:
    Run the fine-tuned YOLO streetview defect detector on Ohio Google Street
    View images and save YOLO label files for downstream PCR scoring.

    Important behavior:
    - One `.txt` file is written for every image, even when the detector finds
      nothing. Empty files are intentional and preserve clean-road images for
      downstream parsing.
    - Output layout matches the parser used in 1.2_pcr_streetview_images.py.

Usage:
    python 4.8_predict_ohio_streetview.py
    python 4.8_predict_ohio_streetview.py --device 0 --conf 0.20
"""

from __future__ import annotations

import argparse
import csv
from collections import Counter
from pathlib import Path

from PIL import Image
from ultralytics import YOLO


ROOT = Path(
    "C:/Users/rawatsa/OneDrive - University of Cincinnati/"
    "StataProjects/ohio_taxation"
)
DATA_DIR = ROOT / "data" / "roads"
SV_DIR = DATA_DIR / "ohio" / "google streetview photos"

PROJECT_DIR = DATA_DIR / "runs_ohio" / "yolo11_rdd2024_streetview_detector"
DEFAULT_RUN_NAME = "streetview_det_yolo11m"
DEFAULT_PREDICT_NAME = "predict_ohio"

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


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run YOLO streetview defect detection on Ohio Street View images."
    )
    parser.add_argument(
        "--source_dir",
        type=str,
        default=str(SV_DIR),
        help="Directory containing Ohio Street View images.",
    )
    parser.add_argument(
        "--project",
        type=str,
        default=str(PROJECT_DIR),
        help="Ultralytics project directory used in 4.7.",
    )
    parser.add_argument(
        "--run_name",
        type=str,
        default=DEFAULT_RUN_NAME,
        help="Training run name from 4.7.",
    )
    parser.add_argument(
        "--predict_name",
        type=str,
        default=DEFAULT_PREDICT_NAME,
        help="Prediction subdirectory name under project.",
    )
    parser.add_argument(
        "--weights",
        type=str,
        default="",
        help="Optional explicit weights path. Defaults to best.pt from the training run.",
    )
    parser.add_argument(
        "--imgsz",
        type=int,
        default=640,
        help="Inference image size.",
    )
    parser.add_argument(
        "--conf",
        type=float,
        default=0.25,
        help="Confidence threshold.",
    )
    parser.add_argument(
        "--iou",
        type=float,
        default=0.60,
        help="NMS IoU threshold.",
    )
    parser.add_argument(
        "--device",
        type=str,
        default="cpu",
        help="Ultralytics device string, e.g. 'cpu', '0', '0,1'.",
    )
    parser.add_argument(
        "--batch",
        type=int,
        default=16,
        help="Inference batch size. Use a smaller value if GPU memory is tight.",
    )
    parser.add_argument(
        "--save_images",
        action="store_true",
        help="Also save rendered prediction images.",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Delete the prediction folder first instead of resuming.",
    )
    return parser.parse_args()


def resolve_weights(project_dir: Path, run_name: str, explicit: str) -> Path:
    if explicit:
        weights = Path(explicit)
        if not weights.exists():
            raise FileNotFoundError(f"Explicit weights path not found: {weights}")
        return weights

    run_dir = project_dir / run_name / "weights"
    best = run_dir / "best.pt"
    last = run_dir / "last.pt"
    if best.exists():
        return best
    if last.exists():
        return last
    raise FileNotFoundError(
        "Could not find detector weights.\n"
        f"Expected one of:\n- {best}\n- {last}\n"
        "Run 4.7_train_streetview_model.py first."
    )


def list_images(source_dir: Path) -> list[Path]:
    exts = {".jpg", ".jpeg", ".png", ".webp"}
    if not source_dir.exists():
        return []
    return sorted(
        p for p in source_dir.iterdir()
        if p.is_file() and p.suffix.lower() in exts
    )


def lookup_label(names, class_id: int) -> str:
    if isinstance(names, dict):
        return str(names.get(class_id, class_id))
    if isinstance(names, list):
        if 0 <= class_id < len(names):
            return str(names[class_id])
    return CLASS_NAMES.get(class_id, str(class_id))


def batched(items: list[Path], batch_size: int):
    for start in range(0, len(items), batch_size):
        yield items[start:start + batch_size]


def write_yolo_label_file(txt_path: Path, boxes) -> tuple[int, float, str, str, list[int]]:
    max_conf = 0.0
    top_class = ""
    top_label = ""
    clses: list[int] = []

    with open(txt_path, "w", encoding="utf-8") as f:
        if boxes is None or len(boxes) == 0:
            return 0, max_conf, top_class, top_label, clses

        xywhn = boxes.xywhn.detach().cpu().tolist()
        confs = boxes.conf.detach().cpu().tolist()
        clses = [int(x) for x in boxes.cls.detach().cpu().tolist()]

        best_idx = max(range(len(clses)), key=lambda idx: confs[idx])
        max_conf = float(confs[best_idx])
        top_class = str(clses[best_idx])
        top_label = CLASS_NAMES.get(clses[best_idx], top_class)

        for cls_id, (xc, yc, w, h), conf in zip(clses, xywhn, confs):
            f.write(
                f"{cls_id} "
                f"{float(xc):.6f} {float(yc):.6f} "
                f"{float(w):.6f} {float(h):.6f} "
                f"{float(conf):.6f}\n"
            )

    return len(clses), max_conf, top_class, top_label, clses


def annotated_image_path(output_dir: Path, image_path: Path) -> Path:
    return output_dir / image_path.name


def save_annotated_image(output_path: Path, result) -> None:
    # Ultralytics returns BGR images from result.plot(); convert to RGB for PIL.
    plotted = result.plot()
    Image.fromarray(plotted[..., ::-1]).save(output_path)


def main() -> None:
    args = parse_args()
    source_dir = Path(args.source_dir)
    project_dir = Path(args.project)
    output_dir = project_dir / args.predict_name
    labels_dir = output_dir / "labels"

    weights_path = resolve_weights(project_dir, args.run_name, args.weights)
    image_paths = list_images(source_dir)
    if not image_paths:
        raise FileNotFoundError(f"No images found in: {source_dir}")

    if args.overwrite and output_dir.exists():
        import shutil
        shutil.rmtree(output_dir)

    labels_dir.mkdir(parents=True, exist_ok=True)
    output_dir.mkdir(parents=True, exist_ok=True)

    todo_images: list[Path] = []
    for image_path in image_paths:
        label_exists = (labels_dir / f"{image_path.stem}.txt").exists()
        image_exists = (not args.save_images) or annotated_image_path(output_dir, image_path).exists()
        if not (label_exists and image_exists):
            todo_images.append(image_path)
    already_done = len(image_paths) - len(todo_images)

    print("=" * 78)
    print("OHIO STREETVIEW DEFECT DETECTION")
    print("=" * 78)
    print(f"  Source dir:      {source_dir}")
    print(f"  Weights:         {weights_path}")
    print(f"  Output dir:      {output_dir}")
    print(f"  Total images:    {len(image_paths)}")
    print(f"  Already done:    {already_done}")
    print(f"  Remaining:       {len(todo_images)}")
    print(f"  Confidence:      {args.conf}")
    print(f"  IoU:             {args.iou}")
    print(f"  Device:          {args.device}")
    print(f"  Batch size:      {args.batch}")

    if not todo_images:
        print("\nNothing to do.")
        return

    model = YOLO(str(weights_path))
    counts = Counter()
    n_images = 0

    summary_csv = output_dir / "ohio_streetview_detection_summary.csv"
    summary_fields = [
        "image_path",
        "label_path",
        "n_detections",
        "max_conf",
        "top_class",
        "top_label",
    ]

    with open(summary_csv, "a" if summary_csv.exists() else "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=summary_fields)
        if f.tell() == 0:
            writer.writeheader()

        for batch_paths in batched(todo_images, args.batch):
            preds = model.predict(
                source=[str(p) for p in batch_paths],
                imgsz=args.imgsz,
                conf=args.conf,
                iou=args.iou,
                device=args.device,
                batch=min(args.batch, len(batch_paths)),
                stream=True,
                verbose=False,
                save=False,
                save_txt=False,
                save_conf=False,
                project=str(project_dir),
                name=args.predict_name,
                exist_ok=True,
            )

            for source_path, result in zip(batch_paths, preds):
                n_images += 1
                image_path = Path(source_path)
                txt_path = labels_dir / f"{image_path.stem}.txt"
                n_det, max_conf, top_class, top_label, clses = write_yolo_label_file(
                    txt_path,
                    result.boxes,
                )
                if args.save_images:
                    save_annotated_image(
                        annotated_image_path(output_dir, image_path),
                        result,
                    )

                if n_det > 0:
                    top_label = lookup_label(model.names, int(top_class))
                    for cls in clses:
                        counts[lookup_label(model.names, cls)] += 1

                writer.writerow(
                    {
                        "image_path": str(image_path),
                        "label_path": str(txt_path),
                        "n_detections": n_det,
                        "max_conf": max_conf,
                        "top_class": top_class,
                        "top_label": top_label,
                    }
                )

                if n_images % 200 == 0:
                    print(f"  processed {n_images}/{len(todo_images)} images")

            f.flush()

    print("\nRun complete")
    print("-" * 78)
    print(f"  Newly processed images: {n_images}")
    print(f"  Labels dir:             {labels_dir}")
    print(f"  Summary CSV:            {summary_csv}")
    print(f"  Detection class counts: {dict(counts)}")


if __name__ == "__main__":
    main()
