from __future__ import annotations

from pathlib import Path
import csv
from collections import Counter

import numpy as np
from ultralytics import YOLO


# -----------------------------------------------------------------------------
# Purpose
# - Load the YOLO11 *classification* model fine-tuned in `4.3_yolo11_fine_tune_satellite_images.py`
# - Run inference on Ohio images in `data/roads/ohio/{above,below}`
# - Save CSV outputs similar to `4.1_roads_ai_model_oh.jl`:
#     ohio_above_preds.csv, ohio_below_preds.csv
#     columns: image_path, pred_id, pred_label, max_prob, (p0,p1,p2 if available)
# -----------------------------------------------------------------------------


# ===== Paths (edit if needed) =====
DATA_DIR = Path(r"C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads")

# This is the dataset root that `4.3` uses to train the Ultralytics classifier
YOLO_ROOT = DATA_DIR / "runs_ohio" / "yolo11_finetune_satellite_images"

# Training run name used in `4.3` (`name=...` in model.train)
RUN_NAME = "yolo11n_cls_roads"

# Ohio image folders (same as `4.1`)
OHIO_ABOVE_DIR = DATA_DIR / "ohio" / "above"
OHIO_BELOW_DIR = DATA_DIR / "ohio" / "below"

# Output directory (kept alongside trained weights, similar to `4.1`)
MODEL_DIR = YOLO_ROOT / RUN_NAME
PRED_OUTDIR = MODEL_DIR / "ohio_preds"

# Class id -> label mapping (0=low,1=medium,2=high)
CLASS_NAMES = {0: "low_quality", 1: "medium_quality", 2: "high_quality"}

# ===== Inference knobs =====
IMGSZ = 224
BATCH = 8
DEVICE = "cpu"  # change to "0" for first CUDA GPU if available
PRINT_EVERY = 200
WRITE_ALL_P = True  # write p0,p1,p2 if model outputs 3 probs


def list_images(folder: Path) -> list[Path]:
    exts = {".jpg", ".jpeg", ".png", ".webp"}
    if not folder.exists():
        return []
    return sorted([p for p in folder.iterdir() if p.is_file() and p.suffix.lower() in exts])


def resolve_weights(model_dir: Path) -> Path:
    best = model_dir / "weights" / "best.pt"
    last = model_dir / "weights" / "last.pt"
    if best.exists():
        return best
    if last.exists():
        return last
    raise FileNotFoundError(
        "Could not find YOLO weights. Expected one of:\n"
        f"- {best}\n"
        f"- {last}\n"
        "\nDid you run `4.3_yolo11_fine_tune_satellite_images.py` successfully?"
    )


def probs_to_numpy(probs_obj) -> np.ndarray:
    """Convert Ultralytics probs object to a 1D numpy array."""
    # Ultralytics returns a `Probs` object with `.data` as torch tensor
    data = getattr(probs_obj, "data", None)
    if data is None:
        # fallback: try `.numpy()` or `.tolist()`
        if hasattr(probs_obj, "numpy"):
            arr = probs_obj.numpy()
            return np.array(arr, dtype=float).reshape(-1)
        if hasattr(probs_obj, "tolist"):
            return np.array(probs_obj.tolist(), dtype=float).reshape(-1)
        raise TypeError("Could not extract probabilities from result.probs")

    # torch.Tensor path
    if hasattr(data, "detach"):
        data = data.detach()
    if hasattr(data, "cpu"):
        data = data.cpu()
    if hasattr(data, "numpy"):
        return np.array(data.numpy(), dtype=float).reshape(-1)

    # final fallback
    return np.array(data, dtype=float).reshape(-1)


def id_to_label(class_id: int) -> str:
    return CLASS_NAMES.get(int(class_id), str(class_id))


def run_infer(model: YOLO, image_paths: list[Path], tag: str) -> Path | None:
    if not image_paths:
        print(f"[{tag}] no images found, skipping.")
        return None

    PRED_OUTDIR.mkdir(parents=True, exist_ok=True)
    out_csv = PRED_OUTDIR / f"{tag}_preds.csv"

    print(f"[{tag}] running inference on {len(image_paths)} images")
    print(f"[{tag}] writing: {out_csv}")

    counts = Counter()
    wrote_header = False
    n_done = 0

    # stream=True yields results one-by-one (lower memory)
    preds = model.predict(
        source=[str(p) for p in image_paths],
        imgsz=IMGSZ,
        batch=BATCH,
        device=DEVICE,
        stream=True,
        verbose=False,
    )

    with open(out_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)

        for r in preds:
            probs = probs_to_numpy(r.probs)
            pred_id = int(getattr(r.probs, "top1", int(np.argmax(probs))))
            max_prob = float(getattr(r.probs, "top1conf", float(np.max(probs))))
            pred_label = id_to_label(pred_id)

            if not wrote_header:
                hdr = ["image_path", "pred_id", "pred_label", "max_prob"]
                if WRITE_ALL_P and probs.shape[0] == 3:
                    hdr += ["p0", "p1", "p2"]
                w.writerow(hdr)
                wrote_header = True

            row = [str(Path(r.path)), pred_id, pred_label, max_prob]
            if WRITE_ALL_P and probs.shape[0] == 3:
                row += [float(probs[0]), float(probs[1]), float(probs[2])]
            w.writerow(row)

            counts[pred_label] += 1
            n_done += 1

            if n_done % PRINT_EVERY == 0:
                print(f"[{tag}] {n_done}/{len(image_paths)} processed")

    print(f"[{tag}] done: {n_done}/{len(image_paths)}")
    print(f"[{tag}] class distribution: {dict(counts)}")
    return out_csv


def main() -> None:
    print("=" * 70)
    print("YOLO11 OHIO INFERENCE (CLASSIFICATION)")
    print("=" * 70)

    if not YOLO_ROOT.exists():
        raise FileNotFoundError(f"YOLO_ROOT does not exist: {YOLO_ROOT}")

    weights_path = resolve_weights(MODEL_DIR)
    print("Using weights:")
    print(f"  {weights_path}")

    above = list_images(OHIO_ABOVE_DIR)
    below = list_images(OHIO_BELOW_DIR)

    print("Input folders:")
    print(f"  above: {OHIO_ABOVE_DIR} ({len(above)} images)")
    print(f"  below: {OHIO_BELOW_DIR} ({len(below)} images)")

    model = YOLO(str(weights_path))

    # show what Ultralytics thinks the class names are (often '0','1','2')
    try:
        print("Model names (from Ultralytics):", getattr(model, "names", None))
    except Exception:
        pass

    run_infer(model, above, "ohio_above")
    run_infer(model, below, "ohio_below")

    print("\nOutputs:")
    print(f"  {PRED_OUTDIR / 'ohio_above_preds.csv'}")
    print(f"  {PRED_OUTDIR / 'ohio_below_preds.csv'}")


if __name__ == "__main__":
    main()
