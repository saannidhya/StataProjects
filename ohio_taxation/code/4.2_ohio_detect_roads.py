from __future__ import annotations

import csv
import os
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Tuple, List

import torch
from PIL import Image

# ---- Progress bar (optional) ----
try:
    from tqdm import tqdm  # type: ignore
except Exception:
    tqdm = None  # fallback

# ---- Try importing SAM3 from transformers (official HF route) ----
try:
    from transformers import Sam3Processor, Sam3Model  # type: ignore
except Exception as e:
    raise ImportError(
        "Could not import Sam3Processor/Sam3Model from transformers.\n"
        "You likely need a newer Transformers version that includes SAM-3.\n"
        "Try upgrading transformers, e.g.:\n"
        "  pip install -U transformers accelerate tqdm\n"
        f"\nOriginal import error:\n{e}"
    )


# =========================
# User paths (edit if needed)
# =========================
INPUT_DIR = Path(
    "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/ohio/google maps photos"
)
OUTPUT_DIR = INPUT_DIR / "sam3"  # requested output folder


# =================================================
# Heuristic thresholds (more forgiving defaults)
# =================================================
@dataclass
class RoadHeuristics:
    # minimum fraction of whole image covered by the union of "road" masks
    min_area_frac: float = 0.12
    # minimum fraction of pixels in the bottom quarter that are masked as road
    min_bottom_quarter_frac: float = 0.12
    # minimum centroid Y position (0=top, 1=bottom) of the union mask
    min_centroid_y: float = 0.45
    # require that the mask touches the bottom row (common for streetview road)
    require_bottom_touch: bool = False


# =========================
# Helpers
# =========================
def iter_images(root: Path, exts: Tuple[str, ...] = (".jpg", ".jpeg", ".png")) -> Iterable[Path]:
    """Recursively yield image files under root, skipping the OUTPUT_DIR subtree."""
    root = root.resolve()
    out = OUTPUT_DIR.resolve()
    for p in root.rglob("*"):
        if not p.is_file():
            continue
        if p.suffix.lower() not in exts:
            continue
        # skip anything already inside the output folder
        try:
            p.resolve().relative_to(out)
            continue  # it's under OUTPUT_DIR
        except Exception:
            pass
        yield p


def safe_open_rgb(path: Path) -> Image.Image:
    with Image.open(path) as im:
        return im.convert("RGB")


def mask_stats(union_mask: torch.Tensor) -> dict:
    """
    union_mask: Bool tensor [H, W]
    Returns area_frac, bottom_quarter_frac, centroid_y, bottom_touch
    """
    if union_mask.dtype != torch.bool:
        union_mask = union_mask.bool()

    H, W = union_mask.shape
    total = float(H * W)

    area_frac = union_mask.sum().item() / total

    bottom_start = int(0.75 * H)
    bottom_quarter = union_mask[bottom_start:, :]
    bottom_quarter_frac = bottom_quarter.sum().item() / float(bottom_quarter.numel())

    bottom_touch = bool(union_mask[-1, :].any().item())

    ys, _xs = torch.where(union_mask)
    if ys.numel() == 0:
        centroid_y = 0.0
    else:
        centroid_y = (ys.float().mean().item() / float(H))

    return dict(
        area_frac=area_frac,
        bottom_quarter_frac=bottom_quarter_frac,
        centroid_y=centroid_y,
        bottom_touch=bottom_touch,
        H=H,
        W=W,
    )


def looks_like_road(union_mask: torch.Tensor, h: RoadHeuristics) -> Tuple[bool, dict]:
    stats = mask_stats(union_mask)

    ok = True
    ok &= stats["area_frac"] >= h.min_area_frac
    ok &= stats["bottom_quarter_frac"] >= h.min_bottom_quarter_frac
    ok &= stats["centroid_y"] >= h.min_centroid_y
    if h.require_bottom_touch:
        ok &= stats["bottom_touch"]

    return bool(ok), stats


def ensure_parent(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)


# =========================
# SAM3 inference
# =========================
@torch.inference_mode()
def road_union_mask_sam3(
    model: Sam3Model,
    processor: Sam3Processor,
    image: Image.Image,
    text_prompt: str = "road",
    device: str = "cpu",
    threshold: float = 0.50,
    mask_threshold: float = 0.50,
) -> Tuple[torch.Tensor, int]:
    """
    Returns:
      union_mask: Bool tensor [H, W] in original image size
      n_masks: number of instance masks found
    """
    inputs = processor(images=image, text=text_prompt, return_tensors="pt")
    inputs = inputs.to(device)

    outputs = model(**inputs)

    target_sizes = inputs.get("original_sizes")
    if target_sizes is None:
        # fallback: use PIL size
        target_sizes = torch.tensor([[image.size[1], image.size[0]]], device=device)  # [H, W]

    results = processor.post_process_instance_segmentation(
        outputs,
        threshold=threshold,
        mask_threshold=mask_threshold,
        target_sizes=target_sizes.detach().cpu().tolist(),
    )[0]

    masks = results.get("masks", None)
    if masks is None or len(masks) == 0:
        H, W = image.size[1], image.size[0]
        return torch.zeros((H, W), dtype=torch.bool), 0

    # masks: [N, H, W]
    masks = masks.bool()
    union = masks.any(dim=0)  # [H, W]
    return union, int(masks.shape[0])


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    device = "cuda" if torch.cuda.is_available() else "cpu"
    dtype = torch.float16 if device == "cuda" else torch.float32

    print(f"Loading SAM-3 on device={device} dtype={dtype} ...")
    model = Sam3Model.from_pretrained("facebook/sam3", torch_dtype=dtype).to(device)
    processor = Sam3Processor.from_pretrained("facebook/sam3")
    model.eval()

    # More forgiving heuristics:
    heur = RoadHeuristics(
        min_area_frac=0.12,
        min_bottom_quarter_frac=0.12,
        min_centroid_y=0.45,
        require_bottom_touch=False,
    )

    prompt = "road"  # you can try: "road surface" or "street"
    log_path = OUTPUT_DIR / "sam3_filter_log.csv"

    # Build list so we can show tqdm total/ETA
    img_list: List[Path] = list(iter_images(INPUT_DIR, exts=(".jpg", ".jpeg", ".png")))
    n_total = len(img_list)
    print(f"Found {n_total} images to process (excluding {OUTPUT_DIR}).")

    rows = []
    kept = 0

    # Toggle if you want every filename printed even with tqdm:
    VERBOSE_EACH_IMAGE = True

    iterator = img_list
    if tqdm is not None:
        iterator = tqdm(img_list, total=n_total, desc="SAM3 filtering", unit="img")

    for i, img_path in enumerate(iterator, start=1):
        rel = img_path.resolve().relative_to(INPUT_DIR.resolve())
        out_path = OUTPUT_DIR / rel
        status = "REJECT"
        err = ""

        if VERBOSE_EACH_IMAGE and tqdm is None:
            print(f"Processing {i}/{n_total}: {rel}")
        elif VERBOSE_EACH_IMAGE and tqdm is not None:
            # tqdm-friendly log line
            iterator.set_postfix_str(f"{i}/{n_total} {rel.name}")

        try:
            image = safe_open_rgb(img_path)
            union_mask, n_masks = road_union_mask_sam3(
                model=model,
                processor=processor,
                image=image,
                text_prompt=prompt,
                device=device,
                threshold=0.50,
                mask_threshold=0.50,
            )

            ok, stats = looks_like_road(union_mask, heur)

            if ok:
                ensure_parent(out_path)
                shutil.copy2(img_path, out_path)
                kept += 1
                status = "KEEP"

            rows.append(
                dict(
                    file=str(rel),
                    status=status,
                    prompt=prompt,
                    n_masks=n_masks,
                    area_frac=stats["area_frac"],
                    bottom_quarter_frac=stats["bottom_quarter_frac"],
                    centroid_y=stats["centroid_y"],
                    bottom_touch=stats["bottom_touch"],
                    error=err,
                )
            )

        except Exception as e:
            err = repr(e)
            rows.append(
                dict(
                    file=str(rel),
                    status="ERROR",
                    prompt=prompt,
                    n_masks="",
                    area_frac="",
                    bottom_quarter_frac="",
                    centroid_y="",
                    bottom_touch="",
                    error=err,
                )
            )

        # If tqdm is not available, print periodic progress
        if tqdm is None and (i % 25 == 0 or i == n_total):
            print(f"Processed {i}/{n_total} images | kept {kept}")

    # write CSV log
    fieldnames = [
        "file",
        "status",
        "prompt",
        "n_masks",
        "area_frac",
        "bottom_quarter_frac",
        "centroid_y",
        "bottom_touch",
        "error",
    ]
    with open(log_path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        for r in rows:
            w.writerow(r)

    print(f"\nDONE. Kept {kept}/{n_total} images.")
    print(f"Output folder: {OUTPUT_DIR}")
    print(f"Log: {log_path}")


if __name__ == "__main__":
    # Optional: reduce CPU thread oversubscription (uncomment if your machine lags)
    # torch.set_num_threads(max(1, os.cpu_count() // 2))
    main()
