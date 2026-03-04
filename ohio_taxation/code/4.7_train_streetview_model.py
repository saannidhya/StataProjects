#==========================================================================#
# File: 4.7_train_streetview_model.py
# Author: Saani Rawat
# Date: 04 Mar 2026
# Description: Fine-tune ConvNeXt v2 on RDD2024 streetview images with
#              PCR-grounded 3-class labels.  Designed for Google Colab Pro
#              (T4/A100 GPU).  Can also run on CPU for pipeline testing.
#
# Dependencies:
#   - transformers, torch, torchvision, scikit-learn, pandas, PIL
#   - Split CSVs from 4.6_rdd2024_to_pcr_labels.py:
#       data/roads/rdd2024_pcr_train.csv
#       data/roads/rdd2024_pcr_val.csv
#       data/roads/rdd2024_pcr_test.csv
#
# Usage (local CPU test):
#   python 4.7_train_streetview_model.py --epochs 1 --batch_size 2
#
# Usage (Colab GPU):
#   python 4.7_train_streetview_model.py --data_root /content/drive/MyDrive/ohio_taxation/data/roads
#
# --- Colab setup (uncomment when running in Colab) ---
# !pip install transformers torch torchvision scikit-learn pandas
# from google.colab import drive
# drive.mount('/content/drive')
#==========================================================================#

from __future__ import annotations

import argparse
import json
import math
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
from PIL import Image
from torchvision import transforms
from transformers import ConvNextV2ForImageClassification, AutoImageProcessor
from sklearn.metrics import (
    balanced_accuracy_score,
    classification_report,
    confusion_matrix,
)

# ---- Defaults ---------------------------------------------------------------

DEFAULT_DATA_ROOT = Path(
    "C:/Users/rawatsa/OneDrive - University of Cincinnati/"
    "StataProjects/ohio_taxation/data/roads"
)
DEFAULT_MODEL_NAME = "facebook/convnextv2-base-22k-224"
DEFAULT_OUTPUT_DIR_NAME = "hf_finetuned_convnextv2_streetview"

CLASS_NAMES = {0: "low_quality", 1: "medium_quality", 2: "high_quality"}
NUM_CLASSES = 3


# ---- CLI --------------------------------------------------------------------

def parse_args():
    parser = argparse.ArgumentParser(
        description="Fine-tune ConvNeXt v2 on RDD2024 streetview images"
    )
    parser.add_argument(
        "--data_root", type=str, default=str(DEFAULT_DATA_ROOT),
        help="Root directory containing split CSVs and image folders",
    )
    parser.add_argument(
        "--model_name", type=str, default=DEFAULT_MODEL_NAME,
        help="HuggingFace model identifier or local path",
    )
    parser.add_argument(
        "--epochs", type=int, default=30,
        help="Maximum number of training epochs (default: 30)",
    )
    parser.add_argument(
        "--batch_size", type=int, default=16,
        help="Batch size per device (default: 16)",
    )
    parser.add_argument(
        "--grad_accum", type=int, default=2,
        help="Gradient accumulation steps (effective batch = batch_size * grad_accum)",
    )
    parser.add_argument(
        "--lr", type=float, default=2e-5,
        help="Peak learning rate (default: 2e-5)",
    )
    parser.add_argument(
        "--weight_decay", type=float, default=0.01,
        help="Weight decay (default: 0.01)",
    )
    parser.add_argument(
        "--patience", type=int, default=5,
        help="Early stopping patience in epochs (default: 5)",
    )
    parser.add_argument(
        "--warmup_frac", type=float, default=0.05,
        help="Fraction of total steps for LR warmup (default: 0.05)",
    )
    parser.add_argument(
        "--num_workers", type=int, default=2,
        help="DataLoader workers (default: 2)",
    )
    return parser.parse_args()


# ---- Dataset ----------------------------------------------------------------

class RoadQualityDataset(Dataset):
    """PyTorch Dataset for road quality images with PCR labels."""

    def __init__(self, csv_path: Path, transform=None):
        self.df = pd.read_csv(csv_path)
        self.transform = transform
        # Verify required columns
        required = {"image_path", "int_label"}
        missing = required - set(self.df.columns)
        if missing:
            raise ValueError(f"CSV {csv_path} missing columns: {missing}")

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        row = self.df.iloc[idx]
        img = Image.open(row["image_path"]).convert("RGB")
        label = int(row["int_label"])
        if self.transform:
            img = self.transform(img)
        return img, label


# ---- Transforms -------------------------------------------------------------

def get_train_transforms():
    return transforms.Compose([
        transforms.RandomResizedCrop(224, scale=(0.8, 1.0)),
        transforms.RandomHorizontalFlip(p=0.5),
        transforms.ColorJitter(brightness=0.3, contrast=0.3, saturation=0.2, hue=0.1),
        transforms.GaussianBlur(kernel_size=3, sigma=(0.1, 2.0)),
        transforms.RandomRotation(15),
        transforms.ToTensor(),
        transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225]),
    ])


def get_val_transforms():
    return transforms.Compose([
        transforms.Resize(256),
        transforms.CenterCrop(224),
        transforms.ToTensor(),
        transforms.Normalize(mean=[0.485, 0.456, 0.406], std=[0.229, 0.224, 0.225]),
    ])


# ---- LR Scheduler -----------------------------------------------------------

def get_cosine_schedule_with_warmup(optimizer, warmup_steps, total_steps):
    """Linear warmup then cosine decay to 0."""

    def lr_lambda(current_step):
        if current_step < warmup_steps:
            return float(current_step) / float(max(1, warmup_steps))
        progress = float(current_step - warmup_steps) / float(
            max(1, total_steps - warmup_steps)
        )
        return max(0.0, 0.5 * (1.0 + math.cos(math.pi * progress)))

    return torch.optim.lr_scheduler.LambdaLR(optimizer, lr_lambda)


# ---- Training loop -----------------------------------------------------------

def train_one_epoch(model, loader, criterion, optimizer, scheduler, device,
                    grad_accum_steps):
    model.train()
    total_loss = 0.0
    n_samples = 0
    optimizer.zero_grad()

    for step, (images, labels) in enumerate(loader):
        images = images.to(device)
        labels = labels.to(device)

        outputs = model(images)
        loss = criterion(outputs.logits, labels) / grad_accum_steps
        loss.backward()

        if (step + 1) % grad_accum_steps == 0 or (step + 1) == len(loader):
            optimizer.step()
            scheduler.step()
            optimizer.zero_grad()

        total_loss += loss.item() * grad_accum_steps * images.size(0)
        n_samples += images.size(0)

    return total_loss / n_samples


@torch.no_grad()
def evaluate(model, loader, criterion, device):
    model.eval()
    total_loss = 0.0
    n_samples = 0
    all_preds = []
    all_labels = []
    all_probs = []

    for images, labels in loader:
        images = images.to(device)
        labels = labels.to(device)

        outputs = model(images)
        loss = criterion(outputs.logits, labels)

        probs = torch.softmax(outputs.logits, dim=-1)
        preds = probs.argmax(dim=-1)

        total_loss += loss.item() * images.size(0)
        n_samples += images.size(0)

        all_preds.extend(preds.cpu().numpy())
        all_labels.extend(labels.cpu().numpy())
        all_probs.extend(probs.cpu().numpy())

    avg_loss = total_loss / n_samples
    all_preds = np.array(all_preds)
    all_labels = np.array(all_labels)
    all_probs = np.array(all_probs)

    bal_acc = balanced_accuracy_score(all_labels, all_preds)
    acc = (all_preds == all_labels).mean()

    return {
        "loss": avg_loss,
        "accuracy": acc,
        "balanced_accuracy": bal_acc,
        "preds": all_preds,
        "labels": all_labels,
        "probs": all_probs,
    }


# ---- Main -------------------------------------------------------------------

def main():
    args = parse_args()
    data_root = Path(args.data_root)
    output_dir = data_root / DEFAULT_OUTPUT_DIR_NAME
    output_dir.mkdir(parents=True, exist_ok=True)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print("=" * 70)
    print("STREETVIEW ROAD QUALITY MODEL TRAINING")
    print("=" * 70)
    print(f"  Device:         {device}")
    print(f"  Data root:      {data_root}")
    print(f"  Model:          {args.model_name}")
    print(f"  Output dir:     {output_dir}")
    print(f"  Epochs:         {args.epochs}")
    print(f"  Batch size:     {args.batch_size} (x{args.grad_accum} accum = "
          f"{args.batch_size * args.grad_accum} effective)")
    print(f"  Learning rate:  {args.lr}")
    print(f"  Patience:       {args.patience}")

    # ------------------------------------------------------------------
    # 1. Load data
    # ------------------------------------------------------------------
    print("\n[1/5] Loading datasets ...")

    train_csv = data_root / "rdd2024_pcr_train.csv"
    val_csv = data_root / "rdd2024_pcr_val.csv"
    test_csv = data_root / "rdd2024_pcr_test.csv"

    for p in [train_csv, val_csv, test_csv]:
        if not p.exists():
            raise FileNotFoundError(f"Missing: {p}\nRun 4.6_rdd2024_to_pcr_labels.py first.")

    train_ds = RoadQualityDataset(train_csv, transform=get_train_transforms())
    val_ds = RoadQualityDataset(val_csv, transform=get_val_transforms())
    test_ds = RoadQualityDataset(test_csv, transform=get_val_transforms())

    print(f"  Train: {len(train_ds)} images")
    print(f"  Val:   {len(val_ds)} images")
    print(f"  Test:  {len(test_ds)} images")

    # Class distribution
    train_df = pd.read_csv(train_csv)
    class_counts = train_df["int_label"].value_counts().sort_index().values
    print(f"  Train class counts: {dict(enumerate(class_counts))}")

    train_loader = DataLoader(
        train_ds, batch_size=args.batch_size, shuffle=True,
        num_workers=args.num_workers, pin_memory=True,
    )
    val_loader = DataLoader(
        val_ds, batch_size=args.batch_size, shuffle=False,
        num_workers=args.num_workers, pin_memory=True,
    )
    test_loader = DataLoader(
        test_ds, batch_size=args.batch_size, shuffle=False,
        num_workers=args.num_workers, pin_memory=True,
    )

    # ------------------------------------------------------------------
    # 2. Build model + class-weighted loss
    # ------------------------------------------------------------------
    print("\n[2/5] Loading model ...")

    model = ConvNextV2ForImageClassification.from_pretrained(
        args.model_name,
        num_labels=NUM_CLASSES,
        ignore_mismatched_sizes=True,
    )
    model = model.to(device)

    # Class-weighted cross-entropy: weight inversely proportional to count
    weights = 1.0 / class_counts.astype(np.float64)
    weights = weights / weights.min()  # normalize so smallest weight = 1.0
    class_weights = torch.tensor(weights, dtype=torch.float32).to(device)
    criterion = nn.CrossEntropyLoss(weight=class_weights)

    print(f"  Model parameters: {sum(p.numel() for p in model.parameters()):,}")
    print(f"  Class weights: {dict(enumerate(weights.round(2)))}")

    # ------------------------------------------------------------------
    # 3. Optimizer + scheduler
    # ------------------------------------------------------------------
    optimizer = torch.optim.AdamW(
        model.parameters(), lr=args.lr, weight_decay=args.weight_decay
    )

    steps_per_epoch = math.ceil(len(train_loader) / args.grad_accum)
    total_steps = steps_per_epoch * args.epochs
    warmup_steps = int(total_steps * args.warmup_frac)

    scheduler = get_cosine_schedule_with_warmup(optimizer, warmup_steps, total_steps)

    print(f"  Steps/epoch: {steps_per_epoch}, Total steps: {total_steps}, "
          f"Warmup: {warmup_steps}")

    # ------------------------------------------------------------------
    # 4. Training loop with early stopping
    # ------------------------------------------------------------------
    print("\n[3/5] Training ...")
    print(f"  {'Epoch':>5} {'Train Loss':>11} {'Val Loss':>9} "
          f"{'Val Acc':>8} {'Val BAcc':>9} {'LR':>10} {'Time':>6}")
    print("  " + "-" * 65)

    best_bal_acc = 0.0
    patience_counter = 0

    for epoch in range(1, args.epochs + 1):
        t0 = time.time()

        train_loss = train_one_epoch(
            model, train_loader, criterion, optimizer, scheduler,
            device, args.grad_accum,
        )

        val_results = evaluate(model, val_loader, criterion, device)

        elapsed = time.time() - t0
        current_lr = optimizer.param_groups[0]["lr"]

        print(f"  {epoch:5d} {train_loss:11.4f} {val_results['loss']:9.4f} "
              f"{val_results['accuracy']:8.4f} {val_results['balanced_accuracy']:9.4f} "
              f"{current_lr:10.2e} {elapsed:5.0f}s")

        # Early stopping on balanced accuracy
        if val_results["balanced_accuracy"] > best_bal_acc:
            best_bal_acc = val_results["balanced_accuracy"]
            patience_counter = 0
            # Save best model
            model.save_pretrained(output_dir)
            # Also save the processor config for inference
            processor = AutoImageProcessor.from_pretrained(args.model_name)
            processor.save_pretrained(output_dir)
        else:
            patience_counter += 1
            if patience_counter >= args.patience:
                print(f"\n  Early stopping at epoch {epoch} "
                      f"(best val balanced acc: {best_bal_acc:.4f})")
                break

    print(f"\n  Best validation balanced accuracy: {best_bal_acc:.4f}")

    # ------------------------------------------------------------------
    # 5. Evaluate on test set
    # ------------------------------------------------------------------
    print("\n[4/5] Evaluating on test set ...")

    # Reload best model
    model = ConvNextV2ForImageClassification.from_pretrained(
        output_dir, num_labels=NUM_CLASSES
    ).to(device)

    # Evaluate with unweighted loss for clean metrics
    test_criterion = nn.CrossEntropyLoss()
    test_results = evaluate(model, test_loader, test_criterion, device)

    print(f"\n  Test accuracy:          {test_results['accuracy']:.4f}")
    print(f"  Test balanced accuracy: {test_results['balanced_accuracy']:.4f}")
    print(f"  Test loss:              {test_results['loss']:.4f}")

    # Classification report
    target_names = [CLASS_NAMES[i] for i in range(NUM_CLASSES)]
    report = classification_report(
        test_results["labels"], test_results["preds"],
        target_names=target_names, output_dict=True,
    )
    print("\n  Classification Report:")
    print(classification_report(
        test_results["labels"], test_results["preds"],
        target_names=target_names,
    ))

    # Confusion matrix
    cm = confusion_matrix(test_results["labels"], test_results["preds"])
    print("  Confusion Matrix:")
    print(f"  {'':>15} {'Pred Low':>10} {'Pred Med':>10} {'Pred High':>10}")
    for i, row in enumerate(cm):
        print(f"  {'True ' + target_names[i]:>15} {row[0]:>10} {row[1]:>10} {row[2]:>10}")

    # ------------------------------------------------------------------
    # 6. Save outputs
    # ------------------------------------------------------------------
    print("\n[5/5] Saving outputs ...")

    # Save metrics summary
    metrics = {
        "test_accuracy": float(test_results["accuracy"]),
        "test_balanced_accuracy": float(test_results["balanced_accuracy"]),
        "test_loss": float(test_results["loss"]),
        "best_val_balanced_accuracy": float(best_bal_acc),
        "classification_report": report,
        "confusion_matrix": cm.tolist(),
        "class_names": CLASS_NAMES,
        "args": vars(args),
    }
    metrics_path = output_dir / "metrics_summary.json"
    with open(metrics_path, "w") as f:
        json.dump(metrics, f, indent=2)
    print(f"  Metrics: {metrics_path}")

    # Save per-split predictions
    for split_name, split_csv, split_loader in [
        ("train", train_csv, train_loader),
        ("val", val_csv, val_loader),
        ("test", test_csv, test_loader),
    ]:
        results = evaluate(model, split_loader, test_criterion, device)
        split_df = pd.read_csv(split_csv)
        pred_df = pd.DataFrame({
            "image_path": split_df["image_path"].values,
            "true_label": results["labels"],
            "pred_label": results["preds"],
            "p0": results["probs"][:, 0],
            "p1": results["probs"][:, 1],
            "p2": results["probs"][:, 2],
        })
        pred_path = output_dir / f"{split_name}_preds.csv"
        pred_df.to_csv(pred_path, index=False)
        print(f"  {split_name} preds: {pred_path}")

    print("\n" + "=" * 70)
    print("Done. Model saved to:", output_dir)
    print("=" * 70)


if __name__ == "__main__":
    main()
