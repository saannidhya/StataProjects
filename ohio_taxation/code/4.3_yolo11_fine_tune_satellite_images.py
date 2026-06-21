from pathlib import Path
import shutil
import yaml
from ultralytics import YOLO
import numpy as np
import pandas as pd

# input location
input_dir = Path(r"C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads")

# Output directory for saved model (change if you switch model)
# outdir = input_dir / "runs_ohio" / "yolo11_finetune_satellite_images"

# Import dataframes created in 4.0_roads_ai_model.jl (from CSVs)
train_csv = input_dir / "roadRunner_train_data.csv"
test_csv = input_dir / "roadRunner_test_data.csv"
val_csv = input_dir / "roadRunner_val_data.csv"

# Check that split CSVs exist
missing = [p for p in (train_csv, test_csv, val_csv) if not p.exists()]
if missing:
    raise FileNotFoundError(
        "Missing split CSVs (expected outputs from 4.0_roads_ai_model.jl):\n"
        + "\n".join(str(p) for p in missing)
    )

train_data = pd.read_csv(train_csv)
test_data = pd.read_csv(test_csv)
val_data = pd.read_csv(val_csv)

print("=" * 70)
print("DATA SHAPE")
print("=" * 70)
print(f"Train: {train_data.shape[0]} samples")
print(f"Val:   {val_data.shape[0]} samples")
print(f"Test:  {test_data.shape[0]} samples")

print("\n" + "=" * 70)
print("SANITY CHECKS")
print("=" * 70)

# 1. Check unique labels across splits
train_labels = np.sort(train_data["int_label"].unique())
val_labels = np.sort(val_data["int_label"].unique())
test_labels = np.sort(test_data["int_label"].unique())
print(f"Train labels: {train_labels}")
print(f"Val labels:   {val_labels}")
print(f"Test labels:  {test_labels}")

num_classes = len(train_labels)
assert num_classes == 3, f"Expected 3 classes, got {num_classes}"
print(f"✓ All splits have {num_classes} classes (0=low, 1=medium, 2=high quality)")

# 2. Check class distribution
print("\nClass distribution:")
for split_name, df in [("Train", train_data), ("Val", val_data), ("Test", test_data)]:
    counts = df["int_label"].value_counts().sort_index()
    print(f"  {split_name:6s}: {counts.to_dict()}")

# 3. Check image paths exist
def check_images_exist(df, split_name):
    missing = []
    for idx, row in df.iterrows():
        img_path = Path(row["image_path"])
        if not img_path.exists():
            missing.append(str(img_path))
    if missing:
        print(f"✗ {split_name}: {len(missing)} missing images (first 5):")
        for p in missing[:5]:
            print(f"    {p}")
        return False
    print(f"✓ {split_name}: All {len(df)} images exist")
    return True

all_exist = True
for split_name, df in [("Train", train_data), ("Val", val_data), ("Test", test_data)]:
    all_exist &= check_images_exist(df, split_name)

if not all_exist:
    print("\n⚠ Some images are missing. Please check paths.")
else:
    print("\n✓ All image files found!")

# 4. Create YOLO classification folder structure
# For YOLO classification: dataset/train/class_id/image.png
print("\n" + "=" * 70)
print("CREATING YOLO CLASSIFICATION FOLDER STRUCTURE")
print("=" * 70)

YOLO_ROOT = input_dir / "runs_ohio" / "yolo11_finetune_satellite_images"
TRAIN_DIR = YOLO_ROOT / "train"
VAL_DIR = YOLO_ROOT / "val"
TEST_DIR = YOLO_ROOT / "test"

# Create directories
for class_id in range(num_classes):
    (TRAIN_DIR / str(class_id)).mkdir(parents=True, exist_ok=True)
    (VAL_DIR / str(class_id)).mkdir(parents=True, exist_ok=True)
    (TEST_DIR / str(class_id)).mkdir(parents=True, exist_ok=True)

# Copy images to appropriate folders
def organize_images(df, target_dir, split_name):
    print(f"Organizing {split_name} images...")
    for idx, row in df.iterrows():
        src = Path(row["image_path"])
        class_id = row["int_label"]
        dst = target_dir / str(class_id) / src.name
        if src.exists():
            shutil.copy2(src, dst)
    print(f"  ✓ {split_name} complete: {len(df)} images copied")

organize_images(train_data, TRAIN_DIR, "train")
organize_images(val_data, VAL_DIR, "val")
organize_images(test_data, TEST_DIR, "test")

# Verify folder structure
print("\nVerifying folder structure:")
for split_dir, split_name in [(TRAIN_DIR, "train"), (VAL_DIR, "val"), (TEST_DIR, "test")]:
    for class_id in range(num_classes):
        class_dir = split_dir / str(class_id)
        count = len(list(class_dir.glob("*.png")))
        print(f"  {split_name}/{class_id}: {count} images")

# Class names mapping
CLASS_NAMES = {0: "low_quality", 1: "medium_quality", 2: "high_quality"}

print("\n✓ YOLO classification folder structure ready!")
print(f"  Root: {YOLO_ROOT}")
print(f"  Classes: {CLASS_NAMES}")


#========================================================================#
# Fine-tuning a pre-trained YOLOv11 model on the Satellite Images
#========================================================================#
print("\n" + "=" * 70)
print("FINE-TUNING YOLO11 CLASSIFICATION MODEL")
print("=" * 70)

# Load pre-trained YOLO11 classification model
model = YOLO("yolo11n-cls.pt")  # Note: -cls for classification

# Train the model
# NOTE: For classification, pass the directory path (not a YAML file)
results = model.train(
    data=str(YOLO_ROOT),
    epochs=30,
    imgsz=224,  # Standard for classification
    batch=8,
    device="cpu",  # Change to 0 for GPU
    workers=0,
    amp=False,
    project=str(YOLO_ROOT),
    name="yolo11n_cls_roads",
    patience=10,  # Early stopping
    save=True,
    plots=True
)


#========================================================================#
# Evaluating the fine-tuned model on the validation set
#========================================================================#
print("\n" + "=" * 70)
print("VALIDATION METRICS")
print("=" * 70)

metrics = model.val(data=str(YOLO_ROOT), split="val")

# Extract metrics
top1 = float(getattr(metrics, "top1", np.nan))
top5 = float(getattr(metrics, "top5", np.nan))

overall = pd.DataFrame([{
    "Top-1 Accuracy": top1,
    "Top-5 Accuracy": top5,
}])

print("\nOVERALL METRICS")
print(overall.to_string(index=False))

# Per-class metrics (if available)
try:
    # Confusion matrix
    if hasattr(metrics, "confusion_matrix"):
        cm = metrics.confusion_matrix.matrix
        cm_df = pd.DataFrame(
            cm,
            index=[f"True_{CLASS_NAMES[i]}" for i in range(num_classes)],
            columns=[f"Pred_{CLASS_NAMES[i]}" for i in range(num_classes)]
        )
        print("\nCONFUSION MATRIX")
        print(cm_df)
        cm_df.to_csv(str(YOLO_ROOT / "yolo11n_cls_roads" / "confusion_matrix.csv"))
except Exception as e:
    print(f"Could not extract confusion matrix: {e}")

# Save overall metrics
metrics_path = YOLO_ROOT / "yolo11n_cls_roads" / "val_metrics.csv"
overall.to_csv(str(metrics_path), index=False)
print(f"\n✓ Metrics saved to: {metrics_path}")


#========================================================================#
# Performing inference on validation images
#========================================================================#
print("\n" + "=" * 70)
print("INFERENCE ON VALIDATION SET")
print("=" * 70)

pred = model.predict(
    source=str(VAL_DIR),
    imgsz=224,
    device="cpu",
    save=True,
    project=str(YOLO_ROOT),
    name="pred_val_examples"
)

print(f"✓ Predictions saved to: {YOLO_ROOT / 'pred_val_examples'}")

# Create a predictions summary
pred_results = []
for r in pred:
    img_path = Path(r.path)
    top1_class = int(r.probs.top1)
    top1_conf = float(r.probs.top1conf)
    pred_results.append({
        "image": img_path.name,
        "predicted_class": top1_class,
        "predicted_label": CLASS_NAMES[top1_class],
        "confidence": top1_conf
    })

pred_df = pd.DataFrame(pred_results)
pred_csv = YOLO_ROOT / "pred_val_examples" / "predictions_summary.csv"
pred_df.to_csv(str(pred_csv), index=False)
print(f"✓ Prediction summary saved to: {pred_csv}")

print("\n" + "=" * 70)
print("TRAINING COMPLETE")
print("=" * 70)
print(f"Model weights: {YOLO_ROOT / 'yolo11n_cls_roads' / 'weights' / 'best.pt'}")
print(f"Validation plots: {YOLO_ROOT / 'yolo11n_cls_roads'}")
print(f"Predictions: {YOLO_ROOT / 'pred_val_examples'}")