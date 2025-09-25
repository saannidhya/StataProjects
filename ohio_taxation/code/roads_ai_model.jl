using Random, StatsBase
using CSV, DataFrames
using Glob, Images
using Pkg
using Conda
using BSON
using PyCall, Flux, CUDA, Images, MLUtils
using ImageTransformations, FileIO

# necessary checks
println("python:", PyCall.pyimport("platform").python_version())
println("exe    :", PyCall.pyimport("sys").executable)
println("torch  :", PyCall.pyimport("torch").__version__)
println("HF     :", PyCall.pyimport("transformers").__version__)

# input location
input_dir = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads"

# Import CSV file
roadRunner_labels = CSV.read(joinpath(input_dir, "roadRunner_labels.csv"), DataFrame)
unique_labels = sort!(unique(roadRunner_labels.int_label))
num_classes   = length(unique_labels)

# Import all PNG images from the directory
image_paths = glob("*.png", joinpath(input_dir, "centered_roadrunner_pngs"))
# images = [load(path) for path in image_paths]

#=======================================================================================#
# Train, Validation, and Test Split 
#=======================================================================================#

# Extract IDs from image paths
image_ids = [(parse(Int, split(split(basename(path), "_")[2], ".")[1]), path) for path in image_paths]

image_df = DataFrame(id = first.(image_ids), image_path = last.(image_ids))

# Join with roadRunner_labels on id
roadRunner_labels = leftjoin(roadRunner_labels, image_df, on = :id)

# Create stratified train/validation/test splits with weighted sampling

# Set random seed for reproducibility
Random.seed!(42)

# Define split proportions
train_prop = 0.7
val_prop = 0.15
test_prop = 0.15

# Create weighted stratified splits
function create_weighted_stratified_splits(df::DataFrame, label_col::Symbol; 
                                         train_prop=0.7, val_prop=0.15, test_prop=0.15,
                                         weights=[3.0, 2.0, 1.0])  # Higher weights for lower labels
    
    splits = DataFrame()
    
    for (i, label) in enumerate(sort(unique(df[!, label_col])))
        label_data = df[df[!, label_col] .== label, :]
        n_label = nrow(label_data)
        
        # Apply weight to determine number of samples for each split
        weight = weights[min(i, length(weights))]
        
        # Calculate split sizes with weighting
        n_train = max(1, Int(round(n_label * train_prop * weight / sum(weights))))
        n_val = max(1, Int(round(n_label * val_prop * weight / sum(weights))))
        n_test = max(1, Int(round(n_label * test_prop * weight / sum(weights))))
        
        # Ensure we don't exceed total samples
        total_needed = n_train + n_val + n_test
        if total_needed > n_label
            # Proportionally reduce
            factor = n_label / total_needed
            n_train = max(1, Int(round(n_train * factor)))
            n_val = max(1, Int(round(n_val * factor)))
            n_test = n_label - n_train - n_val
        end
        
        # Randomly sample indices
        indices = randperm(n_label)
        
        # Create split assignments
        split_assignments = vcat(
            fill("train", n_train),
            fill("val", n_val), 
            fill("test", n_test),
            fill("unused", n_label - n_train - n_val - n_test)
        )
        
        # Create temporary dataframe for this label
        temp_df = copy(label_data[indices, :])
        temp_df.split = split_assignments
        
        splits = vcat(splits, temp_df)
    end
    
    return splits
end

# Apply the weighted stratified split
roadRunner_labels = create_weighted_stratified_splits(roadRunner_labels, :int_label)

# Display split statistics
println("Split distribution by label:")
split_stats = combine(groupby(roadRunner_labels, [:int_label, :split]), nrow => :count)
println(split_stats)

# Create separate dataframes for each split (excluding unused samples)
train_data = roadRunner_labels[roadRunner_labels.split .== "train", :]
val_data = roadRunner_labels[roadRunner_labels.split .== "val", :]
test_data = roadRunner_labels[roadRunner_labels.split .== "test", :]

println("\nFinal split sizes:")
println("Train: $(nrow(train_data)) samples")
println("Validation: $(nrow(val_data)) samples") 
println("Test: $(nrow(test_data)) samples")


CSV.write(joinpath(input_dir, "roadRunner_labels_w_img_path.csv"), roadRunner_labels)
CSV.write(joinpath(input_dir, "roadRunner_train_data.csv"), train_data)
CSV.write(joinpath(input_dir, "roadRunner_test_data.csv"), test_data)
CSV.write(joinpath(input_dir, "roadRunner_val_data.csv"), val_data)

vscodedisplay(roadRunner_labels)

grouped_counts = combine(groupby(roadRunner_labels, :int_label), nrow => :count)
#    1 │         0    712
#    2 │         1   5419
#    3 │         2  48616


#=======================================================================================#
# Python Setup 
#=======================================================================================#
## Setup and Model loading

# Install required Python packages
# Conda.add("transformers")
# Conda.add("pytorch")
# Conda.add("torchvision") 
# Conda.add("pillow")
# Conda.add("numpy")

# const ENV_NAME   = "pytorch"
# const ENV_PATH   = joinpath(Conda.ROOTENV, "envs", ENV_NAME)
# const PY_EXE     = Sys.iswindows() ? joinpath(ENV_PATH, "python.exe") : joinpath(ENV_PATH, "bin", "python")
# env = "pytorch"
# Conda.create(env)

# 1) Create the env if it doesn't exist (Conda.create expects a PATH in your Conda.jl)
# if !isdir(ENV_PATH)
#     mkpath(ENV_PATH)
#     Conda.create(ENV_PATH)  # << key change: create by PATH, not name
# end

# # 2) Point PyCall to that env's python and (re)build if needed
# if get(ENV, "PYTHON", "") != PY_EXE || !isfile(PyCall.PYTHONHOME[])  # force if wrong python in use
#     ENV["PYTHON"] = PY_EXE
#     Pkg.build("PyCall")
#     # It’s safest to stop here because PyCall needs a fresh session to load the new Python DLL.
#     # You can comment the next two lines if you are running from REPL and will restart manually.
#     @info "PyCall rebuilt to use $PY_EXE. Please restart Julia and re-run the script."
#     exit()  # comment this out if you don't want an automatic exit after rebuild
# end

# # 3) Install Python deps *into this env* via pip (avoid conda/pip mixing for these wheels)
# Conda.pip_interop(true, ENV_NAME)  # important: target the named env by name
# Conda.pip("install", ["--upgrade","pip","setuptools","wheel"], ENV_NAME)

# # CPU PyTorch (use the CUDA index URL variant below if you want GPU)
# Conda.pip("install", ["torch","torchvision","torchaudio"], ENV_NAME)

# # HuggingFace stack (pin safetensors to a stable Windows wheel)
# Conda.pip("install", ["safetensors==0.4.4","transformers==4.44.2","numpy","pillow"], ENV_NAME)

# --- If you want CUDA 12.1 builds instead of CPU, replace the torch line above with:
# Conda.pip("install",
#     ["torch","torchvision","torchaudio","--index-url","https://download.pytorch.org/whl/cu121"],
#     ENV_NAME)

# 4) Quick sanity print (helps confirm we’re using the right Python)
# @info "Using Python" PY_EXE


#=======================================================================================#
# Fine-tuning ConvNeXt V2 for Road Blemish Classification
#=======================================================================================#


# ========= HF Fine-tuning (pure PyTorch via PyCall) =========
using PyCall

const MODEL_NAME = "facebook/convnextv2-base-1k-224"   # good starter
# Alternative (backbone is heavier): "facebook/dinov2-base" works too
# const MODEL_NAME = "facebook/dinov2-base"

# Build lists for Python
train_paths = collect(String.(train_data.image_path))
train_labels = collect(Int.(train_data.int_label))
val_paths   = collect(String.(val_data.image_path))
val_labels  = collect(Int.(val_data.int_label))


# Output directory for saved model
outdir = joinpath(input_dir, "hf_finetuned_convnextv2")  # change if you switch model

# Hand off to Python to run the training loop (no accelerate/datasets needed)
py"""
import os
import torch
from PIL import Image
from torch.utils.data import Dataset, DataLoader
from transformers import AutoImageProcessor, AutoModelForImageClassification
from torch.optim import AdamW
import numpy as np

device = "cuda" if torch.cuda.is_available() else "cpu"
print("Using device:", device)

train_paths = $train_paths
train_labels = $train_labels
val_paths   = $val_paths
val_labels  = $val_labels
model_name  = $MODEL_NAME
outdir      = $outdir
num_classes = int($num_classes)

os.makedirs(outdir, exist_ok=True)

processor = AutoImageProcessor.from_pretrained(model_name)
# Ensure Python lists, not numpy arrays
train_labels = train_labels.tolist() if hasattr(train_labels, "tolist") else list(train_labels)
val_labels   = val_labels.tolist()   if hasattr(val_labels, "tolist")   else list(val_labels)

# Get the sorted list of class ids
classes = sorted(set(map(int, train_labels)) | set(map(int, val_labels)))

id2label = {i: str(i) for i in classes}
label2id = {str(i): i for i in classes}

model = AutoModelForImageClassification.from_pretrained(
    model_name,
    num_labels=num_classes,
    id2label=id2label,
    label2id=label2id,
    ignore_mismatched_sizes=True,
).to(device)

class RoadDataset(Dataset):
    def __init__(self, paths, labels):
        self.paths = paths
        self.labels = labels
    def __len__(self): return len(self.paths)
    def __getitem__(self, idx):
        img = Image.open(self.paths[idx]).convert("RGB")
        return img, int(self.labels[idx])

def collate(batch):
    imgs, labels = zip(*batch)
    enc = processor(images=list(imgs), return_tensors="pt")
    enc["labels"] = torch.tensor(labels, dtype=torch.long)
    return enc

# num_workers=0 is safest when called via PyCall on Windows
train_loader = DataLoader(RoadDataset(train_paths, train_labels), batch_size=16, shuffle=True,  num_workers=0, collate_fn=collate)
val_loader   = DataLoader(RoadDataset(val_paths,   val_labels),   batch_size=32, shuffle=False, num_workers=0, collate_fn=collate)

optimizer = AdamW(model.parameters(), lr=5e-5, weight_decay=0.01)

def evaluate():
    model.eval()
    tot = 0; correct = 0; tot_loss = 0.0
    with torch.no_grad():
        for batch in val_loader:
            for k in ["pixel_values", "labels"]:
                batch[k] = batch[k].to(device)
            out = model(**batch)
            loss = out.loss
            logits = out.logits
            tot_loss += float(loss.item()) * batch["labels"].size(0)
            preds = logits.argmax(dim=-1)
            correct += int((preds == batch["labels"]).sum().item())
            tot += int(batch["labels"].size(0))
    return tot_loss / max(1, tot), correct / max(1, tot)

EPOCHS = 3
best_val_loss = float("inf")
for epoch in range(1, EPOCHS+1):
    model.train()
    running = 0.0
    seen = 0
    for step, batch in enumerate(train_loader, start=1):
        for k in ["pixel_values", "labels"]:
            batch[k] = batch[k].to(device)
        out = model(**batch)
        loss = out.loss
        loss.backward()
        optimizer.step()
        optimizer.zero_grad()

        bs = int(batch["labels"].size(0))
        running += float(loss.item()) * bs
        seen += bs

        if step % 50 == 0:
            print(f"Epoch {epoch} | step {step} | train_loss {running/seen:.4f}")

    val_loss, val_acc = evaluate()
    print(f"Epoch {epoch} done. val_loss={val_loss:.4f}  val_acc={val_acc:.4f}")

    if val_loss < best_val_loss:
        best_val_loss = val_loss
        model.save_pretrained(outdir)
        processor.save_pretrained(outdir)
        print("Saved best model to", outdir)

print("Training complete.")
"""

#=======================================================================================#
# Julia REPL crashed, likely due to memory issues? But the model seems to have trained and saved.
# Error message:
# The terminal process "C:\Users\rawatsa\AppData\Local\Programs\Julia-1.11.7\bin\julia.exe '-i', '--banner=no', '--project=c:\Users\rawatsa\OneDrive - University of Cincinnati\StataProjects\ohio_taxation\data\roads\Road Quality', 'c:\Users\rawatsa\.vscode\extensions\julialang.language-julia-1.149.2\scripts\terminalserver\terminalserver.jl', '\\.\pipe\vsc-jl-repl-51a8d868-e5ba-4f3a-af64-d5a9ac689e1b', '\\.\pipe\vsc-jl-repldbg-95b7016a-c1d3-4c10-9a96-1ba4d66f6b8d', '\\.\pipe\vsc-jl-cr-0d269a71-8812-4981-b6c2-9306a4772a1f', 'USE_REVISE=true', 'USE_PLOTPANE=true', 'USE_PROGRESS=true', 'ENABLE_SHELL_INTEGRATION=true', 'DEBUG_MODE=false'" terminated with exit code: 1.
#=======================================================================================#

# Checking if model runs:

# importing train, test, validation data again to be safe
train_data = CSV.read(joinpath(input_dir, "roadRunner_train_data.csv"), DataFrame)
test_data  = CSV.read(joinpath(input_dir, "roadRunner_test_data.csv"), DataFrame)
val_data = CSV.read(joinpath(input_dir, "roadRunner_val_data.csv"), DataFrame)

# Build some sample paths
sample_paths = collect(String.(val_data.image_path[1:min(8, nrow(val_data))]))

py"""
from transformers import AutoImageProcessor, AutoModelForImageClassification
from PIL import Image
import torch, os

outdir = $outdir
device = "cuda" if torch.cuda.is_available() else "cpu"

processor = AutoImageProcessor.from_pretrained(outdir)
model = AutoModelForImageClassification.from_pretrained(outdir).to(device)

imgs = [Image.open(p).convert("RGB") for p in $sample_paths]
enc  = processor(images=imgs, return_tensors="pt")
with torch.no_grad():
    logits = model(pixel_values=enc["pixel_values"].to(device)).logits
preds = logits.argmax(-1).cpu().tolist()

print("Loaded checkpoint from:", outdir)
print("id2label:", model.config.id2label)
print("sample preds:", preds)
"""

# println("First 8 rows of validation data:")
# println(first(val_data, 8))


#=======================================================================================#
# Fine-tuned model: In-sample and out-of-sample predictions
#=======================================================================================#

# Build lists for all splits
train_paths = collect(String.(train_data.image_path))
train_labels = collect(Int.(train_data.int_label))
val_paths   = collect(String.(val_data.image_path))
val_labels  = collect(Int.(val_data.int_label))
test_paths  = collect(String.(test_data.image_path))
test_labels = collect(Int.(test_data.int_label))


py"""
import os, csv, json, numpy as np, torch
from PIL import Image
from torch.utils.data import DataLoader, Dataset
from transformers import AutoImageProcessor, AutoModelForImageClassification

# Load the trained model and processor from the output directory
outdir = $outdir
device = "cuda" if torch.cuda.is_available() else "cpu"

processor = AutoImageProcessor.from_pretrained(outdir)
model     = AutoModelForImageClassification.from_pretrained(outdir).to(device)

# Custom Dataset class to handle image paths and labels
class RoadDataset(Dataset):
    def __init__(self, paths, labels):
        self.paths = list(paths)  # Convert to list for safety
        self.labels = list(map(int, labels))  # Ensure labels are integers
    def __len__(self): return len(self.paths)
    def __getitem__(self, idx):
        # Load image and convert to RGB format
        img = Image.open(self.paths[idx]).convert("RGB")
        return img, self.labels[idx]

# Collate function to batch images and labels for the DataLoader
def collate(batch):
    imgs, labels = zip(*batch)
    # Process images using the HuggingFace processor
    enc = processor(images=list(imgs), return_tensors="pt")
    # Add labels to the batch
    enc["labels"] = torch.tensor(labels, dtype=torch.long)
    return enc

# Helper function to create a DataLoader for a given dataset split
def make_loader(paths, labels, bs=32):
    return DataLoader(RoadDataset(paths, labels), batch_size=bs, shuffle=False, num_workers=0, collate_fn=collate)

# Main evaluation function that computes metrics and saves predictions
def eval_and_dump(paths, labels, split_name, bs=32):
    # Create DataLoader for this split
    loader = make_loader(paths, labels, bs)
    model.eval()  # Set model to evaluation mode
    
    # Initialize lists to store results
    all_true, all_pred, all_prob, all_top3 = [], [], [], []
    
    # Process all batches without gradient computation
    with torch.no_grad():
        for batch in loader:
            # Move pixel values to device and get model predictions
            pv = batch["pixel_values"].to(device)
            logits = model(pixel_values=pv).logits
            
            # Convert logits to probabilities
            probs  = torch.softmax(logits, dim=-1)
            
            # Get top-1 predictions (highest probability class)
            top1 = probs.argmax(-1).cpu().numpy()
            
            # Get top-3 predictions for top-k accuracy calculation
            top3 = torch.topk(probs, k=min(3, probs.shape[1]), dim=-1).indices.cpu().numpy()
            
            # Store predictions, probabilities, and true labels
            all_pred.extend(top1.tolist())
            all_top3.extend(top3.tolist())
            all_prob.extend(probs.max(-1).values.cpu().tolist())  # Max probability for each sample
            all_true.extend(batch["labels"].numpy().tolist())

    # Convert to numpy arrays for metric calculations
    y    = np.array(all_true, dtype=int)  # True labels
    yhat = np.array(all_pred, dtype=int)  # Predicted labels
    
    # Build confusion matrix
    K = len(model.config.id2label)  # Number of classes
    cm = np.zeros((K, K), dtype=int)
    for t, p in zip(y, yhat): cm[t, p] += 1

    # Calculate classification metrics
    acc  = float((y == yhat).mean())  # Overall accuracy
    prec = np.divide(np.diag(cm), np.maximum(cm.sum(0), 1))  # Per-class precision
    rec  = np.divide(np.diag(cm), np.maximum(cm.sum(1), 1))  # Per-class recall
    f1   = np.divide(2*prec*rec, np.maximum(prec+rec, 1e-12))  # Per-class F1-score
    support = cm.sum(1)  # Number of samples per class
    top3 = float(np.mean([ int(t) in row for t, row in zip(y, all_top3) ]))  # Top-3 accuracy

    # Save detailed predictions to CSV file
    out_csv = os.path.join(outdir, f"{split_name}_preds.csv")
    with open(out_csv, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["image_path","true","pred","max_prob"])  # CSV header
        # Write each prediction with image path, true label, predicted label, and confidence
        for pth, t, p, s in zip(paths, y.tolist(), yhat.tolist(), all_prob):
            w.writerow([pth, int(t), int(p), float(s)])

    # Print summary metrics to console
    print(f"[{split_name}] acc={acc:.4f} top3={top3:.4f}")
    print("  per-class precision:", [round(float(x),4) for x in prec])
    print("  per-class recall   :", [round(float(x),4) for x in rec])
    print("  per-class f1       :", [round(float(x),4) for x in f1])
    print("  confusion matrix:\\n", cm)
    print("  wrote:", out_csv)

    # Return metrics dictionary for JSON storage
    return {
        "n": int(len(y)),
        "acc": acc,
        "top3_acc": top3,
        "per_class": {
            "precision": [float(x) for x in prec],
            "recall":    [float(x) for x in rec],
            "f1":        [float(x) for x in f1],
            "support":   [int(x)   for x in support],
        },
        "confusion_matrix": cm.tolist(),
        "csv": out_csv,
    }

# Evaluate model on all three splits (train, validation, test)
metrics = {
    "train": eval_and_dump($train_paths, $train_labels, "train", bs=32),
    "val":   eval_and_dump($val_paths,   $val_labels,   "val",   bs=32),
    "test":  eval_and_dump($test_paths,  $test_labels,  "test",  bs=32),
}

# Save comprehensive metrics summary to JSON file
with open(os.path.join(outdir, "metrics_summary.json"), "w") as f:
    json.dump(metrics, f, indent=2)
print("Saved metrics JSON to:", os.path.join(outdir, "metrics_summary.json"))
"""

#=========================================================================================#
# Fine-tuned model: evaluation results
#=========================================================================================#

# Extract Python metrics back to Julia
train_metrics = py"metrics['train']"
val_metrics = py"metrics['val']"
test_metrics = py"metrics['test']"

# Convert to Julia dictionaries and display results
println("\n=== MODEL EVALUATION RESULTS ===")
println("\nTRAIN SET:")
println("  Accuracy: $(round(train_metrics["acc"], digits=4))")
# println("  Top-3 Accuracy: $(round(train_metrics["top3_acc"], digits=4))")
println("  Sample size: $(train_metrics["n"])")

println("\nVALIDATION SET:")
println("  Accuracy: $(round(val_metrics["acc"], digits=4))")
# println("  Top-3 Accuracy: $(round(val_metrics["top3_acc"], digits=4))")
println("  Sample size: $(val_metrics["n"])")

println("\nTEST SET:")
println("  Accuracy: $(round(test_metrics["acc"], digits=4))")
# println("  Top-3 Accuracy: $(round(test_metrics["top3_acc"], digits=4))")
println("  Sample size: $(test_metrics["n"])")

# Per-class metrics for test set
test_per_class = DataFrame(
    class = 0:(length(test_metrics["per_class"]["precision"])-1),
    precision = test_metrics["per_class"]["precision"],
    recall = test_metrics["per_class"]["recall"],
    f1_score = test_metrics["per_class"]["f1"],
    support = test_metrics["per_class"]["support"]
)

println("\nPER-CLASS METRICS (Test Set):")
println(test_per_class)

# Load prediction results as DataFrames
train_preds = CSV.read(joinpath(outdir, "train_preds.csv"), DataFrame)
val_preds = CSV.read(joinpath(outdir, "val_preds.csv"), DataFrame)
test_preds = CSV.read(joinpath(outdir, "test_preds.csv"), DataFrame)

println("\nPREDICTION FILES LOADED:")
println("  Train predictions: $(nrow(train_preds)) rows")
println("  Validation predictions: $(nrow(val_preds)) rows")
println("  Test predictions: $(nrow(test_preds)) rows")

# Summary of model performance
model_summary = Dict(
    "model_path" => outdir,
    "num_classes" => num_classes,
    "train_acc" => train_metrics["acc"],
    "val_acc" => val_metrics["acc"],
    "test_acc" => test_metrics["acc"],
    "train_size" => train_metrics["n"],
    "val_size" => val_metrics["n"],
    "test_size" => test_metrics["n"]
)

println("\n=== MODEL SUMMARY ===")
for (key, value) in model_summary
    println("  $(key): $(value)")
end

#=======================================================================================#
# Base model predictions: not fine-tuned
# By base, I mean a linear probe on frozen features and a 3-class classifier
#=======================================================================================#

# ============================== BASELINE (Frozen Backbone, Linear Probe) ==============================
# Reuse your existing splits
train_paths = collect(String.(train_data.image_path))
train_labels = collect(Int.(train_data.int_label))
val_paths   = collect(String.(val_data.image_path))
val_labels  = collect(Int.(val_data.int_label))
test_paths  = collect(String.(test_data.image_path))
test_labels = collect(Int.(test_data.int_label))

baseline_outdir = joinpath(input_dir, "hf_baseline_convnextv2_frozen")

py"""
import os, csv, json, numpy as np, torch, time
from PIL import Image
from torch.utils.data import Dataset, DataLoader
from transformers import AutoImageProcessor, AutoModelForImageClassification
from torch.optim import AdamW
import torch.nn.functional as F

# ---------- config ----------
model_name = $MODEL_NAME  # "facebook/convnextv2-base-1k-224"
num_classes = int($num_classes)
outdir = $baseline_outdir
os.makedirs(outdir, exist_ok=True)
device = "cuda" if torch.cuda.is_available() else "cpu"
BATCH_TRAIN = 64
BATCH_EVAL  = 64
EPOCHS      = 2            # head-only; usually quick
LR_HEAD     = 1e-3
# ----------------------------

processor = AutoImageProcessor.from_pretrained(model_name)
# fixed mapping 0..num_classes-1
id2label = {i: str(i) for i in range(num_classes)}
label2id = {v: k for k, v in id2label.items()}

# fresh 3-class head; ignore_mismatched_sizes swaps out the 1k head to a 3-class head
model = AutoModelForImageClassification.from_pretrained(
    model_name,
    num_labels=num_classes,
    id2label=id2label,
    label2id=label2id,
    ignore_mismatched_sizes=True,
).to(device)

# freeze everything except the classifier head
for n,p in model.named_parameters():
    if not n.startswith("classifier."):
        p.requires_grad = False

head_params = [p for n,p in model.named_parameters() if n.startswith("classifier.")]
optimizer = AdamW(head_params, lr=LR_HEAD, weight_decay=0.0)

# --- dataset/dataloader (same as fine-tuning pipeline) ---
class RoadDataset(Dataset):
    def __init__(self, paths, labels):
        self.paths  = list(paths)
        self.labels = list(map(int, labels))
    def __len__(self): return len(self.paths)
    def __getitem__(self, idx):
        img = Image.open(self.paths[idx]).convert("RGB")
        return img, self.labels[idx]

def collate(batch):
    imgs, labels = zip(*batch)
    enc = processor(images=list(imgs), return_tensors="pt")
    enc["labels"] = torch.tensor(labels, dtype=torch.long)
    return enc

train_loader = DataLoader(RoadDataset($train_paths, $train_labels), batch_size=BATCH_TRAIN, shuffle=True,  num_workers=0, collate_fn=collate)
val_loader   = DataLoader(RoadDataset($val_paths,   $val_labels),   batch_size=BATCH_EVAL,  shuffle=False, num_workers=0, collate_fn=collate)
test_loader  = DataLoader(RoadDataset($test_paths,  $test_labels),  batch_size=BATCH_EVAL,  shuffle=False, num_workers=0, collate_fn=collate)

def eval_loader(loader, split_name):
    model.eval()
    all_true, all_pred, all_prob = [], [], []
    with torch.no_grad():
        for batch in loader:
            pv = batch["pixel_values"].to(device)
            logits = model(pixel_values=pv).logits
            probs  = F.softmax(logits, dim=-1)
            all_pred.extend(probs.argmax(-1).cpu().tolist())
            all_prob.extend(probs.max(-1).values.cpu().tolist())
            all_true.extend(batch["labels"].cpu().tolist())

    y = np.array(all_true, dtype=int); yhat = np.array(all_pred, dtype=int)
    K = len(id2label)
    cm = np.zeros((K, K), dtype=int)
    for t,p in zip(y,yhat): cm[t,p] += 1
    acc  = float((y==yhat).mean())
    prec = np.divide(np.diag(cm), np.maximum(cm.sum(0), 1))
    rec  = np.divide(np.diag(cm), np.maximum(cm.sum(1), 1))
    f1   = np.divide(2*prec*rec, np.maximum(prec+rec, 1e-12))

    # CSV
    out_csv = os.path.join(outdir, f"{split_name}_preds.csv")
    with open(out_csv, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["image_path","true","pred","max_prob"])
        # use the original order of dataset for paths
        paths = $val_paths if split_name=="val" else ($test_paths if split_name=="test" else $train_paths)
        for pth, t, p, s in zip(paths, y.tolist(), yhat.tolist(), all_prob):
            w.writerow([pth, int(t), int(p), float(s)])

    print(f"[BASELINE-{split_name}] acc={acc:.4f}")
    print("  per-class precision:", [round(float(x),4) for x in prec])
    print("  per-class recall   :", [round(float(x),4) for x in rec])
    print("  per-class f1       :", [round(float(x),4) for x in f1])
    print("  confusion matrix:\\n", cm)
    print("  wrote:", out_csv)

    # Return metrics dictionary for JSON storage
    return {
        "n": int(len(y)),
        "acc": acc,
        # "top3_acc": top3,
        "per_class": {
            "precision": [float(x) for x in prec],
            "recall":    [float(x) for x in rec],
            "f1":        [float(x) for x in f1],
            "support":   [int(x)   for x in support],
        },
        "confusion_matrix": cm.tolist(),
        "csv": out_csv,
    }

# Evaluate model on all three splits (train, validation, test)
metrics = {
    "train": eval_and_dump($train_paths, $train_labels, "train", bs=32),
    "val":   eval_and_dump($val_paths,   $val_labels,   "val",   bs=32),
    "test":  eval_and_dump($test_paths,  $test_labels,  "test",  bs=32),
}

# Save comprehensive metrics summary to JSON file
with open(os.path.join(outdir, "metrics_summary.json"), "w") as f:
    json.dump(metrics, f, indent=2)
print("Saved metrics JSON to:", os.path.join(outdir, "metrics_summary.json"))
"""

#=======================================================================================#
# Baseline model: evaluation results
#=======================================================================================#


# Load prediction results as DataFrames
train_preds = CSV.read(joinpath(baseline_outdir, "train_preds.csv"), DataFrame)
val_preds = CSV.read(joinpath(baseline_outdir, "val_preds.csv"), DataFrame)
test_preds = CSV.read(joinpath(baseline_outdir, "test_preds.csv"), DataFrame)

println("\nPREDICTION FILES LOADED:")
println("  Train predictions: $(nrow(train_preds)) rows")
println("  Validation predictions: $(nrow(val_preds)) rows")
println("  Test predictions: $(nrow(test_preds)) rows")

# Summary of model performance
# model_summary = Dict(
#     "model_path" => baseline_outdir,
#     "num_classes" => num_classes,
#     "train_acc" => train_metrics["acc"],
#     "val_acc" => val_metrics["acc"],
#     "test_acc" => test_metrics["acc"],
#     "train_size" => train_metrics["n"],
#     "val_size" => val_metrics["n"],
#     "test_size" => test_metrics["n"]
# )

# println("\n=== MODEL SUMMARY ===")
# for (key, value) in model_summary
#     println("  $(key): $(value)")
# end
