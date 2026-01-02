#===================================================================================================================================##
#
# Purpose: Using fine-tuned convnextv2 in roads_ai_model.jl to predict road quality classes for Ohio areas with "close" elections
#
#===================================================================================================================================#

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

# set root location 
root = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data_dir = root * "/data/roads"
code_dir = root * "/code"
model_dir = root * "/data/roads/hf_finetuned_convnextv2"
# directory with images
img_dir_a = data_dir * "/ohio/above"
img_dir_b = data_dir * "/ohio/below"



#=========================================================================================#
# Load fine-tuned model and tokenizer 
#=========================================================================================#

# load the fine-tuned model and processor
model = PyCall.pyimport("transformers").AutoModelForImageClassification.from_pretrained(model_dir)
processor = PyCall.pyimport("transformers").AutoImageProcessor.from_pretrained(model_dir)
# model and processor are now ready for inference

#=========================================================================================#
# Importing images (there are no labels)
#=========================================================================================#

# get all image paths
img_paths_a = glob("*.jpg", img_dir_a)
img_paths_b = glob("*.jpg", img_dir_b)


#==========================================================================================#
# Inference on "above" and "below" images
#==========================================================================================#

# --------------------------------------------
# Inference on "above" and "below" directories
# --------------------------------------------

# Where to save the outputs
pred_outdir = joinpath(model_dir, "ohio_preds")
isdir(pred_outdir) || mkpath(pred_outdir)

py"""
import os, csv, torch, numpy as np
from PIL import Image
from torch.utils.data import Dataset, DataLoader
from transformers import AutoImageProcessor, AutoModelForImageClassification

# ==== KNOBS ====
BATCH        = 64    # raise if you have RAM; lower if you see OOM
NUM_WORKERS  = 0     # keep 0 when called via PyCall on Windows
WRITE_ALL_P  = True  # write p0,p1,p2 columns (handy for thresholds)
PRINT_EVERY  = 50    # progress print frequency in steps
# ===============

model_dir    = $model_dir
outdir       = $pred_outdir
paths_above  = $img_paths_a
paths_below  = $img_paths_b

device = "cuda" if torch.cuda.is_available() else "cpu"
processor = AutoImageProcessor.from_pretrained(model_dir)
model     = AutoModelForImageClassification.from_pretrained(model_dir).to(device)
id2label  = model.config.id2label

def id_to_name(i):
    # robust mapping whether keys are int or str in the config
    try:
        return id2label[i]
    except Exception:
        return id2label.get(str(i), str(i))

class ImgSet(Dataset):
    def __init__(self, paths):
        self.paths = list(paths)
    def __len__(self): return len(self.paths)
    def __getitem__(self, idx):
        p = self.paths[idx]
        try:
            img = Image.open(p).convert("RGB")
        except Exception as e:
            # if unreadable image, return a small white image and mark path
            img = Image.new("RGB", (224,224), color=(255,255,255))
        return img, p  # return both image and path

def collate(batch):
    imgs, paths = zip(*batch)
    enc = processor(images=list(imgs), return_tensors="pt")
    enc["paths"] = list(paths)  # carry paths through the loader
    return enc

def run_infer(paths, tag):
    ds = ImgSet(paths)
    if len(ds) == 0:
        print(f"[{tag}] no images found, skipping.")
        return None

    loader = DataLoader(ds, batch_size=BATCH, shuffle=False,
                        num_workers=NUM_WORKERS, collate_fn=collate)

    model.eval()
    out_csv = os.path.join(outdir, f"{tag}_preds.csv")
    wrote_header = False
    n = len(ds)

    with open(out_csv, "w", newline="") as f, torch.no_grad():
        w = csv.writer(f)

        for step, batch in enumerate(loader, start=1):
            pv = batch["pixel_values"].to(device)
            logits = model(pixel_values=pv).logits
            probs  = torch.softmax(logits, dim=-1).cpu().numpy()
            preds  = probs.argmax(axis=1)
            maxp   = probs.max(axis=1)

            paths_b = batch["paths"]

            # Write header once
            if not wrote_header:
                hdr = ["image_path", "pred_id", "pred_label", "max_prob"]
                if WRITE_ALL_P and probs.shape[1] == 3:
                    hdr += ["p0","p1","p2"]
                w.writerow(hdr)
                wrote_header = True

            for i in range(len(paths_b)):
                row = [paths_b[i], int(preds[i]), id_to_name(int(preds[i])), float(maxp[i])]
                if WRITE_ALL_P and probs.shape[1] == 3:
                    row += [float(probs[i,0]), float(probs[i,1]), float(probs[i,2])]
                w.writerow(row)

            # progress/ETA print
            if step % PRINT_EVERY == 0 or step == len(loader):
                done = step * loader.batch_size
                done = min(done, n)
                print(f"[{tag}] {done}/{n} processed")

    # quick summary counts
    # reload to count by class robustly
    import collections
    counts = collections.Counter()
    with open(out_csv, newline="") as f:
        r = csv.DictReader(f)
        for row in r:
            counts[row["pred_label"]] += 1

    print(f"[{tag}] wrote:", out_csv)
    print(f"[{tag}] class distribution:", dict(counts))
    return out_csv

csv_above = run_infer(paths_above, "ohio_above")
csv_below = run_infer(paths_below, "ohio_below")
"""


# Load the CSVs back to Julia for quick inspection (if they were created)
csv_above = joinpath(pred_outdir, "ohio_above_preds.csv")
csv_below = joinpath(pred_outdir, "ohio_below_preds.csv")

if isfile(csv_above)
    above_preds = CSV.read(csv_above, DataFrame)
    println("\n[ABOVE] head:")
    println(first(above_preds, 5))
    println("[ABOVE] counts by predicted label:")
    println(combine(groupby(above_preds, :pred_label), nrow => :count))
end

if isfile(csv_below)
    below_preds = CSV.read(csv_below, DataFrame)
    println("\n[BELOW] head:")
    println(first(below_preds, 5))
    println("[BELOW] counts by predicted label:")
    println(combine(groupby(below_preds, :pred_label), nrow => :count))
end

