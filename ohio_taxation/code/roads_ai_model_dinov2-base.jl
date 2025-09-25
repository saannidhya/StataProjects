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


# Import all PNG images from the directory
image_paths = glob("*.png", joinpath(input_dir, "centered_roadrunner_pngs"))
# images = [load(path) for path in image_paths]

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

# vscodedisplay(roadRunner_labels)

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
# Fine-tuning DINOv2/ConvNeXt V2 for Road Blemish Classification
#=======================================================================================#

# Import Python libraries
# transformers = PyCall.pyimport("transformers")
# torch = PyCall.pyimport("torch")
# np = PyCall.pyimport("numpy")

# # Load DINOv2 model
# function load_dinov2(model_name="facebook/dinov2-base")
#     processor = transformers.AutoImageProcessor.from_pretrained(model_name)
#     model = transformers.AutoModel.from_pretrained(model_name)
#     return processor, model
# end

# # Load ConvNeXt V2 model  
# function load_convnext_v2(model_name="facebook/convnext-base-224-22k")
#     processor = transformers.AutoImageProcessor.from_pretrained(model_name)
#     model = transformers.AutoModelForImageClassification.from_pretrained(model_name)
#     return processor, model
# end

# ### Data Loading and Preprocessing ### 

# # Custom dataset structure
# struct VisionDataset
#     images::Vector{String}  # Image file paths
#     labels::Vector{Int}     # Class labels
#     transform::Function     # Preprocessing function
# end

# function load_image_dataset(data_dir::String, label_file::String)
#     # Load the roadRunner labels with image paths
#     df = CSV.read(joinpath(data_dir, label_file), DataFrame)
#     images = df.image_path
#     labels = df.int_label
    
#     return images, labels
# end

# # Load the dataset
# # images, labels = load_image_dataset(input_dir, "roadRunner_labels_w_img_path.csv")

# # Preprocessing function for DINOv2/ConvNeXt
# function preprocess_image(image_path::String, processor)
#     # Load image using Julia
#     img = load(image_path)
    
#     # Convert to RGB array format expected by transformers
#     img_array = permutedims(channelview(RGB.(img)), (3, 2, 1))
#     img_array = Float32.(img_array)
    
#     # Use HuggingFace processor
#     inputs = processor(images=img_array, return_tensors="pt")
#     return inputs["pixel_values"]
# end

# # Create data loader
# function create_dataloader(images, labels, processor; batch_size=16, shuffle=true)
#     dataset = [(preprocess_image(img, processor), label) 
#                for (img, label) in zip(images, labels)]
    
#     if shuffle
#         dataset = dataset[randperm(length(dataset))]
#     end
    
#     return DataLoader(dataset, batchsize=batch_size)
# end

# ### Fine-tuning Setup ###

# # Fine-tuning wrapper for DINOv2
# mutable struct DINOv2Classifier
#     backbone
#     processor  
#     classifier
#     num_classes::Int
# end

# function DINOv2Classifier(num_classes::Int; model_name="facebook/dinov2-base")
#     processor, backbone = load_dinov2(model_name)
    
#     # Freeze backbone parameters (optional)
#     try
#         for param in backbone.parameters()
#             param.requires_grad = false
#         end
#     catch e
#         println("Warning: Could not freeze backbone parameters: $e")
#     end
    
#     # Get feature dimension (768 for dinov2-base, 384 for small, 1024 for large)
#     feature_dim = if occursin("small", model_name)
#         384
#     elseif occursin("large", model_name) 
#         1024
#     else
#         768  # base model
#     end
    
#     # Create classifier head in Julia/Flux
#     classifier = Chain(
#         Dense(feature_dim, 256, relu),
#         Dropout(0.3),
#         Dense(256, num_classes)
#     ) |> gpu  # Move to GPU if available
    
#     return DINOv2Classifier(backbone, processor, classifier, num_classes)
# end

# # Alternative simpler approach using direct PyCall
# function extract_features_dinov2(backbone, processor, image_batch)
#     # Process images through backbone
#     py"""
#     import torch
#     import numpy as np
    
#     with torch.no_grad():
#         outputs = $(backbone)($image_batch)
#         # Get CLS token embeddings
#         features = outputs.last_hidden_state[:, 0, :].detach().cpu().numpy()
#     """
    
#     return Array{Float32}(py"features")
# end

# # Simplified model structure
# struct SimpleDINOv2Classifier
#     backbone::Any
#     processor::Any
#     classifier::Chain
# end

# function SimpleDINOv2Classifier(num_classes::Int; model_name="facebook/dinov2-base")
#     processor, backbone = load_dinov2(model_name)
    
#     # Determine feature dimension
#     feature_dim = 768  # Adjust based on model variant
    
#     classifier = Chain(
#         Dense(feature_dim, 512, relu),
#         BatchNorm(512),
#         Dropout(0.5),
#         Dense(512, 256, relu), 
#         BatchNorm(256),
#         Dropout(0.3),
#         Dense(256, num_classes)
#     )
    
#     if CUDA.functional()
#         classifier = classifier |> gpu
#     end
    
#     return SimpleDINOv2Classifier(backbone, processor, classifier)
# end

# function forward_pass(m::SimpleDINOv2Classifier, image_batch)
#     # Extract features
#     features = extract_features_dinov2(m.backbone, m.processor, image_batch)
    
#     # Move to GPU if available
#     if CUDA.functional()
#         features = features |> gpu
#     end
    
#     # Apply classifier
#     return m.classifier(features)
# end


# ### Training Loop ###

# function train_model!(model, train_loader, val_loader; 
#                      epochs=10, lr=1e-4, device=gpu)
    
#     # Move model to device
#     model = model |> device
    
#     # Optimizer
#     optimizer = ADAM(lr)
    
#     # Loss function
#     loss_fn = Flux.crossentropy
    
#     # Training loop
#     for epoch in 1:epochs
#         println("Epoch $epoch/$epochs")
        
#         # Training phase
#         Flux.trainmode!(model)
#         train_loss = 0.0
#         train_acc = 0.0
        
#         for (i, (batch_x, batch_y)) in enumerate(train_loader)
#             batch_x, batch_y = batch_x |> device, batch_y |> device
            
#             # Forward pass and loss
#             loss, grads = Flux.withgradient(model) do m
#                 ŷ = m(batch_x)
#                 loss_fn(ŷ, batch_y)
#             end
            
#             # Backward pass
#             Flux.update!(optimizer, model, grads[1])
            
#             # Metrics
#             train_loss += loss
#             train_acc += mean(Flux.onecold(model(batch_x)) .== Flux.onecold(batch_y))
            
#             if i % 10 == 0
#                 println("  Batch $i: Loss = $(loss)")
#             end
#         end
        
#         # Validation phase
#         Flux.testmode!(model)
#         val_loss, val_acc = validate_model(model, val_loader, loss_fn, device)
        
#         println("  Train Loss: $(train_loss/length(train_loader))")
#         println("  Train Acc:  $(train_acc/length(train_loader))")
#         println("  Val Loss:   $val_loss")
#         println("  Val Acc:    $val_acc")
#         println()
#     end
    
#     return model
# end

# function validate_model(model, val_loader, loss_fn, device)
#     total_loss = 0.0
#     total_acc = 0.0
    
#     for (batch_x, batch_y) in val_loader
#         batch_x, batch_y = batch_x |> device, batch_y |> device
        
#         ŷ = model(batch_x)
#         loss = loss_fn(ŷ, batch_y)
#         acc = mean(Flux.onecold(ŷ) .== Flux.onecold(batch_y))
        
#         total_loss += loss
#         total_acc += acc
#     end
    
#     return total_loss/length(val_loader), total_acc/length(val_loader)
# end

# ### Usage Example ###

# # Main training script
# function main()
#     # Configuration
#     num_classes = 10  # Adjust for your dataset
#     batch_size = 16
#     epochs = 20
#     learning_rate = 1e-4
    
#     # Load data
#     images, labels = load_image_dataset(input_dir, "roadRunner_train_data.csv")
    
#     # Create model
#     model = DINOv2Classifier(num_classes)
    
#     # Split data
#     n_train = Int(0.8 * length(images))
#     train_images, train_labels = images[1:n_train], labels[1:n_train]
#     val_images, val_labels = images[n_train+1:end], labels[n_train+1:end]
    
#     # Create data loaders
#     train_loader = create_dataloader(train_images, train_labels, model.processor; 
#                                    batch_size=batch_size, shuffle=true)
#     val_loader = create_dataloader(val_images, val_labels, model.processor; 
#                                  batch_size=batch_size, shuffle=false)
    
#     # Train model
#     trained_model = train_model!(model, train_loader, val_loader; 
#                                 epochs=epochs, lr=learning_rate)
    
#     # Save model
#     BSON.@save joinpath(input_dir, "finetuned_dinov2.bson") trained_model
    
#     return trained_model
# end

# # Run training
# trained_model = main()


# ========= HF Fine-tuning (pure PyTorch via PyCall) =========
using PyCall

# const MODEL_NAME = "facebook/convnextv2-base-1k-224"   # good starter
# Alternative (backbone is heavier): "facebook/dinov2-base" works too
const MODEL_NAME = "facebook/dinov2-base"

# Build lists for Python
train_paths = collect(String.(train_data.image_path))
train_labels = collect(Int.(train_data.int_label))
val_paths   = collect(String.(val_data.image_path))
val_labels  = collect(Int.(val_data.int_label))

unique_labels = sort!(unique(roadRunner_labels.int_label))
num_classes   = length(unique_labels)

# Output directory for saved model
# outdir = joinpath(input_dir, "hf_finetuned_convnextv2")  # change if you switch model
outdir = joinpath(input_dir, "hf_finetuned_dinov2")  # change if you switch model

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
