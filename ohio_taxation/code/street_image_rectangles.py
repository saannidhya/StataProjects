

import os
import cv2
import numpy as np
import pandas as pd


# Load the CSV file into a Pandas DataFrame
csv_file_path = 'C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/annotations/blemishes_data.csv'  # Replace with your CSV file path
df = pd.read_csv(csv_file_path)

# Define the directories for images and output
images_dir = 'C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images'  # Replace with your images directory path
output_dir = 'C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/annotations/image annotations'  # Replace with your desired output directory path

def draw_boxes_on_images(df, images_dir, output_dir) -> None:
    """
    Draws bounding boxes (xmin, ymin, xmax, ymax) onto each image specified in 'df'
    and saves the result in 'output_dir'.
    
    :param df: Pandas DataFrame with columns ['file', 'xmin', 'ymin', 'xmax', 'ymax'] at minimum
    :param images_dir: Folder where the original images are located
    :param output_dir: Folder where annotated images will be saved
    """
    # Create output directory if needed
    os.makedirs(output_dir, exist_ok=True)

    # Group the bounding box entries by their associated image file
    for file_name, group in df.groupby('file'):
        # Load the image
        # image_path = os.path.join(images_dir, file_name)
        image_path = os.path.join(images_dir, file_name + '.jpg')        
        image = cv2.imread(image_path)

        # If image wasn't found, skip
        if image is None:
            print(f"Warning: Could not open {image_path}. Skipping.")
            continue

        # Draw each bounding box for this image
        for _, row in group.iterrows():
            xmin, ymin, xmax, ymax = int(row.xmin), int(row.ymin), int(row.xmax), int(row.ymax)
            
            # Draw the bounding box in green, thickness=2
            cv2.rectangle(image, (xmin, ymin), (xmax, ymax), (0, 255, 0), 2)
            
            # Optionally, write the label above the box:
            label = row['name']  # or whatever field in your DF denotes label
            cv2.putText(image, label, (xmin, ymin - 5),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.6, (0, 255, 0), 2, cv2.LINE_AA)
        # print("Bounding boxes drawn for image:", file_name)
        # Save the annotated image
        out_path = os.path.join(output_dir, file_name + '_annotated.jpg')
        cv2.imwrite(out_path, image)


draw_boxes_on_images(df, images_dir, output_dir)

# Could not open files from 004281 to 004316. Need to inspect these files.

'''
>>> draw_boxes_on_images(df, images_dir, output_dir)
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004281.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004282.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004283.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004284.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004285.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004286.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004287.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004288.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004289.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004290.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004291.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004292.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004293.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004294.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004295.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004296.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004297.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004298.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004299.jpg. Skipping.
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004310.jpg. Skipping.      
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004311.jpg. Skipping.      
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004312.jpg. Skipping.      
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004313.jpg. Skipping.      
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004314.jpg. Skipping.      
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004315.jpg. Skipping.      
Warning: Could not open C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/Road Quality/United_States/train/images\United_States_004316.jpg. Skipping.  
'''