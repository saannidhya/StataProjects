"""
Name: road_quality.py
Purpose: Analyze and process road quality data for Ohio taxation project
Date: 2024-10-13
Log:
    - 2024-10-13: Created initial file with header
"""

# Import necessary libraries to communicate with OpenAI API
# import openai
import os
import base64
import json
import requests

# Define the API key for OpenAI
api_key = "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"

# Function to encode the image
def encode_image(image_path):
  with open(image_path, "rb") as image_file:
    return base64.b64encode(image_file.read()).decode('utf-8')
  
# image_path
os.getcwd()

# Path to your image
image_path = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/roadRunner/centered_roadrunner_pngs/20201018_1402_16.png"

base64_image = encode_image(image_path)

headers = {
    "Content-Type": "application/json",
    "Authorization": f"Bearer {api_key}"
}

payload = {
  "model": "gpt-4o-mini",
  "messages": [
    {
      "role": "user",
      "content": [
        {
          "type": "text",
          "text": "Give me a rating for this road. The rating can be 0 (poor), 1 (fair), 2 (execllent). Just give me a number, nothing else."
        },
        {
          "type": "image_url",
          "image_url": {
            "url": f"data:image/jpeg;base64,{base64_image}"
          }
        }
      ]
    }
  ],
  "max_tokens": 300
}

response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)

response.json()['choices'][0]['message']['content']

