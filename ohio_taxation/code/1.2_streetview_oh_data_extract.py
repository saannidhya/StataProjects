"""
Name: streetview_oh_data_extract.py
Purpose: Extract Google Street View images for roads in Ohio data
Date: 2025-01-21
Log:
    - 2025-01-21: Created initial file with header
"""


import os
import streetview
from streetview import search_panoramas


API_KEY = "bogus_key"

panos = search_panoramas(lat=40.08225, lon=-82.91497)
first = panos[0]


len(panos)

type(panos)

set([dict(pano)["date"] for pano in panos])

print(first)

image = streetview.get_streetview(
    pano_id="i--qIWFFheuaPkuwQ2g_oQ",
    api_key=API_KEY
)


image_pan = streetview.get_panorama(pano_id="i--qIWFFheuaPkuwQ2g_oQ")

image_pan.save("image.jpg", "jpeg")

image.save("google_street_test_image.jpg", "jpeg")

os.getcwd()