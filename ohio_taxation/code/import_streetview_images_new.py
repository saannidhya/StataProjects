#==========================================================================#
# File: import_streetview_images_new.py
# Author: Saani Rawat
# Date: 21 Dec 25
# Description: Import streetview images from Google Street View API
# Dependencies: import_streetview_images.py, oh_road_get_coords.r, roads_support.R
#==========================================================================#

import streetview as sv
from streetview import search_panoramas
import os
import sys
import geopandas as gpd
from shapely.geometry import Polygon, MultiPolygon, LineString, MultiLineString, Point, MultiPoint, GeometryCollection
import logging
import time
from dotenv import load_dotenv
import math, random

# Loading environment variables and setting up API key
load_dotenv()
sv.api_key = os.getenv('GOOGLE_API_KEY')

# set random seed
random.seed(42)

panos = search_panoramas(lat=39.142443862322494, lon=-84.51683048704483)
first = panos[0]

# date = 2019-05
loc = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/ohio/"


image = sv.get_streetview(pano_id = "t4BnnJnU-ueUz_zYsz9-pg", api_key= sv.api_key)

image.save(out_loc + "/ludlow.jpg", "jpeg")


def fetch_and_save_streetview(pano, out_dir, cosbidfp, namelsad, stname, api_key,
                              heading=None, pitch=-15, fov=60, source="outdoor"):
    pano_id, date, lat, lon = pano
    if date is None:
        return

    try:
        pic = sv.get_streetview(
            pano_id=pano_id,
            api_key=api_key,
            heading=heading,   # <-- key change
            pitch=pitch,       # <-- key change
            fov=fov           # <-- key change
            # source=source      # <-- key change (outdoor)
        )

        safe_name = namelsad.replace(" ", "_")
        safe_stname = str(stname).replace(" ", "_")
        filename = f"{cosbidfp}_{safe_stname}_{safe_name}_{date}_{lat}_{lon}_h{heading}_p{pitch}_f{fov}.jpg"

        filepath = os.path.join(out_dir, filename)
        pic.save(filepath, "jpeg")
        print(f"Saved: {filepath}")

    except Exception as e:
        print(f"Error fetching pano_id={pano_id} (date={date}): {e}")


fetch_and_save_streetview(
    pano=pano_tuple,
    out_dir=out_dir,
    cosbidfp=cosbidfp,
    namelsad=namelsad,
    stname=stname,
    api_key=api_key,
    heading=hdg,
    pitch=-15,
    fov=60,
    source="outdoor"
)


#==========================================================================#
# Test 
#==========================================================================#


# 2) Pick ONE test coordinate (use one from your examples)
test_lat, test_lon = 39.14221503281548, -84.5058709808294

# 3) Find panoramas near that point
panos = search_panoramas(lat=test_lat, lon=test_lon)

# 4) Filter to dated panos >= 2010 and pick the most recent
filtered = []
for p in panos:
    if p.date is None:
        continue
    year = int(p.date.split("-")[0])
    if year >= 2010:
        filtered.append(p)

if not filtered:
    print("No panoramas found after 2010 at this point.")
else:
    # "YYYY-MM" sorts nicely as a string, so max(date) works
    best = max(filtered, key=lambda p: p.date)

    pano_tuple = (best.pano_id, best.date, best.lat, best.lon)

out_loc = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/ohio/google streetview photos"


# Call YOUR function once
fetch_and_save_streetview(
    pano=pano_tuple,
    out_dir=out_loc,
    cosbidfp="TESTCOSBID",
    namelsad="TESTNAMELSAD",
    stname="TESTROAD",
    api_key=sv.api_key,
    heading=270,   # try 0, 90, 180, 270 too
    pitch=-15,
    fov=60
    # source="outdoor"
)


#==========================================================================#
# Importing TIGER lines for local roads
#==========================================================================#

gdf_roads = gpd.read_file(loc + "oh_roads_by_cousub.geojson")



# Getting all the coordinates of the polygons (road) for each row in the geodataframe
#
def get_road_coords(gdf):
    """
    Returns a list of tuples for each row in the GeoDataFrame:
      [
        (
            [(lat, lon), (lat, lon), ...],  # All coords for that row's geometry
            row.COSBIDFP00,
            row.NAME00,
            row.NAMELSAD00,
            row.CLASSFP00,
            row.FULLNAME,
            row.UR00
        ),
        ...
      ]

    This handles the following geometry types:
      - Point
      - MultiPoint
      - LineString
      - MultiLineString
      - GeometryCollection
    """

    def extract_coords(geom):
        """
        Recursively extract (lat, lon) coordinate pairs from a geometry.
        Supports Point, MultiPoint, LineString, MultiLineString, GeometryCollection.
        Returns a list of (lat, lon) tuples.
        """
        coords_list = []

        if geom is None:
            return coords_list

        # 1. Point
        if isinstance(geom, Point):
            # Single coordinate: (x=longitude, y=latitude)
            x, y = geom.x, geom.y
            coords_list.append((y, x))  # flip for (lat, lon)

        # 2. MultiPoint
        elif isinstance(geom, MultiPoint):
            for pt in geom.geoms:
                x, y = pt.x, pt.y
                coords_list.append((y, x))

        # 3. LineString
        elif isinstance(geom, LineString):
            for x, y in geom.coords:
                coords_list.append((y, x))

        # 4. MultiLineString
        elif isinstance(geom, MultiLineString):
            for line in geom.geoms:
                for x, y in line.coords:
                    coords_list.append((y, x))

        # 5. GeometryCollection
        elif isinstance(geom, GeometryCollection):
            # Recursively handle each geometry in the collection
            for sub_geom in geom.geoms:
                coords_list.extend(extract_coords(sub_geom))

        # 6. Polygon 
        elif isinstance(geom, Polygon):
            exterior_coords = list(geom.exterior.coords)
            coords_list.extend([(coord[1], coord[0]) for coord in exterior_coords])

        # 7. MultiPolygon
        elif isinstance(geom, MultiPolygon):
            for poly in geom.geoms:
                exterior_coords = list(poly.exterior.coords)
                coords_list.extend([(coord[1], coord[0]) for coord in exterior_coords])                

        # If other geometry types appear, handle or skip
        else:
            print(f"Skipping unhandled geometry type: {geom.geom_type}")

        return coords_list

    output_tuples = []
    for row in gdf.itertuples():
        # Extract coords for this row
        coords = extract_coords(row.geometry)

        # Build the tuple with coords + whichever attributes you need
        # Adjust to match your actual columns
        output_tuples.append(
            (
                coords,          # list of (lat, lon) pairs
                row.COSBIDFP00,
                row.NAME00,
                row.NAMELSAD00,
                row.CLASSFP00,
                row.FULLNAME,
                row.UR00
            )
        )

    return output_tuples

# Call the function to get road coordinates
lat_lon_tuples = get_road_coords(gdf_roads)
len(lat_lon_tuples)

# Subset coordinates: keep at most 3 random coordinates per road
lat_lon_tuples_subset = []
for coords, cosbidfp, name00, namelsad, classfp, fullname, ur00 in lat_lon_tuples:
    if len(coords) <= 3:
        sampled_coords = coords
    else:
        sampled_coords = random.sample(coords, 3)
    
    lat_lon_tuples_subset.append((sampled_coords, cosbidfp, name00, namelsad, classfp, fullname, ur00))


#-----------------------------------------------------------------------------------------------;
#       MAIN CODE to get MULTIPLE images from streetview, based on lat, lon of close elections
#-----------------------------------------------------------------------------------------------;

def bearing_deg(lat1, lon1, lat2, lon2):
    """
    Returns initial bearing in degrees from (lat1,lon1) -> (lat2,lon2). This is the compass direction one would start out on when traveling along a great-circle path from the first point to the second. Tries to mimic movement along roads as closely as possible.
    0 = North, 90 = East, 180 = South, 270 = West
    """
    phi1, phi2 = math.radians(lat1), math.radians(lat2)
    dlam = math.radians(lon2 - lon1)

    x = math.sin(dlam) * math.cos(phi2)
    y = math.cos(phi1) * math.sin(phi2) - math.sin(phi1) * math.cos(phi2) * math.cos(dlam)

    brng = math.degrees(math.atan2(x, y))
    return (brng + 360) % 360

def fetch_and_save_streetview(pano, out_dir, cosbidfp, namelsad, stname, api_key,
                              heading=None, pitch=-15, fov=60, source="outdoor"):
    pano_id, date, lat, lon = pano
    if date is None:
        return

    try:
        pic = sv.get_streetview(
            pano_id=pano_id,
            api_key=api_key,
            heading=heading,   # <-- key change
            pitch=pitch,       # <-- key change
            fov=fov           # <-- key change
            # source=source      # <-- key change (outdoor)
        )

        safe_name = namelsad.replace(" ", "_")
        safe_stname = str(stname).replace(" ", "_")
        filename = f"{cosbidfp}_{safe_stname}_{safe_name}_{date}_{lat}_{lon}_h{heading}_p{pitch}_f{fov}.jpg"

        filepath = os.path.join(out_dir, filename)
        pic.save(filepath, "jpeg")
        print(f"Saved: {filepath}")

    except Exception as e:
        print(f"Error fetching pano_id={pano_id} (date={date}): {e}")


def fetch_streetview_images_for_road(coords_list, cosbidfp, namelsad, stname, out_dir, api_key, min_year=2010):
    n = len(coords_list)
    if n < 2:
        return

    for i, (lat, lon) in enumerate(coords_list):
        # pick a neighbor to define road direction
        if i < n - 1:
            lat2, lon2 = coords_list[i + 1]
        else:
            lat2, lon2 = coords_list[i - 1]

        # compute bearing from (lat, lon) to (lat2, lon2). This is our heading, meaning the direction the camera should face to look "down at the road", as this is the direction of travel along the road.
        hdg = bearing_deg(lat, lon, lat2, lon2)

        try:
            panos = search_panoramas(lat=lat, lon=lon)

            filtered_panos = []
            for p in panos:
                if p.date is None:
                    continue
                year_str = p.date.split("-")[0]
                try:
                    year = int(year_str)
                except ValueError:
                    continue
                if year >= min_year:
                    filtered_panos.append((p.pano_id, p.date, p.lat, p.lon))

            if not filtered_panos:
                print(f"No panoramas for cosbidfp={cosbidfp}, stname={stname} at {lat},{lon} after {min_year}")
                continue

            for pano_tuple in filtered_panos:
                # Option 1: single “down-the-road” view
                fetch_and_save_streetview(
                    pano=pano_tuple,
                    out_dir=out_dir,
                    cosbidfp=cosbidfp,
                    namelsad=namelsad,
                    stname=stname,
                    api_key=api_key,
                    heading=hdg,
                    pitch=-15,
                    fov=60,
                    source="outdoor"
                )

                # Option 2 (optional): also grab the reverse direction
                # fetch_and_save_streetview(..., heading=(hdg + 180) % 360, pitch=-15, fov=60, source="outdoor")

        except Exception as e:
            print(f"Error searching panoramas for namelsad={namelsad}, stname={stname} at {lat},{lon}: {e}")

lat_lon_tuples_subset
# for coords_list, cosbid, _ , namelsad, classfp, stname, _ in lat_lon_tuples:
for coords_list, cosbid, _ , namelsad, classfp, stname, _ in lat_lon_tuples_subset:

    print(f"Processing cosbid={cosbid}, stname={stname}, namelsad={namelsad}")

    fetch_streetview_images_for_road(
        coords_list=coords_list,
        cosbidfp=cosbid,
        namelsad=namelsad,
        stname=stname,
        out_dir="C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/ohio/google streetview photos/",
        api_key=sv.api_key,
        min_year=2010
    )

