"""
Purpose: Extract Google Street View images for roads in Ohio only for areas with close elections
Date: 2025-02-18
Log:
    - 2025-02-18: Created initial file with header. 
    - 2025-02-19: Added code to extract images for roads in Ohio with close elections. Ran the main section of the code.
"""

import streetview as sv
from streetview import search_panoramas
import os
import sys
import geopandas as gpd
from shapely.geometry import Polygon, MultiPolygon, LineString, MultiLineString, Point, MultiPoint, GeometryCollection
import logging
import time

# Enter your Google Street View API key here.
sv.api_key = "" 

loc = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/ohio/"
# print(os.environ.keys())
out_loc = loc + "google maps photos/"

LOG_PATH = out_loc + "/run.log"

#------------------------------------------------------------------------------------;
#       Sample code to get ONE image from streetview
#------------------------------------------------------------------------------------;

panos = search_panoramas(lat=39.142443862322494, lon=-84.51683048704483)
first = panos[0]

# date = 2019-05

image = sv.get_streetview(pano_id = "t4BnnJnU-ueUz_zYsz9-pg", api_key= sv.api_key)

image.save(out_loc + "/ludlow.jpg", "jpeg")


# pano_meta = sv.get_panorama_metadata(pano_id = "t4BnnJnU-ueUz_zYsz9-pg", api_key= sv.api_key)



#------------------------------------------------------------------------------------;
#       Sample code to get MULTIPLE images from streetview, IF you have lat, lon
#------------------------------------------------------------------------------------;

# trying a tuple of lat, lon
locations = (
    (39.135150075547145, -84.51765124296685), 
    (39.14221503281548, -84.5058709808294),  
    (39.14635881208335, -84.49960534125992), 
    (39.356040625140146, -84.36560218976558)
)

count = 0
for loc in (locations[0],):    
    count += 1
    panos = search_panoramas(lat=loc[0], lon=loc[1])
    filtered_panos = [(p.pano_id, p.date, p.lat, p.lon) for p in panos if p.date is not None]
    pano_id = filtered_panos[-1][0]
    pic = sv.get_streetview(pano_id = pano_id, api_key= sv.api_key)
    pic.save(out_loc + "/loc" + str(count) + ".jpg", "jpeg")

# panos = search_panoramas(lat=locations[0][0], lon=locations[0][1])
# filtered_panos = [(p.pano_id, p.date, p.lat, p.lon) for p in panos if p.date is not None]
# # take the last one
# pano_id = filtered_panos[-1][0]
# pic = sv.get_streetview(pano_id = pano_id, api_key= sv.api_key)
# pic.save(out_loc + "/check.jpg", "jpeg")

#-----------------------------------------------------------------------------------------------;
#       MAIN CODE to get MULTIPLE images from streetview, based on lat, lon of close elections
#-----------------------------------------------------------------------------------------------;

# gdf_roads = gpd.read_file("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/TIGERS/tl_2010_39_cousub00/tl_2010_39_cousub00.shp")
gdf_roads = gpd.read_file(loc + "oh_roads_by_cousub.geojson")

# get list of close elections
file = open(out_loc + "tendigit_fips_close_elections_gs_bw.txt", "r")
tendigit_fips_list = [line.strip() for line in file]
file.close()  

# only keep the rows with close elections
gdf_roads = gdf_roads[gdf_roads.COSBIDFP00.isin(tendigit_fips_list)].reset_index(drop=True)

gdf_roads.geometry
# Check the types of geometries in the geometry column
geometry_types = gdf_roads.geometry.apply(lambda geom: geom.geom_type).unique()
print("Unique geometry types in the GeoDataFrame:", geometry_types)


"""
Getting all the coordinates of the polygons (road) for each row in the geodataframe
"""
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

# Next step: Extract images for each road. Each lat, lon tuple is a list of coordinates for that road


def fetch_and_save_streetview(pano, out_dir, cosbidfp, namelsad, stname, api_key):
    """
    Function to get road images for each road pano_id.
    Given a single panorama record and other metadata,
    fetches the street view image and saves it to disk.
    
    pano: tuple like (pano_id, date, lat, lon)
    out_dir: directory path to save images
    cosbidfp, namelsad: relevant road attributes
    api_key: your Street View API key
    """
    pano_id, date, lat, lon = pano
    
    # If date is None, skip
    if date is None:
        return
    
    try:
        # Example uses the 'streetview' package
        pic = sv.get_streetview(pano_id=pano_id, api_key=api_key)

        # Constructing a file name. Incorporates road info, date, lat, lon
        # e.g. "3906966502_Richfield_2007-09_41.2551_-83.88226.jpg"
        safe_name = namelsad.replace(" ", "_")  # or re.sub(...) to remove special chars
        filename = f"{cosbidfp}_{stname}_{safe_name}_{date}_{lat}_{lon}.jpg"

        # Save in out_dir
        filepath = os.path.join(out_dir, filename)
        pic.save(filepath, "jpeg")
        print(f"Saved: {filepath}")
    
    except Exception as e:
        print(f"Error fetching pano_id={pano_id} (date={date}): {e}")



def fetch_streetview_images_for_road(coords_list, cosbidfp, namelsad, stname, out_dir, api_key, min_year=2010):
    """
    For a single road:
      - Loop through each boundary coordinate
      - Search all panoramas
      - For each panorama with a valid date >= min_year, fetch & save the Street View image
    """
    for (lat, lon) in coords_list:
        try:
            # search_panoramas is your function from the 'streetview' package
            panos = search_panoramas(lat=lat, lon=lon)
            
            # Filter to panoramas that have a date and year >= min_year.
            # The date format is typically "YYYY-MM", e.g. "2011-06" or "2016-10".
            filtered_panos = []
            for p in panos:
                if p.date is not None:
                    # Safely parse the year from "YYYY-MM"
                    # (assuming there's always a dash, e.g. "2011-06")
                    year_str = p.date.split("-")[0]
                    
                    try:
                        year = int(year_str)
                    except ValueError:
                        # If something went wrong parsing, skip this panorama
                        continue
                    
                    if year >= min_year:
                        filtered_panos.append((p.pano_id, p.date, p.lat, p.lon))
            
            if not filtered_panos:
                # No images after min_year at this coordinate
                print(f"No panoramas found for cosbidfp: {cosbidfp}, stname: {stname} at lat={lat}, lon={lon} after {min_year}")
                continue
            
            # Now, loop over all the panoramas that meet the date criteria
            for pano_tuple in filtered_panos:
                fetch_and_save_streetview(
                    pano=pano_tuple,
                    out_dir=out_dir,
                    cosbidfp=cosbidfp,
                    namelsad=namelsad,
                    stname=stname,
                    api_key=api_key
                )
            
        except Exception as e:
            print(f"Error searching panoramas for namelsad={namelsad}, stname={stname} at lat={lat}, lon={lon}: {e}")



# SPIN IT UP! MOMENT OF TRUTH.
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.FileHandler(LOG_PATH), 
        logging.StreamHandler(sys.stdout)
    ]
)

start_time = time.time()

for coords_list, cosbid, _ , namelsad, classfp, stname, _ in lat_lon_tuples:
    logging.info(f"Processing cosbid={cosbid}, stname={stname}, namelsad={namelsad}")
    fetch_streetview_images_for_road(
        coords_list=coords_list,
        cosbidfp=cosbid,
        namelsad=namelsad,
        stname=stname,
        out_dir=out_loc,
        api_key=sv.api_key
    )

elapsed_time = time.time() - start_time
logging.info(f"Run completed in {elapsed_time:.2f} seconds")

