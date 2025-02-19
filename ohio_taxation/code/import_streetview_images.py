"""
Purpose: Extract Google Street View images for roads in Ohio only for areas with close elections
Date: 2025-02-18
Log:
    - 2025-02-18: Created initial file with header. 
"""

import streetview as sv
from streetview import search_panoramas
import os
import sys
import geopandas as gpd

# sv.api_key = os.environ['GOOGLE_MAPS_API_KEY']
sv.api_key = "AIzaSyCmMbHMc2Dnpi_BGICwQs-r5HsY31x5AtQ"

# print(os.environ.keys())
out_loc = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/ohio/google maps photos"

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

gdf_roads = gpd.read_file("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/TIGERS/tl_2010_39_cousub00/tl_2010_39_cousub00.shp")

# Convert latitude and longitude columns to float and store in a list of tuples
# lat_lon_tuples = [
#     (float(row.INTPTLAT00), float(row.INTPTLON00), row.COSBIDFP00, row.NAMELSAD00, row.CLASSFP00) 
#     for row in gdf_roads.itertuples()
# ]

# get list of close elections
file = open("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/tendigit_fips_close_elections_gs_bw.txt", "r")
tendigit_fips_list = [line.strip() for line in file]
file.close()  

# only keep the rows with close elections
gdf_roads = gdf_roads[gdf_roads.COSBIDFP00.isin(tendigit_fips_list)].reset_index(drop=True)

"""
Getting all the coordinates of the polygons (road) for each row in the geodataframe
"""
lat_lon_tuples = []
for row in gdf_roads.itertuples():
    geom = row.geometry
    coords_list = []

    if isinstance(geom, Polygon):
        exterior_coords = list(geom.exterior.coords)
        coords_list.extend([(coord[1], coord[0]) for coord in exterior_coords])

    elif isinstance(geom, MultiPolygon):
        for poly in geom.geoms:
            exterior_coords = list(poly.exterior.coords)
            coords_list.extend([(coord[1], coord[0]) for coord in exterior_coords])

    else:
        # Something other than Polygon or MultiPolygon (LineString, Point, etc.)
        print(f"Skipping non-polygon geometry type: {geom.geom_type}")
        continue

    lat_lon_tuples.append(
        (
            coords_list,
            row.COSBIDFP00,
            row.NAMELSAD00,
            row.CLASSFP00
        )
    )


# import shapely
# from shapely.geometry import Polygon, MultiPolygon



# for loc in lat_lon_tuples:    
#     panos = search_panoramas(lat=loc[0], lon=loc[1])
#     filtered_panos = [(p.pano_id, p.date, p.lat, p.lon) for p in panos if p.date is not None]
#     if not filtered_panos:
#         print("No images found for location: ", loc[3])
#         continue
#     pano_id = filtered_panos[-1][0]
#     pic = sv.get_streetview(pano_id = pano_id, api_key= sv.api_key)
#     pic.save(out_loc + "/" + loc[3] + "_" + filtered_panos[-1][1] + ".jpg", "jpeg")


def fetch_and_save_streetview(pano, out_dir, cosbidfp, namelsad, api_key):
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
        filename = f"{cosbidfp}_{safe_name}_{date}_{lat}_{lon}.jpg"

        # Save in out_dir
        filepath = os.path.join(out_dir, filename)
        pic.save(filepath, "jpeg")
        print(f"Saved: {filepath}")
    
    except Exception as e:
        print(f"Error fetching pano_id={pano_id} (date={date}): {e}")


def fetch_streetview_images_for_road(coords_list, cosbidfp, namelsad, classfp, out_dir, api_key):
    """
    For a single road:
      - Loop through each boundary coordinate
      - Search all panoramas
      - For each panorama with a valid date, fetch & save the Street View image
    """
    for (lat, lon) in coords_list:
        try:
            # search_panoramas is your function from the 'streetview' package
            panos = search_panoramas(lat=lat, lon=lon)
            
            # If you want to include *all* dated panoramas, do NOT just pick the last one
            # Instead, iterate over all that have a non-None date
            filtered_panos = [(p.pano_id, p.date, p.lat, p.lon) 
                              for p in panos if p.date is not None]
            
            if not filtered_panos:
                # No images at this coordinate
                continue
            
            # If you want multiple images across different years, loop them all:
            for p in filtered_panos:
                fetch_and_save_streetview(
                    pano=p,
                    out_dir=out_dir,
                    cosbidfp=cosbidfp,
                    namelsad=namelsad,
                    api_key=api_key
                )
            
        except Exception as e:
            print(f"Error searching panoramas for road={namelsad} at lat={lat}, lon={lon}: {e}")
            # continue to the next coordinate


# SPIN IT UP! MOMENT OF TRUTH.
for coords_list, cosbid, namelsad, classfp in lat_lon_tuples:
    # If the polygon has no boundary coords, skip
    if not coords_list:
        continue
    
    # Try fetching Street View images for all boundary coordinates
    fetch_streetview_images_for_road(
        coords_list=coords_list,
        cosbidfp=cosbid,
        namelsad=namelsad,
        classfp=classfp,
        out_dir=out_loc,
        api_key=sv.api_key
    )