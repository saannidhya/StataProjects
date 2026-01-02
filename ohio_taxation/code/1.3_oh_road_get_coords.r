library(sf)
library(dplyr)

root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- file.path(root, "data")
tiger_loc <- file.path(data, "roads/ohio/oh_roads_tiger_lines/")

# polygons
oh_cousub <- st_read(file.path(tiger_loc, "tl_2010_39_cousub00/", "tl_2010_39_cousub00.shp")) %>%
  select(
    COSBIDFP00, NAME00, NAMELSAD00, LSAD00, CLASSFP00, UR00, geometry
  ) %>%  filter(LSAD00 == "44")  # townships

oh_place <- st_read(file.path(tiger_loc, "tl_2010_39_place00/", "tl_2010_39_place00.shp")) %>%
  # keep only incorporated places: City (25) + Village (47)
  filter(LSAD00 %in% c("25","47")) %>%
  select(PLCIDFP00, NAME00, NAMELSAD00, LSAD00, geometry)

# roads: IMPORTANT — use All Roads (ROADS), not prisecroads

# Get all county-level road shapefiles
road_files <- list.files(
    path = tiger_loc,
    pattern = "tl_2010_39\\d{3}_roads\\.shp$",
    recursive = TRUE,
    full.names = TRUE
)
length(road_files)

# Read and combine all county road files
oh_roads <- purrr::map_dfr(road_files, ~ st_read(.x, quiet = TRUE) %>% select(LINEARID, FULLNAME, RTTYP, MTFCC, geometry))
nrow(oh_roads)

# 1) keep local streets (tune this!)
roads_local <- oh_roads %>%
  filter(
    MTFCC == "S1400",
    !RTTYP %in% c("C", "I","U","S"),  # drop Interstate/US/State routes using route-type codes
    RTTYP == "M",
  )
nrow(roads_local)


# 2) municipal streets: split by place boundaries (will cut lines at borders)

roads_local_valid <- st_make_valid(roads_local)
oh_place_valid <- st_make_valid(oh_place)

roads_muni <- st_intersection(roads_local_valid, oh_place_valid)

# 3) township streets: remove incorporated areas, then intersect with townships
# Optimize: reuse validated geometries and pre-compute township areas
oh_cousub_valid <- st_make_valid(oh_cousub)
place_union <- st_union(oh_place_valid) # dissolve places into single geometry - for faster difference operation

township_only_area <- st_difference(oh_cousub_valid, place_union)

roads_township <- st_intersection(roads_local_valid, township_only_area)

st_write(roads_muni, paste0(tiger_loc, "oh_muni_streets.geojson"), delete_dsn = TRUE)
st_write(roads_township, paste0(tiger_loc, "oh_township_streets.geojson"), delete_dsn = TRUE)