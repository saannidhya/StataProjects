#=========================================================================================================================#
# Purpose : Geocode masterfile_2006q1_2024q4.dta
# Name    : Saani Rawat
# Created : 8/29/2025
# Log     : 
#        1. 8/29/2025: started the code. Ran on a sample.
#        2. 8/30/2025: Ran geocoding on entire data (~9MM obs). This will take time.
#        3.  9/6/2025: Geocoding on cleaned dataset masterfile_2006q1_2024q4.sas7bdat (removed special characters in SAS)
#=========================================================================================================================#

library(tidyverse)
library(tidygeocoder)


root = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/"

# df_s = readr::read_csv(paste0(root, "/AddressOH_LongPanel_1pct_sample.csv"))
# df = readr::read_csv(paste0(root, "/AddressOH_LongPanel_cleaned.csv")) 
df_master = haven::read_sas(paste0(root, "/data/employment/masterfile_2006q1_2024q4.sas7bdat")) # too slow? use sas7bdat?

# colnames(df_master)
#=================================================================#
# Cleaning address column
#=================================================================#



#=================================================================#
# 1% sample
#=================================================================#


# Sample 1% of the rows
# start_time <- Sys.time()

# sample_size <- ceiling(0.01 * nrow(df_master))
# df_sampled <- sample_n(df_master, size = sample_size)

# # nrow(df_sampled)

# df2 <- df_sampled %>%
#   mutate(full_address = paste(Address, City, State, Zip, sep = ", "))

# # Now use geocode()
# # df_geocoded <- df2 %>%
# #   geocode(addr = full_address, method = 'osm')
# # df_geocoded %>% filter(!is.na(lat)) %>% nrow()

# n <- nrow(df2)
# chunk_size <- 10000
# chunks <- split(df2, ceiling(seq_len(n)/chunk_size))

# # Function to geocode each chunk
# geocode_chunk <- function(chunk) {
#   chunk %>%
#     geocode(addr = full_address, method = 'census')
# }

# # df_geocoded_census <- df2 %>%
# #   geocode(addr = full_address, method = 'census',
# #           full_results = TRUE,
# #           api_options = list(census_return_type = 'geographies'))

# print("NOTE: running census geocoding now")
# df_geocoded_census <- map_df(chunks, geocode_chunk)
# print("NOTE: Finished running census geocoding")


# print("NOTE: Geocoded Dataset Info:")
# df_geocoded_census %>% nrow()
# df_geocoded_census %>% filter(!is.na(lat)) %>% nrow() / df_geocoded_census %>% nrow() # proportion not geocoded: 88%

# print("NOTE: Exporting to CSV file")
# write.csv(df_geocoded_census, paste0(root,"/data/employment/masterfile_2006q1_2024q4_1pct_sample.csv"), row.names = FALSE)

# end_time <- Sys.time()
# elapsed_time <- end_time - start_time
# print(paste("Run time:", elapsed_time))

# View(df_geocoded_census)


#=================================================================#
# Full dataset
#=================================================================#
start_time <- Sys.time()

df2 <- df_master %>%
  mutate(full_address = paste(Address, City, State, Zip, sep = ", "))

n <- nrow(df2)
chunk_size <- 10000
chunks <- split(df2, ceiling(seq_len(n)/chunk_size))

# Function to geocode each chunk
geocode_chunk <- function(chunk) {
  chunk %>%
    geocode(addr = full_address, method = 'census')
}
print("NOTE: running census geocoding now")
df_s2_geocoded_census <- map_df(chunks, geocode_chunk)
print("NOTE: Finished running census geocoding")

(df_s2_geocoded_census %>% filter(!is.na(lat)) %>% nrow() / df_s2_geocoded_census %>% nrow() ) %>% round(2)
# ~ 88% of the full dataset was geocoded 

print("NOTE: Exporting to CSV file")
write.csv(df_s2_geocoded_census, paste0(root,"/data/employment/masterfile_2006q1_2024q4_full.csv"), row.names = FALSE)

# df_s2_geocoded_census %>% select(-pl_zip, -full_address) %>% 
#   rename(lon = long) %>%
#   write.csv(paste0(root,"/data/employment/masterfile_2006q1_2024q4_full_cleaned.csv"), row.names = FALSE)

end_time <- Sys.time()

elapsed_time <- end_time - start_time
print(elapsed_time)


# View(df_s2_geocoded_census[1:1000,])



#============================================================================================#
# Need to add tendigit_fips
#============================================================================================#

library(sf)

# Import the shapefile
oh_shp <- st_read(paste0(root, "/data/employment/tl_2020_OH_countysubdivision/tl_2020_39_cousub.shp"))

# Read the CSV file
# df_full <- readr::read_csv(paste0(root, "/data/employment/masterfile_2006q1_2024q4_full.csv")) %>%
#   mutate(tendigit_fips = str_pad(tendigit_fips, 10, pad = "0"))


# Transform polygons to WGS84 to match point lon/lat, keep only needed column
oh_shp_wgs84 <- oh_shp |>
  sf::st_transform(4326) |>
  dplyr::mutate(tendigit_fips = stringr::str_pad(as.character(GEOID), 10, pad = "0")) |>
  dplyr::select(tendigit_fips, geometry)

# nrow(oh_shp_wgs84)
# nrow(oh_shp)

# Prepare indices for rows with valid coords inside Ohio bbox
bb <- sf::st_bbox(oh_shp_wgs84)
valid_idx <- which(
  !is.na(df_s2_geocoded_census$long) &
  !is.na(df_s2_geocoded_census$lat)  &
  df_s2_geocoded_census$long >= bb["xmin"] &
  df_s2_geocoded_census$long <= bb["xmax"] &
  df_s2_geocoded_census$lat  >= bb["ymin"] &
  df_s2_geocoded_census$lat  <= bb["ymax"]
)

# Allocate output column
df_s2_geocoded_census$tendigit_fips <- NA_character_

# Chunked spatial join to handle very large data
chunk_size <- 500000L
idx_chunks <- split(valid_idx, ceiling(seq_along(valid_idx) / chunk_size))

start_time <- Sys.time()
print("NOTE: running spatial join now")
for (i in seq_along(idx_chunks)) {
  idx <- idx_chunks[[i]]
  pts <- sf::st_as_sf(
    df_s2_geocoded_census[idx, ],
    coords = c("long", "lat"),
    crs = 4326,
    remove = FALSE
  )
  joined <- sf::st_join(pts, oh_shp_wgs84, join = sf::st_within, left = TRUE)
  df_s2_geocoded_census$tendigit_fips[idx] <- joined$tendigit_fips
  rm(pts, joined)
  gc()
}

end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(paste("Run time:", elapsed_time))

# df_s2_geocoded_census now has tendigit_fips (10-digit FIPS of the county subdivision)

View(df_s2_geocoded_census[1:1000,])

df_s2_geocoded_census %>%
  group_by(tendigit_fips) %>% 
  summarize(
    n = n())
