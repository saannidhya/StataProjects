#=========================================================================================================================#
# Purpose : Geocode + FIPS CoreLogic  University_of_Cincinnati_OwnerTransfer_v3_dpc_01549150_20240903_114649_data.txt
# Name    : Saani Rawat
# Created : 01/08/2026
# Log     : 
#        1. 01/08/2026: started the code. Ran on a sample.
#        2. 02/01/2026: Added year suffix to make it flexible for different year ranges.
#
# Downstream files:
#       1. 2.9_create_housesales_data.R (creates final housesales dataset using merged OT + PC data)
#=========================================================================================================================#


# Load necessary libraries
library(tidyverse)
library(tidycensus)
library(tidygeocoder)
library(tigris)
library(sf)
library(haven)
library(data.table)
library(dotenv)

# import loc
CoreLogic_loc <- "C:/CoreLogic"
# CoreLogic_loc <- "D:/2024_housing_data"

# out loc
ot_out_loc <- paste0(CoreLogic_loc,"/housing/OwnerTransfer/")
pc_out_loc <- paste0(CoreLogic_loc,"/housing/PropertyCharacteristics/")

# locations
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data_cl <- paste0(root, "/data/housing/CoreLogic")

# Year range configuration for geocoding
# --------------------------------------------------
# NOTE: Adjust start_year and end_year as needed
#--------------------------------------------------- 
start_year <- 2016
end_year <- 2020
years_to_process <- as.character(start_year:end_year)
# Create year suffix for output filename (last 2 digits of start and end year)
year_suffix <- paste0(str_sub(as.character(start_year), -2), str_sub(as.character(end_year), -2))


#=========================================================================================================================#
# Owner Transfer Data - Ohio only
#=========================================================================================================================#

# Import Ohio Owner Transfer file
df_ot_oh <- read_csv(file.path(CoreLogic_loc, "housing/OwnerTransfer/by_state/corelogic_ot_OH.csv"), col_types = cols(.default = "c"))

# Create year column from SALE DERIVED DATE, using SALE DERIVED RECORDING DATE as fallback
# Only keeping specified years for geocoding
df_ot_oh2 <- df_ot_oh %>%
  mutate(
    sale_dt = na_if(str_squish(`SALE DERIVED DATE`), ""),
    rec_dt  = na_if(str_squish(`SALE DERIVED RECORDING DATE`), ""),
    year    = str_sub(coalesce(sale_dt, rec_dt), 1, 4)
  ) %>% filter(year %in% years_to_process)


df_ot_oh3 <- df_ot_oh2 %>%
  mutate(

    street = str_trim(paste( `DEED SITUS HOUSE NUMBER - STATIC`, `DEED SITUS DIRECTION - STATIC`, `DEED SITUS STREET NAME - STATIC`, `DEED SITUS MODE - STATIC`, sep = " ")),
    street = str_replace_all(street, "\\bNA\\b", ""),     
    street = str_squish(street),
    city   = str_squish(`DEED SITUS CITY - STATIC`),
    state  = str_squish(`DEED SITUS STATE - STATIC`),
    zip5   = str_sub(`DEED SITUS ZIP CODE - STATIC`, 1, 5)
  ) %>%
  # drop obviously unusable SITUS rows
  filter(
    !is.na(street),
    street != "",
    !str_detect(street, regex("^PO BOX", ignore_case = TRUE)),
    !str_detect(street, regex("INVALID NUMBER", ignore_case = TRUE))
  ) %>% relocate(street, .after = `DEED SITUS STREET ADDRESS - STATIC`) %>% 
  relocate(city, .after = street) %>%
  relocate(state, .after = city) %>%
  relocate(zip5, .after = state) %>% select(-c("sale_dt", "rec_dt", "DEED SITUS CITY - STATIC", "DEED SITUS STATE - STATIC"))

# write.csv(df_ot_oh3, paste0(ot_out_loc, "corelogic_ot_oh_", year_suffix, "_cleaned.csv"), row.names = FALSE)
# df_ot_oh3 <- read_csv(file.path(ot_out_loc, paste0("corelogic_ot_oh_", year_suffix, "_cleaned.csv")), col_types = cols(.default = "c"))

geocode_census_safe <- function(df, max_tries = 5, timeout_min = 60, batch_limit = 2000) {

  for (attempt in seq_len(max_tries)) {

    res <- tryCatch(
      df %>%
        tidygeocoder::geocode(
          street = street,
          city = city,
          state = state,
          postalcode = zip5,
          method = "census",
          mode = "batch",
          full_results = TRUE,
          api_options = list(census_return_type = "geographies"),
          timeout = timeout_min,     # minutes (default is 20) :contentReference[oaicite:4]{index=4}
          batch_limit = batch_limit  # split requests into smaller batches :contentReference[oaicite:5]{index=5}
        ),
      error = function(e) e
    )

    # If it hard-failed (timeout, etc.), backoff + retry
    if (inherits(res, "error")) {
      Sys.sleep(10 * attempt)
      next
    }

    # Validate: catch HTML/502-ish “responses” masquerading as results
    # bad_text <- any(grepl("Bad Gateway|Census Geocoder while working as a gateway|<html|<p>", res$input_address %||% "", ignore.case = TRUE))
    input_addr <- if ("input_address" %in% names(res)) res$input_address else ""
    bad_text <- any(grepl("Bad Gateway|Census Geocoder while working as a gateway|<html|<p>", input_addr, ignore.case = TRUE))

    too_many_missing <- !("match_indicator" %in% names(res)) || mean(is.na(res$match_indicator)) > 0.3

    if (!bad_text && !too_many_missing) return(res)

    # If response looks corrupted, wait and retry
    Sys.sleep(15 * attempt)
  }

  stop("Census geocoder failed after retries; try again later or reduce batch_limit further.")
}

ot_full_chunk_counter <- 0L
process_ot_chunk_full_ <- function(df_clean, pos) {

  if (nrow(df_clean) == 0L) return(invisible(NULL))

  message("FULL geocode chunk @ pos ", pos, " with ", nrow(df_clean), " rows")

  geo_chunk <- geocode_census_safe(df_clean, timeout_min = 60, batch_limit = 2000)

  ot_full_chunk_counter <<- ot_full_chunk_counter + 1L

  readr::write_csv(
    geo_chunk,
    ot_full_outfile,
    append = ot_full_chunk_counter > 1L
  )

  rm(geo_chunk, df_clean); gc()
  Sys.sleep(3)
  invisible(NULL)
}

ot_full_outfile <- paste0(ot_out_loc, "corelogic_ownertransfer_geocoded_oh_", year_suffix, ".csv")

# Run the full chunked geocoding
start_time_full <- Sys.time()
message("NOTE: Starting OHIO OT geocoding (this may take some time)...")

# Process df_ot_oh in chunks
# chunk_size <- 10000
chunk_size <- 2000
total_rows <- nrow(df_ot_oh3)
num_chunks <- ceiling(total_rows / chunk_size)

message("NOTE: Processing ", total_rows, " rows in ", num_chunks, " chunks...")

for (i in seq_len(num_chunks)) {
  start_idx <- (i - 1) * chunk_size + 1
  end_idx <- min(i * chunk_size, total_rows)
  
  message("Processing chunk ", i, "/", num_chunks, " (rows ", start_idx, "-", end_idx, ")")
  
  # Extract chunk
  chunk_df <- df_ot_oh3[start_idx:end_idx, ]
  
  # Process this chunk using existing function
  process_ot_chunk_full_(chunk_df, pos = start_idx)
}

end_time_full <- Sys.time()
elapsed_min <- as.numeric(difftime(end_time_full, start_time_full, units = "mins"))
message("Run time (mins): ", round(elapsed_min, 1))
message("NOTE: Finished FULL OT geocoding.")
message("Output written to: ", ot_full_outfile)


# Import geocoded Ohio Owner Transfer file
df_ot_oh4 <- read_csv(file.path(ot_out_loc, paste0("corelogic_ownertransfer_geocoded_oh_", year_suffix, ".csv")), col_types = cols(.default = "c"))

# View(df_ot_oh4[1:100, ])

unique(df_ot_oh4$match_indicator)
df_ot_oh4 %>%
  filter(!is.na(lat)) %>%
  nrow() / nrow(df_ot_oh4)
# 93.1% geocoded
df_ot_oh4 %>% group_by(match_indicator) %>%
  summarise(count = n() , prop = n() / nrow(.), .groups = "drop") %>%
  arrange(desc(count)) %>%
  print(n = Inf)



#=========================================================================================================================#
# Add County Subdivision FIPS to Geocoded Owner Transfer Data
#=========================================================================================================================#

options(tigris_use_cache = TRUE)

# Get county subdivision polygons
oh_cousub <- tigris::county_subdivisions(
  state = "OH",
  year = 2023,
  class = "sf",
  cb = TRUE
) %>%
  st_transform(4326) %>%  
  select(
    cousub_geoid = GEOID,
    cousub_name  = NAME,
    cousub_namelsad = NAMELSAD,
    cousub_lsad = LSAD
  )
# unique(oh_cousub$cousub_lsad) ## 44 = township, 25 = city, 47 = village

# Convert your geocoded points to sf
pts <- df_ot_oh4 %>%
  dplyr::filter(!is.na(lat), !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Spatial join to get county subdivision FIPS
df_ot_oh_geocoded_with_cousub <- st_join(
  pts,
  oh_cousub,
  join = st_intersects  
  # join = st_within  # default is st_intersects; within is stricter
)
# Convert sf object back to regular dataframe and write to CSV
df_ot_oh_geocoded_with_cousub_out <- df_ot_oh_geocoded_with_cousub %>% st_drop_geometry()
# nrow(df_ot_oh_geocoded_with_cousub_out)

# Write to CSV
# readr::write_csv(df_ot_oh_geocoded_with_cousub_out, file.path(ot_out_loc, paste0("corelogic_ownertransfer_geocoded_with_cousub_oh_", year_suffix, ".csv")))


# View(df_ot_oh_geocoded_with_cousub_out[1:100, ])

message("County subdivision FIPS added.")

#=========================================================================================================================#
# Add Place FIPS to Geocoded Owner Transfer Data
#=========================================================================================================================#

# Get place polygons for Ohio
oh_places <- tigris::places(
  state = "OH",
  year = 2023,
  class = "sf",
  cb = TRUE
) %>%
  st_transform(4326) %>%
  select(
    place_geoid = GEOID,
    place_name  = NAME,
    place_namelsad = NAMELSAD,
    place_lsad = LSAD
  )

# unique(oh_places$place_lsad)  # 25 = city, 47 = village, 57 = CDP (Census-designated place)

# Convert your data with cousub back to sf (only non-NA lat/long)
pts_for_place <- df_ot_oh_geocoded_with_cousub_out %>%
  dplyr::filter(!is.na(lat), !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Spatial join to get place FIPS
df_ot_oh_geocoded_with_cousub_and_place <- st_join(
  pts_for_place,
  oh_places,
  join = st_intersects  
  # join = st_within  # points must be within place boundaries
)

# Convert back to regular dataframe
df_ot_oh_geocoded_with_cousub_and_place_out <- df_ot_oh_geocoded_with_cousub_and_place %>% st_drop_geometry()

nrow(df_ot_oh_geocoded_with_cousub_and_place_out)/nrow(df_ot_oh4) # 93.1% of total rows have lat/long + cousub/place FIPS

colnames(df_ot_oh_geocoded_with_cousub_and_place_out)

#==========================================================================#
# Adding FIPS_IDs to Geocoded Owner Transfer Data
# For townships, FIPS_ID = State FIPS + County FIPS + Township FIPS (10)
# For places - villages and cities, FIPS_ID = State FIPS + Place FIPS (7)
#==========================================================================#

ot_cols <- c("CLIP", "COMPOSITE PROPERTY LINKAGE KEY", "year", "LAND USE CODE - STATIC", "STATE USE DESCRIPTION - STATIC", "PROPERTY INDICATOR CODE - STATIC", "ACTUAL YEAR BUILT - STATIC", "TOTAL NUMBER OF BUILDINGS", "street", "city", "state", "zip5", "SALE DERIVED DATE", "SALE DERIVED RECORDING DATE" , "PRIMARY CATEGORY CODE", "DEED CATEGORY TYPE CODE", "SALE AMOUNT", "SALE DOCUMENT TYPE CODE", "SALE RECORDED DOCUMENT NUMBER", "RESIDENTIAL INDICATOR", "CASH PURCHASE INDICATOR", "MORTGAGE PURCHASE INDICATOR", "INTERFAMILY RELATED INDICATOR", "INVESTOR PURCHASE INDICATOR", "RESALE INDICATOR", "NEW CONSTRUCTION INDICATOR", "FORECLOSURE REO INDICATOR", "FORECLOSURE REO SALE INDICATOR", "BUYER 1 FULL NAME", "BUYER OCCUPANCY CODE", "BUYER MAILING STREET ADDRESS", "BUYER MAILING CITY", "BUYER MAILING STATE", "BUYER MAILING ZIP CODE", "SELLER 1 FULL NAME",  "lat",  "long", "cousub_geoid",  "cousub_name",  "cousub_namelsad",  "cousub_lsad",  "place_geoid",  "place_name",  "place_namelsad",  "place_lsad")

df_ot_final <- df_ot_oh_geocoded_with_cousub_and_place_out %>%
  dplyr::select(all_of(ot_cols)) %>%
  mutate(fips_id = case_when(
    !is.na(place_geoid) & place_lsad %in% c("25", "47") ~ place_geoid,  # Use place FIPS if city (25) or village (47)
    !is.na(cousub_geoid) & cousub_lsad == "44" ~ cousub_geoid,  # Use county subdivision FIPS if township (44)
    TRUE ~ NA_character_  # If neither condition is met, set as NA
  )) %>% relocate(fips_id, .before = year) %>% filter(!is.na(fips_id))


nrow(df_ot_final)/nrow(df_ot_oh4) # 89.6% of total rows have lat/long + cousub/place FIPS

# View(df_ot_final[1:100, ])


# Write to CSV
readr::write_csv(df_ot_final, file.path(data_cl, paste0("corelogic_ownertransfer_with_fips_", year_suffix, ".csv")))

# View(df_ot_oh_geocoded_with_cousub_and_place_out[1:100, ])


# Next, we need to create a unique row identifier. Right now, CLIPS is not unique because of missing values.
# We also have a column called "COMPOSITE PROPERTY LINKAGE KEY"... Bingo. It is unique in Property Characteristics data.
# Note: "COMPOSITE PROPERTY LINKAGE KEY" is also in df_ot_oh (Owner Transfer data) but not unique there because of multiple transactions per property.
# So we will use "COMPOSITE PROPERTY LINKAGE KEY" as the unique identifier to merge Property Characteristics with Owner Transfer data.





# Import Ohio Property Characteristics with geocoding + FIPS
# df_pc_oh_final <- read_csv(file.path(pc_out_loc, "corelogic_property_geocoded_with_cousub_place_oh.csv"), col_types = cols(.default = "c"))
df_pc_oh_final <- read_csv(file.path(pc_out_loc, "by_state", "corelogic_pc_OH.csv"), col_types = cols(.default = "c"))

# Import Ohio Owner Transfer file
df_ot_final <- read_csv(file.path(data_cl, paste0("corelogic_ownertransfer_with_fips_", year_suffix, ".csv")), col_types = cols(.default = "c"))

# We need to find the unique key columns to merge on. Will be doing left joining property characteristics to owner transfer.
colnames(df_pc_oh_final)
colnames(df_ot_final)
# Question: Are COMPOSITE PROPERTY LINKAGE KEY IDs present in both datasets and can be used as a unique key?

#==========================================================================#
# Merge Geocoded Property Characteristics with Owner Transfer Files
#==========================================================================#

# First, we will keep only necessary columns in both datasets.

# Columns to keep from PC
# pc_cols_to_keep <- c("COMPOSITE PROPERTY LINKAGE KEY", "CLIP", "fips_id", "PROPERTY INDICATOR CODE", "OWNER 1 FULL NAME", "SITUS STREET ADDRESS", "OWNER OCCUPANCY CODE", "MAILING UNIT NUMBER", "MAILING STREET ADDRESS", "MAILING CITY", "MAILING STATE", "MAILING ZIP CODE", "TAX YEAR", "ASSESSED YEAR", "TAX AREA CODE", "HOMESTEAD EXEMPT INDICATOR", "TOTAL NUMBER OF ACRES", "TOTAL LAND SQUARE FOOTAGE", "YEAR BUILT", "TOTAL NUMBER OF BEDROOMS - ALL BUILDINGS", "TOTAL NUMBER OF BATHROOMS - ALL BUILDINGS", "TOTAL NUMBER OF BATHROOMS", "GARAGE TYPE CODE", "TOTAL NUMBER OF PARKING SPACES", "PARKING TYPE CODE", "TOTAL NUMBER OF UNITS - ALL BUILDINGS", "lat", "long", "cousub_namelsad", "cousub_geoid", "place_geoid", "place_namelsad", "place_lsad")
pc_cols_to_keep <- c("CLIP", "COMPOSITE PROPERTY LINKAGE KEY", "APN (PARCEL NUMBER UNFORMATTED)", "CBSA TYPE", "LAND USE CODE", "COUNTY LAND USE DESCRIPTION", "ZONING CODE", "ZONING CODE DESCRIPTION", "PROPERTY INDICATOR CODE", "NUMBER OF BUILDINGS", "SITUS HOUSE NUMBER", "SITUS HOUSE NUMBER SUFFIX", "SITUS DIRECTION", "SITUS STREET NAME", "SITUS MODE", "SITUS QUADRANT", "SITUS UNIT NUMBER", "SITUS CITY", "SITUS STATE", "SITUS ZIP CODE", "SITUS COUNTY", "SITUS CARRIER ROUTE", "SITUS STREET ADDRESS", "LEGAL DESCRIPTION", "OWNER 1 FULL NAME", "OWNER 1 CORPORATE INDICATOR", "OWNER ETAL CODE", "OWNER OWNERSHIP RIGHTS CODE", "OWNER RELATIONSHIP TYPE CODE", "OWNER OCCUPANCY CODE", "MAILING HOUSE NUMBER", "MAILING HOUSE NUMBER SUFFIX", "MAILING HOUSE NUMBER 2", "MAILING DIRECTION", "MAILING STREET NAME", "MAILING MODE", "MAILING QUADRANT", "MAILING UNIT NUMBER", "MAILING CITY", "MAILING STATE", "MAILING ZIP CODE", "MAILING CARRIER ROUTE", "MAILING STREET ADDRESS", "MAILING OPT OUT INDICATOR", "CALCULATED TOTAL VALUE", "CALCULATED LAND VALUE", "CALCULATED IMPROVEMENT VALUE", "CALCULATED TOTAL VALUE SOURCE CODE", "ASSESSED TOTAL VALUE", "ASSESSED LAND VALUE", "ASSESSED IMPROVEMENT VALUE", "MARKET TOTAL VALUE", "MARKET LAND VALUE", "MARKET IMPROVEMENT VALUE", "APPRAISED TOTAL VALUE", "APPRAISED LAND VALUE", "APPRAISED IMPROVEMENT VALUE", "TOTAL TAX AMOUNT", "NET TAX AMOUNT", "TAX YEAR", "ASSESSED YEAR", "TAX AREA CODE", "TOTAL PROPERTY TAX RATE PERCENT", "TAXABLE IMPROVEMENT VALUE", "TAXABLE LAND VALUE", "TAXABLE OTHER VALUE", "NET TAXABLE VALUE", "HOMESTEAD EXEMPT INDICATOR", "SENIOR EXEMPT INDICATOR", "DISABLED EXEMPT INDICATOR", "VETERAN EXEMPT INDICATOR", "TOTAL TAX EXEMPTION AMOUNT", "CALCULATED TOTAL TAX EXEMPTION AMOUNT", "FRONT FOOTAGE", "DEPTH FOOTAGE", "TOTAL NUMBER OF ACRES", "TOTAL LAND SQUARE FOOTAGE", "EASEMENT TYPE CODE", "YEAR BUILT", "EFFECTIVE YEAR BUILT", "TOTAL NUMBER OF BEDROOMS - ALL BUILDINGS", "TOTAL NUMBER OF ROOMS - ALL BUILDINGS", "TOTAL NUMBER OF BATHROOMS - ALL BUILDINGS", "TOTAL NUMBER OF FULL BATHS - ALL BUILDINGS", "TOTAL NUMBER OF BATHROOMS", "AIR CONDITIONING TYPE CODE", "BASEMENT FINISH TYPE CODE", "BASEMENT TYPE CODE", "FIREPLACE INDICATOR", "TOTAL NUMBER OF FIREPLACES", "FIREPLACE TYPE CODE", "FLOOR TYPE CODE", "HEATING TYPE CODE", "STORIES TYPE CODE", "TOTAL NUMBER OF STORIES", "BUILDING TYPE CODE", "BUILDING IMPROVEMENT TYPE CODE", "BUILDING IMPROVEMENT CONDITION CODE", "CONSTRUCTION TYPE CODE", "EXTERIOR WALL TYPE CODE", "FOUNDATION TYPE CODE", "FRAME TYPE CODE", "GARAGE TYPE CODE", "TOTAL NUMBER OF PARKING SPACES", "PARKING TYPE CODE", "POOL INDICATOR", "POOL TYPE CODE", "BUILDING QUALITY CODE", "ROOF COVER TYPE CODE", "ROOF TYPE CODE", "BUILDING STYLE TYPE CODE", "TOTAL NUMBER OF UNITS - ALL BUILDINGS", "UNIVERSAL BUILDING SQUARE FEET", "UNIVERSAL BUILDING SQUARE FEET SOURCE CODE", "BUILDING AREA SQUARE FEET", "TOTAL LIVING AREA SQUARE FEET - ALL BUILDINGS", "GROUND FLOOR AREA SQUARE FEET", "BUILDING GROSS AREA SQUARE FEET", "BUILDING ADJUSTED AREA SQUARE FEET", "BASEMENT AREA SQUARE FEET", "GARAGE OR PARKING SQUARE FEET", "SECOND FLOOR AREA SQUARE FEET", "FINISHED BASEMENT AREA SQUARE FEET", "UNFINISHED BASEMENT AREA SQUARE FEET", "FUEL TYPE CODE", "ELECTRICITY/WIRING TYPE CODE", "SEWER TYPE CODE", "UTILITIES TYPE CODE", "WATER TYPE CODE", "BUILDING FUEL TYPE CODE", "LAST ASSESSOR UPDATE DATE", "TAXROLL CERTIFICATION DATE", "TAXROLL EDITION NUMBER", "BEGINNING TAX YEAR DATE", "ENDING TAX YEAR DATE", "TOTAL SQUARE FOOTAGE OPEN AREAS", "TOTAL AREA SQUARE FOOTAGE - ALL BUILDINGS", "TOTAL AREA SQUARE FOOTAGE OFFICE SPACE - ALL BUILDINGS", "RECORD ACTION INDICATOR", "BUILDING IMPROVEMENT CONDITION CODE")

df_pc_oh_final_fp2 <- df_pc_oh_final %>% select(all_of(pc_cols_to_keep)) %>% rename(CLIP_PC = `CLIP`)

# colnames(df_pc_oh_final)
# unique(df_pc_oh_final$`BUILDING QUALITY CODE`)
# colnames(df_pc_oh_final)
# colnames(df_ot_oh3)

# Columns to keep from OT
# ot_cols_to_keep <- c("COMPOSITE PROPERTY LINKAGE KEY", "CLIP", "LAND USE CODE - STATIC", "STATE USE DESCRIPTION - STATIC", "ZONING CODE - STATIC", "PROPERTY INDICATOR CODE - STATIC", "ACTUAL YEAR BUILT - STATIC", "DEED SITUS STREET ADDRESS - STATIC", "DEED SITUS CITY - STATIC", "DEED SITUS STATE - STATIC", "PRIMARY CATEGORY CODE", "DEED CATEGORY TYPE CODE", "SALE AMOUNT", "SALE DERIVED DATE", "SALE DERIVED RECORDING DATE" , "SALE DOCUMENT TYPE CODE", "SALE RECORDED DOCUMENT NUMBER", "RESIDENTIAL INDICATOR", "CASH PURCHASE INDICATOR", "MORTGAGE PURCHASE INDICATOR", "INTERFAMILY RELATED INDICATOR", "INVESTOR PURCHASE INDICATOR", "RESALE INDICATOR", "NEW CONSTRUCTION INDICATOR", "FORECLOSURE REO INDICATOR", "FORECLOSURE REO SALE INDICATOR", "BUYER 1 FULL NAME", "BUYER OCCUPANCY CODE", "BUYER MAILING STREET ADDRESS", "BUYER MAILING CITY", "BUYER MAILING STATE", "BUYER MAILING ZIP CODE", "SELLER 1 FULL NAME")

# df_ot_final2 <- df_ot_final %>% select(all_of(ot_cols_to_keep)) %>% rename(CLIP_OT = `CLIP`)
df_ot_final2 <- df_ot_final %>% rename(CLIP_OT = `CLIP`)
  

# Find common column names between the two datasets
common_cols <- intersect(colnames(df_ot_final2), colnames(df_pc_oh_final_fp2))


#--------------------------------------------#
# First join - COMPOSITE PROPERTY LINKAGE KEY
#--------------------------------------------#


# Perform left join of Owner Transfer with Property Characteristics on COMPOSITE PROPERTY LINKAGE KEY
df_ot_oh_merged <- df_ot_final2 %>%
  left_join(df_pc_oh_final_fp2, by = "COMPOSITE PROPERTY LINKAGE KEY")

# View(df_ot_oh_merged[1:10000, ])
# nrow(df_ot_oh_merged)

# View(df_ot_oh_merged[1:100,])

# How many OT rows found a match in PC on "COMPOSITE PROPERTY LINKAGE KEY" (row-level)?
ot_join_stats <- tibble(
  total_ot_rows      = nrow(df_ot_final2),
  ot_missing_key     = sum(is.na(df_ot_final2$`COMPOSITE PROPERTY LINKAGE KEY`) |
                             df_ot_final2$`COMPOSITE PROPERTY LINKAGE KEY` == ""),
  ot_rows_matched    = nrow(semi_join(
    df_ot_final2,
    df_pc_oh_final_fp2 %>% distinct(`COMPOSITE PROPERTY LINKAGE KEY`),
    by = "COMPOSITE PROPERTY LINKAGE KEY"
  )),
  ot_rows_unmatched  = nrow(anti_join(
    df_ot_final2,
    df_pc_oh_final_fp2 %>% distinct(`COMPOSITE PROPERTY LINKAGE KEY`),
    by = "COMPOSITE PROPERTY LINKAGE KEY"
  ))
) %>%
  mutate(
    matched_prop   = ot_rows_matched / total_ot_rows,
    unmatched_prop = ot_rows_unmatched / total_ot_rows
  )

# print(ot_join_stats)

#--------------------------------------------#
# Second join - CLIP
#--------------------------------------------#

# Identify which rows matched in the first join
# A row matched if COMPOSITE PROPERTY LINKAGE KEY exists in both datasets
keys_in_pc <- df_pc_oh_final_fp2 %>% 
  filter(!is.na(`COMPOSITE PROPERTY LINKAGE KEY`) & `COMPOSITE PROPERTY LINKAGE KEY` != "") %>%
  pull(`COMPOSITE PROPERTY LINKAGE KEY`) %>%
  unique()

df_ot_oh_merged <- df_ot_oh_merged %>%
  mutate(matched_on_composite_key = `COMPOSITE PROPERTY LINKAGE KEY` %in% keys_in_pc)

# For unmatched rows, try to fill in data from PC using CLIP_OT = CLIP_PC
# Create a lookup from PC data using CLIP_PC as key
pc_lookup <- df_pc_oh_final_fp2 %>% 
  select(all_of(c("CLIP_PC", setdiff(pc_cols_to_keep, "CLIP")))) %>%
  # If there are duplicates on CLIP_PC, keep the first occurrence
  distinct(CLIP_PC, .keep_all = TRUE)

# For rows that didn't match on COMPOSITE PROPERTY LINKAGE KEY, try matching on CLIP
df_ot_oh_merged <- df_ot_oh_merged %>%
  mutate(
    across(
      .cols = all_of(setdiff(names(pc_lookup), c("CLIP_PC", "COMPOSITE PROPERTY LINKAGE KEY"))),
      .fns = ~{
        # Only fill NA/missing values with CLIP-based match for unmatched rows
        if_else(
          !matched_on_composite_key & !is.na(CLIP_OT) & (is.na(.x) | .x == ""),
          pc_lookup[[cur_column()]][match(CLIP_OT, pc_lookup$CLIP_PC)],
          .x
        )
      },
      .names = "{.col}"
    )
  )

# Check final matching status
df_ot_oh_merged <- df_ot_oh_merged %>% 
  mutate(has_pc_data = !is.na(`APN (PARCEL NUMBER UNFORMATTED)`) & 
                       trimws(`APN (PARCEL NUMBER UNFORMATTED)`) != "" & 
                       trimws(`APN (PARCEL NUMBER UNFORMATTED)`) != "NA")

# Summary of matching
message("Final Match summary:")
df_ot_oh_merged %>% group_by(has_pc_data) %>%
  summarise(count = n() , prop = n() / nrow(.), .groups = "drop") %>%
  arrange(desc(count)) %>%
  print(n = Inf)


# Coverage of OT + PC merged data by year is lower than original housesales_9521_slim.dta, but still okay-ish.
# Coverage of OT + PC merged data by year is lower than OT alone because some OT rows could not be matched to PC data.

# Output as csv
readr::write_csv(df_ot_oh_merged, file.path(data_cl, paste0("corelogic_ownertransfer_propertycharacteristics_merged_oh_", year_suffix, ".csv")))


#=========================================================================================================================#
# Geocoding Sample using Google Maps API (for testing purposes only - not used in final code)
# NOTE: Google Maps API has a free tier but requires billing setup and has usage limits. Use with caution.
#=========================================================================================================================#

# already geocded using census geocoder. We will compare a sample of those results with Google Maps geocoding to validate accuracy.
df_ot_final <- read_csv(file.path(data_cl, paste0("corelogic_ownertransfer_with_fips_", year_suffix, ".csv")), col_types = cols(.default = "c"))

# Load env vars from .env (your existing approach)
load_dot_env(file = file.path(root, "code/.env"))

# Your .env currently has GOOGLE_API_KEY, but tidygeocoder expects GOOGLEGEOCODE_API_KEY
google_key <- Sys.getenv("GOOGLE_API_KEY")

# Make it available under the name tidygeocoder expects
Sys.setenv(GOOGLEGEOCODE_API_KEY = google_key)


df_sample <- df_ot_final %>%
  mutate(row_id = row_number()) %>%
  filter(
    !is.na(street), street != "",
    !is.na(city),   city   != "",
    !is.na(state),  state  != ""
  ) %>%
  mutate(zip5 = na_if(zip5, "")) %>%  # treat "" as missing
  unite("addr_google", street, city, state, zip5, sep = ", ", remove = FALSE, na.rm = TRUE) %>%
  mutate(addr_google = str_squish(addr_google))

set.seed(123)
sample_n <- min(10000L, nrow(df_sample))

df_sample_geo <- df_sample %>%
  slice_sample(n = sample_n) %>%
  tidygeocoder::geocode(
    address = addr_google,
    method  = "google",
    lat     = lat_google,
    long    = long_google,
    full_results = TRUE
  )
  
# Spatially assign Google-based place/county-subdivision FIPS
oh_places_g <- oh_places %>%
  dplyr::select(place_geoid_g = place_geoid, place_lsad_g = place_lsad)

oh_cousub_g <- oh_cousub %>%
  dplyr::select(cousub_geoid_g = cousub_geoid, cousub_lsad_g = cousub_lsad)

df_google_fips <- df_sample_geo %>%
  dplyr::filter(!is.na(lat_google), !is.na(long_google)) %>%
  sf::st_as_sf(coords = c("long_google", "lat_google"), crs = 4326, remove = FALSE) %>%
  sf::st_join(oh_places_g, join = sf::st_intersects) %>%
  sf::st_join(oh_cousub_g, join = sf::st_intersects) %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    fips_id_google = dplyr::case_when(
      !is.na(place_geoid_g) & place_lsad_g %in% c("25", "47") ~ place_geoid_g,
      !is.na(cousub_geoid_g) & cousub_lsad_g == "44" ~ cousub_geoid_g,
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(row_id, fips_id_google)


# Write Google FIPS comparison data to file
# readr::write_csv(df_google_fips, file.path(data_cl, paste0("corelogic_ot_google_sample_fips_", year_suffix, ".csv")))

# Compare original vs Google-based FIPS
df_fips_compare <- df_sample_geo %>%
  dplyr::left_join(df_google_fips, by = "row_id") %>%
  dplyr::mutate(
    fips_match_status = dplyr::case_when(
      is.na(fips_id) & is.na(fips_id_google) ~ "both_missing",
      !is.na(fips_id) & !is.na(fips_id_google) & fips_id == fips_id_google ~ "same",
      !is.na(fips_id) & !is.na(fips_id_google) & fips_id != fips_id_google ~ "different",
      is.na(fips_id) & !is.na(fips_id_google) ~ "missing_original_only",
      !is.na(fips_id) & is.na(fips_id_google) ~ "missing_google_only",
      TRUE ~ "other"
    )
  ) %>% relocate(fips_id, .before = fips_id_google)

View(df_fips_compare)

df_fips_compare %>%
  dplyr::count(fips_match_status) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::arrange(desc(n)) %>%
  print(n = Inf)

readr::write_csv(
  df_fips_compare,
  file.path(data_cl, paste0("corelogic_ot_google_fips_compare_sample10k_", year_suffix, ".csv"))
)
