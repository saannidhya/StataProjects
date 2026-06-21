#=========================================================================================================================#
# Purpose : Geocode + FIPS CoreLogic  University_of_Cincinnati_OwnerTransfer_v3_dpc_01549150_20240903_114649_data.txt
# Name    : Saani Rawat
# Created : 12/01/2025
# Log     : 
#        1. 12/01/2025: started the code. Ran on a sample.
#        2. 12/10/2025: Added county subdivision FIPS code addition.
#        3. 12/11/2025: Added place FIPS code addition. 
#        4. 12/20/2025: Added code to split Owner Transfer data into state-level files.
#        5. 01/08/2026: Added code to merge geocoded Property Characteristics data to Owner Transfer data using COMPOSITE PROPERTY LINKAGE KEY and CLIP.
#=========================================================================================================================#


# Load necessary libraries
library(tidyverse)
library(tidycensus)
library(tidygeocoder)
library(tigris)
library(sf)
library(haven)

# import loc
CoreLogic_loc <- "C:/CoreLogic"

# out loc
ot_out_loc <- paste0(CoreLogic_loc,"/housing/OwnerTransfer/")
pc_out_loc <- paste0(CoreLogic_loc,"/housing/PropertyCharacteristics/")

#=====================================================================#
# Split Property Characteristics into State-Level Files
#=====================================================================#

# Property Characteristics data file path
pc_data_file <- file.path(CoreLogic_loc, "University_of_Cincinnati_hist_property3_dpc_01549149_15_20240903_174500_data", "University_of_Cincinnati_hist_property3_dpc_01549149_15_20240903_174500_data.txt")

# Output directory for state files
state_out_dir <- file.path(pc_out_loc, "by_state")
if (!dir.exists(state_out_dir)) dir.create(state_out_dir, recursive = TRUE)

#--- Write state-level files ---

# Initialize file handles (we'll write headers on first chunk for each state)
states_written <- character(0)

write_state_files_chunk <- function(df, pos) {
  message("Processing chunk @ pos ", pos, " with ", nrow(df), " rows")
  
  # Get unique states in this chunk
  chunk_states <- unique(df$`SITUS STATE`)
  chunk_states <- chunk_states[!is.na(chunk_states) & chunk_states != ""]
  
  for (st in chunk_states) {
    state_df <- df %>% filter(`SITUS STATE` == st)
    
    if (nrow(state_df) == 0L) next
    
    # Use .csv for easier downstream use (fwrite is fastest but write_csv is fine)
    state_file <- file.path(state_out_dir, paste0("corelogic_pc_", st, ".csv"))
    
    # Append if file exists, otherwise write with header
    append_mode <- st %in% states_written
    
    readr::write_csv(
      state_df,
      state_file,
      append = append_mode
    )
    
    if (!append_mode) {
      states_written <<- c(states_written, st)
    }
  }

  # function returns nothing. Invisible suppresses printing NULL.
  invisible(NULL)
}

message("NOTE: Writing state-level files...")
write_start <- Sys.time()

readr::read_delim_chunked(
  file       = pc_data_file,
  delim      = "|",
  col_types  = cols(.default = "c"),
  chunk_size = 100000,   # moderate chunk size for writing
  callback   = DataFrameCallback$new(write_state_files_chunk),
  progress   = TRUE
)

write_end <- Sys.time()
message("NOTE: Finished writing state files. Run time: ", 
        round(difftime(write_end, write_start, units = "mins"), 2), " minutes")
message("Files written to: ", state_out_dir)
message("States processed: ", paste(sort(states_written), collapse = ", "))
# 72,907,268 rows in total


#=========================================================================================================================#
# Property Characteristics Data - FULL GEOCODING (chunked) - Ohio only 
#=========================================================================================================================#

# Property Characteristics - Ohio only file
df_pc_oh <- read_csv(file.path(state_out_dir, "corelogic_pc_OH.csv"), col_types = cols(.default = "c"))


clean_pc_chunk <- function(df) {
  df %>%
    mutate(
      street = str_squish(`SITUS STREET ADDRESS`),
      city   = str_squish(`SITUS CITY`),
      state  = str_squish(`SITUS STATE`),
      zip5   = str_sub(`SITUS ZIP CODE`, 1, 5)
    ) %>%
    # drop obviously unusable SITUS rows
    filter(
      !is.na(street),
      street != "",
      !str_detect(street, regex("^PO BOX", ignore_case = TRUE)),
      !str_detect(street, regex("INVALID NUMBER", ignore_case = TRUE))
    )
}

pc_full_outfile <- file.path(pc_out_loc, "corelogic_property_full_geocoded_oh.csv")

# If re-running, start fresh
if (file.exists(pc_full_outfile)) file.remove(pc_full_outfile)

pc_full_chunk_counter <- 0L

process_pc_chunk_full <- function(df, pos) {
  # 1. Clean SITUS address columns
  df_clean <- clean_pc_chunk(df)

  if (nrow(df_clean) == 0L) return(invisible(NULL))

  message("FULL geocode chunk @ pos ", pos, " with ", nrow(df_clean), " rows")

  # 2. Geocode this chunk
  geo_chunk <- df_clean %>%
    geocode(
      street     = street,
      city       = city,
      state      = state,
      postalcode = zip5,
      method     = "census",
      full_results = TRUE,
      api_options = list(census_return_type = "geographies")
    ) 

  # 3. Append to CSV
  pc_full_chunk_counter <<- pc_full_chunk_counter + 1L

  readr::write_csv(
    geo_chunk,
    pc_full_outfile,
    append = pc_full_chunk_counter > 1L
  )

  rm(geo_chunk, df_clean); gc()
  invisible(NULL)
  Sys.sleep(2)  # brief pause to be nice to the geocoding service
}

# Run the full chunked geocoding
start_time_full <- Sys.time()
message("NOTE: Starting FULL PC geocoding (this may take a VERY long time)...")

# Process df_pc_oh in chunks
chunk_size <- 10000
total_rows <- nrow(df_pc_oh)
num_chunks <- ceiling(total_rows / chunk_size)

message("NOTE: Processing ", total_rows, " rows in ", num_chunks, " chunks...")

for (i in seq_len(num_chunks)) {
  start_idx <- (i - 1) * chunk_size + 1
  end_idx <- min(i * chunk_size, total_rows)
  
  message("Processing chunk ", i, "/", num_chunks, " (rows ", start_idx, "-", end_idx, ")")
  
  # Extract chunk
  chunk_df <- df_pc_oh[start_idx:end_idx, ]
  
  # Process this chunk using existing function
  process_pc_chunk_full(chunk_df, pos = start_idx)
}

end_time_full <- Sys.time()
elapsed_time_full <- end_time_full - start_time_full
message("NOTE: Finished FULL PC geocoding. Run time: ", elapsed_time_full)
message("Output written to: ", pc_full_outfile)


# Import the geocoded CSV file
corelogic_property_full_geocoded <- read_csv(pc_full_outfile)
# View(corelogic_property_full_geocoded[1:1000, ])

# proportion geocoded by tidygeocoder: 0.9103692
corelogic_property_full_geocoded %>%
  filter(!is.na(lat)) %>%
  nrow() / corelogic_property_full_geocoded %>% nrow()  

# proportion geocoded by CoreLogic (PARCEL LEVEL LONGITUDE not NA): 0.08653028
# sum(!is.na(corelogic_property_full_geocoded[["PARCEL LEVEL LONGITUDE"]]))/ nrow(corelogic_property_full_geocoded)  # 0.08653028

# sum(!is.na(corelogic_property_full_geocoded[["BLOCK LEVEL LONGITUDE"]]))/ nrow(corelogic_property_full_geocoded)  # 0.9450272


df_pc_oh <- read_csv(file.path(state_out_dir, "corelogic_pc_OH.csv"))
nrow(df_pc_oh)
# 3,205,426

df_pc_oh_geocoded <- readr::read_csv(paste0(pc_out_loc, "corelogic_property_full_geocoded_oh.csv")) 
nrow(df_pc_oh_geocoded %>% filter(!is.na(lat)))
# 2,912,951 geocoded addresses in Ohio.
# View(df_pc_oh_geocoded[1:100,])

nrow(df_pc_oh_geocoded %>% filter(!is.na(lat)))/ nrow(df_pc_oh)
# 91% of addresses geocoded in Ohio.


#=========================================================================================================================#
# Add County Subdivision FIPS to Geocoded Property Characteristics Data
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
pts <- df_pc_oh_geocoded %>%
  dplyr::filter(!is.na(lat), !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Spatial join to get county subdivision FIPS
df_pc_oh_geocoded_with_cousub <- st_join(
  pts,
  oh_cousub,
  join = st_intersects  
  # join = st_within  # default is st_intersects; within is stricter
)
# Convert sf object back to regular dataframe and write to CSV
df_pc_oh_geocoded_with_cousub_out <- df_pc_oh_geocoded_with_cousub %>% st_drop_geometry()
nrow(df_pc_oh_geocoded_with_cousub_out)

# Write to CSV
readr::write_csv(df_pc_oh_geocoded_with_cousub_out, file.path(pc_out_loc, "corelogic_property_geocoded_with_cousub_oh.csv"))



View(df_pc_oh_geocoded_with_cousub_out[1:100, ])

message("County subdivision FIPS added. Output written to: ", file.path(pc_out_loc, "corelogic_property_geocoded_with_cousub_oh.csv"))

#=========================================================================================================================#
# Add Place FIPS to Geocoded Property Characteristics Data
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
pts_for_place <- df_pc_oh_geocoded_with_cousub_out %>%
  dplyr::filter(!is.na(lat), !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Spatial join to get place FIPS
df_pc_oh_geocoded_with_cousub_and_place <- st_join(
  pts_for_place,
  oh_places,
  join = st_intersects  
  # join = st_within  # points must be within place boundaries
)

# Convert back to regular dataframe
df_pc_oh_geocoded_with_cousub_and_place_out <- df_pc_oh_geocoded_with_cousub_and_place %>% st_drop_geometry()

nrow(df_pc_oh_geocoded_with_cousub_and_place_out)

View(df_pc_oh_geocoded_with_cousub_and_place_out[1:100, ])

# Write to CSV
readr::write_csv(df_pc_oh_geocoded_with_cousub_and_place_out, file.path(pc_out_loc, "corelogic_property_geocoded_with_cousub_place_oh.csv"))

# View(df_pc_oh_geocoded_with_cousub_and_place_out[1:100, ])

# Output as Stata .dta file
# Clean column names for Stata compatibility (max 32 chars, no spaces/special chars)
df_stata_out <- df_pc_oh_geocoded_with_cousub_and_place_out %>%
  janitor::clean_names() 

# Shorten column names systematically for Stata (max 32 chars)
colnames(df_stata_out) <- colnames(df_stata_out) %>%
  str_replace_all("description", "desc") %>%
  str_replace_all("number_of", "num") %>%
  str_replace_all("total_", "tot_") %>%
  str_replace_all("amount", "amt") %>%
  str_replace_all("transaction", "trans") %>%
  str_replace_all("delivery_point_validation", "dpv") %>%
  str_replace_all("first_name_and_middle_initial", "name_mi") %>%
  str_replace_all("calculated", "calc") %>%
  str_replace_all("statistical", "stat") %>%
  str_replace_all("core_based", "cb") %>%
  str_replace_all("elementary", "elem") %>%
  str_replace_all("district", "dist") %>%
  str_replace_all("buildings", "bldg") %>%
  str_replace_all("bathrooms", "bath") %>%
  str_replace_all("bedrooms", "bedrm") %>%
  str_replace_all("exemption", "exempt") %>%
  str_replace_all("indicator", "ind") %>%
  str_replace_all("mailing", "mail") %>%
  str_replace_all("foreclosure", "forecl")

df_stata_out <- df_stata_out %>%
          rename(build_impr_cond_code = building_improvement_condition_code,
                 univ_bldg_sqft_src_code = universal_building_square_feet_source_code,
                 tot_living_area_sqft_all_bldg = tot_living_area_square_feet_all_bldg,
                 bldg_adj_area_sqft = building_adjusted_area_square_feet,
                 fin_bsmnt_area_sqft = finished_basement_area_square_feet,
                 unfin_bsmnt_area_sqft = unfinished_basement_area_square_feet,
                 tot_area_sqft_office_all_bldg = tot_area_square_footage_office_space_all_bldg,
                 longitude = long,
                 latitude = lat) %>%
                 select(-legal_desc)

# colnames(df_stata_out)

# Diagnose the problematic column/cell
message("Checking column 74...")
names(df_stata_out)[74]
df_stata_out[13, 74]

# Check for string length issues across all character columns
message("Checking for long strings...")
for (col in names(df_stata_out)) {
  if (is.character(df_stata_out[[col]])) {
    max_len <- max(nchar(df_stata_out[[col]], keepNA = TRUE), na.rm = TRUE)
    if (max_len > 2045) {
      message("Column '", col, "' has max length: ", max_len)
    }
  }
}

# Truncate long strings to Stata's limit
# df_stata_out <- df_stata_out %>%
#   mutate(across(where(is.character), ~str_sub(.x, 1, 2045)))

write_dta(df_stata_out, file.path(pc_out_loc, "corelogic_property_geocoded_with_cousub_place_oh.dta"))


#=========================================================================================================================#
# Check if CLIPS is a unique identifier
#=========================================================================================================================#

# Looks like assessed_year is only till 2005 2006 2007 2008 2009 for Ohio..

sort(unique(df_stata_out$year_built))
sort(unique(df_stata_out$assessed_year))
sort(unique(df_stata_out$tax_year))


colnames(df_stata_out)

# Check for duplicates in CLIPS column
clips_check <- df_stata_out %>%
  group_by(clip) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

if (nrow(clips_check) == 0) {
  message("CLIPS is a unique identifier - no duplicates found.")
} else {
  message("WARNING: CLIPS is NOT unique - found ", nrow(clips_check), " duplicated values")
  message("Total duplicate observations: ", sum(clips_check$count))
}

# Check for missing CLIPS values
missing_clips <- sum(is.na(df_stata_out$clip))
message("Missing CLIPS values: ", missing_clips, " (", round(missing_clips/nrow(df_stata_out)*100, 2), "%)")

# KEY FINDING: CLIPS is UNIQUE. However, 10% of observations have missing CLIPS IDs.


#=====================================================================#
# Split Owner Transfer into State-Level Files
#=====================================================================#

ot_data_file <- file.path(CoreLogic_loc, "University_of_Cincinnati_OwnerTransfer_v3_dpc_01549150_20240903_114649_data", "University_of_Cincinnati_OwnerTransfer_v3_dpc_01549150_20240903_114649_data.txt")

# ot_data_file <- file.path(CoreLogic_loc, "University_of_Cincinnati_hist_property3_dpc_01549149_15_20240903_174500_meta", "SampleRecords.txt")

# # Get column names without importing the whole file
# ot_colnames <- readr::read_delim(
#   file = ot_data_file,
#   delim = "|",
#   col_types = cols(.default = "c"),
#   n_max = 10  # Read 0 rows, only gets column names
# )
# # %>% names()
# View(ot_colnames)
# message("Column names in Owner Transfer data:")
# print(ot_colnames)

# Output directory for state files
state_ot_out_dir <- file.path(ot_out_loc, "by_state")
if (!dir.exists(state_ot_out_dir)) dir.create(state_ot_out_dir, recursive = TRUE)

#--- Write state-level files ---

# Initialize file handles (we'll write headers on first chunk for each state)
states_written <- character(0)

write_state_files_chunk_ot <- function(df, pos) {
  message("Processing chunk @ pos ", pos, " with ", nrow(df), " rows")
  
  # Get unique states in this chunk
  chunk_states <- unique(toupper(df$`DEED SITUS STATE - STATIC`))

  chunk_states <- chunk_states[!is.na(chunk_states) & chunk_states != ""]
  
  for (st in chunk_states) {

    state_df <- df %>% filter(toupper(`DEED SITUS STATE - STATIC`) == st)
    
    if (nrow(state_df) == 0L) next
    
    # Use .csv for easier downstream use (fwrite is fastest but write_csv is fine)
    state_file <- file.path(state_ot_out_dir, paste0("corelogic_ot_", st, ".csv"))
    
    # Append if file exists, otherwise write with header
    append_mode <- st %in% states_written
    
    readr::write_csv(
      state_df,
      state_file,
      append = append_mode
    )
    
    if (!append_mode) {
      states_written <<- c(states_written, st)
    }
  }

  # function returns nothing. Invisible suppresses printing NULL.
  invisible(NULL)
}

message("NOTE: Writing state-level files...")
write_start <- Sys.time()

readr::read_delim_chunked(
  file       = ot_data_file,
  delim      = "|",
  col_types  = cols(.default = "c"),
  chunk_size = 100000,   # moderate chunk size for writing
  callback   = DataFrameCallback$new(write_state_files_chunk_ot),
  progress   = TRUE
)

write_end <- Sys.time()
message("NOTE: Finished writing state files. Run time: ", 
        round(difftime(write_end, write_start, units = "mins"), 2), " minutes")
message("Files written to: ", state_ot_out_dir)
message("States processed: ", paste(sort(states_written), collapse = ", "))

# States processed: A, AE, AK, AL, AP, AR, AZ, CA, CO, CT, DA, DC, DE, DR, F, FL, GA, GU, HI, I, IA, ID, IL, IN, KS, KY, LA, LL, LN, M0, MA, MD, ME, MI, MN, MO, MS, MT, N, NC, ND, NE, NH, NJ, NM, NV, NY, OD, OH, OK, OR, PA, PR, RI, SC, SD, ST, T, TC, TN, TR, TX, UT, V, VA, VI, VT, WA, WI, WV, WY, YX
# NOTE: Finished writing state files. Run time: 856.89 minutes i.e. 14.28 hours

#=====================================================================#
# Get names of all datasets in Owner Transfer by_state directory
#=====================================================================#

# List all files in the by_state directory
ot_state_files <- list.files(
  path = state_ot_out_dir,
  pattern = "\\.csv$",
  full.names = FALSE
)

message("Found ", length(ot_state_files), " state-level Owner Transfer files:")
print(ot_state_files)

# Extract state codes from filenames
state_codes <- str_extract(ot_state_files, "(?<=corelogic_ot_)[^.]+")
message("\nState codes: ", paste(sort(state_codes), collapse = ", "))


#==========================================================================#
# Adding FIPS_IDs to Geocoded Property Characteristics Data
# For townships, FIPS_ID = State FIPS + County FIPS + Township FIPS (10)
# For places - villages and cities, FIPS_ID = State FIPS + Place FIPS (7)
#==========================================================================#

# Import Ohio Property Characteristics with geocoding + FIPS
df_pc_oh_final <- read_csv(file.path(pc_out_loc, "corelogic_property_geocoded_with_cousub_place_oh.csv"), col_types = cols(.default = "c"))


df_pc_oh_final_fp <- df_pc_oh_final %>%
  mutate(
    fips_id = case_when(
      !is.na(place_geoid) & place_lsad %in% c("25", "47") ~ place_geoid,  # Use place FIPS if city (25) or village (47)
      !is.na(cousub_geoid) & cousub_lsad == "44" ~ cousub_geoid,  # Use county subdivision FIPS if township (44)
      TRUE ~ NA_character_  # If neither condition is met, set as NA
    )
  ) %>% relocate(fips_id, .after = CLIP)

fips_count <- df_pc_oh_final_fp %>% group_by(fips_id) %>%
  summarise(count = n(), prop = count / nrow(.), .groups = "drop") %>%
  filter(!is.na(fips_id)) %>%
  arrange(desc(count)) 

sum(is.na(df_pc_oh_final_fp$fips_id)) # 252 with no FIPS_ID
sum(is.na(df_pc_oh_final_fp$fips_id))/nrow(df_pc_oh_final_fp)  # very very small

# Next, we need to create a unique row identifier. Right now, CLIPS is not unique because of missing values.
# We also have a column called "COMPOSITE PROPERTY LINKAGE KEY"... Bingo. It is unique.
# Note: "COMPOSITE PROPERTY LINKAGE KEY" is also in df_ot_oh (Owner Transfer data).

# Check if COMPOSITE PROPERTY LINKAGE KEY is a unique identifier
composite_check <- df_pc_oh_final_fp %>%
  group_by(`COMPOSITE PROPERTY LINKAGE KEY`) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

if (nrow(composite_check) == 0) {
  message("COMPOSITE PROPERTY LINKAGE KEY is a unique identifier - no duplicates found.")
} else {
  message("WARNING: COMPOSITE PROPERTY LINKAGE KEY is NOT unique - found ", nrow(composite_check), " duplicated values")
  message("Total duplicate observations: ", sum(composite_check$count))
}

# Check for missing COMPOSITE PROPERTY LINKAGE KEY values
missing_composite <- sum(is.na(df_pc_oh_final_fp$`COMPOSITE PROPERTY LINKAGE KEY`))
message("Missing COMPOSITE PROPERTY LINKAGE KEY values: ", missing_composite, " (", round(missing_composite/nrow(df_pc_oh_final_fp)*100, 2), "%)")

colnames(df_pc_oh_final_fp)
colnames(df_ot_oh)
View(df_ot_oh[1:100, ])

sum(is.na(df_ot_oh$`SALE DERIVED DATE`))
sum(is.na(df_ot_oh$`SALE DERIVED RECORDING DATE`))



#==========================================================================#
# Merge check: Geocoded Property Characteristics with Owner Transfer Files
#==========================================================================#


# Import Ohio Property Characteristics with geocoding + FIPS
df_pc_oh_final <- read_csv(file.path(pc_out_loc, "corelogic_property_geocoded_with_cousub_place_oh.csv"), col_types = cols(.default = "c"))

# Import Ohio Owner Transfer file
df_ot_oh <- read_csv(file.path(state_ot_out_dir, "corelogic_ot_OH.csv"), col_types = cols(.default = "c"))

# We need to find the unique key columns to merge on. Will be doing left joining property characteristics to owner transfer.
colnames(df_pc_oh_final)
colnames(df_ot_oh)

# Question: Are COMPOSITE PROPERTY LINKAGE KEY IDs present in both datasets and can be used as a unique key?

# Check 1: Are COMPOSITE PROPERTY LINKAGE KEY IDs present in both datasets?
message("\n=== COMPOSITE PROPERTY LINKAGE KEY Merge Compatibility Checks ===\n")

# Check for COMPOSITE PROPERTY LINKAGE KEY column existence
message("Check 1: COMPOSITE PROPERTY LINKAGE KEY column presence")
pc_has_composite <- "COMPOSITE PROPERTY LINKAGE KEY" %in% colnames(df_pc_oh_final)
ot_has_composite <- "COMPOSITE PROPERTY LINKAGE KEY" %in% colnames(df_ot_oh)
message("  Property Characteristics has COMPOSITE PROPERTY LINKAGE KEY: ", pc_has_composite)
message("  Owner Transfer has COMPOSITE PROPERTY LINKAGE KEY: ", ot_has_composite)

# Check 2: Missing COMPOSITE PROPERTY LINKAGE KEY values in each dataset
message("\nCheck 2: Missing COMPOSITE PROPERTY LINKAGE KEY values")
pc_missing_composite <- sum(is.na(df_pc_oh_final$`COMPOSITE PROPERTY LINKAGE KEY`))
ot_missing_composite <- sum(is.na(df_ot_oh$`COMPOSITE PROPERTY LINKAGE KEY`))
message("  PC missing COMPOSITE PROPERTY LINKAGE KEY: ", pc_missing_composite, " (", round(pc_missing_composite/nrow(df_pc_oh_final)*100, 2), "%)")
message("  OT missing COMPOSITE PROPERTY LINKAGE KEY: ", ot_missing_composite, " (", round(ot_missing_composite/nrow(df_ot_oh)*100, 2), "%)")
# PC missing COMPOSITE PROPERTY LINKAGE KEY: 0 (0%)
# OT missing COMPOSITE PROPERTY LINKAGE KEY: 1142 (0.02%)

# Check 3: COMPOSITE PROPERTY LINKAGE KEY uniqueness in each dataset
message("\nCheck 3: COMPOSITE PROPERTY LINKAGE KEY uniqueness")
pc_composite_dups <- df_pc_oh_final %>% 
  filter(!is.na(`COMPOSITE PROPERTY LINKAGE KEY`)) %>% 
  group_by(`COMPOSITE PROPERTY LINKAGE KEY`) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  filter(count > 1)
ot_composite_dups <- df_ot_oh %>% 
  filter(!is.na(`COMPOSITE PROPERTY LINKAGE KEY`)) %>% 
  group_by(`COMPOSITE PROPERTY LINKAGE KEY`) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  filter(count > 1)
message("  PC duplicated COMPOSITE PROPERTY LINKAGE KEY: ", nrow(pc_composite_dups), " IDs, ", sum(pc_composite_dups$count), " total obs")
message("  OT duplicated COMPOSITE PROPERTY LINKAGE KEY: ", nrow(ot_composite_dups), " IDs, ", sum(ot_composite_dups$count), " total obs")


# Check 4: COMPOSITE PROPERTY LINKAGE KEY overlap between datasets
message("\nCheck 4: COMPOSITE PROPERTY LINKAGE KEY overlap")
pc_composite <- df_pc_oh_final %>% filter(!is.na(`COMPOSITE PROPERTY LINKAGE KEY`)) %>% pull(`COMPOSITE PROPERTY LINKAGE KEY`) %>% unique()
ot_composite <- df_ot_oh %>% filter(!is.na(`COMPOSITE PROPERTY LINKAGE KEY`)) %>% pull(`COMPOSITE PROPERTY LINKAGE KEY`) %>% unique()
composite_in_both <- intersect(pc_composite, ot_composite)
composite_only_pc <- setdiff(pc_composite, ot_composite)
composite_only_ot <- setdiff(ot_composite, pc_composite)
message("  Unique COMPOSITE PROPERTY LINKAGE KEY in PC: ", length(pc_composite))
message("  Unique COMPOSITE PROPERTY LINKAGE KEY in OT: ", length(ot_composite))
message("  COMPOSITE PROPERTY LINKAGE KEY in both datasets: ", length(composite_in_both), " (", round(length(composite_in_both)/length(pc_composite)*100, 2), "% of PC)")
message("  COMPOSITE PROPERTY LINKAGE KEY only in PC: ", length(composite_only_pc))
message("  COMPOSITE PROPERTY LINKAGE KEY only in OT: ", length(composite_only_ot))


#==========================================================================#
# Merge Geocoded Property Characteristics with Owner Transfer Files
#==========================================================================#

# First, we will keep only necessary columns in both datasets.

# Columns to keep from PC
pc_cols_to_keep <- c("COMPOSITE PROPERTY LINKAGE KEY", "CLIP", "fips_id", "PROPERTY INDICATOR CODE", "OWNER 1 FULL NAME", "SITUS STREET ADDRESS", "OWNER OCCUPANCY CODE", "MAILING UNIT NUMBER", "MAILING STREET ADDRESS", "MAILING CITY", "MAILING STATE", "MAILING ZIP CODE", "TAX YEAR", "ASSESSED YEAR", "TAX AREA CODE", "HOMESTEAD EXEMPT INDICATOR", "TOTAL NUMBER OF ACRES", "TOTAL LAND SQUARE FOOTAGE", "YEAR BUILT", "TOTAL NUMBER OF BEDROOMS - ALL BUILDINGS", "TOTAL NUMBER OF BATHROOMS - ALL BUILDINGS", "TOTAL NUMBER OF BATHROOMS", "GARAGE TYPE CODE", "TOTAL NUMBER OF PARKING SPACES", "PARKING TYPE CODE", "TOTAL NUMBER OF UNITS - ALL BUILDINGS", "lat", "long", "cousub_namelsad", "cousub_geoid", "place_geoid", "place_namelsad", "place_lsad")

df_pc_oh_final_fp2 <- df_pc_oh_final_fp %>% select(all_of(pc_cols_to_keep)) %>% rename(CLIP_PC = `CLIP`)

# Columns to keep from OT
ot_cols_to_keep <- c("COMPOSITE PROPERTY LINKAGE KEY", "CLIP", "LAND USE CODE - STATIC", "STATE USE DESCRIPTION - STATIC", "ZONING CODE - STATIC", "PROPERTY INDICATOR CODE - STATIC", "ACTUAL YEAR BUILT - STATIC", "DEED SITUS STREET ADDRESS - STATIC", "DEED SITUS CITY - STATIC", "DEED SITUS STATE - STATIC", "PRIMARY CATEGORY CODE", "DEED CATEGORY TYPE CODE", "SALE AMOUNT", "SALE DERIVED DATE", "SALE DOCUMENT TYPE CODE", "SALE RECORDED DOCUMENT NUMBER", "RESIDENTIAL INDICATOR", "CASH PURCHASE INDICATOR", "MORTGAGE PURCHASE INDICATOR", "INTERFAMILY RELATED INDICATOR", "INVESTOR PURCHASE INDICATOR", "RESALE INDICATOR", "NEW CONSTRUCTION INDICATOR", "RESIDENTIAL INDICATOR", "FORECLOSURE REO INDICATOR", "FORECLOSURE REO SALE INDICATOR", "BUYER 1 FULL NAME", "BUYER OCCUPANCY CODE", "BUYER MAILING STREET ADDRESS", "BUYER MAILING CITY", "BUYER MAILING STATE", "BUYER MAILING ZIP CODE", "SELLER 1 FULL NAME")

df_ot_oh2 <- df_ot_oh %>% select(all_of(ot_cols_to_keep)) %>% rename(CLIP_OT = `CLIP`)

# Find common column names between the two datasets
common_cols <- intersect(colnames(df_ot_oh2), colnames(df_pc_oh_final_fp2))

message("\n=== Common Column Names ===")
message("Number of common columns: ", length(common_cols))
message("Common columns: ", paste(common_cols, collapse = ", "))

#--------------------------------------------#
# First join - COMPOSITE PROPERTY LINKAGE KEY
#--------------------------------------------#


# Perform left join of Owner Transfer with Property Characteristics on COMPOSITE PROPERTY LINKAGE KEY
df_ot_oh_merged <- df_ot_oh2 %>%
  left_join(df_pc_oh_final_fp2, by = "COMPOSITE PROPERTY LINKAGE KEY")

# View(df_ot_oh_merged[1:10000, ])
# nrow(df_ot_oh_merged)

sum(!is.na(df_ot_oh_merged$lat))  

sum(is.na(df_ot_oh_merged$lat))  


#--------------------------------------------#
# Second join - CLIP
#--------------------------------------------#


# First identify rows where lat is NA (meaning no match on COMPOSITE PROPERTY LINKAGE KEY)
unmatched_indices <- is.na(df_ot_oh_merged$lat)

# For these unmatched rows, try to fill in data from PC using CLIP_OT = CLIP_PC

# Create a lookup from PC data using CLIP_PC as key
pc_lookup <- df_pc_oh_final_fp %>% 
  select(all_of(pc_cols_to_keep)) %>%
  rename(CLIP_PC = CLIP) %>%
  # If there are duplicates on CLIP_PC, keep the first occurrence
  distinct(CLIP_PC, .keep_all = TRUE)

# For unmatched rows, try to join based on CLIP
df_ot_oh_merged <- df_ot_oh_merged %>%
  mutate(
    # Create temporary columns for the CLIP-based lookup
    across(
      .cols = all_of(setdiff(names(pc_lookup), c("CLIP_PC", "COMPOSITE PROPERTY LINKAGE KEY"))),
      .fns = ~{
        # Only fill NA values with CLIP-based match
        if_else(
          unmatched_indices & !is.na(CLIP_OT),
          pc_lookup[[cur_column()]][match(CLIP_OT, pc_lookup$CLIP_PC)],
          .x
        )
      },
      .names = "{.col}"
    )
  )


sum(!is.na(df_ot_oh_merged$lat))  # 3035450 geocoded addresses in Owner Transfer after merge.

sum(is.na(df_ot_oh_merged$lat))  # 2864659 not geocoded addresses in Owner Transfer after merge.

sum(is.na(df_ot_oh_merged$lat))/nrow(df_ot_oh_merged)  # 37% have no property characteristics after merge.

## some cleaning ## 
colnames(df_ot_oh_merged)

df_ot_oh_merged_sub <- df_ot_oh_merged %>%
  mutate(year = str_sub(`SALE DERIVED DATE`, 1, 4)) %>% 
  filter(!is.na(fips_id)) %>%
  relocate(year, .after = CLIP_OT) %>% relocate(fips_id, .after = CLIP_OT)

sort(unique(df_ot_oh_merged_sub$year))
View(df_ot_oh_merged_sub[1:100, ])

# Group by year and count observations
year_counts <- df_ot_oh_merged_sub %>%
  group_by(year) %>%
  summarise(count = n(), prop = count / nrow(.), .groups = "drop") %>%
  arrange(year)

message("\n=== Transaction Counts by Year ===")
print(year_counts, n = nrow(year_counts))

# output as CSV and Stata
readr::write_csv(df_ot_oh_merged, file.path(ot_out_loc, "corelogic_oh_full_24.csv"))
write_dta(df_ot_oh_merged, file.path(ot_out_loc, "corelogic_oh_full_24.dta"))


sdd_counts_by_year <- df_ot_oh %>%
  mutate(year = str_sub(`SALE DERIVED DATE`, 1, 4)) %>%
  group_by(year) %>%
  summarise(count = n(), prop = count / nrow(.), .groups = "drop") 

View(sdd_counts_by_year)

colnames(df_ot_oh)
