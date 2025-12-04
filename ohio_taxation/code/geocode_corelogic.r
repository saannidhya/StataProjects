#=========================================================================================================================#
# Purpose : Geocode + FIPS CoreLogic  University_of_Cincinnati_OwnerTransfer_v3_dpc_01549150_20240903_114649_data.txt
# Name    : Saani Rawat
# Created : 12/1/2025
# Log     : 
#        1. 12/1/2025: started the code. Ran on a sample.
#=========================================================================================================================#


# Load necessary libraries
library(tidyverse)
library(tidycensus)
library(tidygeocoder)

# import loc
CoreLogic_loc <- "C:/CoreLogic"

# out loc
ot_out_loc <- paste0(CoreLogic_loc,"/housing/OwnerTransfer/")
pc_out_loc <- paste0(CoreLogic_loc,"/housing/PropertyCharacteristics/")

#=========================================================================================================================#
# Owner Transfer Data
#=========================================================================================================================#

# Read the txt file
sample_data_ot <- read.table(
    file.path(CoreLogic_loc, "University_of_Cincinnati_OwnerTransfer_v3_dpc_01549150_20240903_114649_meta", "SampleRecords.txt"),
    header = TRUE,
    sep = "|",
    quote = "",
    fill = TRUE,
    stringsAsFactors = FALSE
)
# View(sample_data_ot)
sample_data_ot_clean <- sample_data_ot %>%
  mutate(
    street = str_squish(DEED.SITUS.STREET.ADDRESS...STATIC),
    city   = str_squish(DEED.SITUS.CITY...STATIC),
    state  = str_squish(DEED.SITUS.STATE...STATIC),
    zip5   = str_sub(DEED.SITUS.ZIP.CODE...STATIC, 1, 5)
  ) %>%
  # drop obviously unusable SITUS rows
  filter(
    !is.na(street),
    street != "",
    !str_detect(street, regex("^PO BOX", ignore_case = TRUE)),
    !str_detect(street, regex("INVALID NUMBER", ignore_case = TRUE))
  )
View(sample_data_ot_clean)

dd_ot <- read.table(
    file.path(CoreLogic_loc, "University_of_Cincinnati_OwnerTransfer_v3_dpc_01549150_20240903_114649_meta", "dd.txt"),
    header = TRUE,
    sep = "|",
    quote = "",
    fill = TRUE,
    stringsAsFactors = FALSE,
    comment.char = ""
)
View(dd_ot)

sample_data_ot_geo <- sample_data_ot_clean %>%
  geocode(
    street     = street,
    city       = city,
    state      = state,
    postalcode = zip5,
    method     = "census",
    full_results = TRUE,
    api_options = list(census_return_type = "geographies") # comes back with FIPS codes!!!
  )
# View(sample_data_ot_geo )

# Add FIPS codes
sample_data_ot_geo %>%
  mutate(
    county_geoid_5  = paste0(STATEFP, COUNTYFP),
    tract_geoid_11  = paste0(STATEFP, COUNTYFP, TRACTCE),      # names depend on output
    tendigit_fips = paste0(STATEFP, COUNTYFP, COUSUBFP)
  )

#=======================================#
## Full Data
#=======================================#

df_ot <- read.table(
    file.path(CoreLogic_loc, "University_of_Cincinnati_OwnerTransfer_v3_dpc_01549150_20240903_114649_data", "University_of_Cincinnati_OwnerTransfer_v3_dpc_01549150_20240903_114649_data.txt"),
    header = TRUE,
    sep = "|",
    quote = "",
    fill = TRUE,
    stringsAsFactors = FALSE,
    comment.char = ""
)

############ 1% Sample Geocoding ################


# Sample 1% of the rows
start_time <- Sys.time()

sample_size <- ceiling(0.01 * nrow(df_ot))
df_sampled_ot <- sample_n(df_ot, size = sample_size)

# nrow(df_sampled)

df2_ot <- df_sampled_ot %>%
  mutate(full_address = paste(Address, City, State, Zip, sep = ", "))

# Now use geocode()
# df_geocoded <- df2 %>%
#   geocode(addr = full_address, method = 'osm')
# df_geocoded %>% filter(!is.na(lat)) %>% nrow()

n <- nrow(df2_ot)
chunk_size <- 10000
chunks <- split(df2_ot, ceiling(seq_len(n)/chunk_size))

# Function to geocode each chunk
geocode_chunk <- function(chunk) {
  chunk %>%
    geocode(addr = full_address, method = 'census')
}

# df_geocoded_census <- df2 %>%
#   geocode(addr = full_address, method = 'census',
#           full_results = TRUE,
#           api_options = list(census_return_type = 'geographies'))

print("NOTE: running census geocoding now")
df_ot_geocoded <- map_df(chunks, geocode_chunk)
print("NOTE: Finished running census geocoding")


print("NOTE: Geocoded Dataset Info:")
df_ot_geocoded %>% nrow()
df_ot_geocoded %>% filter(!is.na(lat)) %>% nrow() / df_ot_geocoded %>% nrow() # proportion not geocoded: 88%

print("NOTE: Exporting to CSV file")
write.csv(df_ot_geocoded, paste0(ot_out_loc,"/corelogic_owner_transfer_1pct_sample.csv"), row.names = FALSE)

end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(paste("Run time:", elapsed_time))

#=========================================================================================================================#
# Property Characteristics Data
#=========================================================================================================================#

sample_data_pc <- read.table(
    file.path(CoreLogic_loc, "University_of_Cincinnati_hist_property3_dpc_01549149_15_20240903_174500_meta", "SampleRecords.txt"),
    header = TRUE,
    sep = "|",
    quote = "",
    fill = TRUE,
    stringsAsFactors = FALSE
)

View(sample_data_pc)

dd_pc <- read.table(
    file.path(CoreLogic_loc, "University_of_Cincinnati_hist_property3_dpc_01549149_15_20240903_174500_meta", "dd.txt"),
    header = TRUE,
    sep = "|",
    quote = "",
    fill = TRUE,
    stringsAsFactors = FALSE,
    comment.char = ""
)
View(dd_pc)


sample_data_pc_clean <- sample_data_pc %>%
  mutate(
    street = str_squish(SITUS.STREET.ADDRESS),
    city   = str_squish(SITUS.CITY),
    state  = str_squish(SITUS.STATE),
    zip5   = str_sub(SITUS.ZIP.CODE, 1, 5)
  ) %>%
  # drop obviously unusable SITUS rows
  filter(
    !is.na(street),
    street != "",
    !str_detect(street, regex("^PO BOX", ignore_case = TRUE)),
    !str_detect(street, regex("INVALID NUMBER", ignore_case = TRUE))
  )
View(sample_data_pc_clean)

sample_data_pc_geo <- sample_data_pc_clean %>%
  geocode(
    street     = street,
    city       = city,
    state      = state,
    postalcode = zip5,
    method     = "census",
    full_results = TRUE,
    api_options = list(census_return_type = "geographies") # comes back with FIPS codes!!!
  )

View(sample_data_pc_geo)

# sample_data_pc2[1:5,c("lat","long", "full_address")] %>% pull(lat)  %>% .[1] %>% format(, digits = 12)
# sample_data_pc2[1:5,c("lat","long", "full_address")] %>% pull(long)  %>% .[1] %>% format(, digits = 12)

# sample_data_pc2 %>% 
#     select(contains("SITUS"), "full_address") %>% View()



#=====================================================================#
# Property Characteristics - FULL GEOCODING (chunked)
#=====================================================================#


# Path to full Property Characteristics txt file
pc_data_file <- file.path(
  CoreLogic_loc,
  "University_of_Cincinnati_hist_property3_dpc_01549149_15_20240903_174500_data",
  "University_of_Cincinnati_hist_property3_dpc_01549149_15_20240903_174500_data.txt"
)

# Small helper to clean a PC chunk (same logic as your sample_data_pc_clean)
# NOTE: read_delim preserves spaces in column names, so use backticks
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

pc_full_outfile <- file.path(pc_out_loc, "corelogic_property_full_geocoded.csv")

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
}

# Run the full chunked geocoding
start_time_full <- Sys.time()
message("NOTE: Starting FULL PC geocoding (this may take a VERY long time)...")

readr::read_delim_chunked(
  file       = pc_data_file,
  delim      = "|",
  col_types  = cols(.default = "c"),
  chunk_size = 10000,   # smaller chunk to avoid overloading geocoder / RAM
  callback   = DataFrameCallback$new(process_pc_chunk_full),
  progress   = TRUE
)

end_time_full <- Sys.time()
elapsed_time_full <- end_time_full - start_time_full
message("NOTE: Finished FULL PC geocoding. Run time: ", elapsed_time_full)
message("Output written to: ", pc_full_outfile)


# Import the geocoded CSV file
corelogic_property_full_geocoded <- read_csv(pc_full_outfile)
View(corelogic_property_full_geocoded[1:1000, ])

# proportion geocoded by tidygeocoder: 0.9103692
corelogic_property_full_geocoded %>%
  filter(!is.na(lat)) %>%
  nrow() / corelogic_property_full_geocoded %>% nrow()  

# proportion geocoded by CoreLogic (PARCEL LEVEL LONGITUDE not NA): 0.08653028
sum(!is.na(corelogic_property_full_geocoded[["PARCEL LEVEL LONGITUDE"]]))/ nrow(corelogic_property_full_geocoded)  # 0.08653028

sum(!is.na(corelogic_property_full_geocoded[["BLOCK LEVEL LONGITUDE"]]))/ nrow(corelogic_property_full_geocoded)  # 0.9450272


#=====================================================================#
# Split Property Characteristics into State-Level Files
#=====================================================================#

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