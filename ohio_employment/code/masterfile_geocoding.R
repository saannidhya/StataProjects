#=========================================================================================================================#
# Purpose : Geocode masterfile_2006q1_2025q3.dta using Census batch geocoder via tidygeocoder,
#           then spatial-join with OH county subdivisions to assign TENDIGIT_FIPS.
# Name    : Saani Rawat
# Created : 02/24/2026
# Log     :
#        1. 02/24/2026: created. Geocodes unique addresses first, then merges back to full dataset.
#        2. 03/06/2026: running for 2025q3 update.
#        3. 06/14/2026: updated for 2025q4 update. masterfile_2006q1_2025q3.dta had zip column under pl_zip, which was leading to geocoding failure for year 2021. 
# Inputs  : masterfile_2006q1_2025q4.dta or .csv (if available)
# Outputs : masterfile_2006q1_2025q4_unique_addresses_geocoded.csv  (unique addresses geocoded)
#           masterfile_2006q1_2025q4_geocoded.csv                   (full dataset with lat/long + TENDIGIT_FIPS)
#
# Notes   :
#   - Uses haven::read_dta (faster than read_sas for this file).
#   - Geocodes only UNIQUE (address, city, state, zip) combos (~993K) rather than
#     every row (~9MM+), then merges results back. Saves substantial API time.
#   - Uses Census batch geocoder with retry logic (handles 502 / Bad Gateway).
#   - Writes geocoded chunks incrementally to CSV to avoid memory issues.
#   - TENDIGIT_FIPS assigned via sf spatial join with tigris county subdivisions.
#=========================================================================================================================#

library(tidyverse)
library(haven)
library(tidygeocoder)

# Force sf/tigris to use the PROJ/GDAL data shipped with the installed sf package.
# This avoids CRS lookup failures when external apps (e.g., GeoDa) set incompatible
# PROJ_LIB/GDAL_DATA environment variables globally.
sf_root <- system.file(package = "sf")
sf_proj <- file.path(sf_root, "proj")
sf_gdal <- file.path(sf_root, "gdal")
if (nzchar(sf_root) && dir.exists(sf_proj) && dir.exists(sf_gdal)) {
  Sys.setenv(
    PROJ_LIB  = sf_proj,
    PROJ_DATA = sf_proj,
    GDAL_DATA = sf_gdal
  )
}

library(sf)
library(tigris)

#=========================================================================================================================#
# Configuration
#=========================================================================================================================#

extracts_loc <- "C:/QCEW Data - Ohio/ES202/extracts"
out_loc      <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_employment/data"

masterfile_name <- "masterfile_2006q1_2025q4"

# Geocoding settings
CHUNK_SIZE   <- 2000L    # rows per Census batch request
BATCH_LIMIT  <- 2000L    # tidygeocoder batch_limit param
TIMEOUT_MIN  <- 60L      # timeout in minutes per batch
MAX_RETRIES  <- 5L       # max retries on failure per chunk
SLEEP_BETWEEN_CHUNKS <- 3  # seconds between chunks

#=========================================================================================================================#
# 1. Import masterfile
#=========================================================================================================================#

start_import <- Sys.time()

# Try csv first, then dta (whichever is available in the folder)
csv_path <- file.path(out_loc, paste0(masterfile_name, ".csv"))
dta_path <- file.path(out_loc, paste0(masterfile_name, ".dta"))
if (file.exists(csv_path)) {
  message("NOTE: Importing ", masterfile_name, ".csv ...")
  df_master <- readr::read_csv(csv_path, show_col_types = FALSE) %>% janitor::clean_names()
} else if (file.exists(dta_path)) {
  message("NOTE: Importing ", masterfile_name, ".dta ...")
  df_master <- haven::read_dta(dta_path) %>% janitor::clean_names()
} else {
  stop("No masterfile found: neither ", csv_path, " nor ", dta_path, " exists.")
}

message("NOTE: Imported ", format(nrow(df_master), big.mark = ","), " rows in ",
        round(difftime(Sys.time(), start_import, units = "mins"), 1), " minutes.")

# janitor::clean_names() already lowercases; tolower() call removed (redundant)

#=========================================================================================================================#
# 2. Extract & clean unique addresses
#=========================================================================================================================#

# Bad-address patterns (mirrors SAS cleaning in ohio_employment_data_extracts.sas)
bad_addresses <- c("**address needed**", "** address needed **", ".", "0", ",", "1", "'", "none", "no address provided")

df_unique <- df_master %>%
  distinct(address, city, state, zip) %>%
  mutate(
    address = str_squish(str_to_lower(address)),
    city    = str_squish(str_to_lower(city)),
    state   = str_squish(str_to_upper(state)),
    zip     = str_squish(as.character(zip))
  ) %>%
  filter(
    !is.na(address),
    address != "",
    !(address %in% bad_addresses),
    state == "OH",
    # Remove addresses with special characters that break geocoding / ArcGIS
    !str_detect(address, "\u2192"),            # arrow character
    !str_detect(address, "9988 mermill")        # known UTF-8 problem (4 obs)
  ) %>%
  distinct(address, city, state, zip) %>%
  mutate(addr_id = row_number())

message("NOTE: ", format(nrow(df_unique), big.mark = ","), " unique addresses to geocode.")

#=========================================================================================================================#
# 3. Robust Census geocoder with retry logic
#    (adapted from 1.6_geocode_corelogic_ot.R)
#=========================================================================================================================#

geocode_census_safe <- function(df, max_tries = MAX_RETRIES, timeout_min = TIMEOUT_MIN, batch_limit = BATCH_LIMIT) {

  for (attempt in seq_len(max_tries)) {

    res <- tryCatch(
      df %>%
        tidygeocoder::geocode(
          street     = address,
          city       = city,
          state      = state,
          postalcode = zip,
          method     = "census",
          mode       = "batch",
          full_results   = TRUE,
          api_options    = list(census_return_type = "geographies"),
          timeout        = timeout_min,
          batch_limit    = batch_limit
        ),
      error = function(e) e
    )

    # Hard failure (timeout, connection error): back off and retry
    if (inherits(res, "error")) {
      message("  Attempt ", attempt, "/", max_tries, " failed: ", conditionMessage(res))
      Sys.sleep(10 * attempt)
      next
    }

    # Validate: catch HTML / 502 "Bad Gateway" responses masquerading as results
    input_addr <- if ("input_address" %in% names(res)) res$input_address else ""
    bad_text   <- any(grepl("Bad Gateway|Census Geocoder while working as a gateway|<html|<p>",
                            input_addr, ignore.case = TRUE))

    too_many_missing <- !("match_indicator" %in% names(res)) || mean(is.na(res$match_indicator)) > 0.3

    if (!bad_text && !too_many_missing) return(res)

    # Corrupted response: wait and retry
    message("  Attempt ", attempt, "/", max_tries, " returned corrupted response. Retrying...")
    Sys.sleep(15 * attempt)
  }

  stop("Census geocoder failed after ", max_tries, " retries. Try again later or reduce batch_limit.")
}

#=========================================================================================================================#
# 4. Chunked geocoding with incremental CSV writes
#=========================================================================================================================#

outfile <- file.path(out_loc, paste0(masterfile_name, "_unique_addresses_geocoded.csv"))

total_rows  <- nrow(df_unique)
num_chunks  <- ceiling(total_rows / CHUNK_SIZE)

# Resume support: if outfile already exists, count rows already geocoded and skip those chunks
start_chunk <- 1L
if (file.exists(outfile) && file.size(outfile) > 0) {
  existing_lines <- length(readLines(outfile, warn = FALSE)) - 1L  # subtract header
  start_chunk    <- floor(existing_lines / CHUNK_SIZE) + 1L
  if (start_chunk > 1L) {
    message("NOTE: Found ", format(existing_lines, big.mark = ","),
            " existing rows in output. Resuming from chunk ", start_chunk, "/", num_chunks, ".")
  }
}

message("NOTE: Starting Census batch geocoding (",
        format(total_rows, big.mark = ","), " addresses in ", num_chunks, " chunks) ...")
start_geocode <- Sys.time()

for (i in seq(start_chunk, num_chunks)) {

  start_idx <- (i - 1L) * CHUNK_SIZE + 1L
  end_idx   <- min(i * CHUNK_SIZE, total_rows)

  message("  Chunk ", i, "/", num_chunks, " (rows ", format(start_idx, big.mark = ","),
          "-", format(end_idx, big.mark = ","), ")")

  chunk_df <- df_unique[start_idx:end_idx, ]

  geo_chunk <- geocode_census_safe(chunk_df)

  append_mode <- file.exists(outfile) && file.size(outfile) > 0
  readr::write_csv(geo_chunk, outfile, append = append_mode)

  rm(geo_chunk, chunk_df)
  gc()
  Sys.sleep(SLEEP_BETWEEN_CHUNKS)
}

elapsed_geocode <- round(difftime(Sys.time(), start_geocode, units = "mins"), 1)
message("NOTE: Finished geocoding in ", elapsed_geocode, " minutes.")
message("NOTE: Geocoded output written to: ", outfile)

#=========================================================================================================================#
# 5. Read back geocoded unique addresses & report match rate
#=========================================================================================================================#

df_geocoded <- readr::read_csv(outfile, col_types = cols(.default = "c")) %>%
  mutate(
    lat  = as.numeric(lat),
    long = as.numeric(long)
  )

match_rate <- round(sum(!is.na(df_geocoded$lat)) / nrow(df_geocoded) * 100, 1)
message("NOTE: Geocoding match rate: ", match_rate, "%")

df_geocoded %>%
  group_by(match_indicator) %>%
  summarise(count = n(), prop = round(n() / nrow(df_geocoded), 3), .groups = "drop") %>%
  arrange(desc(count)) %>%
  print(n = Inf)

#=========================================================================================================================#
# 6. Spatial join: assign COSUB_FIPS via OH county subdivisions
#=========================================================================================================================#

message("NOTE: Downloading Ohio county subdivision polygons ...")
options(tigris_use_cache = TRUE)

oh_cousub <- tigris::county_subdivisions(
  state = "OH",
  year = 2023,
  class = "sf",
  cb = TRUE
) %>%
  st_transform(4326) %>%
  select(
    cousub_geoid    = GEOID,
    cousub_name     = NAME,
    cousub_namelsad = NAMELSAD,
    cousub_lsad     = LSAD
  )

# Initialise result columns as NA
df_geocoded$cousub_fips     <- NA_character_
df_geocoded$cousub_namelsad <- NA_character_
df_geocoded$cousub_lsad     <- NA_character_

bb <- st_bbox(oh_cousub)
valid_idx <- which(
  !is.na(df_geocoded$long) & !is.na(df_geocoded$lat) &
  df_geocoded$long >= bb["xmin"] & df_geocoded$long <= bb["xmax"] &
  df_geocoded$lat  >= bb["ymin"] & df_geocoded$lat  <= bb["ymax"]
)

SJ_CHUNK   <- 500000L
idx_chunks <- split(valid_idx, ceiling(seq_along(valid_idx) / SJ_CHUNK))

message("NOTE: Running spatial join (", length(idx_chunks), " chunk(s)) ...")
start_sj <- Sys.time()

for (j in seq_along(idx_chunks)) {
  idx <- idx_chunks[[j]]
  pts <- st_as_sf(
    # Only lat/long + .row_id: avoids name collisions with oh_cousub columns
    # (cousub_namelsad, cousub_lsad) that would cause st_join to suffix them
    # .x/.y and make the back-assignment silently write NA.
    df_geocoded[idx, c("long", "lat")] %>% mutate(.row_id = idx),
    coords = c("long", "lat"),
    crs    = 4326,
    remove = FALSE
  )
  joined <- st_join(pts, oh_cousub, join = st_intersects, left = TRUE) %>%
    st_drop_geometry() %>%
    group_by(.row_id) %>%
    slice(1) %>%          # keep first match only (deduplicates one-to-many)
    ungroup()

  df_geocoded$cousub_fips[joined$.row_id]     <- joined$cousub_geoid
  df_geocoded$cousub_namelsad[joined$.row_id] <- joined$cousub_namelsad
  df_geocoded$cousub_lsad[joined$.row_id]     <- joined$cousub_lsad

  rm(pts, joined); gc()
}

elapsed_sj <- round(difftime(Sys.time(), start_sj, units = "mins"), 1)
message("NOTE: Spatial join completed in ", elapsed_sj, " minutes.")

fips_rate <- round(sum(!is.na(df_geocoded$cousub_fips)) / sum(!is.na(df_geocoded$lat)) * 100, 1)
message("NOTE: FIPS assignment rate (of geocoded rows): ", fips_rate, "%")

#=========================================================================================================================#
# 7. Spatial join: assign place_fips via OH places
#=========================================================================================================================#

message("NOTE: Downloading Ohio places polygons ...")

oh_places <- tigris::places(
  state = "OH",
  year  = 2023,
  class = "sf",
  cb    = TRUE
) %>%
  st_transform(4326) %>%
  select(place_fips = GEOID, place_lsad = LSAD, place_namelsad = NAMELSAD, geometry)

# Initialise result columns
df_geocoded$place_fips     <- NA_character_
df_geocoded$place_lsad     <- NA_character_
df_geocoded$place_namelsad <- NA_character_

bb_places <- st_bbox(oh_places)
valid_idx_places <- which(
  !is.na(df_geocoded$long) & !is.na(df_geocoded$lat) &
  df_geocoded$long >= bb_places["xmin"] & df_geocoded$long <= bb_places["xmax"] &
  df_geocoded$lat  >= bb_places["ymin"] & df_geocoded$lat  <= bb_places["ymax"]
)

idx_chunks_places <- split(valid_idx_places, ceiling(seq_along(valid_idx_places) / SJ_CHUNK))

message("NOTE: Running places spatial join (", length(idx_chunks_places), " chunk(s)) ...")
start_sj_places <- Sys.time()

for (j in seq_along(idx_chunks_places)) {
  idx <- idx_chunks_places[[j]]
  pts <- st_as_sf(
    # Same fix as Step 6: only lat/long + .row_id to avoid name collisions
    # with oh_places columns (place_fips, place_lsad, place_namelsad).
    df_geocoded[idx, c("long", "lat")] %>% mutate(.row_id = idx),
    coords = c("long", "lat"),
    crs    = 4326,
    remove = FALSE
  )
  joined <- st_join(pts, oh_places, join = st_intersects, left = TRUE) %>%
    st_drop_geometry() %>%
    group_by(.row_id) %>%
    slice(1) %>%
    ungroup()

  df_geocoded$place_fips[joined$.row_id]     <- joined$place_fips
  df_geocoded$place_lsad[joined$.row_id]     <- joined$place_lsad
  df_geocoded$place_namelsad[joined$.row_id] <- joined$place_namelsad

  rm(pts, joined); gc()
}

elapsed_sj_places <- round(difftime(Sys.time(), start_sj_places, units = "mins"), 1)
message("NOTE: Places spatial join completed in ", elapsed_sj_places, " minutes.")

place_fips_rate <- round(sum(!is.na(df_geocoded$place_fips)) / sum(!is.na(df_geocoded$lat)) * 100, 1)
message("NOTE: Place FIPS assignment rate (of geocoded rows): ", place_fips_rate, "%")


#=========================================================================================================================#
# 8. Merge geocoded unique addresses back to the full masterfile & create FIPS_ID
#=========================================================================================================================#

message("NOTE: Preparing merge keys for full masterfile ...")

# Normalise address fields in df_master to match the cleaned keys stored in df_geocoded
df_master_keyed <- df_master %>%
  mutate(
    .address_key = str_squish(str_to_lower(address)),
    .city_key    = str_squish(str_to_lower(city)),
    .state_key   = str_squish(str_to_upper(state)),
    .zip_key     = str_squish(as.character(zip))
  )

# Build a de-duplicated lookup from the geocoded unique addresses.
# df_geocoded still holds lat/long + cousub_fips + place_fips from Steps 5-7.
df_lookup <- df_geocoded %>%
  select(
    .address_key    = address,
    .city_key       = city,
    .state_key      = state,
    .zip_key        = zip,
    lat, long,
    match_indicator, match_type, matched_address,
    cousub_fips, cousub_namelsad, cousub_lsad,
    place_fips, place_lsad, place_namelsad
  ) %>%
  distinct(.address_key, .city_key, .state_key, .zip_key, .keep_all = TRUE)

message("NOTE: Merging geocoded lookup onto ", format(nrow(df_master), big.mark = ","),
        " rows in masterfile ...")
start_merge <- Sys.time()

df_final <- df_master_keyed %>%
  left_join(df_lookup,
            by = c(".address_key", ".city_key", ".state_key", ".zip_key")) %>%
  select(-.address_key, -.city_key, -.state_key, -.zip_key)

elapsed_merge <- round(difftime(Sys.time(), start_merge, units = "secs"), 1)

geo_coverage  <- round(sum(!is.na(df_final$lat)) / nrow(df_final) * 100, 1)

message("NOTE: Merge complete in ", elapsed_merge, " seconds.")
message("NOTE: Rows in final dataset : ", format(nrow(df_final), big.mark = ","))
message("NOTE: Geocoding coverage    : ", geo_coverage,  "% of all rows")

# Build final FIPS_ID using the same logic as 1.6_geocode_corelogic_ot.R:
#   - city (LSAD "25") or village (LSAD "47")  → use place_fips
#   - civil township (LSAD "44")                → use cousub_fips
#   - anything else                             → NA
df_final2 <- df_final %>%
  mutate(FIPS_ID = case_when(
    !is.na(place_fips)  & place_lsad  %in% c("25", "47") ~ place_fips,
    !is.na(cousub_fips) & cousub_lsad == "44"             ~ cousub_fips,
    TRUE                                                   ~ NA_character_
  )) %>% relocate(FIPS_ID) %>% rename(lon = long)

fips2_coverage <- round(sum(!is.na(df_final2$FIPS_ID)) / nrow(df_final2) * 100, 1)
message("NOTE: FIPS_ID coverage (case_when) : ", fips2_coverage, "% of all rows")

#=========================================================================================================================#
# 9. Export
#=========================================================================================================================#

final_outfile <- file.path(out_loc, paste0(masterfile_name, "_geocoded.csv"))
message("NOTE: Exporting final dataset to: ", final_outfile)
readr::write_csv(df_final2, final_outfile)
message("NOTE: Done. Output: ", final_outfile)

# convert to Stata .dta if needed (haven::write_dta can handle large files with v15 format)
final_dta_outfile <- file.path(out_loc, paste0(masterfile_name, "_geocoded.dta"))
message("NOTE: Exporting final dataset to Stata .dta: ", final_dta_outfile)
haven::write_dta(df_final2, final_dta_outfile)  
message("NOTE: Done. Output: ", final_dta_outfile)


message("NOTE: Creating matched dataset with non-missing FIPS_ID and ordering key vars")
matched_outfile <- file.path(out_loc, paste0(masterfile_name, "_geocoded_match.dta"))
df_match <- df_final2 %>% filter(!is.na(FIPS_ID)) 
message("NOTE: Exporting matched dataset to: ", matched_outfile)
haven::write_dta(df_match, matched_outfile)
message("NOTE: Done. Output: ", matched_outfile)

