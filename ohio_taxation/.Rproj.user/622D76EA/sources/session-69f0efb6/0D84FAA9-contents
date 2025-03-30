#================================================================================================================#
# Purpose : Geocode employment data provided by ODJFS
# Name    : Saani Rawat
# Created : 01/17/2024
# Log     : 
#        1. 01/17/2024: started the code. Ran on a sample.
#        2. 01/22/2024: Finished the geocoding run.
# Inputs : unique_addresses.csv
# Outputs: unique_addresses_census_geocoded.csv, unique_addresses_census_geocoded_0.01_pct_sample.csv
#================================================================================================================#

library(tidyverse)
library(tidygeocoder)

root = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_employment/data"

df = readr::read_csv(paste0(root, "/unique_addresses.csv")) %>% janitor::clean_names()

#=================================================================#
# Geocoding 0.01% sample
#=================================================================#

# Sample 1% of the rows
sample_size <- ceiling(0.01 * nrow(df))
df_sampled <- sample_n(df, size = sample_size)

df_sampled

df2 <- df_sampled %>%
  mutate(full_address = paste(address, city, state, zip, sep = ", "))

df_geocoded_census <- df2 %>%
  geocode(addr = full_address, method = 'census',
          full_results = TRUE,
          api_options = list(census_return_type = 'geographies'))

df_geocoded_census %>% nrow()
df_geocoded_census %>% filter(!is.na(lat)) %>% nrow() / df_geocoded_census %>% nrow()

write.csv(df_geocoded_census, paste0(root,"/unique_addresses_census_geocoded_0.01_pct_sample.csv"), row.names = FALSE)

#=================================================================#
# Geocoding Full dataset
#=================================================================#

start_time <- Sys.time()

# filtering addresses with 9988 mermill out since they contain characters that give UTF-8 encoding error
# Note: these are only 4 observations.
df2 <- df %>%
  mutate(full_address = paste(address, city, state, zip, sep = ", ")) %>% 
  filter(!(str_detect(full_address, "9988 mermill")))

n <- nrow(df2)
chunk_size <- 10000
chunks <- split(df2, ceiling(seq_len(n)/chunk_size))

# Function to geocode each chunk with error handling for each ()
geocode_chunk <- function(chunk) {
  chunk %>%
    geocode(addr = full_address, method = 'census',
            full_results = TRUE,
            api_options = list(census_return_type = 'geographies'))
}

print("NOTE: running census geocoding now")
df_s2_geocoded_census <- map_df(chunks, geocode_chunk)
print("NOTE: Finished running census geocoding")

print("NOTE: Exporting to CSV file")
write.csv(df_s2_geocoded_census, paste0(root,"/unique_addresses_census_geocoded.csv"), row.names = FALSE)

end_time <- Sys.time()

elapsed_time <- end_time - start_time
print(elapsed_time)
