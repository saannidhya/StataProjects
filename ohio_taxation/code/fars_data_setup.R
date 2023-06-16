#================================================================================================================#
# Purpose : Merge accident.sas7bdat datasets by year from FARS into one dataset for Ohio only
# Name    : Saani Rawat
# Created : 06/14/2023
# Log     : 
#       06/14/2023: finished creating accident data for Ohio
#================================================================================================================#

library(utils)

print(data)


# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")


as.character(1980:2021)

# taking data from each year, extracting only Ohio, and then adding it to other years

# storing all fars df names as a list
accident_files <- list.files(paste0(data,"/fars"), pattern = "accident", recursive = TRUE)
accident_dataset_names <- paste0(stringr::str_remove(stringr::str_extract(fars_files, "(?<=/)[^/]+(?=\\.)"), ".sas7bdat"), 
                             "_", 
                             stringr::str_extract(fars_files, "\\d{4}"))


# import data
accident_dfs <- purrr::map(list.files(paste0(data,"/fars"),
                                     pattern = "accident",
                                     recursive = TRUE,
                                     full.names = TRUE),
                          haven::read_sas)

# assign names to fars dfs
accident_dfs <- stats::setNames(fars_dfs, fars_dataset_names)

length(accident_dfs)

# Note: Ohio's state number is 30. Hence, we extract only Ohio-related observations. Ohio seems to have ~ 1200 fatal crashes per year
oh_accident_dfs <- purrr::map(accident_dfs, ~ .x %>%
                               janitor::clean_names() %>%
                               filter(state == 39)
                               )


# longitude and latitude information was recorded starting 1999. So, we only take the main cols starting 1999.
cols_list <- c("state", "st_case", "year","ve_forms", "persons", "fatals", 
               "county", "city", "day", "month", "hour", "minute", "latitude", "longitud",
               "tway_id", "nhs", "route")
oh_accident_dfs_sub <- purrr::map(oh_accident_dfs[paste0("accident_",1999:2021)], ~ .x %>%
                                  select(all_of(cols_list)) %>%
                                  mutate(latitude = as.numeric(latitude), longitud = as.numeric(longitud))
                                    )



# creating one dataset which contains Ohio's accident data for all years, along with longitude and latitude information
# For years 1999 and 2000, the longitude and latitude information is unintelligible (coded as 88888888). Thus, we take years 2001 onwards.
oh_accident <- do.call(rbind, oh_accident_dfs_sub) %>% filter(year >= 2001) %>% filter(!(is.na(latitude) | is.na(longitud)))

oh_accident


# exporting this Ohio accident dataset. 
write.csv(oh_accident, file = paste0(data,"/fars/","oh_accident_2001-2021.csv"), row.names = FALSE)

# Next steps: using ArcGIS to use the geocoded crash level information and add county subdivision information 
# using a spatial join.


