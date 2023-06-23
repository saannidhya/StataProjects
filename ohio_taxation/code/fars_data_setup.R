#================================================================================================================#
# Purpose : Merge accident.sas7bdat datasets by year from FARS into one dataset for Ohio only
# Name    : Saani Rawat
# Created : 06/14/2023
# Log     : 
#       06/14/2023: finished creating accident data for Ohio
#================================================================================================================#

library(utils)
library(tidyverse)

print(data)


# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")


#===========================================================================#
# FARS Accident Data Cleaning before spatial join ----
#===========================================================================#

# as.character(1980:2021)

# taking data from each year, extracting only Ohio, and then adding it to other years

# storing all fars df names as a list
accident_files <- list.files(paste0(data,"/fars"), pattern = "accident", recursive = TRUE)
accident_files2 <- accident_files[ !grepl("fars_arcgis", accident_files)]
accident_files3 <- accident_files2[!grepl("\\.csv$", accident_files2)]

accident_dataset_names <- paste0(stringr::str_remove(stringr::str_extract(accident_files3, "(?<=/)[^/]+(?=\\.)"), ".sas7bdat"), 
                             "_", 
                             stringr::str_extract(accident_files3, "\\d{4}"))

accident_full_files <- list.files(paste0(data,"/fars"),
                                                    pattern = "accident",
                                                    recursive = TRUE,
                                                    full.names = TRUE)
accident_full_files2 <- accident_full_files[ !grepl("fars_arcgis", accident_full_files)]
accident_dataset_fullnames <- accident_full_files2[!grepl("\\.csv$", accident_full_files2)]

# import data
accident_dfs <- purrr::map(accident_dataset_fullnames, haven::read_sas)

# assign names to fars dfs
accident_dfs <- stats::setNames(accident_dfs, accident_dataset_names)

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
# using a spatial join. See fars_arcgis project for details.


#===========================================================================#
# FARS Accident Data Aggregation after spatial join ----
#===========================================================================#

# importing Ohio accident dataset after the spatial join. This dataset contains GEOID = 10-digit FIPS code

fars_accident <- readr::read_csv(paste0(data,"/fars/","fars_arcgis/","oh_accident_2001_ExportTable.csv"))

fars_accident_cln <- fars_accident %>%
                        rename(tendigit_fips = GEOID) %>% 
                        select(tendigit_fips, year, everything()) %>%
                        arrange(tendigit_fips, year)


fars_accident_agg <- fars_accident_cln %>%
                              group_by(tendigit_fips, NAME, NAMELSAD, year) %>%
                              summarize(count = n(), 
                                        fatals_sum = sum(fatals), fatals_avg = mean(fatals),
                                        persons_sum = sum(persons), persons_avg = mean(persons ),
                                        veforms_sum = sum(ve_forms), ve_forms_avg = mean(ve_forms),
                              )


View(fars_accident_agg)  


#===========================================================================#
# comparing with roads tendigit fips ----
#===========================================================================#

fars_accident_agg %>% select(tendigit_fips, NAME, NAMELSAD)  %>% arrange(NAME, NAMELSAD) %>% unique() %>% write.csv(file = paste0(data,"/fars/", "fars_fips.csv"))

# rds <- haven::read_dta(paste0(data,"/roads_levies2_9118.dta"))
rds <- haven::read_dta(paste0(data,"/roads_levies2_census_9118.dta"))

rds %>% janitor::clean_names() %>% select(tendigit_fips, subdivision_name) %>% unique() %>% write.csv(file = paste0(data,"/fars/", "roads_fips.csv"))

# Based on excel comparison, 686 unique tendigit fips matched between roads and fars datasets, 216 did not match.

#===========================================================================#
# Joining fars dataset with roads dataset ----
#===========================================================================#
rd_var_list = c("year", "pop", "TENDIGIT_FIPS", "TENDIGIT_FIPS_year", "childpov", "poverty", "pctwithkids", "pctsinparhhld", "pctnokids", "pctlesshs", "pcthsgrad", 
                "pctsomecoll", "pctbachelors", "pctgraddeg", "unemprate", "medfamy", "pctrent", "pctown", "pctlt5", "pct5to17", "pct18to64", "pct65pls", "pctwhite", 
                "pctblack", "pctamerind", "pctapi", "pctotherrace", "pctmin", "raceherfindahl", "pcthisp", "pctmarried", "pctnevermarr", "pctseparated", "pctdivorced", 
                "lforcepartrate", "incherfindahl", "inctaxrate", "tax_type", "purpose2", "description", "millage_percent", "duration", "votes_for", "votes_against")

roads_and_census <- rds %>%
                      select(rd_var_list) %>% 
                      janitor::clean_names() %>%
                      mutate(votes_pct_for = (votes_for / (votes_for + votes_against))*100,
                             votes_pct_for_cntr = abs(votes_pct_for - cutoff)) %>%
                      group_by(tendigit_fips, year) %>% 
                      arrange(tendigit_fips, year, votes_pct_for_cntr) %>% 
                      mutate(count = row_number()) %>% 
                      filter(count == 1) %>% 
                      mutate(yr_t_minus_2 = year - 2, 
                             yr_t_minus_1 = year - 1,
                             yr_t_plus_1 = year + 1,
                             yr_t_plus_2 = year + 2,
                             yr_t_plus_3 = year + 3,
                             yr_t_plus_4 = year + 4,
                             yr_t_plus_5 = year + 5,
                             yr_t_plus_6 = year + 6,
                             yr_t_plus_7 = year + 7,
                             yr_t_plus_8 = year + 8,
                             yr_t_plus_9 = year + 9,
                             yr_t_plus_10 = year + 10,
                             rd_flag = 1) %>%
                      select(tendigit_fips, year, starts_with("yr_"), everything()) %>% 
                      arrange(tendigit_fips, year)

yrs <- c(paste0("yr_t_minus_",as.character(1:2)), paste0("yr_t_plus_",as.character(1:10)))


fars <- purrr::map(yrs, ~ fars_accident_agg %>% 
                    janitor::clean_names() %>%
                    arrange(tendigit_fips, year) %>%
                    mutate(fars_flag = 1) %>%
                    mutate({{.x}} := as.numeric(year)) %>%
                    select(-c(year)) %>%
                    select(tendigit_fips , .x, everything())
)
names(fars) <- yrs

fars_mgd <- purrr::map2(fars, yrs, function(x, y){
  x %>% inner_join(roads_and_census, by = c("tendigit_fips", y))
})

View(fars_mgd$yr_t_plus_10)

# Each observation in this dataset represents a year in which road tax levy voting took place, whether the levy passed or failed
# and the accident count in successive years afterwards

purrr::map_dbl(fars_mgd, nrow)  

View(fars_mgd$yr_t_plus_3)

