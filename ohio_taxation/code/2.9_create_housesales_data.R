#=========================================================================================================================#
# Purpose : Create the cleaned house sales dataset, similar to housesales_9521_slim.dta (1995-2021)
# Name    : Saani Rawat
# Created : 01/18/2026
# Updated : 02/01/2026 - Made dynamic to support any year range (2000-2024 supported for CPI deflators)
# Log     : 
#        1. 01/08/2026: started the code. 
#        2. 02/01/2026: Added year_suffix support to make it flexible for different year ranges
# Dependencies: 
#        1.  1.6_geocode_corelogic_ot.R (for geocoding and merging the OT + PC data)
#
# Inputs:
#       1. corelogic_ownertransfer_propertycharacteristics_merged_oh_<<year_suffix>>.csv (from 1.6_geocode_corelogic_ot.R)
#
# Outputs:
#       1. housesales_<<year_suffix>>_slim.dta
#
# Note: CPI deflators are available for years 2000-2024. Adjust start_year/end_year as needed.
#=========================================================================================================================#



# Load necessary libraries
library(tidyverse)
library(tidycensus)
library(tidygeocoder)
library(tigris)
library(sf)
library(haven)
library(data.table)



# import loc
CoreLogic_loc <- "C:/CoreLogic"
# CoreLogic_loc <- "D:/2024_housing_data"

# out loc
ot_out_loc <- paste0(CoreLogic_loc,"/housing/OwnerTransfer/")
pc_out_loc <- paste0(CoreLogic_loc,"/housing/PropertyCharacteristics/")

# locations
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data_cl <- paste0(root, "/data/housing/CoreLogic")

# Year range configuration
# --------------------------------------------------
# NOTE: Adjust start_year and end_year as needed
#--------------------------------------------------- 
start_year <- 2016
end_year <- 2020
years_to_process <- as.character(start_year:end_year)
# Create year suffix for output filename (last 2 digits of start and end year)
year_suffix <- paste0(str_sub(as.character(start_year), -2), str_sub(as.character(end_year), -2))


# Import the joined OwnerTransfer + PropertyCharacteristics data for specified year range
df_ot_oh_merged <- readr::read_csv(file.path(data_cl, paste0("corelogic_ownertransfer_propertycharacteristics_merged_oh_", year_suffix, ".csv")), col_types = cols(.default = "c"))

#================================================================================#
# Cleaning the columns before appending to housesales_9521_slim.dta
#================================================================================#

colnames(df_ot_oh_merged)

# We need to:
# 1. deflate SALE AMOUNT to 2010 dollars using CPI
# 2. remove obs with number_of_units > 1 .. use --> `TOTAL NUMBER OF UNITS - ALL BUILDINGS`
# 3. drop if TRANSACTION_TYPE==9 (meaning non-arm's length transaction) .. use
# 4. drop if property_indicator == 21 (multifamily) .. use --> `PROPERTY INDICATOR CODE`
# 5. drop foreclosure, gift, interfamily deed, quit claim deed, sheriff’s deed
# drop if DOCUMENT_TYPE=="FD" | DOCUMENT_TYPE=="GF" | DOCUMENT_TYPE=="IF" | DOCUMENT_TYPE=="QC" | DOCUMENT_TYPE=="QJ" | DOCUMENT_TYPE=="SD" | DOCUMENT_TYPE=="TE"
# 6. create agehouse = year - year_built
# 7. AC variable:
# generate ac = 0
# replace ac = 1 if air_conditioning=="ACE" | air_conditioning =="AHT"| air_conditioning =="APK"
# replace ac = . if missing(air_conditioning)
# 8. finbase variable:
# generate finbase = 0
# replace finbase = 1 if basement_finish=="BFL" | basement_finish =="FCF"| basement_finish =="FFI" | basement_finish =="FFL" | basement_finish =="FFP"| basement_finish =="FPF"
# replace finbase = . if missing(basement_finish)
# generate basement = 0
# replace basement = 1 if basement_description=="CELLAR" | basement_description =="FINISHED"| basement_description =="FULL" | basement_description =="PARTIAL" | basement_description =="PARTIAL FINISHED"| basement_description =="SLAB" | basement_description =="UNFINISHED"| basement_description =="WALK OUT"
# replace basement = . if missing(basement_description)
# *there are 401,665 obs missing basement_description
# 9. create condition variables
# 10. create fireplace variable .. use --> `FIREPLACE INDICATOR`
# 11. create garage variable .. use --> `GARAGE TYPE CODE`
# 12. create pool variable .. use --> `POOL INDICATOR`
# 13. create onestory variable .. use --> `TOTAL NUMBER OF STORIES`
# 14. create splitlevel, twostory, threestoryplus variables .. use --> `TOTAL NUMBER OF STORIES`
# 15. create condo, colonial, log, modular, old, ranch, row, underground, english variables .. use --> `BUILDING STYLE TYPE CODE`
# 16. create oil, solar variables .. use --> `FUEL TYPE CODE`
# 17. create publicsewer, wellwater, privwater variables .. use --> `SEWER TYPE CODE`, `WATER TYPE CODE`


# df_ot_oh_merged <- df_ot_oh_merged %>%
#   filter(is.na(`TOTAL NUMBER OF UNITS - ALL BUILDINGS`) | 
#          as.numeric(`TOTAL NUMBER OF UNITS - ALL BUILDINGS`) <= 1)

# Sorted unique values for key CoreLogic variables (OT + PC merged)
vars_to_inspect <- c("TOTAL NUMBER OF UNITS - ALL BUILDINGS", "PRIMARY CATEGORY CODE", "DEED CATEGORY TYPE CODE", "PROPERTY INDICATOR CODE", "SALE DOCUMENT TYPE CODE", "AIR CONDITIONING TYPE CODE", "BASEMENT FINISH TYPE CODE", "BASEMENT TYPE CODE", "BUILDING IMPROVEMENT CONDITION CODE", "FIREPLACE INDICATOR", "GARAGE TYPE CODE", "POOL INDICATOR", "TOTAL NUMBER OF STORIES", "BUILDING STYLE TYPE CODE", "FUEL TYPE CODE", "SEWER TYPE CODE", "WATER TYPE CODE")

for (v in vars_to_inspect) {
  if (!v %in% names(df_ot_oh_merged)) {
    message("\n--- ", v, " (MISSING COLUMN) ---")
    next
  }
  vals <- sort(unique(df_ot_oh_merged[[v]]), na.last = TRUE)
  message("\n--- ", v, " (n=", length(vals), ") ---")
  print(vals)
}

# Note: The following line references a dataframe from 1.6_geocode_corelogic_ot.R
# and is commented out since df_ot_oh2 doesn't exist in this script's scope
# unique(df_ot_oh2$`SALE TYPE CODE`)

# sort(as.numeric(unique(df_ot_oh_merged$`TOTAL NUMBER OF UNITS - ALL BUILDINGS`)))

# housesales_9521_slim.dta has the following columns:

# NAMELSAD10
# NAMELSAD_1
# TENDIGIT_FIPS
# COUNTYFP10
# SALE_AMOUNT
# acres
# universal_built
# year_built
# total_rooms
# total_baths_calculated
# year
# agehouse
# ac
# basement
# cond_exc
# cond_fair
# cond_good
# cond_poor
# cond_vgood
# onestory
# condo

#==================================================================#
# Deflating SALE AMOUNT to 2010 dollars using CPI
# Note: We use this - https://data.bls.gov/cgi-bin/cpicalc.pl to 
# get the deflator multipliers
# January used as the month for all years. 2010 = 1.00 base year
#==================================================================#

# CPI deflators to 2010 dollars (base year = 2010)
# Source: BLS CPI-U Annual Averages
cpi_deflators <- tibble(
  year = as.character(2000:2024),
  deflator = c(
    1.28, # 2000
    1.24, # 2001
    1.22, # 2002
    1.19, # 2003
    1.17, # 2004
    1.14, # 2005
    1.09, # 2006
    1.07, # 2007
    1.03, # 2008
    1.03, # 2009
    1.00, # 2010
    0.98, # 2011
    0.96, # 2012
    0.94, # 2013
    0.93, # 2014
    0.93, # 2015
    0.91, # 2016
    0.89, # 2017
    0.87, # 2018
    0.86, # 2019
    0.84, # 2020
    0.83, # 2021
    0.77, # 2022
    0.72, # 2023
    0.70  # 2024
  )
)

df_ot_oh_merged2 <- df_ot_oh_merged %>%
  mutate(SALE_AMOUNT = as.numeric(`SALE AMOUNT`)) %>%
  left_join(cpi_deflators, by = "year") %>%
  mutate(SALE_AMOUNT = SALE_AMOUNT * deflator) %>%
  select(-`SALE AMOUNT`, -deflator)

# View(df_ot_oh_merged2[1:100, ])

#==============================================================#
# Creating the required variables for housesales_9521_slim.dta
#==============================================================#

# Helper: parse numeric safely (character columns are common in this pipeline)
num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))

# CoreLogic codes to mirror Stata lists
garage_attached_codes <- c(
  "020","060","061","080","110","112","113","114","120",
  "320","370","400","420","450","452","460","470",
  "610","720","780","790","800","B00"
)

finbase_codes <- c("BFL","FCF","FFI","FFL","FFP","FPF")
ac_codes <- c("ACE","AHT","APK")

df_ot_oh_merged3 <- df_ot_oh_merged2 %>%
  mutate(
    # Coalesce duplicated concepts (PC vs OT-static) where relevant
    property_indicator = coalesce(`PROPERTY INDICATOR CODE`, `PROPERTY INDICATOR CODE - STATIC`),
    year_num = num(year),
    year_built = coalesce(num(`YEAR BUILT`), num(`ACTUAL YEAR BUILT - STATIC`)),
    number_of_units = num(`TOTAL NUMBER OF UNITS - ALL BUILDINGS`),
    stories_number = num(`TOTAL NUMBER OF STORIES`),

    # keeping for now
    primary_category_code = `PRIMARY CATEGORY CODE`,
    document_type = `SALE DOCUMENT TYPE CODE`,

    # ----- Constructed variables (Stata-style dummies with missing -> NA) -----
    agehouse = if_else(!is.na(year_num) & !is.na(year_built), year_num - year_built, NA_real_),

    ac = case_when(
      is.na(`AIR CONDITIONING TYPE CODE`) ~ NA_real_,
      `AIR CONDITIONING TYPE CODE` %in% ac_codes ~ 1,
      TRUE ~ 0
    ),

    finbase = case_when(
      is.na(`BASEMENT FINISH TYPE CODE`) ~ NA_real_,
      `BASEMENT FINISH TYPE CODE` %in% finbase_codes ~ 1,
      TRUE ~ 0
    ),

    basement = case_when(
      is.na(`BASEMENT TYPE CODE`) ~ NA_real_,
      trimws(`BASEMENT TYPE CODE`) %in% c("000", "001") ~ 0, # need to check this because Dr. B uses basement_description, which I don't have
      TRUE ~ 1
    ),

    # condition dummies from BUILDING IMPROVEMENT CONDITION CODE
    cond_exc = case_when(
      is.na(`BUILDING IMPROVEMENT CONDITION CODE`) ~ NA_real_,
      `BUILDING IMPROVEMENT CONDITION CODE` %in% c("EXC", "GTE") ~ 1,
      TRUE ~ 0
    ),
    cond_fair = case_when(
      is.na(`BUILDING IMPROVEMENT CONDITION CODE`) ~ NA_real_,
      `BUILDING IMPROVEMENT CONDITION CODE` == "FAI" ~ 1,
      TRUE ~ 0
    ),
    cond_good = case_when(
      is.na(`BUILDING IMPROVEMENT CONDITION CODE`) ~ NA_real_,
      `BUILDING IMPROVEMENT CONDITION CODE` == "GOO" ~ 1,
      TRUE ~ 0
    ),
    cond_poor = case_when(
      is.na(`BUILDING IMPROVEMENT CONDITION CODE`) ~ NA_real_,
      `BUILDING IMPROVEMENT CONDITION CODE` %in% c("POO", "UNS") ~ 1,
      TRUE ~ 0
    ),
    cond_vgood = case_when(
      is.na(`BUILDING IMPROVEMENT CONDITION CODE`) ~ NA_real_,
      `BUILDING IMPROVEMENT CONDITION CODE` == "VGO" ~ 1,
      TRUE ~ 0
    ),

    fireplace = case_when(
      is.na(`FIREPLACE INDICATOR`) ~ NA_real_,
      `FIREPLACE INDICATOR` == "Y" ~ 1,
      TRUE ~ 0
    ),

    attgarage = case_when(
      is.na(`GARAGE TYPE CODE`) ~ NA_real_,
      `GARAGE TYPE CODE` %in% garage_attached_codes ~ 1,
      TRUE ~ 0
    ),

    pool = case_when(
      is.na(`POOL INDICATOR`) ~ NA_real_,
      `POOL INDICATOR` == "Y" ~ 1,
      TRUE ~ 0
    ),

    onestory = case_when(
      is.na(stories_number) ~ NA_real_,
      stories_number <= 1 ~ 1,
      TRUE ~ 0
    ),
    splitlevel = case_when(
      is.na(stories_number) ~ NA_real_,
      stories_number > 1 & stories_number < 2 ~ 1,
      TRUE ~ 0
    ),
    twostory = case_when(
      is.na(stories_number) ~ NA_real_,
      stories_number >= 2 & stories_number < 3 ~ 1,
      TRUE ~ 0
    ),
    threestoryplus = case_when(
      is.na(stories_number) ~ NA_real_,
      stories_number >= 3 ~ 1,
      TRUE ~ 0
    ),

    style = `BUILDING STYLE TYPE CODE`,
    bilevel = case_when(is.na(style) ~ NA_real_, style %in% c("BIL", "SPL") ~ 1, TRUE ~ 0),
    condo = case_when(is.na(style) ~ NA_real_, style %in% c("CON", "TWN") ~ 1, TRUE ~ 0),
    colonial = case_when(is.na(style) ~ NA_real_, style == "COL" ~ 1, TRUE ~ 0),
    log = case_when(is.na(style) ~ NA_real_, style == "LOG" ~ 1, TRUE ~ 0),
    modular = case_when(is.na(style) ~ NA_real_, style %in% c("MDR", "MOB") ~ 1, TRUE ~ 0),
    old = case_when(is.na(style) ~ NA_real_, style == "OLD" ~ 1, TRUE ~ 0),
    ranch = case_when(is.na(style) ~ NA_real_, style %in% c("RAN", "RRA") ~ 1, TRUE ~ 0),
    row = case_when(is.na(style) ~ NA_real_, style == "ROW" ~ 1, TRUE ~ 0),
    underground = case_when(is.na(style) ~ NA_real_, style == "UND" ~ 1, TRUE ~ 0),
    english = case_when(is.na(style) ~ NA_real_, style %in% c("GEO", "TUD", "VIC") ~ 1, TRUE ~ 0),

    fuel = `FUEL TYPE CODE`,
    woodcoal = case_when(is.na(fuel) ~ NA_real_, fuel %in% c("FCO", "FCW", "FWD") ~ 1, TRUE ~ 0),
    oil = case_when(is.na(fuel) ~ NA_real_, fuel %in% c("00I", "FKE", "FLP", "FOI") ~ 1, TRUE ~ 0),
    solar = case_when(is.na(fuel) ~ NA_real_, fuel == "FSO" ~ 1, TRUE ~ 0),

    sewer = `SEWER TYPE CODE`,
    water = `WATER TYPE CODE`,
    publicsewer = case_when(is.na(sewer) ~ NA_real_, sewer == "SPU" ~ 1, TRUE ~ 0),
    wellwater = case_when(is.na(water) ~ NA_real_, water %in% c("WCI", "WWE") ~ 1, TRUE ~ 0),
    privwater = case_when(is.na(water) ~ NA_real_, water == "WPR" ~ 1, TRUE ~ 0)
  ) %>%
  # --- Apply the drops ---
  filter(
    is.na(number_of_units) | number_of_units <= 1, # drop multifamily
    !(property_indicator == "21"), # drop multifamily
    !(document_type %in% c("FD", "GF", "IF", "QC", "QJ", "SD", "TE")) # drop certain document types
  ) %>%
  filter(`INTERFAMILY RELATED INDICATOR` == "0") # keep arms-length transactions only


# df_ot_oh_merged2 %>% group_by(`INTERFAMILY RELATED INDICATOR`) %>%
#   summarise(count = n() , prop = n() / nrow(.), .groups = "drop") %>%
#   arrange(desc(count)) %>%
#   print(n = Inf)


# View(df_ot_oh_merged3[1:100, ])
# colnames(df_ot_oh_merged3)

df_ot_oh_merged4 <- df_ot_oh_merged3 %>%
    mutate(NAMELSAD10  = place_namelsad, 
          NAMELSAD_1 = cousub_namelsad,
          FIPS_ID = fips_id, 
          SALE_AMOUNT = SALE_AMOUNT,
          acres = as.numeric(`TOTAL NUMBER OF ACRES`),
          universal_built = as.numeric(`UNIVERSAL BUILDING SQUARE FEET`),
          year_built = year_built, 
            total_rooms = as.numeric(`TOTAL NUMBER OF ROOMS - ALL BUILDINGS`),
            total_baths_calculated = coalesce(as.numeric(`TOTAL NUMBER OF FULL BATHS - ALL BUILDINGS`), as.numeric(`TOTAL NUMBER OF BATHROOMS - ALL BUILDINGS`)),
            year = year_num,
            agehouse = agehouse,
            ac = ac,
            basement = basement,
            cond_exc = cond_exc,
            cond_fair = cond_fair,
            cond_good = cond_good,
            cond_poor = cond_poor,
            cond_vgood = cond_vgood,
            onestory = onestory,
            condo = condo) %>%
    select(NAMELSAD10, NAMELSAD_1, FIPS_ID, SALE_AMOUNT, acres, universal_built, year_built, total_rooms, total_baths_calculated, year, agehouse, ac, basement, cond_exc, cond_fair, cond_good, cond_poor, cond_vgood, onestory, condo, lat, long) %>%
    rename(lon = long)

# Write the cleaned dataset to disk as Stata .dta file
haven::write_dta(df_ot_oh_merged4, file.path(data_cl, paste0("housesales_", year_suffix, "_slim.dta")))

# Import the cleaned dataset from disk
# housesales <- haven::read_dta(file.path(data_cl, paste0("housesales_", year_suffix, "_slim.dta")))

# colnames(housesales_2124)

# housesales_2124 %>% group_by(agehouse) %>%
#   summarise(count = n() , prop = n() / nrow(.), .groups = "drop") %>%
#   arrange(agehouse) %>%
#   print(n = Inf)

# housesales_2124 %>% group_by(year_built) %>%
#   summarise(count = n() , prop = n() / nrow(.), .groups = "drop") %>%
#   arrange(year_built) %>%
#   print(n = Inf)
