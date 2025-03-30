#==========================================================================================================#
# Purpose : Employment Data setup before programs. Loads all employment datasets. Filters, cleans and aggregates datasets
# Name    : Saani Rawat
# Created : 07/03/2023
# Log     : 
#           07/03/2023: created the script. Finished for aggregate data.
#           07/14/2023: created datasets by industry
#           07/26/2023: adding code to create employment/pop and wage/pop data
#           08/13/2023: adding code to create firm creation and destruction rates
#           11/23/2023: added some comments. Commented some code out
#           05/02/2024: updated the code to directly load cleaned employment dataset (check git for prev versions)
#           05/03/2024: generated aggregate employment data by fips, year and quarter using cleaned employment dataset
#           05/14/2023: added code to deal with split cities in employment data
#==========================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects"
data_emp <- paste0(root,"/ohio_employment/data")
code_emp <- paste0(root,"/ohio_employment/code")
data_tax <- paste0(root,"/ohio_taxation/data")
code_tax <- paste0(root,"/ohio_taxation/code")
shared_odjfs <- "//cobshares.uccob.uc.edu/economics$/Julia/roads/odjfs"

# loading packages
packages <- c("Rbearcat", "tidyverse", "lubridate", "haven", "stringr", "here", "knitr", "janitor", "scales","data.table","rdrobust")
check_and_install <- function(pkg){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
lapply(packages, check_and_install)

#================================================================#
#  importing roads_and_census dataset ----
#================================================================# 
roads_and_census <- haven::read_dta(paste0(data_tax,"/roads_and_census.dta")) %>%
  select(-matches("yr_t_")) %>%
  filter(description == "R" & duration != "1000") %>%
  janitor::clean_names() %>%
  mutate(votes_pct_against = 100 - votes_pct_for) %>%
  mutate(treated = if_else(votes_pct_against > cutoff, 1, 0)) %>%
  mutate(yr_t_minus_3 = year - 3,
         yr_t_minus_2 = year - 2,
         yr_t_minus_1 = year - 1,
         yr_t_plus_0 = year,
         yr_t_plus_1 = year + 1,
         yr_t_plus_2 = year + 2,
         yr_t_plus_3 = year + 3,
         yr_t_plus_4 = year + 4,
         yr_t_plus_5 = year + 5,
         yr_t_plus_6 = year + 6,
         yr_t_plus_7 = year + 7,
         yr_t_plus_8 = year + 8,
         yr_t_plus_9 = year + 9,
         yr_t_plus_10 = year + 10) %>%
  select(tendigit_fips, year, starts_with("yr_"), everything()) %>% 
  arrange(tendigit_fips, year)


#================================================================#
#  importing census only dataset ----
#================================================================# 
# covariates list 
vars_list <- c("TENDIGIT_FIPS", "year"  ,"pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl")
# loading census df
census <- haven::read_dta(paste0(data_tax,"/census_property_9021.dta")) %>%
  # dplyr::select(vars_list) %>%
  janitor::clean_names()

#================================================================#
#  importing file to identify split cities ----
#================================================================#
split_cities <- readxl::read_excel(paste0(shared_odjfs,"/ohio-only-all-geocodes-2016.xlsx")) %>% janitor::clean_names() %>%
  filter(str_detect(`name_note_if_split_between_two_counties`, "(split)")) %>%
  mutate(placefp10 = str_pad(placefp10, width = 5, side = "left", pad = "0")) 

split_cities$placefp10

#================================================================#
#  loading cleaned employment dataset (with geocoded addresses) ----
#================================================================#

employment_df <- haven::read_dta(paste0(data_tax,"/employment/employment_data_cleaned.dta"))

# deflating using 2010 as base (numbers taken from Dr. Brasington's email)
employment_df2 <- employment_df %>%
  mutate(
    wage = case_when(
      year == 2020 ~ wage * 0.84,
      year == 2019 ~ wage * 0.86,
      year == 2018 ~ wage * 0.88,
      year == 2017 ~ wage * 0.90,
      year == 2016 ~ wage * 0.92,
      year == 2015 ~ wage * 0.94,
      year == 2014 ~ wage * 0.94,
      year == 2013 ~ wage * 0.95,
      year == 2012 ~ wage * 0.97,
      year == 2011 ~ wage * 0.99,
      year == 2010 ~ wage * 1.00,
      year == 2009 ~ wage * 1.04,
      year == 2008 ~ wage * 1.04,
      year == 2007 ~ wage * 1.08,
      year == 2006 ~ wage * 1.10,
      TRUE ~ wage  # Default case to handle any years not listed
    )
  )

# get the last 5 digits of tendigit_fips as placefp10
# employment_df3 <- employment_df2 %>% 
#   mutate(placefp10 = as.numeric(substr(as.character(tendigit_fips), 6, 10)))
# 
# employment_df3 %>% filter(placefp10 %in% split_cities$placefp10) %>% View()

# employment_df2 %>% # change to employment_df to see the original data
#   group_by(year) %>%
#   summarize(
#     mean = mean(wage, na.rm = TRUE),
#     median = median(wage, na.rm = TRUE),
#     min = min(wage, na.rm = TRUE),
#     max = max(wage, na.rm = TRUE),
#     q1 = quantile(wage, 0.25, na.rm = TRUE),  # First quartile
#     q2 = quantile(wage, 0.50, na.rm = TRUE),  # Second quartile, same as median
#     q3 = quantile(wage, 0.75, na.rm = TRUE),  # Third quartile
#     count = n(),
#     .groups = 'drop'  # This drops the grouping so the result is no longer grouped
#   )

  
#===============================================#
# Data Subsetting (take only relevant cols) ----
#===============================================#

emp <- employment_df2 %>% 
        select(c(year, quarter, pad, uin, rep_unit, tendigit_fips, ein, naics, wage, persons)) 

#============================================#
# Data Aggregation (Overall) ----
#============================================#

# aggregating by fips, year and quarter
emp_df_agg_fips_yr_qtr <- emp %>% 
  group_by(tendigit_fips, year, quarter) %>%
  summarise(tot_wages = sum(wage, na.rm = TRUE),
            tot_persons = sum(persons, na.rm = TRUE)) # adding avg employees in all the establishments
# emp_df_agg_fips_yr_qtr %>% View()


#=========================================================================================#
# dealing with split cities in employment data and assigning them same code as voting data
#=========================================================================================#

emp_df_agg_fips_yr_qtr2 <- emp_df_agg_fips_yr_qtr %>% 
  mutate(stfips = substr(as.character(tendigit_fips), 1, 2),
         cofips = substr(as.character(tendigit_fips), 3, 5),
         placefp10 = substr(as.character(tendigit_fips), 6, 10)) %>% 
  arrange(placefp10, cofips, year, quarter)

emp_df_agg_fips_yr_qtr3 <- emp_df_agg_fips_yr_qtr2 %>% left_join(split_cities %>% select(tendigit_fips, placefp10), by = "placefp10") %>%
  mutate(tendigit_fips = if_else(is.na(tendigit_fips.y), tendigit_fips.x, tendigit_fips.y)) %>%
  relocate(tendigit_fips, everything()) %>% select(-tendigit_fips.x, -tendigit_fips.y) %>% 
  group_by(tendigit_fips, year, quarter) %>%
  summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
            tot_persons = sum(tot_persons, na.rm = TRUE))

# emp_df_agg_fips_yr_qtr2 %>% left_join(split_cities, by = "placefp10") %>% filter(!is.na(name_note_if_split_between_two_counties)) %>% View()

# haven::write_dta(emp_df_agg_fips_yr_qtr3, path = paste0(data_tax,"/employment/emp_df_agg_fips_yr_qtr.dta"))

# Key note: for persons, whenever a time dimension collapses, we do avg, otherwise we do sum because persons are "stock", not "flow".


# Aggregating further by year and quarter only for comparison with QCEW reports by ODJFS (see "emp_benchmarking" tab in excel sheet)
# emp_df_agg_yr_qtr <- emp_df_agg_fips_yr_qtr %>% 
#   group_by(year, quarter) %>%
#   summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
#             avg_persons = sum(tot_persons, na.rm = TRUE))

# Using emp_df_agg_fips_yr_qtr to aggregate by fips, year
emp_df_agg_fips_yr <- emp_df_agg_fips_yr_qtr3 %>% 
  group_by(tendigit_fips, year) %>%
  summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
            avg_persons = round(mean(tot_persons, na.rm = TRUE)))

# haven::write_dta(emp_df_agg_fips_yr, path = paste0(data_tax,"/employment/emp_df_agg_fips_yr.dta"))



#============================================#
# Data Quality Check (Ohio Time series) ----
#============================================#
emp_df_agg_yr <- emp_df_agg_fips_yr %>% 
  ungroup() %>%
  group_by(year) %>%
  summarize(tot_wages = sum(tot_wages, na.rm = TRUE),
            avg_persons = round(sum(avg_persons, na.rm = TRUE))) %>% ungroup() %>%
  mutate(tot_wages_b = tot_wages/1000000000,
         avg_persons_mm = avg_persons/1000000) # converting to billions and millions

# generate a time series plot using ggplot with year in x axis and tot_wages and avg_persons in y axis
ggplot(data = emp_df_agg_yr) +
  theme_minimal() +
  geom_rect(aes(xmin = 2008, xmax = 2009, ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.8) +
  geom_rect(aes(xmin = 2019, xmax = 2021, ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.8) +
  geom_line(mapping = aes(x = year, y = tot_wages_b)) +
  labs(title = "Ohio Wages: 2006-2020", x = "Year", y = "Wages (in billions)")

ggplot(data = emp_df_agg_yr) +
  theme_minimal() +
  geom_rect(aes(xmin = 2008, xmax = 2009, ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.8) +
  geom_rect(aes(xmin = 2019, xmax = 2021, ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.8) +
  geom_line(mapping = aes(x = year, y = avg_persons_mm)) + 
  labs(title = "Ohio Employment: 2006-2020", x = "Year", y = "Employment (in millions)")
  
#============================================#
# Data Aggregation (by Industry) ----
#============================================#

emp2 <- emp %>%
  mutate(naics_2dg = as.numeric(substr(as.character(naics), 1, 2)))

# unique(emp2$naics_2dg) %>% sort()

# emp2 %>% filter(naics_2dg == 0) # 3 observations with naics code as 0. We can safely ignore them.

# creating NAICS look-up dataframe (created using NAIcS website: https://www.naics.com/search/)
naics_df <- data.frame(
  naics_2dg = c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 55, 56, 61, 62, 71, 72, 81, 92),
  `sector_title` = c("agriculture, forestry, fishing and hunting", "mining", "utilities", "construction", "manufacturing", "manufacturing", "manufacturing", "wholesale trade", "retail trade", "retail trade", "transportation and warehousing", "transportation and warehousing", "information", "finance and insurance", "real estate rental and leasing", "professional, scientific, and technical services", "management of companies and enterprises", "administrative and support and waste services", "educational services", "health care and social assistance", "arts, entertainment, and recreation", "accommodation and food services", "other services (except public administration)", "public administration")
)

emp_2dg <- emp2 %>% left_join(naics_df, by = "naics_2dg") %>% 
  arrange(tendigit_fips, year, quarter)

# emp_2dg %>% filter(is.na(sector_title)) # 999999 is naics code for unclassified companies i.e. companies who have not been assigned a NAICS code yet. Nothing we can do about these companies.

# naics_df$naics_2dg

emp_by_indstry <- purrr::map(naics_df$naics_2dg, ~ emp_2dg %>% filter(naics_2dg == .x))
names(emp_by_indstry) <- naics_df$naics_2dg

emp_agg_by_indstry_qtr <- purrr::map(emp_by_indstry, ~ .x %>% 
                                   group_by(tendigit_fips, year, quarter, naics_2dg, sector_title) %>%
                                   summarise(tot_wages = sum(wage, na.rm = TRUE),
                                             tot_persons = sum(persons, na.rm = TRUE)) # adding avg employees in all the establishments
                                 )


# dealing with split cities in employment data and assigning them same code as voting data

emp_agg_by_indstry_qtr2 <- purrr::map(emp_agg_by_indstry_qtr, ~ .x %>% 
                                              mutate(stfips = substr(as.character(tendigit_fips), 1, 2),
                                                     cofips = substr(as.character(tendigit_fips), 3, 5),
                                                     placefp10 = substr(as.character(tendigit_fips), 6, 10)) %>%
                                              arrange(placefp10, cofips, year, quarter) )

emp_agg_by_indstry_qtr3 <- purrr::map(emp_agg_by_indstry_qtr2, ~ .x %>% left_join(split_cities %>% select(tendigit_fips, placefp10), by = "placefp10") %>%
mutate(tendigit_fips = if_else(is.na(tendigit_fips.y), tendigit_fips.x, tendigit_fips.y)) %>%
  relocate(tendigit_fips, everything()) %>% select(-tendigit_fips.x, -tendigit_fips.y) %>% 
  group_by(tendigit_fips, year, quarter, naics_2dg, sector_title) %>%
  summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
            tot_persons = sum(tot_persons, na.rm = TRUE)) )

# map_dbl(emp_agg_by_indstry_qtr, nrow)   - map_dbl(emp_agg_by_indstry_qtr3, nrow)

emp_agg_by_indstry_yr <- purrr::map(emp_agg_by_indstry_qtr3, ~ .x %>% 
                                   group_by(tendigit_fips, year, naics_2dg, sector_title) %>%
                                   summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
                                             avg_persons = round(mean(tot_persons, na.rm = TRUE)))
)

# exporting industry-level employment datasets as Stata datasets
purrr::map2(emp_agg_by_indstry_yr, names(emp_agg_by_indstry_yr), ~ haven::write_dta(.x,
                                                                  path = paste0(data_tax,"/employment/industry/df_emp_", .y, ".dta")))
  
#==========================================================#
# Analysis on tendigit_fips ----
#==========================================================#

intersect(emp_df_agg_fips_yr$tendigit_fips %>% unique(), roads_and_census$tendigit_fips %>% unique())

setdiff(emp_df_agg_fips_yr$tendigit_fips %>% unique(), roads_and_census$tendigit_fips %>% unique())

rd_fips <- roads_and_census %>% select(tendigit_fips) %>% unique()

emp_fips <- emp_df_agg_fips_yr %>% select(tendigit_fips) %>% unique() %>%
  mutate(emp_fips_flg = 1)

rd_emp_fips <- rd_fips %>% 
                left_join(emp_fips, by = "tendigit_fips")

# total fips in roads_and_census df
nrow(rd_fips)
# total fips that we were able to match with employment df 
nrow(rd_emp_fips %>% filter(!is.na(emp_fips_flg)))
# total fips  that we were not able to match with employment df 
nrow(rd_fips) - nrow(rd_emp_fips %>% filter(!is.na(emp_fips_flg))) # need to take a note of these 192 FIPS codes


#==============================================================#
# Merging employment data with census + roads voting data ----
#==============================================================#

# past and future years list
yrs <- c(paste0("yr_t_minus_",as.character(1:3)), "yr_t_plus_0", paste0("yr_t_plus_",as.character(1:10)))

emps <- purrr::map(yrs, ~ emp_df_agg_fips_yr %>% 
                    arrange(tendigit_fips, year) %>%
                    mutate(emp_flag = 1) %>%
                    mutate({{.x}} := as.numeric(year)) %>%
                    select(-c(year))
)
names(emps) <- yrs
dfs_emp_agg <- purrr::map2(emps, yrs, function(x, y){
  
  x %>% inner_join(roads_and_census, by = c("tendigit_fips", y)) %>%
    relocate(tendigit_fips, year, everything()) %>% 
    select(-c(yrs[yrs != y], emp_flag)) %>%
    ungroup()
})

dfs_emp_ln_agg <- purrr::map(dfs_emp_agg, ~ .x %>%
                             mutate(ln_wages = log(tot_wages), ln_avg_persons = log(avg_persons)) %>%
                             filter(!(tot_wages == 0)) %>%
                             filter(!(avg_persons == 0))
)

# exporting dfs_emp_agg as Stata datasets
purrr::map2(dfs_emp_agg, names(dfs_emp_agg), ~ haven::write_dta(.x,
                                           path = paste0(data_tax,"/employment/dfs_emp_agg_", .y, ".dta")))
# exporting dfs_emp_ln_agg as Stata datasets
purrr::map2(dfs_emp_ln_agg, names(dfs_emp_ln_agg), ~ haven::write_dta(.x,
                                           path = paste0(data_tax,"/employment/dfs_emp_ln_agg_", .y, ".dta")))

#========================================================#
# |- Creating employment/pop and wages/pop variables ----
#========================================================#
# need population data from census (most matched: 20,313 out of 23,876. Few were NAs due to missing population information)
emp_df_agg_fips_yr_per <- emp_df_agg_fips_yr %>% 
                              inner_join(census, by = c("tendigit_fips", "year")) %>%
                              mutate(wages_per_cap = tot_wages/pop, emp_per_cap = avg_persons/pop, wages_by_per = tot_wages/avg_persons) %>% 
                              select(c("tendigit_fips", "year", "tot_wages", "pop", "avg_persons", "wages_per_cap", "emp_per_cap", "wages_by_per")) %>%
                              filter(!is.na(wages_per_cap)) %>% filter(!is.na(emp_per_cap))
# Need to run merge with t-2 to t+10 census data

# preparing outcome variables for merge with census and voting data
emps_per <- purrr::map(yrs, ~ emp_df_agg_fips_yr_per %>% 
                     arrange(tendigit_fips, year) %>%
                     mutate(emp_flag = 1) %>%
                     mutate({{.x}} := as.numeric(year)) %>%
                     select(-c(year))
)
names(emps_per) <- yrs


# merging outcome var with census and voting data
dfs_emp_agg_per <- purrr::map2(emps_per, yrs, function(x, y){
  x %>% inner_join(roads_and_census, by = c("tendigit_fips", y)) %>%
    # select(-pop) %>% # do not want this as one of the covariates now since we are using it with outcomes
    mutate(ln_wages_per_cap = log(wages_per_cap), ln_emp_per_cap = log(emp_per_cap)) %>%
    filter(!(wages_per_cap == 0)) %>%
    filter(!(emp_per_cap == 0))
})


