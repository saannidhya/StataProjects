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
#==========================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects"
data_emp <- paste0(root,"/ohio_employment/data")
code_emp <- paste0(root,"/ohio_employment/code")
data_tax <- paste0(root,"/ohio_taxation/data")
code_tax <- paste0(root,"/ohio_taxation/code")

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
#  loading employment dataset (with geocoded addresses) ----
#================================================================#

employment_df <- haven::read_sas(paste0(data_emp,"/odjfs_employment_df.sas7bdat")) %>%
                  janitor::clean_names()  %>% 
                  mutate(persons = (m1 + m2 + m3)/3, 
                         unique_id = paste0(pad,uin,rep_unit))

# unique_id column has problems because when you paste the three cols, some unique_ids tend to overlap i.e. 0 + 10 + 1 = 0 + 1 + 10 = 0101 

employment_df %>% select(meei) %>% unique()
employment_df
#================================================================#
#  importing roads_and_census dataset ----
#================================================================# 
roads_and_census <- haven::read_dta(paste0(data_tax,"/roads_and_census.dta")) %>%
                      janitor::clean_names() 
  
#================================================================#
#  importing census only dataset ----
#================================================================# 
# covariates list 
vars_list <- c("TENDIGIT_FIPS", "year" ,"TENDIGIT_FIPS_year" ,"pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl", "inctaxrate")
# loading census df
census <- haven::read_dta(paste0(data,"/cosub_place_panel_property2_9018.dta")) %>%
  dplyr::select(vars_list) %>%
  janitor::clean_names()


#================================================================#
#  Data checks ----
#================================================================#
# df is your data frame
duplicate_rows <- employment_df[duplicated(employment_df[, c("year", "quarter", "pad", "uin", "rep_unit")]) | 
                       duplicated(employment_df[, c("year", "quarter", "pad", "uin", "rep_unit")], fromLast = TRUE),]

# print duplicate rows
# View(duplicate_rows)

# Note: for year 2019 and quarter 3 & 4, some of the observations were assigned two different geocodes (long and lat) by ArcGIS.
#       I do not know why this might be (54 observations)
# Update: This happened because master dataset from SAS was appending Q3 and Q4 for 2019 twice. This has been changed in SAS master dataset.
#         However, because ArcGIS geocoding was done on old dataset, odjfs_employment_df.sas7bdat contains these duplicates.
# How I handled this: looked at duplicates line by line for Q3, Q4 2019 and then checked if it had a corresponding uins
# in 2019Q2, 2019Q1 etc.

# employment_df %>% filter((uin == "0831615") & (rep_unit == "00155")) %>% View()
## need to remove the following observations:
#================================================================#
# 1.  uin == 1425962, rep == 00000, tendigit_fips == 3915326166
# 2.  uin == 1156401, rep == 00000, tendigit_fips == 3909955118
# 3.  uin == 0607666, rep == 00000, tendigit_fips == 3902940824
# 4.  uin == 0831615, rep == 00155, tendigit_fips == 3911381494
# 5.  uin == 1657015, rep == 00004, tendigit_fips == 3915901336 (no prev quarter to compare with, arbitarily chose 3915901336 to remove)
# 6.  uin == 1543034, rep == 00019, tendigit_fips == 3904541740
# 7.  uin == 0773502, rep == 00000, tendigit_fips == 3904981242
# 8.  uin == 1722768, rep == 00000, tendigit_fips == 3909344856  (no prev quarter to compare with, arbitarily chose 3909344856 to remove)
# 9.  uin == 0981827, rep == 00000, tendigit_fips == 3909372067
# 10. uin == 1316409, rep == 00000, tendigit_fips == 3904918000
# 11. uin == 1457637, rep == 00000, tendigit_fips == 3900551688
# 12. uin == 0957724, rep == 00000, tendigit_fips == 3904983342
# 13. uin == 1715275, rep == 00000, tendigit_fips == 3915508056
# 14. uin == 1478390, rep == 00000, tendigit_fips == 3901384602
# 15. uin == 0733251, rep == 00103, tendigit_fips == 3903571416
# 16. uin == 1438339, rep == 00000, tendigit_fips == 3914779674
# 17. uin == 0716484, rep == 00000, tendigit_fips == 3904983342
# 18. uin == 1248178, rep == 00000, tendigit_fips == 3915528098 
# 19. uin == 1471008, rep == 00000, tendigit_fips == 3915528098 
# 20. uin == 1688663, rep == 00000, tendigit_fips == 3903766656
# 21. uin == 1495014, rep == 00034, tendigit_fips == 3915580892
# 22. uin == 1457285, rep == 00000, tendigit_fips == 3903504500
# 23. uin == 1512191, rep == 00000, tendigit_fips == 3916521238
# 24. uin == 0726873, rep == 00122, tendigit_fips == 3903537240 (prev quarters were unreliable, arbitarily chose 3909344856 to remove)
# 25. uin == 0726873, rep == 00131, tendigit_fips == 3906115000
# 26. uin == 1495014, rep == 00003, tendigit_fips == 3905704220
# These observations were removed after checking year 2019 quarter 3,4 values for the specific uins with quarters 1, 2
#================================================================#


# Create a data frame with the above 26 conditions for removal
remove_df <- data.frame(
  uin = c("1425962", "1156401", "0607666", "0831615", "1657015", "1543034", "0773502", "1722768", "0981827", 
          "1316409", "1457637", "0957724", "1715275", "1478390", "0733251", "1438339", "0716484", "1248178", 
          "1471008", "1688663", "1495014", "1457285", "1512191", "0726873", "0726873", "1495014"),
  rep_unit = c('00000', '00000', '00000', '00155', '00004', '00019', '00000', '00000', '00000', 
          '00000', '00000', '00000', '00000', '00000', '00103', '00000', '00000', '00000', 
          '00000', '00000', '00034', '00000', '00000', '00122', '00131', '00003'),
  tendigit_fips = c(3915326166, 3909955118, 3902940824, 3911381494, 3915901336, 3904541740, 
                    3904981242, 3909344856, 3909372067, 3904918000, 3900551688, 3904983342, 
                    3915508056, 3901384602, 3903571416, 3914779674, 3904983342, 3915528098, 
                    3915528098, 3903766656, 3915580892, 3903504500, 3916521238, 3903537240, 
                    3906115000, 3905704220)
)

# Remove the duplicate rows
employment_df2 <- anti_join(employment_df, remove_df, by = c("uin", "rep_unit", "tendigit_fips"))


# num observations removed
nrow(employment_df) -  nrow(employment_df2)


# if we check duplicates now: 
# employment_df2[duplicated(employment_df2[, c("year", "quarter", "pad", "uin", "rep_unit")]) | 
#                 duplicated(employment_df2[, c("year", "quarter", "pad", "uin", "rep_unit")], fromLast = TRUE),]

# no duplicates left now based on year, quarter, pad, uin, rep_unit

# View(employment_df2)

# haven::write_dta(employment_df2, "H:/Julia/roads/odjfs/odjfs_employment_df2.dta")

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
            tot_persons = sum(persons, na.rm = TRUE))
# emp_df_agg_fips_yr_qtr %>% View()


# Aggregating further by year and quarter only for comparison with QCEW reports by ODJFS (see "emp_benchmarking" tab in excel sheet)
# emp_df_agg_yr_qtr <- emp_df_agg_fips_yr_qtr %>% 
#   group_by(year, quarter) %>%
#   summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
#             avg_persons = sum(tot_persons, na.rm = TRUE))

# Using emp_df_agg_fips_yr_qtr to aggregate by fips, year
emp_df_agg_fips_yr <- emp_df_agg_fips_yr_qtr %>% 
  group_by(tendigit_fips, year) %>%
  summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
            avg_persons = mean(tot_persons, na.rm = TRUE))


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

emp_2dg <- emp2 %>% left_join(naics_df, by = "naics_2dg")

# emp_2dg %>% filter(is.na(sector_title)) # 999999 is naics code for unclassified companies i.e. companies who have not been assigned a NAICS code yet. Nothing we can do about these companies.

# naics_df$naics_2dg

emp_by_indstry <- purrr::map(naics_df$naics_2dg, ~ emp_2dg %>% filter(naics_2dg == .x))
names(emp_by_indstry) <- naics_df$naics_2dg

emp_agg_by_indstry_qtr <- purrr::map(emp_by_indstry, ~ .x %>% 
                                   group_by(tendigit_fips, year, quarter, naics_2dg, sector_title) %>%
                                   summarise(tot_wages = sum(wage, na.rm = TRUE),
                                             avg_persons = sum(persons, na.rm = TRUE))
                                 )

emp_agg_by_indstry <- purrr::map(emp_agg_by_indstry_qtr, ~ .x %>% 
                                   group_by(tendigit_fips, year, naics_2dg, sector_title) %>%
                                   summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
                                             avg_persons = mean(avg_persons, na.rm = TRUE))
)

  
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
nrow(rd_fips) - nrow(rd_emp_fips %>% filter(!is.na(emp_fips_flg)))



#==============================================================#
# Merging employment data with census + roads voting data ----
#==============================================================#

# past and future years list
yrs <- c(paste0("yr_t_minus_",as.character(1:2)), paste0("yr_t_plus_",as.character(1:10)))

emps <- purrr::map(yrs, ~ emp_df_agg_fips_yr %>% 
                    arrange(tendigit_fips, year) %>%
                    mutate(emp_flag = 1) %>%
                    mutate({{.x}} := as.numeric(year)) %>%
                    select(-c(year))
)
names(emps) <- yrs


dfs_emp_agg <- purrr::map2(emps, yrs, function(x, y){
  x %>% inner_join(roads_and_census, by = c("tendigit_fips", y))
})

# purrr::map_dbl(dfs_emp_agg, nrow)
# purrr::map_dbl(dfs_agg, nrow)

# View(dfs_emp_agg$yr_t_plus_2)
# emp_df_agg_fips_yr %>% filter(tendigit_fips == 3900108350)
max(roads_and_census$year)   

#========================================#
# |- Transforming outcome variables ----
#========================================#
dfs_emp_agg2 <- purrr::map(dfs_emp_agg, ~ .x %>% 
                             mutate(ln_wages = log(tot_wages), ln_avg_persons = log(avg_persons)) %>%
                             filter(!(tot_wages == 0)) %>%
                             filter(!(avg_persons == 0))
)
dfs_emp_agg3 <- purrr::map2(names(dfs_emp_agg2), dfs_emp_agg2, ~ .y %>% select(-starts_with("yr_t_"), .x) %>% 
                              relocate(year, .after = tendigit_fips) %>% 
                              relocate(.x, .after = year) %>% 
                              ungroup()
)
names(dfs_emp_agg3) <- names(dfs_emp_agg2)


# exporting dfs_emp_agg as Stata datasets
purrr::map2(dfs_emp_agg3, names(dfs_emp_agg3), ~ haven::write_dta(.x, 
                                           path = paste0(data_tax,"/employment/dfs_emp_agg_", .y, ".dta")))


#========================================================#
# |- Creating employment/pop and wages/pop variables ----
#========================================================#
# need population data from census (most matched: 20,313 out of 23,876. Few were NAs due to missing population information)
emp_df_agg_fips_yr_per <- emp_df_agg_fips_yr %>% 
                              inner_join(census, by = c("tendigit_fips", "year")) %>%
                              mutate(wages_per_cap = tot_wages/pop, emp_per_cap = avg_persons/pop) %>% 
                              select(c("tendigit_fips", "year", "tot_wages", "avg_persons", "wages_per_cap", "emp_per_cap")) %>%
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
    select(-pop) %>% # do not want this as one of the covariates now since we are using it with outcomes
    mutate(ln_wages_per_cap = log(wages_per_cap), ln_emp_per_cap = log(emp_per_cap)) %>%
    filter(!(wages_per_cap == 0)) %>%
    filter(!(emp_per_cap == 0))
})


#====================================================================#
# |- Generating emp and wages vars for only specific NAICS codes ----
#====================================================================#

naics_2dg_unique <- sort(unique(emp2$naics_2dg))

naics_include <- c("11" = 1, "21" = 1, "22" = 0, "23" = 1, "31" = 1, "32" = 1, "33" = 1,
                   "42" = 1, "44" = 1, "45" = 1, "48" = 1, "49" = 1, "51" = 0, "52" = 0,
                   "53" = 0, "54" = 0, "55" = 0, "56" = 1, "61" = 0, "62" = 0, "71" = 1,
                   "72" = 1, "81" = 1, "92" = 1)

emp_by_naics_2dg_count <- emp2 %>%
                        group_by(naics_2dg) %>%
                        summarize(n = n(), .groups = "drop")

# Create a flag
emp2$include_flag <- ifelse(emp2$naics_2dg %in% names(naics_include[naics_include == 1]), 1, 0)

# Note: observation which have a code of "0" or "99" will be part of 0 i.e. industries that are not directly impacted by roads

# count by this flag
emp2 %>%
  group_by(include_flag) %>%
  summarize(n = n(), .groups = "drop")

emp_naics <- emp2 %>% 
  filter(include_flag == 1)




#=====================================================================#
#  Generating firm creation/destruction rates using ODJFS data ----
#=====================================================================#
df_sort_mth <- haven::read_sas(paste0(data_tax,"/employment","/df_sort_mth.sas7bdat"))

