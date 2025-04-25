#==========================================================================================================#
# Purpose : Data setup for additional levies as per Biasi, Lafortune and Schonholzer (2025)
#           Need "stacked data" that controls for levy history and future elections.    
# Name    : Saani Rawat
# Created : 04/22/2025
# Log     : 1. Created  the script
#==========================================================================================================#


library(tidyverse)
library(haven)
library(data.table)

# locations
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#
#>                            OUTCOME VARIABLES
#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------#
# fips_sub + year level wages and employment
#----------------------------------------------------------------------------------#

emp_df <- haven::read_dta("data/employment/employment_data_cleaned.dta") 

emp_df2 <- emp_df %>%
  mutate(persons = round(persons)) %>%
  relocate(unique_id, .before = year) %>%
  relocate(tendigit_fips, .after = unique_id) %>%
  distinct(unique_id, quarter, year, .keep_all = TRUE) %>%
  arrange(unique_id, year, quarter) 

# nrow(emp_df) - nrow(emp_df2) # 118. 2020 dups.. expected.

# aggregate by tendigit_fips + year + quarter
setDT(emp_df2)

emp_df2[, `:=`(jobs_created   = pmax(persons - shift(persons, 1), 0),
               jobs_destroyed = pmax(shift(persons, 1) - persons, 0) ), by = unique_id]
emp_df2[, naics_2digit := floor(naics / 1e4)]

# Step 1: Aggregate at (tendigit_fips, year, quarter)
emp_df_agg_qtr <- emp_df2[, .(
  num_employed = sum(round(persons), na.rm = TRUE),
  wage = sum(wage, na.rm = TRUE),
  jobs_created = sum(jobs_created, na.rm = TRUE),
  jobs_destroyed = sum(jobs_destroyed, na.rm = TRUE)
), by = .(tendigit_fips, year, quarter)]

# Step 2: Aggregate at (tendigit_fips, year)
emp_df_agg_yr <- emp_df_agg_qtr[, .(
  num_employed = round(mean(num_employed, na.rm = TRUE)),
  wage = sum(wage, na.rm = TRUE),
  jobs_created = sum(jobs_created, na.rm = TRUE),
  jobs_destroyed = sum(jobs_destroyed, na.rm = TRUE)
), by = .(tendigit_fips, year)]

# Step 3: Add wage per employee
# emp_df_agg_qtr[, wage_per_emp := round(wage / num_employed)]
emp_df_agg_yr[, `:=`( wage_per_emp = round(wage / num_employed),
                      job_creation_rate = round(jobs_created / num_employed, 3),
                      job_destruction_rate = round(jobs_destroyed / num_employed, 3))]

summary(emp_df_agg_yr)

length(unique(emp_df_agg_yr$tendigit_fips)) # 1600 fips


# Getting industry-specific employment data

# counting number of distinct unique_id per naics code
emp_df2 %>%
  group_by(naics_2digit) %>%
  summarise(num_unique_ids = n_distinct(unique_id)) %>%
  arrange(desc(num_unique_ids)) 

valid_naics <- unique(emp_df2[naics_2digit != 0, naics_2digit]) %>% sort

# list for naics
agg_list <- vector("list", length(valid_naics))
names(agg_list) <- valid_naics

# Loop through NAICS codes and compute aggregation
for (i in seq_along(valid_naics)) {
  naics_code <- valid_naics[i]
  
  # Subset and aggregate
  
  # qtr
  emp_df_agg_qtr_ <- emp_df2[naics_2digit == naics_code, .(
    num_employed = sum(round(persons), na.rm = TRUE),
    wage = sum(wage, na.rm = TRUE),
    jobs_created = sum(jobs_created, na.rm = TRUE),
    jobs_destroyed = sum(jobs_destroyed, na.rm = TRUE)
  ), by = .(naics_2digit, tendigit_fips, year, quarter)]
  # year
  agg_list[[i]] <- emp_df_agg_qtr_[naics_2digit == naics_code, .(
    num_employed = round(mean(num_employed, na.rm = TRUE)),
    wage = sum(wage, na.rm = TRUE),
    jobs_created = sum(jobs_created, na.rm = TRUE),
    jobs_destroyed = sum(jobs_destroyed, na.rm = TRUE)
  ), by = .(naics_2digit, tendigit_fips, year)]
}

# Combine all into one data.table
emp_df_agg_by_naics <- rbindlist(agg_list)


emp_df_agg_by_naics[, `:=`( wage_per_emp = round(wage / num_employed),
                      job_creation_rate = round(jobs_created / num_employed, 3),
                      job_destruction_rate = round(jobs_destroyed / num_employed, 3))]


# export to csv
write.csv(emp_df_agg_by_naics, paste0(data, "/employment/employment_data_agg_by_naics.csv"), row.names = FALSE)
write.csv(emp_df_agg_yr, paste0(data, "/employment/emp_df_agg_yr.csv"), row.names = FALSE)



#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#
#>                            Running Variable: Elections data
#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#


roads <- haven::read_dta("data/roads_and_census.dta") %>% 
  mutate(if_pass = if_else(votes_pct_for > 50, 1, 0),
         margin = (votes_pct_for - 50)/100,
         votes_pct_for = votes_pct_for/ 100,
         votes_pct_against = votes_pct_against/100,
         hold_election = 1,
         margin_ds = margin*if_pass) %>% janitor::clean_names() %>%
         select(tendigit_fips, year, subdivisionname, subdivisiontype, county, votes_pct_for, margin, margin_ds, taxtype, purpose2, description, millagepercent, duration, votesfor, votesagainst, votes_pct_for, votes_pct_against, hold_election, if_pass, margin, margin_ds)

# unique(roads$description)
# roads %>% filter(description == "A") %>% filter(between(year, 2006, 2019))
# 
# intersect(unique(roads$tendigit_fips), unique(emp_df_agg_by_naics$tendigit_fips))

#------------------------------------------------------------------------------------------------------------------#
# Stacked" Panel of elections data, each election is identified by tendigit_fips and cohort
#------------------------------------------------------------------------------------------------------------------#

roads_panel <- crossing(
  tendigit_fips = sort(unique(roads$tendigit_fips)),
  year = min(roads$year):max(roads$year), # year relative to election year
  cohort = sort(unique(roads$year)), # election year. All years in which an election was held
 ) %>% arrange(tendigit_fips, cohort, year) %>% 
  inner_join(roads %>% filter(description == "A") %>% rename(cohort = year), by = c("tendigit_fips", "cohort"))  %>%
  mutate(across(
    !c(tendigit_fips, year, cohort),  # all columns except the keys
    ~ if_else(year != cohort, NA, .)  # set to NA unless year == cohort
  )) %>%
  left_join(roads %>% filter(description == "A"), by = c("tendigit_fips", "year"))  

roads_panel <- roads_panel %>%
  mutate(across(
    ends_with(".x"),
    .fns = ~ coalesce(.x, get(sub(".x$", ".y", cur_column()))),
    .names = "{sub('.x$', '', .col)}"
  )) %>%
  select(
    -ends_with(".x"),
    -ends_with(".y")
  ) %>% 
  group_by(tendigit_fips, cohort)  %>%
  mutate(treat = as.integer(any(if_pass == 1 & year == cohort))) %>%
  ungroup() %>% 
  mutate(event_time = year - cohort) 

#------------------------------------------------------------------------------------------------------------------#
# Creating levy proposal (EL's) and levy authorization (DL's) flags MANUALLY
#------------------------------------------------------------------------------------------------------------------#

roads_panel_flags <- roads_panel %>%
  mutate(
    ### History variables ###
    EL_0 = if_else(hold_election == 1, 1, 0) |> replace_na(0),
    DL_0 = if_else(if_pass == 1, 1, 0) |> replace_na(0),
    ML_0 = if_else(hold_election == 1, margin, 0) |> replace_na(0),

    EL_1 = if_else(lag(hold_election) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL_1 = if_else(lag(if_pass) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML_1 = if_else(lag(hold_election) == 1 & !is.na(subdivisionname), lag(margin), 0) |> replace_na(0),

    EL_2 = if_else(lag(hold_election, 2) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL_2 = if_else(lag(if_pass, 2) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML_2 = if_else(lag(hold_election, 2) == 1 & !is.na(subdivisionname), lag(margin, 2), 0) |> replace_na(0),

    EL_3 = if_else(lag(hold_election, 3) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL_3 = if_else(lag(if_pass, 3) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML_3 = if_else(lag(hold_election, 3) == 1 & !is.na(subdivisionname), lag(margin, 3), 0) |> replace_na(0),

    EL_4 = if_else(lag(hold_election, 4) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL_4 = if_else(lag(if_pass, 4) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML_4 = if_else(lag(hold_election, 4) == 1 & !is.na(subdivisionname), lag(margin, 4), 0) |> replace_na(0),

    EL_5 = if_else(lag(hold_election, 5) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL_5 = if_else(lag(if_pass, 5) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML_5 = if_else(lag(hold_election, 5) == 1 & !is.na(subdivisionname), lag(margin, 5), 0) |> replace_na(0),

    EL_6 = if_else(lag(hold_election, 6) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL_6 = if_else(lag(if_pass, 6) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML_6 = if_else(lag(hold_election, 6) == 1 & !is.na(subdivisionname), lag(margin, 6), 0) |> replace_na(0),

    EL_7 = if_else(lag(hold_election, 7) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL_7 = if_else(lag(if_pass, 7) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML_7 = if_else(lag(hold_election, 7) == 1 & !is.na(subdivisionname), lag(margin, 7), 0) |> replace_na(0),

    EL_8 = if_else(lag(hold_election, 8) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL_8 = if_else(lag(if_pass, 8) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML_8 = if_else(lag(hold_election, 8) == 1 & !is.na(subdivisionname), lag(margin, 8), 0) |> replace_na(0),

    EL_9 = if_else(lag(hold_election, 9) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL_9 = if_else(lag(if_pass, 9) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML_9 = if_else(lag(hold_election, 9) == 1 & !is.na(subdivisionname), lag(margin, 9), 0) |> replace_na(0),

    EL_10 = if_else(lag(hold_election, 10) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL_10 = if_else(lag(if_pass, 10) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML_10 = if_else(lag(hold_election, 10) == 1 & !is.na(subdivisionname), lag(margin, 10), 0) |> replace_na(0),
    
    ### Future variables ###
    EL1 = if_else(lead(hold_election) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL1 = if_else(lead(if_pass) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML1 = if_else(lead(hold_election) == 1 & !is.na(subdivisionname), lead(margin), 0) |> replace_na(0),
    
    EL2 = if_else(lead(hold_election, 2) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL2 = if_else(lead(if_pass, 2) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML2 = if_else(lead(hold_election, 2) == 1 & !is.na(subdivisionname), lead(margin, 2), 0) |> replace_na(0),
    
    EL3 = if_else(lead(hold_election, 3) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL3 = if_else(lead(if_pass, 3) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML3 = if_else(lead(hold_election, 3) == 1 & !is.na(subdivisionname), lead(margin, 3), 0) |> replace_na(0),
    
    EL4 = if_else(lead(hold_election, 4) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL4 = if_else(lead(if_pass, 4) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML4 = if_else(lead(hold_election, 4) == 1 & !is.na(subdivisionname), lead(margin, 4), 0) |> replace_na(0),
    
    EL5 = if_else(lead(hold_election, 5) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL5 = if_else(lead(if_pass, 5) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML5 = if_else(lead(hold_election, 5) == 1 & !is.na(subdivisionname), lead(margin, 5), 0) |> replace_na(0),
    
    EL6 = if_else(lead(hold_election, 6) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL6 = if_else(lead(if_pass, 6) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML6 = if_else(lead(hold_election, 6) == 1 & !is.na(subdivisionname), lead(margin, 6), 0) |> replace_na(0),
    
    EL7 = if_else(lead(hold_election, 7) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL7 = if_else(lead(if_pass, 7) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML7 = if_else(lead(hold_election, 7) == 1 & !is.na(subdivisionname), lead(margin, 7), 0) |> replace_na(0),
    
    EL8 = if_else(lead(hold_election, 8) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL8 = if_else(lead(if_pass, 8) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML8 = if_else(lead(hold_election, 8) == 1 & !is.na(subdivisionname), lead(margin, 8), 0) |> replace_na(0),
    
    EL9 = if_else(lead(hold_election, 9) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL9 = if_else(lead(if_pass, 9) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML9 = if_else(lead(hold_election, 9) == 1 & !is.na(subdivisionname), lead(margin, 9), 0) |> replace_na(0),
    
    EL10 = if_else(lead(hold_election, 10) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    DL10 = if_else(lead(if_pass, 10) == 1 & !is.na(subdivisionname), 1, 0 ) |> replace_na(0),
    ML10 = if_else(lead(hold_election, 10) == 1 & !is.na(subdivisionname), lead(margin, 10), 0) |> replace_na(0)
    
  )

# export to csv
write_csv(roads_panel_flags, "data/roads_panel_flags.csv")

#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#
#                           Combining roads and employment data
#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#

# Merging roads_panel_flags with emp_df_agg

roads_emp_stacked <- roads_panel_flags %>%
  inner_join(emp_df_agg_yr, by = c("tendigit_fips", "year")) %>%
  mutate(across(c(wage_per_emp, job_creation_rate, job_destruction_rate),
        ~ ifelse(is.nan(.) | is.infinite(.), 0, .) ))


roads_emp_stacked_fips <- roads_panel_flags %>%
  inner_join(emp_df_agg_by_naics, by = c("tendigit_fips", "year")) %>%
  mutate(across(c(wage_per_emp, job_creation_rate, job_destruction_rate),
                ~ ifelse(is.nan(.) | is.infinite(.), 0, .) ))

# export to csv
write_csv(roads_emp_stacked, "data/employment/roads_emp_stacked.csv")
write_csv(roads_emp_stacked_fips, "data/employment/roads_emp_stacked_fips.csv")