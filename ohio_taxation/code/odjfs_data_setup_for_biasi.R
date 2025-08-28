#==========================================================================================================#
# Purpose : Data setup for additional levies as per Biasi, Lafortune and Schonholzer (2025)
#           Need "stacked data" that controls for levy history and future elections.    
# Name    : Saani Rawat
# Created : 04/22/2025
# Log     : 1. 04/22/2025: Created  the script
#           2. 05/07/2025: Added firms created and first destroyed variables
#           3. 08/25/2025: Fixed some aggregation bugs. Produced Biasi-formatted tables for BLS regressions 
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

# unique(emp_df2$meei)
# sort(unique(emp_df2$county_fips))
# colnames(emp_df2)
# emp_df2$county_fips <- substr(emp_df2$tendigit_fips, 3, 5)
# emp_df2 %>% group_by(meei) %>% count()

# View(emp_df2[1:100, ])

# nrow(emp_df) - nrow(emp_df2) # 118. 2020 dups.. expected.

# aggregate by tendigit_fips + year + quarter
setDT(emp_df2)

# Sort before using shift()
setorder(emp_df2, unique_id, year, quarter)
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

# prepare dataset for yearly aggregation
emp_df3 <- emp_df2[, .(num_employed = round(mean(persons, na.rm = TRUE)),
                        wage = sum(wage, na.rm = TRUE)), by = .(unique_id, tendigit_fips, year, naics)]

# Order for year-over-year flows
setorder(emp_df3, unique_id, year)

# Note: recreate jobs_created and jobs_destroyed to align periods, because we are aggregating to a different time period.
emp_df3[, `:=`(jobs_created   = pmax(num_employed - shift(num_employed, 1), 0),
               jobs_destroyed = pmax(shift(num_employed, 1) - num_employed, 0) ), by = unique_id]
emp_df3[, naics_2digit := floor(naics / 1e4)]
# View(emp_df3[1:1000, ])

# Step 2: Aggregate at (tendigit_fips, year)
emp_df_agg_yr <- emp_df3[, .(
  num_employed = round(sum(num_employed, na.rm = TRUE)),
  wage = sum(wage, na.rm = TRUE),
  jobs_created = sum(jobs_created, na.rm = TRUE),
  jobs_destroyed = sum(jobs_destroyed, na.rm = TRUE),
  num_firms = n_distinct(unique_id),
  num_employed_per_firm = round(sum(num_employed, na.rm = TRUE) / n_distinct(unique_id))
), by = .(tendigit_fips, year)]

# Step 3: Add wage per employee
# emp_df_agg_qtr[, wage_per_emp := round(wage / num_employed)]
emp_df_agg_yr[, `:=`( wage_per_emp = ifelse(num_employed == 0, 0, round(wage / num_employed)),
            job_creation_rate = ifelse(num_employed == 0, 0, round(jobs_created / num_employed)),
            job_destruction_rate = ifelse(num_employed == 0, 0, round(jobs_destroyed / num_employed)))]

# reasonable numbers: 5.5MM people employed in 2019, total wages 
emp_df_agg_yr %>% 
  group_by(year) %>%
  summarise(num_employed = sum(num_employed, na.rm = TRUE),
            wage = sum(wage, na.rm = TRUE),
            jobs_created = sum(jobs_created, na.rm = TRUE),
            jobs_destroyed = sum(jobs_destroyed, na.rm = TRUE),
            num_firms = sum(num_firms, na.rm = TRUE)
          )

length(unique(emp_df_agg_yr$tendigit_fips))
length(unique(emp_df_agg_yr$year))

# Check for unbalanced panel in emp_df_agg_yr
# panel_check <- emp_df_agg_yr %>%
#   group_by(tendigit_fips) %>%
#   summarise(
#     num_years = n(),
#     years_present = list(sort(year)),
#     .groups = 'drop'
#   ) %>%
#   filter(num_years < 15) %>%
#   arrange(num_years)
# panel_check

# summary(emp_df_agg_yr)

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
  # year
  agg_list[[i]] <- emp_df3[naics_2digit == naics_code, .(
    num_employed = round(sum(num_employed, na.rm = TRUE)),
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

# names(emp_df_agg_by_naics)
# View(emp_df_agg_by_naics)
# summary(emp_df_agg_by_naics)

# export to csv
write.csv(emp_df_agg_by_naics, paste0(data, "/employment/employment_data_agg_by_naics.csv"), row.names = FALSE)
write.csv(emp_df_agg_yr, paste0(data, "/employment/emp_df_agg_yr.csv"), row.names = FALSE)



#----------------------------------------------------------------------------------#
# Firms created and firms destroyed variables
#----------------------------------------------------------------------------------#

# get one row per firm-year
# firm_year <- emp_df2 %>% dplyr::as_tibble() %>% filter(year <= 2019) %>% # removing 2020 as COVID year is giving data problems
#   distinct(tendigit_fips, unique_id, year)  

# identify first and last year for each firm
# firm_span <- firm_year %>%                           
#   group_by(unique_id) %>%                            
#   summarise(first_year = min(year),
#             last_year  = max(year),
#             .groups     = "drop")

# min_sample_year <- min(firm_span$first_year, na.rm = TRUE)
# max_sample_year <- max(firm_span$last_year, na.rm = TRUE)

# # create new variables that indicates whether the firm was created or destroyed
# firm_year_flags <- firm_year %>% 
#                       left_join(firm_span, by = "unique_id") %>%         
#                       mutate(created   = as.integer(year == first_year & first_year > min_sample_year),
#                              destroyed = as.integer(year == last_year & last_year < max_sample_year))
                    
# firm_churn <- firm_year_flags %>% 
#   group_by(tendigit_fips, year) %>%                  
#   summarise(firms_created   = sum(created,   na.rm = TRUE),
#             firms_destroyed = sum(destroyed, na.rm = TRUE),
#             .groups          = "drop")

# # Shows that series are stable
# firm_churn_year <- firm_churn %>% 
#   group_by(year) %>% 
#   summarise(firms_created   = sum(firms_created,   na.rm = TRUE),
#             firms_destroyed = sum(firms_destroyed, na.rm = TRUE),
#             .groups          = "drop")



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
         select(tendigit_fips, year, subdivisionname, subdivisiontype, county, votes_pct_for, margin, margin_ds, taxtype, purpose2, description, millagepercent, duration, votesfor, votesagainst, votes_pct_for, votes_pct_against, hold_election, if_pass, margin, margin_ds,
                # census variables
                pop, medfamy, childpov, poverty, pctwithkids, pctsinparhhld, pctnokids, pctlesshs, pcthsgrad, pctsomecoll, pctbachelors, pctgraddeg, unemprate, pctrent, pctown, pctlt5, pct5to17, pct18to64, pct65pls, pctwhite, pctblack, pctamerind, pctapi, pctotherrace, pctmin, raceherfindahl, pcthisp, pctmarried, pctnevermarr, pctseparated, pctdivorced, lforcepartrate, incherfindahl
         ) %>% relocate(if_pass, .after = description) %>%
         filter(description == "A") # Biasi is for new levies only
# census <- haven::read_dta("data/roads_and_census.dta") %>% 
#   select(- all_of(c("taxtype", "purpose2", "description", "millagepercent", "duration", "votesfor", "votesagainst", "votes_pct_for", "votes_pct_against", "votesfor", "votesagainst", "duration", "votes_pct_for_cntr"))) %>% 
#   select(-starts_with("yr_t_"))
# 
# colnames(census)

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
  inner_join(roads %>% rename(cohort = year), by = c("tendigit_fips", "cohort"))  %>%
  # inner_join(roads %>% filter(description == "A") %>% rename(cohort = year), by = c("tendigit_fips", "cohort"))  %>%
  mutate(across(
    !c(tendigit_fips, year, cohort),  # all columns except the keys
    ~ if_else(year != cohort, NA, .)  # set to NA unless year == cohort
  )) %>%
  left_join(roads, by = c("tendigit_fips", "year"))
  # left_join(roads %>% filter(description == "A"), by = c("tendigit_fips", "year"))  
View(roads_panel)
# unique(roads$description)

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

roads_panel <- roads_panel %>% arrange(tendigit_fips, cohort, year)
roads_panel_flags <- roads_panel %>%
  dplyr::group_by(tendigit_fips, cohort) %>%
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

# View(roads_panel_flags)

# export to csv
# write_csv(roads_panel_flags, "data/roads_panel_flags.csv")

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
colnames(roads_emp_stacked)
View(roads_emp_stacked)

# unique(emp_df_agg_by_naics$naics_2digit)
roads_emp_stacked_by_naics <- purrr::map( unique(emp_df_agg_by_naics$naics_2digit), function(naics) {
                    roads_panel_flags %>%
                      inner_join(emp_df_agg_by_naics %>% filter(naics_2digit == naics) , by = c("tendigit_fips", "year")) %>%
                      mutate(naics_code = naics) %>%
                      relocate(naics_2digit, .after = year) %>%
                      filter(year >= cohort - 5 & year <= cohort + 10)
                    }) %>% bind_rows()

# roads_emp_stacked_by_naics <- roads_panel_flags %>%
#   inner_join(emp_df_agg_by_naics, by = c("tendigit_fips", "year"), relationship = "many-to-many") %>%
#   mutate(across(c(wage_per_emp, job_creation_rate, job_destruction_rate),
#                 ~ ifelse(is.nan(.) | is.infinite(.), 0, .) )) %>%
#   relocate(naics_2digit, .after = year) %>%
#   arrange(naics_2digit, tendigit_fips, cohort, year)

# View(roads_emp_stacked_by_naics)


# Only keeping "clean controls" i.e. controls that never passed an election in t+1 to t+10 i.e. # Remove control obs that have if_pass == 1 in years t+1 to t+10 after cohort. This will ensure that we only keep "clean controls", equivalent to the "never treated" group in Biasi.

# Aggregated
roads_emp_stacked2 <- roads_emp_stacked %>%
  group_by(tendigit_fips, cohort) %>%
  mutate(treated = if_else(any(cohort == year & if_pass == 1), 1, 0),
         control = if_else(any(cohort == year & if_pass == 0), 1, 0)) %>%
  relocate(treated, .after = if_pass) %>%
  relocate(control, .after = treated) %>%
  group_by(tendigit_fips, cohort) %>%
  mutate(future_pass = any(year > cohort & year <= cohort + 10 & if_pass == 1, na.rm = TRUE)) %>%
  filter(!(control == 1 & future_pass == TRUE)) %>%
  select(-c(treated, control, future_pass)) %>%
  ungroup()
# View(roads_emp_stacked2)


# By NAICS
roads_emp_stacked_by_naics2 <- roads_emp_stacked_by_naics %>%
  group_by(naics_2digit, tendigit_fips, cohort) %>%
  mutate(treated = if_else(any(cohort == year & if_pass == 1), 1, 0),
         control = if_else(any(cohort == year & if_pass == 0), 1, 0)) %>%
  relocate(treated, .after = if_pass) %>%
  relocate(control, .after = treated) %>%
  group_by(naics_2digit, tendigit_fips, cohort) %>%
  mutate(future_pass = any(year > cohort & year <= cohort + 10 & if_pass == 1, na.rm = TRUE)) %>%
  filter(!(control == 1 & future_pass == TRUE)) %>%
  select(-c(treated, control, future_pass)) %>%
  ungroup() 

nrow(roads_emp_stacked_by_naics2)
# View(roads_emp_stacked_by_naics2)

# export to csv
write_csv(roads_emp_stacked2, "data/employment/roads_emp_stacked.csv")
write_csv(roads_emp_stacked_by_naics2, "data/employment/roads_emp_stacked_by_naics.csv")

