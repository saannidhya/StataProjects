#==========================================================================================================#
# Purpose : ODJFS data setup for RDD 
#              
# Name    : Saani Rawat
# Created : 04/22/2025
# Log     : 1. Created  the script
#==========================================================================================================#

# Load necessary libraries
library(tidyverse)
library(rdrobust)


roads <- haven::read_dta("data/roads_and_census.dta") %>% 
            mutate(if_pass = if_else(votes_pct_for > 50, 1, 0),
                   margin = (votes_pct_for - 50)/100,
                   votes_pct_for = votes_pct_for/ 100,
                   votes_pct_against = votes_pct_against/100,
                   hold_election = 1,
                   margin_ds = margin*if_pass) %>% janitor::clean_names() %>%
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
                   yr_t_plus_10 = year + 10,
                   rd_flag = 1)  %>%
  select(tendigit_fips, year, starts_with("yr_"), everything()) %>% 
  arrange(tendigit_fips, year)


#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#
#                           Combining roads and Agg employment data: t-3 to t+10
#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#

yrs <- c(paste0("yr_t_minus_",as.character(1:3)), paste0("yr_t_plus_",as.character(0:10)))

ess <- purrr::map(yrs, ~ emp_df_agg_yr %>% as_tibble() %>%
                    mutate(across(c(wage_per_emp, job_creation_rate, job_destruction_rate),
                                  ~ ifelse(is.nan(.) | is.infinite(.), 0, .) )) %>%
                    janitor::clean_names() %>%
                    arrange(tendigit_fips, year) %>%
                    mutate(tendigit_fips = as.numeric(tendigit_fips),
                           emp_flag = 1) %>%
                    mutate({{.x}} := as.numeric(year)) %>%
                    select(-c(year))
)
names(ess) <- yrs


mgd_ess <- purrr::map2(ess, yrs, function(x, y){
  x %>% inner_join(roads, by = c("tendigit_fips", y)) %>% 
    relocate(year, .after = tendigit_fips) %>%
    relocate(y, .after= year) %>%
    mutate(across(c(num_employed, wage, wage_per_emp, jobs_created, jobs_destroyed),  
                  ~ log(. + 1), .names = "log_{col}") )  # take log of multiple variables at once
})
names(mgd_ess) <- yrs



#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#
#                           Combining roads and employment data by naics code: t-3 to t+10
#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#


ess_naics <- purrr::map(yrs, ~ emp_df_agg_by_naics %>% as_tibble() %>%
                    mutate(across(c(wage_per_emp, job_creation_rate, job_destruction_rate),
                                  ~ ifelse(is.nan(.) | is.infinite(.), 0, .) )) %>%
                    janitor::clean_names() %>%
                    arrange(tendigit_fips, naics_code, year) %>%
                    mutate(tendigit_fips = as.numeric(tendigit_fips),
                           emp_flag = 1) %>%
                    mutate({{.x}} := as.numeric(year)) %>%
                    select(-c(year))
)
names(ess_naics) <- yrs

mgd_ess_naics <- purrr::map2(ess_naics, yrs, function(x, y){
  x %>% inner_join(roads, by = c("tendigit_fips", y)) %>% 
    relocate(year, .after = tendigit_fips) %>%
    relocate(y, .after= year) %>%
    mutate(across(c(num_employed, wage, wage_per_emp, jobs_created, jobs_destroyed),  
                  ~ log(. + 1), .names = "log_{col}") ) 
})
names(mgd_ess_naics) <- yrs

#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#
#                   Running some prelim regressions
#------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#

colnames(mgd_ess$yr_t_plus_6)
 
# FULL #
mgd_ess_t <- map(mgd_ess, ~ dummy_cols(.x, select_columns = c("year"), remove_first_dummy = TRUE) %>% relocate(starts_with("year_"), .after = "year"))

## No covs ##
ess_reg <- purrr::map(mgd_ess_t, .f = function(y){
  rdrobust(  y = y$log_wage_per_emp,
             x = y$votes_pct_for,
             c = 0.5,
             # covs = y %>%
             #   select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = y$tendigit_fips)
})
tes_ess_reg <- te_tables(ess_reg)
plot_te(tes_ess_reg, title = "Treatment Effect Estimates")


## Covs ##

covs_ess_wpe <- purrr::map(mgd_ess_t , ~ find_covs(.x, 
                                               y = "log_wage_per_emp", 
                                               covs_list = covs_list, 
                                               run_var = "votes_pct_for", cutoff = 0.5  ))

mgd_ess_tfe_names <- map(mgd_ess_t, ~ colnames(.x) %>% grep("year_", ., value = TRUE))

covs_ess_wpe_tfe <- map2(covs_ess_wpe, mgd_ess_tfe_names, ~c(.x, .y))

ess_reg_covs <- purrr::map2(covs_ess_wpe_tfe, mgd_ess_t, .f = function(x,y){
  rdrobust(  y = y$log_wage_per_emp,
             x = y$votes_pct_for,
             c = 0.5,
             covs = y %>% select(x),
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = y$tendigit_fips)
})
tes_ess_reg_covs <- te_tables(ess_reg_covs)
plot_te(tes_ess_reg_covs, title = "Treatment Effect Estimates")






#> Notes from running test regressions:
#> 1. log_wage_per_emp seems to pick up starting year 5. Can s.e be decreased with covariates and weights?
#> 2. job_destruction_rate seems to be decreasing starting year 8

# BY NAICS #
# job_creation_rate, job_destruction_rate, num_employed, wage_per_emp, wage

colnames(mgd_ess_naics$yr_t_plus_0)
sort(unique(mgd_ess_naics$yr_t_plus_0$naics_2digit))


ess_naics_reg <- purrr::map(mgd_ess_naics2, .f = function(y){
  y <- y %>% filter(naics_2digit %in% c(48) ) 
  rdrobust(  y = y$num_employed,
             x = y$votes_pct_for,
             c = 0.5,
             # covs = y %>%
             #   select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = y$tendigit_fips)
})
tes_ess_naics_reg <- te_tables(ess_naics_reg)
plot_te(tes_ess_naics_reg, title = "Treatment Effect Estimates by NAICS")


#> Notes from running test regressions:
#> 1. num_employed in transporation (naics = 48) seem to be going up starting year 2. People hired for road work?
#> 2. log_num_employed in real estate (naics = 53) seems to be going up starting year 5. People getting hired in real estate as a result of better roads?
#> 3. 
#> 


# finding appropriate covariates
cvs_naics_48 <- purrr::map(map(mgd_ess_naics, ~ (.x %>% filter(naics_2digit %in% c(48) & (num_employed != 0) )  )) 
                           , ~find_covs(.x, y = "num_employed", covs_list = covs_list, run_var = "votes_pct_for", cutoff = 0.5))




ess_naics_reg48 <- purrr::map2(cvs_naics_48, mgd_ess_naics, .f = function(x,y){
  y <- y %>% filter(naics_2digit %in% c(48) ) 
  rdrobust(  y = y$num_employed,
             x = y$votes_pct_for,
             c = 0.5,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = y$tendigit_fips)
})
tes_ess_naics_reg48 <- te_tables(ess_naics_reg48)
plot_te(tes_ess_naics_reg48, title = "Treatment Effect Estimates by NAICS")