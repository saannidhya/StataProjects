#==========================================================================================================#
# Purpose : Identify which covariates are balanced between treatment and control groups
# Name    : Saani Rawat
# Created : 07/27/2022
# Log     : 07/29/2022 - finished loop for covariate balance test for all datasets
#==========================================================================================================#


# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))

#============================================================================================================#
#                        Aggregated datasets: Covariates Balance Test ----
#============================================================================================================#

# |- Mean of Treatment vs Control (all variables) ----
dfs_agg_mean <- purrr::map(dfs_agg_covs, function(df)
                                         df %>%
                                         mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0)) %>%
                                         group_by(treated) %>%
                                         summarize(across(-c(tendigit_fips,year,vote_year, tendigit_fips_year, votes_pct_for), 
                                                          ~mean(.x, na.rm=TRUE) ))
                 )

# T-test function ----

t_test <- function(df, var) {
  
  # initializing
  dataset <- deparse(substitute(df))
  t_list <- list()
  
  # forming treatment and control groups for a variable
  treated_grp <- df %>% mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0)) %>% filter(treated == 1 ) %>% select(var)
  control_grp <- df %>% mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0)) %>% filter(treated == 0 ) %>% select(var)
  
  #performing t-test
  two_smp_test <- t.test(control_grp, treated_grp)
  
  # storing results
  t_list["dataset"] <- dataset
  t_list["variable"] <- var
  t_list["control group's mean"] <- two_smp_test$estimate[1]
  t_list["treatment group's mean"] <- two_smp_test$estimate[2]
  t_list["t-statistic"] <- two_smp_test$statistic
  t_list["std error"] <- two_smp_test$stderr
  t_list["p-value"] <- two_smp_test$p.value
  
  return(t_list)
  
}

# T-test (all variables) ----
col_list <- dfs_agg_covs$housing_roads_census_t_plus_1_matches %>%
  select(-c(tendigit_fips,year,vote_year, tendigit_fips_year, votes_pct_for)) %>%
  colnames()

all_tests <- purrr::map2(.x = dfs_agg_covs, .y = names(dfs_agg_covs), .f = function(df, df_name) {
  # dataset name
  dataset <- deparse(substitute(df))
  
  # perform t-test on one dataset at a time
  t_tests <- purrr::map(.x = col_list, .f = ~ t_test(df = df,
                                                     var = .x))  %>%
    bind_rows() %>%
    mutate(dataset = df_name) %>%
    
    
  # # print t-test results from that dataset in a list
  return(t_tests)

}) %>% bind_rows()

# Output as a csv file
write.csv(all_tests, paste0(tables,"/covariates_balance_test.csv"))
