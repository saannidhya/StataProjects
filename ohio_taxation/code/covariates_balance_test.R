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
source(paste0(code,"/employment_data_setup.R"))

# importing roads and census dataset. Selecting only renewals and levies that do not last forever. Separating into treatment and control groups.
roads_and_census <- haven::read_dta(paste0(data,"/roads_and_census.dta")) %>%
  select(-matches("yr_t_")) %>%
  select(-c("inctaxrate")) %>%
  filter(description == "R" & duration != 1000) %>%
  janitor::clean_names() %>%
  mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0))    

#============================================================================================================#
#                        Aggregated housing datasets: Covariates Balance Test ----
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

# take all covariate names 
col_list <- dfs_agg_covs$housing_roads_census_t_plus_1_matches %>%
  select(-c(tendigit_fips,year,vote_year, tendigit_fips_year, votes_pct_for)) %>%
  colnames()


## global sample ##
all_tests <- purrr::map2(.x = dfs_agg_covs, .y = names(dfs_agg_covs), .f = function(df, df_name) {
  # dataset name
  dataset <- deparse(substitute(df))
  
  # perform t-test on one dataset at a time
  t_tests <- purrr::map(.x = col_list, .f = ~ t_test(df = df,
                                                     var = .x))  %>%
    bind_rows() %>%
    mutate(dataset = df_name)
    
    
  # # print t-test results from that dataset in a list
  return(t_tests)

}) %>% bind_rows()

# Output as a csv file
write.csv(all_tests, paste0(tables,"/covariates_balance_test_global.csv"))

## effective sample ##

dfs_agg_covs_cut <- purrr::map(dfs_agg_covs, ~ .x %>%
                                 filter(between(votes_pct_for, cutoff - 5, cutoff + 5)) )

all_tests_cut <- purrr::map2(.x = dfs_agg_covs_cut, .y = names(dfs_agg_covs_cut), .f = function(df, df_name) {
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
write.csv(all_tests, paste0(tables,"/covariates_balance_test_effective.csv"))

#============================================================================================================#
#     Covariate discontinuity test ----
#============================================================================================================#
# |- RD using covariates ####

covs_list <- col_list[!col_list %in% c('median_sale_amount', 'median_ln_sale_amount')]

regs_covs <- purrr::map( .x = dfs_agg_covs,
                         .f = function(df){
                          reg_covs <- purrr::map(.x = covs_list, 
                                      .f = function(var){
                                        rdrobust::rdrobust(y = df[[var]], 
                                                           x = df$votes_pct_for, 
                                                           c = cutoff, 
                                                           all = TRUE)
                                      })
                          names(reg_covs) <- covs_list
                          return(reg_covs)
                 })




#============================================================================================================#
#                        Aggregated employment datasets: Covariates Balance Test ----
#============================================================================================================#

# |- Mean of Treatment vs Control (all variables) ----
dfs_agg_emp_mean <- purrr::map(dfs_emp_agg3, function(df)
  df %>%
    select(-c(starts_with("yr_t_"))) %>%
    mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0)) %>%
    group_by(treated) %>%
    summarize(across(-c(tendigit_fips,year, tendigit_fips_year, votes_pct_for), 
                     ~mean(.x, na.rm=TRUE) ))
)

# T-test (all variables) ----

# take all covariate names 
col_list_emp <- dfs_emp_agg3$yr_t_plus_1 %>%
  select(-c(tendigit_fips,year,yr_t_plus_1, tendigit_fips_year, purpose2, tax_type, votes_for, votes_against, 
            votes_pct_for, description, millage_percent, duration, votes_pct_for_cntr, emp_flag)) %>%
  colnames()


## global sample ##
all_tests_emp <- purrr::map2(.x = dfs_emp_agg3 , 
                             .y = names(dfs_emp_agg3), 
                             .f = function(df, df_name) {
  # dataset name
  dataset <- deparse(substitute(df))
  
  # perform t-test on one dataset at a time
  t_tests <- purrr::map(.x = col_list_emp, .f = ~ t_test(df = df,
                                                     var = .x))  %>%
    bind_rows() %>%
    mutate(dataset = df_name) 
    
    
    # # print t-test results from that dataset in a list
    return(t_tests)
  
}) %>% bind_rows()  %>% mutate(significant = if_else(`p-value` < 0.05, "yes", "no"))


# Output as a csv file
write.csv(all_tests_emp, paste0(tables,"/covariates_balance_test_global_emp.csv"), row.names = FALSE)


## effective sample ##

dfs_emp_agg3_cut <- purrr::map(dfs_emp_agg3, ~ .x %>%
                                 filter(between(votes_pct_for, cutoff - 10, cutoff + 10)) )

all_tests_emp_cut <- purrr::map2(.x = dfs_emp_agg3_cut, .y = names(dfs_emp_agg3_cut), .f = function(df, df_name) {
  # dataset name
  dataset <- deparse(substitute(df))
  
  # perform t-test on one dataset at a time
  t_tests <- purrr::map(.x = col_list_emp, .f = ~ t_test(df = df,
                                                     var = .x))  %>%
    bind_rows() %>%
    mutate(dataset = df_name) %>%
    
    
    # # print t-test results from that dataset in a list
    return(t_tests)
  
}) %>% bind_rows() %>% mutate(significant = if_else(`p-value` < 0.05, "yes", "no"))

# Output as a csv file
write.csv(all_tests_emp_cut, paste0(tables,"/covariates_balance_test_effective_emp.csv"), row.names = FALSE)



#============================================================================================================#
#                        Balancing on the covariates in design phase (no outcome vars introduced) ----
#============================================================================================================#

#=======================================#
# Using matching techniques
#=======================================#
m.out <- MatchIt::matchit(formula = treated ~ pop + childpov + poverty + pctwithkids + pctsinparhhld + pctnokids + pctlesshs + 
                              pcthsgrad + pctsomecoll + pctbachelors + pctgraddeg + unemprate + medfamy + pctrent + pctown + 
                              pctlt5 + pct5to17 + pct18to64 + pct65pls + pctwhite + pctblack + pctamerind + pctapi + pctotherrace + 
                              pctmin + raceherfindahl + pcthisp + pctmarried + pctnevermarr + pctseparated + pctdivorced + lforcepartrate + incherfindahl,
                            data = roads_and_census, method = 'nearest', estimand = "ATT")

summary(m.out)
bal_tab = bal.tab(m.out, un = T)
love.plot(bal_tab, thresholds = 0.1)

nrow(m.out$X)

#=======================================#
# Using weighting techniques
#=======================================#

# using weightit and propensity score to balance covariates
w.out <- WeightIt::weightit(formula = treated ~ pop + childpov + poverty + pctwithkids + pctsinparhhld + pctnokids + pctlesshs + 
                   pcthsgrad + pctsomecoll + pctbachelors + pctgraddeg + unemprate + medfamy + pctrent + pctown + 
                   pctlt5 + pct5to17 + pct18to64 + pct65pls + pctwhite + pctblack + pctamerind + pctapi + pctotherrace + 
                   pctmin + raceherfindahl + pcthisp + pctmarried + pctnevermarr + pctseparated + pctdivorced + lforcepartrate + incherfindahl,
                 data = roads_and_census, method = 'ps', estimand = "ATE")
bal_tab = bal.tab(w.out, un = T)
love.plot(bal_tab, thresholds = 0.1)
summary(w.out)

weighted_roads_and_census <- roads_and_census %>% mutate(weights = w.out$weights)

# Need to take a subset of covariates and then do weighting based on those variables.


#=======================================#
# doing t-test on entire sample
#=======================================#
# data("lalonde", package = "cobalt") #If not yet loaded
# covs <- subset(lalonde, select = -c(treat, re78, nodegree, married))
# lalonde$p.score <- glm(treat ~ age + educ + race + re74 + re75,
#                        data = lalonde,
#                        family = "binomial")$fitted.values
# bal.tab(covs, treat = lalonde$treat)

roads_and_census_means <-   roads_and_census %>%
  mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0)) %>%
  group_by(treated) %>%
  summarize(across(-c(tendigit_fips, year, tendigit_fips_year, tax_type, purpose2, description, 
                      votes_pct_for, votes_pct_for_cntr, votes_against, votes_for), 
                   ~mean(.x, na.rm=TRUE) ))


t_tests <- purrr::map(.x = col_list[col_list != c("median_sale_amount", "median_ln_sale_amount", "inctaxrate")], 
                      .f = ~ t_test(df = roads_and_census, 
                                    var = .x)) %>% 
            bind_rows() 



covs_bal_tab <- bal.tab(roads_and_census %>% select(col_list[col_list != c("median_sale_amount", "median_ln_sale_amount", "inctaxrate")]),
                        treat = roads_and_census$treated)
love.plot(covs_bal_tab, thresholds = 0.1)

# th <- roads_and_census %>% filter(treated == 1) %>% select(pctlesshs) %>% pull() 
# 
# mean(th)/sd(th) - mean(tu)/sd(tu)
# 
# 
# tu <- roads_and_census %>% filter(treated == 0) %>% select(pctlesshs) %>% pull() 
# tu
# mean(tu)/sd(tu)
