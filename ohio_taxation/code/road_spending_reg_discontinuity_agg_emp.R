#================================================================================================================#
# Purpose : Use wages and average employed persons as outcome of interest for Road spending and Taxes project
# Name    : Saani Rawat
# Created : 07/05/2023
# Log     : 
#        1. 07/05/2023: started the code
#        2. 07/14/2023: added outcome variable: employment/pop , wages/pop

#================================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")


# running data setup code
# source(paste0(code,"/employment_data_setup.R"))
source(paste0(code,"/utility_functions.R"))
# beepr::beep("mario")

#==============================================#
#  Importing employment datasets as a list ----
#==============================================#
# storing all employment dfs prepared for RD as a list
emp_dataset_names <- stringr::str_remove(list.files(paste0(data,"/employment"),
                                                    pattern = "dfs_emp_agg",
                                                    recursive = FALSE),
                                         paste0(".", "dta")) %>% gsub(pattern = "dfs_emp_agg_", replacement = "")
# import data
dfs_emp_agg <- purrr::map(list.files(paste0(data,"/employment"),
                                     pattern = "dfs_emp_agg",
                                     recursive = FALSE,
                                     full.names = TRUE),
                          haven::read_dta)
# assign names to housing dfs
dfs_emp_agg <- stats::setNames(dfs_emp_agg, emp_dataset_names)

## subsetting "R" (renewals) only
dfs_emp_agg_R <- purrr::map(dfs_emp_agg, ~ .x %>% filter(description == "R"))

## also converting outcome vars into log terms (remove 0 avg_persons obs since log function does not take 0 as input. This only removes 1 obs, and that too for yr_t_minus_3)
dfs_emp_agg_ln_R <- purrr::map(dfs_emp_agg, ~ .x %>% filter((description == "R") & (avg_persons != 0) ) %>% mutate(ln_tot_wages = log(tot_wages), ln_avg_persons = log(avg_persons)) ) 


# Also importing employment datasets by discovery
emp_by_ind_names <- stringr::str_remove(list.files(paste0(data,"/employment/industry"),
                                                    pattern = "df_emp_",
                                                    recursive = FALSE),
                                         paste0(".", "dta")) %>% gsub(pattern = "df_emp_", replacement = "")
# import data
dfs_emp_by_ind <- purrr::map(list.files(paste0(data,"/employment/industry"),
                                     pattern = "df_emp_",
                                     recursive = FALSE,
                                     full.names = TRUE),
                          haven::read_dta)
# assign names to housing dfs
dfs_emp_by_ind <- stats::setNames(dfs_emp_by_ind, emp_by_ind_names)


#========================================#
#  Manipulation test (X variable) ----
#========================================#
# dfs_emp_agg$yr_t_plus_2 %>% View()

dens_test <- rddensity::rddensity(X = roads_and_census$votes_pct_for, c = cutoff, massPoints = FALSE)
summary(dens_test)

dens_test$test$p_jk

dens_tests_emp <- purrr::map(dfs_emp_agg_R, ~ rddensity::rddensity(X = .x$votes_pct_for, c = cutoff))
summary(dens_tests_emp$yr_t_minus_1)
summary(dens_tests_emp$yr_t_minus_2)
summary(dens_tests_emp$yr_t_plus_1)
summary(dens_tests_emp$yr_t_plus_2)
summary(dens_tests_emp$yr_t_plus_3)
summary(dens_tests_emp$yr_t_plus_4)
summary(dens_tests_emp$yr_t_plus_5)
summary(dens_tests_emp$yr_t_plus_6)
summary(dens_tests_emp$yr_t_plus_7)
summary(dens_tests_emp$yr_t_plus_8)
summary(dens_tests_emp$yr_t_plus_9)
summary(dens_tests_emp$yr_t_plus_10)
# all passed!

# Mcrary test
# Give it the running variable and the cutpoint
# it will automatically produce a plot and select the number of bins and the bandwidth
# The output will be the p-value for the presence of a discontinuity

rdd::DCdensity(dfs_emp_agg_R$yr_t_plus_1$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg_R$yr_t_plus_2$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg_R$yr_t_plus_3$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg_R$yr_t_plus_4$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg_R$yr_t_plus_5$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg_R$yr_t_plus_6$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg_R$yr_t_plus_7$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg_R$yr_t_plus_8$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg_R$yr_t_plus_9$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg_R$yr_t_plus_10$votes_pct_for, c = 50)

#=========================#
#  RD plots ----
#=========================#

# Total wages
purrr::walk2(dfs_emp_agg_R, names(dfs_emp_agg_R), ~print(rdrobust::rdplot(y = .x$tot_wages, 
                                                             x = .x$votes_pct_for, 
                                                             c = 50, p = 1, title = .y)))

# Avg Number of persons employed
purrr::walk2(dfs_emp_agg_R, names(dfs_emp_agg_R), ~print(rdrobust::rdplot(y = .x$avg_persons, 
                                                                     x = .x$votes_pct_for, 
                                                                     c = 50, p = 1, title = .y)))

# Log of total wages
purrr::walk2(dfs_emp_agg_ln_R, names(dfs_emp_agg_ln_R), ~print(rdrobust::rdplot(y = .x$ln_tot_wages, 
                                                                     x = .x$votes_pct_for, 
                                                                     c = 50, p = 1, title = .y)))

# Avg Number of persons employed
purrr::walk2(dfs_emp_agg_ln_R, names(dfs_emp_agg_ln_R), ~print(rdrobust::rdplot(y = .x$ln_avg_persons, 
                                                                     x = .x$votes_pct_for, 
                                                                     c = 50, p = 1, title = .y)))


#====================================================#
# running regressions (aggregate) without covs ----
#====================================================#


## storing univariate RDD models (outcome vs running variable, no covariates) from t-2 to t+10

# tot wages
regs_emp_wages <- purrr::map(.x = dfs_emp_agg_R, ~ rdrobust::rdrobust(y = .x$tot_wages, x = .x$votes_pct_for, c = cutoff, all = TRUE))
# summary(regs_emp_wages$yr_t_plus_3)

# t.e table
te_emp_wages <- te_tables(regs_emp_wages)
plot_te(te_emp_wages)


# median(purrr::map_dbl(dfs_emp_agg3, ~ nrow(.x)))

# avg persons
regs_emp_persons <- purrr::map(.x = dfs_emp_agg_R, ~ rdrobust::rdrobust(y = .x$avg_persons, x = .x$votes_pct_for, c = cutoff, all = TRUE))

# t.e table
te_emp_pers <- te_tables(regs_emp_persons)
plot_te(te_emp_pers)

# summary(regs_emp_wages$yr_t_plus_6)

# log of tot wages
regs_emp_ln_wages <- purrr::map(.x = dfs_emp_agg_ln_R, ~ rdrobust::rdrobust(y = .x$ln_tot_wages, x = .x$votes_pct_for, c = cutoff, all = TRUE))
te_emp_ln_wages <- te_tables(regs_emp_ln_wages)
plot_te(te_emp_ln_wages)


regs_emp_ln_avg_persons <- purrr::map(.x = dfs_emp_agg_ln_R, ~ rdrobust::rdrobust(y = .x$ln_avg_persons, x = .x$votes_pct_for, c = cutoff, all = TRUE))
te_emp_ln_avg_persons <- te_tables(regs_emp_ln_avg_persons)
plot_te(te_emp_ln_avg_persons)


#=========================================#
# finding covariates ----
#=========================================#


# take all covariate names 
covs_list_emp <- dfs_emp_agg_R$yr_t_plus_1 %>%
  select(-c(tendigit_fips,year,yr_t_plus_1, tendigit_fips_year, purpose2, tax_type, votes_for, votes_against, 
            votes_pct_for, description, millage_percent, duration, votes_pct_for_cntr, emp_flag, 
            tot_wages, avg_persons, inctaxrate)) %>%
  colnames()

# finding covariates
covs_final_emp_ln_pers <- purrr::map(dfs_emp_agg_ln_R, ~find_covs(.x, y = "ln_avg_persons", covs_list = covs_list_emp))
covs_final_emp_pers <- purrr::map(dfs_emp_agg_R, ~find_covs(.x, y = "avg_persons", covs_list = covs_list_emp))
covs_final_emp_ln_wages <- purrr::map(dfs_emp_agg_ln_R, ~find_covs(.x, y = "ln_tot_wages", covs_list = covs_list_emp))
covs_final_emp_wages <- purrr::map(dfs_emp_agg_R, ~find_covs(.x, y = "tot_wages", covs_list = covs_list_emp))

## finding covariates with the right signs
covs_final_emp_ln_pers_s <- purrr::map(dfs_emp_agg_ln_R, ~find_covs_sign(.x, y = "ln_avg_persons", covs_list = covs_list_emp, sign = "positive"))
covs_final_emp_ln_wages_s <- purrr::map(dfs_emp_agg_ln_R, ~find_covs_sign(.x, y = "ln_tot_wages", covs_list = covs_list_emp, sign = "positive"))

#-----------------------#
# log average persons   #
#-----------------------#


# before correction of sign
regs_emp_ln_pers <- purrr::map2( dfs_emp_agg3, 
                                  covs_final_emp_ln_pers, 
                                  ~ rdrobust::rdrobust(y = .x$ln_avg_persons, x = .x$votes_pct_for, 
                                                           covs = .x %>% select(.y) ,  
                                                           c = cutoff, all = TRUE)
                                  )

treatment_effect_summary(regs_emp_ln_pers) %>% rownames_to_column() %>% as_tibble() %>% 
  mutate(significant = if_else(pval < 0.05, "yes", "no"))


# yr 1 had collinearity
rdrobust::rdrobust(y = dfs_emp_agg3$yr_t_plus_1$ln_avg_persons, x = dfs_emp_agg3$yr_t_plus_1$votes_pct_for, 
                   covs = dfs_emp_agg3$yr_t_plus_1 %>% select(covs_final_emp_ln_pers$yr_t_plus_1) %>% select(-c("pctwhite")),  
                   c = cutoff, all = TRUE)

# yr 2 had collinearity
rdrobust::rdrobust(y = dfs_emp_agg3$yr_t_plus_2$ln_avg_persons, x = dfs_emp_agg3$yr_t_plus_2$votes_pct_for, 
                   covs = dfs_emp_agg3$yr_t_plus_2 %>% select(covs_final_emp_ln_pers$yr_t_plus_2) %>% select(-c("pctwhite", "pctnevermarr")),  
                   c = cutoff, all = TRUE) %>% summary()
# yr 4 had collinearity
rdrobust::rdrobust(y = dfs_emp_agg3$yr_t_plus_4$ln_avg_persons, x = dfs_emp_agg3$yr_t_plus_4$votes_pct_for, 
                   covs = dfs_emp_agg3$yr_t_plus_4 %>% select(covs_final_emp_ln_pers$yr_t_plus_4) %>% select(-c("pctrent")),  
                   c = cutoff, all = TRUE) %>% summary()

# removing variables that caused collinearity
covs_final_emp_ln_pers$yr_t_plus_1 <- covs_final_emp_ln_pers$yr_t_plus_1[!covs_final_emp_ln_pers$yr_t_plus_1 %in% "pctwhite"]
covs_final_emp_ln_pers$yr_t_plus_2 <- covs_final_emp_ln_pers$yr_t_plus_2[!covs_final_emp_ln_pers$yr_t_plus_2 %in% c("pctwhite", "pctnevermarr")]
covs_final_emp_ln_pers$yr_t_plus_4 <- covs_final_emp_ln_pers$yr_t_plus_4[!covs_final_emp_ln_pers$yr_t_plus_4 %in% c("pctrent")]

# running again
regs_emp_ln_pers <- purrr::map2( dfs_emp_agg3, 
                                 covs_final_emp_ln_pers, 
                                 ~ rdrobust::rdrobust(y = .x$ln_avg_persons, x = .x$votes_pct_for, 
                                                      covs = .x %>% select(.y) ,  
                                                      c = cutoff, all = TRUE)
)

treatment_effect_summary(regs_emp_ln_pers) %>% rownames_to_column() %>% as_tibble() %>% 
  mutate(significant = if_else(pval < 0.05, "yes", "no"))


# before correction of sign
# correcting sign (neg to pos)
covs_final_emp_ln_pers_s$yr_t_minus_1 <- c("poverty", "pctown")
regs_emp_ln_pers_s <- purrr::map2( dfs_emp_agg3, 
                                 covs_final_emp_ln_pers_s, 
                                 ~ rdrobust::rdrobust(y = .x$ln_avg_persons, x = .x$votes_pct_for, 
                                                      covs = .x %>% select(.y) ,  
                                                      c = cutoff, all = TRUE)
)
treatment_effect_summary(regs_emp_ln_pers_s) %>% rownames_to_column() %>% as_tibble() %>% 
  mutate(significant = if_else(pval < 0.05, "yes", "no"))

# yr t+ 1
rdrobust::rdrobust(y = dfs_emp_agg3$yr_t_plus_1$ln_avg_persons, x = dfs_emp_agg3$yr_t_plus_1$votes_pct_for, 
                   covs = dfs_emp_agg3$yr_t_plus_1 %>% select(covs_final_emp_ln_pers_s$yr_t_plus_1) %>% select(-c("pctrent")) ,  
                   c = cutoff, all = TRUE)
# yr t+ 3
rdrobust::rdrobust(y = dfs_emp_agg3$yr_t_plus_3$ln_avg_persons, x = dfs_emp_agg3$yr_t_plus_3$votes_pct_for, 
                   covs = dfs_emp_agg3$yr_t_plus_3 %>% select(covs_final_emp_ln_pers_s$yr_t_plus_3) %>% select(-c("pctrent")),  
                   c = cutoff, all = TRUE)
# yr t + 4
rdrobust::rdrobust(y = dfs_emp_agg3$yr_t_plus_4$ln_avg_persons, x = dfs_emp_agg3$yr_t_plus_4$votes_pct_for, 
                   covs = dfs_emp_agg3$yr_t_plus_4 %>% select(covs_final_emp_ln_pers_s$yr_t_plus_4) %>% select(-c("pctrent")),  
                   c = cutoff, all = TRUE)


#-----------------------#
# average persons       #
#-----------------------#
regs_emp_pers <- purrr::map2( dfs_emp_agg3, 
                              covs_final_emp_pers, 
                                  ~ rdrobust::rdrobust(y = .x$tot_wages, x = .x$votes_pct_for, 
                                                       covs = .x %>% select(.y) ,  
                                                       c = cutoff, all = TRUE)
)
treatment_effect_summary(regs_emp_pers) %>% rownames_to_column() %>% as_tibble() %>% View()


#-----------------------#
# log average wages     #
#-----------------------#
regs_emp_ln_wages <- purrr::map2( dfs_emp_agg3, 
                                  covs_final_emp_ln_wages, 
                                  ~ rdrobust::rdrobust(y = .x$ln_wages, x = .x$votes_pct_for, 
                                                       covs = .x %>% select(.y) ,  
                                                       c = cutoff, all = TRUE)
)
treatment_effect_summary(regs_emp_ln_wages) %>% rownames_to_column() %>% as_tibble() %>% View()

# correcting sign (neg to pos)
covs_final_emp_ln_wages_s$yr_t_minus_1 <- c("poverty", "pctown")
regs_emp_ln_wages_s <- purrr::map2( dfs_emp_agg3, 
                                    covs_final_emp_ln_wages_s, 
                                   ~ rdrobust::rdrobust(y = .x$ln_wages, x = .x$votes_pct_for, 
                                                        covs = .x %>% select(.y) ,  
                                                        c = cutoff, all = TRUE)
)
treatment_effect_summary(regs_emp_ln_wages_s) %>% rownames_to_column() %>% as_tibble() %>% 
  mutate(significant = if_else(pval < 0.05, "yes", "no"))

#-----------------------#
# average wages         #
#-----------------------#
regs_emp_wages <- purrr::map2( dfs_emp_agg3, 
                               covs_final_emp_wages, 
                                  ~ rdrobust::rdrobust(y = .x$tot_wages, x = .x$votes_pct_for, 
                                                       covs = .x %>% select(.y) ,  
                                                       c = cutoff, all = TRUE)
)
treatment_effect_summary(regs_emp_wages) %>% rownames_to_column() %>% as_tibble() %>% View()


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Removing top and bottom x% of the observations ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
x_cut <- 0.05

dfs_emp_agg3_winsor <- purrr::map(dfs_emp_agg3, 
                                  ~ .x %>% 
                                    filter((tot_wages > quantile(tot_wages, x_cut)) & (tot_wages < quantile(tot_wages, 1-x_cut))))
  
# histogram (winsored vs original) example
ggplot(data = dfs_emp_agg3$yr_t_plus_1) +
  geom_density(mapping = aes(x = tot_wages))
ggplot(data = dfs_emp_agg3_winsor$yr_t_plus_1) +
  geom_density(mapping = aes(x = tot_wages))

ggplot(data = dfs_emp_agg3$yr_t_plus_1) +
  geom_density(mapping = aes(x = ln_wages))
ggplot(data = dfs_emp_agg3_winsor$yr_t_plus_1) +
  geom_density(mapping = aes(x = ln_wages))

ggplot(data = dfs_emp_agg3$yr_t_plus_1) +
  geom_density(mapping = aes(x = avg_persons))
ggplot(data = dfs_emp_agg3_winsor$yr_t_plus_1) +
  geom_density(mapping = aes(x = avg_persons))

ggplot(data = dfs_emp_agg3$yr_t_plus_1) +
  geom_density(mapping = aes(x = ln_avg_persons))
ggplot(data = dfs_emp_agg3_winsor$yr_t_plus_1) +
  geom_density(mapping = aes(x = avg_persons))

# Note: tot_wages and avg_persons (before log) are still highly skewed even after winsorization


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++# 
# Using Employment/pop and Wages/pop as outcome variables ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++# 

# density plots
ggplot(data = dfs_emp_agg_per$yr_t_plus_2) +
  geom_density(mapping = aes(x = log(wages_per_cap) ))
ggplot(data = dfs_emp_agg_per$yr_t_plus_2) +
  geom_density(mapping = aes(x = emp_per_cap ))

regs_emp_per_emp <- purrr::map(.x = dfs_emp_agg_per, ~ rdrobust::rdrobust(y = .x$emp_per_cap, 
                                                                          x = .x$votes_pct_for, c = cutoff, all = TRUE))
treatment_effect_summary(regs_emp_per_emp) %>% rownames_to_column() %>% as_tibble() 

regs_emp_per_ln_emp <- purrr::map(.x = dfs_emp_agg_per, ~ rdrobust::rdrobust(y = .x$ln_emp_per_cap, 
                                                                               x = .x$votes_pct_for, c = cutoff, all = TRUE))
treatment_effect_summary(regs_emp_per_ln_emp) %>% rownames_to_column() %>% as_tibble() 


regs_emp_per_wages <- purrr::map(.x = dfs_emp_agg_per, ~ rdrobust::rdrobust(y = .x$wages_per_cap, 
                                                                          x = .x$votes_pct_for, c = cutoff, all = TRUE))
treatment_effect_summary(regs_emp_per_wages) %>% rownames_to_column() %>% as_tibble() 

regs_emp_per_ln_wages <- purrr::map(.x = dfs_emp_agg_per, ~ rdrobust::rdrobust(y = .x$ln_wages_per_cap, 
                                                                            x = .x$votes_pct_for, c = cutoff, all = TRUE))
treatment_effect_summary(regs_emp_per_ln_wages) %>% rownames_to_column() %>% as_tibble() 


# wages
covs_final_emp_per_wages <- purrr::map(dfs_emp_agg_per, ~find_covs_sign(.x, y = "wages_per_cap", 
                                                                        covs_list = covs_list_emp[!(covs_list_emp %in% c("pop", "inctaxrate"))],
                                                                        sign = "positive"))
regs_emp_per_wages <- purrr::map2( dfs_emp_agg_per, 
                                 covs_final_emp_per_wages, 
                                 ~ rdrobust::rdrobust(y = .x$wages_per_cap, x = .x$votes_pct_for, 
                                                      covs = .x %>% select(.y) ,  
                                                      c = cutoff, all = TRUE)
)

treatment_effect_summary(regs_emp_per_wages) %>% rownames_to_column() %>% as_tibble() %>% 
  mutate(significant = if_else(pval < 0.05, "yes", "no"))


dfs_emp_agg_per$yr_t_plus_4


# ln wages per cap
covs_final_emp_per_ln_wages <- purrr::map(dfs_emp_agg_per, ~find_covs_sign(.x, y = "ln_wages_per_cap", 
                                                                        covs_list = covs_list_emp[!(covs_list_emp %in% c("pop", "inctaxrate"))],
                                                                        sign = "positive"))


# cov_list <- covs_list_emp[!(covs_list_emp %in% c("pop", "inctaxrate"))]

