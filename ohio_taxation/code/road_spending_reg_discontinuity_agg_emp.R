#================================================================================================================#
# Purpose : Use wages and average employed persons as outcome of interest for Road spending and Taxes project
# Name    : Saani Rawat
# Created : 07/05/2023
# Log     : 
#        1. 07/05/2023: started the code
#        2. 07/14/2023: added outcome variable: employment/pop , wages/pop
#        3. 05/03/2024: Added Wages per person variable. Found that the effect is significant for wages per person.
#        3. 05/04/2024: Doing industry level analysis
#================================================================================================================#

# loading packages
packages <- c("Rbearcat", "tidyverse", "lubridate", "haven", "stringr", "here", "knitr", "janitor", "scales","data.table","rdrobust")
for (pkg in packages){
  library(pkg, character.only = TRUE)
}

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables/employment")
plots <- paste0(data,"/outputs/plots/employment")

cutoff = 50

# running data setup code
# source(paste0(code,"/employment_data_setup.R"))
source(paste0(code,"/utility_functions.R"))
# beepr::beep("mario")

# covariates list 
covs_list <- c("pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl")

#==============================================#
# importing roads and census dataset. 
#==============================================#
# Selecting only renewals and levies that do not last forever. Separating into treatment and control groups.
roads_and_census <- haven::read_dta(paste0(data,"/roads_and_census.dta")) %>%
  select(-matches("yr_t_")) %>%
  filter(description == "R" & duration != "1000") %>%
  janitor::clean_names() %>%
  mutate(votes_pct_against = 100 - votes_pct_for) %>%
  mutate(treated = if_else(votes_pct_against > cutoff, 1, 0)) %>%
  # mutate(yr_t_minus_3 = year - 3, 
  #        yr_t_minus_2 = year - 2, 
  #        yr_t_minus_1 = year - 1,
  #        yr_t_plus_0 = year,
  #        yr_t_plus_1 = year + 1,
  #        yr_t_plus_2 = year + 2,
  #        yr_t_plus_3 = year + 3,
  #        yr_t_plus_4 = year + 4,
  #        yr_t_plus_5 = year + 5,
  #        yr_t_plus_6 = year + 6,
  #        yr_t_plus_7 = year + 7,
  #        yr_t_plus_8 = year + 8,
  #        yr_t_plus_9 = year + 9,
  #        yr_t_plus_10 = year + 10) %>%
  select(tendigit_fips, year, starts_with("yr_"), everything()) %>% 
  arrange(tendigit_fips, year)

# roads_and_census %>% 
#   group_by(tendigit_fips, treated) %>%
#   summarise(n = n()) %>%
#   filter(tendigit_fips %in% c(3914581606, 3916582418, 3900777686, 3905576628, 3907559514))
# 
#   filter(treated == 1) %>%
#   arrange(desc(n))
  
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

naics_df <- data.frame(
  naics_2dg = c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 55, 56, 61, 62, 71, 72, 81, 92),
  `sector_title` = c("agriculture, forestry, fishing and hunting", "mining", "utilities", "construction", "manufacturing", "manufacturing", "manufacturing", "wholesale trade", "retail trade", "retail trade", "transportation and warehousing", "transportation and warehousing", "information", "finance and insurance", "real estate rental and leasing", "professional, scientific, and technical services", "management of companies and enterprises", "administrative and support and waste services", "educational services", "health care and social assistance", "arts, entertainment, and recreation", "accommodation and food services", "other services (except public administration)", "public administration")
)


#========================================#
#  Manipulation test (X variable) ----
#========================================#
# dfs_emp_agg$yr_t_plus_2 %>% View()

dens_test <- rddensity::rddensity(X = roads_and_census$votes_pct_against, c = cutoff, massPoints = FALSE)
summary(dens_test)

dens_test$test$p_jk

dens_tests_emp <- purrr::map(dfs_emp_agg, ~ rddensity::rddensity(X = .x$votes_pct_against, c = cutoff))
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

rdd::DCdensity(dfs_emp_agg$yr_t_plus_1$votes_pct_against, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_2$votes_pct_against, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_3$votes_pct_against, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_4$votes_pct_against, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_5$votes_pct_against, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_6$votes_pct_against, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_7$votes_pct_against, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_8$votes_pct_against, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_9$votes_pct_against, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_10$votes_pct_against, c = 50)

#=========================#
#  RD plots ----
#=========================#

# Total wages
purrr::walk2(dfs_emp_agg, names(dfs_emp_agg), ~print(rdrobust::rdplot(y = .x$tot_wages, 
                                                             x = .x$votes_pct_for, 
                                                             c = 50, p = 1, title = .y)))

# Avg Number of persons employed
purrr::walk2(dfs_emp_agg, names(dfs_emp_agg), ~print(rdrobust::rdplot(y = .x$avg_persons, 
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
regs_emp_wages <- purrr::map(.x = dfs_emp_agg, ~ rdrobust::rdrobust(y = .x$tot_wages, x = .x$votes_pct_against, c = cutoff, all = TRUE))
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

# output for paper draft 
write.csv(te_emp_ln_wages, paste0(tables, "/te_emp_ln_wages.csv"), row.names = FALSE)
write.csv(te_emp_ln_avg_persons, paste0(tables, "/te_emp_ln_avg_persons.csv"), row.names = FALSE)


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

#====================================================#
# Using Wages/Employment as outcome ----
#====================================================#

dfs_emp_agg_p <- map(dfs_emp_agg, ~ .x %>% mutate(wages_per_emp = tot_wages/avg_persons) %>%
      relocate(tendigit_fips, year, tot_wages, avg_persons, wages_per_emp, everything()) %>%
        filter(!(is.na(wages_per_emp) | (avg_persons == 0) | is.nan(wages_per_emp)) )) 

dfs_emp_agg_p$yr_t_minus_3$wages_per_emp %>% summary()

# running RDD without covariates
regs_emp_wages_per_emp <- purrr::map(.x = dfs_emp_agg_p, ~ rdrobust::rdrobust(y = .x$wages_per_emp, x = .x$votes_pct_against, c = cutoff, all = TRUE))
te_emp_wages_per_emp <- te_tables(regs_emp_wages_per_emp)
plot_te(te_emp_wages_per_emp)

# running RDD with covariates
covs_final_emp_wages_per_emp <- purrr::map(dfs_emp_agg_p, ~find_covs_sign(.x, y = "wages_per_emp", 
                                                                        covs_list = covs_list[!(covs_list %in% c("pop", "inctaxrate"))],
                                                                        sign = "negative"))

covs_final_emp_wages_per_emp_m <- purrr::map(dfs_emp_agg_p, ~find_covs(.x, y = "wages_per_emp", 
                                                                          covs_list = covs_list[!(covs_list %in% c("pop", "inctaxrate"))]))

# covs_final_emp_wages_per_emp_m2 <-
cv_f_emp <- covs_final_emp_wages_per_emp_m
cv_f_emp2 <- covs_final_emp_wages_per_emp_m
cv_f_emp$yr_t_minus_3 <- c("medfamy", "pctrent", "pctbachelors", "pctblack")
cv_f_emp$yr_t_minus_2 <- c("medfamy", "poverty", "pctrent")
cv_f_emp$yr_t_minus_1 <- c("medfamy", "poverty", "pctrent")
# cv_f_emp$yr_t_minus_2 <- cv_f_emp$yr_t_minus_2
cv_f_emp$yr_t_plus_0 <- cv_f_emp$yr_t_plus_0[!(cv_f_emp$yr_t_plus_0 %in% c("pctrent", "pctnevermarr"))]
cv_f_emp$yr_t_plus_1 <- cv_f_emp$yr_t_plus_1[!(cv_f_emp$yr_t_plus_1 %in% c("pctrent", "pctnevermarr"))]
cv_f_emp$yr_t_plus_2 <- cv_f_emp$yr_t_plus_2[!(cv_f_emp$yr_t_plus_2 %in% c("pctrent", "pctnevermarr"))]
cv_f_emp$yr_t_plus_4 <- cv_f_emp$yr_t_plus_4[!(cv_f_emp$yr_t_plus_4 %in% c("pctmin", "pctnevermarr"))]
cv_f_emp$yr_t_plus_8 <- cv_f_emp$yr_t_plus_8[!(cv_f_emp$yr_t_plus_8 %in% c("pctmin", "pctnevermarr"))]
cv_f_emp$yr_t_plus_10 <- cv_f_emp$yr_t_plus_10[!(cv_f_emp$yr_t_plus_10 %in% c("pctmin", "pctrent"))]

# beepr::beep("mario")

gs_emps <- purrr::map2(cv_f_emp, dfs_emp_agg_p, .f = function(x,y){
  rdrobust(  y = y$wages_per_emp,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_emp <- te_tables(gs_emps, ci_level = 1.96) 
plot_te(tes_emp, title = "Visualization of Treatment Effects: Wages per employee", subtitle = "Aggregate") 

# Note 1: we pick up some effect starting year 3 and then the effect is transitory and then it disappears by year 6
# Note 2: we find a positive relationship between cutting road spending and wages per employee, which can seem counterintuitive on surface. How do bad roads increase wages per employee? But perhaps it is not the roads per se, but the money going to roads that is now being "better used".
# Mechanisms: 1. More money in the hands of people (less taxes) 2. Change in Labor Market composition? 


# list of multico years: yr_t_minus_3, yr_t_minus_2,yr_t_minus_1,yr_t_plus_0,yr_t_plus_1,yr_t_plus_2,yr_t_plus_4,yr_t_plus_8,yr_t_plus_10

y <- dfs_emp_agg_p$yr_t_plus_10
x <-      cv_f_emp$yr_t_plus_10[!(cv_f_emp$yr_t_plus_10 %in% c("pctmin", "pctrent"))]
rdrobust(y = y$wages_per_emp,
         x = y$votes_pct_against,
         c = cutoff,
         covs = y %>%
           select(x) ,
         all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)


# checking effect size for the years that start with "yr_t_plus_"
tes_emp %>% filter(grepl("yr_t_plus_", dataset)) %>% select(robust_coef) %>% pull %>% mean / map_dbl(dfs_emp_agg_p[4:14], ~mean(.x$wages_per_emp)) %>% mean
# 8.7% increase in wages per employee over the 10 years

# rdd_lm(df = dfs_emp_agg_p$yr_t_minus_1,
#        y = "wages_per_emp", 
#        x = "votes_pct_against", 
#        covs = covs_final_emp_wages_per_emp_m$yr_t_minus_1,
#        cutoff = 50) %>% summary()


#====================================================#
# Industry level analysis ----
#====================================================#

# select only `31`, `32` and `33` from the list of datasets
# dfs_emp_by_ind[c("31", "32", "33")] %>% bind_rows

#====================================================#
# |--- Transportation and Warehousing sector ##
#====================================================#

df_trans <- dfs_emp_by_ind[c("48", "49")] %>% bind_rows %>%
                  group_by(tendigit_fips, year, sector_title) %>%
                  summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
                            avg_persons = sum(avg_persons, na.rm = TRUE)) %>% 
                  mutate(wages_per_emp = tot_wages / avg_persons) %>%
                  filter(!(is.na(wages_per_emp)| is.infinite(wages_per_emp)) )  %>% ungroup
# creating year-by-year datasets
yrs <- c(paste0("yr_t_minus_",as.character(1:3)), "yr_t_plus_0", paste0("yr_t_plus_",as.character(1:10)))
dfs_trans_emp <- purrr::map(yrs, ~ df_trans %>% 
                     arrange(tendigit_fips, year) %>%
                     mutate(emp_flag = 1) %>%
                     mutate({{.x}} := as.numeric(year)) %>%
                     select(-c(year))
)
names(dfs_trans_emp) <- yrs


# adding covariates
dfs_emp_trans_agg <- purrr::map2(dfs_trans_emp, yrs, function(x, y){
  x %>% inner_join(roads_and_census, by = c("tendigit_fips", y)) %>%
    relocate(tendigit_fips, year, everything()) %>% 
    select(-c(yrs[yrs != y], emp_flag)) %>%
    ungroup()
})

# RDDs without covariates
regs_trans <- purrr::map(.x = dfs_emp_trans_agg, ~ rdrobust::rdrobust(y = .x$wages_per_emp, x = .x$votes_pct_against, c = cutoff, all = TRUE))


tes_ts <- te_tables(regs_trans)
plot_te(tes_ts, title = "Visualization of Treatment Effects: Wages per employee", subtitle = "Transporting and Warehousing sector")


# RDDs with covariates
cv_trans <- purrr::map(dfs_emp_trans_agg, ~find_covs(.x, y = "wages_per_emp", 
                                                                       covs_list = covs_list[!(covs_list %in% c("pop", "inctaxrate"))]))

cv_f_tr <- cv_trans
cv_f_tr$yr_t_minus_3 <- c("medfamy", "pctrent", "pctbachelors", "pctblack")
cv_f_tr$yr_t_minus_2 <- c("medfamy", "poverty", "pctrent")
cv_f_tr$yr_t_minus_1 <- c("medfamy", "poverty", "pctrent")
cv_f_tr$yr_t_plus_0 <- cv_f_tr$yr_t_plus_0[!(cv_f_tr$yr_t_plus_0 %in% c("pctrent", "pcthsgrad"))]
cv_f_tr$yr_t_plus_1 <- cv_f_tr$yr_t_plus_1[!(cv_f_tr$yr_t_plus_1 %in% c("pctrent","incherfindahl", "pctmin", "pctsinparhhld"))]
cv_f_tr$yr_t_plus_2 <- cv_f_tr$yr_t_plus_2[!(cv_f_tr$yr_t_plus_2 %in% c("pctmin", "pctrent","incherfindahl", "pcthsgrad", "pctapi"))]
cv_f_tr$yr_t_plus_2 <- c("pctlesshs", "pctbachelors", "pctblack", "pctapi")
cv_f_tr$yr_t_plus_3 <- cv_f_tr$yr_t_plus_3[!(cv_f_tr$yr_t_plus_3 %in% c("pctnevermarr", "pctrent", "pcthsgrad"))]
cv_f_tr$yr_t_plus_10 <- cv_f_tr$yr_t_plus_10[!(cv_f_tr$yr_t_plus_10 %in% c("pctmin", "raceherfindahl","incherfindahl", "pcthsgrad"))]

gs_trans <- purrr::map2(cv_f_tr, dfs_emp_trans_agg, .f = function(x,y){
  rdrobust(  y = y$wages_per_emp,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_trans <- te_tables(gs_trans) 
plot_te(tes_trans, title = "Visualization of Treatment Effects: Wages per employee", subtitle = "Transporting and Warehousing sector") 

y <- dfs_emp_trans_agg$yr_t_plus_1
x <-           cv_f_tr$yr_t_plus_1
rdrobust(  y = y$wages_per_emp,
           x = y$votes_pct_against,
           c = cutoff,
           covs = y %>% select(x) ,
           all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2) %>% summary()

# 0, 1, 2, 3, 10

#====================================================#
# |--- Manufacturing sector ##
#====================================================#

df_man <- dfs_emp_by_ind[c("31", "32", "33")] %>% bind_rows %>%
  group_by(tendigit_fips, year, sector_title) %>%
  summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
            avg_persons = sum(avg_persons, na.rm = TRUE)) %>% 
  mutate(wages_per_emp = tot_wages / avg_persons) %>%
  filter(!(is.na(wages_per_emp)| is.infinite(wages_per_emp)) )  %>% ungroup
dfs_man_emp <- purrr::map(yrs, ~ df_man %>% 
                            arrange(tendigit_fips, year) %>%
                            mutate(emp_flag = 1) %>%
                            mutate({{.x}} := as.numeric(year)) %>%
                            select(-c(year))
)
names(dfs_man_emp) <- yrs

# adding covariates
dfs_emp_man_agg <- purrr::map2(dfs_man_emp, yrs, function(x, y){
  x %>% inner_join(roads_and_census, by = c("tendigit_fips", y)) %>%
    relocate(tendigit_fips, year, everything()) %>% 
    select(-c(yrs[yrs != y], emp_flag)) %>%
    ungroup()
})

# RDDs without covariates
regs_man <- purrr::map(.x = dfs_emp_man_agg, ~ rdrobust::rdrobust(y = .x$wages_per_emp, x = .x$votes_pct_against, c = cutoff, all = TRUE))

tes_ts <- te_tables(regs_man)
plot_te(tes_ts, title = "Visualization of Treatment Effects: Wages per employee", subtitle = "Manufacturing sector")


# RDDs with covariates
cv_man <- purrr::map(dfs_emp_man_agg, ~find_covs(.x, y = "wages_per_emp", 
                                                  covs_list = covs_list[!(covs_list %in% c("pop", "inctaxrate"))]))
cv_f_man <- cv_man
cv_f_man$yr_t_minus_3 <- c("medfamy", "pctrent", "pctbachelors", "pctblack")
cv_f_man$yr_t_minus_2 <- c("medfamy", "poverty", "pctrent")
cv_f_man$yr_t_minus_1 <- c("medfamy", "poverty", "pctrent")

gs_man <- purrr::map2(cv_f_man, dfs_emp_man_agg, .f = function(x,y){
  rdrobust(  y = y$wages_per_emp,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_man <- te_tables(gs_man) 
plot_te(tes_man, title = "Visualization of Treatment Effects: Wages per employee", subtitle = "Manufacturing sector") 



#====================================================#
# |--- Trade sector ##
#====================================================#

df_trade <- dfs_emp_by_ind[c("42", "44", "45")] %>% bind_rows %>%
  group_by(tendigit_fips, year, sector_title) %>%
  summarise(tot_wages = sum(tot_wages, na.rm = TRUE),
            avg_persons = sum(avg_persons, na.rm = TRUE)) %>% 
  mutate(wages_per_emp = tot_wages / avg_persons) %>%
  filter(!(is.na(wages_per_emp)| is.infinite(wages_per_emp)) )  %>% ungroup
dfs_trade_emp <- purrr::map(yrs, ~ df_trade %>% 
                              arrange(tendigit_fips, year) %>%
                              mutate(emp_flag = 1) %>%
                              mutate({{.x}} := as.numeric(year)) %>%
                              select(-c(year))
)
names(dfs_trade_emp) <- yrs

# adding covariates
dfs_emp_trade_agg <- purrr::map2(dfs_trade_emp, yrs, function(x, y){
  x %>% inner_join(roads_and_census, by = c("tendigit_fips", y)) %>%
    relocate(tendigit_fips, year, everything()) %>% 
    select(-c(yrs[yrs != y], emp_flag)) %>%
    ungroup()
})

# RDDs without covariates
regs_trade <- purrr::map(.x = dfs_emp_trade_agg, ~ rdrobust::rdrobust(y = .x$wages_per_emp, x = .x$votes_pct_against, c = cutoff, all = TRUE))

tes_ts <- te_tables(regs_trade)
plot_te(tes_ts, title = "Visualization of Treatment Effects: Wages per employed person", subtitle = "Trade sector")


# RDDs with covariates
cv_trade <- purrr::map(dfs_emp_trade_agg, ~find_covs(.x, y = "wages_per_emp", 
                                                     covs_list = covs_list[!(covs_list %in% c("pop", "inctaxrate"))]))

gs_trade <- purrr::map2(cv_trade, dfs_emp_trade_agg, .f = function(x,y){
  rdrobust(  y = y$wages_per_emp,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_trade <- te_tables(gs_trade) 
plot_te(tes_trade, title = "Visualization of Treatment Effects: Wages per employed person", subtitle = "Trade sector") 

