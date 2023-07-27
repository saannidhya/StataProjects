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
source(paste0(code,"/employment_data_setup.R"))
source(paste0(code,"/utility_functions.R"))
# beepr::beep("mario")



# View(dfs_emp_agg3$yr_t_plus_10)

# dfs_emp_agg2$yr_t_plus_3 %>% select(tendigit_fips , year,tot_wages, ln_wages)
# purrr::map(dfs_emp_agg, .x)
# colnames(dfs_emp_agg$yr_t_plus_4)

#========================================#
# |- Manipulation test (X variable) ----
#========================================#
# dfs_emp_agg$yr_t_plus_2 %>% View()

dens_tests_emp <- purrr::map(dfs_emp_agg, ~ rddensity::rddensity(X = .x$votes_pct_for, c = cutoff))
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

dfs_emp_agg$yr_t_plus_1$votes_pct_for
rdd::DCdensity(dfs_emp_agg$yr_t_plus_1$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_2$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_3$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_4$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_5$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_6$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_7$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_8$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_9$votes_pct_for, c = 50)
rdd::DCdensity(dfs_emp_agg$yr_t_plus_10$votes_pct_for, c = 50)

#=========================#
# |- Exploratory plots ----
#=========================#


data = emp_df_agg_yr_qtr %>% mutate(date = as.Date(paste0(ifelse(quarter == 4, year + 1, year), 
                                                          "-", 
                                                          ifelse(quarter == 4, 1, (quarter*3) + 1), 
                                                          "-01")) - 1, 
                                    ln_tot_wages = log(tot_wages), ln_avg_persons = log(avg_persons))

ggplot(data = data) + 
  geom_line(mapping = aes(x = date, y = tot_wages))

data_long <- pivot_longer(data = data %>% select(c(date, ln_tot_wages, ln_avg_persons)),
                           cols = c(ln_tot_wages, ln_avg_persons),
                           names_to = "variable",
                           values_to = "value")

ggplot(data = data_long) + 
  geom_line(mapping = aes(x = date, y = value, color = variable))

ggplot(data = data_long, aes(x = date, y = value, color = variable)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free_y") +
  labs(x = "Date", y = "Value", title = "Log Wages and Log Employment: Plots")

# correlation between log wages and employment
cor(data$ln_tot_wages, data$ln_avg_persons)
cor(data$tot_wages, data$avg_persons)

#=========================#
# |- RD plots ----
#=========================#

for (i in 30:50){
  rdrobust::rdplot(y = dfs_agg$housing_roads_census_t_plus_9_matches$median_sale_amount, 
                   x = dfs_agg$housing_roads_census_t_plus_9_matches$votes_pct_for, 
                   c = 50, p = 1, nbins = c(i,i), title = paste0(as.character(i)))
}
# Total wages
purrr::map2(dfs_emp_agg, names(dfs_emp_agg), ~print(rdrobust::rdplot(y = .x$tot_wages, 
                                                             x = .x$votes_pct_for, 
                                                             c = 50, p = 1, title = .y)))

# Avg Number of persons employed
purrr::map2(dfs_emp_agg, names(dfs_emp_agg), ~print(rdrobust::rdplot(y = .x$avg_persons, 
                                                                     x = .x$votes_pct_for, 
                                                                     c = 50, p = 1, title = .y)))

# Log of total wages
purrr::map2(dfs_emp_agg2, names(dfs_emp_agg2), ~print(rdrobust::rdplot(y = .x$ln_wages, 
                                                                     x = .x$votes_pct_for, 
                                                                     c = 50, p = 1, title = .y)))

# Avg Number of persons employed
purrr::map2(dfs_emp_agg2, names(dfs_emp_agg2), ~print(rdrobust::rdplot(y = .x$ln_avg_persons, 
                                                                     x = .x$votes_pct_for, 
                                                                     c = 50, p = 1, title = .y)))

purrr::map_dbl(dfs_emp_agg2, ~ .x %>% filter(is.na(votes_pct_for) | is.na(ln_wages)) %>% nrow())

dfs_emp_agg2$yr_t_plus_1$votes_pct_for
dfs_emp_agg2$yr_t_plus_1 %>% filter(is.na(ln_wages)) %>% nrow()


rdrobust::rdplot(y = dfs_emp_agg2$yr_t_plus_2$ln_wages, 
                 x = dfs_emp_agg2$yr_t_plus_2$votes_pct_for, 
                 c = 50, p = 1, title = "")

typeof(dfs_emp_agg2$yr_t_plus_2$ln_wages[2])

dfs_emp_agg2$yr_t_plus_2$ln_wages[is.na(dfs_emp_agg2$yr_t_plus_2$ln_wages)]

#=========================================#
# Exploratory Data Analysis ----
#=========================================#

d <- dfs_emp_agg$yr_t_plus_4

ggplot(data = d) +
  geom_point(mapping = aes(x = votes_pct_for, avg_persons))

hist(d)

plot(d$avg_persons)

plot(log(d$tot_wages))

# one county subdivision, 3904918000, has values avg employees greater than 100,000 
filter(d, avg_persons > 100000)

# 3904918000 = city of columbus has by far the largest number of persons employed
filter(d, tendigit_fips == 3904918000) %>% View()
colnames(d)
d %>% select(tendigit_fips, year, yr_t_plus_2, votes_pct_for) %>% filter(tendigit_fips == 3904918000)


#====================================================#
# running regressions (aggregate) without covs ----
#====================================================#


## storing univariate RDD models (outcome vs running variable, no covariates) from t-2 to t+10

# tot wages
regs_emp_wages <- purrr::map(.x = dfs_emp_agg3, ~ rdrobust::rdrobust(y = .x$tot_wages, x = .x$votes_pct_for, c = cutoff, all = TRUE))

summary(regs_emp_wages$yr_t_plus_3)

# median(purrr::map_dbl(dfs_emp_agg3, ~ nrow(.x)))

# avg persons
regs_emp_persons <- purrr::map(.x = dfs_emp_agg3, ~ rdrobust::rdrobust(y = .x$avg_persons, x = .x$votes_pct_for, c = cutoff, all = TRUE))


# summary(regs_emp_wages$yr_t_plus_6)

# log of tot wages
regs_emp_ln_wages <- purrr::map(.x = dfs_emp_agg3, ~ rdrobust::rdrobust(y = .x$ln_wages, x = .x$votes_pct_for, c = cutoff, all = TRUE))

regs_emp_ln_avg_persons <- purrr::map(.x = dfs_emp_agg3, ~ rdrobust::rdrobust(y = .x$ln_avg_persons, x = .x$votes_pct_for, c = cutoff, all = TRUE))

regs_emp_ln_avg_persons$yr_t_plus_2 %>% summary()

treatment_effect_summary <- function(list){
  return(data.frame(bias_corrected_coef = purrr::map_dbl(list, ~ .x$coef[2]), 
                    pval = purrr::map_dbl(list, ~ .x$pv[2])) ) 
}




treatment_effect_summary(regs_emp_wages) %>% rownames_to_column() %>% as_tibble() %>% View()
treatment_effect_summary(regs_emp_persons) %>% rownames_to_column() %>% as_tibble() %>% View()
treatment_effect_summary(regs_emp_ln_wages) %>% rownames_to_column() %>% as_tibble() %>% View()
treatment_effect_summary(regs_emp_ln_avg_persons) %>% rownames_to_column() %>% as_tibble() %>% View()

rdrobust::rdrobust(y = dfs_emp_agg2$yr_t_plus_1$ln_wages, x = dfs_emp_agg2$yr_t_plus_1$votes_pct_for, c = cutoff, all = TRUE) %>% summary()
rdrobust::rdrobust(y = dfs_emp_agg2$yr_t_plus_2$ln_wages, x = dfs_emp_agg2$yr_t_plus_2$votes_pct_for, c = cutoff, all = TRUE) %>% summary()
rdrobust::rdrobust(y = dfs_emp_agg2$yr_t_plus_3$ln_wages, x = dfs_emp_agg2$yr_t_plus_3$votes_pct_for, c = cutoff, all = TRUE) %>% summary()
rdrobust::rdrobust(y = dfs_emp_agg2$yr_t_plus_4$ln_wages, x = dfs_emp_agg2$yr_t_plus_4$votes_pct_for, c = cutoff, all = TRUE) %>% summary()
rdrobust::rdrobust(y = dfs_emp_agg2$yr_t_plus_5$ln_wages, x = dfs_emp_agg2$yr_t_plus_5$votes_pct_for, c = cutoff, all = TRUE) %>% summary()
rdrobust::rdrobust(y = dfs_emp_agg2$yr_t_plus_6$ln_wages, x = dfs_emp_agg2$yr_t_plus_6$votes_pct_for, c = cutoff, all = TRUE) %>% summary()
rdrobust::rdrobust(y = dfs_emp_agg2$yr_t_plus_7$ln_wages, x = dfs_emp_agg2$yr_t_plus_7$votes_pct_for, c = cutoff, all = TRUE) %>% summary()
rdrobust::rdrobust(y = dfs_emp_agg2$yr_t_plus_8$ln_wages, x = dfs_emp_agg2$yr_t_plus_8$votes_pct_for, c = cutoff, all = TRUE) %>% summary()
rdrobust::rdrobust(y = dfs_emp_agg2$yr_t_plus_9$ln_wages, x = dfs_emp_agg2$yr_t_plus_9$votes_pct_for, c = cutoff, all = TRUE) %>% summary()
rdrobust::rdrobust(y = dfs_emp_agg2$yr_t_plus_10$ln_wages, x = dfs_emp_agg2$yr_t_plus_10$votes_pct_for, c = cutoff, all = TRUE) %>% summary()

dfs_emp_agg2$yr_t_plus_2 %>% select(c(tot_wages,ln_wages)) %>% View()

sum(is.na(dfs_emp_agg2$yr_t_plus_2$ln_wages))
sum(is.na(dfs_emp_agg2$yr_t_plus_2$votes_pct_for))

dfs_emp_agg2$yr_t_plus_4 %>% colnames()

# dfs_emp_agg2$yr_t_plus_4 %>% select( tendigit_fips, year, yr_t_plus_4, everything(), -starts_with("yr_t_")) %>% colnames()





# dfs_emp_agg2$yr_t_plus_4 %>% select(-starts_with("yr_t_"), yr_t_plus_4) %>% 
#   relocate(year, .after = tendigit_fips) %>% 
#   relocate(yr_t_plus_4, .after = year)


# write.csv(dfs_emp_agg2$yr_t_plus_4, "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/dfs_emp_agg2_yr_t_plus_4.csv")

#=========================================#
# finding covariates ----
#=========================================#


# take all covariate names 
covs_list_emp <- dfs_emp_agg3$yr_t_plus_1 %>%
  select(-c(tendigit_fips,year,yr_t_plus_1, tendigit_fips_year, purpose2, tax_type, votes_for, votes_against, 
            votes_pct_for, description, millage_percent, duration, votes_pct_for_cntr, emp_flag, 
            ln_wages, ln_avg_persons, tot_wages, avg_persons)) %>%
  colnames()

# finding covariates
covs_final_emp_ln_pers <- purrr::map(dfs_emp_agg3, ~find_covs(.x, y = "ln_avg_persons", covs_list = covs_list_emp))
covs_final_emp_pers <- purrr::map(dfs_emp_agg3, ~find_covs(.x, y = "avg_persons", covs_list = covs_list_emp))
covs_final_emp_ln_wages <- purrr::map(dfs_emp_agg3, ~find_covs(.x, y = "ln_wages", covs_list = covs_list_emp))
covs_final_emp_wages <- purrr::map(dfs_emp_agg3, ~find_covs(.x, y = "tot_wages", covs_list = covs_list_emp))

## finding covariates with the right signs
covs_final_emp_ln_pers_s <- purrr::map(dfs_emp_agg3, ~find_covs_sign(.x, y = "ln_avg_persons", covs_list = covs_list_emp, sign = "positive"))
covs_final_emp_ln_wages_s <- purrr::map(dfs_emp_agg3, ~find_covs_sign(.x, y = "ln_wages", covs_list = covs_list_emp, sign = "positive"))

#-----------------------#
# log average persons   #
#-----------------------#

###########################
# before correction of sign
###########################
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

###########################
# before correction of sign
###########################
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


######################################################################
# Removing top and bottom x% of the observations ----
######################################################################
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


######################################################################
# Using Employment/pop and Wages/popa as outcome variables ----
######################################################################

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

