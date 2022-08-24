#==========================================================================================================#
# Purpose : Build on road_spending_reg_discontinuity_agg.R and add covariates to the regression
# Name    : Saani Rawat
# Created : 07/30/2022
# Log     : 
#           08/01/2022: 
#==========================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

# running data setup code
# source(paste0(code,"/housing_data_setup.R"))
# running covariates balance test
source(paste0(code,"/covariates_balance_test.R"))
source(paste0(code,"/utility_functions.R"))

# user-defined parameters  ----
t_test_sig_level <- 0.05


#============================================================================================================#
#     Introducing covariates (using median sale_amount for each county, vote and year)  into regression ---- 
#============================================================================================================#

# covariates which passed t-test (failed to reject null) for the specific dataset and are not potential outcome vars
var_list <- purrr::map(names(dfs_agg_covs), 
                       ~ all_tests %>%
                         filter((dataset == .x) & 
                                  (`p-value` > t_test_sig_level) & 
                                  !(variable %in% c('median_sale_amount','median_ln_sale_amount','median_sale_amount_per_sq_feet'))) %>%
                         select(variable) %>%
                         pull() )
names(var_list) <- names(dfs_agg_covs)

# median sale amount
covs_final <- purrr::map(dfs_agg_covs, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))


### regressions with covariates for median sale amount
g2 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_2_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_2_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_2_matches %>%
                   select(covs_final$housing_roads_census_t_plus_2_matches)  ,
                 all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
g2 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_3_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_3_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_3_matches %>%
                   select(covs_final$housing_roads_census_t_plus_3_matches)  ,
                 all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
g4 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_4_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_4_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_4_matches %>%
                   select(covs_final$housing_roads_census_t_plus_4_matches) ,
                 all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2) 
g5 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_5_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_5_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_5_matches %>%
                   select(covs_final$housing_roads_census_t_plus_5_matches) ,
                 all = TRUE) 
g6 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_6_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_6_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_6_matches %>%
                   select(covs_final$housing_roads_census_t_plus_6_matches) ,
                 all = TRUE) 
g7 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_7_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_7_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_7_matches %>%
                   select(covs_final$housing_roads_census_t_plus_7_matches) ,
                 all = TRUE) 
g8 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_8_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_8_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_8_matches %>%
                   select(covs_final$housing_roads_census_t_plus_8_matches) ,
                 all = TRUE) 
g9 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_9_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_9_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_9_matches %>%
                   select(covs_final$housing_roads_census_t_plus_9_matches) ,
                 all = TRUE) 
g10 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_10_matches$median_sale_amount,
                  x = dfs_agg_covs$housing_roads_census_t_plus_10_matches$votes_pct_for,
                  c = cutoff,
                  covs = dfs_agg_covs$housing_roads_census_t_plus_10_matches %>%
                    select(covs_final$housing_roads_census_t_plus_10_matches) ,
                  all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2) 
summary(g4)
summary(g5)
summary(g6)
summary(g7)
summary(g8)
summary(g9)
summary(g10)

#============================================================================================================#
#     Introducing covariates (using median sale_amount per square feet)  into regression ---- 
#============================================================================================================#

# median sale amount per square feet
covs_final_per <- purrr::map(dfs_agg_per_covs, ~find_covs(.x, y = "median_sale_amount_per_sq_feet", covs_list = covs_list))
## 


### regressions with covariates for median sale amount per square feet

f4 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_4_matches$median_sale_amount_per_sq_feet,
                 x = dfs_agg_per_covs$housing_roads_census_t_plus_4_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_per_covs$housing_roads_census_t_plus_4_matches %>%
                   select(covs_final_per$housing_roads_census_t_plus_4_matches) ,
                 all = TRUE) 
f5 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_5_matches$median_sale_amount_per_sq_feet,
                 x = dfs_agg_per_covs$housing_roads_census_t_plus_5_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_per_covs$housing_roads_census_t_plus_5_matches %>%
                   select(covs_final_per$housing_roads_census_t_plus_5_matches) ,
                 all = TRUE) 
f6 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_6_matches$median_sale_amount_per_sq_feet,
                 x = dfs_agg_per_covs$housing_roads_census_t_plus_6_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_per_covs$housing_roads_census_t_plus_6_matches %>%
                   select(covs_final_per$housing_roads_census_t_plus_6_matches) ,
                 all = TRUE) 
f7 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_7_matches$median_sale_amount_per_sq_feet,
                 x = dfs_agg_per_covs$housing_roads_census_t_plus_7_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_per_covs$housing_roads_census_t_plus_7_matches %>%
                   select(covs_final_per$housing_roads_census_t_plus_7_matches) ,
                 all = TRUE) 
f8 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_8_matches$median_sale_amount_per_sq_feet,
                 x = dfs_agg_per_covs$housing_roads_census_t_plus_8_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_per_covs$housing_roads_census_t_plus_8_matches %>%
                   select(covs_final_per$housing_roads_census_t_plus_8_matches) ,
                 all = TRUE) 
f9 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_9_matches$median_sale_amount_per_sq_feet,
                 x = dfs_agg_per_covs$housing_roads_census_t_plus_9_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_per_covs$housing_roads_census_t_plus_9_matches %>%
                   select(covs_final_per$housing_roads_census_t_plus_9_matches) ,
                 all = TRUE) 
f10 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_10_matches$median_sale_amount_per_sq_feet,
                  x = dfs_agg_per_covs$housing_roads_census_t_plus_10_matches$votes_pct_for,
                  c = cutoff,
                  covs = dfs_agg_per_covs$housing_roads_census_t_plus_10_matches %>%
                    select(covs_final_per$housing_roads_census_t_plus_10_matches) ,
                  all = TRUE) 
summary(f4)
summary(f5)
summary(f6)
summary(f7)
summary(f8)
summary(f9)
summary(f10)


# lm(dfs_agg_covs$housing_roads_census_t_plus_4_matches, 
#    formula = median_sale_amount ~ pctwithkids + pctsinparhhld + pctlesshs + pctrent + pct5to17 + pct18to64 + pctamerind + pctotherrace + incherfindahl) %>%
#   summary()
# 
# 
# lm(dfs_agg_covs$housing_roads_census_t_minus_2_matches %>% select(c("median_sale_amount",covs_list_t_minus_2)), 
#    formula = median_sale_amount ~ .) %>%
#   summary()
# 
# Reduce(intersect, covs_final)


################### Final Covariates List ###############
# median sale amount
covs_list_t_minus_1 = c("pctwithkids","pctlesshs","pctsomecoll","pct18to64","pctwhite","pctblack","pctamerind","pctapi","pctmin","pcthisp","pctnevermarr","incherfindahl","inctaxrate")
covs_list_t_minus_2 = c("pop","unemprate","pctlt5","pctwhite","pctblack","pctamerind","pctapi","pctotherrace","raceherfindahl","pcthisp","pctnevermarr")
covs_list_t_plus_1 = c("childpov","poverty","pctwithkids","pctsinparhhld","pctnokids","pcthsgrad","pctsomecoll","pctrent","pctown","pct5to17","raceherfindahl","pcthisp","inctaxrate")
covs_list_t_plus_2 = c("pctwithkids","pctnokids","pctlesshs","pct18to64","pctwhite","pctblack","pctamerind","pctapi","pctmin","raceherfindahl","incherfindahl")
covs_list_t_plus_3 = c("pctlt5","pctwhite","pctblack","pctapi","pctotherrace","raceherfindahl","pctnevermarr","incherfindahl","inctaxrate")
covs_list_t_plus_4 = c("pctwithkids","pctsinparhhld","pctnokids","pctlesshs","pctrent","pctown","pct5to17","pct18to64","pctamerind","pctotherrace","incherfindahl")
covs_list_t_plus_5 = c("pop","pctsinparhhld","pctlesshs","pctsomecoll","unemprate","pctrent","pct18to64","pctwhite","pctblack","pctamerind","pctmin","raceherfindahl","incherfindahl")
covs_list_t_plus_6 = c("childpov","poverty","pctsinparhhld","unemprate","pct18to64","pctmarried","pctseparated")
covs_list_t_plus_7 = c("poverty","pctsinparhhld","unemprate","pctrent","pct18to64","pctamerind","pctotherrace","pctseparated")
covs_list_t_plus_8 = c("pop","pctsinparhhld","pctlesshs","pctrent","pct18to64","pctwhite","pctblack","pctamerind","raceherfindahl","pctseparated")
covs_list_t_plus_9 = c("pop","childpov","poverty","pctsinparhhld","unemprate","medfamy","pct65pls","pctwhite","pctblack","raceherfindahl","pctseparated","inctaxrate")
covs_list_t_plus_10 = c("unemprate","pctrent","pctlt5","pct18to64","pct65pls","pctblack","pctamerind","pctotherrace","pcthisp","pctseparated")



# lm(dfs_agg_covs$housing_roads_census_t_plus_2_matches %>% select(c("median_sale_amount",covs_list_t_plus_2)) %>% select(-c("pctmin")), 
#    formula = median_sale_amount ~ .) %>%
#   summary()



#============================================================================================================#
#    |- Calculating V.I.F for selected covariates ----
#============================================================================================================#

df <- dfs_agg_covs$housing_roads_census_t_plus_9_matches %>% 
  mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0))
rdrobust::rdrobust(y = df$median_sale_amount,
                   x = df$votes_pct_for, 
                   c = cutoff, all = TRUE, 
                   kernel = "uni", 
                   h = c(5, 5), 
                   vce = "hc0",
                   covs = df %>% select(c("pop", # total population of the county subdivision
                                          "pctsinparhhld", # proportion of households with own children under 18 living in the household, no spouse present
                                          "pctrent", # proportion of housing units that are renter-occupied
                                          "pctwhite", # proportion of white population
                                          "pctseparated", # proportion of population 15 and older married but separated
                                          "incherfindahl","pctmin"))
) %>% summary()


mod1 <- lm(formula = median_sale_amount ~  votes_pct_for_cntrd + treated + votes_pct_for_cntrd_abv + 
             pop + pctsinparhhld + pctrent + pctwhite + pctseparated + incherfindahl, 
           data = df %>% filter(abs(votes_pct_for_cntrd) <= 5) )
summary(mod1)

car::vif(mod1)