#==========================================================================================================#
# Purpose : Build on road_spending_reg_discontinuity_agg.R and add covariates to the regression
# Name    : Saani Rawat
# Created : 07/30/2022
# Log     : 
#           08/01/2022: First step of adding covariates to RDD
#           06/17/2023: combined several previously made changes. See github repo for details
#           10/25/2023: Ran RDD on dfs_agg_pure_covs (data uncontaminated by passed additional levies)
#           10/18/2024: Adding House Price Growth outcome variable to RDD
#           11/16/2024: regression updates
#           12/16/2024: Adding reference point for treatment effect estimates of t-1
#           01/10/2025: Adding year F.E to the regressions, as per Cellini, Ferreira, and Rothstein (2010)
#==========================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

source(paste0(code,"/utility_functions.R"))

# running data setup code
source(paste0(code,"/housing_data_setup.R"))
# running covariates balance test
# source(paste0(code,"/covariates_balance_test.R"))
# uncontaminated datasets
source(paste0(code,"/roads_data_setup.R"))


# user-defined parameters  ----
t_test_sig_level <- 0.05


# fips_map <- readxl::read_excel(paste0(data,"/ohio-only-all-geocodes-2016.xlsx")) %>%
#   select(any_of(c("TENDIGIT_FIPS","name (note if split between two counties)"))) %>%
#   rename(name = "name (note if split between two counties)") %>% janitor::clean_names()
# 
# roads_and_census %>% filter(between(votes_pct_against, 40, 60)) %>%
#   filter(treated == 1) %>%
#   select(tendigit_fips, year, votes_pct_against) %>%
#   group_by(tendigit_fips) %>%
#   summarise(min_year = min(year), max_year = max(year), count = n(), mean_votes_pct_against = mean(votes_pct_against)) %>%
#   arrange(desc(count)) %>% 
#   filter(max_year >= 2000) %>%
#   left_join(fips_map, by = "tendigit_fips") %>% readr::write_csv(paste0(data,"/outputs/tables/rd_fips_close_elections_bw10_.csv"))
# 
#============================================================================================================#
#     Introducing covariates (using median sale_amount for each county, vote and year)  into regression ---- 
#============================================================================================================#

# covariates which passed t-test (failed to reject null) for the specific dataset and are not potential outcome vars
# var_list <- purrr::map(names(dfs_agg_covs), 
#                        ~ all_tests %>%
#                          filter((dataset == .x) & 
#                                   (`p-value` > t_test_sig_level) & 
#                                   !(variable %in% c('median_sale_amount','median_ln_sale_amount','median_sale_amount_per_sq_feet'))) %>%
#                          select(variable) %>%
#                          pull() )


# names(var_list) <- names(dfs_agg_covs)
covs_list <- c("pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl")

# some common vars upon eye-balling: pop, poverty, pct_white, pctsinparhhld, medfamy

### overall sample mean

## mean house value, keeping only t + 0 to t + 10, after removing 1%
map_dbl(winsorize_data(dfs, "sale_amount", lower = 0.01, upper = 0.99, na.rm = TRUE), ~mean(.x$sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()
map_dbl(winsorize_data(dfs, "sale_amount", lower = 0.01, upper = 0.99, na.rm = TRUE), ~sd(.x$sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()
map(winsorize_data(dfs, "sale_amount", lower = 0.01, upper = 0.99, na.rm = TRUE), ~summary(.x$sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] 

# map_dbl(dfs_agg, ~mean(.x$median_sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()
# map_dbl(dfs_agg, ~sd(.x$median_sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()

### regressions with covariates for median sale amount

#|||||||||||||||||||||||||||||||||||||||||||||||||#
# Different Covariates for each outcome period
#|||||||||||||||||||||||||||||||||||||||||||||||||#

# selecting the best set of covariates for each median sale amount period
covs_final <- purrr::map(dfs_agg_covs, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))

# cleaning covs_final to avoid multicollinearity
covs_final$housing_roads_census_t_minus_3_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5")
covs_final$housing_roads_census_t_minus_2_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5")
covs_final$housing_roads_census_t_minus_1_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5")
covs_final$housing_roads_census_t_plus_0_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5")
# covs_final$housing_roads_census_t_plus_0_matches <- covs_final$housing_roads_census_t_plus_0_matches[!(covs_final$housing_roads_census_t_plus_0_matches %in% c("pctmin"))]
covs_final$housing_roads_census_t_plus_1_matches <- c("pop", "pctblack", "unemprate", "pctrent")
# covs_final$housing_roads_census_t_plus_1_matches <- covs_final$housing_roads_census_t_plus_1_matches[!(covs_final$housing_roads_census_t_plus_1_matches %in% c("pctmin"))]
covs_final$housing_roads_census_t_plus_2_matches <- c("pop",covs_final$housing_roads_census_t_plus_2_matches[!(covs_final$housing_roads_census_t_plus_2_matches %in% c("pctnokids", "pctmin", "pct5to17", "pctblack", "pop"))])
covs_final$housing_roads_census_t_plus_3_matches <- covs_final$housing_roads_census_t_plus_3_matches[!(covs_final$housing_roads_census_t_plus_3_matches %in% c("pctnokids"))]
covs_final$housing_roads_census_t_plus_4_matches <- covs_final$housing_roads_census_t_plus_4_matches[!(covs_final$housing_roads_census_t_plus_4_matches %in% c("pctnokids","pctown"))]
covs_final$housing_roads_census_t_plus_5_matches <- c("pctwithkids", "pctsinparhhld", "unemprate", "pctrent", "pctlt5", "pctblack")
# covs_final$housing_roads_census_t_plus_7_matches <- covs_final$housing_roads_census_t_plus_7_matches[!(covs_final$housing_roads_census_t_plus_7_matches %in% c("pctmin"))]
covs_final$housing_roads_census_t_plus_8_matches <- covs_final$housing_roads_census_t_plus_8_matches[!(covs_final$housing_roads_census_t_plus_8_matches %in% c("pctmin", "pctnokids"))]
covs_final$housing_roads_census_t_plus_9_matches <- c("medfamy", "poverty", "pctsinparhhld", "pctrent", "pct18to64", "pctmarried")
covs_final$housing_roads_census_t_plus_10_matches <- covs_final$housing_roads_census_t_plus_10_matches[!(covs_final$housing_roads_census_t_plus_10_matches %in% c("pctmin"))]


# beepr::beep("mario")
# 
# sel_covs_10
# using forward selection function 
# dfs_agg_covss <- map(dfs_agg_covs, ~ .x %>% 
#                        mutate(treated = ifelse( votes_pct_against > cutoff, 1, 0),
#                               treat_times_votes = votes_pct_against * treated))
# covs_fwd <- purrr::map(dfs_agg_covss, ~forward_selection_covs(.x, y = "median_sale_amount", x = "votes_pct_against", covs_list = covs_list))

## local polynomial approach ##
# t + 10, t + 5, pctnokids
# names(dfs_agg_covs$housing_roads_census_t_minus_1_matches)

gs <- purrr::map2(covs_final, dfs_agg_covs, .f = function(x,y){
                              # print(paste0("Covariates list: ", deparse(substitute(y))))
                              # print(paste0("Covariates list: ", x))
                              rdrobust(  y = y$median_sale_amount,
                                         x = y$votes_pct_against,
                                         c = cutoff,
                                         covs = y %>%
                                           select(x) ,
                                         all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = y$tendigit_fips) })
purrr::walk2(names(gs), gs, .f = function(x, y) {
  print(paste0("Outcome variable is ",x))
  summary(y) 
})
tes_gs <- te_tables(gs)
plot_te(tes_gs, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")
plot_te_recenter(tes_gs, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")

tes_gs %>% filter(ord >= 0) %>% select(robust_coef) %>% pull %>% mean / 166000

# get mean optimal bandwidth
purrr::walk2(gs, names(gs), ~print(paste0("Eff. Bandwidth (h) for ", .y, ": " , round(.x$bws[1,],1))))

#==============================================#
# Adding Time Fixed Effects
#==============================================#

dfs_agg_covs_w_tfe <- map(dfs_agg_covs, ~ dummy_cols(.x, select_columns = c("year"), remove_first_dummy = TRUE) %>% relocate(starts_with("year_"), .after = "year"))

# dummy_cols(dfs_agg_covs$housing_roads_census_t_plus_0_matches, select_columns = c("year"), remove_first_dummy = TRUE) %>% 
#   relocate(starts_with("year_"), .after = "year")

dfs_agg_covs_tfe_names <- map(dfs_agg_covs_w_tfe, ~ colnames(.x) %>% grep("year_", ., value = TRUE))

covs_final_w_tfe <- map2(covs_final, dfs_agg_covs_tfe_names, ~c(.x, .y))

gs_reg <- purrr::map2(covs_final_w_tfe, dfs_agg_covs_w_tfe, .f = function(x,y){
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = y$tendigit_fips)
})
plot_te(tes_gs_reg, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")
plot_te_recenter(tes_gs_reg, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")

tes_gs_reg %>% filter((ord >= 0)) %>%
  select(robust_coef) %>% pull %>% mean

# average effective bandwidth
map_dbl(gs_reg[4:14], ~ .x$bws[1,1]) %>% mean
# 9.425709

map_dbl(gs_reg[4:14], ~ .x$bws[1,1]) # Effective (h)
map_dbl(gs_reg[4:14], ~ .x$bws[2,1]) # bias (b)
map_dbl(gs_reg[4:14], ~ sum(.x$N) ) # Total obs
map_dbl(gs_reg[4:14], ~ sum(.x$N_h) ) # Effective obs


#------------------------------------------------------------------------------------------------#

# from gs list, select elements that start with "housing_roads_census_t_plus_"
tes_gs %>% filter((ord >= 0)) %>%
  select(robust_coef) %>% pull %>% mean
coef <- tes_gs %>% filter((ord >= 4)) %>%
  select(robust_coef) %>% pull %>% mean # used in paper
base <- map_dbl(dfs, ~mean(.x$sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()
coef/base

# output for paper draft 
# write.csv(tes_gs, paste0(tables, "/tes_gs.csv"), row.names = FALSE)


# nrow(dfs_agg_covs$housing_roads_census_t_plus_1_matches)
# gs$housing_roads_census_t_plus_1_matches$coef/gs$housing_roads_census_t_plus_1_matches$se
# gs$housing_roads_census_t_plus_1_matches$bwselect
# 
# gs$housing_roads_census_t_plus_4_matches$Estimate[2]
# 
# gs$housing_roads_census_t_plus_1_matches$bws
# 
# mean(c(20992, 19867, 14461, 15129, 18532, 22645, 13438))

# bandwidth
purrr::walk2(gs, names(gs), ~print(paste0("Eff. Bandwidth (h) for ", .y, ": " , round(.x$bws[1,],1))))
purrr::walk2(gs, names(gs), ~print(paste0("Bias Bandwidth (b) for ", .y, ": " , round(.x$bws[2,],1))))

# observations
purrr::walk2(gs, names(gs), ~print(paste0("Eff. Observations for ", .y, ": " , sum(.x$N_h))))
purrr::walk2(gs, names(gs), ~print(paste0("Total Observations for ", .y, ": " , sum(.x$N))))

# mean bandwidth - left
map_dbl(gs, ~ round(.x$bws[1,1],1)  )


#============================================================================================================#
#     Robustness Test: 1% Winsorization ----
#============================================================================================================#

## winsorization of median_sale_amount
dfs_winsorized <- winsorize_data(dfs, "sale_amount", na.rm = TRUE)
# mean house value, keeping only t + 0 to t + 10
map_dbl(dfs_winsorized, ~mean(.x$sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()
map_dbl(dfs_winsorized, ~sd(.x$sale_amount, na.rm = TRUE)) %>% .[grepl("t_plus", names(.)) ] %>% mean()

# removing top and bottom 1% of observations
dfs_agg_covs_winsored <- winsorize_data(dfs_agg_covs, "median_sale_amount")

# local polynomial approach
g_regs_w <- purrr::map2(covs_final, dfs_agg_covs_winsored, .f = function(x,y){
  # print(paste0("Outcome variable is ",deparse(substitute(y))))
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>% select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = y$tendigit_fips)
})
tes_g_w <- te_tables(g_regs_w)
plot_te(tes_g_w)
plot_te_recenter(tes_g_w)

#==============================================#
# Adding Time Fixed Effects: 1% winsorized
#==============================================#

dfs_agg_covs_win_tfe <- map(dfs_agg_covs_winsored, ~ dummy_cols(.x, select_columns = c("year"), remove_first_dummy = TRUE) %>% relocate(starts_with("year_"), .after = "year"))

# dummy_cols(dfs_agg_covs$housing_roads_census_t_plus_0_matches, select_columns = c("year"), remove_first_dummy = TRUE) %>% 
#   relocate(starts_with("year_"), .after = "year")

dfs_agg_covs_win_tfe_names <- map(dfs_agg_covs_win_tfe, ~ colnames(.x) %>% grep("year_", ., value = TRUE))

covs_final_win_tfe <- map2(covs_final, dfs_agg_covs_win_tfe_names, ~c(.x, .y))

gs_reg_win <- purrr::map2(covs_final_win_tfe, dfs_agg_covs_win_tfe, .f = function(x,y){
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = y$tendigit_fips)
})

tes_gs_reg_win <- te_tables(gs_reg_win)
plot_te(tes_gs_reg_win, title = "Treatment Effect Estimates: Median House Price", subtitle = "after 1% Winsorization")
plot_te_recenter(tes_gs_reg_win, title = "Treatment Effect Estimates: Median House Price", subtitle = "after 1% Winsorization")

# average effective bandwidth
map_dbl(gs_reg_win[4:14], ~ .x$bws[1,1]) %>% mean

#============================================================================================================#
#     Introducing covariates (using median sale_amount per square feet)  into regression ---- 
#============================================================================================================#

# median sale amount per square feet
covs_final_per <- purrr::map(dfs_agg_per_covs, ~find_covs(.x, y = "median_sale_amount_per_sq_feet", covs_list = covs_list))
## 
# beepr::beep("mario")


### regressions with covariates for median sale amount per square feet
f1 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_1_matches$median_sale_amount_per_sq_feet,
                 x = dfs_agg_per_covs$housing_roads_census_t_plus_1_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_per_covs$housing_roads_census_t_plus_1_matches %>%
                   select(covs_final_per$housing_roads_census_t_plus_1_matches) ,
                 all = TRUE) 
f2 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_2_matches$median_sale_amount_per_sq_feet,
                 x = dfs_agg_per_covs$housing_roads_census_t_plus_2_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_per_covs$housing_roads_census_t_plus_2_matches %>%
                   select(covs_final_per$housing_roads_census_t_plus_2_matches) ,
                 all = TRUE) 
f3 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_3_matches$median_sale_amount_per_sq_feet,
                 x = dfs_agg_per_covs$housing_roads_census_t_plus_3_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_per_covs$housing_roads_census_t_plus_3_matches %>%
                   select(covs_final_per$housing_roads_census_t_plus_3_matches) %>% select(-c(pctblack, pctmin)) ,
                 all = TRUE) 
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
                   select(covs_final_per$housing_roads_census_t_plus_5_matches) %>% select(-c(pctmin)) ,
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
                   select(covs_final_per$housing_roads_census_t_plus_7_matches) %>% select(-(pctnokids)) ,
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
                   select(covs_final_per$housing_roads_census_t_plus_9_matches) %>% select(-c(pctmin)) ,
                 all = TRUE) 
f10 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_10_matches$median_sale_amount_per_sq_feet,
                  x = dfs_agg_per_covs$housing_roads_census_t_plus_10_matches$votes_pct_for,
                  c = cutoff,
                  covs = dfs_agg_per_covs$housing_roads_census_t_plus_10_matches %>%
                    select(covs_final_per$housing_roads_census_t_plus_10_matches) ,
                  all = TRUE) 

f_regs <- list(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)



#============================================================================================================#
#    |- Calculating V.I.F for selected covariates ----
#============================================================================================================#

df <- dfs_agg_covs$housing_roads_census_t_plus_9_matches %>% 
  mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0),
         votes_pct_for_cntrd = votes_pct_for - cutoff,
         votes_pct_for_cntrd_abv = votes_pct_for_cntrd*treated)
# df$votes_pct_for_cntrd

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


# beepr::beep("mario")

#============================================================================================================#
#   Robustness Test:  Aggregated Results (using roads_agg_pure_covs i.e. uncontaminated dataset) ----
#============================================================================================================#

# using median_sale_amount
# covs_my_list <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctlesshs", "pctsinparhhld", "pctlt5")

## find covariates
covs_final_un <- purrr::map(dfs_agg_pure_covs, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))

# cleaning covs_final to avoid multicollinearity
covs_final_un$housing_roads_census_t_minus_3_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5")
covs_final_un$housing_roads_census_t_minus_2_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5")
covs_final_un$housing_roads_census_t_minus_1_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5")
covs_final_un$housing_roads_census_t_plus_0_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5")
covs_final_un$housing_roads_census_t_plus_1_matches <- c("pop")
covs_final_un$housing_roads_census_t_plus_2_matches <- covs_final_un$housing_roads_census_t_plus_2_matches[!(covs_final_un$housing_roads_census_t_plus_2_matches %in% c("pctnokids"))]
covs_final_un$housing_roads_census_t_plus_3_matches <- c("pctwhite")
covs_final_un$housing_roads_census_t_plus_4_matches <- covs_final_un$housing_roads_census_t_plus_4_matches[!(covs_final_un$housing_roads_census_t_plus_4_matches %in% c("pctown"))]
covs_final_un$housing_roads_census_t_plus_7_matches <- covs_final_un$housing_roads_census_t_plus_7_matches[!(covs_final_un$housing_roads_census_t_plus_7_matches %in% c("pctown"))]
covs_final_un$housing_roads_census_t_plus_9_matches <- covs_final_un$housing_roads_census_t_plus_9_matches[!(covs_final_un$housing_roads_census_t_plus_9_matches %in% c("pctown"))]


## run regressions
g_p_regs <- purrr::map2(covs_final_un, dfs_agg_pure_covs, .f = function(x,y){
                                                        rdrobust(  y = y$median_sale_amount,
                                                                   x = y$votes_pct_against,
                                                                   c = cutoff,
                                                                   covs = y %>%
                                                                     select(x) ,
                                                                   all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_g_p <- te_tables(g_p_regs)
plot_te(tes_g_p, title = "T.E Estimates: Uncontaminated voting data")

# bandwidth
purrr::walk2(g_p_regs, names(g_p_regs), ~print(paste0("Eff. Bandwidth (h) for ", .y, ": " , round(.x$bws[1,],1))))
purrr::walk2(g_p_regs, names(g_p_regs), ~print(paste0("Bias Bandwidth (b) for ", .y, ": " , round(.x$bws[2,],1))))

# observations
purrr::walk2(g_p_regs, names(g_p_regs), ~print(paste0("Eff. Observations for ", .y, ": " , sum(.x$N_h))))
purrr::walk2(g_p_regs, names(g_p_regs), ~print(paste0("Total Observations for ", .y, ": " , sum(.x$N))))

# output for paper draft 
# write.csv(tes_g_p, paste0(tables, "/tes_g_p.csv"), row.names = FALSE)


###=== Adding Time F.E ===###

dfs_agg_pure_covs_w_tfe <- map(dfs_agg_pure_covs, ~ dummy_cols(.x, select_columns = c("year"), remove_first_dummy = TRUE) %>% relocate(starts_with("year_"), .after = "year"))

# merging covariates with time dummies
dfs_agg_pure_covs_tfe_names <- map(dfs_agg_pure_covs_w_tfe, ~ colnames(.x) %>% grep("year_", ., value = TRUE))
covs_final_un_w_tfe <- map2(covs_final, dfs_agg_pure_covs_tfe_names, ~c(.x, .y))

gs_reg_pure <- purrr::map2(covs_final_un_w_tfe, dfs_agg_pure_covs_w_tfe, .f = function(x,y){
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})

tes_gs_reg_pure <- te_tables(gs_reg_pure)
plot_te(tes_gs_reg_pure, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")
plot_te_recenter(tes_gs_reg_pure, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")



map_dbl(gs_reg_pure, ~ .x$bws[1,1]) # eff bw
map_dbl(gs_reg_pure, ~ .x$bws[2,1]) # bias bw

# total obs
map_dbl(dfs_agg_pure_covs , ~ nrow(.x))

gs_reg_pure

# y <- dfs_agg_pure_covs_w_tfe$housing_roads_census_t_plus_5_matches
# x <- c("pctwithkids", "pctsinparhhld", "unemprate", "pctrent", "pctlt5", "pctblack", "year_1997", "year_1998", "year_1999", "year_2000", "year_2001", "year_2002", "year_2003", "year_2004", "year_2005", "year_2006", "year_2007", "year_2008", "year_2009", "year_2010", "year_2011", "year_2012", "year_2013", "year_2014", "year_2015", "year_2016", "year_2017", "year_2018", "year_2019", "year_2020", "year_2021")
# 
# 
# rdrobust(  y = y$median_sale_amount,
#            x = y$votes_pct_against,
#            c = cutoff,
#            covs = y %>%
#              select(x) ,
#            all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2) %>% summary


#============================================================================================================#
#     Outcome variable: Housing Price Growth ---- 
#============================================================================================================#

# housing price growth
hs_agg_mgd$housing_roads_census_t_plus_1_matches

hs_agg_mgd_ <- purrr::map(hs_agg_mgd, ~na.omit(.x))

roads_and_census %>% filter(between(votes_pct_against, 40,60)) %>%
  filter(year >= 2008) %>%
  select(tendigit_fips) %>% unique() %>% nrow()

# selecting the best set of covariates for each housing price growth period
covs_final_gr <- purrr::map(hs_agg_mgd_, ~find_covs(.x, y = "median_sale_amount_growth", covs_list = covs_list))

nrow(hs_agg_mgd_$housing_roads_census_t_plus_0_matches)

roads_and_census %>% filter(between(votes_pc))

map_dbl(hs_agg_mgd_, ~nrow(.x %>% filter(between(votes_pct_against, 40,60))))

# run regressions



# remove NAs from datasets
hs_agg_mgd_ <- purrr::map(hs_agg_mgd, ~na.omit(.x))

gs_gr <- purrr::map2(covs_final_gr, hs_agg_mgd, .f = function(x,y){
  # print(paste0("Covariates list: ", deparse(substitute(y))))
  # print(paste0("Covariates list: ", x))
  rdrobust(  y = y$median_sale_amount_growth,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})