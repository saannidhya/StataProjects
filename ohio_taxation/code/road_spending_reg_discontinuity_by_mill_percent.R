#================================================================================================================#
# Purpose : Split by millage size to see whether millage size affects housing price
# Name    : Saani Rawat
# Created : 06/16/2023
# Problem: 
# Log     : 
#       06/16/2023: 
#================================================================================================================#



# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))


#============================================================================================================#
#                         Splitting by millage size of > 1.9 and < 1.9 ----
#============================================================================================================#

dfs_agg_mill_g_1.9 <- purrr::map(dfs_agg_mill, ~ .x %>% filter(millage_percent > 1.9))

dfs_agg_mill_l_1.9 <- purrr::map(dfs_agg_mill, ~ .x %>% filter(millage_percent <= 1.9))

sort(unique(dfs_agg_mill$housing_roads_census_t_plus_4_matches$millage_percent))

#==================================================================================#
# running regressions (aggregate) for millage size > 1.9 ----
#==================================================================================#

### |- median sale amount ####

# storing univariate RDD models (outcome vs running variable, no covariates) from t-2 to t+10
regs_g_1.9 <- purrr::map(.x = dfs_agg_mill_g_1.9, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))

# summary(regs_g_1.9$housing_roads_census_t_minus_1_matches)
# summary(regs_g_1.9$housing_roads_census_t_minus_2_matches)
# 
# summary(regs_g_1.9$housing_roads_census_t_plus_1_matches)
# summary(regs_g_1.9$housing_roads_census_t_plus_2_matches)
# summary(regs_g_1.9$housing_roads_census_t_plus_3_matches)
# 
# summary(regs_g_1.9$housing_roads_census_t_plus_4_matches)
# summary(regs_g_1.9$housing_roads_census_t_plus_5_matches)
# summary(regs_g_1.9$housing_roads_census_t_plus_6_matches)
# summary(regs_g_1.9$housing_roads_census_t_plus_7_matches)
# summary(regs_g_1.9$housing_roads_census_t_plus_8_matches)
# summary(regs_g_1.9$housing_roads_census_t_plus_9_matches)
# summary(regs_g_1.9$housing_roads_census_t_plus_10_matches)


dist(dfs_agg_mill$housing_roads_census_t_plus_4_matches$millage_percent)

#==================================================================================#
# running regressions (aggregate) for millage size <= 1.9 ----
#==================================================================================#
regs_l_1.9 <- purrr::map(.x = dfs_agg_mill_l_1.9, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))

# summary(regs_l_1.9$housing_roads_census_t_minus_1_matches)
# summary(regs_l_1.9$housing_roads_census_t_minus_2_matches)
# 
# summary(regs_l_1.9$housing_roads_census_t_plus_1_matches)
# summary(regs_l_1.9$housing_roads_census_t_plus_2_matches)
# summary(regs_l_1.9$housing_roads_census_t_plus_3_matches)
# 
# summary(regs_l_1.9$housing_roads_census_t_plus_4_matches)
# summary(regs_l_1.9$housing_roads_census_t_plus_5_matches)
# summary(regs_l_1.9$housing_roads_census_t_plus_6_matches)
# summary(regs_l_1.9$housing_roads_census_t_plus_7_matches)
# summary(regs_l_1.9$housing_roads_census_t_plus_8_matches)
# summary(regs_l_1.9$housing_roads_census_t_plus_9_matches)
# summary(regs_l_1.9$housing_roads_census_t_plus_10_matches)





df_compare <- data.frame(regs_g_1.9 = purrr::map_dbl(regs_g_1.9, ~ .x$coef[2]), 
                         regs_l_1.9 = purrr::map_dbl(regs_l_1.9, ~ .x$coef[2]),
                         diff = purrr::map_dbl(regs_g_1.9, ~ .x$coef[2]) - purrr::map_dbl(regs_l_1.9, ~ .x$coef[2]))

df_compare







##################################################

for (i in seq(0,1.8, by = 0.1)){
  dfs_agg_mill_g_1.9 <- purrr::map(dfs_agg_mill, ~ .x %>% filter(millage_percent > (1.9 + i) ))
  
  dfs_agg_mill_l_1.9 <- purrr::map(dfs_agg_mill, ~ .x %>% filter(millage_percent <= (1.9 - i) ))

  #==================================================================================#
  # running regressions (aggregate) for millage size > 1.9 ----
  #==================================================================================#
  
  ### |- median sale amount ####
  
  # storing univariate RDD models (outcome vs running variable, no covariates) from t-2 to t+10
  regs_g_1.9 <- purrr::map(.x = dfs_agg_mill_g_1.9, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))

  #==================================================================================#
  # running regressions (aggregate) for millage size <= 1.9 ----
  #==================================================================================#
  regs_l_1.9 <- purrr::map(.x = dfs_agg_mill_l_1.9, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))
  
  
  
  df_compare <- data.frame(regs_g_1.9 = purrr::map_dbl(regs_g_1.9, ~ .x$coef[2]), 
                           regs_l_1.9 = purrr::map_dbl(regs_l_1.9, ~ .x$coef[2]),
                           diff = purrr::map_dbl(regs_g_1.9, ~ .x$coef[2]) - purrr::map_dbl(regs_l_1.9, ~ .x$coef[2]))
  
  print(paste0("cutoffs are: ", 1.9 + i, " and ", 1.9 - i))
  print(df_compare)

}

