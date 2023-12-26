#================================================================================================================#
# Purpose : Run RD based on urban vs rural distinction
# Name    : Saani Rawat
# Created : 06/13/2023
# Problem: Then, I will have a large number of obs above cutoff and very few obs below cutoff
# Log     : 
#       06/13/2023: 
#================================================================================================================#


# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))
source(paste0(code,"/utility_functions.R"))

#=================================================================================#
# Aggregate datasets without covariates ----
#=================================================================================#


# creating urban flags using clusterdummy and uadummy. 0 == rural and 1 == urban
dfs_agg_twp <- purrr::map(dfs_agg, ~ .x %>%
                                     left_join(twp_places_urban, by = "tendigit_fips") %>%
                                     mutate(urban_flg_cd = if_else(is.na(clusterdummy), 0, clusterdummy),
                                            urban_flg_ua = if_else(is.na(uadummy ), 0, uadummy)))

# splitting the datasets into urban and rural, by both: clusterdummy and uadummy
dfs_agg_urb_cd <- purrr::map(dfs_agg_twp, ~ .x %>% filter(urban_flg_cd == 1) %>% select(-(urban_flg_ua)))
dfs_agg_urb_ua <- purrr::map(dfs_agg_twp, ~ .x %>% filter(urban_flg_ua == 1) %>% select(-(urban_flg_cd)))
dfs_agg_rur_cd <- purrr::map(dfs_agg_twp, ~ .x %>% filter(urban_flg_cd == 0) %>% select(-(urban_flg_ua)))
dfs_agg_rur_ua <- purrr::map(dfs_agg_twp, ~ .x %>% filter(urban_flg_ua == 0) %>% select(-(urban_flg_cd)))


#=================================================================================#
# running regressions without covariates (aggregate) ----
#=================================================================================#
regs_urb_cd <- purrr::map(.x = dfs_agg_urb_cd, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))
regs_urb_ua <- purrr::map(.x = dfs_agg_urb_ua, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))
regs_rur_cd <- purrr::map(.x = dfs_agg_rur_cd, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))
regs_rur_ua <- purrr::map(.x = dfs_agg_rur_ua, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))


summary(regs_urb_cd$housing_roads_census_t_plus_4_matches)

# find_covs <- function(df, y, covs_list)
  


treatment_effect_summary(regs_urb_cd)
treatment_effect_summary(regs_urb_ua)
treatment_effect_summary(regs_rur_cd)
treatment_effect_summary(regs_rur_ua)


# regs_urb_cd$housing_roads_census_t_plus_6_matches$

# summary(regs_urb_cd$housing_roads_census_t_plus_4_matches)
# regs_urb_cd$housing_roads_census_t_plus_4_matches$Estimate
# 
# dfs_agg_urb_cd$housing_roads_census_t_plus_4_matches$votes_pct_for_cntrd <- dfs_agg_urb_cd$housing_roads_census_t_plus_4_matches$votes_pct_for - cutoff
# 
# dfs_agg_urb_cd$housing_roads_census_t_plus_4_matches <- mutate(dfs_agg_urb_cd$housing_roads_census_t_plus_4_matches,
#                                                                        treated = if_else(votes_pct_for_cntrd >= 0, 1, 0))
# 
# dfs_agg_urb_cd$housing_roads_census_t_plus_4_matches %>% View()
# 
# dfs_agg_urb_cd$housing_roads_census_t_plus_4_matches$treated
# 
# rr <- lm(data = dfs_agg_urb_cd$housing_roads_census_t_plus_4_matches, formula = median_sale_amount ~ votes_pct_for_cntrd + treated + treated*votes_pct_for_cntrd)
# summary(rr)

#=================================================================================#
# Aggregate datasets with covariates ----
#=================================================================================#

# creating urban flags using clusterdummy and uadummy. 0 == rural and 1 == urban
dfs_agg_covs_twp <- purrr::map(dfs_agg_covs, ~ .x %>%
                                 left_join(twp_places_urban, by = "tendigit_fips") %>%
                                 mutate(urban_flg_cd = if_else(is.na(clusterdummy), 0, clusterdummy),
                                        urban_flg_ua = if_else(is.na(uadummy ), 0, uadummy)))

# splitting the datasets into urban and rural, by both: clusterdummy and uadummy
dfs_agg_covs_urb_cd <- purrr::map(dfs_agg_covs_twp, ~ .x %>% filter(urban_flg_cd == 1) %>% select(-(urban_flg_ua)))
dfs_agg_covs_urb_ua <- purrr::map(dfs_agg_covs_twp, ~ .x %>% filter(urban_flg_ua == 1) %>% select(-(urban_flg_cd)))
dfs_agg_covs_rur_cd <- purrr::map(dfs_agg_covs_twp, ~ .x %>% filter(urban_flg_cd == 0) %>% select(-(urban_flg_ua)))
dfs_agg_covs_rur_ua <- purrr::map(dfs_agg_covs_twp, ~ .x %>% filter(urban_flg_ua == 0) %>% select(-(urban_flg_cd)))



#=================================================================================#
# running regressions with covariates (aggregate) ----
#=================================================================================#

covs_list_names <- gsub(pattern = "housing_roads_census", replacement = "covs_list", x = names(dfs_agg_covs))
covs_list_names_clean <- gsub(pattern = "_matches", replacement = "", x =covs_list_names)

# creating a list that contains all the covariate lists from t - 2 to t + 10
selected_covs_lists = mget(covs_list_names_clean)


# running regressions with covariates
regs_covs_urb_cd <- purrr::map2(dfs_agg_covs_urb_cd, selected_covs_lists ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_for, 
                                                     c = cutoff, 
                                                     covs = x %>% select(y),
                                                     all = TRUE)
                                }
            )

treatment_effect_summary(regs_covs_urb_cd)


regs_covs_urb_ua <- purrr::map2(dfs_agg_covs_urb_ua, selected_covs_lists ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_for, 
                                                     c = cutoff, 
                                                     covs = x %>% select(y),
                                                     all = TRUE)
                                }
)

regs_covs_rur_cd <- purrr::map2(dfs_agg_covs_rur_cd, selected_covs_lists ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_for, 
                                                     c = cutoff, 
                                                     covs = x %>% select(y),
                                                     all = TRUE)
                                }
)

regs_covs_rur_ua <- purrr::map2(dfs_agg_covs_rur_ua, selected_covs_lists ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_for, 
                                                     c = cutoff, 
                                                     covs = x %>% select(y),
                                                     all = TRUE)
                                }
)

treatment_effect_summary(regs_covs_urb_cd)
treatment_effect_summary(regs_covs_urb_ua)
treatment_effect_summary(regs_covs_rur_cd)
treatment_effect_summary(regs_covs_rur_ua)


