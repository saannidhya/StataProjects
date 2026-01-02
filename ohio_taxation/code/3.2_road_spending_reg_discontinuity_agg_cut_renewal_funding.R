#================================================================================================================#
# Purpose : Run RD after removing observations for a county subdivision's which failed to renew their tax levy
# Name    : Saani Rawat
# Created : 06/12/2023
# Problem: Then, I will have a large number of obs above cutoff and very few obs below cutoff
# Log     : 
#       06/12/2023: Started writing the code
#================================================================================================================#


# For each county subdivision, we remove all those observations that follow the first cut in funding i.e. even in "renewal tax levies", we remove
# observations that come after the first time the levy failed to pass
dfs_agg_cut_renewals <- purrr::map(dfs_agg, ~ .x %>%
                                     mutate(vote_result = if_else(votes_pct_for >= cutoff, 1, 0)) %>%
                                     group_by(tendigit_fips) %>%
                                     mutate(year_zero = ifelse(any(vote_result == 0), min(vote_year[vote_result == 0], na.rm = TRUE), 0)) %>%
                                     filter(vote_year <= year_zero | year_zero == 0)  %>%
                                     ungroup()
                           )


# Note: This gives us way too many passed levies vs failed levies
purrr::map(dfs_agg_cut_renewals, function(x){
  cat(paste0("Obs with passed levies: ", count(filter(x, vote_result == 1)),"\n"))
  cat(paste0("Obs with failed levies: ", count(filter(x, vote_result == 0)),"\n"))
}
)

purrr::map(dfs_agg_cut_renewals, function(x){
  cat(paste0("Obs with passed levies: ", count(filter(x, (vote_result == 1) & (votes_pct_for  >= 40) & (votes_pct_for  <= 60)) ),"\n"))
  cat(paste0("Obs with failed levies: ", count(filter(x, (vote_result == 0) & (votes_pct_for  >= 40) & (votes_pct_for  <= 60))),"\n"))
}
)


#=========================================#
# running regressions  ----
#=========================================#

### |- median sale amount ####

# storing univariate RDD models (outcome vs running variable, no covariates) from t-2 to t+10
regs_cut_renewals <- purrr::map(.x = dfs_agg_cut_renewals, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))
# regs_summary <- purrr::map(.x = dfs_agg, ~ summary(rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE)))

# no effect before the voting result was decided (passed/failed)
summary(regs_cut_renewals$housing_roads_census_t_minus_1_matches)
summary(regs_cut_renewals$housing_roads_census_t_minus_2_matches)

# Little effect immediately after the voting result was decided. It takes time to build roads and people to change their preferences.
# years 1, 2 and 3 after voting result was decided
summary(regs_cut_renewals$housing_roads_census_t_plus_1_matches)
summary(regs_cut_renewals$housing_roads_census_t_plus_2_matches)
summary(regs_cut_renewals$housing_roads_census_t_plus_3_matches)

# Effect starts to appear year 4 onwards and goes on to year 10. Roads have been built and people are starting to move in.
summary(regs_cut_renewals$housing_roads_census_t_plus_4_matches)
summary(regs_cut_renewals$housing_roads_census_t_plus_5_matches)
summary(regs_cut_renewals$housing_roads_census_t_plus_6_matches)
summary(regs_cut_renewals$housing_roads_census_t_plus_7_matches)
summary(regs_cut_renewals$housing_roads_census_t_plus_8_matches)
summary(regs_cut_renewals$housing_roads_census_t_plus_9_matches)
summary(regs_cut_renewals$housing_roads_census_t_plus_10_matches)




