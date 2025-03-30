###################################################################################################################################################
# created: 03/30/2024
# by: Saani Rawat
# purpose: perform Miscellaneous Robustness Tests
# Log:
# 1. 03/30/2024 : Created the script. Started with Great Financial Crisis (GFC) Robustness Test
###################################################################################################################################################

# Load Libraries
library(tidyverse)
library(lubridate)

#-----------------------------------------#
# how to capture Great Financial Crisis?
#-----------------------------------------#

# method 1: remove any voting that happened in 2007-2009
dfs_agg_covs_gfc1 <-  map(dfs_agg_covs, ~ .x %>%  filter(!between(vote_year, 2007, 2009)))
gs_gfc1 <- purrr::map2(covs_final, dfs_agg_covs_gfc1, .f = function(x,y){
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_gs_gfc1 <- te_tables(gs_gfc1)
plot_te(tes_gs_gfc1, title = "Treatment Effect Estimates: Median House Price ", subtitle = "(after removing vote_year during Great Recession 2007-09)")

# method 2: remove any housing price years that happened in 2007-2009. Meaning, if voting happened in 2005, but housing price
# data is from 2007-2009, then remove that voting data from 2007-2009
dfs_agg_covs_gfc2 <-  map(dfs_agg_covs, ~ .x %>%  filter(!between(year, 2007, 2009)))
gs_gfc2 <- purrr::map2(covs_final, dfs_agg_covs_gfc2, .f = function(x,y){
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_gs_gfc2 <- te_tables(gs_gfc2)
plot_te(tes_gs_gfc2, title = "Treatment Effect Estimates: Median House Price", subtitle = "(after removing year during Great Recession 2007-09)")

# method 3: split sample into < 2006 and 2006-2021 to capture GFC
dfs_agg_covs_gfc3 <-  map(dfs_agg_covs, ~ .x %>%  filter(!vote_year <= 2006))
gs_gfc3 <- purrr::map2(covs_final, dfs_agg_covs_gfc3, .f = function(x,y){
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_gs_gfc3 <- te_tables(gs_gfc3)
plot_te(tes_gs_gfc3, title = "Treatment Effect Estimates: Median House Price", subtitle = "(years 2006 and before only)")

dfs_agg_covs_gfc4 <-  map(dfs_agg_covs, ~ .x %>%  filter(!vote_year > 2006))
gs_gfc4 <- purrr::map2(covs_final, dfs_agg_covs_gfc4, .f = function(x,y){
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_gs_gfc4 <- te_tables(gs_gfc4)
plot_te(tes_gs_gfc4, title = "Treatment Effect Estimates: Median House Price", subtitle = "(years after 2006 only)")

#-----------------------------------------#
# Difference in Discontinuities
#-----------------------------------------#

