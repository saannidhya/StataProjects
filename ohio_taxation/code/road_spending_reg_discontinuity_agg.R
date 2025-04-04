#================================================================================================================#
# Purpose : Use median sale amount as outcome of interest for Road spending and Taxes project
# Name    : Saani Rawat
# Created : 07/16/2022
# Log     : 
#       07/16/2022: finished the program. Replicated in Stata as well. See road_spending_reg_discontinuity_agg.do
#       08/02/2022: added sale_amount_per_sq_feet as outcome of interest
#================================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))


#============================================================================================================#
#                         Aggregated Results (using median sale_amount for each county, vote and year) ----
#============================================================================================================#


#========================================#
# |- Manipulation test (X variable) ----
#========================================#
dens_tests <- purrr::map(dfs_agg, ~ rddensity::rddensity(X = .x$votes_pct_for, c = cutoff))
summary(dens_tests$housing_roads_census_t_minus_1_matches)
summary(dens_tests$housing_roads_census_t_minus_2_matches)
summary(dens_tests$housing_roads_census_t_plus_1_matches)
summary(dens_tests$housing_roads_census_t_plus_1_matches)
summary(dens_tests$housing_roads_census_t_plus_2_matches)
summary(dens_tests$housing_roads_census_t_plus_3_matches)
summary(dens_tests$housing_roads_census_t_plus_4_matches)
summary(dens_tests$housing_roads_census_t_plus_5_matches)
summary(dens_tests$housing_roads_census_t_plus_6_matches)
summary(dens_tests$housing_roads_census_t_plus_7_matches)
summary(dens_tests$housing_roads_census_t_plus_8_matches)
summary(dens_tests$housing_roads_census_t_plus_9_matches)
summary(dens_tests$housing_roads_census_t_plus_10_matches) # all passed!

# Mcrary test for t+10
# Give it the running variable and the cutpoint
# it will automatically produce a plot and select the number of bins and the bandwidth
# The output will be the p-value for the presence of a discontinuity
rdd::DCdensity(dfs_agg$housing_roads_census_t_plus_10_matches$votes_pct_for, c = 50)

#=========================#
# |- RD plots ----
#=========================#

dfs$housing_roads_census_t_minus_1_matches 

### median sale amount ###

# t+10 as an example (14 is the best)
for (i in 30:50){
  rdrobust::rdplot(y = dfs_agg$housing_roads_census_t_plus_9_matches$median_sale_amount, 
                   x = dfs_agg$housing_roads_census_t_plus_9_matches$votes_pct_for, 
                   c = 50, p = 1, nbins = c(i,i), title = paste0(as.character(i)))
}

purrr::map2(dfs_agg, names(dfs_agg), ~print(rdrobust::rdplot(y = .x$median_sale_amount, 
                                                             x = .x$votes_pct_for, 
                                                             c = 50, p = 1, title = .y)))

### sale_amount_per_sq_feett ###
for (i in 30:50){
  rdrobust::rdplot(y = dfs_agg$housing_roads_census_t_plus_9_matches$median_sale_amount, 
                   x = dfs_agg$housing_roads_census_t_plus_9_matches$votes_pct_for, 
                   c = 50, p = 1, nbins = c(i,i), title = paste0(as.character(i)))
}

purrr::map2(dfs_agg_per, names(dfs_agg_per), ~print(rdrobust::rdplot(y = .x$median_sale_amount_per_sq_feet, 
                                                                     x = .x$votes_pct_for, 
                                                                     c = 50, p = 1, title = .y)))


#=========================================#
# running regressions (aggregate) ----
#=========================================#

### median sale amount ###

# storing univariate RDD models (outcome vs running variable, no covariates) from t-2 to t+10
regs <- purrr::map(.x = dfs_agg, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))
# regs_summary <- purrr::map(.x = dfs_agg, ~ summary(rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE)))

# no effect before the voting result was decided (passed/failed)
summary(regs$housing_roads_census_t_minus_1_matches)
summary(regs$housing_roads_census_t_minus_2_matches)

# Little effect immediately after the voting result was decided. It takes time to build roads and people to change their preferences.
# years 1, 2 and 3 after voting result was decided
summary(regs$housing_roads_census_t_plus_1_matches)
summary(regs$housing_roads_census_t_plus_2_matches)
summary(regs$housing_roads_census_t_plus_3_matches)

# Effect starts to appear year 4 onwards and goes on to year 10. Roads have been built and people are starting to move in.
summary(regs$housing_roads_census_t_plus_4_matches)
summary(regs$housing_roads_census_t_plus_5_matches)
summary(regs$housing_roads_census_t_plus_6_matches)
summary(regs$housing_roads_census_t_plus_7_matches)
summary(regs$housing_roads_census_t_plus_8_matches)
summary(regs$housing_roads_census_t_plus_9_matches)
summary(regs$housing_roads_census_t_plus_10_matches)


# lm() and rdrobust() giving exactly the same coefficient
# narrow2 <- dfs_agg$housing_roads_census_t_plus_9_matches %>% 
#   mutate(above50 = if_else(votes_pct_for >= cutoff, 1, 0),
#          x = votes_pct_for - cutoff,
#          x_above = above50*x) 
# mod1 <- lm(median_sale_amount ~ above50 + x + x_above, data = filter(narrow2, abs(x) <= 10))
# summary(mod1)
# reg1 <- rdrobust::rdrobust(y = narrow2$median_sale_amount, x = narrow2$x, c = 0, all = TRUE, kernel = "uni", h = 10, vce="hc0")
# summary(reg1)  

# using local randomization method;
rdrandinf(dfs_agg$housing_roads_census_t_plus_9_matches$median_sale_amount, 
          dfs_agg$housing_roads_census_t_plus_9_matches$votes_pct_for, 
          cutoff = cutoff)


### sale_amount_per_sq_feett ###
regs_per <- purrr::map(.x = dfs_agg_per, ~ rdrobust::rdrobust(y = .x$median_sale_amount_per_sq_feet, x = .x$votes_pct_for, c = cutoff, all = TRUE))


purrr::map(regs_per, .f = ~ .x %>% summary() )

summary(regs_per$housing_roads_census_t_minus_1_matches)
summary(regs_per$housing_roads_census_t_minus_2_matches)
summary(regs_per$housing_roads_census_t_plus_1_matches)
summary(regs_per$housing_roads_census_t_plus_2_matches)
summary(regs_per$housing_roads_census_t_plus_3_matches)
summary(regs_per$housing_roads_census_t_plus_4_matches)
summary(regs_per$housing_roads_census_t_plus_5_matches)
summary(regs_per$housing_roads_census_t_plus_6_matches)
summary(regs_per$housing_roads_census_t_plus_7_matches)
summary(regs_per$housing_roads_census_t_plus_8_matches)
summary(regs_per$housing_roads_census_t_plus_9_matches)
summary(regs_per$housing_roads_census_t_plus_10_matches)



#=========================================#
# |- Introducing covariates ----
#=========================================#




create_bw_dfs <- function(df_list, cutoff, bandwidth){
  purrr::map(df_list, ~ .x %>% filter(between(votes_pct_for, cutoff-bandwidth, cutoff+bandwidth)))
}

dfs_agg_5 <- create_bw_dfs(dfs_agg, 50, 5)
dfs_agg_10 <- create_bw_dfs(dfs_agg, 50, 10)

# 5 seems to be working better than 10 (10 may be too wide)
regs_5 <- purrr::map(.x = dfs_agg_5, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))
summary(regs_5$housing_roads_census_t_plus_4_matches)

regs_10 <- purrr::map(.x = dfs_agg_10, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE))
summary(regs_10$housing_roads_census_t_plus_1_matches)
summary(regs_10$housing_roads_census_t_plus_2_matches)
summary(regs_10$housing_roads_census_t_plus_3_matches)
summary(regs_10$housing_roads_census_t_plus_4_matches)
summary(regs_10$housing_roads_census_t_plus_5_matches)
summary(regs_10$housing_roads_census_t_plus_6_matches)
summary(regs_10$housing_roads_census_t_plus_7_matches)
summary(regs_10$housing_roads_census_t_plus_8_matches)
summary(regs_10$housing_roads_census_t_plus_9_matches)
summary(regs_10$housing_roads_census_t_plus_10_matches)

?rdrobust::rdrobust



mod <- rdrobust::rdrobust(y = dfs_agg_covars$housing_roads_census_t_plus_7_matches$median_sale_amount, 
                   x = dfs_agg_covars$housing_roads_census_t_plus_7_matches$votes_pct_for, 
                   c = cutoff, covs = dfs_agg_covars$housing_roads_census_t_plus_7_matches[,4:7], all = T)

mod$bws
