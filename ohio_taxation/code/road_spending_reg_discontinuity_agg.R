#================================================================================================================#
# Purpose : Use median sale amount as outcome of interest for Road spending and Taxes project
# Name    : Saani Rawat
# Created : 07/16/2022
# Log     : 
#       07/16/2022: finished the program. Replicated in Stata as well. See road_spending_reg_discontinuity_agg.do
#       08/02/2022: added sale_amount_per_sq_feet as outcome of interest
#       06/17/2023: added utility_functions.R to the code
#       10/25/2023: Ran RDD on dfs_agg_pure (data uncontaminated by passed additional levies)
#       11/23/2023: made changes to density tests- did them before introducing outcome vars
#       02/03/2024: switching to votes_pct_against
#================================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")

# running data setup code
# source(paste0(code,"/ohio_road_housing_census_merge.R"))
source(paste0(code,"/housing_data_setup.R"))
source(paste0(code,"/utility_functions.R"))

#============================================================================================================#
#                         Aggregated Results (using median sale_amount for each county, vote and year) ----
#============================================================================================================#


#========================================#
# |- Manipulation test (X variable) ----
#========================================#

dens_test <- rddensity::rddensity(X = roads_and_census$votes_pct_against, c = cutoff, massPoints = FALSE)
summary(dens_test)

dens_test$test$p_jk

# checking for duplicate values based on the running variable
# dp <- dups$housing_roads_census_t_plus_1_matches[duplicated(dups$housing_roads_census_t_plus_1_matches) | duplicated(dups$housing_roads_census_t_plus_1_matches, fromLast = TRUE)]
# dfs_agg$housing_roads_census_t_plus_1_matches %>% filter(votes_pct_for == dp[2])
# roads_and_census %>% filter(votes_pct_for == dp[2]) %>% View()

# range of p-vals after introducing outcome  variables
purrr::map_dbl(dfs_agg, ~ rddensity::rddensity(X = .x$votes_pct_against, c = cutoff, massPoints = FALSE)$test$p_jk) %>% min() %>% round(2)
purrr::map_dbl(dfs_agg, ~ rddensity::rddensity(X = .x$votes_pct_against, c = cutoff, massPoints = FALSE)$test$p_jk) %>% max() %>% round(2)


# Mcrary test
# Give it the running variable and the cutpoint
# it will automatically produce a plot and select the number of bins and the bandwidth
# The output will be the p-value for the presence of a discontinuity
rdd::DCdensity(roads_and_census$votes_pct_against, c = 50)

#==========================================#
# |- RD plots (used Stata for plots) ----
#==========================================#

### median sale amount ###

# t+10 as an example (14 is the best)
rdrobust::rdplot(dfs_agg$housing_roads_census_t_plus_2_matches$median_sale_amount,
                 dfs_agg$housing_roads_census_t_plus_2_matches$votes_pct_against,
                 c = 50, p = 1, binselect = "esmv")
rdrobust::rdplot(dfs_agg$housing_roads_census_t_plus_4_matches$median_sale_amount,
                 dfs_agg$housing_roads_census_t_plus_4_matches$votes_pct_against,
                 c = 50, p = 1, binselect = "esmv")
rdrobust::rdplot(dfs_agg$housing_roads_census_t_plus_5_matches$median_sale_amount,
                 dfs_agg$housing_roads_census_t_plus_5_matches$votes_pct_against,
                 c = 50, p = 1, binselect = "esmv")
rdrobust::rdplot(dfs_agg$housing_roads_census_t_plus_6_matches$median_sale_amount,
                 dfs_agg$housing_roads_census_t_plus_6_matches$votes_pct_against,
                 c = 50, p = 1, binselect = "esmv")
rdrobust::rdplot(dfs_agg$housing_roads_census_t_plus_7_matches$median_sale_amount,
                 dfs_agg$housing_roads_census_t_plus_7_matches$votes_pct_against,
                 c = 50, p = 1, binselect = "esmv")
rdrobust::rdplot(dfs_agg$housing_roads_census_t_plus_8_matches$median_sale_amount,
                 dfs_agg$housing_roads_census_t_plus_8_matches$votes_pct_against,
                 c = 50, p = 1, binselect = "esmv")


# trying different number of bins (from 30 to 50)
for (i in 30:50){
  rdrobust::rdplot(y = dfs_agg$housing_roads_census_t_plus_9_matches$median_sale_amount, 
                   x = dfs_agg$housing_roads_census_t_plus_9_matches$votes_pct_against, 
                   c = 50, p = 1, nbins = c(i,i), title = paste0(as.character(i)))
}

purrr::map2(dfs_agg, names(dfs_agg), ~print(rdrobust::rdplot(y = .x$median_sale_amount, 
                                                             x = .x$votes_pct_against, 
                                                             c = 50, p = 1, title = .y)))

### sale_amount_per_sq_feett ###
for (i in 30:50){
  rdrobust::rdplot(y = dfs_agg$housing_roads_census_t_plus_9_matches$median_sale_amount, 
                   x = dfs_agg$housing_roads_census_t_plus_9_matches$votes_pct_against, 
                   c = 50, p = 1, nbins = c(i,i), title = paste0(as.character(i)))
}

purrr::map2(dfs_agg_per, names(dfs_agg_per), ~print(rdrobust::rdplot(y = .x$median_sale_amount_per_sq_feet, 
                                                                     x = .x$votes_pct_against, 
                                                                     c = 50, p = 1, title = .y)))


#=========================================#
# running regressions (aggregate) ----
#=========================================#

### |- median sale amount as the outcome var ####

# using local polynomial method;
# storing univariate RDD models (outcome vs running variable, no covariates) from t-2 to t+10
regs <- purrr::map(.x = dfs_agg, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))
# regs_summary <- purrr::map(.x = dfs_agg, ~ summary(rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_for, c = cutoff, all = TRUE)))

# no effect before the voting result was decided (passed/failed)
summary(regs$housing_roads_census_t_minus_3_matches)
summary(regs$housing_roads_census_t_minus_2_matches)
summary(regs$housing_roads_census_t_minus_1_matches)

# Little effect immediately after the voting result was decided. It takes time to build roads and people to change their preferences.
# years 1, 2 and 3 after voting result was decided
summary(regs$housing_roads_census_t_plus_0_matches)
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

tes <- te_tables(regs)
plot_te(tes)

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
regs_rand <- purrr::map(dfs_agg, ~ rdlocrand::rdrandinf(.x$median_sale_amount,
                                                     .x$votes_pct_against,
                                                     cutoff = cutoff,
                                                     ci = 0.05))
tes_rand <- te_tables(regs_rand, rand = TRUE)
ggplot(tes_rand, aes(ord, statistic)) +       
  geom_point(size = 3, shape = 19, color = "blue") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Visualization of Treatment Effects based on Local Randomization",
    subtitle = "With confidence intervals",
    x = "Year",
    y = "Treatment Effect",
    color = "Position"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) + scale_x_continuous(breaks = c(-3:10))


#===============================================================================#
# Winsorization (removing top and bottom 1% of obs based on outcome vars)
#===============================================================================#

# Q. What is the cause of this large variation in T.E for year 5 and year 10? must be 1 or 2 outliers?
# after removing top and bottom 1%, std dev in outcome var looks much more even across datasets
dfs_agg_winsored <- winsorize_data(dfs_agg, "median_sale_amount")

purrr::map_dbl(dfs_agg_winsored, ~ .x %>% select(median_sale_amount) %>% pull() %>% mean()) %>% mean()

# using local polynomial method;

regs_w <- purrr::map(.x = dfs_agg_winsored, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))
purrr::walk2(names(regs_w), regs_w, .f = function(x, y) {
  print(paste0("Outcome variable is ",x))
  summary(y) 
})

tes_w <- te_tables(regs_w)
plot_te(tes_w, title = "Visualization of Treatment Effects: Median House Price", subtitle = "Winsorized 1%")


# using local randomization method;

regs_rand_w <- purrr::map(dfs_agg_winsored, ~ rdlocrand::rdrandinf(.x$median_sale_amount,
                                                                   .x$votes_pct_against,
                                                                   cutoff = cutoff,
                                                                   ci = 0.05))
tes_rand_w <- te_tables(regs_rand_w, rand=TRUE)
ggplot(tes_rand_w, aes(ord, statistic)) +       
  geom_point(size = 3, shape = 19, color = "blue") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Visualization of Treatment Effects based on Local Randomization",
    subtitle = "Winsorized 1%",
    x = "Year",
    y = "Treatment Effect",
    color = "Position"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) + scale_x_continuous(breaks = c(-3:10))

# beepr::beep("mario")

### |- median_sale_amount_per_sq_feet as the outcome var ####

# using local polynomial approximation
regs_per <- purrr::map(.x = dfs_agg_per, ~ rdrobust::rdrobust(y = .x$median_sale_amount_per_sq_feet, x = .x$votes_pct_against, c = cutoff, all = TRUE))
purrr::walk2(names(regs_per), regs_per, .f = function(x, y) {
  print(paste0("Outcome variable is ",x))
  summary(y) 
})

tes_per <- te_tables(regs_per)
plot_te(tes_per, title = "Visualization of T.E Estimates: Median Housing Price per Sq foot", subtitle = "With 95% confidence intervals")

# using local randomization method
regs_per_rand <- purrr::map(dfs_agg_per, ~ rdlocrand::rdrandinf(.x$median_sale_amount_per_sq_feet,
                                                                         .x$votes_pct_against,
                                                                         cutoff = cutoff,
                                                                         ci = 0.05))
tes_rand_w <- te_tables(regs_per_rand, rand=TRUE)
ggplot(tes_rand_w, aes(ord, statistic)) +       
  geom_point(size = 3, shape = 19, color = "blue") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Visualization of Treatment Effects based on Local Randomization",
    subtitle = "With confidence intervals",
    x = "Year",
    y = "Treatment Effect",
    color = "Position"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) + scale_x_continuous(breaks = c(-3:10))

#==============================================================================================================#
# Winsorization of median_sale_amount_per_sq_feet (removing top and bottom 1% of obs based on outcome vars)
#==============================================================================================================#

dfs_agg_per_winsored <- winsorize_data(dfs_agg_per, "median_sale_amount_per_sq_feet")


# local polynomial approach
regs_per_w <- purrr::map(.x = dfs_agg_per_winsored, ~ rdrobust::rdrobust(y = .x$median_sale_amount_per_sq_feet, x = .x$votes_pct_against, c = cutoff, all = TRUE))
purrr::walk2(names(regs_per), regs_per, .f = function(x, y) {
  print(paste0("Outcome variable is ",x))
  summary(y) 
})

tes_per_w <- te_tables(regs_per_w)
plot_te(tes_per_w, title = "Visualization of T.E: Median House Price per sq foot", subtitle = "With 95% confidence intervals")

# local randomization approach
regs_per_rand_w <- purrr::map(dfs_agg_per_winsored, ~ rdlocrand::rdrandinf(.x$median_sale_amount_per_sq_feet,
                                                        .x$votes_pct_against,
                                                        cutoff = cutoff,
                                                        ci = 0.05))
tes_rand_per_w <- te_tables(regs_per_rand_w, rand=TRUE)
ggplot(tes_rand_per_w, aes(ord, statistic)) +       
  geom_point(size = 3, shape = 19, color = "blue") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "T.E based on Local Randomization: Median House Price per sq foot",
    subtitle = "With 95% confidence intervals",
    x = "Year",
    y = "Treatment Effect",
    color = "Position"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) + scale_x_continuous(breaks = c(-3:10))



#============================================================================================================#
#                         Aggregated Results (using roads_agg_pure i.e. uncontaminated dataset) ----
#============================================================================================================#

#=========================================#
# running regressions (aggregate) ----
#=========================================#

### |- median sale amount as the outcome var ####
# storing univariate RDD models (outcome vs running variable, no covariates) from t-2 to t+10
dfs_agg_pure <- purrr::map(dfs_agg_pure, ~ .x %>% mutate(votes_pct_against = 100 - votes_pct_for))

regs_pure <- purrr::map(.x = dfs_agg_pure, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))
tes_pure <- te_tables(regs_pure)
plot_te(tes_pure, title = "Visualization of T.E: Median House Price", subtitle = "with confidence intervals")