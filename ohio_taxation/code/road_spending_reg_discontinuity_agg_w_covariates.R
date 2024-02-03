#==========================================================================================================#
# Purpose : Build on road_spending_reg_discontinuity_agg.R and add covariates to the regression
# Name    : Saani Rawat
# Created : 07/30/2022
# Log     : 
#           08/01/2022: First step of adding covariates to RDD
#           06/17/2023: combined several previously made changes. See github repo for details
#           10/25/2023: Ran RDD on dfs_agg_pure_covs (data uncontaminated by passed additional levies)
#==========================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))
# running covariates balance test
source(paste0(code,"/covariates_balance_test.R"))
source(paste0(code,"/utility_functions.R"))
# uncontaminated datasets
source(paste0(code,"/roads_data_setup.R"))


# user-defined parameters  ----
t_test_sig_level <- 0.05


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
covs_final$housing_roads_census_t_plus_1_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5")
covs_final$housing_roads_census_t_plus_2_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5")
covs_final$housing_roads_census_t_plus_3_matches <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5", "pctlesshs")
covs_final$housing_roads_census_t_plus_8_matches <- covs_final$housing_roads_census_t_plus_8_matches[covs_final$housing_roads_census_t_plus_8_matches != "pctmin"]
covs_final

## local polynomial approach ##
gs <- purrr::map2(covs_final, dfs_agg_covs, .f = function(x,y){
                              # print(paste0("Outcome variable is ",deparse(substitute(y))))
                              rdrobust(  y = y$median_sale_amount,
                                         x = y$votes_pct_for,
                                         c = cutoff,
                                         covs = y %>%
                                           select(x) ,
                                         all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
purrr::walk2(names(gs), gs, .f = function(x, y) {
  print(paste0("Outcome variable is ",x))
  summary(y) 
})
tes_gs <- te_tables(gs)
plot_te(tes_gs, title = "Visualization of Treatment Effects", subtitle = "With covariates")
ggplot(tes_gs, aes(ord, bias_corrected_coef)) +       
  geom_point(size = 3, shape = 19, color = "blue") +
  geom_errorbar(aes(ymin = conf_int_low, ymax = conf_int_high), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Visualization of Treatment Effects",
    subtitle = "With covariates",
    x = "Year",
    y = "Treatment Effect",
    color = "Position"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) + scale_x_continuous(breaks = c(-2, -1, 1:10))


# output for paper draft 
write.csv(tes_gs, paste0(tables, "/tes_gs.csv"), row.names = FALSE)


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
purrr::walk2(gs, names(gs), ~print(paste0("Eff. Bandwidth (p) for ", .y, ": " , round(.x$bws[1,],1))))
purrr::walk2(gs, names(gs), ~print(paste0("Bias Bandwidth (b) for ", .y, ": " , round(.x$bws[2,],1))))

# observations
purrr::walk2(gs, names(gs), ~print(paste0("Eff. Observations for ", .y, ": " , sum(.x$N_h))))
purrr::walk2(gs, names(gs), ~print(paste0("Total Observations for ", .y, ": " , sum(.x$N))))

#|||||||||||||||||||||||||||||||||||||||||||||||||#
# Same Covariates for each outcome period
#|||||||||||||||||||||||||||||||||||||||||||||||||#

# pop, poverty, pct_white, pctsinparhhld, medfamy
covs_my_list <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5", "pctlesshs")
g_regs <- purrr::map(dfs_agg_covs, ~ rdrobust::rdrobust(y = .x$median_sale_amount, 
                                                             x = .x$votes_pct_for, c = cutoff, 
                                                             covs = .x %>% select(all_of(covs_my_list)),
                                                             all = TRUE))
purrr::walk2(names(g_regs), g_regs, .f = function(x, y) {
  print(paste0("Outcome variable is ",x))
  summary(y) 
})
tes_g <- te_tables(g_regs)
plot_te(tes_g, title = "Visualization of Treatment Effects", subtitle = "With covariates")

# lm(dfs_agg_covs$housing_roads_census_t_plus_10_matches,
#    formula = median_sale_amount ~ unemprate+pctrent+pctlt5+pct18to64+pct65pls+pctblack+pctamerind+pctotherrace+pcthisp+pctseparated) %>%
#   summary()

# local randomization approach #
g_regs_rand <- purrr::map(dfs_agg_covs, ~ rdlocrand::rdrandinf(.x$median_sale_amount,
                                                               .x$votes_pct_for,
                                                               covariates = .x %>% select(all_of(covs_my_list)),
                                                               cutoff = cutoff,
                                                               ci = 0.05,
                                                               wmasspoints = TRUE))
purrr::walk2(names(g_regs_rand), g_regs_rand, .f = function(x, y) {
  print(paste0("Outcome variable is ",x))
  summary(y) 
})
tes_g_rand <- te_tables(g_regs_rand, rand = TRUE)
# plot_te(tes_g_rand, title = "Visualization of Treatment Effects based on Local Randomization", subtitle = "With covariates")

ggplot(tes_g_rand, aes(ord, statistic)) +       
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


## winsorization of median_sale_amount
# removing top and bottom 1% of observations
dfs_agg_covs_winsored <- winsorize_data(dfs_agg_covs, "median_sale_amount")

# local polynomial approach
covs_list <- c("pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl")

covs_my_list <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctsinparhhld", "pctlt5", "pctlesshs")
g_regs_w <- purrr::map(dfs_agg_covs_winsored, ~ rdrobust::rdrobust(y = .x$median_sale_amount, 
                                                        x = .x$votes_pct_for, c = cutoff, 
                                                        covs = .x %>% select(all_of(covs_my_list)),
                                                        all = TRUE))
purrr::walk2(names(g_regs_w), g_regs_w, .f = function(x, y) {
  print(paste0("Outcome variable is ",x))
  summary(y) 
})
tes_g_w <- te_tables(g_regs_w)
plot_te(tes_g_w)

# local randomization approach
g_regs_rand_w <- purrr::map(dfs_agg_covs_winsored, ~ rdlocrand::rdrandinf(.x$median_sale_amount,
                                                               .x$votes_pct_for,
                                                               covariates = .x %>% select(all_of(covs_my_list)),
                                                               cutoff = cutoff,
                                                               ci = 0.05,
                                                               wmasspoints = TRUE))
tes_g_rand_w <- te_tables(g_regs_rand_w, rand = TRUE)
ggplot(tes_g_rand_w, aes(ord, statistic)) +       
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


#============================================================================================================#
#     Introducing covariates (using median sale_amount per square feet)  into regression ---- 
#============================================================================================================#

# median sale amount per square feet
covs_final_per <- purrr::map(dfs_agg_per_covs, ~find_covs(.x, y = "median_sale_amount_per_sq_feet", covs_list = covs_list))
## 
beepr::beep("mario")


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

#|||||||||||||||||||||||||||||||||||||||||||||||||#
# Same Covariates for each outcome period
#|||||||||||||||||||||||||||||||||||||||||||||||||#
covs_my_list <- c("poverty")
f_regs <- purrr::map(dfs_agg_per_covs, ~ rdrobust::rdrobust(y = .x$median_sale_amount_per_sq_feet, 
                                                        x = .x$votes_pct_for, c = cutoff, 
                                                        covs = .x %>% select(all_of(covs_my_list)),
                                                        all = TRUE))

tes_f <- te_tables(f_regs)
plot_te(tes_f)


################### Final Covariates List ###############
# median sale amount
covs_list_t_minus_2 = dfs_agg_covs$housing_roads_census_t_minus_2_matches %>%
                        select(covs_final$housing_roads_census_t_minus_2_matches) %>% select(-c(pctotherrace, pctblack)) %>% colnames()
covs_list_t_minus_1 = dfs_agg_covs$housing_roads_census_t_minus_1_matches %>%
                        select(covs_final$housing_roads_census_t_minus_1_matches) %>% select(-c(pctblack, pctmin)) %>% colnames()
covs_list_t_plus_1 = dfs_agg_covs$housing_roads_census_t_plus_1_matches %>%
                        select(covs_final$housing_roads_census_t_plus_1_matches) %>% select(-c(pctnokids, pctrent)) %>% colnames()
covs_list_t_plus_2 = dfs_agg_covs$housing_roads_census_t_plus_2_matches %>%
                        select(covs_final$housing_roads_census_t_plus_2_matches) %>% select(-c(pctnokids, pctmin))  %>% colnames()
covs_list_t_plus_3 = dfs_agg_covs$housing_roads_census_t_plus_3_matches %>%
                        select(covs_final$housing_roads_census_t_plus_3_matches)  %>% colnames()
covs_list_t_plus_4 = dfs_agg_covs$housing_roads_census_t_plus_4_matches %>%
                        select(covs_final$housing_roads_census_t_plus_4_matches) %>% select(-c(pctnokids, pctown)) %>% colnames()
covs_list_t_plus_5 = dfs_agg_covs$housing_roads_census_t_plus_5_matches %>%
                        select(covs_final$housing_roads_census_t_plus_5_matches) %>% colnames()
covs_list_t_plus_6 = dfs_agg_covs$housing_roads_census_t_plus_6_matches %>%
                        select(covs_final$housing_roads_census_t_plus_6_matches) %>% colnames()
covs_list_t_plus_7 = dfs_agg_covs$housing_roads_census_t_plus_7_matches %>%
                        select(covs_final$housing_roads_census_t_plus_7_matches) %>% colnames()
covs_list_t_plus_8 = dfs_agg_covs$housing_roads_census_t_plus_8_matches %>%
                        select(covs_final$housing_roads_census_t_plus_8_matches) %>% colnames()
covs_list_t_plus_9 = dfs_agg_covs$housing_roads_census_t_plus_9_matches %>%
                        select(covs_final$housing_roads_census_t_plus_9_matches)  %>% colnames()
covs_list_t_plus_10 = dfs_agg_covs$housing_roads_census_t_plus_10_matches %>%
                        select(covs_final$housing_roads_census_t_plus_10_matches) %>% colnames()

hh <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_minus_2_matches$median_sale_amount,
           x = dfs_agg_covs$housing_roads_census_t_minus_2_matches$votes_pct_for,
           c = cutoff,
           covs = dfs_agg_covs$housing_roads_census_t_minus_2_matches %>%
             select(all_of(covs_list_t_minus_2)) ,
           all = TRUE) 



# median_sale_amount_per_sq_feet
covs_list_per_t_minus_2 = dfs_agg_per_covs$housing_roads_census_t_minus_2_matches %>%
                        select(covs_final_per$housing_roads_census_t_minus_2_matches) %>% colnames()
covs_list_per_t_minus_1 = dfs_agg_per_covs$housing_roads_census_t_minus_1_matches %>%
                        select(covs_final_per$housing_roads_census_t_minus_1_matches) %>% colnames()
covs_list_per_t_plus_1 = dfs_agg_per_covs$housing_roads_census_t_plus_1_matches %>%
                        select(covs_final_per$housing_roads_census_t_plus_1_matches) %>% colnames()
covs_list_per_t_plus_2 = dfs_agg_per_covs$housing_roads_census_t_plus_2_matches %>%
                        select(covs_final_per$housing_roads_census_t_plus_2_matches)  %>% colnames()
covs_list_per_t_plus_3 = dfs_agg_per_covs$housing_roads_census_t_plus_3_matches %>%
                        select(covs_final_per$housing_roads_census_t_plus_3_matches) %>% select(-c(pctblack, pctmin))  %>% colnames()
covs_list_per_t_plus_4 = dfs_agg_per_covs$housing_roads_census_t_plus_4_matches %>%
                        select(covs_final_per$housing_roads_census_t_plus_4_matches) %>% colnames()
covs_list_per_t_plus_5 = dfs_agg_per_covs$housing_roads_census_t_plus_5_matches %>%
                        select(covs_final_per$housing_roads_census_t_plus_5_matches) %>% colnames()
covs_list_per_t_plus_6 = dfs_agg_per_covs$housing_roads_census_t_plus_6_matches %>%
                        select(covs_final_per$housing_roads_census_t_plus_6_matches) %>% colnames()
covs_list_per_t_plus_7 = dfs_agg_per_covs$housing_roads_census_t_plus_7_matches %>%
                        select(covs_final_per$housing_roads_census_t_plus_7_matches)  %>% select(-(pctnokids)) %>% colnames()
covs_list_per_t_plus_8 = dfs_agg_per_covs$housing_roads_census_t_plus_8_matches %>%
                        select(covs_final_per$housing_roads_census_t_plus_8_matches) %>% colnames()
covs_list_per_t_plus_9 = dfs_agg_per_covs$housing_roads_census_t_plus_9_matches %>%
                        select(covs_final_per$housing_roads_census_t_plus_9_matches) %>% select(-c(pctmin))  %>% colnames()
covs_list_per_t_plus_10 = dfs_agg_per_covs$housing_roads_census_t_plus_10_matches %>%
                        select(covs_final_per$housing_roads_census_t_plus_10_matches) %>% colnames()


# lm(dfs_agg_per_covs$housing_roads_census_t_plus_1_matches %>% select(c("median_sale_amount_per_sq_feet",covs_list_per_t_plus_1)),
#    formula = median_sale_amount_per_sq_feet ~ .) %>%
#   summary()


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
#                         Aggregated Results (using roads_agg_pure_covs i.e. uncontaminated dataset) ----
#============================================================================================================#

# using median_sale_amount
covs_my_list <- c("pop", "poverty", "pctmin", "medfamy",  "pct18to64", "pctlesshs", "pctsinparhhld", "pctlt5")

## same covariates
g_p_regs <- purrr::map(dfs_agg_pure_covs, ~ rdrobust::rdrobust(y = .x$median_sale_amount,
                                                               x = .x$votes_pct_for, c = cutoff,
                                                               covs = .x %>% select(all_of(covs_my_list)),
                                                               all = TRUE))
tes_g_p <- te_tables(g_p_regs)
plot_te(tes_g_p, title = "T.E Estimates: Uncontaminated voting data", subtitle = "Treatment Effect estimates")

# using median_sale_amount_per_sq_foot

