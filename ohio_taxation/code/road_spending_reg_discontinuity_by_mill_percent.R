#================================================================================================================#
# Purpose : Split by millage size to see whether millage size affects housing price
# Name    : Saani Rawat
# Created : 06/16/2023
# Problem: 
# Log     : 
#       06/16/2023: 
#       12/16/2024: updated the code to have t-1 as the reference point
#       1/24/2024 : updating based on CFR 2010
#================================================================================================================#



# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
plots <- paste0(data,"/outputs/plots")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))


#============================================================================================================#
#                         Splitting by millage size of > 1.9 and < 1.9 ----
#============================================================================================================#

# computing the mean
mean_mill <- purrr::map_dbl(dfs_agg_mill, ~ round(mean(as.numeric(.x$millagepercent), na.rm = TRUE),1))

# Q. How much dollars are we talking per person on levies?
# Each levy is approx 2 mills on average, mean house is 166k. So,
2/1000 * 166000
# $332 per household tax

# separating based on above and below mean
dfs_agg_mill_g_1.9 <- purrr::map2(dfs_agg_mill, mean_mill, ~ .x %>% filter(as.numeric(millagepercent)  > .y) %>% 
                                    mutate(millagepercent = as.numeric(millagepercent),
                                           treated = if_else(votes_pct_against > cutoff, 1, 0)  ) )

dfs_agg_mill_l_1.9 <- purrr::map2(dfs_agg_mill, mean_mill, ~ .x %>% filter(as.numeric(millagepercent)  <= .y) %>% 
                                    mutate(millagepercent = as.numeric(millagepercent),
                                           treated = if_else(votes_pct_against > cutoff, 1, 0) ) )


# adding covariates
dfs_agg_gm_covs <- purrr::map(.x = dfs_agg_mill_g_1.9, ~ .x %>% 
                             dplyr::left_join(y = census, by = c("tendigit_fips","vote_year")) %>%
                             ungroup()
)

dfs_agg_lm_covs <- purrr::map(.x = dfs_agg_mill_l_1.9, ~ .x %>% 
                                dplyr::left_join(y = census, by = c("tendigit_fips","vote_year")) %>%
                                ungroup()
)

#=====================================================================================================#
#=====================================================================================================#
# running regressions: No Covariates ----
#=====================================================================================================#
#=====================================================================================================#


#==================================================================================#
# running regressions (aggregate) for millage size > 1.9 ----
#==================================================================================#

### |- median sale amount ####

# storing univariate RDD models (outcome vs running variable, no covariates) from t-2 to t+10
regs_g_1.9 <- purrr::map(.x = dfs_agg_mill_g_1.9, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))

# dist(dfs_agg_mill$housing_roads_census_t_plus_4_matches$millage_percent)

tes_g_1.9 <- te_tables(regs_g_1.9)
tes_g_1.9
png(paste0(plots,"/tes_g_1.9.png"))
plot_te(tes_g_1.9, title = "Millage percent greater than mean of 1.9%", subtitle = "Treatment Effect Estimates", caption = "")
dev.off()

#==================================================================================#
# running regressions (aggregate) for millage size <= 1.9 ----
#==================================================================================#

regs_l_1.9 <- purrr::map(.x = dfs_agg_mill_l_1.9, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))

tes_l_1.9 <- te_tables(regs_l_1.9)
tes_l_1.9
png(paste0(plots,"/tes_l_1.9.png"))
plot_te(tes_l_1.9, title = "Millage percent less than mean of 1.9%", subtitle = "Treatment Effect Estimates", caption = "")
dev.off()


map(dfs_agg_mill_l_1.9, tab)

# append the two datasets regs_g_1.9 and regs_l_1.9
tes_g_1.9 <- tes_g_1.9 %>% mutate(cat = "> mean tax levy",
                                  conf_int_low  = if_else(year == "t_minus_1", robust_coef, conf_int_low),
                                  conf_int_high = if_else(year == "t_minus_1", robust_coef, conf_int_high) )
tes_l_1.9 <- tes_l_1.9 %>% mutate(cat = "<= mean tax levy",
                                  conf_int_low  = if_else(year == "t_minus_1", robust_coef, conf_int_low),
                                  conf_int_high = if_else(year == "t_minus_1", robust_coef, conf_int_high) )
tes_size <- rbind(tes_g_1.9, tes_l_1.9) %>% mutate(ord = if_else(cat == "> mean tax levy", ord - 0.15, ord + 0.15))



ggplot(tes_size, aes(ord, robust_coef, color = cat)) +       
  geom_point(size = 3, shape = 19) +
  geom_errorbar(aes(ymin = conf_int_low, ymax = conf_int_high, color = cat), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = tes_size %>% filter(cat == "> mean tax levy" & ord == -1.15) %>% pull(robust_coef), linetype = "dashed", color = "#1f77b4", size = 1) +
  geom_hline(yintercept = tes_size %>% filter(cat == "<= mean tax levy" & ord == -0.85) %>% pull(robust_coef), linetype = "dashed", color = "#ff7f0e", size = 1) +  
  labs(
    x = "Year",
    y = "Treatment Effect",
    title = "Treatment Effects based on Millage size",
    subtitle = "Average Millage size = 1.9%"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    legend.title = element_blank()
  ) + 
  scale_x_continuous(breaks = c(-3:10)) +
  scale_color_manual(values = c("> mean tax levy" = "#1f77b4", "<= mean tax levy" = "#ff7f0e")) 


#=====================================================================================================#
#=====================================================================================================#
# running regressions: With Covariates ----
#=====================================================================================================#
#=====================================================================================================#

covs_list <- c("pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl")

covs_final_gm <- purrr::map(dfs_agg_gm_covs, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))

covs_final_lm <- purrr::map(dfs_agg_lm_covs, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))

#==================================================================================#
# Above mean millage size
#==================================================================================#

gs_gm <- purrr::map2(covs_final_gm, dfs_agg_gm_covs, .f = function(x,y){
                      rdrobust(  y = y$median_sale_amount,
                                 x = y$votes_pct_against,
                                 c = cutoff,
                                 covs = y %>%
                                   select(x) ,
                                 all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_gs_gm <- te_tables(gs_gm)
plot_te(tes_gs_gm, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")
plot_te_recenter(tes_gs_gm, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")

## with Time Fixed Effects ##

# adding time dummies to data
dfs_agg_covs_gm_w_tfe <- map(dfs_agg_gm_covs, ~ dummy_cols(.x, select_columns = c("year"), remove_first_dummy = TRUE) %>% relocate(starts_with("year_"), .after = "year"))

# merging time dummies with covariates
dfs_agg_covs_gm_tfe_names <- map(dfs_agg_covs_gm_w_tfe, ~ colnames(.x) %>% grep("year_", ., value = TRUE))
covs_final_gm_w_tfe <- map2(covs_final_gm, dfs_agg_covs_gm_tfe_names, ~c(.x, .y))


gs_gm <- purrr::map2(covs_final_gm_w_tfe, dfs_agg_covs_gm_w_tfe, .f = function(x,y){
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(c('medfamy')) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})
tes_gs_gm <- te_tables(gs_gm)
plot_te(tes_gs_gm, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")
plot_te_recenter(tes_gs_gm, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")


#==================================================================================#
# Below mean millage size
#==================================================================================#

gs_lm <- purrr::map2(covs_final_lm, dfs_agg_lm_covs, .f = function(x,y){
  rdrobust(  y = y$median_sale_amount,
             x = y$votes_pct_against,
             c = cutoff,
             covs = y %>%
               select(x) ,
             all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2)
})


