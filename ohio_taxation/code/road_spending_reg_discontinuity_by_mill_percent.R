#================================================================================================================#
# Purpose : Split by millage size to see whether millage size affects housing price
# Name    : Saani Rawat
# Created : 06/16/2023
# Problem: 
# Log     : 
#       06/16/2023: 
#       12/16/2024: updated the code to have t-1 as the reference point
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

dfs_agg_mill_g_1.9 <- purrr::map2(dfs_agg_mill, mean_mill, ~ .x %>% filter(as.numeric(millagepercent)  > .y) %>% 
                                    mutate(millagepercent = as.numeric(millagepercent) ) )

dfs_agg_mill_l_1.9 <- purrr::map2(dfs_agg_mill, mean_mill, ~ .x %>% filter(as.numeric(millagepercent)  <= .y) %>% 
                                    mutate(millagepercent = as.numeric(millagepercent) ) )

sort(unique(dfs_agg_mill$housing_roads_census_t_plus_4_matches$millagepercent))

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

