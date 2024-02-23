#================================================================================================================#
# Purpose : Run RD based on urban vs rural distinction
# Name    : Saani Rawat
# Created : 06/13/2023
# Log     : 
#       06/13/2023: 
#       01/30/2024: updated code. Main analysis using "ua" urban flag category. Added covs to analysis
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

# importing the urban vs rural file
twp_places_urban <- haven::read_dta(paste0(data,"/twp_places_urban.dta")) %>% janitor::clean_names()

# creating urban flags using clusterdummy and uadummy. 0 == rural and 1 == urban
dfs_agg_twp <- purrr::map(dfs_agg, ~ .x %>%
                                     left_join(twp_places_urban, by = "tendigit_fips") %>%
                                     mutate(urban_flg_cd = if_else(is.na(clusterdummy), 0, clusterdummy),
                                            urban_flg_ua = if_else(is.na(uadummy ), 0, uadummy))) # if NA, then assuming the region is rural

# splitting the datasets into urban and rural, by both: clusterdummy and uadummy
dfs_agg_urb_cd <- purrr::map(dfs_agg_twp, ~ .x %>% filter(urban_flg_cd == 1) %>% select(-(urban_flg_ua)) )
dfs_agg_urb_ua <- purrr::map(dfs_agg_twp, ~ .x %>% filter(urban_flg_ua == 1) %>% select(-(urban_flg_cd)) )
dfs_agg_rur_cd <- purrr::map(dfs_agg_twp, ~ .x %>% filter(urban_flg_cd == 0) %>% select(-(urban_flg_ua)) )
dfs_agg_rur_ua <- purrr::map(dfs_agg_twp, ~ .x %>% filter(urban_flg_ua == 0) %>% select(-(urban_flg_cd)) )


#=================================================================================#
# running regressions without covariates (aggregate) ----
#=================================================================================#
regs_urb_cd <- purrr::map(.x = dfs_agg_urb_cd, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))
regs_urb_ua <- purrr::map(.x = dfs_agg_urb_ua, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))
regs_rur_cd <- purrr::map(.x = dfs_agg_rur_cd, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))
regs_rur_ua <- purrr::map(.x = dfs_agg_rur_ua, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))


# summary(regs_urb_cd$housing_roads_census_t_plus_4_matches)

tes_urb_cd <- te_tables(regs_urb_cd)
tes_urb_ua <- te_tables(regs_urb_ua)
tes_rur_cd <- te_tables(regs_rur_cd)
tes_rur_ua <- te_tables(regs_rur_ua)

plot_te(tes_urb_cd)
plot_te(tes_urb_ua)
plot_te(tes_rur_cd)
plot_te(tes_rur_ua)

# find_covs <- function(df, y, covs_list)




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
dfs_agg_covs_urb_cd <- purrr::map(dfs_agg_covs_twp, ~ .x %>% filter(urban_flg_cd == 1) %>% select(-(urban_flg_ua)) )
dfs_agg_covs_urb_ua <- purrr::map(dfs_agg_covs_twp, ~ .x %>% filter(urban_flg_ua == 1) %>% select(-(urban_flg_cd)) )
dfs_agg_covs_rur_cd <- purrr::map(dfs_agg_covs_twp, ~ .x %>% filter(urban_flg_cd == 0) %>% select(-(urban_flg_ua)) )
dfs_agg_covs_rur_ua <- purrr::map(dfs_agg_covs_twp, ~ .x %>% filter(urban_flg_ua == 0) %>% select(-(urban_flg_cd)) )



#=================================================================================#
# running regressions with covariates (aggregate) ----
#=================================================================================#

# selecting the best set of covariates for each median sale amount period
covs_final_urb_cd <- purrr::map(dfs_agg_covs_urb_cd, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))
covs_final_urb_ua <- purrr::map(dfs_agg_covs_urb_ua, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))
covs_final_rur_cd <- purrr::map(dfs_agg_covs_rur_cd, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))
covs_final_rur_ua <- purrr::map(dfs_agg_covs_rur_ua, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))


covs_list_names <- gsub(pattern = "housing_roads_census", replacement = "covs_list", x = names(dfs_agg_covs))
covs_list_names_clean <- gsub(pattern = "_matches", replacement = "", x =covs_list_names)

# creating a list that contains all the covariate lists from t - 3 to t + 10
selected_covs_lists = mget(covs_list_names_clean)

covs_final_urb_cd$housing_roads_census_t_minus_1_matches
covs_final_urb_cd$housing_roads_census_t_minus_2_matches[!covs_final_urb_cd$housing_roads_census_t_minus_2_matches %in% c("pctown","pctmin")]
covs_final_urb_cd$housing_roads_census_t_minus_3_matches[!covs_final_urb_cd$housing_roads_census_t_minus_2_matches %in% c("pctown","pctmin")]
covs_final_urb_cd$housing_roads_census_t_plus_0_matches[!covs_final_urb_cd$housing_roads_census_t_minus_2_matches %in% c("pctown","pctmin")]
covs_final_urb_cd$housing_roads_census_t_plus_1_matches[!covs_final_urb_cd$housing_roads_census_t_minus_2_matches %in% c("pctown","pctmin")]

covs_final_urb_cd2 <- purrr::map(covs_final_urb_cd, ~ .x[!(.x %in% c("pctown","pctmin","pctnokids",NA))])

# running regressions with covariates
regs_covs_urb_cd <- purrr::map2(dfs_agg_covs_urb_cd, covs_final_urb_cd2 ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_against, 
                                                     c = cutoff, 
                                                     # covs = x %>% select(y),
                                                     covs =  x %>% select(c("pop", "poverty", "pctmin", "pctown")),
                                                     all = TRUE)
                                }
            )
tes_covs_urb_cd <- te_tables(regs_covs_urb_cd) 
plot_te(tes_covs_urb_cd)

regs_covs_urb_ua <- purrr::map2(dfs_agg_covs_urb_ua, covs_final_urb_ua ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_against, 
                                                     c = cutoff, 
                                                     # covs = x %>% select(y),
                                                     # covs =  x %>% select(c("pop", "poverty", "pctmin", "pctown")),
                                                     covs =  x %>% select(c("pctmin", "pctown")),
                                                     all = TRUE)
                                }
)
tes_covs_urb_ua <- te_tables(regs_covs_urb_ua) 
# plot_te(tes_urb_ua)
plot_te(tes_covs_urb_ua)

# lm_model <- map(dfs_agg_covs_urb_ua, ~ lm(.x$median_sale_amount ~ .x$votes_against + .x$pop + .x$poverty + .x$pctmin + .x$pctown) )
# 
# map(lm_model, vif)
# map(lm_model, summary)

regs_covs_rur_cd <- purrr::map2(dfs_agg_covs_rur_cd, covs_final_rur_cd ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_against, 
                                                     c = cutoff, 
                                                     covs =  x %>% select(c("pop", "poverty", "pctmin", "pctown")),
                                                     # covs = x %>% select(y),
                                                     all = TRUE)
                                }
)
tes_covs_rur_cd <- te_tables(regs_covs_rur_cd) 
plot_te(tes_covs_rur_cd)


regs_covs_rur_ua <- purrr::map2(dfs_agg_covs_rur_ua, covs_final_rur_ua ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_against, 
                                                     c = cutoff, 
                                                     covs =  x %>% select(c("pop", "poverty", "pctmin", "pctown", "medfamy", "unemprate", "raceherfindahl")),
                                                     # covs = x %>% select(y),
                                                     all = TRUE)
                                }
)
tes_covs_rur_ua <- te_tables(regs_covs_rur_ua) 
plot_te(tes_covs_rur_ua)
# plot_te(tes_rur_ua)


# append the two datasets tes_covs_urb_ua and tes_covs_rur_ua
tes_covs_urb_ua <- tes_covs_urb_ua %>% mutate(cat = "urban")
tes_covs_rur_ua <- tes_covs_rur_ua %>% mutate(cat = "rural")

tes_covs_ua <- rbind(tes_covs_urb_ua, tes_covs_rur_ua) %>% mutate(ord = if_else(cat == "rural", ord - 0.15, ord + 0.15))


ggplot(tes_covs_ua, aes(ord, robust_coef, color = cat)) +       
  geom_point(size = 3, shape = 19) +
  geom_errorbar(aes(ymin = conf_int_low, ymax = conf_int_high, color = cat), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(
    x = "Year",
    y = "Treatment Effect",
    color = "Position",
    title = "Treatment Effects: Urban vs Rural"
    # caption = "Note: Here is where you can add your notes or source information.Note: Here is where you can add your notes or source information.Note: Here is where you can add your notes or source information.Note: Here is where you can add your notes or source information.Note: Here is where you can add your notes or source information.Note: Here is where you can add your notes or source information.Note: Here is where you can add your notes or source information."
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
  scale_x_continuous(breaks = c(-3:10))



