#================================================================================================================#
# Purpose : Run RD based on urban vs rural distinction
# Name    : Saani Rawat
# Created : 06/13/2023
# Log     : 
#       06/13/2023: 
#       01/30/2024: updated code. Main analysis using "ua" urban flag category. Added covs to analysis
#       11/16/2024: updating regs
#================================================================================================================#


# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))
source(paste0(code,"/utility_functions.R"))

covs_list <- c("pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl")


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

tes_urb_ua[4:14,"robust_coef"] %>% pull %>% mean / map_dbl(dfs_agg_urb_ua, ~median(.x$median_sale_amount))[4:14] %>% mean


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


# running regressions with covariates
regs_covs_urb_cd <- purrr::map2(dfs_agg_covs_urb_cd, covs_final_urb_cd ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_against, 
                                                     c = cutoff, 
                                                     # covs = x %>% select(y),
                                                     # covs =  x %>% select(c("pop", "poverty", "pctmin", "pctown")),
                                                     all = TRUE)
                                }
            )
tes_covs_urb_cd <- te_tables(regs_covs_urb_cd) 
plot_te(tes_covs_urb_cd)

covs_final_urb_ua2 <- covs_final_urb_ua
covs_final_urb_ua2 <- list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
names(covs_final_urb_ua2) <- names(dfs_agg_covs)
covs_final_urb_ua2$housing_roads_census_t_minus_3_matches <- c("pop","pcthsgrad", "medfamy")
covs_final_urb_ua2$housing_roads_census_t_minus_2_matches <- c("pctdivorced","pcthsgrad", "medfamy")
covs_final_urb_ua2$housing_roads_census_t_minus_1_matches <- c("pctdivorced","pcthsgrad", "medfamy")
covs_final_urb_ua2$housing_roads_census_t_plus_0_matches <- c("medfamy")
covs_final_urb_ua2$housing_roads_census_t_plus_1_matches <- c("medfamy")
covs_final_urb_ua2$housing_roads_census_t_plus_2_matches <- c("medfamy")
covs_final_urb_ua2$housing_roads_census_t_plus_3_matches <- c("medfamy")
covs_final_urb_ua2$housing_roads_census_t_plus_4_matches <- c("medfamy")
# covs_final_urb_ua2$housing_roads_census_t_plus_5_matches <- c("medfamy")
# covs_final_urb_ua2$housing_roads_census_t_plus_5_matches <- c("poverty", "pctown")
covs_final_urb_ua2$housing_roads_census_t_plus_5_matches <- c("medfamy", "childpov", "pctapi")
covs_final_urb_ua2$housing_roads_census_t_plus_6_matches <- c("medfamy")
covs_final_urb_ua2$housing_roads_census_t_plus_7_matches <- c("medfamy", "poverty", "pctmin", "lforcepartrate", "unemprate", "incherfindahl", "pctdivorced")
covs_final_urb_ua2$housing_roads_census_t_plus_8_matches <- c("medfamy")
covs_final_urb_ua2$housing_roads_census_t_plus_9_matches <- c("medfamy", "poverty", "pctmin", "lforcepartrate", "unemprate", "incherfindahl")
covs_final_urb_ua2$housing_roads_census_t_plus_10_matches<- c("medfamy")

# urban
regs_covs_urb_ua <- purrr::map2(dfs_agg_covs_urb_ua, covs_final_urb_ua2 ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_against, 
                                                     c = cutoff, 
                                                     covs = x %>% select(y),
                                                     # covs =  x %>% select(c("pop")),
                                                     # covs =  x %>% select("medfamy"),
                                                     all = TRUE)
                                }
)
tes_covs_urb_ua <- te_tables(regs_covs_urb_ua) 
plot_te(tes_covs_urb_ua)

# rural
regs_covs_rur_ua <- purrr::map2(dfs_agg_covs_rur_ua, c("") ,
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
# plot_te(tes_covs_rur_ua)
# plot_te(tes_rur_ua)


# append the two datasets tes_covs_urb_ua and tes_covs_rur_ua
tes_covs_urb_ua <- tes_covs_urb_ua %>% mutate(cat = "urban")
tes_covs_rur_ua <- tes_covs_rur_ua %>% mutate(cat = "rural")

tes_covs_ua <- rbind(tes_covs_urb_ua, tes_covs_rur_ua) %>% mutate(ord = if_else(cat == "rural", ord - 0.15, ord + 0.15))

### Main urban vs rural plot in paper ###
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
  ylim(c(NA, 50000)) 

### Re-centered Main urban vs rural plot in paper ###
tes_covs_ua_ <- tes_covs_ua %>% mutate(conf_int_low  = if_else(year == "t_minus_1", robust_coef, conf_int_low),
                                       conf_int_high = if_else(year == "t_minus_1", robust_coef, conf_int_high))
ggplot(tes_covs_ua_, aes(ord, robust_coef, color = cat)) +       
  geom_point(size = 3, shape = 19) +
  geom_errorbar(aes(ymin = conf_int_low, ymax = conf_int_high, color = cat), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = tes_covs_ua_ %>% filter(cat == "urban" & ord == -0.85) %>% pull(robust_coef), linetype = "dashed", color = "#66b2b2", size = 1) +
  geom_hline(yintercept = tes_covs_ua_ %>% filter(cat == "rural" & ord == -1.15) %>% pull(robust_coef), linetype = "dashed", color = "orange", size = 1) +
  labs(
    x = "Year",
    y = "Treatment Effect",
    color = "Position",
    title = "Treatment Effects: Urban vs Rural"
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
  ylim(c(NA, 50000)) 


#==============================================#
# Adding Time Fixed Effects
#==============================================#

# urban
dfs_agg_covs_urb_ua_w_tfe <- map(dfs_agg_covs_urb_ua, ~ dummy_cols(.x, select_columns = c("year"), remove_first_dummy = TRUE) %>% relocate(starts_with("year_"), .after = "year"))

# merging year dummies and covariates list 
dfs_agg_covs_urb_ua_tfe_names <- map(dfs_agg_covs_urb_ua_w_tfe, ~ colnames(.x) %>% grep("year_", ., value = TRUE))
covs_final_urb_ua_w_tfe <- map2(covs_final_urb_ua2, dfs_agg_covs_urb_ua_tfe_names, ~c(.x, .y))

regs_covs_urb_ua_tfe <- purrr::map2(dfs_agg_covs_urb_ua_w_tfe, covs_final_urb_ua_w_tfe ,
                                function(x, y){
                                  rdrobust::rdrobust(y = x$median_sale_amount,
                                                     x = x$votes_pct_against, 
                                                     c = cutoff, 
                                                     covs = x %>% select(y),
                                                     # covs =  x %>% select(c("pop")),
                                                     # covs =  x %>% select("medfamy"),
                                                     all = TRUE)
                                }
)

tes_regs_covs_urb_ua_tfe <- te_tables(regs_covs_urb_ua_tfe)
plot_te(tes_regs_covs_urb_ua_tfe, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")
plot_te_recenter(tes_regs_covs_urb_ua_tfe, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")

# rural
dfs_agg_covs_rur_ua_w_tfe <- map(dfs_agg_covs_rur_ua, ~ dummy_cols(.x, select_columns = c("year"), remove_first_dummy = TRUE) %>% relocate(starts_with("year_"), .after = "year"))

# merging time dummies with covariates list
dfs_agg_covs_rur_ua_tfe_names <- map(dfs_agg_covs_rur_ua_w_tfe, ~ colnames(.x) %>% grep("year_", ., value = TRUE))
covs_final_rur_ua_w_tfe <- map(dfs_agg_covs_rur_ua_tfe_names, ~ c(c("pop", "poverty", "pctmin", "pctown", "medfamy", "unemprate", "raceherfindahl"), .x))

regs_covs_rur_ua_tfe <- purrr::map2(dfs_agg_covs_rur_ua_w_tfe, covs_final_rur_ua_w_tfe ,
                                    function(x, y){
                                      rdrobust::rdrobust(y = x$median_sale_amount,
                                                         x = x$votes_pct_against, 
                                                         c = cutoff, 
                                                         covs = x %>% select(y),
                                                         all = TRUE)
                                    }
)

tes_regs_covs_rur_ua_tfe <- te_tables(regs_covs_rur_ua_tfe)
plot_te(tes_regs_covs_rur_ua_tfe, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")
plot_te_recenter(tes_regs_covs_rur_ua_tfe, title = "Treatment Effect Estimates: Median House Price", subtitle = "With covariates")



### Re-centered Main urban vs rural plot in paper WITH Time F.E ###
tes_regs_covs_ua <- rbind(tes_regs_covs_urb_ua_tfe %>% mutate(cat = "urban"), tes_regs_covs_rur_ua_tfe %>% mutate(cat = "rural")) %>% 
  mutate(ord = if_else(cat == "rural", ord - 0.15, ord + 0.15),
         conf_int_low  = if_else(year == "t_minus_1", robust_coef, conf_int_low),
         conf_int_high = if_else(year == "t_minus_1", robust_coef, conf_int_high)) 


ggplot(tes_regs_covs_ua, aes(ord, robust_coef, color = cat)) +       
  geom_point(size = 3, shape = 19) +
  geom_errorbar(aes(ymin = conf_int_low, ymax = conf_int_high, color = cat), 
                width = 0.2, color = "grey50", size = 0.7) +
  geom_hline(yintercept = tes_regs_covs_ua %>% filter(cat == "urban" & ord == -0.85) %>% pull(robust_coef), linetype = "dashed", color = "#66b2b2", size = 1) +
  geom_hline(yintercept = tes_regs_covs_ua %>% filter(cat == "rural" & ord == -1.15) %>% pull(robust_coef), linetype = "dashed", color = "orange", size = 1) +
  labs(
    x = "Year",
    y = "Treatment Effect",
    color = "Position",
    title = "Treatment Effects: Urban vs Rural"
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
  ylim(c(NA, 50000)) 

#=================================================================================#
# winsorizing the data after identifying urban and rural ----
#=================================================================================#


housing_dfs_twp <- purrr::map(map(housing_dfs, ~.x %>% clean_names), ~ .x %>%
                            left_join(twp_places_urban, by = "tendigit_fips") %>%
                            mutate(urban_flg_cd = if_else(is.na(clusterdummy), 0, clusterdummy),
                                   urban_flg_ua = if_else(is.na(uadummy ), 0, uadummy))) 

# splitting the datasets into urban and rural, by both: clusterdummy and uadummy
housing_dfs_twp_urb_cd <- purrr::map(housing_dfs_twp, ~ .x %>% filter(urban_flg_cd == 1) %>% select(-(urban_flg_ua)) )
housing_dfs_twp_urb_ua <- purrr::map(housing_dfs_twp, ~ .x %>% filter(urban_flg_ua == 1) %>% select(-(urban_flg_cd)) )
housing_dfs_twp_rur_cd <- purrr::map(housing_dfs_twp, ~ .x %>% filter(urban_flg_cd == 0) %>% select(-(urban_flg_ua)) )
housing_dfs_twp_rur_ua <- purrr::map(housing_dfs_twp, ~ .x %>% filter(urban_flg_ua == 0) %>% select(-(urban_flg_cd)) )

# winsorizing the data by each urban and rural category
housing_dfs_twp_urb_cd_w <- winsorize_data(housing_dfs_twp_urb_cd, "sale_amount", na.rm = TRUE)
housing_dfs_twp_urb_ua_w <- winsorize_data(housing_dfs_twp_urb_ua, "sale_amount", na.rm = TRUE)
housing_dfs_twp_rur_cd_w <- winsorize_data(housing_dfs_twp_rur_cd, "sale_amount", na.rm = TRUE)
housing_dfs_twp_rur_ua_w <- winsorize_data(housing_dfs_twp_rur_ua, "sale_amount", na.rm = TRUE)


housing_dfs_twp_w <- winsorize_data(housing_dfs_twp, "sale_amount", na.rm = TRUE)

map(housing_dfs_twp_urb_ua_w, ~summary(.x$sale_amount))
map(housing_dfs_twp_rur_ua_w, ~summary(.x$sale_amount))

# cleaning the datasets
dfs_twp_urb_ua_w <- purrr::map(housing_dfs_twp_urb_ua_w, ~.x %>% 
                    filter(description == "R" & duration != "1000") %>%
                    drop_na(sale_amount) %>%
                    mutate(votes_pct_against = 100 - votes_pct_for) %>%
                    mutate(treated = if_else(votes_pct_against > cutoff, 1, 0),
                           ln_sale_amount = log(sale_amount))            
)
dfs_twp_rur_ua_w <- purrr::map(housing_dfs_twp_rur_ua_w, ~.x %>% 
                    filter(description == "R" & duration != "1000") %>%
                    drop_na(sale_amount) %>%
                    mutate(votes_pct_against = 100 - votes_pct_for) %>%
                    mutate(treated = if_else(votes_pct_against > cutoff, 1, 0),
                           ln_sale_amount = log(sale_amount))            
)

# aggregating the datasets (without and with covariates)
yr_t_names <- dfs_twp_urb_ua_w$housing_roads_census_t_plus_10_matches %>% select(starts_with("yr_t_")) %>% colnames() %>% sort()
dfs_agg_urb_ua_w <- purrr::map2(.x = dfs_twp_urb_ua_w, .y = yr_t_names, ~ .x %>% 
                         drop_na(sale_amount) %>%
                         group_by(tendigit_fips, eval(parse(text = .y)), year, votes_pct_against) %>%
                         rename(vote_year = year, year = `eval(parse(text = .y))`) %>%
                         summarize(median_sale_amount = median(sale_amount, na.rm = TRUE),
                                   median_ln_sale_amount = median(ln_sale_amount, na.rm = TRUE))              
)
dfs_agg_covs_urb_ua_w <- purrr::map(.x = dfs_agg_urb_ua_w, ~ .x %>% 
                             dplyr::left_join(y = census, by = c("tendigit_fips","vote_year")) %>%
                             ungroup()
)
dfs_agg_rur_ua_w <- purrr::map2(.x = dfs_twp_rur_ua_w, .y = yr_t_names, ~ .x %>% 
                                  drop_na(sale_amount) %>%
                                  group_by(tendigit_fips, eval(parse(text = .y)), year, votes_pct_against) %>%
                                  rename(vote_year = year, year = `eval(parse(text = .y))`) %>%
                                  summarize(median_sale_amount = median(sale_amount, na.rm = TRUE),
                                            median_ln_sale_amount = median(ln_sale_amount, na.rm = TRUE))              
)
dfs_agg_covs_rur_ua_w <- purrr::map(.x = dfs_agg_rur_ua_w, ~ .x %>% 
                                      dplyr::left_join(y = census, by = c("tendigit_fips","vote_year")) %>%
                                      ungroup()
)

# running regressions without covariates
regs_urb_ua_w <- purrr::map(dfs_agg_urb_ua_w, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))
tes_urb_ua_w <- te_tables(regs_urb_ua_w)
plot_te(tes_urb_ua_w)
mean(tes_urb_ua_w[4:14,"robust_coef"] %>% pull() ) / mean(map_dbl(dfs_agg_urb_ua_w, ~median(.x$median_sale_amount))[4:14])

regs_rur_ua_w <- purrr::map(dfs_agg_rur_ua_w, ~ rdrobust::rdrobust(y = .x$median_sale_amount, x = .x$votes_pct_against, c = cutoff, all = TRUE))
tes_rur_ua_w <- te_tables(regs_rur_ua_w)
plot_te(tes_rur_ua_w)

# running regressions with covariates
regs_covs_urb_ua_w <- purrr::map2(dfs_agg_covs_urb_ua_w, covs_final_urb_ua ,
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
tes_covs_urb_ua_w <- te_tables(regs_covs_urb_ua_w)
tes_covs_urb_ua_w[4:14,"robust_coef"] %>% pull %>% mean / map_dbl(dfs_agg_covs_urb_ua_w, ~median(.x$median_sale_amount))[4:14] %>% mean

corr_mat_list <- map(dfs_agg_covs_urb_ua[4:14], ~ cor(.x %>% 
           select("median_sale_amount", covs_my_list)))

plot_te(tes_covs_urb_ua_w)

cor(dfs_agg_covs_urb_ua_w$housing_roads_census_t_plus_4_matches %>% select(-tendigit_fips, -vote_year, -votes_pct_against, -median_sale_amount, -median_ln_sale_amount) %>% select(-contains("pct")), use = "complete.obs")

v <- cor(dfs_agg_covs_urb_ua_w$housing_roads_census_t_plus_4_matches %>% 
      select("median_sale_amount", covs_my_list), 
    use = "complete.obs")
z <- cor(dfs_agg_covs_urb_ua_w$housing_roads_census_t_plus_4_matches %>% 
           select("votes_pct_against", covs_my_list), 
         use = "complete.obs")
z2 <- cor(dfs_agg_covs_urb_ua_w$housing_roads_census_t_plus_4_matches %>% 
           select("votes_pct_against", covs_list), 
         use = "complete.obs")


map(corr_mat_list ,corrplot::corrplot(~ .x ))

corrplot::corrplot(v, type = "lower", method = "number", number.digits = 1)
corrplot::corrplot(z, type = "lower", method = "number", number.digits = 1)
corrplot::corrplot(z2, type = "lower", method = "number", number.digits = 1)


regs_covs_urb_ua_w2 <- purrr::map2(dfs_agg_covs_urb_ua_w, covs_final_urb_ua ,
                                  function(x, y){
                                    rdrobust::rdrobust(y = x$median_sale_amount,
                                                       x = x$votes_pct_against, 
                                                       c = cutoff, 
                                                       # covs = x %>% select(y),
                                                       # covs =  x %>% select(c("pop", "poverty", "pctmin", "pctown")),
                                                       covs =  x %>% select(covs_my_list, "pctgraddeg"),
                                                       all = TRUE)
                                  }
)
tes_covs_urb_ua_w2 <- te_tables(regs_covs_urb_ua_w2)
plot_te(tes_covs_urb_ua_w2)


tes_covs_urb_ua_w2[4:14,"robust_coef"] %>% pull %>% mean / map_dbl(dfs_agg_covs_urb_ua_w, ~median(.x$median_sale_amount))[4:14] %>% mean
