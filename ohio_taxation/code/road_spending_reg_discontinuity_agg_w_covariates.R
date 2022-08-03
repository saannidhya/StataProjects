#==========================================================================================================#
# Purpose : Build on road_spending_reg_discontinuity_agg.R and add covariates to the regression
# Name    : Saani Rawat
# Created : 07/30/2022
# Log     : 
#           08/01/2022: 
#==========================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))

# user-defined parameters  ----
t_test_sig_level <- 0.05


#============================================================================================================#
#     Introducing covariates into regression (using median sale_amount for each county, vote and year) ----
#============================================================================================================#

# covariates which passed t-test (failed to reject null) for the specific dataset and are not potential outcome vars
var_list <- all_tests %>%
              filter((dataset == "housing_roads_census_t_plus_10_matches") & 
                       (`p-value` > t_test_sig_level) & 
                       !(variable %in% c('median_sale_amount','median_ln_sale_amount','median_sale_amount_per_sq_feet'))) %>%
              select(variable) %>%
              pull()


df <- dfs_agg_covs$housing_roads_census_t_plus_9_matches %>% 
        mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0))

# |- after adding one variable at a time, one possible covariate combo ----
rdds <- purrr::map(dfs_agg_covs, ~ rdrobust::rdrobust(y = .x$median_sale_amount,
                                              x = .x$votes_pct_for, 
                                              c = cutoff, 
                                              covs = .x %>% select(c("pop", # total population of the county subdivision
                                                                     "pctsinparhhld", # proportion of households with own children under 18 living in the household, no spouse present
                                                                     "pctrent", # proportion of housing units that are renter-occupied
                                                                     "pctwhite", # proportion of white population
                                                                     "pctseparated", # proportion of population 15 and older married but separated
                                                                     "incherfindahl", # Leik (1966) index of racial heterogeneity, where 0 is homogeneity and 1 is equal weighting of races (maximum heterogeneity)
                                                                     "pctmin")), # percent of student enrollment that is non-white or white Hispanic for each school year
                                              all = TRUE)
           )

summary(rdds$housing_roads_census_t_plus_10_matches)

rdrobust::rdrobust(y = df$median_sale_amount,
                   x = df$votes_pct_for, 
                   c = cutoff, 
                   covs = df %>% select(c("pop", # total population of the county subdivision
                                          "pctsinparhhld", # proportion of households with own children under 18 living in the household, no spouse present
                                          "pctrent", # proportion of housing units that are renter-occupied
                                          "pctwhite", # proportion of white population
                                          "pctseparated", # proportion of population 15 and older married but separated
                                          "incherfindahl", # Leik (1966) index of racial heterogeneity, where 0 is homogeneity and 1 is equal weighting of races (maximum heterogeneity)
                                          "pctmin")), # percent of student enrollment that is non-white or white Hispanic for each school year
                   all = TRUE) %>% summary()

# |- RD plot ----
rdrobust::rdplot(y = df$median_sale_amount,
                   x = df$votes_pct_for, 
                   c = cutoff, 
                   covs = df %>% select(c("pop", # total population of the county subdivision
                                          "pctsinparhhld", # proportion of households with own children under 18 living in the household, no spouse present
                                          "pctrent", # proportion of housing units that are renter-occupied
                                          "pctwhite", # proportion of white population
                                          "pctseparated", # proportion of population 15 and older married but separated
                                          "incherfindahl", # Leik (1966) index of racial heterogeneity, where 0 is homogeneity and 1 is equal weighting of races (maximum heterogeneity)
                                          "pctmin")) # percent of student enrollment that is non-white or white Hispanic for each school year
                   )

rdrobust::rdrobust(y = df$median_sale_amount,
                   x = df$votes_pct_for, 
                   c = cutoff, all = TRUE, kernel = "uni", h = c(5, 5), vce = "hc0") %>% summary()



#============================================================================================================#
#     Calculating V.I.F for selected covariates ----
#============================================================================================================#

df <- dfs_agg_covs$housing_roads_census_t_plus_9_matches %>% 
  mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0))
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



# function that takes a list and gives p-value of treatment effect for each covariate
v_list <- c("pop", "pctsinparhhld", "pctrent", "pctwhite", "pctseparated", "incherfindahl","pctmin")

test_func <- function(v_list){
  
  p_val_list <- list()
  reg <- rdrobust::rdrobust(y = df$median_sale_amount,
                     x = df$votes_pct_for,
                     c = cutoff, 
                     all = TRUE,
                     vce = "hc0"
                    )
  p_val_list["no_covars"] <- reg$pv[1] # conventional
  
  for (v in v_list){
  reg <-  rdrobust::rdrobust(y = df$median_sale_amount,
                       x = df$votes_pct_for,
                       c = cutoff, 
                       all = TRUE,
                       vce = "hc0",
                       covs = df %>% select(v)
                    )
  p_val_list[v] <-  reg$pv[1] # conventional
  }
  return(p_val_list)
}

test_func(v_list)



