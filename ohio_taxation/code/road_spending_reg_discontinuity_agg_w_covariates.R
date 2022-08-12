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
# source(paste0(code,"/housing_data_setup.R"))
# running covariates balance test
source(paste0(code,"/covariates_balance_test.R"))

# user-defined parameters  ----
t_test_sig_level <- 0.05


#============================================================================================================#
#     Introducing covariates (using median sale_amount for each county, vote and year)  into regression ---- 
#============================================================================================================#

# covariates which passed t-test (failed to reject null) for the specific dataset and are not potential outcome vars
var_list <- purrr::map(names(dfs_agg_covs), 
                       ~ all_tests %>%
                         filter((dataset == .x) & 
                                  (`p-value` > t_test_sig_level) & 
                                  !(variable %in% c('median_sale_amount','median_ln_sale_amount','median_sale_amount_per_sq_feet'))) %>%
                         select(variable) %>%
                         pull() )
names(var_list) <- names(dfs_agg_covs)

df <- dfs_agg_covs$housing_roads_census_t_plus_9_matches %>% 
        mutate(treated = if_else(votes_pct_for >= cutoff, 1, 0))

# find common variables between t+1, ..., t+10 (these variables passed covariates balance test for all years)
Reduce(intersect, var_list)


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


rdds$housing_roads_census_t_plus_1_matches

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
#    |- Calculating V.I.F for selected covariates ----
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

test_func <- function(dfs, v_list){
  
  # initialize an empty list
  p_val_list <- list()
  
  # run regressions without covariates
  no_cov_reg <- purrr::map(dfs, 
                    ~rdrobust::rdrobust(y = {.x}$median_sale_amount,
                     x = {.x}$votes_pct_for,
                     c = cutoff, 
                     all = TRUE,
                     vce = "hc0"
                    )$pv[1])
  p_val_list[["no_covars"]] <- no_cov_reg # conventional
  
  for (v in v_list){
  # run regression with one covariate only
  cov_reg <-  purrr::map(dfs, 
                         ~ rdrobust::rdrobust(y = {.x}$median_sale_amount,
                                              x = {.x}$votes_pct_for,
                                              c = cutoff, 
                                              all = TRUE,
                                              vce = "hc0",
                                              covs = {.x} %>% select(v)
                         )$pv[1])
  p_val_list[[v]] <-  cov_reg # conventional
  }
  return(p_val_list)
}

covs_reg_pvalues <- test_func(dfs_agg_covs, v_list)

covs_reg_pvalues <- test_func(dfs_agg_covs, covs_list)


#============================================================================================================#
#     Introducing covariates (using median sale_amount per square feet)  into regression ---- 
#============================================================================================================#

# rdrobust(y = dfs_agg_per_covs$housing_roads_census_t_plus_4_matches$median_sale_amount_per_sq_feet,
#          x = dfs_agg_per_covs$housing_roads_census_t_plus_4_matches$votes_pct_for, 
#          c = cutoff, 
#          all = TRUE) %>% summary()$pv[[1]]
# rdrobust(y = dfs_agg_per_covs$housing_roads_census_t_plus_4_matches$median_sale_amount_per_sq_feet,
#          x = dfs_agg_per_covs$housing_roads_census_t_plus_4_matches$votes_pct_for, 
#          c = cutoff, 
#          covs = dfs_agg_per_covs$housing_roads_census_t_plus_4_matches %>% select(c(pop)) ,
#          all = TRUE) %>% summary()

find_covs <- function(df, y, covs_list){
  
  # initialize
  cv_list <- NULL
  fnl <- list()
  
  # RD without any covariates
  og <- rdrobust(y = df[[y]],
                 x = df$votes_pct_for, 
                 c = cutoff, 
                 covs = NULL ,
                 all = TRUE)
  # storing p-value of treatment effect
  og_p <- og$pv[[1]]
  
  # RD including one covariate at a time
  for (variable in covs_list){
    
    # add variable to cv_list
    cv_list <- c(cv_list,variable)
    nw <- rdrobust(y = df[[y]],
                   x = df$votes_pct_for, 
                   c = cutoff, 
                   covs = df %>% select(cv_list) ,
                   all = TRUE)
    nw_p <- nw$pv[[1]]
    
    # if p-value of treatment effect does not decrease after include the new covariate, discard the new covariate 
    if (nw_p >= og_p) cv_list <- cv_list[!cv_list %in% c(variable)] 
    
    # print(paste0("current variable: ",variable," | ", "current cv_list: ", paste(cv_list,collapse = ", ")))
    
    if (!(length(cv_list) == 0)){
      # update the original regression
      og <- rdrobust(y = df[[y]],
                     x = df$votes_pct_for,
                     c = cutoff,
                     covs = df %>% select(cv_list) ,
                     all = TRUE)
      og_p <- og$pv[[1]]
    }
  }
  
  fnl[["covariates"]] <- cv_list

}

# different sets of covariates for different years
# median sale amount
covs_final <- purrr::map(dfs_agg_covs, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))
# median sale amount per square feet
covs_final_per <- purrr::map(dfs_agg_per_covs, ~find_covs(.x, y = "median_sale_amount_per_sq_feet", covs_list = covs_list))

### regressions with covariates for median sale amount
g4 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_4_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_4_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_4_matches %>%
                   select(covs_final$housing_roads_census_t_plus_4_matches) ,
                 all = TRUE) 
g5 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_5_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_5_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_5_matches %>%
                   select(covs_final$housing_roads_census_t_plus_5_matches) ,
                 all = TRUE) 
g6 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_6_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_6_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_6_matches %>%
                   select(covs_final$housing_roads_census_t_plus_6_matches) ,
                 all = TRUE) 
g7 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_7_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_7_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_7_matches %>%
                   select(covs_final$housing_roads_census_t_plus_7_matches) ,
                 all = TRUE) 
g8 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_8_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_8_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_8_matches %>%
                   select(covs_final$housing_roads_census_t_plus_8_matches) ,
                 all = TRUE) 
g9 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_9_matches$median_sale_amount,
                 x = dfs_agg_covs$housing_roads_census_t_plus_9_matches$votes_pct_for,
                 c = cutoff,
                 covs = dfs_agg_covs$housing_roads_census_t_plus_9_matches %>%
                   select(covs_final$housing_roads_census_t_plus_9_matches) ,
                 all = TRUE) 
g10 <- rdrobust(  y = dfs_agg_covs$housing_roads_census_t_plus_10_matches$median_sale_amount,
                  x = dfs_agg_covs$housing_roads_census_t_plus_10_matches$votes_pct_for,
                  c = cutoff,
                  covs = dfs_agg_covs$housing_roads_census_t_plus_10_matches %>%
                    select(covs_final$housing_roads_census_t_plus_10_matches) ,
                  all = TRUE) 
summary(g4)
summary(g5)
summary(g6)
summary(g7)
summary(g8)
summary(g9)
summary(g10)

### regressions with covariates for median sale amount per square feet

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
                   select(covs_final_per$housing_roads_census_t_plus_5_matches) ,
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
                   select(covs_final_per$housing_roads_census_t_plus_7_matches) ,
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
                   select(covs_final_per$housing_roads_census_t_plus_9_matches) ,
                 all = TRUE) 
f10 <- rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_10_matches$median_sale_amount_per_sq_feet,
                  x = dfs_agg_per_covs$housing_roads_census_t_plus_10_matches$votes_pct_for,
                  c = cutoff,
                  covs = dfs_agg_per_covs$housing_roads_census_t_plus_10_matches %>%
                    select(covs_final_per$housing_roads_census_t_plus_10_matches) ,
                  all = TRUE) 
summary(f4)
summary(f5)
summary(f6)
summary(f7)
summary(f8)
summary(f9)
summary(f10)
rdrobust(  y = dfs_agg_per_covs$housing_roads_census_t_plus_7_matches$median_sale_amount_per_sq_feet,
           x = dfs_agg_per_covs$housing_roads_census_t_plus_7_matches$votes_pct_for,
           c = cutoff,
           all = TRUE) %>% summary()

