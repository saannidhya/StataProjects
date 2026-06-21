






# loading packages
packages <- c("Rbearcat", "tidyverse", "lubridate", "haven", "stringr", "here", "knitr", "janitor", "scales","data.table","rdrobust", "fastDummies")
for (pkg in packages){
  library(pkg, character.only = TRUE)
}

# CHOOSE: dta or csv
hs_file_type <- "dta"

# global vars
cutoff <- 50

# specify the shared location
if (hs_file_type == "dta") {
  shared <- "//cobshares.uccob.uc.edu/economics$/Julia/roads"
} else if (hs_file_type == "csv") {
  shared <- paste0(data, "/housing")
} else {
  stop("Invalid 'hs_file_type': must be either 'dta' or 'csv'")
}


#============================================#
#  Importing Housing datasets as a list ----
#============================================#
# storing all housing dfs as a list
dataset_names <- if (hs_file_type == "dta") {
  stringr::str_remove(
    list.files(shared, pattern = "matches", recursive = FALSE),
    "\\.dta$"
  )
} else {
  stringr::str_remove(
    list.files(shared, pattern = "matches", recursive = FALSE),
    "\\.csv$"
  )
}

# import data
housing_dfs <- if (hs_file_type == "dta") {
  purrr::map(
    list.files(shared, pattern = "matches", recursive = FALSE, full.names = TRUE),
    haven::read_dta
  )
} else {
  purrr::map(
    list.files(shared, pattern = "matches", recursive = FALSE, full.names = TRUE),
    readr::read_csv
  )
}
# assign names to housing dfs
housing_dfs <- stats::setNames(housing_dfs, dataset_names)

start_time_full <- Sys.time()
# Residualize SALE_AMOUNT by removing effects of hedonics, year, and TENDIGIT_FIPS
housing_dfs <- purrr::map(housing_dfs, function(df) {
    # Identify hedonic variables (excluding outcome, treatment, and fixed effects)
    hedonic_vars <- c("total_rooms", "total_baths_calculated", "agehouse", "ac",  "basement", "cond_good",  "cond_fair", "cond_poor", "onestory", "condo")
    
    # Create formula for residualization
    formula_str <- paste0(
        "SALE_AMOUNT ~ ", 
        paste(hedonic_vars, collapse = " + ")
        ," + factor(TENDIGIT_FIPS) + factor(year)"
    )
    
    # Run regression
    resid_model <- lm(as.formula(formula_str), data = df, na.action = na.exclude)
    
    # Store residuals as new variable
    # df$SALE_AMOUNT_resid <- residuals(resid_model)
    df$SALE_AMOUNT_resid <- stats::residuals(resid_model)  # now length == nrow(df)
    
    return(df)
})
end_time_full <- Sys.time()
elapsed_time_full <- end_time_full - start_time_full
message("NOTE: Run time: ", elapsed_time_full)



# Run regression
rd_hs <- purrr::map(housing_dfs, ~ rdrobust(y = .x$SALE_AMOUNT_resid,
                                            x = .x$votes_pct_against,
                                            c = 50,
                                            all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = .x$TENDIGIT_FIPS))

smry_rd <- map(rd_hs, ~ summary(.x))

summary(rd_hs$housing_roads_census_t_plus_0_matches)
summary(rd_hs$housing_roads_census_t_plus_1_matches)
summary(rd_hs$housing_roads_census_t_plus_2_matches)
summary(rd_hs$housing_roads_census_t_plus_3_matches)
summary(rd_hs$housing_roads_census_t_plus_4_matches)
summary(rd_hs$housing_roads_census_t_plus_5_matches)
summary(rd_hs$housing_roads_census_t_plus_6_matches)
summary(rd_hs$housing_roads_census_t_plus_7_matches)
summary(rd_hs$housing_roads_census_t_plus_8_matches)
summary(rd_hs$housing_roads_census_t_plus_9_matches)
summary(rd_hs$housing_roads_census_t_plus_10_matches)

gs <- purrr::map2(covs_final, dfs_agg_covs, .f = function(x,y){
                              # print(paste0("Covariates list: ", deparse(substitute(y))))
                              # print(paste0("Covariates list: ", x))
                              rdrobust(  y = y$median_sale_amount,
                                         x = y$votes_pct_against,
                                         c = cutoff,
                                         covs = y %>%
                                           dplyr::select(x) ,
                                         all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = y$tendigit_fips) })


# finding best covariates
covs_list <- c("pop" ,"childpov" ,"poverty" ,"pctwithkids" ,"pctsinparhhld" ,"pctnokids" ,
               "pctlesshs" ,"pcthsgrad" ,"pctsomecoll" ,"pctbachelors" ,"pctgraddeg" ,"unemprate" ,"medfamy" ,"pctrent" ,"pctown" ,"pctlt5" ,
               "pct5to17" ,"pct18to64" ,"pct65pls" ,"pctwhite" ,"pctblack" ,"pctamerind" ,"pctapi" ,"pctotherrace" ,"pctmin" ,"raceherfindahl" ,
               "pcthisp" ,"pctmarried" ,"pctnevermarr" ,"pctseparated" ,"pctdivorced" ,"lforcepartrate" ,"incherfindahl")

# selecting the best set of covariates for each median sale amount period
covs_final <- purrr::map(dfs_agg_covs, ~find_covs(.x, y = "median_sale_amount", covs_list = covs_list))
