
# takes in a dataset, an outcome variable and a covariate list and outputs a covariate list that gives the lowest p-value for treatment effect
find_covs <- function(df, y, covs_list, dummies = NULL){
  
  # initialize
  cv_list <- NULL
  fnl <- list()
  
  # RD without any covariates
  if (!is.null(dummies)){
    og <- rdrobust(y = df[[y]],
                   x = df$votes_pct_against,
                   c = cutoff,
                   covs = df %>% select(all_of(dummies)),
                   all = TRUE)
    # print("og ran with dummies")
  }
  else {
    og <- rdrobust(y = df[[y]],
                   x = df$votes_pct_against,
                   c = cutoff,
                   covs = NULL,
                   all = TRUE)
  }  
  
  # storing p-value of treatment effect
  og_p <- og$pv[[1]]
  
  # RD including one covariate at a time
  for (variable in covs_list){
    
    # add variable to cv_list
    cv_list <- c(cv_list,variable)
    if (!is.null(dummies)){
      nw <- rdrobust(y = df[[y]],
                     x = df$votes_pct_against, 
                     c = cutoff, 
                     covs = df %>% select(all_of(c(dummies, cv_list))),
                     all = TRUE)
      # print("nw ran with dummies")
    }
    else {
      nw <- rdrobust(y = df[[y]],
                     x = df$votes_pct_against, 
                     c = cutoff, 
                     covs = df %>% select(cv_list),
                     all = TRUE)
    }
    nw_p <- nw$pv[[1]]

    # if p-value of treatment effect does not decrease after include the new covariate, discard the new covariate 
    if (nw_p >= og_p) cv_list <- cv_list[!cv_list %in% c(variable)] 
    
    # print(paste0("current variable: ",variable," | ", "current cv_list: ", paste(cv_list,collapse = ", ")))
    
    if (!(length(cv_list) == 0)){
      # update the original regression
      og <- rdrobust(y = df[[y]],
                     x = df$votes_pct_against,
                     c = cutoff,
                     covs = df %>% select(cv_list) ,
                     all = TRUE)
      og_p <- og$pv[[1]]
    }
  }
  
  fnl[["covariates"]] <- cv_list
  
}

# takes in a list of RDD regression results and returns a dataframe containing bias-corrected treatment estimate and the corresponding p-values 
treatment_effect_summary <- function(list){
  return(data.frame(robust_coef = purrr::map_dbl(list, ~ .x$coef[3]), 
                    pval = purrr::map_dbl(list, ~ .x$pv[3]) ,
                    se = purrr::map_dbl(list, ~ .x$se[3]) )) 
}

treatment_effect_rand_summary <- function(list){
  return(data.frame(statistic = purrr::map_dbl(list, ~ .x$obs.stat), 
                    pval = purrr::map_dbl(list, ~ .x$p.value) ,
                    ci_low = purrr::map_dbl(list, ~ .x$ci[1]),
                    ci_high = purrr::map_dbl(list, ~ .x$ci[2]))) 
}

# treatment effect table
te_tables <- function(list, rand=FALSE, ci_level = 1.96){
  if (rand == FALSE){
    treatment_effect_summary(list) %>% 
      mutate(conf_int_low = robust_coef - ci_level*se,
             conf_int_high = robust_coef + ci_level*se) %>% as_tibble(rownames = "dataset") %>% 
      mutate(year = str_extract(dataset, pattern = "t_[a-z]+_[0-9]+"),
             ord = str_extract(dataset, pattern = "minus_[0-9]+|plus_[0-9]+"),
             ord = as.numeric(ifelse(str_detect(ord, "minus"), 
                                     paste0("-", str_extract(ord, "[0-9]+")), 
                                     str_extract(ord, "[0-9]+")))) %>% arrange(ord)
  }
  else {
    treatment_effect_rand_summary(list) %>% as_tibble(rownames = "dataset") %>% 
      mutate(year = str_extract(dataset, pattern = "t_[a-z]+_[0-9]+"),
             ord = str_extract(dataset, pattern = "minus_[0-9]+|plus_[0-9]+"),
             ord = as.numeric(ifelse(str_detect(ord, "minus"), 
                                     paste0("-", str_extract(ord, "[0-9]+")), 
                                     str_extract(ord, "[0-9]+")))) %>% arrange(ord)
  }
}

find_covs_sign <- function(df, y, covs_list, sign = c("positive","negative")){
  
  # initialize
  cv_list <- NULL
  fnl <- list()
  
  # check for sign argument
  if (!(sign %in% c("positive", "negative"))) {
    stop("Invalid value for 'sign'. Please choose 'positive' or 'negative'.")
  }  
  
  # RD without any covariates
  og <- rdrobust(y = df[[y]],
                 x = df$votes_pct_against, 
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
                   x = df$votes_pct_against, 
                   c = cutoff, 
                   covs = df %>% select(cv_list) ,
                   all = TRUE)
    nw_p <- nw$pv[[1]]
    nw_est <- nw$coef[[1]]
    
    # if p-value of treatment effect does not decrease after include the new covariate, discard the new covariate 
    if (nw_p >= og_p || (sign == "positive" && nw_est < 0) || (sign == "negative" && nw_est > 0)) {
      cv_list <- cv_list[!cv_list %in% c(variable)] 
    } else {
      # update the original regression
      og <- nw
      og_p <- og$pv[[1]]
    }
    
  }
  
  fnl[["covariates"]] <- cv_list
  
}

winsorize_data <- function(datasets, y, lower = 0.01, upper = 0.99, na.rm = FALSE){
  purrr::map(datasets, function(x){
    lower_bound <- quantile(x[[y]], lower, na.rm = na.rm)
    upper_bound <- quantile(x[[y]], upper, na.rm = na.rm) 
    return(x[x[[y]] > lower_bound & x[[y]] < upper_bound, ])
  })
}

plot_te <- function(te_table, title = ggplot2::waiver(), subtitle = ggplot2::waiver(), caption = ggplot2::waiver()){
  ggplot(te_table, aes(ord, robust_coef)) +       
    geom_point(size = 3, shape = 19, color = "blue") +
    geom_errorbar(aes(ymin = conf_int_low, ymax = conf_int_high), 
                  width = 0.2, color = "grey50", linewidth = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Year",
      y = "Treatment Effect",
      color = "Position"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) + scale_x_continuous(breaks = c(-3:10))
}

plot_te_recenter <- function(te_table, ref_year = -1, title = ggplot2::waiver(), subtitle = ggplot2::waiver(), caption = ggplot2::waiver()){
  te_table_ <- te_table %>% mutate(conf_int_low  = if_else(ord == ref_year, robust_coef, conf_int_low),
                                   conf_int_high = if_else(ord == ref_year, robust_coef, conf_int_high))
  
  ggplot(te_table_, aes(ord, robust_coef)) +       
    geom_point(size = 3, shape = 19, color = "blue") +
    geom_errorbar(aes(ymin = conf_int_low, ymax = conf_int_high), 
                  width = 0.2, color = "grey50", linewidth = 0.7) +
    geom_hline(yintercept = te_table_ %>% filter(ord == ref_year) %>% pull(robust_coef), linetype = "dashed", color = "red", size = 1) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Year",
      y = "Treatment Effect",
      color = "Position"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) + scale_x_continuous(breaks = c(-3:10))
}

# Performs forward selection of covariates based on p-value cutoff for enhancing the model's explanatory power regarding treatment effect.
forward_selection_covs <- function(df, y, x = "votes_pct_against", covs_list){
  cv_list <- NULL
  
  # original lm() reg
  og_form <- as.formula(paste(y, "~", paste(c(x, "treated", "treat_times_votes"), collapse = " + ")))
  og <- lm(formula = og_form, data = df)
  og_t <- summary(og)$coefficients[,"Pr(>|t|)"][["treated"]]
  og_f <- summary(og)$fstatistic[["value"]]
  
  for (variable in covs_list){
    # add variable to cv_list
    formula_str <- paste(y, "~", paste(c(x, "treated", "treat_times_votes", cv_list, variable), collapse = " + "))
    cv_list <- c(cv_list,variable)
    formula_obj <- as.formula(formula_str)
    nw <- lm(data = df, formula = formula_obj)
    if (variable %in% names(summary(nw)$coefficients[,"Pr(>|t|)"])) nw_z <- summary(nw)$coefficients[,"Pr(>|t|)"][[variable]] else next
    nw_t <- summary(nw)$coefficients[,"Pr(>|t|)"][["treated"]]
    nw_f <- summary(nw)$fstatistic[["value"]]
    
    # if p-value of treatment effect does not decrease after including the new covariate, discard the new covariate 
    if (nw_t >= og_t) cv_list <- cv_list[!cv_list %in% c(variable)] 
    
    if (!(length(cv_list) == 0)){
      # update the original regression
      formula_str <- paste(y, "~", paste(c(x, "treated", "treat_times_votes", cv_list), collapse = " + "))
      formula_obj <- as.formula(formula_str)
      og <- lm(formula = formula_obj, data = df)
      og_t <- summary(nw)$coefficients[,"Pr(>|t|)"][["treated"]]
      og_f <- summary(nw)$fstatistic[["value"]]
    }
  }
  return(cv_list)
}

# takes in X, Y and a list of covariates and implements Regression Discontinuity Design (RDD) method using lm() function
rdd_lm <- function(df, y, x, covs, cutoff = 50){
  # run rdrobust first and get the bandwidths
  if (!is.null(covs)) {
    rob <- rdrobust(y = df[[y]],
                    x = df[[x]],
                    c = cutoff,
                    covs = df %>% select(covs) ,
                    all = TRUE)
    
  } else {
    rob <- rdrobust(y = df[[y]],
                    x = df[[x]],
                    c = cutoff,
                    all = TRUE)
  }
  summary(rob)
  bws <- rob$bws["h", ]
  
  # specify dataset based on bandwidths
  df <- df %>% mutate(x_c = .data[[x]] - cutoff,
                      treated = if_else(x_c >= 0, 1, 0),
                      treat_times_votes = x_c*treated) %>% 
    select(c(y, x_c, treated, treat_times_votes, covs)) %>% 
    filter(x_c >= -bws[["left"]] & x_c <= bws[["right"]])
  
  formula_str <- paste(y, "~", paste(c("x_c", "treated", "treat_times_votes", covs), collapse = " + "))
  formula_obj <- as.formula(formula_str)
  return(lm(data = df, formula = formula_obj))
}
# tgt <- rd_model <- rdd_lm(df = dfs_agg_covs$housing_roads_census_t_plus_4_matches, 
#                           y = "median_sale_amount", x = "votes_pct_against", 
#                           covs = covs_final$housing_roads_census_t_plus_4_matches)
# summary(tgt)


# tgt$housing_roads_census_t_plus_4_matches$coefficients[["treated"]]

treatment_effect_summary_lm <- function(list){
  return(data.frame(coef = map_dbl(list,~ summary(.x)$coefficients["treated", "Estimate"]), 
                    pval = map_dbl(list,~ summary(.x)$coefficients["treated", "Pr(>|t|)"]) ,
                    se = map_dbl(list,~ summary(.x)$coefficients["treated", "Std. Error"]) )) 
}
te_tables_lm <- function(list){
    treatment_effect_summary_lm(list) %>% 
      mutate(conf_int_low = coef - 1.96*se,
             conf_int_high = coef + 1.96*se) %>% as_tibble(rownames = "dataset") %>% 
      mutate(year = str_extract(dataset, pattern = "t_[a-z]+_[0-9]+"),
             ord = str_extract(dataset, pattern = "minus_[0-9]+|plus_[0-9]+"),
             ord = as.numeric(ifelse(str_detect(ord, "minus"), 
                                     paste0("-", str_extract(ord, "[0-9]+")), 
                                     str_extract(ord, "[0-9]+")))) %>% arrange(ord)
}
# treatment_effect_summary_lm(tgt)
# te_tables_lm(tgt)

compare_covariates <- function(data, city_col, covariates, city1, city2) {
  # Set options to avoid scientific notation
  options(scipen = 999)
  
  # Filter data for the two specified cities
  data_city1 <- subset(data, data[[city_col]] == city1)
  data_city2 <- subset(data, data[[city_col]] == city2)
  
  # Initialize a results data frame to store comparisons
  results <- data.frame(
    Covariate = covariates,
    City1_Mean = sapply(covariates, function(cov) mean(data_city1[[cov]], na.rm = TRUE)),
    City1_SD = sapply(covariates, function(cov) sd(data_city1[[cov]], na.rm = TRUE)),
    City2_Mean = sapply(covariates, function(cov) mean(data_city2[[cov]], na.rm = TRUE)),
    City2_SD = sapply(covariates, function(cov) sd(data_city2[[cov]], na.rm = TRUE)),
    t_stat = NA,  # Placeholder for t-statistics
    p_value = NA  # Placeholder for p-values
  )
  
  # Perform t-tests for each covariate and store results
  for (i in seq_along(covariates)) {
    cov <- covariates[i]
    t_test_result <- t.test(data_city1[[cov]], data_city2[[cov]], var.equal = TRUE, na.rm = TRUE)
    results$t_stat[i] <- t_test_result$statistic
    results$p_value[i] <- t_test_result$p.value
  }
  
  return(results)
}

# finding the right covariates
right_covs <- function(df, covs_list, num_covs = 3, cutoff = 50, pv_above_flag = TRUE, pv_threshold = 0.05, coef_above_zero = TRUE, y = "median_sale_amount", running_var = "votes_pct_against"){
 # start by
  purrr::map(combn(covs_list, num_covs, simplify = FALSE), function(x) {
    
    # find the covariate where treatment effect is positive and insignificant
    rg <- rdrobust::rdrobust(y = df[[y]],
                             x = df[[running_var]], 
                             c = cutoff, 
                             covs = df %>% select(x),
                             all = TRUE)
    
    if (coef_above_zero & pv_above_flag) {
      if (rg$coef[3] > 0 & rg$pv[3] > pv_threshold) return(x)
    } else if (coef_above_zero & !pv_above_flag) {
      if (rg$coef[3] > 0 & rg$pv[3] < pv_threshold) return(x)
    } else if (!coef_above_zero & pv_above_flag) {
      if (rg$coef[3] < 0 & rg$pv[3] > pv_threshold) return(x)
    } else {
      if (rg$coef[3] < 0 & rg$pv[3] < pv_threshold) return(x)
    }
    
  }) %>% Filter(Negate(is.null), .) %>% return
}
  
#   purrr::map(combn(covs_list, 7, simplify = FALSE), function(x) {
#   
#   # find the covariate where treatment effect is positive and insignificant
#   rg <- rdrobust::rdrobust(y = dfs_agg_covs$housing_roads_census_t_plus_10_matches$median_sale_amount,
#                            x = dfs_agg_covs$housing_roads_census_t_plus_10_matches$votes_pct_against, 
#                            c = cutoff, 
#                            covs = dfs_agg_covs$housing_roads_census_t_plus_10_matches %>% select(x),
#                            all = TRUE)
#   
#   if (rg$coef[3] < 0 & rg$pv[3] < 0.05) return(x)
#   
# }) %>% Filter(Negate(is.null), .)