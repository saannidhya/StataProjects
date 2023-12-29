
# takes in a dataset, an outcome variable and a covariate list and outputs a covariate list that gives the lowest p-value for treatment effect
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

# takes in a list of RDD regression results and returns a dataframe containing bias-corrected treatment estimate and the corresponding p-values 
treatment_effect_summary <- function(list){
  return(data.frame(bias_corrected_coef = purrr::map_dbl(list, ~ .x$coef[2]), 
                    pval = purrr::map_dbl(list, ~ .x$pv[2]) ,
                    se = purrr::map_dbl(list, ~ .x$se[2]) )) 
}

treatment_effect_rand_summary <- function(list){
  return(data.frame(statistic = purrr::map_dbl(list, ~ .x$obs.stat), 
                    pval = purrr::map_dbl(list, ~ .x$p.value) ,
                    ci_low = purrr::map_dbl(list, ~ .x$ci[1]),
                    ci_high = purrr::map_dbl(list, ~ .x$ci[2]))) 
}

# treatment effect table
te_tables <- function(list, rand=FALSE){
  if (rand == FALSE){
    treatment_effect_summary(list) %>% 
      mutate(conf_int_low = bias_corrected_coef - 1.96*se,
             conf_int_high = bias_corrected_coef + 1.96*se) %>% as_tibble(rownames = "dataset") %>% 
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

winsorize_data <- function(datasets, y, lower = 0.01, upper = 0.99){
  purrr::map(datasets, function(x){
    lower_bound <- quantile(x[[y]], lower)
    upper_bound <- quantile(x[[y]], upper) 
    return(x[x[[y]] > lower_bound & x[[y]] < upper_bound, ])
  })
}

plot_te <- function(te_table, title = "", subtitle = ""){
  ggplot(te_table, aes(ord, bias_corrected_coef)) +       
    geom_point(size = 3, shape = 19, color = "blue") +
    geom_errorbar(aes(ymin = conf_int_low, ymax = conf_int_high), 
                  width = 0.2, color = "grey50", size = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Year",
      y = "Treatment Effect",
      color = "Position"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) + scale_x_continuous(breaks = c(-2, -1, 1:10))
}
