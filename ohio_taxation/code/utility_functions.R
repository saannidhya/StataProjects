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