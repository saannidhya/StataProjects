#================================================================================================================#
# Purpose : To perform covariate discontinuity test. A covariate that has a "jump" at the cutoff of the running
#           variable cannot be used for RD analysis.
# Name    : Saani Rawat
# Created : 07/21/2023
# Log     : 
#           07/21/2023: wrote the program to automate the test for all covariates, regardless of the dataset
#================================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

# running data setup code
source(paste0(code,"/utility_functions.R"))
source(paste0(code,"/housing_data_setup.R"))
source(paste0(code,"/employment_data_setup.R"))


# input dataset. This should be a list of t-2 to t+10 datasets (can be employment, fars or housing)
# E.g. dfs_agg_covs, dfs_emp_agg3
df = dfs_emp_agg3


# take all covariate names 
covariate_list <- c("pctnokids" ,"pctgraddeg" ,"pctlt5" ,"pctblack" ,"raceherfindahl" ,"pctdivorced" ,"childpov" ,
                    "pctlesshs" ,"unemprate" ,"pct5to17" ,"pctamerind" ,"pcthisp" ,"lforcepartrate" ,"poverty" ,
                    "pcthsgrad" ,"medfamy" ,"pct18to64" ,"pctapi" ,"pctmarried" ,"incherfindahl" ,"pctwithkids" ,
                    "pctsomecoll" ,"pctrent" ,"pct65pls" ,"pctotherrace" ,"pctnevermarr" ,"inctaxrate" ,"pop" ,
                    "pctsinparhhld" ,"pctbachelors" ,"pctown" ,"pctwhite" ,"pctmin" ,"pctseparated")

#================================================================#
# running covariate discontinuity regressions (aggregate) ----
#================================================================#

covs_regs_ls <- vector("list", length(df)) # Initialize list of lists
names(covs_regs_ls) <- names(df) # Assign names to covs_regs_ls based on df
for (i in seq_along(df)){
  dataset <- df[[i]]
  
  covs_regs <- purrr::map(covariate_list, 
                          ~ rdrobust::rdrobust(y = dataset %>% select(.x) %>% pull(), x = dataset$votes_pct_for, c = cutoff, all = TRUE))
  names(covs_regs) <- covariate_list  
  
  covs_regs_ls[[i]] <- covs_regs
}
# beepr::beep("mario") 

# Note: covs_regs_ls is a list of lists. Each element of  covs_regs_ls is a list of regression results based on using 34 covariate variables as outcomes 

#================================================================#
# running covariate discontinuity regressions (aggregate) ----
#================================================================#
covs_regs_pvals <- purrr::imap(covs_regs_ls, ~treatment_effect_summary(.x) %>% rownames_to_column() %>% tibble() %>%
                              mutate(dataset = .y, significant = if_else(pval < 0.05, 1, 0))) %>%
                              bind_rows() %>% arrange(rowname) %>% rename(variable = rowname)
  
# Note: .y referes to the name of .x
# None of the variables in the covariate list had a consistently significant Treatment Effect

# View(covs_regs_pvals)
write.csv(covs_regs_pvals, paste0(tables, "/covariate_discontinuity_test.csv"), row.names = FALSE)
