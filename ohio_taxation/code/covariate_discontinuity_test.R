#================================================================================================================#
# Purpose : To perform covariate discontinuity test. A covariate that has a "jump" at the cutoff of the running
#           variable cannot be used for RD analysis.
# Name    : Saani Rawat
# Created : 07/21/2023
# Log     : 
#           07/21/2023: wrote the program to automate the test for all covariates, regardless of the dataset
#           02/02/2024: updated the run. Looking at pre-treatment covariates as 
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
# source(paste0(code,"/employment_data_setup.R"))


# input dataset. This should be a list of t-2 to t+10 datasets (can be employment, fars or housing)
# E.g. dfs_agg_covs, dfs_emp_agg3
# df = dfs_emp_agg3
# df = dfs_agg_covs

# take all covariate names 
covariate_list <- c("pctnokids" ,"pctgraddeg" ,"pctlt5" ,"pctblack" ,"raceherfindahl" ,"pctdivorced" ,"childpov" ,
                    "pctlesshs" ,"unemprate" ,"pct5to17" ,"pctamerind" ,"pcthisp" ,"lforcepartrate" ,"poverty" ,
                    "pcthsgrad" ,"medfamy" ,"pct18to64" ,"pctapi" ,"pctmarried" ,"incherfindahl" ,"pctwithkids" ,
                    "pctsomecoll" ,"pctrent" ,"pct65pls" ,"pctotherrace" ,"pctnevermarr" ,"pop" ,
                    "pctsinparhhld" ,"pctbachelors" ,"pctown" ,"pctwhite" ,"pctmin" ,"pctseparated")

# importing roads and census dataset. Selecting only renewals and levies that do not last forever. Separating into treatment and control groups.
roads_and_census <- haven::read_dta(paste0(data,"/roads_and_census.dta")) %>%
  select(-matches("yr_t_")) %>%
  filter(description == "R" & duration != 1000) %>%
  janitor::clean_names() %>%
  mutate(treated = if_else(votes_pct_against >= cutoff, 1, 0))    

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
# running covariate discontinuity regressions  ----
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

cv_regs <- purrr::map(chr_lst, 
           ~ rdrobust::rdrobust(y = roads_and_census %>% select(.x) %>% pull(), x = roads_and_census$votes_pct_against, c = cutoff, all = TRUE))
names(cv_regs) <- chr_lst  

te_cv_regs <- te_tables(cv_regs) %>% arrange(pval)
sort(te_cv_regs$pval)
min(te_cv_regs$pval)
max(te_cv_regs$pval)
# View(te_cv_regs)

write.csv(select(te_cv_regs, -c("year", "ord")), paste0(tables, "/covariate_discontinuity_test.csv"), row.names = FALSE)
