# Name : Saani
# Purpose: 
# 1. observe road tax voting data around cutoff (within different bandwidths), create co-variate summary tables
# 2. exploring running variable, outcome variable
# Log:
# 1. 03-10-2022
# 2. 03-12-2022

# loading libraries
library(tidyverse)
library(readr)
library(readstata13)
library(haven)
library(rddensity)
library(purrr)
# library(Rctmd)

# set your working directory (don't wanna use here::here())
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"

# setwd(root)
# reading in dataset
# road_df <- read.dta13("data/road_tax_voting.dta")
# road_df <- haven::read_dta("data/road_tax_voting.dta")
df <- haven::read_sas(paste0(root,"/data/tax_census_emplmnt_oh.sas7bdat"))
road_df <- haven::read_sas(paste0(root,"/data/road_tax_and_census_oh.sas7bdat")) %>% filter(description == "R")
road_df$pct_votes_for <- road_df$prop_votes_for * 100

# only taking passed levies
road_df_for <- road_df %>%
                filter(prop_votes_for >= 0.5)

# only taking failed tax levies
road_df_agnst <- road_df %>%
  filter(prop_votes_for < 0.50)

# performing density discontinuity test by Cattaneo, Jansson, and Ma (2020)
denstest <- rddensity(road_df$pct_votes_for, c = 0)
denstest <- rddensity(road_df$prop_votes_for, c = 0.5)
rdplotdensity(denstest, road_df$prop_votes_for)
summary(denstest)

mean(road_df_for$prop_votes_for)
mean(road_df_agnst$prop_votes_for) 

# counties around certain cut-offs
count_around_cutoffs <- function(cutoff){
    cut_list <- list()
    cut_list$barely_passed <- count(road_df_for %>% filter(pct_votes_for <= 50 + cutoff))[[1]]
    cut_list$barely_failed <- count(road_df_agnst %>% filter(pct_votes_for > 50 - cutoff)) [[1]]
    
    return(cut_list)
  
}

count_around_cutoffs(2) # 72, 47
count_around_cutoffs(3) # 121, 63
count_around_cutoffs(5) # 256, 98
count_around_cutoffs(10) # 727, 133
count_around_cutoffs(15) # 1392, 148

#############################################################################
# Comparing Covariates between full-sample, passed levies and failed levies
############################################################################
cov_tables <- function(cut_list){
  g <- map(cut_list, ~road_df %>%
             select(c(tendigit_fips, subdivision_name, county, year, vote_result, lforcepartrate, starts_with("pct"))) %>%
             filter( between(pct_votes_for,50-{.x},50+{.x})) %>% 
             group_by(vote_result) %>%
             summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))
  )
  
  f <- map(cut_list, ~road_df %>%
             select(c(tendigit_fips, subdivision_name, county, year, vote_result, lforcepartrate, starts_with("pct"))) %>%
             filter( between(pct_votes_for,50-{.x},50+{.x})) %>% 
             summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
             mutate(vote_result = "full")
  )
  cov_tbl <- map(seq(1:length(cut_list)), ~rbind(g[[{.x}]], f[[{.x}]]) %>% 
                   select(-c(contains("lead"), contains("lag"), tendigit_fips, year)))
  return(cov_tbl)
}

#############################################################################
# Density Plot test for regression discontinuity
############################################################################

# density plot
ggplot(road_df, mapping = aes(x = pct_votes_for, fill = vote_result)) +
  geom_histogram(binwidth = 1, boundary = 50, color = "white") + 
  geom_vline(xintercept = 50) +
  theme_bw() 

# mccrary density test
density_test <- rddensity(road_df$pct_votes_for, c = 50)
rdplotdensity(rdd = density_test, 
              X = road_df$pct_votes_for,
              type = "both")

 cov_tbls <- cov_tables(cut_list = c(2,3,5,10,12))
pivot_longer(data = cov_tbls[[1]], lforcepartrate:pctdivorced)

p_list <- c("poverty", "poverty_lag1", "poverty_lag2")

map(p_list, ~ ggplot(data =road_df, aes(x = pct_votes_for, y =  get({.x}), color = vote_result)) +
      geom_point() +
      stat_smooth(method = "lm")
      )

# util_plt_point(df = road_df, x = pct_votes_for, y = poverty, color = vote_result, x_refline = 50, smooth = TRUE, method = "lm")

#############################################################################
# Plotting Running and outcome variables 
#############################################################################
ggplot(data = df %>% filter(between(prop_votes_for, 0.40, 0.60)), aes(x = prop_votes_for, y =  vote_result, color = vote_result)) +
  geom_point() + 
  geom_vline(xintercept = 0.5) +
  geom_jitter()

#############################################################################
# Plotting Running and outcome variables 
#############################################################################

# o_list <- c(colnames(select(df,contains("num_employed_lead"))), colnames(select(df,contains("wage_lead"))))
o_list <- c(colnames(select(df,contains("num_employed_lead"))))

map(o_list, ~ ggplot(data = df %>% filter(num_employed <= 1500000)) +
      geom_point(aes(x = num_employed, y =  get({.x}), color = vote_result)) + 
      ylab({.x}) + 
      # geom_vline(xintercept = 0.5) + 
      stat_smooth(aes(x = prop_votes_for, y =  get({.x}), color = vote_result), method = "lm" ) + 
      scale_y_continuous(labels = scales::comma))

args(ggplot)


