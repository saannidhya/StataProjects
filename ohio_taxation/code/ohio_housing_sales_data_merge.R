

# packages
library(haven)
library(tidyverse)

# location
root = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"

# loading datasets into memory
oh_housing_old <- haven::read_stata(file = paste0(root,"/data/ohiohousesales9516_cleaned.dta"))

#oh_housing_new <- haven::read_stata(file = paste0(root,"/data/ohiohousesales_1621_cleaned.dta"), n_max = 100, encoding = "latin1")

write_sas(oh_housing_old,paste0(root,"/data/ohiohousesales9516_cleaned_R.dta"))

oh_housing_old <- readr::read_csv(paste0(root,"/data/outputs/ohiohousesales9516_cleaned.csv"))
