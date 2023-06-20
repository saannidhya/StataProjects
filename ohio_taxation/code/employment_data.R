#================================================================================================================#
# Purpose : Check to see what is wrong with unique addresses merge
# Name    : Saani Rawat
# Created : 06/20/2023
# Problem: 
# Log     : 
#       06/20/2023: 
#================================================================================================================#

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_employment"
data <- paste0(root,"/data")
code <- paste0(root,"/code")


local <- "C:/QCEW Data - Ohio/ES202/extracts" 

# importing packages
pkg_list <- c("tidyverse", "haven", "readr")
purrr::map(pkg_list, function(x) require(x, character.only = TRUE))

#===========================================================================#
# Employment data before spatial join ----
#===========================================================================#

# importing data
start.time <- Sys.time()
master <- haven::read_sas(paste0(local, "/masterfile_2006q1_2021q2.sas7bdat"))
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

# start.time <- Sys.time()
# master2 <- sas7bdat::read.sas7bdat(paste0(local, "/masterfile_2006q1_2021q2.sas7bdat"))
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# print(time.taken)

# identifying unique addresses from master dataset

master %>% colnames()
unique_addresses <- master %>%
                      janitor::clean_names()  %>%
                      mutate(address = trimws(address), city = trimws(city)) %>%
                      distinct(address, city, state, zip) %>%
                      filter(!(is.na(address)) & (trimws(state) == "OH") & !(address %in% c("**ADDRESS NEEDED**", "** ADDRESS NEEDED **", ".", "0",",", "1", "'","NONE", "NO ADDRESS PROVIDED")))

nrow(unique_addresses)



#===========================================================================#
# Employment data before spatial join ----
#===========================================================================#

unique_addresses_sp <- read_csv(paste0(data,"/address_geocoding/address_geocoding_arcgis/unique_addresses_spatial_join.csv"))


