#================================================================================================================#
# Purpose : Run RD based on urban vs rural distinction
# Name    : Saani Rawat
# Created : 06/13/2023
# Problem: Then, I will have a large number of obs above cutoff and very few obs below cutoff
# Log     : 
#       06/13/2023: 
#================================================================================================================#


# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")

# running data setup code
source(paste0(code,"/housing_data_setup.R"))

# importing the urban vs rural file. Problem: not 