#==========================================================================================================#
# Purpose : Employment: Data Benchmarking    
# Name    : Saani Rawat
# Created : 08/21/2025
# Log     : 1. 
#==========================================================================================================#


library(tidyverse)
library(haven)
library(data.table)

# locations
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")


# Naics map
naics_map <- read_csv("data/employment/naics_2digit_mapping_2022.csv",
                      col_types = cols(
                        naics2 = col_character(),
                        sector_title = col_character()
                      ))


# ohio QCEW published reports: https://ohiolmi.com/Home/QCEW/QCEWpubs
oh_qcew <- read_csv("data/employment/ohio_qcew_2006q1_2020q4.csv") %>% select(-source_url) %>%
  mutate(quarter = as.numeric(substr(quarter, 2, 2))) %>%
  rename(total_wages = total_wages_dollars) %>%
  mutate(avg_wage_per_person = total_wages / average_employment) %>%
  mutate(source = "qcew report")
  



#----------------------------------------------------------------------------------#
# fips_sub + year + quarter level wages and employment
#----------------------------------------------------------------------------------#

emp_df <- haven::read_dta("data/employment/employment_data_cleaned.dta") 

emp_df2 <- emp_df %>%
  mutate(persons = round(persons)) %>%
  relocate(unique_id, .before = year) %>%
  relocate(tendigit_fips, .after = unique_id) %>%
  distinct(unique_id, quarter, year, .keep_all = TRUE) %>% # takes care of duplicates, cuz some exist in original
  arrange(unique_id, year, quarter) 


# group by year, quarter and aggregate
emp_df3 <- emp_df2 %>%
  group_by(year, quarter) %>%
  summarise(average_employment = sum(persons, na.rm = TRUE),
            total_wages = sum(wage, na.rm = TRUE)/1000) %>%
  ungroup() %>%
  mutate(avg_wage_per_person = total_wages / average_employment,
         source = "our data")



# emp_df_by_naics <- emp_df2 %>%
#   mutate(naics_2dg = substr(as.character(naics), 1, 2)) %>%
#   group_by(year, quarter, naics_2dg) %>%
#   summarise(average_employment  = sum(persons, na.rm = TRUE),
#             total_wages = sum(wage, na.rm = TRUE)) %>%
#   ungroup() %>%
  # mutate(avg_wage_per_person = total_wages / average_employment,
  #        source = "our data")


colnames(emp_df2)

View(emp_df3)


#----------------------------------------------------------------------------------#
# Check Data Coverage
#----------------------------------------------------------------------------------#

colnames(oh_qcew)
colnames(emp_df3)

emp_comp <- rbind(emp_df3, oh_qcew) %>%
  mutate(
    date = zoo::as.yearqtr(paste(year, quarter), format = "%Y %q")
  )

View(emp_comp)

emp_coverage <- emp_comp %>%
  select(date, average_employment, source) %>%
  pivot_wider(
    names_from = source,
    values_from = average_employment
  ) %>%
  mutate(
    diff = abs((`our data` - `qcew report`)/`qcew report`)
  ) 

#----------------------------------------------------------------------------------#
# Data Coverage Plots
#----------------------------------------------------------------------------------#

# average employment
ggplot(emp_comp, aes(x = date, y = average_employment / 1e6, color = source, group = source)) +
  geom_line(size = 1) +
  labs(
    title = "Average Employment (in MMs): Our Data vs QCEW Report",
    x = "Quarter",
    y = "Average Employment (in MMs)"
  ) +
  theme_minimal()

# total wages
ggplot(emp_comp, aes(x = date, y = total_wages/1e6, color = source, group = source)) +
  geom_line(size = 1) +
  labs(
    title = "Total Wages (in BNs): Our Data vs QCEW Report",
    x = "Quarter",
    y = "Total Wages (in BNs)"
  ) +
  theme_minimal()

ggplot(emp_comp, aes(x = date, y = avg_wage_per_person, color = source, group = source)) +
  geom_line(size = 1) +
  labs(
    title = "Average Wage per Person: Our Data vs QCEW Report",
    x = "Quarter",
    y = "Average Wage per Person (in USD)"
  ) +
  theme_minimal()
