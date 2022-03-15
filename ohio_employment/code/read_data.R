
library(haven)

root <- "C:/QCEW Data - Ohio/ES202"


df <- haven::read_dta(file = "C:/QCEW Data - Ohio/ES202/MasterFile_2006Q1_2020Q4.dta", n_max = 50000)

df2 <- df %>%
        select(c("Year":"Wage")) %>%
          filter(Year == 2006)

View(df)

unique(df$Zip)
