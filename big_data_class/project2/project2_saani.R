

##########################################################################################
# RDD project replication
# by Saani Rawat
# date: 6th July 2022
##########################################################################################


library(rdrobust)
library(tidyverse)

grade5 <- haven::read_dta("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/big_data_class/project2/grade5.dta")

narrow <- grade5 %>% filter((school_enrollment <= 60) & (school_enrollment >= 20))

# binned scatterplot
rdplot(narrow$avgmath, narrow$school_enrollment, c= 40.5, p = 1, nbins = 20) # linear
rdplot(narrow$avgmath, narrow$school_enrollment, c= 40.5, p = 2, nbins = 20) # quadratic


# density plot
schools <- narrow %>%
            group_by(schlcode) %>%
              summarise(school_enrollment = mean(school_enrollment, na.rm = TRUE))
ggplot(schools, aes(school_enrollment)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept=40.5,  color = "red") 

# importing BM standard errors code
source("C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/big_data_class/project2/BM_StandardErrors.R")

# 
narrow <- grade5 %>% 
            filter((school_enrollment <= 80)) %>%
            mutate(above40 = if_else(school_enrollment > 40, 1, 0),
                   x = school_enrollment - 40,
                   x_above = above40*x) 

# running RD regression
mod1 <- lm(avgmath ~ above40 + x + x_above, data = narrow)
mod2 <- lm(avgverb ~ above40 + x + x_above, data = narrow)

mod1 <- lm(avgmath ~ above40 + x + x_above, data = narrow %>% filter(between(x,-5,5)) )

model1 <- rdrobust::rdrobust(y = narrow$avgmath, x = narrow$x, c = 0, all = TRUE, kernel = "uni", h = c(5, 5))

summary(model1)

summary(mod1)

summary(mod2)

              


# lm() and rdrobust() giving exactly the same coefficient
# narrow2 <- dfs_agg$housing_roads_census_t_plus_9_matches %>% 
#   mutate(above50 = if_else(votes_pct_for >= cutoff, 1, 0),
#          x = votes_pct_for - cutoff,
#          x_above = above50*x) 
# mod1 <- lm(median_sale_amount ~ above50 + x + x_above, data = filter(narrow2, abs(x) <= 10))
# summary(mod1)
# reg1 <- rdrobust::rdrobust(y = narrow2$median_sale_amount, x = narrow2$x, c = 0, all = TRUE, kernel = "uni", h = 10, vce="hc0")
# summary(reg1)  