#================================================================================================================#
# Purpose : Support for paper and presentations. Miscellaneous support work goes here based on Qs from audience.
# Name    : Saani Rawat
# Created : 11/8/2024
# Log     : 1. 1/12/2025: made a more formal update to code. Added Qs that each snippet answers.
#           2. 1/16/2025: results from test run of fine-tuned gpt-4 model by Vikram
#           3. 2/26/2025: Added regression analysis for Road quality
#================================================================================================================#

library(fixest)
library(MASS) 

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

source(paste0(code,"/utility_functions.R"))


#==========================================================================================================#
# Q. Is there an overall importance of roads? Why does having good roads matter for a country/area/state?
# GDP Per Capita vs Road Quality
# Q. How is road quality index developed by World Bank?
#==========================================================================================================#

# import excel file
gdp_vs_road_quality_world_bank <- readxl::read_excel(paste0(data,"/gdp_vs_road_quality_world_bank.xlsx")) 

# plotting the data using ggplot

yr_list <- c("2017", "2018", "2019", "2020")

pt <- purrr::map(yr_list, ~ pivot_wider(gdp_vs_road_quality_world_bank %>% select(c(`Economy ISO3`, `Economy Name`, Indicator, .x )), 
                                  names_from = `Indicator`, values_from = .x ) %>% 
             rename(road_quality = `GCI 4.0: Road quality index (0-100, best)`, gdp =  `GDP (current US$)`,
                    gdp_per_cap = `GDP per capita (current US$)`) 
           )
names(pt) <- yr_list

# Fit a linear model
model <- lm(log(gdp_per_cap) ~ road_quality, data = pt$`2018`)
slope <- coef(model)[2]  # Extract the slope coefficient

ggplot(data = pt$`2018`, aes(x = road_quality, y = log(gdp_per_cap))) +
  geom_point(color = "#2E86C1", size = 3, alpha = 0.7) +  # Adding color, size, and transparency to points
  geom_smooth(method = "lm", color = "#E74C3C", linetype = "dashed", size = 1.2) +  # Changing line color, type, and size
  theme_minimal(base_size = 15) +  # Adjusting base font size
  labs(
    title = "GDP per Capita vs Road Quality: 2018",
    x = "Road Quality",
    y = "Log of GDP per Capita",
    caption = "Source: World Bank Global Competitive Index 4.0 "
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "#34495E"),  # Centering, bolding, and coloring the title
    axis.title = element_text(face = "italic", size = 15, color = "#34495E"),  # Styling axis titles
    axis.text = element_text(size = 12, color = "#2C3E50"),  # Styling axis text
    panel.grid.major = element_line(color = "#D5D8DC", size = 0.8),  # Customizing grid lines
    panel.grid.minor = element_blank(),  # Removing minor grid lines
    # plot.background = element_rect(fill = "#F5F5F5"),  # Changing plot background color
    legend.position = "none"
  ) 
  # ggsave(paste0(plots,"/gdp_vs_road_quality.png"))


#==========================================================================================================#
# Observing individual cities, villages and townships
# A tale of two tonwships: Andover and Morgan (within same county)
#==========================================================================================================#

# 2008 Waynesville voted for a cut, saw a decline in general fund exp of 35%
roads_and_census %>% filter(tendigit_fips == 3903580990) %>% relocate(treated , .after = votesagainst)

# Amanda, cut took place in 2005
roads_and_census %>% filter(tendigit_fips == 3904501630) %>% relocate(treated , .after = votesagainst)

# Beavercreek
roads_and_census %>% filter(tendigit_fips == 3905704720) %>% relocate(treated , .after = votesagainst)

# export to csv
roads_and_census %>% select(tendigit_fips, year, pop) %>% arrange(desc(pop),tendigit_fips, year) %>%
  write_csv(paste0(data,"/pop_list.csv"))

rdt <- roads_and_census %>% filter(tendigit_fips %in% c(3900902750, 3903573264, 3904129694, 3904580206, 3905704220, 3905704720, 3905704724, 3905725914, 3906116616, 3906131752, 3908549056, 3908559430, 3908585484, 3909303352, 3909356966, 3909903198, 3909907468, 3910380304, 3910962848, 3911333922, 3911336610, 3911377504, 3911381494, 3915112000, 3915138094, 3915141314, 3915162078, 3915162988))


roads_and_census %>% filter(tendigit_fips %in% c(3908559430)) %>% relocate(treated , .after = votesagainst)
                                                 
rdt %>% 
  mutate(renewals = if_else(treated == 0, 1, 0),
         cuts = if_else(treated == 1, 1, 0)) %>%
  group_by(tendigit_fips, treated) %>%
  summarise(
    renewals = sum(renewals),
    cuts = sum(cuts)
  ) %>% 
  filter(treated == 0) %>%
  arrange(desc(renewals))


cuts <- rdt %>% 
  mutate(renewals = if_else(treated == 0, 1, 0),
         cuts = if_else(treated == 1, 1, 0)) %>%
  group_by(tendigit_fips, treated) %>%
  summarise(
    renewals = sum(renewals),
    cuts = sum(cuts)
  ) %>% 
  filter(treated == 1) %>%
  arrange(desc(cuts))
cuts


# Andover vs Morgan townships

# Andover
roads_and_census %>% filter(tendigit_fips == 3900702064) %>% relocate(treated , .after = votesagainst) %>% 
  select(pop, medfamy, childpov) %>%
  summarize(mean = mean(pop), sd = sd(pop))

# morgan
roads_and_census %>% filter(tendigit_fips == 3900752066) %>% relocate(treated , .after = votesagainst) 


compare_covariates <- function(data, city_col, covariates, city1, city2) {
  # Set options to avoid scientific notation
  options(scipen = 999)  
  # Filter data for the two specified cities
  data_city1 <- subset(data, data[[city_col]] == city1)
  data_city2 <- subset(data, data[[city_col]] == city2)
  
  # Initialize a results data frame to store comparisons
  results <- data.frame(
    Covariate = covariates,
    City1_Mean = sapply(covariates, function(cov) mean(data_city1[[cov]], na.rm = TRUE)),
    City1_SD = sapply(covariates, function(cov) sd(data_city1[[cov]], na.rm = TRUE)),
    City2_Mean = sapply(covariates, function(cov) mean(data_city2[[cov]], na.rm = TRUE)),
    City2_SD = sapply(covariates, function(cov) sd(data_city2[[cov]], na.rm = TRUE))
  )
  
  return(results)
}

# covariates with p-val < 0.05
compare_covariates(roads_and_census, "tendigit_fips", c("pop", "medfamy", "childpov"), 3900702064, 3900752066)


# 3900702064 = Andover, 3900752066 = Morgan
compare_covariates(roads_and_census, "tendigit_fips", covs_list, 3900702064, 3900752066) %>% filter(p_value < 0.05)
#> Compared to Morgan, Andover has 
#> 1. more people
#> 2. more poverty
#> 3. less median family income
#> 4. more renters than owners
#> 5. older population
#> 6. less people married
#> 7. less educated population
#> The two townships are not identical. AND, their referendum results are different.

# The two

roads_and_census %>% filter(between(votes_pct_against, cutoff - tes_bw, cutoff + tes_bw))

# comparing house price now
compare_covariates(housing_dfs$housing_roads_census_t_plus_0_matches, "TENDIGIT_FIPS", c("SALE_AMOUNT"), 3900702064, 3900752066)

housing_dfs$housing_roads_census_t_plus_0_matches %>% filter(tendi)

colnames(housing_dfs$housing_roads_census_t_plus_0_matches)

#==========================================================================================================#
# What areas have the closest votes that failed? i.e. they could've been in control but got treated
# Identifying county subdivisions with the closest votes that failed
#==========================================================================================================#


tes_gs_bw <- mean(map_dbl(gs[4:length(gs)], ~ .x$bws[1,1]))

# importing subdivision and county name
cty_sub_names <- readxl::read_excel(paste0(data,"/ohio-only-all-geocodes-2016.xlsx")) %>% janitor::clean_names() %>% 
                    select(all_of(c("tendigit_fips", "name_note_if_split_between_two_counties", "county_name", "split_flag"))) %>% 
                    rename(subdivision = name_note_if_split_between_two_counties, county = county_name) %>%
                    mutate(subdivision = if_else(split_flag == 1,
                               trimws(str_replace(subdivision, "(village|city).*", "\\1")),
                               subdivision)) 


# Find the county subdivisions with the closest votes that failed
closest_votes <- roads_and_census %>%
                      filter(between(votes_pct_against, cutoff - tes_gs_bw, cutoff + tes_gs_bw)) %>%
                      arrange(votes_pct_against) %>% 
                      group_by(tendigit_fips) %>%
                      summarize(num_votes = n(), min_year = min(year), max_year = max(year), mean_vote_result = mean(votes_pct_against), num_failed = sum(treated), num_passed = num_votes - num_failed, max_pop = max(pop)) %>% 
                      arrange(desc(num_votes)) %>%
                      left_join(cty_sub_names, by = "tendigit_fips")

closest_votes %>% arrange(desc(num_failed)) %>%
  readr::write_csv(paste0(data,"/outputs/tables/rd_fips_close_elections_gs_bw.csv"))

cty_sub_names <- readxl::read_excel(paste0(data,"/ohio-only-all-geocodes-2016.xlsx")) %>% janitor::clean_names() %>% 
  select(all_of(c("tendigit_fips", "name_note_if_split_between_two_counties", "county_name", "split_flag"))) %>% 
  rename(subdivision = name_note_if_split_between_two_counties, county = county_name) %>%
  mutate(subdivision = if_else(split_flag == 1,
                               trimws(str_replace(subdivision, "(village|city).*", "\\1")),
                               subdivision)) 

roads_and_census %>% 
  left_join(cty_sub_names, by = "tendigit_fips") %>% relocate(c(subdivision, county) , .after = tendigit_fips) %>% filter(pop > 10000) %>%
  group_by(tendigit_fips, subdivision, county) %>% 
  summarize(num_elections = n(), min_year = min(year), max_year = max(year), pop = mean(pop)) %>% 
  arrange(desc(pop)) %>% View()


  # filter(tolower(county) == "ashtabula" & year == 2012) %>% View()

mm <- purrr::map(housing_dfs, ~ .x %>% filter(year > 1991 & !is.na(SALE_AMOUNT)) %>% 
            summarize(mean = mean(SALE_AMOUNT), median = median(SALE_AMOUNT), sd = sd(SALE_AMOUNT)))

mean(purrr::map_dbl(mm[4:14], ~ .x$mean))

housing_dfs$housing_roads_census_t_plus_0_matches %>% 
  filter(year > 1991 & !is.na(SALE_AMOUNT)) %>% 
  summarize(mean = mean(SALE_AMOUNT), median = median(SALE_AMOUNT), sd = sd(SALE_AMOUNT))

# -16441/170000

# Exporting areas with "close elections" starting 2010. output it as .Rdata.
closest_votes %>% filter(max_year >= 2010) %>% pull(tendigit_fips) %>% unique %>% as.character() %>%
  writeLines(., paste0(data,"/roads/tendigit_fips_close_elections_gs_bw.txt"))


#==========================================================================================================#
# TIGERS shapefile identifying roads
# 
#==========================================================================================================#

oh_cosub <- sf::read_sf(paste0(data,"/roads/TIGERS/tl_2010_39_cousub00/tl_2010_39_cousub00.shp"))
oh_prisec <- sf::read_sf(paste0(data,"/roads/TIGERS/tl_2010_39_prisecroads/tl_2010_39_prisecroads.shp"))

close_fips <- readLines(paste0(data,"/roads/tendigit_fips_close_elections_gs_bw.txt"))

oh_cosub_sub <- oh_cosub %>%
  select(COSBIDFP00, NAME00, NAMELSAD00, UR00 , CLASSFP00, geometry) %>%
  filter(COSBIDFP00 %in% close_fips)

oh_prisec_local <- oh_prisec %>%
  filter(RTTYP == "M") %>%
  select(LINEARID, FULLNAME, RTTYP, MTFCC, geometry)

oh_roads_by_cousub <- st_intersection(oh_cosub_sub, oh_prisec_local)

# plot(oh_roads_by_cousub$geometry, col = "blue", main = "Roads by County Subdivision")

# Export the object as a shapefile
# st_write(oh_roads_by_cousub, paste0(data, "/roads/ohio/oh_roads_by_cousub.gpkg"), delete_dsn = TRUE)

st_write(oh_roads_by_cousub, paste0(data, "/roads/ohio/oh_roads_by_cousub.geojson"), delete_dsn = TRUE)

# oh_prisec %>% filter(RTTYP  == "M") %>% .$MTFCC %>% st_drop_geometry() %>% unique
# 
# plot(oh_prisec["FULLNAME"])
# plot(oh_prisec %>% filter(RTTYP  == "M") %>% .["FULLNAME"])



#==========================================================================================================#
# Regressions with covariates as outcome
#==========================================================================================================#

#==========================================================================================================#
# Hedonic Regression 
#==========================================================================================================#


# Hedonic regression with year F.E 
hm1 <- purrr::map(dfs_agg_covs, ~ lm(data = .x, 
                             median_sale_amount ~ pop + childpov + poverty + pctwithkids + pctsinparhhld + pctlesshs + pcthsgrad + pctsomecoll + pctbachelors + pctgraddeg + unemprate + medfamy + pctown + pctlt5 + pct5to17 + pct18to64 + pct65pls + pctwhite + pctblack + pctamerind + pctapi + pctotherrace + raceherfindahl + pcthisp + pctmarried + pctnevermarr + pctseparated + pctdivorced + lforcepartrate + incherfindahl + factor(year)) )

purrr::map(hm1, summary)

# Hedonic regression with year F.E and county F.E

dfs_agg_covs



#==========================================================================================================#
#  Q. Does your tax cut for road tax levies correlate with cut for any other levies (in the same year), 
#     after controlling for other covariates, year and area F.E?
#==========================================================================================================#

# method 1. Correlate between road tax levy cut and other levy cuts, after controlling for other covariates
#======================================================================#
# Dataset with other tax levies ----
#======================================================================#

# importing excel file
referendums <- readxl::read_excel(paste0(data, "/tax_levies_ohio7.xlsx")) %>% janitor::clean_names()

colnames(referendums)
referendums$purpose2 %>% unique %>% sort

#===================================================#
# Police
#===================================================#

rds_with_police <- rds %>% filter(description == "R") %>%
  inner_join(filter(referendums, tolower(purpose2) == "police"), 
             by = c("year", "tendigit_fips")) %>% 
  mutate(votes_pct_against_police = (votes_against/ (votes_for + votes_against))*100,
         police_treated  = if_else(votes_pct_against_police > cutoff, 1, 0),
         votes_pct_against = 100 - votes_pct_for,
         roads_treated = if_else(votes_pct_against > cutoff, 1, 0) )

lm_police <- lm(police_treated ~ roads_treated + factor(year) + factor(tendigit_fips) + medfamy + poverty + pop + pctrent + pctbachelors, data = rds_with_police) %>% summary 

#===================================================#
# Fire
#===================================================#

rds_with_fire <- rds %>% filter(description == "R") %>%
  inner_join(filter(referendums, tolower(purpose2) == "fire"), 
             by = c("year", "tendigit_fips")) %>% 
  mutate(votes_pct_against_fire = (votes_against/ (votes_for + votes_against))*100,
         fire_treated  = if_else(votes_pct_against_fire > cutoff, 1, 0),
         votes_pct_against = 100 - votes_pct_for,
         roads_treated = if_else(votes_pct_against > cutoff, 1, 0) )

lm_fire <- lm(fire_treated ~ roads_treated + factor(year) + factor(tendigit_fips) + medfamy + poverty + pop + pctrent + pctbachelors, data = rds_with_fire) %>% summary 

#===================================================#
# Current Expenses
#===================================================#

rds_with_current <- rds %>% filter(description == "R") %>%
  inner_join(filter(referendums, tolower(purpose2) == "current expenses"), 
             by = c("year", "tendigit_fips")) %>% 
  mutate(votes_pct_against_curr_exp = (votes_against/ (votes_for + votes_against))*100,
         current_treated  = if_else(votes_pct_against_curr_exp > cutoff, 1, 0),
         votes_pct_against = 100 - votes_pct_for,
         roads_treated = if_else(votes_pct_against > cutoff, 1, 0) )

lm_current <- lm(current_treated ~ roads_treated + factor(year) + factor(tendigit_fips) + medfamy + poverty + pop + pctrent + pctbachelors, data = rds_with_current)  %>% summary

# cor(rds_with_current$roads_treated, rds_with_current$current_treated)
# table(rds_with_current$roads_treated, rds_with_current$current_treated)

#===================================================#
# Recreation
#===================================================#

rds_with_rec <- rds %>% filter(description == "R") %>%
  inner_join(filter(referendums, tolower(purpose2) == "recreation"), 
             by = c("year", "tendigit_fips")) %>% 
  mutate(votes_pct_against_rec = (votes_against / (votes_for + votes_against))*100,
         rec_treated  = if_else(votes_pct_against_rec > cutoff, 1, 0),
         votes_pct_against = 100 - votes_pct_for,
         roads_treated = if_else(votes_pct_against > cutoff, 1, 0) )

lm_rec <- lm(rec_treated ~ roads_treated + factor(year) + factor(tendigit_fips) + medfamy + poverty + pop + pctrent + pctbachelors, data = rds_with_rec) %>% summary

#===================================================#
# School District (fips info not available,  
#                  so doing at county level)
#===================================================#
rds_cty <- filter(referendums, tolower(purpose2) == "roads" & description == "R") %>% 
  mutate(votes_pct_against_rds = 100 - (votes_for / (votes_for + votes_against))*100,
         roads_treated = if_else(votes_pct_against_rds > cutoff, 1, 0)) %>%
  group_by(county, year) %>%
  summarize(road_cuts = sum(roads_treated)) 
  
schl_cty <- filter(referendums, tolower(purpose2) == "school") %>% 
  mutate(votes_pct_against_schl = 100 - (votes_for / (votes_for + votes_against))*100,
         school_treated = if_else(votes_pct_against_schl > cutoff, 1, 0)) %>%
  group_by(county, year) %>%
  summarize(school_cuts = sum(school_treated)) 

rds_with_school <- inner_join(rds_cty, schl_cty, by = c("county", "year")) 

lm_school <- lm(school_cuts ~ road_cuts + factor(year) + factor(county), data = rds_with_school) %>% summary

# Conclusion: Road tax levies ONLY correlate with Current Expense tax levies.

# coefficients
lm_police$coefficients["roads_treated", "Estimate"]
lm_fire$coefficients["roads_treated", "Estimate"]
lm_current$coefficients["roads_treated", "Estimate"]
lm_rec$coefficients["roads_treated", "Estimate"]
lm_school$coefficients["road_cuts", "Estimate"]

# standard errors
lm_police$coefficients["roads_treated", "Std. Error"]
lm_fire$coefficients["roads_treated", "Std. Error"]
lm_current$coefficients["roads_treated", "Std. Error"]
lm_rec$coefficients["roads_treated", "Std. Error"]
lm_school$coefficients["road_cuts", "Std. Error"]

# method 2. Run a regression with road tax levy cut as outcome and other levy cuts as covariates


#==========================================================================================================#
#  Q. Does road quality actually change after the tax cuts?
#     Preliminary results from test run.
#==========================================================================================================#

# before referendum	after referendum
# passed the levy	1.07	1
# failed the levy	1.39	0.93

# Create the data
data <- data.frame(
  Time_of_Vote = rep(c("Before Referendum", "After Referendum"), each = 2),
  Levy_Status = rep(c("Passed the Levy", "Failed the Levy"), 2),
  Avg_Road_Quality_Rating = c(1.00, 1.375, 1.066667, 0.9310345)
)

# Set the correct order for the Time_of_Vote variable
data$Time_of_Vote <- factor(data$Time_of_Vote, levels = c("Before Referendum", "After Referendum"))

# Plot the data
ggplot(data, aes(x = Time_of_Vote, y = Avg_Road_Quality_Rating, group = Levy_Status, color = Levy_Status)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Road Quality Ratings Before and After Referendum",
    x = "Time of the Vote",
    y = "Avg Road Quality Rating",
    color = "Levy Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )


data_change <- data.frame(
  Levy_Status = c("Passed the Levy", "Failed the Levy"),
  Change_in_Rating = c(1.066667 - 1, 0.9310345 - 1.375)
)

ggplot(data_change, aes(x = Levy_Status, y = Change_in_Rating, fill = Levy_Status)) +
  geom_bar(stat = "identity", width = 0.5) +  # Reduce bar width
  labs(
    title = "Change in Avg Road Quality Rating (Before and After Referendum)",
    x = "Levy Status",
    y = "Change in Avg Road Quality Rating",
    fill = "Levy Status"
  ) +
  scale_fill_manual(values = c("Passed the Levy" = "lavender", "Failed the Levy" = "salmon")) + # Custom colors
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "none"  # Remove legend
  )

# Regression analysis #
above_roads <- readr::read_csv(paste0(data,"/roads/ohio/above/above_predictions_with_flag.csv")) %>% 
  mutate(treat_flag = 1, # Above means these areas are above the cutoff for % votes against i.e. cut their renewal taxes 
         road_quality_score = round(((predicted_label + confidence) / 3) * 99 + 1, 1),
         road_quality_score2 = case_when(
           predicted_label == 0 ~ 1 + (1 - 0.5*confidence) * 99 / 3 ,
           TRUE                 ~ 1 + predicted_label * 99 / 3  + 0.5*confidence * 99 / 3 
         ) |> round(1)
         )  %>%
  mutate(year = stringr::str_extract(image, "(\\d{4}|\\d{2})(?=\\.jpg)"),
         year = ifelse(nchar(year) == 2, paste0("20", year), year),
         year = as.integer(year))

below_roads <- readr::read_csv(paste0(data,"/roads/ohio/below/below_predictions_with_flag.csv")) %>% 
  mutate(treat_flag = 0,
         road_quality_score = round(((predicted_label + confidence) / 3) * 99 + 1, 1),
         road_quality_score2 = case_when(
           predicted_label == 0 ~ 1 + (1 - 0.5*confidence) * 99 / 3 ,
           TRUE                 ~ 1 + predicted_label * 99 / 3  + 0.5*confidence * 99 / 3 
         ) |> round(1)
         ) %>%
  mutate(year = stringr::str_extract(image, "(\\d{4}|\\d{2})(?=\\.jpg)"),
         year = ifelse(nchar(year) == 2, paste0("20", year), year),
         year = as.integer(year))

roads_close <- bind_rows(above_roads, below_roads) %>% mutate(did = post_election_flag*treat_flag)

# before and after: treatment group
road_above_lm <- lm(data = above_roads, formula = predicted_label ~ post_election_flag)
summary(road_above_lm)
# before and after: control group
road_below_lm <- lm(data = below_roads, formula = predicted_label ~ post_election_flag)
summary(road_below_lm)


lm(data = roads_close %>% filter(post_election_flag == 0), formula = predicted_label ~ treat_flag ) %>% summary
lm(data = roads_close %>% filter(post_election_flag == 1), formula = predicted_label ~ treat_flag ) %>% summary

road_lm <- lm(data = roads_close, formula = road_quality_score2 ~  treat_flag + post_election_flag + treat_flag*post_election_flag)
summary(road_lm)

above_roads[above_roads$post_election_flag == 1, "predicted_label"] %>% pull(predicted_label) %>% mean
below_roads[below_roads$post_election_flag == 1, "predicted_label"] %>% pull(predicted_label) %>% mean

# Diff-in-Diff analysis #
 

mean(roads_close$predicted_label)
mean(above_roads$predicted_label)

# group means 
roads_close %>%
  group_by(treat_flag, post_election_flag) %>%
  summarize(mean = mean(predicted_label), sd = sd(predicted_label) ,
            mean_score = mean(road_quality_score), sd_score = sd(road_quality_score) ,
            mean_score2 = mean(road_quality_score2), sd_score2 = sd(road_quality_score2) 
            # ,n = n()
            )


roads_lm <- lm(data = roads_close, formula = predicted_label ~ post_election_flag + treat_flag + did) 
summary(roads_lm)

# effect of cutting local road tax on road quality
roads_did <- feols(predicted_label ~ did | post_election_flag + treat_flag, data = roads_close, cluster = ~tendigit_fips) 
summary(roads_did)

model2 <- lm(predicted_label ~ treat_flag * post_election_flag, data = roads_close)
# Cluster by township (tendigit_fips)
coeftest(model2, vcov = vcovCL, cluster = ~year)

model3 <- lm(road_quality_score ~ treat_flag * post_election_flag  + factor(County), data = roads_close)
# Cluster by township (tendigit_fips)
coeftest(model3, vcov = vcovCL, cluster = ~tendigit_fips)

feols(road_quality_score ~ did  | post_election_flag + treat_flag, data = roads_close) %>% summary 
feols(road_quality_score2 ~ did  | post_election_flag + treat_flag, data = roads_close) %>% summary 

feols(road_quality_score ~ did  | post_election_flag + treat_flag, data = roads_close, cluster = ~tendigit_fips) %>% summary 
feols(road_quality_score2 ~ did  | post_election_flag + treat_flag, data = roads_close, cluster = ~tendigit_fips) %>% summary 


feols(road_quality_score ~ did  | post_election_flag + treat_flag, se = "jk", data = roads_close, cluster = ~year) %>% summary 

feols(road_quality_score2 ~ did  | post_election_flag + treat_flag, data = roads_close, cluster = ~year) %>% summary 


roads_close$tendigit_fips2 <- as.factor(roads_close$tendigit_fips)

jhk <- feols(road_quality_score ~ did  | post_election_flag + treat_flag, data = roads_close, cluster = ~tendigit_fips) %>%
  boottest("did", B = 999, clustid = ~tendigit_fips, param = "did")

m2  <- feols(road_quality_score2 ~ did | post_election_flag + treat_flag, data = roads_close)

vcov_cr2 <- clubSandwich::vcovCR(m2, cluster = roads_close$tendigit_fips, type = "CR2")
coef_test(m2, vcov = vcov_cr2, test = "Satterthwaite") 


did_mod <- feols(
  predicted_label ~ did | post_election_flag + treat_flag,
  data = roads_close                       # <-- no SEs yet
)
summary(did_mod, vcov = ~ year)
V_cr2 <- clubSandwich::vcovCR(did_mod,
                              cluster = roads_close$tendigit_fips,
                              type    = "CR2")        # small-sample adj. :contentReference[oaicite:3]{index=3}

clubSandwich::coef_test(did_mod, vcov = V_cr2, test = "Satterthwaite") 

V_cr3j <- summclust::vcov_CR3J(did_mod, cluster = "year")  # leverages leave-one-cluster-out :contentReference[oaicite:5]{index=5}
keep <- names(coef(did_mod))          # returns "did"
V_cr3j_trim <- V_cr3j[keep, keep, drop = FALSE]
summary(did_mod, .vcov = V_cr3j_trim)  


did_mod0 <- feols(predicted_label ~ did | post_election_flag + treat_flag,
                  data = roads_close)

boot_res <- fwildclusterboot::boottest(
  did_mod0,
  param    = "did",
  clustid  = "tendigit_fips",   # OR c("tendigit_fips","year") for 2-way
  B        = 9999           # draws
  # bootwild = "rademacher"       # default, fastest :contentReference[oaicite:7]{index=7}
)

boot_res$p_val  

roads_twp_yr <- roads_close |>
  dplyr::group_by(tendigit_fips, year,
                  treat_flag, post_election_flag) |>
  dplyr::summarise(score = mean(road_quality_score2), .groups = "drop") |>
  dplyr::mutate(did = treat_flag * post_election_flag)

agg_mod <- feols(score ~ did | post_election_flag + treat_flag,
                 data = roads_twp_yr)

summary(agg_mod, vcov = ~ tendigit_fips)

summary(did_mod,
        vcov = ~ tendigit_fips,
        ssc  = ssc(cluster.df = "conventional",   # or "min", default
                   adj        = TRUE,            # HC1 vs HC0 factor
                   cluster.adj = TRUE))          # Bell-McCaffrey adj. :contentReference[oaicite:10]{index=10}


## Taking into account trinary outcome ##

# 1. Option 1: Linear Probability Model
# poor
roads_close$poor_quality <- as.integer(roads_close$predicted_label == 0)
did_poor <- feols(poor_quality ~ did | post_election_flag + treat_flag, data = roads_close, cluster = ~tendigit_fips)
summary(did_poor)

# medium
roads_close$medium_quality <- as.integer(roads_close$predicted_label == 1)
did_medium <- feols(medium_quality ~ did | post_election_flag + treat_flag, data = roads_close, cluster = ~County)
summary(did_medium)

# good
roads_close$good_quality <- as.integer(roads_close$predicted_label == 2)
did_good <- feols(good_quality ~ did | post_election_flag + treat_flag, data = roads_close, cluster = ~County)
summary(did_good)

# 2. Option 2: Ordered Logit Model
roads_close$did_interact <- roads_close$post_election_flag * roads_close$treat_flag
roads_ord_did <- polr(factor(predicted_label) ~ did_interact + factor(post_election_flag) + factor(treat_flag), data = roads_close, method = "logistic")
summary(roads_ord_did)

# Using confidence variable

#------------------------------------------#
## RD regression within the bandwidth
#------------------------------------------#

roads_close

roads_and_census %>% filter(tendigit_fips %in% unique(roads_close$tendigit_fips))

yrs_tbl <- roads_close |>                          # 81×13
  distinct(tendigit_fips, year) |>                 # keep only fips–year pairs
  group_by(tendigit_fips) |>
  summarise(img_years = list(year), .groups = "drop")

elec_ok <- roads_and_census |>                     # 279×49
  inner_join(yrs_tbl, by = "tendigit_fips") |>
  rowwise() |>
  mutate(
    has_before = any(img_years <  year),           # at least one image earlier
    has_after  = any(img_years >  year),           # at least one image later
    keep       = has_before & has_after            # both must be TRUE
  ) |>
  ungroup() |>
  filter(keep) |>                                  # ➊ only elections with B&A
  select(-img_years, -has_before, -has_after, -keep) %>%
  arrange(tendigit_fips, year)


rd_df <- roads_close %>% 
  left_join(elec_ok, by = c("tendigit_fips")) %>%
  arrange(tendigit_fips, year.x) 

rd_df_pre <- rd_df %>% filter(post_election_flag == 0)
rd_df_post <- rd_df %>% filter(post_election_flag == 1)



h_left  <- abs(min(rd_df_post$votes_pct_against, na.rm = TRUE) - cutoff)          # distance to the left-most point
h_right <- abs(max(rd_df_post$votes_pct_against, na.rm = TRUE) - cutoff)          # distance to the right-most point

rdrobust(  y = rd_df_pre$predicted_label,,
           x = rd_df_pre$votes_pct_against,
           c = cutoff,
           bwselect = "manual",
           h = c(h_left, h_right) ,
           # covs = y %>%
           #   dplyr::select(x) ,
           all = TRUE, kernel = "tri", bwselect = "mserd", p = 1, q = 2, cluster = rd_df_pre$tendigit_fips) %>% summary

rdrobust(  y = rd_df_post$predicted_label,
           x = rd_df_post$votes_pct_against,
           c = cutoff,
           # bwselect = "manual",
           h = c(h_left, h_right) ,
           # covs = y %>%
           #   dplyr::select(x) ,
           all = TRUE) %>% summary

rd_df_post %>%
  group_by(treat_flag) %>%
  summarize(mean = mean(predicted_label), sd = sd(predicted_label) ,
            mean_score = mean(road_quality_score), sd_score = sd(road_quality_score) ,
            mean_score2 = mean(road_quality_score2), sd_score2 = sd(road_quality_score2) 
            # ,n = n()
  )


#==========================================================================================================#
#  Q. Does one election change the probability of having another election?
#     # Check most common duration
#>    # check how often referendums take place in a city (more than 5 years?)
#>
#==========================================================================================================#

roads_and_census$duration %>% unique %>% sort

# Roughly 90% of the time, 5 years
roads_and_census %>% mutate(duration = as.numeric(duration)) %>% group_by(duration) %>% summarize(prop = n()/nrow(roads_and_census) )
# Most common duration is 5 years

fre <- roads_and_census %>% group_by(tendigit_fips) %>%
  summarize(max_year = max(year), min_year = min(year), count = n()) %>%
  mutate(diff = max_year - min_year, freq = diff/count) %>% 
  filter(freq != 0)
  
fre$freq %>% summary
# Elections happen every 4 years on average. So last year of levy might have a "double" effect.


#==========================================================================================================#
#  Latest failed referendum for "close' elections and cities, townships with population above 10,000
#>    
#==========================================================================================================#


fips_list <- c(3902374119, 3915162988, 3915142168, 3908174608, 3909356966, 3915319036, 3911377504, 3911377504, 3900729624, 3903573264, 3903580990, 3908585484, 3909963968, 3913946578, 3915156294, 3902351912, 3902978890, 3904781718, 3906176028, 3908559430, 3909975126, 3915162078, 3915328448, 3905503590, 3905911003, 3905704720, 3905704724, 3913303086, 3900902750, 3903526446, 3904129694, 3905513988, 3908518196, 3908523618, 3908546494, 3909903198, 3915141314, 3915318658, 3901366628, 3902346788, 3902923730, 3903310030, 3904361714, 3906116616, 3906131752, 3908549056, 3908559416, 3909907468, 3915112000, 3915138094, 3915374130, 3917341328)

# subsetting based on the fips that satisfy the required criteria, taking failed levies only, group by and take max year for each fips

roads_and_census %>% 
  filter(treated == 1 & tendigit_fips %in% fips_list) %>% 
  group_by(tendigit_fips) %>%
  summarize(latest_fail_yr = max(year)) %>% filter(latest_fail_yr >= 2010) -> fail_yrs


fail_yrs

