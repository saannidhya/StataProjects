

# specify the set up location
root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
data <- paste0(root,"/data")
code <- paste0(root,"/code")
tables <- paste0(data,"/outputs/tables")
plots <- paste0(data,"/outputs/plots")

source(paste0(code,"/utility_functions.R"))

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

# pt_17 <- pivot_wider(gdp_vs_road_quality_world_bank %>% select(c(`Economy ISO3`, `Economy Name`, Indicator, "2017")), 
#             names_from = `Indicator`, values_from = "2017") %>% 
#       rename(road_quality = `GCI 4.0: Road quality index (0-100, best)`, gdp =  `GDP (current US$)`,
#              gdp_per_cap = `GDP per capita (current US$)`)
# pt_18 <- pivot_wider(gdp_vs_road_quality_world_bank %>% select(c(`Economy ISO3`, `Economy Name`, Indicator, `2018`)), 
#                      names_from = `Indicator`, values_from = `2018`) %>% 
#   rename(road_quality = `GCI 4.0: Road quality index (0-100, best)`, gdp =  `GDP (current US$)`,
#          gdp_per_cap = `GDP per capita (current US$)`)


# ggplot(data = pt_17, aes(x=road_quality,y=log(gdp_per_cap)))+
#   geom_point()+
#   geom_smooth(method="lm")+
#   theme_minimal()+
#   labs(title="GDP per capita vs Road Quality",
#        x="Road Quality",
#        y="GDP per capita")+
#   theme(legend.position = "none")

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

# ggplot(data = pt$`2018`, aes(x = road_quality, y = gdp_per_cap)) +
#   geom_point(color = "#2E86C1", size = 3, alpha = 0.7) +  # Adding color, size, and transparency to points
#   geom_smooth(method = "lm", color = "#E74C3C", linetype = "dashed", size = 1.2) +  # Changing line color, type, and size
#   theme_minimal(base_size = 15) +  # Adjusting base font size
#   labs(
#     title = "GDP per Capita vs Road Quality: 2018",
#     x = "Road Quality",
#     y = "Log of GDP per Capita",
#     caption = "Source: World Bank Global Competitive Index 4.0 "
#   ) +
#   theme(
#     plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "#34495E"),  # Centering, bolding, and coloring the title
#     axis.title = element_text(face = "italic", size = 15, color = "#34495E"),  # Styling axis titles
#     axis.text = element_text(size = 12, color = "#2C3E50"),  # Styling axis text
#     panel.grid.major = element_line(color = "#D5D8DC", size = 0.8),  # Customizing grid lines
#     panel.grid.minor = element_blank(),  # Removing minor grid lines
#     plot.background = element_rect(fill = "#F5F5F5"),  # Changing plot background color
#     legend.position = "none"
#   ) + ylim(-100, 100000)


# ggsave(paste0(plots,"/gdp_vs_road_quality_world_bank.png"),width=10,height=7)


# 2008 Waynesville voted for a cut, saw a decline in general fund exp of 35%
roads_and_census %>% filter(tendigit_fips == 3903580990) %>% relocate(treated , .after = votesagainst)

# Amanda, cut took place in 2005
roads_and_census %>% filter(tendigit_fips == 3904501630) %>% relocate(treated , .after = votesagainst)

# Beavercreek
roads_and_census %>% filter(tendigit_fips == 3905704720) %>% relocate(treated , .after = votesagainst)

# export to csv
roads_and_census %>% select(tendigit_fips, year, pop) %>% arrange(tendigit_fips, desc(year)) %>%
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

compare_covariates(roads_and_census, "tendigit_fips", c("pop", "medfamy", "childpov"), 3900702064, 3900752066)
