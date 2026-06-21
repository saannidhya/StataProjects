library(sf)
library(tidyverse)
library(haven)

root <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
shared <- "//cobshares.uccob.uc.edu/economics$/Julia/roads"
out_dir <- file.path(root, "data/outputs")

hs1620 <- read_dta(file.path(shared, "housesales_1620_slim.dta"))
hs2124 <- read_dta(file.path(shared, "housesales_2124_slim.dta"))

library(tidycensus)
library(tigris)

# Set options for tigris
options(tigris_use_cache = TRUE)

#============================================================================================#
#  Broad Map: No subsetting within county_subdivisions or places
#============================================================================================#

# Get Ohio county subdivisions # LSAD codes: 44 = township, 47 = village, 25 = city
ohio_cousubs <- county_subdivisions(state = "OH", cb = TRUE, year = 2021)

# Get Ohio counties boundaries
ohio_counties <- counties(state = "OH", cb = TRUE, year = 2021)


# Get Ohio places # LSAD codes: 25 = city, 47 = village, 57 = CDP (Census Designated Place)
ohio_places <- places(state = "OH", cb = TRUE, year = 2021)

# Calculate the intersection OUTSIDE of the plot
# This allows you to check if it worked before plotting
intersections <- st_intersection(ohio_cousubs, ohio_places)

# Plot the result
p <- ggplot() +
    geom_sf(data = ohio_cousubs, fill = NA, color = "blue", alpha = 0.5) +
    geom_sf(data = ohio_counties, fill = NA, color = "black", size = 1.5) +
    geom_sf(data = ohio_places, fill = NA, color = "green", alpha = 0.5) +
    geom_sf(data = intersections, fill = "red", color = NA, alpha = 0.5) +
        labs(title = "Ohio County Subdivisions and Places",
                 subtitle = "Blue: County Subdivisions, Green: Places, Red: Overlapping Areas") +
    theme_minimal()
ggsave(filename = file.path(out_dir, "plots/temp/ohio_boundaries.png"),plot = p,width = 12,height = 10,dpi = 300)



# convert hs1620 to sf object using lat/lon columns
hs1620_sf <- hs1620 %>%
    filter(!is.na(lat) & !is.na(lon)) %>%
    mutate(
        lat = as.numeric(lat),
        lon = as.numeric(lon)
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4269, remove = FALSE)

# Convert hs2124 to sf object using lat/lon columns
hs2124_sf <- hs2124 %>%
    filter(!is.na(lat) & !is.na(lon)) %>%
    mutate(
        lat = as.numeric(lat),
        lon = as.numeric(lon)
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4269, remove = FALSE)


# Plot with housing transactions
p_with_sales <- ggplot() +
    geom_sf(data = ohio_cousubs, fill = NA, color = "blue", alpha = 0.5) +
    geom_sf(data = ohio_counties, fill = NA, color = "black", size = 1.5) +
    geom_sf(data = ohio_places, fill = NA, color = "green", alpha = 0.5) +
    geom_sf(data = intersections, fill = "red", color = NA, alpha = 0.5) +
    geom_sf(data = hs1620_sf, color = "black", size = 0.5, alpha = 0.3) +
    geom_sf(data = hs2124_sf, color = "black", size = 0.5, alpha = 0.3) +
    labs(title = "Ohio Housing Sales with Geographic Boundaries",
         subtitle = "Blue: County Subdivisions, Green: Places, Red: Overlapping Areas, Black: Housing Transactions") +
    theme_minimal()

ggsave(filename = file.path(out_dir, "plots/temp/ohio_boundaries_with_sales1624.png"),plot = p_with_sales,width = 12,height = 10,dpi = 300)


#============================================================================================#
#  Extracting and coloring
#============================================================================================#


p2 <- ggplot() +
  geom_sf(data = ohio_cousubs %>% filter(LSAD == "44"), fill = NA, color = "blue", alpha = 0.5) +
    geom_sf(data = ohio_counties, fill = NA, color = "black", size = 1.5) +
  geom_sf(data = ohio_cousubs %>% filter(LSAD == "47"), fill = NA, color = "green", alpha = 0.5) +
    geom_sf(data = ohio_cousubs %>% filter(LSAD == "25"), fill = NA, color = "red", alpha = 0.5) +
    labs(title = "Ohio County Subdivisions: Townships, Villages, and Cities",
         subtitle = "Blue: Townships, Green: Villages, Red: Cities") +
  theme_minimal()

ggsave(filename = file.path(out_dir, "plots/temp/ohio_cousubs_by_type.png"), plot = p2, width = 16, height = 14, dpi = 600)

p_with_sales_type <- ggplot() +
    geom_sf(data = ohio_cousubs %>% filter(LSAD == "44"), fill = NA, color = "blue", alpha = 0.5) +
    geom_sf(data = ohio_counties, fill = NA, color = "black", size = 1.5) +
    geom_sf(data = ohio_cousubs %>% filter(LSAD == "47"), fill = NA, color = "green", alpha = 0.5) +
    geom_sf(data = ohio_cousubs %>% filter(LSAD == "25"), fill = NA, color = "red", alpha = 0.5) +
    geom_sf(data = hs1620_sf, color = "black", size = 0.5, alpha = 0.3) +
    geom_sf(data = hs2124_sf, color = "black", size = 0.5, alpha = 0.3) +
    labs(title = "Ohio Housing Sales with County Subdivision Boundaries",
         subtitle = "Blue: Townships, Green: Villages, Red: Cities, Black: Housing Transactions") +
    theme_minimal()

ggsave(filename = file.path(out_dir, "plots/temp/ohio_cosub_boundaries_with_sales1624.png"),plot = p_with_sales_type,width = 12,height = 10,dpi = 300)
