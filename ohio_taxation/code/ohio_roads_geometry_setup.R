

library(tidyverse)
library(sf)

tigers_loc <- "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/data/roads/TIGERS/"

tigers_oh10 <- read_sf(paste0(tigers_loc, "tl_2010_39_prisecroads/tl_2010_39_prisecroads.shp"))
co_sub10 <- read_sf(paste0(tigers_loc, "tl_2010_39_cousub00/tl_2010_39_cousub00.shp"))

# plot(tigers_oh)

plot(st_geometry(tigers_oh), main = "Roads in Ohio", col = "blue")
# Plot using ggplot2
ggplot(data = tigers_oh) +
  geom_sf(color = "yellow", size = 0.2) +
  theme_minimal() +
  labs(title = "Roads in Ohio")


ggplot(data = co_sub10) +
  geom_sf(color = "black", size = 0.2) +
  theme_minimal() +
  labs(title = "Areas in Ohio")

# Perform the spatial join
tigers_with_subdivisions <- st_join(tigers_oh, co_sub10, join = st_intersects)

plot(st_geometry(tigers_with_subdivisions))

ggplot() +
  geom_sf(data = co_sub10, fill = NA, color = "black", size = 0.5) +  # Subdivision boundaries in black
  geom_sf(data = tigers_with_subdivisions, color = "orange", size = 0.3) +  # Roads in orange
  theme_minimal() +
  labs(title = "Roads and County Subdivisions in Ohio",
       caption = "Subdivision boundaries in black; roads in orange") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot() +
  geom_sf(data = co_sub10, fill = NA, color = "black", size = 0.5) +  # Subdivision boundaries in black
  geom_sf(data = tigers_with_subdivisions %>% filter(FULLNAME == "I- 71"), color = "pink", size = 0.3) +  # Roads in orange
  theme_minimal() +
  labs(title = "Roads and County Subdivisions in Ohio",
       caption = "Subdivision boundaries in black; roads in orange") +
  theme(plot.title = element_text(hjust = 0.5)) 

i71_roads <- tigers_with_subdivisions %>% filter(FULLNAME == "I- 71")

coords_list <- lapply(st_geometry(i71_roads), st_coordinates)

# Print the coordinates for the first segment
coords_list[[2]]

length(coords_list)
