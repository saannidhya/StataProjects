suppressPackageStartupMessages({
  library(haven)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(sf)
  library(tigris)
})

source(file.path("code", "config.R"))

ensure_project_dirs()
configure_sf_runtime()
options(tigris_use_cache = TRUE)

fars_raw <- read_csv(
  paths$inputs$fars_points,
  show_col_types = FALSE
) %>%
  transmute(
    year = as.integer(year),
    fatals = as.numeric(fatals),
    persons = as.numeric(persons),
    ve_forms = as.numeric(ve_forms),
    latitude = as.numeric(latitude),
    longitud = as.numeric(longitud),
    state = as.integer(state),
    st_case = as.character(st_case)
  ) %>%
  filter(
    state == 39,
    year >= 2001,
    year <= 2021,
    !is.na(latitude),
    !is.na(longitud)
  )

fars_sf <- st_as_sf(
  fars_raw,
  coords = c("longitud", "latitude"),
  crs = 4326,
  remove = FALSE
)

oh_cousub <- county_subdivisions(
  state = "OH",
  year = 2023,
  class = "sf",
  cb = TRUE
) %>%
  st_transform(4326) %>%
  select(
    cousub_fips = GEOID,
    cousub_name = NAME,
    cousub_namelsad = NAMELSAD,
    cousub_lsad = LSAD
  )

oh_places <- places(
  state = "OH",
  year = 2023,
  class = "sf",
  cb = TRUE
) %>%
  st_transform(4326) %>%
  select(
    place_fips = GEOID,
    place_name = NAME,
    place_namelsad = NAMELSAD,
    place_lsad = LSAD
  )

# Rebuild the harmonized geography directly from crash points so split cities
# are handled the same way as in the housing and employment pipelines.
fars_local <- fars_sf %>%
  st_join(oh_cousub, left = TRUE, join = st_within) %>%
  st_join(oh_places, left = TRUE, join = st_within) %>%
  mutate(
    fips_id = case_when(
      !is.na(place_fips) & place_lsad %in% c("25", "47") ~ place_fips,
      !is.na(cousub_fips) & cousub_lsad == "44" ~ cousub_fips,
      TRUE ~ NA_character_
    )
  ) %>%
  st_drop_geometry()

fars_panel <- fars_local %>%
  filter(!is.na(fips_id)) %>%
  group_by(fips_id, year) %>%
  summarise(
    fatal_crashes = n(),
    fatalities = sum(fatals, na.rm = TRUE),
    persons_involved = sum(persons, na.rm = TRUE),
    vehicles_involved = sum(ve_forms, na.rm = TRUE),
    .groups = "drop"
  )

annual_controls_raw <- read_dta(paths$inputs$cosub_place_panel)
fips_name <- if ("FIPS_ID" %in% names(annual_controls_raw)) "FIPS_ID" else "Census_FIPS"

panel_grid <- annual_controls_raw %>%
  transmute(
    fips_id = as_fips_chr(.data[[fips_name]]),
    year = as.integer(year)
  ) %>%
  distinct() %>%
  filter(year >= 2001, year <= 2021)

fars_panel_full <- panel_grid %>%
  left_join(fars_panel, by = c("fips_id", "year")) %>%
  mutate(
    fips_id = as_fips_chr(fips_id),
    across(
      c(fatal_crashes, fatalities, persons_involved, vehicles_involved),
      ~ coalesce(.x, 0)
    ),
    any_fatal_crash = as.integer(fatal_crashes > 0)
  )

write_csv(fars_local, paths$outputs$fars_points_local)
write_csv(fars_panel_full, paths$outputs$fars_panel)

message("Wrote local crash points to: ", paths$outputs$fars_points_local)
message("Wrote annual crash panel to: ", paths$outputs$fars_panel)
