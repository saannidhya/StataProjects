project_root <- normalizePath(
  "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_road_safety",
  winslash = "/",
  mustWork = FALSE
)

stata_root <- normalizePath(
  "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects",
  winslash = "/",
  mustWork = FALSE
)

employment_root <- file.path(stata_root, "ohio_employment")
taxation_root <- file.path(stata_root, "ohio_taxation")

paths <- list(
  project_root = project_root,
  code = file.path(project_root, "code"),
  docs = file.path(project_root, "docs"),
  data = file.path(project_root, "data"),
  data_raw = file.path(project_root, "data", "raw"),
  data_derived = file.path(project_root, "data", "derived"),
  data_temp = file.path(project_root, "data", "temp"),
  output = file.path(project_root, "output"),
  output_figures = file.path(project_root, "output", "figures"),
  output_tables = file.path(project_root, "output", "tables"),
  output_logs = file.path(project_root, "output", "logs"),
  data_catalog = file.path(project_root, "data", "external_data_map.csv"),
  inputs = list(
    employment_masterfile_geocoded = file.path(
      employment_root, "data", "masterfile_2006q1_2025q3_geocoded.csv"
    ),
    employment_panel_legacy = file.path(
      taxation_root, "data", "employment", "emp_df_agg_fips_yr.dta"
    ),
    firm_panel_legacy = file.path(
      taxation_root, "data", "employment", "df_firm_vars_fips_yr.sas7bdat"
    ),
    housesales = file.path(
      taxation_root, "data", "housing", "CoreLogic", "housesales_9524_slim.dta"
    ),
    corelogic_property_geocoded = file.path(
      taxation_root, "data", "housing", "CoreLogic",
      "corelogic_property_geocoded_with_cousub_place_oh.csv"
    ),
    fars_points = file.path(
      taxation_root, "data", "fars", "oh_accident_2001-2021.csv"
    ),
    fars_arcgis_join = file.path(
      taxation_root, "data", "fars", "fars_arcgis", "oh_accident_2001_ExportTable.csv"
    ),
    roads_and_census = file.path(
      taxation_root, "data", "roads_and_census.dta"
    ),
    cosub_place_panel = file.path(
      taxation_root, "data", "cosub_place_panel_9023.dta"
    )
  ),
  outputs = list(
    inventory = file.path(project_root, "data", "derived", "input_file_inventory.csv"),
    annual_controls = file.path(project_root, "data", "derived", "cosub_place_panel_fips_id.csv"),
    roads_panel = file.path(project_root, "data", "derived", "roads_and_census_fips_id.csv"),
    fars_points_local = file.path(project_root, "data", "derived", "fars_points_local_gov_2001_2021.csv.gz"),
    fars_panel = file.path(project_root, "data", "derived", "fars_fips_year_2001_2021.csv"),
    housing_tx = file.path(project_root, "data", "derived", "housing_tx_2006_2021.csv.gz"),
    housing_panel = file.path(project_root, "data", "derived", "housing_fips_year_2006_2021.csv"),
    employment_panel = file.path(project_root, "data", "derived", "employment_fips_year_2006_2021.csv"),
    employment_panel_naics2 = file.path(project_root, "data", "derived", "employment_fips_year_naics2_2006_2021.csv"),
    local_gov_year_panel = file.path(project_root, "data", "derived", "local_gov_year_panel_2006_2021.csv"),
    housing_tx_analysis = file.path(project_root, "data", "derived", "housing_tx_analysis_2006_2021.csv.gz"),
    renewal_event_panel = file.path(project_root, "data", "derived", "renewal_event_panel_2006_2021.csv")
  )
)

project_dirs <- c(
  paths$project_root,
  paths$code,
  paths$docs,
  paths$data,
  paths$data_raw,
  paths$data_derived,
  paths$data_temp,
  paths$output,
  paths$output_figures,
  paths$output_tables,
  paths$output_logs
)

ensure_project_dirs <- function() {
  invisible(
    lapply(
      project_dirs,
      dir.create,
      recursive = TRUE,
      showWarnings = FALSE
    )
  )
}

as_fips_chr <- function(x) {
  out <- trimws(as.character(x))
  suppressWarnings({
    is_num <- !is.na(as.numeric(out))
  })
  out[is_num] <- format(as.numeric(out[is_num]), scientific = FALSE, trim = TRUE)
  out <- sub("\\.0+$", "", out)
  out[out %in% c("", "NA", "<NA>", "nan", "NaN")] <- NA_character_
  out
}

input_catalog <- function() {
  readr::read_csv(paths$data_catalog, show_col_types = FALSE)
}

configure_sf_runtime <- function() {
  sf_root <- system.file(package = "sf")
  sf_proj <- file.path(sf_root, "proj")
  sf_gdal <- file.path(sf_root, "gdal")

  if (nzchar(sf_root) && dir.exists(sf_proj) && dir.exists(sf_gdal)) {
    Sys.setenv(
      PROJ_LIB = sf_proj,
      PROJ_DATA = sf_proj,
      GDAL_DATA = sf_gdal
    )
  }
}

normalize_subdivisiontype <- function(x) {
  tolower(trimws(as.character(x)))
}

fips_id_from_tendigit <- function(tendigit_fips, subdivisiontype) {
  fips_chr <- as_fips_chr(tendigit_fips)
  subtype <- normalize_subdivisiontype(subdivisiontype)

  dplyr::case_when(
    subtype %in% c("city", "village") & !is.na(fips_chr) ~ paste0(
      substr(fips_chr, 1, 2),
      substr(fips_chr, nchar(fips_chr) - 4, nchar(fips_chr))
    ),
    subtype == "township" & !is.na(fips_chr) ~ fips_chr,
    TRUE ~ NA_character_
  )
}
