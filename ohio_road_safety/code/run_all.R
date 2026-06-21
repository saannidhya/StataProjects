source(file.path("code", "00_make_dirs.R"))
source(file.path("code", "01_data_inventory.R"))
source(file.path("code", "02_build_fars_local_gov_panel.R"))
source(file.path("code", "03_build_housing_sample.R"))
source(file.path("code", "04_build_employment_panel.R"))
source(file.path("code", "05_build_analysis_panels.R"))
source(file.path("code", "05b_build_renewal_event_panel.R"))

message("Pipeline complete. Run code/06_model_setup.R for the renewal-only lagged baseline.")
