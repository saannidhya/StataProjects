suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
})

source(file.path("code", "config.R"))

ensure_project_dirs()

catalog <- input_catalog() %>%
  mutate(
    exists = file.exists(file_path),
    size_bytes = if_else(exists, as.numeric(file.info(file_path)$size), NA_real_),
    modified_time = if_else(
      exists,
      as.character(file.info(file_path)$mtime),
      NA_character_
    )
  )

write_csv(catalog, paths$outputs$inventory)

missing_files <- catalog %>% filter(!exists)

message("Wrote inventory to: ", paths$outputs$inventory)
message("Files found: ", sum(catalog$exists), " / ", nrow(catalog))

if (nrow(missing_files) > 0) {
  warning("Missing files detected:\n", paste0(" - ", missing_files$file_path, collapse = "\n"))
}
