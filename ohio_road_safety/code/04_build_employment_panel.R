suppressPackageStartupMessages({
  library(data.table)
})

source(file.path("code", "config.R"))

ensure_project_dirs()

keep_cols <- c(
  "year", "quarter", "pad", "uin", "rep_unit", "naics",
  "m1", "m2", "m3", "wage", "FIPS_ID"
)

dt <- fread(
  paths$inputs$employment_masterfile_geocoded,
  select = keep_cols,
  showProgress = TRUE
)

dt[, fips_id := as_fips_chr(FIPS_ID)]

dt <- dt[
  year >= 2006 &
    year <= 2021 &
    !is.na(fips_id)
]

dt[, establishment_id := paste(pad, uin, rep_unit, sep = "_")]
dt[, naics2 := substr(as.character(naics), 1, 2)]
dt[, avg_persons_est_qtr := rowMeans(as.matrix(.SD), na.rm = TRUE), .SDcols = c("m1", "m2", "m3")]

quarterly_flow <- dt[
  ,
  .(
    total_wages_qtr = sum(as.numeric(wage), na.rm = TRUE),
    total_persons_qtr = sum(avg_persons_est_qtr, na.rm = TRUE)
  ),
  by = .(fips_id, year, quarter)
]

annual_flow <- quarterly_flow[
  ,
  .(
    total_wages = sum(total_wages_qtr, na.rm = TRUE),
    avg_persons = round(mean(total_persons_qtr, na.rm = TRUE), 0)
  ),
  by = .(fips_id, year)
]

est_panel <- unique(dt[, .(fips_id, year, establishment_id)])
est_panel[, first_year_in_fips := min(year), by = .(fips_id, establishment_id)]
est_panel[, last_year_in_fips := max(year), by = .(fips_id, establishment_id)]

annual_establishments <- est_panel[
  ,
  .(
    establishments = uniqueN(establishment_id),
    entrants = sum(first_year_in_fips == year, na.rm = TRUE),
    exits = sum(last_year_in_fips == year, na.rm = TRUE)
  ),
  by = .(fips_id, year)
]

employment_panel <- merge(
  annual_flow,
  annual_establishments,
  by = c("fips_id", "year"),
  all = TRUE
)

quarterly_naics2 <- dt[
  ,
  .(
    total_wages_qtr = sum(as.numeric(wage), na.rm = TRUE),
    total_persons_qtr = sum(avg_persons_est_qtr, na.rm = TRUE)
  ),
  by = .(fips_id, year, quarter, naics2)
]

naics2_establishments <- unique(dt[, .(fips_id, year, naics2, establishment_id)])[
  ,
  .(establishments = uniqueN(establishment_id)),
  by = .(fips_id, year, naics2)
]

naics2_flow <- quarterly_naics2[
  ,
  .(
    total_wages = sum(total_wages_qtr, na.rm = TRUE),
    avg_persons = round(mean(total_persons_qtr, na.rm = TRUE), 0)
  ),
  by = .(fips_id, year, naics2)
]

naics2_flow <- merge(
  naics2_flow,
  naics2_establishments,
  by = c("fips_id", "year", "naics2"),
  all = TRUE
)

fwrite(employment_panel, paths$outputs$employment_panel)
fwrite(naics2_flow, paths$outputs$employment_panel_naics2)

message("Wrote annual employment panel to: ", paths$outputs$employment_panel)
message("Wrote annual NAICS-2 employment panel to: ", paths$outputs$employment_panel_naics2)
