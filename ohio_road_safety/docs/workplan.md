# Workplan

## Phase 1: data engineering

1. Verify all external input files with `code/01_data_inventory.R`.
2. Rebuild the FARS spatial join on the harmonized `FIPS_ID` geography.
3. Build a clean `FIPS_ID x year` crash panel for 2001-2021.
4. Build a 2006-2021 housing transaction sample and a local-government-year housing aggregation.
5. Build a 2006-2021 employment and establishment panel from the geocoded ODJFS masterfile.
6. Merge roads, crashes, housing, employment, and annual controls into `local_gov_year_panel_2006_2021.csv`.
7. Build `renewal_event_panel_2006_2021.csv` with one row per road-renewal election and event-time outcomes from `t-3` through `t+6`.

## Phase 2: renewal sample discipline

1. Restrict the baseline election sample to `purpose2 == "roads"` and `description == "R"`.
2. Keep the closest renewal to the cutoff when multiple renewals occur in the same `FIPS_ID x year`.
3. Flag perpetual or nonstandard durations such as `duration == "1000"`.
4. Compute spacing to previous and next renewals to identify isolated events.
5. Define the preferred baseline as close finite-duration renewals.

## Phase 3: timing and mechanism

1. Treat election year as a transition year, not the main treatment year.
2. Build crash outcomes for `t+1`, `t+2`, `t+3`, and `avg(t+1:t+3)`.
3. Build housing and employment outcomes for `avg(t+2:t+4)` in the baseline and `avg(t+2:t+5)` as the longer-run robustness window.
4. Build pre-election averages `avg(t-3:t-1)` and pre-post differences.
5. Run placebo checks on `t-3`, `t-2`, and `t-1`.

## Phase 4: first pass regressions

1. Estimate close-renewal local-linear reduced forms for lagged crash outcomes.
2. Estimate close-renewal local-linear reduced forms for lagged housing outcomes.
3. Estimate close-renewal local-linear reduced forms for lagged employment outcomes.
4. Compare level outcomes with pre-post differences.
5. Decide whether the preferred bandwidth remains 5 or should move to 3 or 7.5.

## Phase 5: robustness and extensions

1. Re-estimate after excluding `duration == "1000"` if not already excluded in baseline.
2. Re-estimate on isolated-event samples only.
3. Use `rdrobust` as a specification check.
4. Add county-clustered and alternative inference choices.
5. Extend to transaction-level housing and point-based crash exposure.
6. Add NAICS-specific employment heterogeneity.

## Phase 6: writing package

1. Draft the institutional section around renewal levies rather than pooled levy types.
2. Draft the identification section around delayed treatment effects.
3. Present the crash first stage before downstream housing and labor results.
4. Explain why election year is not the main treatment year in annual data.
5. Reserve IV estimates for the stage when the crash first stage is clearly established.
