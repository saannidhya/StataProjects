# Variable dictionary

## Geography variables

| Variable | Definition | Source |
| --- | --- | --- |
| `fips_id` | Harmonized local-government ID: place GEOID for cities and villages, county-subdivision GEOID for townships | Employment geocoding, CoreLogic pipeline, rebuilt FARS join, renewal event panel |
| `tendigit_fips` | Legacy 10-digit county-subdivision style identifier | Existing `ohio_taxation` panels |
| `county` | County name | Roads, FARS, ODJFS |
| `subdivisiontype` | `Township`, `City`, or `Village` style local-government type | Roads |

## Election variables

| Variable | Definition |
| --- | --- |
| `election_year` | Year of the road-renewal election |
| `description` | Levy description code; baseline paper uses only `R` |
| `duration` | Renewal duration as recorded in the election data |
| `duration_is_perpetual` | Indicator for `duration == "1000"` |
| `votes_pct_against` | Percent of votes cast against the levy |
| `votes_pct_for` | Percent of votes cast for the levy |
| `margin_against` | `votes_pct_against - 50` |
| `abs_margin_against` | Absolute value of the running variable |
| `failed_levy` | Indicator for `votes_pct_against > 50` |
| `close_3` | Indicator for `abs(margin_against) <= 3` |
| `close_5` | Indicator for `abs(margin_against) <= 5` |
| `close_7_5` | Indicator for `abs(margin_against) <= 7.5` |
| `renewals_same_year` | Number of road renewals observed in the same `FIPS_ID x election_year` before keeping the closest to the cutoff |

## Renewal-spacing variables

| Variable | Definition |
| --- | --- |
| `prev_renewal_year` | Previous renewal year in the same `fips_id` |
| `next_renewal_year` | Next renewal year in the same `fips_id` |
| `years_since_prev_renewal` | Gap to the previous renewal |
| `years_until_next_renewal` | Gap to the next renewal |
| `isolated_3` | No other renewal within 3 years on either side |
| `isolated_5` | No other renewal within 5 years on either side |
| `baseline_close_5` | Recommended first-pass election sample: close-5, finite-duration renewals |
| `recommended_crash_sample` | `baseline_close_5` plus full `t-3:t-1` and `t+1:t+3` availability |
| `recommended_downstream_sample` | `baseline_close_5` plus full `t-3:t-1` and `t+2:t+4` availability, with no next renewal before `t+4` |
| `recommended_downstream_long_sample` | Longer-run robustness sample with full `t-3:t-1` and `t+2:t+5` availability, with no next renewal before `t+5` |

## Election-year controls

| Variable | Definition |
| --- | --- |
| `pop_election` | Population in the election year |
| `medfamy_election` | Median family income in the election year |
| `poverty_election` | Poverty rate in the election year |
| `unemprate_election` | Unemployment rate in the election year |
| `pctown_election` | Owner-occupancy share in the election year |
| `pctrent_election` | Renter share in the election year |

## Crash variables

| Variable | Definition |
| --- | --- |
| `fatal_crashes` | Number of FARS fatal crashes in `fips_id x year` |
| `fatalities` | Sum of `fatals` in `fips_id x year` |
| `any_fatal_crash` | Indicator that at least one fatal crash occurred |
| `fatal_crashes_per_10k` | `10000 * fatal_crashes / pop` |
| `fatalities_per_10k` | `10000 * fatalities / pop` |

## Housing variables

| Variable | Definition |
| --- | --- |
| `sale_amount` | Raw transaction price |
| `log_sale_amount` | `log(sale_amount)` for positive prices |
| `n_sales` | Number of sales in `fips_id x year` |
| `median_sale_amount` | Median sale amount in `fips_id x year` |
| `mean_sale_amount` | Mean sale amount in `fips_id x year` |
| `log_median_sale_amount` | `log(median_sale_amount)` |
| `log_mean_sale_amount` | `log(mean_sale_amount)` |

## Employment variables

| Variable | Definition |
| --- | --- |
| `total_wages` | Sum of annual wages within `fips_id x year` |
| `avg_persons` | Mean annual employment within `fips_id x year` |
| `establishments` | Number of unique establishments in `fips_id x year` |
| `entrants` | Establishment IDs first observed in that `fips_id` and year |
| `exits` | Establishment IDs last observed in that `fips_id` and year |
| `log_total_wages` | `log(total_wages)` for positive wages |
| `log_avg_persons` | `log(avg_persons)` for positive employment |
| `log_establishments` | `log(establishments)` for positive establishment counts |

## Event-time suffixes

| Suffix | Definition |
| --- | --- |
| `_m3`, `_m2`, `_m1` | Outcome values 3, 2, or 1 years before the election |
| `_p0` | Outcome value in the election year |
| `_p1`, `_p2`, ... `_p6` | Outcome values 1 to 6 years after the election |

## Event-window variables

| Variable pattern | Definition |
| --- | --- |
| `{var}_pre_avg_m3_m1` | Average of `{var}` over `t-3:t-1` |
| `{var}_post_avg_p1_p3` | Average of `{var}` over `t+1:t+3` |
| `{var}_post_avg_p2_p4` | Average of `{var}` over `t+2:t+4` |
| `{var}_post_avg_p2_p5` | Average of `{var}` over `t+2:t+5` |
| `{var}_diff_p1_p3_vs_pre` | `avg(t+1:t+3) - avg(t-3:t-1)` |
| `{var}_diff_p2_p4_vs_pre` | `avg(t+2:t+4) - avg(t-3:t-1)` |
| `{var}_diff_p2_p5_vs_pre` | `avg(t+2:t+5) - avg(t-3:t-1)` |
| `{var}_n_pre_m3_m1` | Number of nonmissing values used in the pre-election average |
| `{var}_n_post_p1_p3` | Number of nonmissing values used in `avg(t+1:t+3)` |
| `{var}_n_post_p2_p4` | Number of nonmissing values used in `avg(t+2:t+4)` |
| `{var}_n_post_p2_p5` | Number of nonmissing values used in `avg(t+2:t+5)` |
