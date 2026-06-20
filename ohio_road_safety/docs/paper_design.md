# Paper design

## Working title

Road Safety, Housing Capitalization, and Local Business Activity: Evidence from Ohio

## Revised core question

What happens after a local government narrowly fails to renew an existing road levy? Do fatal-crash rates rise with a lag, and do housing prices and local business outcomes deteriorate after that safety and infrastructure decline sets in?

## Why the renewal-only design is better

The earlier pooled `A/R` design mixed two distinct objects:

- `A`: authorization or new levy authority
- `R`: renewal of an existing levy

For this paper, renewals are cleaner. A failed renewal is easier to interpret as a negative funding shock to ongoing road maintenance. That makes both exogeneity and timing more defensible:

- the local government already had the revenue stream in place
- passage preserves the status quo more than it creates a new program
- failure should not instantly change annual outcomes in election year
- the main effects should emerge later as maintenance is deferred and roads deteriorate

## Main hypothesis

Narrowly failed road-levy renewals reduce road maintenance capacity. The road stock then deteriorates gradually, increasing severe traffic risk first. Housing and business outcomes respond later, after the deterioration becomes visible and persistent.

Predicted timing:

- fatal crashes: strongest effects in `t+1` to `t+3`
- housing outcomes: strongest effects in `t+2` to `t+4`, with `t+2:t+5` as a longer-run robustness window
- employment and establishment outcomes: strongest effects in `t+2` to `t+4`, with `t+2:t+5` as a longer-run robustness window

## Data architecture

### Election events

Source:
`roads_and_census.dta`

Unit:
renewal election

Preferred sample:

- `purpose2 == "roads"`
- `description == "R"`
- baseline bandwidth `abs(votes_pct_against - 50) <= 5`

Important implementation choices:

- use harmonized `FIPS_ID`
- if a jurisdiction has multiple renewal elections in the same year, keep the election closest to the cutoff
- track time to previous and next renewal to guard against overlapping treatment windows
- keep a flag for `duration == "1000"` as a separate robustness issue rather than pooling it silently into the baseline

### Crash outcomes

Source:
rebuilt FARS `FIPS_ID x year` panel

Preferred outcomes:

- `fatal_crashes_per_10k`
- `fatalities_per_10k`
- `any_fatal_crash`

Because fatal crashes are rare, the paper should emphasize:

- single-year horizons `t+1`, `t+2`, `t+3`
- post averages like `avg(t+1:t+3)`
- changes relative to the pre-election average `avg(t-3:t-1)`

### Housing outcomes

Sources:

- annual local-government housing panel from CoreLogic
- transaction-level CoreLogic file for later hedonic extensions

Preferred baseline outcomes:

- `log_median_sale_amount`
- `n_sales`

Preferred timing:

- average post window `avg(t+2:t+4)` in the baseline
- `avg(t+2:t+5)` as a longer-run robustness window
- change relative to `avg(t-3:t-1)`

Transaction-level hedonic work remains valuable, but it should be a secondary extension after the lagged renewal event design is established.

### Employment outcomes

Source:
geocoded ODJFS employment masterfile aggregated to `FIPS_ID x year`

Preferred baseline outcomes:

- `log_total_wages`
- `log_avg_persons`
- `log_establishments`
- `entrants`
- `exits`

Preferred timing:

- average post window `avg(t+2:t+4)` in the baseline
- `avg(t+2:t+5)` as a longer-run robustness window
- change relative to `avg(t-3:t-1)`

## Geography and merge key

The paper uses the harmonized local-government identifier:

- cities and villages: `state FIPS + place GEOID`
- townships: 10-digit county-subdivision GEOID

This is the same hybrid geography already used in the employment and CoreLogic pipelines, and now rebuilt for FARS and the renewal election panel.

## Preferred samples

### Sample A: annual local-government panel

Unit:
`FIPS_ID x year`

Use:

- descriptives
- noncausal crash-price and crash-employment correlations
- source panel for constructing event-time outcomes

### Sample B: renewal event panel

Unit:
renewal election

Use:

- baseline causal design
- first stage on crash outcomes
- reduced form on housing and employment
- pretrend placebo checks

Each row is one road-renewal election with:

- election-year covariates
- running variable and failure indicator
- event-time outcomes from `t-3` through `t+6`
- averaged pre and post windows

### Sample C: transaction-level housing panel

Unit:
property sale

Use:

- secondary hedonic extension
- point-based crash exposure extension

## Empirical strategy

## 1. Election-centered horizon regressions

For election `j` and horizon `h`, estimate:

`Y_j(h) = alpha_h + tau_h Failed_j + beta_1h Margin_j + beta_2h Failed_j * Margin_j + X_j' Gamma_h + e_jh`

where:

- `Failed_j = 1[votes_pct_against > 50]`
- `Margin_j = votes_pct_against - 50`
- the sample is restricted to close renewal elections
- `X_j` includes election-year controls such as population, income, poverty, unemployment, ownership share, and rent share

Interpretation:

- `tau_h` is the reduced-form effect of narrowly failing a renewal at horizon `h`
- event year `t+0` is not the main treatment year

## 2. First stage on road safety

Primary outcomes:

- `fatal_crashes_per_10k_p1`
- `fatal_crashes_per_10k_p2`
- `fatal_crashes_per_10k_p3`
- `fatal_crashes_per_10k_post_avg_p1_p3`
- `fatal_crashes_per_10k_diff_p1_p3_vs_pre`

Why this matters:

- it directly tests the deterioration story
- it is the necessary bridge between road finance and downstream market outcomes

## 3. Reduced form on downstream outcomes

Primary downstream outcomes:

- `log_median_sale_amount_post_avg_p2_p4`
- `log_median_sale_amount_diff_p2_p4_vs_pre`
- `log_total_wages_post_avg_p2_p4`
- `log_avg_persons_post_avg_p2_p4`
- `log_establishments_post_avg_p2_p4`

This is the cleanest first pass because it respects lag structure and reduces annual noise.

## 4. Placebo pretrends

Estimate the same close-renewal RD on:

- `t-3`
- `t-2`
- `t-1`

for crashes, housing, and employment.

If the design is credible, there should be little systematic discontinuity before the renewal election.

## 5. IV extension after first-stage validation

Only after the renewal failure meaningfully shifts crash exposure should the paper move to:

First stage:

`Crash_j = pi Failed_j + f(Margin_j) + X_j' Lambda + v_j`

Second stage:

`Outcome_j = theta CrashHat_j + f(Margin_j) + X_j' Psi + u_j`

The core paper is therefore:

renewal failure -> lagged safety deterioration -> lagged housing and employment response

## Standard errors and inference

Baseline:

- cluster by county in election-level regressions

Also useful:

- alternative bandwidths: 3 and 7.5
- local-linear and local-quadratic specifications
- `rdrobust` as a robustness check after the first-pass `fixest` runs

## Main threats and responses

### 1. Timing mismatch

Threat:
annual outcomes in election year mostly precede the election.

Response:
do not treat `t+0` as the main effect; use lagged windows.

### 2. Rare crash counts

Threat:
fatal crashes are sparse at small geography.

Response:
use post averages and pre-post differences, not only single-year counts.

### 3. Election heterogeneity

Threat:
authorizations and renewals are not the same institutional object.

Response:
baseline uses renewals only.

### 4. Overlapping elections

Threat:
repeated renewals can contaminate long event windows.

Response:
track spacing to previous and next renewals and use isolated-event flags in the recommended sample.

## Main tables

1. Data sources and harmonized geography
2. Summary statistics for renewal elections and baseline outcomes
3. First stage: failed renewal on lagged crash outcomes
4. Reduced form: failed renewal on lagged housing outcomes
5. Reduced form: failed renewal on lagged employment outcomes
6. Placebo pretrend tests
7. IV estimates, conditional on first-stage strength

## Main figures

1. Histogram of renewal-election margins around 50 percent against
2. Dynamic first stage for fatal crashes
3. Dynamic reduced form for housing
4. Dynamic reduced form for employment
5. Ohio map of harmonized local governments and fatal crashes

## Recommended framing

The strongest framing is no longer "do road levies affect outcomes?" It is:

"When an existing road-funding stream is narrowly not renewed, road safety worsens with a lag. Housing and local business activity then adjust to that deterioration."
