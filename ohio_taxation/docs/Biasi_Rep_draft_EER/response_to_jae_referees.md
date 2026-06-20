# Internal Revision Map From JAE Rejection to EER Submission

This file is not intended for submission. It documents how the EER version repairs the main weaknesses raised in the JAE reports.

## Core Repositioning

- The paper is no longer framed as finding a flaw in Biasi, Lafortune, and Schonholzer (2025).
- The EER version treats the BLS full-sample stacked dynamic RD as the baseline.
- The election-margin exercise is now a robustness check, not a claim that the original design is invalid.
- State-level heterogeneity is presented as a descriptive replication extension, not as a fully identified mechanism test.

## Referee 1 Issues

1. **Full-sample RD critique was too strong.**
   - Implemented fix: Section `Robustness to Election-Margin Restrictions` explicitly says the BLS specification is the baseline and narrower windows are sensitivity checks.
   - Code fix: `docs/biasi2025/analysis/eer_bandwidth_sensitivity.do`.

2. **State-level extension overlapped with BLS appendix.**
   - Implemented fix: Introduction and conclusion acknowledge that BLS already studies heterogeneity.
   - New contribution is narrowed to transparent replication, state-level reporting, bandwidth sensitivity, and descriptive state-mechanism mapping.

3. **Mechanism discussion was speculative.**
   - Implemented fix: The paper now links state effects to project mix, capital stock, FRPL share, and minority share, while stating that the evidence is descriptive.
   - Code fix: `docs/biasi2025/analysis/eer_state_mechanisms.do`.

## Referee 2 Issues

1. **Bandwidth critique needed documentation.**
   - Implemented fix: Added a reproducible bandwidth script and manuscript section.

2. **State-level empirical model was unclear.**
   - Implemented fix: Revised `data_and_method.tex` to describe state-specific estimation and sample restrictions.
   - Code fix: `docs/biasi2025/analysis/eer_state_sample_audit.do`.

3. **Mechanisms were invoked but not tested.**
   - Implemented fix: Added mechanism summary and descriptive association tables.

4. **Housing/test-score heterogeneity needed more development.**
   - Implemented fix: Rewrote `results.tex` so the state-level estimates are interpreted as heterogeneity around the confirmed national average rather than as a stand-alone causal claim.
