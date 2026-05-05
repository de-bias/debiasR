# Test Health

Last updated: 2026-05-05

## Summary

- Recommended fast-tier entry point:
  - `Rscript scripts/run_fast_tests.R`
- The runner loads the package with `devtools::load_all(".")` before executing targeted tests.
- Full suite still includes a slower Bayesian test file with optional dependencies.
- Observed behavior today:
  - `Rscript scripts/run_fast_tests.R` passes.
  - `quarto render notes/project-management/STAGE3_MEASURE_BIAS_REVIEW_NOTEBOOK.qmd` passes.
  - core workshop vignettes and updated testing notebooks render cleanly without `debiasRdata` installed by exiting early with an installation note.
  - targeted tests for `measure_bias`, empirical example-data loading, Stage 3 bias residual diagnostics, deterministic adjustment helpers, Stage 2 validation helpers, and the raking smoke test pass under `load_all`.
  - `test-adjust-coefficient.R` skips one optional `pscl`-dependent case when `pscl` is not installed.
  - full `devtools::check(...)` currently fails when it runs the optional Bayesian lane because `test-adjust-multilevel-bayes.R` compares unnamed actual draw summaries to named expected vectors.
  - running `test_dir()` without loading package can produce false failures (`function not found`, data object not found).

## Test Tiers (Recommended)

### Tier 1: Fast deterministic (run on every commit)

- `tests/testthat/test-measure_bias.R`
- `tests/testthat/test-example-data.R`
- `tests/testthat/test-validate-bias-residual-structure.R`
- `tests/testthat/test-adjust_inverse_penetration.R`
- `tests/testthat/test-adjust-selection-rate.R`
- `tests/testthat/test-adjust-selection-rate2.R`
- `tests/testthat/test-adjust-raking-ratio.R`
- `tests/testthat/test-adjust-coefficient.R`
- `tests/testthat/test-validate-flow-overall.R`
- `tests/testthat/test-validate-flow-pairs.R`
- `tests/testthat/test-validate-flow-residuals.R`
- `tests/testthat/test-validate-flow-residual-structure.R`
- `tests/testthat/test-validate-flow-distribution.R`
- `tests/testthat/test-adjust_raking_ratio-smoke.R`

### Tier 2: Bayesian / slow / dependency-sensitive

- `tests/testthat/test-adjust-multilevel-bayes.R`
- Requires optional packages and longer runtime.

## Current Known Test Issues

1. Direct `testthat::test_dir("tests/testthat")` without package load context may fail.
2. Bayesian tests are slower and environment-sensitive because of optional dependencies.
3. Full package check currently fails in `test-adjust-multilevel-bayes.R` when the optional Bayesian lane runs; the observed failure is a names mismatch in draw-summary comparisons, not a Stage 3 diagnostics failure.
4. Some warnings are locale-related (`LC_ALL='C.UTF-8'`) and mostly non-blocking.

## Recommended CI Strategy

1. Job A (required): fast deterministic tests only.
2. Job B (manual / optional): Bayesian tests with explicit dependency install.
3. Ensure CI runs from package root and loads package context before test execution.
4. Keep the fast lane lightweight by installing hard dependencies plus the explicit test runner packages rather than the full optional stack.

## Canonical Commands

```r
# Local fast tier
Rscript scripts/run_fast_tests.R
```

```bash
# Optional-data vignette smoke checks
quarto render vignettes/03-getting-set-up.qmd
quarto render vignettes/testing/empirical-methods-walkthrough.qmd
```

```r
# Local all-tests (dev context)
devtools::load_all(".", quiet = TRUE)
testthat::test_dir("tests/testthat", reporter = "summary")
```
