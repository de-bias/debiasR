# Test Health

Last updated: 2026-05-18

## Summary

- Recommended fast-tier entry point:
  - `Rscript scripts/run_fast_tests.R`
- Recommended broad local development runner:
  - `Rscript scripts/run_dev_tests.R`
- Optional Bayesian runner:
  - `Rscript scripts/run_bayesian_tests.R`
- The runner loads the package with `devtools::load_all(".")` before executing targeted tests.
- Full suite still includes a slower Bayesian test file with optional dependencies.
- Observed behavior today:
  - `Rscript scripts/run_fast_tests.R` passes.
  - merged PR #11 (`Codex/validation distribution`) passed the GitHub Actions fast deterministic workflow for commit `59705b376c26a4b33ecbbc9cd1063b037fd61572`.
  - the current working tree has been validated locally; the pushed head still needs remote GitHub Actions confirmation.
  - package-readiness check with tests/vignettes/manual skipped now completes with 0 errors, 0 warnings, and 1 note:
    `devtools::check(document = FALSE, build_args = "--no-build-vignettes", args = c("--no-manual", "--ignore-vignettes", "--no-tests"), error_on = "never")`.
  - the remaining package-readiness note is that the checker could not verify current time.
  - `debiasRdata` is now declared in `Suggests`, so conditional examples no longer trigger an unstated-dependency warning.
  - `debiasRdata` now exists at <https://github.com/de-bias/debiasRdata>; empirical integration should be validated with the installed companion package.
  - local integration smoke check on 2026-05-18 passed by loading `../debiasRdata` and calling `debiasR_example_data(n_areas = 5, complete_grid = TRUE)`. The helper returned default LAD objects and `distance_source = "debiasRdata_lad_centroids"`.
  - core workshop vignettes `vignettes/01-landing-page.qmd` through `vignettes/08-data.qmd` render against the installed `debiasRdata` route using bounded empirical examples.
  - `quarto render notes/project-management/STAGE3_MEASURE_BIAS_REVIEW_NOTEBOOK.qmd` passes.
  - core workshop vignettes and updated testing notebooks render cleanly without `debiasRdata` installed by exiting early with an installation note.
  - targeted tests for `measure_bias`, empirical example-data loading, Stage 3 bias residual diagnostics, deterministic adjustment helpers, Stage 2 validation helpers, and the raking smoke test pass under `load_all`.
  - `validate_bias_residual_structure()` now has a regression test for the documented `population_lm` residual option.
  - `test-adjust-coefficient.R` skips one optional `pscl`-dependent case when `pscl` is not installed.
  - the Bayesian draw-summary names mismatch has been fixed in the optional Bayesian test file.
  - local optional Bayesian test-file run completed with `rstanarm` installed: no failures, one expected skip for the unavailable-backend fallback path, and expected warnings from locale handling, synthetic-distance fallback, and deliberately low-iteration MCMC diagnostics.
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
3. Full package checks that run the optional Bayesian lane may still be slow on machines with `rstanarm` installed.
4. Empirical tests that require `debiasRdata` should remain conditional because the companion package is optional.
5. Some warnings are locale-related (`LC_ALL='C.UTF-8'`) and mostly non-blocking.

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

```r
# Local development tier, excluding optional Bayesian tests by default
Rscript scripts/run_dev_tests.R
```

```r
# Optional Bayesian tier
Rscript scripts/run_bayesian_tests.R
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
