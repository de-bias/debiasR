# Test Health

Last updated: 2026-04-02

## Summary

- Recommended test invocation in development:
  - `devtools::load_all('.')` before running tests directly.
- Full suite includes deterministic method tests and heavier Bayesian tests.
- Observed behavior today:
  - targeted tests for `measure_bias`, `validate_flow_all`, and smoke tests pass under `load_all`.
  - running `test_dir()` without loading package can produce false failures (`function not found`, data object not found).

## Test Tiers (Recommended)

### Tier 1: Fast deterministic (run on every commit)

- `test-measure_bias.R`
- `test-adjust_inverse_penetration.R`
- `test-adjust-selection-rate.R`
- `test-adjust-selection-rate2.R`
- `test-adjust-raking-ratio.R`
- `test-adjust-coefficient.R`
- `test-validate-flow-all.R`
- `test-adjust_raking_ratio-smoke.R` (can be removed once replaced with meaningful tests)

### Tier 2: Bayesian / slow / dependency-sensitive

- `test-adjust-multilevel-bayes.R`
- Requires optional packages and longer runtime.

## Current Known Test Issues

1. Direct `testthat::test_dir('tests/testthat')` without package load context may fail.
2. A placeholder smoke test exists (`2 * 2 == 4`) and does not validate package behavior.
3. Some warnings are locale-related (`LC_ALL='C.UTF-8'`) and mostly non-blocking.

## Recommended CI Strategy

1. Job A (required): fast deterministic tests only.
2. Job B (optional/allowed-to-fail at first): Bayesian tests with explicit dependency install.
3. Ensure CI runs from package root and loads package context before test execution.

## Canonical Commands

```r
# Local all-tests (dev context)
devtools::load_all(".", quiet = TRUE)
testthat::test_dir("tests/testthat", reporter = "summary")
```

```r
# Local fast tier
devtools::load_all(".", quiet = TRUE)
testthat::test_file("tests/testthat/test-measure_bias.R")
testthat::test_file("tests/testthat/test-adjust_inverse_penetration.R")
testthat::test_file("tests/testthat/test-adjust-selection-rate.R")
testthat::test_file("tests/testthat/test-adjust-selection-rate2.R")
testthat::test_file("tests/testthat/test-adjust-raking-ratio.R")
testthat::test_file("tests/testthat/test-adjust-coefficient.R")
testthat::test_file("tests/testthat/test-validate-flow-all.R")
```

