# Known Issues

Last updated: 2026-04-02

## High Priority

1. API/docs drift during migration
- Description: some materials still mention pre-migration names or old dataset terms.
- Impact: user confusion and onboarding friction.
- Suggested fix: align README + all top-level docs with `adjust_*` and `validate_flow_*` exports.

2. Test execution context sensitivity
- Description: running tests without first loading package context can cause false negatives.
- Impact: unreliable signal in local and CI usage.
- Suggested fix: standardize test runner workflow and CI scripts around `devtools::load_all('.')` or `R CMD check`.

3. Prototype Bayesian pathway not clearly bounded in all docs
- Description: `adjust_multilevel_bayes()` is stage-1 only; stage-2 imputation pending.
- Impact: users may over-interpret readiness/scope.
- Suggested fix: add explicit prototype badge/section in README and function docs summary.

## Medium Priority

1. Placeholder smoke test
- Description: `tests/testthat/test-adjust_raking_ratio-smoke.R` currently validates only trivial arithmetic.
- Impact: minimal quality assurance value.
- Suggested fix: replace with lightweight functional assertions on real package functions.

2. Working tree transition volume
- Description: many renames/deletions/additions are currently in flight.
- Impact: increased merge/review risk and difficult change auditing.
- Suggested fix: consolidate migration in focused PRs (API rename, data migration, docs alignment, tests alignment).

3. README structure section is not fully descriptive
- Description: repository structure section is incomplete.
- Impact: slower onboarding.
- Suggested fix: update with current folders (`R`, `data`, `data-raw`, `vignettes`, `notes/project-management`, `tests`).

## Low Priority

1. Locale warnings in tests (`LC_ALL='C.UTF-8'`)
- Description: repeated non-fatal warnings appear during test runs.
- Impact: noisy logs.
- Suggested fix: normalize locale settings in CI or suppress non-essential locale changes in tests.

## Issue Tracking Template

Use this mini template for each issue:

- Issue:
- Owner:
- Priority:
- First observed:
- Affected files:
- Decision:
- Target resolution date:
- Status:

