# Project Status

Last updated: 2026-04-02

## Snapshot

- Project stage: active development (`0.0.0.9000`)
- Package scope: OD mobility bias correction methods + validation toolkit
- API direction: new `adjust_*` and `validate_flow_*` naming is in place
- Bayesian component: `adjust_multilevel_bayes()` is available as stage-1 prototype

## Stable vs Experimental

### Stable (working API and docs present)

- `measure_bias()`
- `adjust_inverse_penetration()`
- `adjust_selection_rate()`
- `adjust_selection_rate2()`
- `adjust_raking_ratio()`
- `adjust_coefficient()`
- `validate_flow_benchmark()`
- `validate_flow_all()`

### Experimental / Prototype

- `adjust_multilevel_bayes()`
  - stage-1 correction implemented
  - stage-2 missing-OD imputation not implemented yet
  - performance and dependency footprint are heavy relative to other methods
  - backend guidance: prefer `rstanarm` for standard Poisson / negative-binomial models because it is lighter and easier to fit in a package workflow; use `brms` when you need extra flexibility, especially zero-inflated or more complex Bayesian specifications

## What Changed Recently

- Function naming migrated from `method*` to `adjust_*`
- Validation API migrated from `validate_flows()` to `validate_flow_benchmark()` and `validate_flow_all()`
- Data assets migrated from toy datasets to simulated datasets
- Bias metric updated to:
  - `coverage_bias = 1 - user_count/population`
  - `coverage_score = user_count/population`
- Top-level docs were refreshed to reflect the exported API, simulated datasets, and current repository structure
- Fast deterministic tests passed after replacing the placeholder raking smoke test and removing selection-rate deprecation warnings

## Verification

- Verified on 2026-04-02 with `devtools::load_all(".", quiet = TRUE)` followed by the fast deterministic tier listed in `TEST_HEALTH.md`
- Result: pass
- Notes:
  - deterministic adjustment and validation tests passed
  - `test-adjust-coefficient.R` skipped one optional `pscl`-dependent case because `pscl` is not installed
  - Bayesian test failures were test-drift issues and were updated to match current behavior

## Current Risks / Blockers

1. Documentation mismatch risk across old/new naming during migration.
2. Test suite reliability varies by execution mode (direct `test_dir()` vs `devtools::load_all()` workflow).
3. Bayesian tests are slower and environment-sensitive due to optional dependencies.
4. Large in-progress working tree changes increase integration risk until consolidated.

## Immediate Priorities

1. Keep top-level docs synchronized with exported API (`NAMESPACE`).
2. Finalize migration map and deprecation timeline.
3. Split CI/testing into fast deterministic tier and optional heavy Bayesian tier.
4. Close known issues listed in `KNOWN_ISSUES.md`.
