# Project Status

Last updated: 2026-06-25

## Snapshot

- Project stage: active development (`0.0.0.9004`)
- Repository visibility: public on GitHub since 2026-06-04
- Package scope: OD mobility bias correction methods + Stage 2 validation toolkit + Stage 3 bias residual diagnostics + distributional bias diagnostics
- API direction: stable adjustment methods use `adjust_*`; validation helpers use `validate_flow_*`
- Bayesian component: `adjust_multilevel_bayes()` is the main methodological innovation and now has observed and complete-grid prediction scopes; the default `coverage_offset` Bayesian route is approved for observed-flow empirical LAD S1-S4 workflows, while `latent_two_level` is approved as an advanced observed-row repeated-source S3/S4 route
- Current execution board: see [TASK_BOARD.md](TASK_BOARD.md)

## Stable vs Experimental

### Stable adjustment and validation API

- `measure_bias()`
- `measure_bias_distribution()`
- `validate_bias_residual_structure()`
- `adjust_inverse_penetration()`
- `adjust_selection_rate()`
- `adjust_selection_rate2()`
- `adjust_raking_ratio()`
- `adjust_coefficient()`
- `validate_flow_overall()`
- `validate_flow_pairs()`
- `validate_flow_residuals()`
- `validate_flow_residual_structure()`
- `validate_flow_distribution()`
  - legacy aliases retained temporarily: `validate_flow_benchmark()`, `validate_flow_all()`

### Bayesian multilevel path

- `adjust_multilevel_bayes()`
  - observed-flow correction remains backward compatible
  - complete-grid prediction mode is available for strict square OD matrices
  - row-status metadata distinguishes observed MPD rows from zero-filled source-missing cells
  - scenario metadata now distinguishes S1 single-source/single-time, S2 single-source/multiple-time, S3 multiple-source/single-time, and S4 multiple-source/multiple-time inputs
  - S1-S4 repeated source/time scenarios can now be fitted with `model_engine = "bayesian"` or `model_engine = "frequentist"`
  - `observation_model = "coverage_offset"` is approved as the default
    Bayesian implementation for observed-flow LAD empirical workflows after
    full S4 validation with real LAD centroid distances, acceptable diagnostics,
    and competitive benchmark validation metrics
  - `observation_model = "reduced_form"` remains a compatibility and
    sensitivity route rather than the recommended Bayesian implementation
  - local source/time flow data for empirical S1-S4 testing are available
    outside the repository at `/Volumes/DEBIAS/data/outputs/flows`; use the HTW
    branch first for `mapp1`, `mapp2`, and Census benchmark validation at
    LAD/LTLA and MSOA support, but do not commit the raw data or rendered bulky
    outputs into `debiasR`
  - `observation_model = "latent_two_level"` is available as an approved
    advanced Bayesian backend for observed-row repeated source/time structures;
    it creates `latent_flow_id` states, estimates latent true-flow intensities
    with a custom Stan backend, records latent-state metadata and
    identifiability notes, and requires sampler diagnostics for applied use
  - `model_terms` metadata records the resolved default fixed-effect and random-effect structure for the shared S1-S4 scenario contract
  - `model_engine = "frequentist"` remains useful for fast testing, experimentation, and method comparison before committing to Bayesian runtime
  - performance and dependency footprint are heavy relative to fixed-rule adjustment methods
  - backend guidance: `rstanarm` is the default package dependency for standard Poisson / negative-binomial models because it is lighter and easier to fit in a package workflow; use optional `brms` when you need extra flexibility, especially zero-inflated or more complex Bayesian specifications

## What Changed Recently

- External HTW flow outputs under `/Volumes/DEBIAS/data/outputs/flows` now
  provide empirical source/time inputs for S1-S4 validation: Mapp1 weekly/monthly
  files, Mapp2 monthly files, and Census travel-to-work benchmarks at LAD/LTLA
  and MSOA support. The migration/Twitter branch remains secondary until
  code/label harmonisation is audited.
- The `debiasR` repository was made public on GitHub on 2026-06-04. Treat
  repository docs, vignettes, workflows, issues, pull requests, and tracked
  assets as public-facing by default.
- Function naming migrated from `method*` to `adjust_*`
- Validation API migrated from `validate_flows()` to `validate_flow_overall()` and `validate_flow_pairs()`, with legacy aliases retained temporarily for compatibility
- Data assets migrated from toy datasets to simulated test fixtures, while user-facing examples now default to the optional companion package `debiasRdata` (<https://github.com/de-bias/debiasRdata>) for the empirical LAD travel-to-work workflow.
- Adjustment methods are documented as a menu of coverage-based, margin-constrained, benchmark-calibrated, and multilevel modelling options; the Bayesian multilevel path is the central innovation but requires separate runtime and dependency validation.
- CI scaffolding now includes a fast core workflow plus a separate manual Bayesian workflow
- Bias metric updated to:
  - `coverage_bias = 1 - user_count/population`
  - `coverage_score = user_count/population`
- `measure_bias_distribution()` now compares benchmark-population and
  active-user spatial distributions using KL divergence, Jensen-Shannon
  divergence, share differences, and area-level contribution outputs.
- Top-level docs were refreshed to reflect the exported API, the implemented `debiasRdata` companion package, simulated test fixtures, and current repository structure
- User-facing docs now name the default empirical OD matrices explicitly: `lad_OD_travel2work` for observed MPD travel-to-work flows and `census_lad_OD_travel2work` for the Census benchmark. MSOA assets remain available through `geography = "msoa"` when needed.
- `debiasR_example_data()` now supports optional complete-grid OD output with zero-filled absent pairs, row-status indicators, an OD audit for strict square support, and selected-area LAD distances computed from `debiasRdata::lad_centroids`.
- `adjust_multilevel_bayes()` now supports `prediction_scope = "complete_grid"` for supplied square OD matrices; it fits on originally observed source rows when `mpd_observed` is available and predicts across the grid.
- `adjust_multilevel_bayes()` now has explicit scenario/source/time parameters for the S1-S4 multilevel path, with `model_engine = "bayesian"` for posterior fitting and `model_engine = "frequentist"` for fast design, test iteration, and method comparison.
- `adjust_multilevel_bayes()` now accepts a primary R formula interface. Area covariates are prepared with origin/destination suffixes, formula random-effect terms drive model dispatch, and `income_col` remains only as a legacy default-formula helper.
- `adjust_multilevel_bayes()` now also accepts split `mobility_formula` and
  `bias_formula` inputs. The package combines them internally for the current
  reduced-form fit, while metadata records the conceptual true-flow and
  observation-bias components separately.
- Enhancement issue #18 records the genuinely latent two-level Bayesian model,
  where `F_true_ij` is estimated explicitly rather than recovered only through
  a zero-bias counterfactual prediction. The current branch includes a first
  approved advanced custom Stan `latent_two_level` backend that estimates OD or
  OD-time latent true-flow intensities. The backend now exposes latent prior
  and sampler controls, records richer diagnostics, uses identified
  sum-to-zero latent/source/time contrasts, and splits optional Bayesian tests
  into `rstanarm-smoke`, full `rstanarm`, and latent-Stan scopes.
- `validate_flow_distribution()` now supports `comparisons = "all"` so raw
  MPD, adjusted MPD-derived, and benchmark OD-flow allocation distributions can
  be compared through the same KL/JSD contract.
- The validation vignette now classifies this destination-share diagnostic as
  Level 4 distributional allocation validation, keeping Level 3 focused on
  individual origin-destination pair magnitudes, residuals, and outliers.
- A post-public repository hygiene pass reviewed public docs, pkgdown exposure,
  repository metadata, workflow deployment controls, tracked assets, and
  sensitive-content patterns. It removed a tracked workshop `.docx` with
  embedded Word metadata/comments, cleaned absolute local-path links, updated
  GitHub-owned Actions pins, restricted manual pkgdown deployments to `main`,
  clarified conduct-reporting and non-code licensing text, and removed stale
  public docs scaffolding.
- The default S1-S4 formula contract is documented for both engines: S1 uses the base OD/covariate/bias terms, S2 adds `mpd_time`, S3 adds `mpd_source`, and S4 adds `mpd_source + mpd_time`; S4 source-time interaction remains deferred.
- The adjustment vignette now keeps the Bayesian walkthrough focused on the
  default coverage-offset example with constant source/time columns and
  raw/adjusted/benchmark comparison columns.
- The advanced Bayesian adjustment vignette now explains S1-S4 source/time
  structures, the approved advanced `latent_two_level` backend, reduced-form
  compatibility mode, and Bayesian diagnostics.
- The Bayesian vignette exposition was refined on 2026-06-25 so the
  "observation equation" and "true-flow prediction equation" language is scoped
  explicitly to the coverage-offset route. The advanced Bayesian vignette now
  gives separate conceptual equations and interpretation for `coverage_offset`,
  `reduced_form`, and `latent_two_level`, avoiding the earlier risk of implying
  that all variants share the same two-equation structure.
- Empirical approval decision recorded on 2026-06-25: the default
  `coverage_offset` Bayesian implementation is approved as a viable empirical
  alternative for observed-flow LAD S1-S4 workflows. The full LAD S4
  confirmatory run used real `debiasRdata::lad_centroids` distances, 309 LADs,
  74,874 MPD rows, 64,162 validation pairs, `iter = 1000`, and `chains = 4`.
  Both fixed and origin random-intercept coverage-offset fits completed with no
  failures, max R-hat about 1.01, minimum effective sample size 485, and no
  convergence warnings. The Bayesian fits were competitive on MAE and had lower
  RMSE than the benchmark-calibrated deterministic comparators in that S4 run.
  A separate real-data approval now promotes `latent_two_level` as an advanced
  repeated-source route for observed-row S3/S4 workflows with diagnostic
  guardrails.
- Empirical latent approval decision recorded on 2026-06-25:
  `observation_model = "latent_two_level"` is approved as an advanced
  repeated-source S3/S4 route after real HTW MPD source/time runs, real Census
  benchmark validation, real `debiasRdata::lad_centroids` distances, and
  prior-sensitivity checks. The confirmatory default-prior run used
  `iter = 1000`, `chains = 4`, and `latent_max_treedepth = 15`; S3 and S4 both
  completed with no divergences, no treedepth hits, E-BFMI above 0.91, max
  R-hat about 1.023, and minimum effective sample size about 190.
- The adjustment vignette now reads its compact Bayesian example output from a
  precomputed package artifact reporting posterior median and mean summaries;
  maintainers can regenerate it explicitly with
  `Rscript scripts/precompute_v06_bayesian_example.R` when the model or data
  change.
- Issue #58 updated the adjustment vignette advanced section into the practical
  user-facing guide for `adjust_multilevel_bayes()`: it now explains the
  default coverage-offset true-flow model, clarifies that active-user coverage
  enters as a fixed observation offset rather than a fitted bias coefficient,
  maps rendered Bayesian example columns to returned object columns, and gives
  a short S2-S4 repeated source/time callout for the approved advanced
  `latent_two_level` backend. The advanced Bayesian adjustment vignette remains
  the deeper companion reference for formulas, backend choices, and diagnostics.
- Issue #56 added a distinct Level 5 spatial/residual structure diagnostics
  layer to the validation vignette. The new section demonstrates
  `validate_flow_residual_structure()` with residual-versus-benchmark-flow
  correlation, optional Moran's I from user-supplied neighbour links, and
  residual-versus-covariate correlation, while keeping Level 4 focused on
  origin-conditioned destination-share allocation.
- `validate_flow_residual_structure()` now optionally computes Local Moran's I
  and LISA cluster diagnostics for area-level residuals using the same
  user-supplied neighbour-link interface as global Moran's I. The validator uses
  base-R permutation pseudo p-values and does not require `sf`, `spdep`, or
  other spatial dependencies.
- Prototype validation plotting functions are now exported through
  `plot_validation_*()` functions for metric matrices, residual violin plots,
  pairwise flow scatterplots, standard-deviation and quantile residual-band
  stacked bars, distributional allocation heatmaps, pairwise divergence
  matrices, residual-structure diagnostic summaries, and optional LISA cluster
  maps from user-supplied `sf` boundaries. They use the shared flow-comparison
  convention, default to `adjusted_vs_benchmark`, and expose
  `error_measures`, `comparisons`, and `methods` selectors. A short visual review notebook in
  `notes/project-management/VALIDATION_VISUAL_PROTOTYPES.qmd` illustrates the
  designs using deterministic simulated package data.
- Decision recorded on 2026-06-19: these visualisation functions should be used
  in the existing validation vignette rather than documented in a separate
  validation-visualisation vignette. The prototype notebook and rendered notes
  under `notes/project-management/` are internal design/review material.
- Fast core tests passed after replacing the placeholder raking smoke test and removing selection-rate deprecation warnings
- Stage 2 maintainer review is complete: `validate_flow_residual_structure()` is stable public API; optional scalar diagnostics remain dependency-light, while LISA cluster mapping is exposed separately through `plot_validation_lisa_map()` and requires user-supplied `sf` boundaries; the optional `debiasRdata` companion package is the empirical data source.
- Stage 3 measure-bias diagnostics now include active-user coverage residuals, optional Moran's I, benchmark origin/destination flow correlations, covariate correlations, map-ready data, and optional plots through `validate_bias_residual_structure()`.
- Stage 3 maintainer review is complete: `validate_bias_residual_structure()` is stable public API; optional diagnostic plots remain inside the helper for now; a simple population-only linear-regression residual is included as a descriptive diagnostic.
- The Zenodo data gate is documented in `DATA_REDISTRIBUTION_DECISION.md`: do not bundle the full record in `debiasR`; use the separate optional `debiasRdata` package for empirical travel-to-work examples and keep simulated data as lightweight test fixtures.
- `debiasRdata` now exists locally and remotely at <https://github.com/de-bias/debiasRdata>. It supplies MSOA and LAD OD-flow assets, with the LAD route (`lad_OD_travel2work`, `census_lad_OD_travel2work`) now the default for `debiasR`, plus `lad_centroids` for selected-area distance derivation.
- Cleanup pass removed tracked rendered notebook HTML/assets from project-management notes while keeping the Quarto sources, and declared `debiasRdata` in `Suggests` for conditional examples.
- Public-release cleanup removed legacy raw calibration CSVs from `debiasR`.
  Public empirical examples now route through the audited `debiasRdata`
  package; `debiasR` keeps the prebuilt `simulated_*` fixtures for tests and
  lightweight examples.

## Verification

- Verified on 2026-05-05 with `Rscript scripts/run_fast_tests.R`
- Result: pass
- Notes:
  - adjustment, Stage 2 validation, and Stage 3 bias residual diagnostics tests passed
  - core workshop vignettes and updated testing notebooks render cleanly when `debiasRdata` is absent by exiting early with an installation note
  - `quarto render notes/project-management/STAGE3_MEASURE_BIAS_REVIEW_NOTEBOOK.qmd` completed successfully
  - `test-adjust-coefficient.R` skipped one optional `pscl`-dependent case because `pscl` is not installed
  - full `devtools::check(document = FALSE, build_args = "--no-build-vignettes", args = c("--no-manual", "--ignore-vignettes"), error_on = "never")` was also run on 2026-05-05; it completed with 1 error, 2 warnings, and 3 notes before the 2026-05-08 package-readiness cleanup
- GitHub Actions on merged PR #11 (`Codex/validation distribution`) completed the fast core workflow successfully for commit `59705b376c26a4b33ecbbc9cd1063b037fd61572`.
- The current working tree has been validated locally; the pushed head still needs remote GitHub Actions confirmation.
- Verified on 2026-05-08 with `Rscript scripts/run_fast_tests.R`.
- Result: pass.
- Package-readiness check on 2026-05-08 with tests, vignettes, and manual skipped completed with 0 errors, 0 warnings, and 2 notes:
  - `devtools::check(document = FALSE, build_args = "--no-build-vignettes", args = c("--no-manual", "--ignore-vignettes", "--no-tests"), error_on = "never")`
  - historical remaining notes: the then-missing optional companion data package and current time verification
- A local optional Bayesian test-file run completed on 2026-05-08 with `rstanarm` installed. Result: no failures, one expected skip for the unavailable-backend fallback path, and expected warnings from locale handling, synthetic-distance fallback, and intentionally low-iteration MCMC convergence diagnostics.
- Verified on 2026-05-18 by loading the sibling `../debiasRdata` checkout and calling `debiasR_example_data(n_areas = 5, complete_grid = TRUE)`.
- Result: pass. The helper returned both `lad_OD_travel2work` and `census_lad_OD_travel2work`; `metadata$geography` was `lad`, and `distance_source` was `debiasRdata_lad_centroids`.
- Package-readiness cleanup check on 2026-05-18 with tests, vignettes, and manual skipped completed with 0 errors, 0 warnings, and 1 note:
  - `devtools::check(document = FALSE, build_args = "--no-build-vignettes", args = c("--no-manual", "--ignore-vignettes", "--no-tests"), error_on = "never")`
  - remaining note: the checker could not verify current time
- Documentation validation on 2026-05-18 rendered the core workshop vignettes `vignettes/01-landing-page.qmd` through `vignettes/08-data.qmd` against the installed `debiasRdata` route using bounded empirical examples.
- `validate_bias_residual_structure()` now supports and tests the documented `population_lm` residual option.
- Verified on 2026-05-21 with targeted
  `Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test-adjust-multilevel-frequentist-dev.R', reporter = 'summary')"`.
- Result: pass. The targeted file covers S1-S4 scenario resolution, source/time metadata, observed prediction, complete-grid prediction, MSOA-like default frequentist formula-contract fixtures, and `model_terms` metadata.
- Verified on 2026-05-21 with `Rscript scripts/run_fast_tests.R`.
- Result: pass. Existing locale warnings remained, and the optional `lme4` tiny-data smoke path can still print a singular-fit message.
- Package-readiness check on 2026-05-21 with tests, vignettes, and manual skipped completed with 0 errors, 0 warnings, and 1 note:
  - `devtools::check(document = FALSE, build_args = "--no-build-vignettes", args = c("--no-manual", "--ignore-vignettes", "--no-tests"), error_on = "never")`
  - remaining note: the checker could not verify current time
- Vignette validation on 2026-05-21 rendered:
  - `vignettes/06-adjusting-biases.qmd`
  - `vignettes/testing/methods-conceptual-guide.qmd`
  - `vignettes/testing/simulated-methods-walkthrough.qmd`
  - `vignettes/testing/short-illustration.qmd` to a temporary output directory
  - `vignettes/testing/method-comparison.qmd` to a temporary output directory
- Verified on 2026-06-12 with `Rscript scripts/run_fast_tests.R`.
- Result: pass. The fast deterministic tier covers the frequentist S1-S4
  scenario contract, complete-grid metadata, and the unchanged deterministic
  adjustment/validation API.
- Bayesian S2-S4 validation on 2026-06-12 used a targeted `rstanarm` smoke
  check for S4 repeated source/time fitting because the full optional Bayesian
  test file remains slow. The targeted check verifies scenario metadata,
  source/time-specific coverage offsets, and MPD-scale versus true-flow-scale
  prediction algebra.
- Fast deterministic validation on 2026-06-12 passed with
  `Rscript scripts/run_fast_tests.R`. The tier now covers
  `measure_bias_distribution()`, extended `validate_flow_distribution()`
  comparisons, and the latent two-level backend metadata/data-contract path.
  The `0.0.0.9002` latent backend work adds a custom Stan contract and split
  smoke scopes that should pass before opening a ready PR; full optional
  Bayesian scopes should run before closing #18.
- Latent Bayesian hardening on 2026-06-13 adds an optional
  `Rscript scripts/run_bayesian_tests.R latent-stress` scope. The scope fits
  larger S3 repeated-source and S4 source-time complete-grid synthetic
  fixtures with the custom `stan_latent` backend and checks latent true-flow
  invariance, observation-scale variation, zero-filled prediction rows, and
  sampler diagnostics. It also corrects the custom Stan intercept prior scale.
  The later 2026-06-25 real-data approval supersedes the earlier experimental
  promotion gate for `latent_two_level`.
- Verified locally on 2026-06-13 with
  `/Library/Frameworks/R.framework/Resources/bin/Rscript scripts/run_fast_tests.R`,
  `/Library/Frameworks/R.framework/Resources/bin/Rscript scripts/run_bayesian_tests.R latent-stress`,
  and no-argument
  `/Library/Frameworks/R.framework/Resources/bin/Rscript scripts/run_bayesian_tests.R`
  after changing the default Bayesian scope to `smoke`.
- Result: pass. The local latent-stress run used R 4.5.2, `testthat` 3.3.2,
  `rstanarm` 2.32.2, and `rstan` 2.32.7, and completed in about 58 seconds.
- Verified issue #56 locally on 2026-06-13 with `git diff --check`, a
  standalone Quarto render of `vignettes/v07-validation.qmd` to
  `/private/tmp/debiasr-v07-render-issue56`, a pkgdown-style preview to
  `/private/tmp/debiasr-pkgdown-preview-issue56` with `NEWS.md` hidden during
  the build, targeted HTML checks for the Level 5 section and hidden setup, and
  `/Library/Frameworks/R.framework/Resources/bin/Rscript scripts/run_fast_tests.R`.
- Result: pass. The rendered validation article includes Level 5 spatial/residual
  structure diagnostics and keeps Level 4 framed as distributional allocation
  validation.
- Verified Local Moran/LISA residual diagnostics locally on 2026-06-13 with the
  targeted residual-structure test file, `git diff --check`,
  `/Library/Frameworks/R.framework/Resources/bin/Rscript scripts/run_fast_tests.R`,
  a standalone render of `vignettes/v07-validation.qmd` to
  `/private/tmp/debiasr-v07-render-local-moran`, and a pkgdown-style preview to
  `/private/tmp/debiasr-pkgdown-preview-local-moran` with `NEWS.md` hidden
  during the build.
- Result: pass. The rendered validation article and reference page show the
  optional Local Moran/LISA diagnostics, hidden setup chunks remain hidden, and
  no new mandatory spatial dependencies were added.
- Verified the full empirical LAD S4 `coverage_offset` Bayesian route locally
  on 2026-06-25 with a quiet manual runner derived from
  `notes/project-management/BAYESIAN_EMPIRICAL_FLOW_S1S4_VALIDATION.qmd`.
- Result: pass. The run fitted fixed and origin random-intercept Bayesian
  coverage-offset models on 309 LADs and 74,874 S4 MPD source/time rows using
  real LAD centroid distances, `iter = 1000`, and `chains = 4`. Both Bayesian
  fits completed; max R-hat was about 1.01, minimum effective sample size was
  485, and there were no R-hat, ESS, or non-convergence warnings. S4
  adjusted-versus-benchmark metrics placed Bayesian coverage-offset behind
  raking ratio and inverse penetration on MAE but ahead of coefficient OLS, and
  lower than all three on RMSE.

## Current Risks / Blockers

1. Public repository visibility raises the bar for repository hygiene: avoid committing confidential material, credentials, restricted raw data, or development-only artifacts that are not intended for public release.
2. Documentation mismatch risk now mainly sits in archival migration materials and older review notebook sources that intentionally use fixed test fixtures.
3. Test suite reliability still depends on using the curated runner rather than raw `test_dir()` calls.
4. Bayesian tests are slower and environment-sensitive due to MCMC runtime and optional `brms` support; run them manually when Bayesian-lane validation is needed.
5. CI has been scaffolded and the fast core workflow passed on merged PR #11; the current branch head still needs live validation on the next PR or push.
6. Full cartographic residual maps remain user-supplied. The optional LISA map helper uses `sf` boundaries supplied by the user rather than bundling or inferring geometries.
7. The default LAD empirical route now has selected-area distance support through `lad_centroids`; MSOA distance-aware examples still require a future `msoa_OD_distance` or `msoa_centroids` asset if `geography = "msoa"` is needed.

## Immediate Priorities

1. Review public-facing docs, pkgdown pages, repository metadata, and tracked assets after the 2026-06-04 visibility change.
2. Validate the current branch head with local tests and the next GitHub Actions run.
3. Validate the optional/manual Bayesian workflow behavior on GitHub Actions
   when Bayesian-lane validation is needed; the runner now supports separate
   `rstanarm-smoke`, full `rstanarm`, `latent-smoke`, and `latent-stress`
   scopes.
4. Keep top-level docs synchronized with exported API (`NAMESPACE`).
5. Record additional LAD/MSOA runtime expectations as larger empirical latent
   grids are attempted.
6. Confirm the optional/manual Bayesian workflow on GitHub Actions for the
   `latent-smoke` and `latent-stress` scopes.
7. Use MSOA-scale inputs for software/runtime stress tests and LAD-scale inputs for vignettes and teaching material as the S1-S4 scenario work develops.
