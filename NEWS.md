# debiasR 0.0.0.9007

- Flow validation matches shared source and period identifiers and detects ambiguous reference rows.
- OLS adjustments apply fitted intercepts at the calibration level. Raking now reports achieved margins and checks convergence after clipping.
- Both selection-rate methods reuse coverage and benchmark matches during calibration, report the number of scored observations, and reject empty calibration support.
- Inverse penetration flags unsupported weights and detects conflicting coverage records.
- Local Moran permutations handle a single eligible neighbour and accumulate exceedance counts directly. Bayesian predictions compute their interval and median together.

# debiasR 0.0.0.9006

### Repository governance

- Documented the two PR merge routes: the default review route and a
  Francisco-only maintainer-bypass route for PRs opened by or on behalf of
  Francisco Rowe when deadlines or reviewer availability require direct merge
  or squash merge through the PR process.
- Made the required fast deterministic GitHub Actions workflow path-aware so
  package-relevant changes still run `scripts/run_fast_tests.R`, while
  documentation, website-text, and governance-only pull requests pass through
  an explicit green skip step and leave pkgdown to validate website-facing
  changes.

### Adjustment workflows

- Simplified the v06 adjustment vignette's method summary table so it focuses
  on method, benchmark role, rationale, strengths, and limitations, and renamed
  the adjustment-methods section for clearer scanning.
- Added `adjust_all_methods()` as an exported convenience function for fitting
  the main adjustment methods on the same MPD, coverage, benchmark, covariate,
  and distance inputs. The adjustment vignette now uses this package function
  instead of a vignette-local helper.
- Simplified user-facing vignette code by relying on `library(debiasR)` and
  removing visible `debiasR::` namespace prefixes, while keeping explicit
  namespaces in hidden helper code where useful.

### Bias-measurement documentation

- Clarified the proportional baseline explanation in the coverage-bias vignette
  with a two-panel visual showing observed user counts against the
  constant-coverage baseline and residual departures from that baseline.

### Validation documentation

- Reworked the validation vignette around the full real LAD
  origin-destination support from `debiasRdata`, using live deterministic
  adjustment examples, row-audit checks, and exported `validate_flow_*()`
  workflows across the five validation levels. Full-LAD
  Bayesian fitting remains in the advanced Bayesian vignette rather than the
  live validation render because routine vignette builds should not refit MCMC.
- Promoted reusable validation-vignette workflows into exported package API:
  `validate_flow_prepare_output()`, `validate_flow_prepare_outputs()`,
  `validate_flow_overall_methods()`, `validate_flow_residual_methods()`,
  `validate_flow_distribution_methods()`,
  `validate_flow_residual_structure_methods()`, and
  `validate_flow_margins()`.
- Added the full-LAD Bayesian `coverage_offset` validation outputs to the
  validation vignette's method-inputs, row-audit, and five-level validation
  reporting. The article now reads these Bayesian rows from the precomputed
  `inst/extdata/` bundle rather than refitting Bayesian models during render,
  uses selected `coverage_offset` variants in the validation walkthrough, and
  keeps `latent_two_level` validation outputs deferred until a full
  repeated-source result file exists.
- Expanded the v07 Bayesian validation output set with two additional
  full-LAD `coverage_offset` coverage-scale sensitivity specifications and one
  full-LAD `reduced_form` sensitivity specification. The validation vignette
  keeps the true-flow `coverage_offset` rows in the main comparison and reports
  `reduced_form` separately because it is an MPD-scale counterfactual.
- Added a v07 latent two-level validation generation script for observed-row
  repeated-source LAD/LTLA inputs from the external HTW flow files. A bounded
  smoke run validates the data preparation and output schema; the full
  repeated-source confirmatory run remains a long manual job.
- Streamlined the validation vignette so the level-by-level walkthrough relies
  on figures and prose instead of large result tables. Displayed tables are
  limited to the compact validation-measures reference and the method-inputs
  table, now placed at the end of the article.
- Restored a compact validation-measures reference table in v07 explaining the
  main error, correlation, distributional, and residual-structure metrics used
  by `debiasR`.
- Removed the separate v07 broader-method-comparison section. Level 1 now
  focuses on `adjusted_vs_benchmark`, with unadjusted raw MPD represented once
  as the raw-baseline method row. Level 1 uses a broader selected comparison
  set, while Levels 2-5 focus on inverse penetration, coefficient adjustment,
  Bayesian gravity plus education, and unadjusted raw MPD.
- Replaced the validation vignette summary with a recommendation section that
  frames method choice around validation sequence, data availability,
  measurement precision, and time constraints.
- Added method-sorting controls to validation comparison plots and ensured
  integrated raw-MPD benchmark comparisons display the unadjusted raw MPD
  baseline only once, pinned to the bottom or right edge. LISA maps and
  pairwise distribution heatmap facets now also honour method sorting.
- Revised validation scatterplots to use 2x2 square-panel facet grids and
  simpler axis labels that name adjusted, benchmark, or raw flows in people.
- Added prototype `validate_flow_plot_*()` functions for validation metric
  matrices, residual violin plots, pairwise flow scatterplots,
  standard-deviation and quantile residual-band stacked bars, distributional
  allocation heatmaps, residual-structure diagnostics, and optional LISA cluster
  maps from user-supplied `sf` boundaries. The functions now use the shared
  flow-comparison convention, default to `adjusted_vs_benchmark`, and expose
  `error_measures`, `comparisons`, and `methods` selectors for visual
  iteration. Older `plot_validation_*()` and `plot_validate_flow_*()` names
  have been replaced so exported validation functions consistently start with
  `validate_`.
- Added a concise metadata description to the validation vignette so article
  listings and previews describe the full validation workflow.
- Kept the optional flow-visualisation vignette source available for later
  revision, but hid it from the public pkgdown navigation and article index.
- Added a distinct Level 5 spatial/residual structure diagnostics section to
  the validation vignette, demonstrating `validate_flow_residual_structure()`
  with residual-versus-benchmark-flow correlation, optional Moran's I, and
  residual-versus-covariate correlation.
- Extended `validate_flow_residual_structure()` with optional Local Moran's I
  and LISA cluster diagnostics, including permutation pseudo p-values, while
  reusing the existing user-supplied neighbour-link interface and avoiding new
  mandatory spatial dependencies.
- Clarified the validation hierarchy so distributional allocation validation
  remains focused on origin-conditioned destination-share distributions, while
  spatial/residual structure diagnostics focus on remaining
  benchmark-minus-adjusted residual patterns.
- Added a reproducible full-LAD Level 5 LISA map route to the validation
  vignette. Local Moran diagnostics use deterministic nearest-neighbour LAD
  centroid links, while LISA maps render from a cached public ONS 2021 LAD BFC
  boundary download or from a user-supplied `sf` LAD boundary file instead of
  relying on a private mounted disk. Boundary polygons outside the validation
  support are retained in the same grey as not-significant areas rather than
  dropped, while Scottish background polygons are omitted from the vignette map.

### Bayesian adjustment documentation

- Expanded the adjustment vignette advanced section so it is the practical
  user-facing guide to `adjust_multilevel_bayes()`, including the default
  coverage-offset true-flow model, the role of active-user coverage as a fixed
  observation offset, and why `F_true` is a posterior prediction rather than a
  random intercept.
- Added compact input, option, output-column, and diagnostics guides for the
  Bayesian adjustment example, including posterior median and mean output
  summaries.
- Updated the advanced Bayesian vignette to report real-data evidence using
  `adjust_multilevel_bayes()` output column names,
  `attr(result, "result_metadata")` fields, diagnostic fields, and validation
  summaries.
- Added a short S2-S4 repeated source/time callout for the approved advanced
  `observation_model = "latent_two_level"` backend and kept the advanced
  Bayesian vignette as the deeper companion reference.
- Recorded the approval boundary for the Bayesian implementation:
  `coverage_offset` is the validated default empirical LAD model variant,
  while `latent_two_level` is approved as an advanced observed-row S3/S4
  repeated-source model variant after real MPD/Census empirical validation,
  real centroid distances, prior-sensitivity checks, and confirmatory sampler
  diagnostics.
- Clarified that benchmark-trained methods such as raking, selection-rate
  calibration and coefficient calibration can score well because they use
  benchmark margins or OD targets during fitting; Bayesian coverage-offset
  models do not need benchmark OD flows and can reserve them for external
  validation, so similar benchmark performance is a meaningful result.
- Hardened the custom `stan_latent` backend with standardized design matrices,
  source/time-aware latent-unit selection, sum-to-zero latent/source/time
  contrasts, and real-data validation guards that reject synthetic distance and
  zero-filled approval evidence.

# debiasR 0.0.0.9003

### Latent Bayesian hardening

- Added an optional `latent-stress` Bayesian test-runner scope for the custom
  `stan_latent` backend. The scope fits larger S3 repeated-source and S4
  source-time complete-grid synthetic fixtures, then checks latent true-flow
  invariance, observation-scale variation, zero-filled prediction rows, and
  sampler diagnostics.
- Updated the manual Bayesian GitHub Actions workflow so maintainers can choose
  the smoke lane, the latent S3/S4 stress lane, or all optional Bayesian checks.
- Corrected the custom Stan latent true-flow intercept prior so
  `latent_intercept_prior_scale` directly controls the intercept scale rather
  than being multiplied by the coefficient prior scale.
- Kept `observation_model = "latent_two_level"` experimental pending hosted
  manual stress-lane results and empirical runtime notes.

# debiasR 0.0.0.9002

### Latent Bayesian backend

- Completed a public-repository hygiene pass covering public docs, pkgdown
  exposure, workflow deploy guards, conduct/reporting text, non-code license
  clarity, local-path cleanup, and tracked development files.
- Clarified the validation framework terminology by keeping individual
  origin-destination flow checks as Level 3 and presenting
  `validate_flow_distribution()` as Level 4 distributional allocation
  validation.
- Replaced the `observation_model = "latent_two_level"` random-intercept
  prototype with an experimental custom Stan backend selected by
  `backend = "stan_latent"` or `backend = "auto"`.
- The latent backend estimates source-invariant OD or OD-time true-flow
  intensities and models each MPD source/time row as a coverage-scaled noisy
  observation of that latent state.
- Added posterior summary columns for true-flow and MPD-scale means, medians,
  and 95% intervals while preserving the existing `flow_adj`,
  `flow_true_pred`, and `flow_mpd_pred` columns.
- Added deterministic latent-contract tests for backend routing, S1-S4 latent
  state keys, formula partitioning, Stan data construction, and clear backend
  availability errors.
- Updated the optional Bayesian test runner and workflow to include `rstan`
  for the custom latent backend.
- Added explicit latent prior and sampler controls, richer Stan diagnostics,
  generated-quantity overflow guards, and split optional Bayesian test scopes
  for `rstanarm` smoke/full checks and latent Stan checks.

# debiasR 0.0.0.9001

### Bias distribution and latent Bayesian development

- Added `measure_bias_distribution()` as exported package API for comparing
  active-user and benchmark-population spatial distributions with
  `KL(population || active users)`, Jensen-Shannon divergence, share
  differences, and area-level contribution outputs.
- Extended `validate_flow_distribution()` so method-comparison workflows can
  evaluate `raw_vs_benchmark`, `adjusted_vs_benchmark`, and
  `raw_vs_adjusted` origin-conditioned destination-share distributions with a
  shared KL/JSD output contract.
- Added an experimental `observation_model = "latent_two_level"` path to
  `adjust_multilevel_bayes()` for repeated source/time structures. The
  prototype creates `latent_flow_id` states, adds a shared latent-state random
  intercept in the Bayesian observation model, and records latent-state
  metadata and identifiability notes while preserving the frequentist engine
  for shared S1-S4 data-contract testing.
- Added a latent two-level Bayesian design note for enhancement issue #18,
  documenting the intended full model, identifiability constraints, backend
  strategy, diagnostics, and remaining hardening work.
- Updated the measuring-bias and validation vignettes to teach the new
  distributional bias helper and extended flow-distribution validator.
- Split Bayesian adjustment documentation so the main adjustment vignette
  focuses on the default coverage-offset implementation, while a new advanced
  Bayesian adjustment vignette explains the experimental latent prototype,
  reduced-form compatibility mode, S1-S4 source/time structures, and
  diagnostics.

# debiasR 0.0.0.9000

### Public documentation and governance

- Recorded that the `debiasR` repository is public on GitHub as of 2026-06-04
  and updated project context for public-repository hygiene.
- Added GitHub Pages pkgdown deployment support for the public vignette site.
- Added repository code ownership for Francisco Rowe and Carmen Cabrera.
- Updated contribution guidance so all changes to `main` go through pull
  requests with code-owner review.
- Documented public GitHub installation commands for `debiasR` and the
  empirical companion package `debiasRdata`.
- Refreshed the README and introductory vignettes `v02` through `v05` to
  clarify package setup, empirical LAD inputs, coverage-bias measurement, and
  explanatory bias diagnostics.
- Replaced the README workflow schematic with the current DEBIAS workflow image.
- Removed the redundant package-overview vignette from the pkgdown article set
  and routed the top-left site brand link back to the README-backed home page.
- Moved Reference to the final top-menu position and temporarily hid the data
  overview vignette from public pkgdown navigation while keeping its source
  available for later revision.
- Simplified introductory vignette setup chunks to load `debiasR` and
  `debiasRdata` directly for the public empirical LAD examples.
- Rewrote the validation workflow vignette to clarify how users can compare
  raw, adjusted, and benchmark OD flows with the current `validate_flow_*`
  diagnostics.
- Updated the adjustment and validation workflow vignette setup to assume the
  README GitHub installation instructions have been followed, load `debiasR`
  and `debiasRdata` directly, and keep package, helper-loading, and
  data-loading steps in separate chunks where needed.
- Hid vignette-only helper-loading code in the adjustment and validation
  workflow articles while keeping notes that explain where those helpers live
  and how source users can load them.
- Refined the validation vignette interpretation and recommendation text for
  the validation workflow.
- Improved validation vignette tables and scatterplots with smaller table text,
  clearer marginal labels, explicit x/y comparison labels, and centred
  difference colours.
- Refined validation scatterplot facet labels so OD-flow and marginal-flow
  comparisons name the vertical and horizontal datasets more compactly, while
  avoiding repeated marginal-total row labels in split marginal plots.
- Updated validation vignette diagnostic plots with larger residual-heatmap
  text.

### Multilevel scenario development

- Added a coverage-offset true-flow mode to `adjust_multilevel_bayes()`.
  `target_scale = "true_flow"` with `observation_model = "coverage_offset"`
  uses origin, destination, or geometric-mean active-user coverage as a fixed
  observation-process offset and returns `flow_adj` on the estimated true-flow
  scale, with `flow_mpd_pred` retaining the fitted MPD scale.
- Added split `mobility_formula` and `bias_formula` support to
  `adjust_multilevel_bayes()` so users can distinguish conceptual Level-2
  true-flow predictors from Level-1 MPD observation-bias terms while retaining
  the current reduced-form fitting implementation.
- Opened enhancement issue #18 for the planned genuinely latent two-level
  Bayesian model where `F_true_ij` is estimated explicitly.
- Added an S1-S4 scenario contract for `adjust_multilevel_bayes()` covering single/multiple mobile-phone-derived data sources crossed with single/multiple observation periods.
- Added `scenario`, `source_col`, `time_col`, `repeated_observation`, and `model_engine` parameters so the multilevel path can validate and carry source/time metadata without introducing separate user-facing functions.
- Added a primary `formula` interface for `adjust_multilevel_bayes()`, with arbitrary area covariates available as `{covariate}_o` and `{covariate}_d`, formula-specified random effects and slopes, and legacy `custom_formula` / `income_col` compatibility retained.
- Added a frequentist development engine for fast testing of the shared multilevel data contract, complete-grid semantics, and bias-removal algebra; use this engine for quick experimentation and runtime-sensitive method comparison.
- Added `model_terms` metadata to the multilevel result contract so the resolved default fixed-effect and random-effect structure can be inspected directly.
- Enabled the Bayesian engine for S2-S4 repeated source/time scenarios, so the same `scenario`, `source_col`, `time_col`, and `repeated_observation` contract now works with either `model_engine = "bayesian"` or `model_engine = "frequentist"`. Bayesian smoke tests now check the S2-S4 formula contract and an S4 Bayesian fit with source/time-specific coverage offsets.
- Moved `rstanarm` into the default package imports so the standard Bayesian
  backend is installed with `debiasR`, and simplified the adjustment vignette
  so the Bayesian example runs directly.
- Added a Bayesian vignette result table and regeneration script for posterior
  median and mean example output.
- Expanded fast tests with MSOA-like S1-S4 fixtures that exercise the default frequentist formula contract and complete-grid prediction metadata.
- Updated workshop/testing vignettes to show a Bayesian coverage-offset example, raw/adjusted/benchmark comparison columns, active-user coverage notation, and parameter guidance for S1-S4 repeated source/time structures.

### Empirical LAD travel-to-work examples

- Added `debiasR_example_data()` to load and normalise `debiasRdata` LAD travel-to-work inputs into the package `origin`, `destination`, `flow` schema.
- Added optional complete-grid output to `debiasR_example_data()`, including zero-filled absent OD pairs, source row-status indicators, and an OD audit for strict square support.
- Added Census 2021 `ODWP01EW` workplace-flow extraction scripts for the benchmark travel-to-work OD matrices.
- Updated examples to use the optional companion package `de-bias/debiasRdata`, with `lad_OD_travel2work` as the default observed OD matrix and `census_lad_OD_travel2work` as the Census benchmark, while retaining MSOA access through `geography = "msoa"` and `simulated_*` datasets as lightweight test fixtures.
- Added selected-area LAD distance derivation from real `debiasRdata::lad_centroids`, avoiding a packaged full OD distance matrix.
- Removed legacy raw calibration CSVs from the main repository so public data
  assets are served through the audited `debiasRdata` package and lightweight
  simulated fixtures remain prebuilt in `debiasR`.

### Bayesian complete-grid prediction

- Extended `adjust_multilevel_bayes()` with `prediction_scope = "complete_grid"` for supplied square OD matrices.
- Complete-grid mode validates OD support, fits on originally observed MPD rows when `mpd_observed` is available, predicts across the supplied grid, and returns row-status and runtime metadata.
- Refreshed the training vignettes to centre the Bayesian multilevel adjustment path while keeping deterministic methods as transparent baselines.

### Stage 3 measure-bias diagnostics

- Added `validate_bias_residual_structure()` for active-user coverage residual diagnostics linked to `measure_bias()`.
- The helper returns coverage-score, count-scale, standardized count, and population-only linear-model residuals, with optional Moran's I, benchmark origin/destination flow correlations, covariate correlations, map-ready area data, and optional `ggplot2` diagnostics.
- Added a Stage 3 design note and review notebook for inspecting the implemented residual definitions and diagnostics on deterministic fixture data.

### Stage 2 validation layer

- Added richer residual comparison outputs to `validate_flow_residuals()`, including method labels, the Stage 2 signed residual-movement indicator, absolute residual reduction, raw-versus-adjusted residual outlier shares, and direction-of-benchmark movement flags.
- Added `validate_flow_residual_structure()` for residual randomness diagnostics, including benchmark-flow correlation, optional Moran's I from user-supplied neighbour links, optional covariate correlation, map-ready area residuals, and optional `ggplot2` diagnostic plots.
- Added `validate_flow_distribution()` for origin-conditioned destination-share fidelity using `KL(benchmark || adjusted)` and Jensen-Shannon divergence.

### Validation naming update

- The primary validation API now uses `validate_flow_overall()` for summary metrics and `validate_flow_pairs()` for row-level comparisons.
- The older names `validate_flow_benchmark()` and `validate_flow_all()` remain available as backwards-compatible aliases for one release cycle.
- The legacy `validate_flows()` helper has been removed.

### Migration notes

- Package documentation and onboarding now refer to `adjust_*` functions and the current example-data workflow consistently.
- `adjust_multilevel_bayes()` is the main methodological innovation and now has observed and complete-grid prediction scopes; empirical Bayesian rendering still requires runtime and real-distance validation.
