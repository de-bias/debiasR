# Task Board

Last updated: 2026-06-25

This board turns the current roadmap into a short execution plan. Estimated effort is in rough person-hours.

The staged track below is intended to be implemented one stage per chat window. Do not start the next stage until the current stage deliverables and decision notes have been reviewed.

## Now

1. Confirm optional Bayesian CI workflow - `1-2h`
- Enhancement issue #18 now has a design note and an approved advanced custom Stan
  backend for `observation_model = "latent_two_level"` in
  `adjust_multilevel_bayes()`.
- The backend creates `latent_flow_id` states, estimates source-invariant OD or
  OD-time true-flow intensities, and models MPD source/time rows as
  coverage-scaled noisy observations of those latent states.
- The branch exposes latent prior and sampler controls, records richer sampler
  diagnostics, uses identified sum-to-zero latent/source/time contrasts, and
  splits optional Bayesian validation into standard `rstanarm-smoke`, full
  `rstanarm`, and latent-Stan scopes.
- Local tests and real-data empirical validation now support approving
  `latent_two_level` for observed-row repeated-source S3/S4 workflows. The
  remaining workflow check is the hosted/manual GitHub Actions latent lane.
- Local source/time flow data for empirical S1-S4 validation are now available
  outside the repository at `/Volumes/DEBIAS/data/outputs/flows`. The first
  route is the HTW branch, using `mapp1`, `mapp2`, and Census benchmark files
  at LAD/LTLA and MSOA support. Do not commit the raw flow files or bulky
  rendered outputs into `debiasR`.

2. Validate optional Bayesian CI workflow - `1-2h`
- Fast core GitHub Actions validation passed on merged PR #11.
- Current branch fast core tests pass locally.
- Local Bayesian smoke checks use the default `rstanarm` backend; the remaining workflow check is the manual/optional GitHub Actions lane when broader Bayesian validation is required.
- Confirm the optional/manual Bayesian lane on GitHub Actions when Bayesian-lane validation is required; maintainers can now select `smoke`, `latent-stress`, or `all` from the manual workflow.
- The Stage-1 Bayesian implementation now supports S1-S4 source/time scenarios;
  the frequentist engine remains available for fast testing and experimentation.
- The default LAD empirical route now has selected-area distance support through
  `debiasRdata::lad_centroids`.
- The default `coverage_offset` Bayesian route is now empirically approved for
  observed-flow LAD S1-S4 workflows; `latent_two_level` is approved as an
  advanced observed-row S3/S4 repeated-source route with real-data and
  diagnostic guardrails.

## Recently Completed

1. Approve latent two-level Bayesian repeated-source implementation - `complete`
- Completed on 2026-06-25.
- The empirical latent approval route used real HTW MPD source/time files under
  `/Volumes/DEBIAS/data/outputs/flows`, real Census benchmark files, and real
  `debiasRdata::lad_centroids` OD distances. Synthetic distance fallback,
  complete-grid MPD zero-fill rows, and zero-filled benchmark rows are rejected
  for approval evidence.
- Exploratory prior sensitivity fitted S3 and S4 repeated-source LAD subsets
  with default, tighter, and wider latent priors. Confirmatory default-prior
  fits used `iter = 1000`, `chains = 4`, and `latent_max_treedepth = 15`.
- Result: S3 and S4 confirmatory latent fits completed with no failures, no
  divergences, no treedepth hits, E-BFMI above 0.91, max R-hat about 1.023, and
  minimum effective sample size about 190.
- Decision: approve `observation_model = "latent_two_level"` as an advanced
  Bayesian alternative for observed-row repeated-source S3/S4 workflows. It is
  not the default route; users should still start with `coverage_offset` unless
  they need a latent source-invariant true-flow state.

1. Approve default coverage-offset Bayesian empirical implementation - `complete`
- Completed on 2026-06-25.
- The full real LAD S4 validation route fitted both fixed and origin
  random-intercept `observation_model = "coverage_offset"` Bayesian models with
  real `debiasRdata::lad_centroids` distances, 309 LADs, 74,874 MPD rows,
  64,162 validation pairs, `iter = 1000`, and `chains = 4`.
- Result: both Bayesian models completed with no failures, max R-hat about
  1.01, minimum effective sample size 485, and no R-hat, ESS, or
  non-convergence warnings.
- Decision: approve the default `coverage_offset` implementation as a viable
  empirical alternative for observed-flow LAD S1-S4 workflows, especially when
  benchmark OD flows are unavailable for fitting or reserved for validation.
- Scope: this approval covers the default `coverage_offset` route. The separate
  latent repeated-source approval is recorded above.
- Documentation updated to frame Bayesian validation as external validation
  against benchmark-assisted methods rather than a pure in-sample MAE contest.

1. Add validation visual prototype functions - `complete`
- Completed on 2026-06-13.
- Added exported prototype `plot_validation_*()` functions for overall metric
  matrices, residual violin plots, pairwise flow scatterplots,
  standard-deviation and quantile residual outlier stacked bars,
  distributional allocation heatmaps, pairwise divergence matrices, and
  residual-structure diagnostic summaries, plus optional LISA cluster maps from
  user-supplied `sf` boundaries.
- The visual functions use the shared flow-comparison convention, default to
  `adjusted_vs_benchmark`, and expose `error_measures`, `comparisons`, and
  `methods` selectors.
- Added a short visual review notebook at
  `notes/project-management/VALIDATION_VISUAL_PROTOTYPES.qmd` using
  deterministic simulated package data so the chart designs can be iterated
  before promotion into the public validation vignette.
- Decision recorded on 2026-06-19: the `plot_validation_*()` functions will be
  incorporated into the existing validation vignette. There will not be a
  separate validation-visualisation vignette; the visual prototype notebook and
  rendered notes are internal design/review material.

1. Add Local Moran/LISA residual diagnostics - `complete`
- Completed on 2026-06-13.
- `validate_flow_residual_structure()` now has optional Local Moran's I and
  LISA cluster diagnostics for area-level residuals using the existing
  user-supplied neighbour-link interface.
- The validator keeps dependencies light by using base-R permutation pseudo
  p-values and avoiding mandatory `sf`, `spdep`, or cartographic dependencies.
- The validation vignette keeps this as part of Level 5 spatial/residual
  structure diagnostics, not a new validation level.

1. Add spatial/residual structure diagnostics to the validation vignette - `complete`
- Completed on 2026-06-13 for issue #56.
- The validation vignette now presents `validate_flow_residual_structure()` as
  a distinct Level 5 diagnostic layer after distributional allocation
  validation.
- The example demonstrates residual-versus-benchmark-flow correlation,
  optional Moran's I from a user-supplied neighbour-link table, and
  residual-versus-covariate correlation, while keeping illustrative neighbour
  setup hidden from the teaching flow.
- Level 4 remains framed as origin-conditioned destination-share allocation
  validation rather than individual OD-flow magnitude validation.

1. Document Bayesian adjustment options in the adjustment vignette - `complete`
- Completed on 2026-06-13 for issue #58.
- The advanced section of `vignettes/v06-adjusting-biases.qmd` is now the
  practical user-facing guide to `adjust_multilevel_bayes()`, with compact
  guides for option choice, required inputs, returned output columns and
  diagnostics.
- The section explains the default coverage-offset true-flow model, including
  why active-user coverage enters as a fixed observation offset and why the
  estimated true flow is a posterior prediction rather than a random intercept.
- The vignette keeps the S1 Bayesian example precomputed so routine renders do
  not rerun MCMC, and it points readers to the advanced Bayesian adjustment
  vignette for deeper S2-S4, reduced-form and latent-backend details.
- Follow-up on 2026-06-25: the main adjustment vignette now avoids describing
  all Bayesian variants as a generic two-level model. It scopes the MPD
  observation equation and true-flow prediction equation to the
  coverage-offset route only, keeps the coefficient-regression distinction in a
  Quarto callout, and leaves the advanced Bayesian vignette to explain
  `coverage_offset`, `reduced_form`, and `latent_two_level` separately.

1. Post-public repository hygiene pass - `complete`
- Completed on 2026-06-12.
- Reviewed public-facing docs, pkgdown article exposure, repository metadata,
  GitHub workflows, branch-protection metadata, tracked assets, and obvious
  sensitive-content patterns.
- Removed public exposure of a workshop planning `.docx` with embedded Word
  comments/people metadata, cleaned absolute local links from tracked notes,
  updated GitHub-owned Actions to current tags, restricted manual pkgdown
  deploys to `main`, clarified code-of-conduct reporting, added explicit
  CC BY 4.0 non-code license text, and removed stale/duplicated public docs
  scaffolding.
- Confirmed branch protection requires the fast-tests and pkgdown checks on
  `main`, with direct updates limited to Francisco Rowe and Carmen Cabrera.

1. Implement distributional bias API and latent prototype scaffolding - `complete`
- Completed on 2026-06-12 for the current development branch.
- `measure_bias_distribution()` is now exported package API for
  active-user-versus-population distributional bias.
- `validate_flow_distribution()` now supports raw-versus-benchmark,
  adjusted-versus-benchmark, and raw-versus-adjusted comparisons through the
  same origin-conditioned KL/JSD contract.
- `adjust_multilevel_bayes()` then exposed the experimental
  `observation_model = "latent_two_level"` option for repeated source/time
  structures while preserving the frequentist engine for shared S1-S4 data
  contract testing.
- This opened the path for issue #18. The follow-up `0.0.0.9002` work adds the
  first custom Stan latent backend; later 2026-06-25 work approves the
  repeated-source S3/S4 route with real-data and diagnostic guardrails.
- Vignettes and project notes were updated to explain the new package-level
  functions and the latent backend status. The main adjustment vignette now
  teaches the default coverage-offset Bayesian implementation, while the
  advanced Bayesian adjustment vignette carries the latent backend,
  reduced-form compatibility, S1-S4 source/time, and diagnostics material.

1. Make repository public on GitHub - `complete`
- Completed on 2026-06-04.
- Public-release governance, contribution guidance, and pkgdown deployment
  scaffolding are now part of the active project context.
- Legacy raw calibration CSVs are not distributed from `debiasR`; public
  empirical examples use the audited `debiasRdata` companion package.

2. Review Stage 2 validation deliverables - `complete`
- Maintainer review completed on 2026-05-08.
- `validate_flow_residual_structure()` is stable public API.
- Optional validation plots remain inside helpers for now.
- `debiasRdata` is the implemented empirical data route:
  <https://github.com/de-bias/debiasRdata>.

3. Close remaining package-readiness warnings - `complete`
- Long generated notebook paths were removed from tracking; non-standard project folders remain excluded from package builds through `.Rbuildignore`.
- Bayesian NSE warnings were removed by tightening tidyselect/tidy-evaluation expressions.
- The Bayesian draw-summary names mismatch in the optional test file was fixed.
- `debiasRdata` is declared in `Suggests`, closing the conditional-example unstated-dependency warning.
- Package-readiness check with tests/vignettes/manual skipped now has 0 errors and 0 warnings.

4. Keep Bayesian scope aligned - `complete`
- `adjust_multilevel_bayes()` is documented as the main methodological innovation.
- Observed-flow mode remains backward compatible.
- Complete-grid prediction mode is available for strict square OD matrices and preserves row-status metadata.
- Full empirical Bayesian rendering remains gated by runtime and empirical validation.

5. Create `debiasRdata` companion package - `complete`
- Repository: <https://github.com/de-bias/debiasRdata>.
- Included data objects: `msoa_OD_travel2work`,
  `census_msoa_OD_travel2work`, `lad_OD_travel2work`,
  `census_lad_OD_travel2work`, and `lad_centroids`.
- License and source metadata live in the companion package.
- `debiasR::debiasR_example_data(n_areas = 5)` was smoke-tested against the
  local sibling `debiasRdata` checkout on 2026-05-18.
- `debiasR::debiasR_example_data(n_areas = 5, complete_grid = TRUE)` now
  defaults to LAD and derives selected-area distances from `lad_centroids`.

## Later

1. Harden the Bayesian path further - `1-2 days`
- Validate complete-grid Bayesian prediction on real `debiasRdata` OD inputs.
- Keep the LAD coverage-offset route as the approved default empirical path;
  add MSOA distance assets only if MSOA-specific empirical Bayesian examples are
  needed.
- Record feasible empirical grid sizes and runtime expectations for MSOA and
  latent-backend workflows.
- Keep the Bayesian tests in a clear optional CI lane if the scope expands.

2. Prepare a release-ready maintenance pass - `1-2 days`
- Re-run the full package check after the CI and migration work settle.
- Review examples and vignettes for remaining dependency friction.
- Decide whether a tagged pre-release makes sense after stabilization.

## Staged Implementation Track

This section is the working implementation plan for the next feature stages. Each stage is scoped so it can be handled in a separate chat window.

### Stage 2: Validation

Goal: extend validation beyond overall fit so we can compare methods on residual reduction, outlier behavior, and residual structure.

Estimated effort: `2-4 days`

Status: `complete; maintainer reviewed`

Tasks:

1. Define the core validation targets.
- Implemented geographic benchmark-adjusted flow correlation through `validate_flow_overall()`.
- Implemented residual-reduction diagnostics in `validate_flow_residuals()`, including the exact signed Stage 2 movement indicator and an absolute residual-reduction metric where positive values mean less benchmark error.
- Implemented MPD and adjusted residual outlier shares above 1, 2, and 3 standard deviations.
- Implemented distributional allocation fidelity in `validate_flow_distribution()` using origin-conditioned KL divergence and Jensen-Shannon divergence.

2. Implement a residual-reduction indicator for method comparison.
- Implemented the OD-level signed movement indicator as:
  `(benchmark_od_flow - observed_mpd_od_flow) - (benchmark_od_flow - adjusted_mpd_od_flow)`.
- Decision: this algebraic indicator equals `adjusted - observed_mpd`, so positive means upward adjustment. The package also returns `abs_residual_reduction`, where positive means reduced benchmark error, for method comparison.
- Method identifiers are carried in residual summaries, OD-level residual data, and top-worst residual rows.
- Summaries include mean, median, share improved/worsened/unchanged, direction-of-benchmark movement share, and optional distribution plots.

3. Add residual outlier diagnostics.
- Implemented residual shares above 1, 2, and 3 standard deviations.
- Decision: report both raw MPD residuals and adjusted residuals.
- Added outlier-reduction summaries, including reduction in the share above 2 standard deviations.

4. Add residual randomness diagnostics.
- Implemented `validate_flow_residual_structure()` with optional global Moran's I from user-supplied neighbour links.
- Implemented area-level residual `map_data`; optional coordinate-based residual map plots are returned when coordinates and `ggplot2` are available.
- Implemented residual-versus-benchmark-flow Pearson correlation and optional scatter plot.
- Implemented residual-versus-user-selected-covariate Pearson correlation and optional scatter plot.
- Decision: covariates are passed as a plain area-level data frame plus explicit area and covariate column names.
- Maintainer review decision: treat `validate_flow_residual_structure()` as stable public API immediately.
- Maintainer review decision: keep optional diagnostic plots inside the helper for now because they are dependency-light and useful for review; split plotting into separate helpers later only if the plotting surface grows. The current visual prototype now begins that split with package-level `plot_validation_*()` functions.
- Updated visual-prototype decision: expose LISA cluster mapping separately through `plot_validation_lisa_map()`, requiring users to provide `sf` boundaries rather than bundling or inferring geometries.

5. Add distributional allocation diagnostics.
- Implemented destination-share distributions by origin for benchmark and adjusted flows.
- Implemented dedicated helper `validate_flow_distribution()`.
- Implemented `KL(benchmark || adjusted)` as the directional allocation-fidelity metric.
- Implemented Jensen-Shannon divergence as the symmetric companion metric.
- Decision: use union support, configurable positive smoothing with default `epsilon = 1e-8`, origin-level metrics, and optional benchmark-origin-total weighted summaries.
- Classification update: treat this as Level 4 distributional allocation
  validation in the public validation framework, while keeping Level 3 focused
  on individual OD-pair flow magnitudes, residuals, and outliers.

6. Explore a method-assessment penalty indicator.
- Decision: keep this out of the package API for now.
- Rationale: a penalty for calibration inputs is a research-design judgement, not a validation property of an adjusted OD table. It belongs in method-comparison notes until a defensible scoring framework is agreed.

7. Resolve the data and redistribution gate.
- Documented in `DATA_REDISTRIBUTION_DECISION.md`.
- Confirmed the Zenodo record is licensed CC BY 4.0, which permits redistribution with attribution, but the full record is too large for the main package.
- Confirmed `msoa_OD_travel2work.csv.gz` is the source MPD empirical asset. For
  `debiasR`, the default user-facing route is the LAD aggregate
  `lad_OD_travel2work` paired with `census_lad_OD_travel2work`.
- Recommended CRAN-safe option: a separate optional `debiasRdata` package licensed for the data, with `debiasR` using `Suggests`, `requireNamespace()`, `system.file()`, and conditional examples/tests/vignettes.
- Updated direction for `debiasR`: keep simulated/tiny data as lightweight test fixtures, but base user-facing examples on the optional `debiasRdata` LAD travel-to-work workflow.
- Maintainer review decision: accept the optional `debiasRdata` strategy and do not bundle the full Zenodo record in `debiasR`.
- Implementation update: `debiasRdata` now exists at
  <https://github.com/de-bias/debiasRdata> and supplies
  MSOA and LAD travel-to-work assets plus `lad_centroids`.

Deliverables:

- [x] a written validation design note
- [x] an implementation spec for `validate_flow_distribution()`
- [x] a method-comparison residual indicator with explicit interpretation
- [x] residual outlier summaries
- [x] residual randomness diagnostics and plots
- [x] origin-level and summary distributional allocation metrics based on KL divergence and Jensen-Shannon divergence
- [x] a written decision on data redistribution and CRAN suitability
- [x] a written recommendation on whether to use a separate `debiasRdata` package for `msoa_OD_travel2work.csv.gz`
- [x] a Stage 2 implementation report
- [x] a runnable Stage 2 validation review notebook

Decision gate:

- Do we have a validation layer that compares methods on fit, residual structure, and spatial allocation fidelity, and do we know whether the Zenodo-based data can be used in-package?
- Stage 2 answer: yes, maintainer reviewed on 2026-05-08. The implementation covers fit, residual behavior, residual structure, allocation fidelity, and the Zenodo redistribution/data-package decision. `validate_flow_residual_structure()` is stable public API; the current visual prototype adds separate package-level plotting functions, including optional LISA mapping from user-supplied `sf` boundaries; empirical examples use the optional `debiasRdata` package rather than bundling Zenodo data in `debiasR`.

### Stage 3: Measure Bias

Goal: extend the bias-measurement layer with residual randomness diagnostics linking benchmark population and active-user structure.

Estimated effort: `1-2 days`

Status: `complete; maintainer reviewed`

Tasks:

1. Write the Stage 3 design note before implementation.
- Implemented in `STAGE3_MEASURE_BIAS_DESIGN_NOTE.md`.
- Defined the active-user coverage residual as:
  - `global_coverage_score = sum(user_count) / sum(population)`
  - `expected_user_count = population * global_coverage_score`
  - `user_count_residual = user_count - expected_user_count`
  - `coverage_score_residual = coverage_score - global_coverage_score`
  - `standardized_user_count_residual = user_count_residual / sqrt(expected_user_count)`
  - `population_lm_residual = user_count - fitted(user_count ~ population)`
- Decision: use `coverage_score_residual` as the default diagnostic residual because it is scale-free and has a direct active-user coverage interpretation.
- Decision: implement a separate helper, `validate_bias_residual_structure()`, rather than expanding `measure_bias()`.
- Maintainer review decision: treat `validate_bias_residual_structure()` as stable public API immediately.
- Maintainer review decision: add a simple population-only linear-regression residual option. It fits `user_count ~ population` using the benchmark population column and active-user counts in `coverage_df`, then reports observed minus fitted active-user counts. This is a descriptive diagnostic, not a validated active-user sampling model.
- Maintainer review decision: keep optional `ggplot2` plots inside the diagnostics helper for now because they are optional, dependency-light, and useful for review. Split plotting into separate helpers later only if the plotting surface grows.

2. Add spatial randomness diagnostics.
- Implemented optional global Moran's I from a user-supplied neighbour-link table.
- Reused the Stage 2 neighbour-link interface.
- Returned area-level and map-ready residual data.
- Kept cartographic rendering optional and dependency-light with coordinate-based `ggplot2` output only when requested.

3. Relate bias residuals to benchmark OD flows.
- Implemented benchmark origin-total and destination-total diagnostics as separate outputs.
- Computed Pearson correlations between the selected bias residual and each requested benchmark area-flow total.
- Returned scatter-plot-ready data and optional `ggplot2` scatter plots.

4. Relate bias residuals to a user-selected covariate.
- Mirrored the Stage 2 covariate interface.
- Accepted an area-level covariate table plus explicit area and covariate column names.
- Computed Pearson correlation between the selected bias residual and covariate.
- Returned scatter-plot-ready data and optional `ggplot2` scatter plots.

5. Add tests and documentation.
- Added focused tests in `tests/testthat/test-validate-bias-residual-structure.R`.
- Updated `scripts/run_fast_tests.R`.
- Generated `man/validate_bias_residual_structure.Rd` and updated `NAMESPACE`.
- Updated README, NEWS, status notes, test health notes, vignettes, and workflow diagrams.
- Added `STAGE3_IMPLEMENTATION_REPORT.md`.
- Added and rendered `STAGE3_MEASURE_BIAS_REVIEW_NOTEBOOK.qmd`.

Deliverables:

- [x] a Stage 3 design note with a clear residual definition for `measure_bias()`-related diagnostics
- [x] a bias residual diagnostics helper or a documented extension to `measure_bias()`
- [x] spatial randomness summary and map-ready area data
- [x] benchmark-flow and covariate correlation diagnostics
- [x] focused tests and generated documentation
- [x] a Stage 3 implementation report
- [x] a runnable Stage 3 measure-bias review notebook

Decision gate:

- Are the new bias diagnostics interpretable enough to keep in the main package API rather than only in analysis notes?
- Stage 3 answer: yes, maintainer reviewed on 2026-05-05. The implementation uses reproducible coverage residuals directly linked to `measure_bias()`, includes a simple population-only linear-model residual for diagnostics, reuses the Stage 2 diagnostic interfaces, and keeps optional plotting dependency-light.

### Stage 4: Multilevel Model Scenario Extension

Goal: extend `adjust_multilevel_bayes()` so one Bayesian path can support four
mobile-phone-derived input scenarios.

Estimated effort: `3-5 days`

Status: `in progress; Bayesian S1-S4 transfer implemented, empirical HTW validation and LAD teaching examples remain`

Scenario plan:

- See [MULTILEVEL_MODEL_SCENARIO_PLAN.md](MULTILEVEL_MODEL_SCENARIO_PLAN.md).
- S1: single source, single time.
- S2: single source, multiple times.
- S3: multiple sources, single time.
- S4: multiple sources, multiple times.

Working recommendation:

- Add scenario support through parameters in `adjust_multilevel_bayes()`, not
  separate exported functions.
- Use MSOA-scale data for software development and internal stress testing.
- Use LAD-scale data for vignettes and teaching materials.
- Use `model_engine = "frequentist"` as the fast scaffold for formula,
  data-shape, runtime checks, experimentation, and method comparison.
- Use `model_engine = "bayesian"` for the Stage-1 posterior workflow across
  S1-S4 scenarios when optional Bayesian dependencies and runtime budgets are
  available.

Software-development tasks:

1. Define the scenario parameters.
- Decide whether users pass an explicit `scenario` argument, source/time column
  arguments, or both.
- Validate that S1-S4 are distinguishable from the supplied columns.
- Keep existing observed-flow and complete-grid behavior backward compatible.
- Implementation update: `scenario`, `source_col`, `time_col`,
  `repeated_observation`, and `model_engine` are now parameters on
  `adjust_multilevel_bayes()`. The shared scenario contract now works with
  both `model_engine = "bayesian"` and `model_engine = "frequentist"`.

2. Build the internal modelling scaffold.
- Prototype formula and random-effect structures with the faster frequentist
  implementation when runtime matters.
- Keep the selected structure shared by the Bayesian and frequentist backends.
- Keep the frequentist path as a development engine option, not a separate
  exported adjustment function.
- Implementation update: the default S1-S4 formula contract is now
  recorded in `MULTILEVEL_MODEL_SCENARIO_PLAN.md` and returned in
  `model_terms` metadata. S1 has no source/time term; S2 adds `mpd_time`; S3
  adds `mpd_source`; S4 adds `mpd_source + mpd_time`.

3. Test against MSOA-like and empirical inputs.
- Use synthetic MSOA-like data for internal software tests and runtime checks
  because it is the stricter shape for grid size and repeated observations.
- Use `/Volumes/DEBIAS/data/outputs/flows/htw` for the empirical S1-S4
  validation track. Start with LAD/LTLA support for quicker manual runs, then
  repeat stable specifications at MSOA support.
- Treat the migration/Twitter files under `mig` as secondary until the
  code/label harmonisation and geography support have been audited.
- Add focused tests for scenario detection, required columns, output metadata,
  and compatibility with existing Bayesian prediction scopes.
- Implementation update: the fast tier includes deterministic MSOA-like S1-S4
  fixtures for default frequentist formula terms, metadata, and S4
  complete-grid prediction. Optional Bayesian tests cover repeated S2-S4
  source/time fitting with the rstanarm backend when installed, and the
  `latent-stress` scope covers synthetic S3/S4 `stan_latent` stress fixtures.
- Next empirical step: build a local schema audit/normalisation notebook or
  script that maps the HTW files to `origin`, `destination`, `flow`,
  `mpd_source`, `mpd_time`, benchmark flow, geography, and coverage/covariate
  fields without copying raw data into the repository.

Vignette and teaching-material tasks:

1. Design LAD-scale examples.
- Use LAD data for user-facing vignettes because it is easier to render,
  explain, and teach.
- Keep examples small enough for optional Bayesian dependencies and vignette
  rendering constraints.
- Implementation update: the adjustment vignette now uses a Bayesian
  coverage-offset S1 example with one source and one time unit, and describes
  the parameter switches for S2-S4. Full LAD-scale teaching examples for each
  separate scenario remain to be completed.

2. Teach the four scenarios separately.
- Start with S1 as the baseline.
- Introduce time effects with S2, source effects with S3, and the combined
  source-time setting with S4.
- Make clear that the frequentist engine is for fast iteration/comparison and
  the Bayesian engine is the posterior inferential method.

Deliverables:

- [x] a written multilevel model scenario plan
- [x] scenario parameters for `adjust_multilevel_bayes()`
- [x] internal MSOA-like software tests for S1-S4 data contracts
- [ ] empirical S1-S4 schema audit for `/Volumes/DEBIAS/data/outputs/flows/htw`
- [ ] local/manual empirical S1-S4 validation notebook or script covering
  frequentist smoke fits, Bayesian pilots, runtime logging, and validation
  plots
- [ ] runtime and diagnostic summary for coverage-offset and latent two-level
  Bayesian pilots across the available S1-S4 structures
- [ ] LAD vignette examples for the teachable scenario path
- [x] a documented decision on when the frequentist development engine is ready to
  transfer into the Bayesian implementation

Decision gate:

- Do the S1-S4 parameters make the Bayesian path flexible enough for mobile
  phone-derived inputs without turning `adjust_multilevel_bayes()` into an
  opaque general modelling wrapper?
