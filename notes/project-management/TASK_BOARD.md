# Task Board

Last updated: 2026-04-20

This board turns the current roadmap into a short execution plan. Estimated effort is in rough person-hours.

The staged track below is intended to be implemented one stage per chat window. Do not start the next stage until the current stage deliverables and decision notes have been reviewed.

## Now

1. Lock down testing and CI - `4-6h` - `initial scaffolding implemented`
- Added a fast deterministic test runner script.
- Added a required PR workflow for the fast tier.
- Added an optional/manual workflow for Bayesian checks.
- Updated the documented test command to match the workflow.

2. Finish migration cleanup - `3-5h` - `partially implemented`
- Swept for remaining user-facing `method*`, `validate_flows`, or `toy_*` references.
- Cleaned stale scaffold leftovers in docs and vignettes.
- Updated the migration map to reflect the current package surface.
- Remaining step: validate the new workflows in GitHub Actions and fold any last migration follow-ups back into the docs.

## Next

1. Clarify Bayesian scope - `2-3h`
- Keep `adjust_multilevel_bayes()` explicitly marked as a stage-1 prototype.
- Document the supported backends and the stage-2 imputation gap.
- Make sure the status notes and project brief say the same thing.

2. Close the documentation loop - `2-4h`
- Update README/NEWS only if a final wording mismatch remains.
- Tighten any issue notes that are now stale after the migration.
- Make the task/status docs the single source of truth for current work.

## Later

1. Harden the Bayesian path - `1-2 days`
- Decide whether the prototype should be promoted beyond stage 1.
- Add stronger validation and dependency handling if that happens.
- Split the Bayesian tests into a clear optional CI lane if the scope expands.

2. Prepare a release-ready maintenance pass - `1-2 days`
- Re-run the full package check after the CI and migration work settle.
- Review examples and vignettes for remaining dependency friction.
- Decide whether a tagged pre-release makes sense after stabilization.

## Staged Implementation Track

This section is the working implementation plan for the next feature stages. Each stage is scoped so it can be handled in a separate chat window.

### Stage 2: Validation

Goal: extend validation beyond overall fit so we can compare methods on residual reduction, outlier behavior, and residual structure.

Estimated effort: `2-4 days`

Status: `planned`

Tasks:

1. Define the core validation targets.
- Geographic correlation between benchmark and adjusted flows.
- A residual-reduction indicator that compares benchmark-versus-MPD residuals to benchmark-versus-adjusted residuals.
- Residual outlier share above 2 standard deviations.
- Distributional allocation fidelity for benchmark-versus-model destination shares by origin, using KL divergence and Jensen-Shannon divergence.

2. Implement a residual-reduction indicator for method comparison.
- Define the OD-level indicator as:
  `(benchmark_od_flow - observed_mpd_od_flow) - (benchmark_od_flow - adjusted_mpd_od_flow)`.
- Confirm sign convention and interpretation so positive values clearly mean reduced bias.
- Ensure the output carries the adjustment method identifier so results can be compared across methods.
- Decide which summaries should be returned by method: mean, median, share improved, and distribution plots.

3. Add residual outlier diagnostics.
- Compute the share of residuals above 2 standard deviations.
- Decide whether this should be reported for both raw residuals and adjusted residuals.
- Add method-comparison summaries so outlier reduction can be assessed across methods.

4. Add residual randomness diagnostics.
- Global spatial autocorrelation for residuals, for example Moran's I.
- Residual map output plus a summary statistic.
- Correlation between residuals and benchmark OD flows, including scatter plot and Pearson correlation.
- Correlation between residuals and a user-selected covariate, including scatter plot and Pearson correlation.
- Decide how covariates should be passed in so the interface remains transparent.

5. Add distributional allocation diagnostics.
- Compute destination-share distributions by origin for both benchmark and adjusted flows.
- Draft and implement a dedicated helper, `validate_flow_distribution()`.
- Add `KL(benchmark || model)` as a directional allocation-fidelity metric.
- Add Jensen-Shannon divergence as a symmetric and more stable companion metric.
- Decide on support definition, zero smoothing, and origin-level versus weighted aggregate summaries.

6. Explore a method-assessment penalty indicator.
- Draft a candidate metric that penalizes methods that require MPD flows as direct calibration inputs.
- Clarify the conceptual rationale before coding so the metric is defensible rather than arbitrary.
- Decide whether this belongs in-package or only in research notes.

7. Resolve the data and redistribution gate.
- Confirm whether the Zenodo resource at `https://doi.org/10.5281/zenodo.13327082` can be redistributed inside the R package and whether that is compatible with CRAN.
- Check license, attribution, redistribution terms, and any dataset-specific restrictions.
- Check practical packaging constraints, because the current local folder at `/Users/franciscorowe/Library/CloudStorage/Dropbox/Francisco/Research/grants/2023/digital-footprint-accelerator/debias/data/13327082` is about `179M`.
- If redistribution is allowed and package strategy is sensible, assess `/Users/franciscorowe/Library/CloudStorage/Dropbox/Francisco/Research/grants/2023/digital-footprint-accelerator/debias/data/13327082/msoa_OD_travel2work.csv.gz` as the preferred candidate data asset.
- Add an explicit CRAN-safe packaging option: create a separate small data-only package, for example `debiasRdata`, licensed `CC BY 4.0`, containing exactly `msoa_OD_travel2work.csv.gz`, while keeping `debiasR` licensed `MIT + file LICENSE`.
- If that option is chosen, design how `debiasR` would use the data package in practice, for example via `Suggests`, `system.file()`, and conditional examples/tests/vignettes.
- If redistribution is not sensible for CRAN, design an alternative: tiny packaged example plus documented external download.

Deliverables:

- a written validation design note
- an implementation spec for `validate_flow_distribution()`
- a method-comparison residual indicator with explicit interpretation
- residual outlier summaries
- residual randomness diagnostics and plots
- origin-level and summary distributional allocation metrics based on KL divergence and Jensen-Shannon divergence
- a written decision on data redistribution and CRAN suitability
- a written recommendation on whether to use a separate `debiasRdata` package for `msoa_OD_travel2work.csv.gz`

Decision gate:

- Do we have a validation layer that compares methods on fit, residual structure, and spatial allocation fidelity, and do we know whether the Zenodo-based data can be used in-package?

### Stage 3: Measure Bias

Goal: extend the bias-measurement layer with residual randomness diagnostics linking benchmark population and active-user structure.

Estimated effort: `1-2 days`

Status: `planned`

Tasks:

1. Define the residuals to analyze in the measure-bias step.
- Clarify the exact residual formulation based on benchmark population versus active users.
- Decide whether residuals should be raw, standardized, or model-based.

2. Add spatial randomness diagnostics.
- Compute global spatial autocorrelation for these residuals, for example Moran's I.
- Create a map plus a summary statistic.

3. Relate bias residuals to benchmark OD flows.
- Compute correlation between the residuals and benchmark OD flows.
- Return a scatter plot and Pearson correlation.

4. Relate bias residuals to a user-selected covariate.
- Compute correlation between the residuals and a covariate of choice.
- Return a scatter plot and Pearson correlation.
- Decide whether the covariate interface should mirror the Stage 2 validation interface.

Deliverables:

- a clearer residual definition for `measure_bias()`-related diagnostics
- spatial randomness summary and map
- benchmark-flow and covariate correlation diagnostics

Decision gate:

- Are the new bias diagnostics interpretable enough to keep in the main package API rather than only in analysis notes?

### Stage 4: Validation Modelling Extension

Goal: extend the modelling strategy to origin-destination random effects under repeated-observation settings.

Estimated effort: `3-5 days`

Status: `planned`

Working recommendation:

- Start with two separate example datasets, not one unified dataset.
- Reason: this is clearer for users, keeps the two repeated-observation assumptions explicit, makes examples easier to explain, and reduces the risk of one overloaded schema trying to cover two conceptually different designs.
- Revisit a unified internal representation only later if duplication becomes a real maintenance burden.

Tasks:

1. Write a short design note before implementation.
- Compare a two-dataset versus one-dataset approach explicitly.
- Record why the default choice is two datasets unless a strong reason emerges to unify them.
- Keep transparency and user-friendliness as the primary criteria, with computational efficiency secondary.
- Before implementation, explicitly ask which datasets should be used for Stage 4 examples and modelling tests.
- Confirm whether Stage 4 should use simulated data, empirical data, or one of each for the two formulations.

2. Formulation A: repeated observations for an OD pair from a single data source.
- Create a dataset to support this formulation.
- Add level-1 variables needed to control temporal variation.
- Decide what the minimum reproducible example should look like.

3. Formulation B: repeated observations for an OD pair from multiple data sources.
- Create a dataset to support this formulation.
- Add level-1 variables needed to control data-source variation.
- Decide what the minimum reproducible example should look like.

4. Improve modelling flexibility.
- Explore an interface that lets users define the model they want to estimate.
- Keep the modelling API transparent enough that users can see what is being fit.
- Decide how much formula freedom is realistic without making the function too opaque or fragile.

5. Validate the user-facing design.
- Compare whether two separate datasets really do improve transparency and onboarding.
- Check whether a single internal helper could still support both examples without exposing unnecessary complexity.

Deliverables:

- a written modelling design note
- one example dataset for single-source repeated OD observations
- one example dataset for multi-source repeated OD observations
- a recommendation on flexible model specification

Decision gate:

- Do two separate datasets remain the clearest path for users after we draft the examples, or is there a strong enough maintenance case to justify a unified structure?
