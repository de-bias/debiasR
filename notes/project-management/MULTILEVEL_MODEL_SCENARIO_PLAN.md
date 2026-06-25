# Multilevel Model Scenario Plan

Last updated: 2026-06-25

## Purpose

This note defines the scenario support for mobile-phone-derived inputs
in `adjust_multilevel_bayes()`. The aim is to handle variation in data source
and observation time through one transparent Bayesian modelling path.

The Stage-1 Bayesian engine now supports S1-S4 source/time scenarios. The
frequentist engine remains available through `model_engine = "frequentist"` for
fast formula checks, data-shape checks, runtime-sensitive experimentation, and
method comparison before committing to Bayesian sampling.

2026-06-25 update: external empirical flow outputs are now available for
testing the S1-S4 source/time structures at
`/Volumes/DEBIAS/data/outputs/flows`. These files should be used for local
validation runs only; raw files and bulky rendered outputs should not be
committed to `debiasR`.

## Scenario Definitions

S1: single source, single time.

- One mobile-phone-derived OD matrix for one observation period.
- This is the current baseline scenario for observed-flow correction and
  complete-grid prediction.
- Source and time identifiers may be absent or constant.

S2: single source, multiple times.

- One mobile-phone-derived source observed across repeated periods.
- A time identifier is required.
- The model should support temporal variation while preserving the OD structure.

S3: multiple sources, single time.

- Two or more mobile-phone-derived OD matrices for the same observation period.
- A source identifier is required.
- The model should support source-level variation in coverage or reporting
  intensity.

S4: multiple sources, multiple times.

- Multiple mobile-phone-derived sources observed across repeated periods.
- Source and time identifiers are required.
- The model should support source effects, time effects, and a later decision on
  whether a source-time interaction is needed.

## Empirical S1-S4 Data Now Available

The primary empirical route is the travel-to-work branch under
`/Volumes/DEBIAS/data/outputs/flows/htw`, because it contains Census
benchmarks and multiple mobile-phone-derived sources at LAD/LTLA and MSOA
support. The migration branch under `mig` is useful secondary validation
material, but should wait until the code/label harmonisation for the Twitter
OD table has been audited.

| Scenario | Primary HTW construction | Required audit before fitting |
| --- | --- | --- |
| S1 | One `mapp1` or `mapp2` OD table for one source/time slice, validated against the matching Census LAD or MSOA benchmark. | Standardise origin, destination, flow, source, time, geography, and benchmark fields; confirm whether the chosen file is already aggregated or has recoverable period metadata. |
| S2 | One source observed across repeated periods, starting with `mapp1` weekly/monthly outputs when period identifiers can be recovered or constructed. | Confirm `mpd_time` values, repeated OD keys, period coverage, and whether weekly/monthly files are true repeated observations or aggregate summaries. |
| S3 | Multiple sources for one matched period/support, starting with `mapp1` and `mapp2` monthly OD tables. | Harmonise flow columns (`count`, `trips`), source labels, geography support, and source-specific OD coverage before fitting source effects. |
| S4 | Multiple sources over repeated periods, combining the source audit from S3 with the period audit from S2. | Confirm source-time coverage, define `mpd_source`, `mpd_time`, and `source_time`, and decide whether validation compares source-specific MPD-scale predictions or source-invariant true-flow estimates. |

## API Direction

Scenario support should be added through parameters in `adjust_multilevel_bayes()`.

Initial design questions:

- Should users pass `scenario`, `source_col`, and `time_col`, or should
  `scenario` be inferred when source and time columns are supplied?
- Should S1 be represented by missing source/time columns, constant source/time
  columns, or both?
- Which random-effect structures should be available for each scenario while
  keeping the function transparent?

Working recommendation:

- Accept explicit source and time column arguments.
- Allow a scenario argument for clarity and validation.
- Use `model_engine = "frequentist"` for fast S1-S4 checks and
  `model_engine = "bayesian"` for posterior S1-S4 fitting.
- Infer the simplest valid scenario only when `scenario = "auto"`.
- Return scenario metadata so downstream validation and teaching examples can
  show what was fit.
- Keep existing observed-flow and complete-grid prediction behavior unchanged
  for S1.

## Current Shared Formula Contract

The primary user-facing model interface is now `formula`, for example:

`flow ~ rural_pct_o + rural_pct_d + log_distance + bias_e_origin + (1 + log_distance | origin)`

Area-level covariates are joined twice using origin and destination suffixes.
Formula random-effect terms are treated as the source of truth when supplied.
`custom_formula` is retained as a deprecated alias, and `income_col` is retained
only as a legacy helper for the default formula.

Under either engine and no `formula`, the default formula starts from:

`flow ~ income_o + income_d + log_distance + bias_e_origin`

When finite population terms are available, the default also includes
`log_pop_o + log_pop_d`.

Scenario-specific fixed effects are:

- S1: no additional source/time term.
- S2: add `mpd_time`.
- S3: add `mpd_source`.
- S4: add `mpd_source + mpd_time`.

Current random-intercept options are `origin`, `destination`, `od`, `source`,
`time`, `source_time`, and `none`, subject to the relevant grouping column
having at least two levels. If a source or time grouping is requested as the
random intercept, the same source or time fixed effect is omitted from the
default formula to avoid duplicating that structure.

The S4 fixed source-time interaction remains deferred until empirical runtime
and identifiability are reviewed. Users can request `random_intercept =
"source_time"` during development to test source-time pooling without adding a
default fixed interaction.

The S1-S4 formula contract is shared by `model_engine = "bayesian"` and
`model_engine = "frequentist"`. Bayesian fits retain the heavier optional
dependency and sampler-diagnostic burden; frequentist fits remain the fast
contract-checking option.

## Development Data Policy

Use `/Volumes/DEBIAS/data/outputs/flows` for empirical S1-S4 validation.

- Treat the HTW branch as the first empirical S1-S4 testing route because it
  has `mapp1`, `mapp2`, and Census benchmark files at LAD/LTLA and MSOA levels.
- Use the migration/Twitter branch only after its code/label harmonisation and
  geography support have been checked.
- Do not commit raw external flow files, bulky rendered outputs, or local
  caches built from the external volume unless a separate data-management
  decision explicitly allows a small derived artifact.

Use MSOA data for software development and internal testing.

- MSOA inputs are better for stress testing because they expose larger grids,
  repeated observations, and stricter runtime constraints.
- Internal tests should cover scenario detection, missing required columns,
  source/time metadata, and compatibility with the existing prediction scopes.

Use LAD data for vignettes and teaching materials.

- LAD examples are easier to explain and render.
- LAD should remain the default empirical teaching route because it is the
  current user-facing path through `debiasRdata`.

## Workstreams

### Software Development

1. Define the S1-S4 data contract.
- Required columns: origin, destination, flow, benchmark or comparison fields,
  and optional source/time identifiers depending on scenario.
- Confirm how complete-grid row-status metadata interacts with source and time
  identifiers.

2. Build the frequentist scaffold.
- Use a fast internal GLM or GLMM prototype to check formula construction,
  scenario-specific terms, and output shape.
- Use the scaffold to identify runtime and identifiability problems before
  running Bayesian fits.
- Expose it only as a development engine option on `adjust_multilevel_bayes()`;
  do not introduce a separate exported frequentist adjustment function.

3. Implement Bayesian scenario support.
- Defer Bayesian scenario implementation until the frequentist S1-S4 contract is
  complete.
- Then map each reviewed scenario to a transparent Bayesian model formula.
- Preserve current backend policy: `rstanarm` for the practical standard path
  and `brms` only when extra model flexibility is required.
- Return scenario metadata, model terms, and prediction-scope metadata.

4. Add internal MSOA tests.
- Test S1 backward compatibility.
- Test S2 time-column validation and time-level metadata.
- Test S3 source-column validation and source-level metadata.
- Test S4 combined source/time validation and metadata.

### Empirical S1-S4 Validation

1. Audit and normalise the external HTW flow files.
- Read only from `/Volumes/DEBIAS/data/outputs/flows`.
- Create a local normalisation helper for validation notebooks/scripts that
  returns `origin`, `destination`, `flow`, `mpd_source`, `mpd_time`,
  `geography`, `mpd_observed`, and benchmark-compatible columns.
- Record file paths, row counts, column mappings, zero/positive-flow counts,
  geography level, source levels, and time levels in the validation output.

2. Build support and benchmark joins.
- Align each MPD source/time table with the corresponding Census LAD/LTLA or
  MSOA benchmark.
- Reuse existing `debiasRdata` covariate and centroid assets when possible for
  population, active-user coverage, rural/income-like covariates, and distance.
- Decide whether the first empirical runs use observed OD support only or
  complete-grid prediction, then record the row-count expansion.

3. Run a smoke ladder before Bayesian sampling.
- First run each S1-S4 construction with `model_engine = "frequentist"` to
  verify scenario detection, formula terms, random-effect eligibility, returned
  metadata, prediction scope, and visual validation outputs.
- Then run small Bayesian pilots for S1-S4 with conservative area and period
  subsets before increasing area count, source/time count, iterations, or
  chains.
- Always record runtime, warnings, errors, posterior diagnostics, and whether
  source/time terms were dropped because they had too few levels.

4. Compare model specifications.
- Include coverage-offset Bayesian models, the experimental
  `latent_two_level` backend for repeated observations, and deterministic
  baselines such as inverse penetration, selection-rate, raking, and
  coefficient adjustment.
- Test no random intercept, origin, destination, OD, source, time, and
  source-time random intercepts when the scenario supports them.
- Test a small set of random-slope formulas only after the corresponding
  random-intercept model is stable.
- Sort validation plots from best to worst by the chosen benchmark metric while
  keeping the unadjusted raw MPD model visually last.

5. Promote only reproducible outputs.
- Keep raw data external.
- Commit only the validation plan, code, and compact textual summaries unless
  a later data-governance decision approves derived artifacts.
- Treat long-running Bayesian output as local/manual evidence until a smaller
  reproducible smoke subset is identified.

### Vignettes and Teaching Materials

1. Build LAD-scale examples.
- Keep examples small and explicit.
- Avoid forcing Bayesian dependencies during routine vignette rendering when
  the optional backend is unavailable.

2. Teach scenarios in order.
- S1: baseline OD correction.
- S2: temporal variation.
- S3: source variation.
- S4: combined source and temporal variation.

3. Explain the modelling status.
- State that `model_engine = "frequentist"` is a development device for faster
  model-contract iteration.
- Present the Bayesian model as the intended package method.
- Keep prototype and runtime caveats visible until empirical Bayesian runtime is
  validated.
- In teaching text, describe the MPD observation equation and true-flow
  prediction equation as the coverage-offset route, not as a generic structure
  for every `adjust_multilevel_bayes()` variant.
- Use the advanced Bayesian vignette to distinguish `coverage_offset`,
  `reduced_form`, and `latent_two_level`: coverage-offset estimates true flows
  through a fixed coverage offset, reduced-form returns an MPD-scale
  counterfactual by neutralising fitted bias terms, and latent two-level
  estimates shared hidden OD or OD-time states from repeated source/time rows.

## Decision Gate

The software contract is implemented; the current gate is empirical evidence.
Promote the S1-S4 path beyond experimental use only after the HTW flow files
produce reproducible frequentist smoke results and at least one successful
Bayesian pilot for each available structure. If the period audit shows that a
file is an aggregate summary rather than repeated observations, treat it as an
S1 or S3 input until the required period metadata are joined.
