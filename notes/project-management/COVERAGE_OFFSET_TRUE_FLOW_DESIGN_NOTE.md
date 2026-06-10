# Coverage-Offset True-Flow Implementation Design

Last updated: 2026-06-10

## Purpose

This note scopes the first implementation of a coverage-offset true-flow model
for `adjust_multilevel_bayes()`. It is a design note only. It does not change
package code, tests, documentation output, or the current experimental status of
the Bayesian path.

The goal is to make the current split between conceptual true-flow predictors
and observation-bias predictors operational without immediately building the
full latent two-level Bayesian model. The first implementation should estimate a
single observed-flow likelihood with log-coverage offsets that encode known or
estimated active-user coverage at the origin, destination, or both sides of an
OD pair.

## Notation

For each observed or supplied OD pair `(i, j)`:

- `Y_ij`: observed mobile-phone-derived flow.
- `F_ij`: target true flow, on the expected count scale.
- `c_i^O`: origin-side coverage score for area `i`, usually active users in
  area `i` divided by benchmark population in area `i`.
- `c_j^D`: destination-side coverage score for area `j`.
- `x_ij`: OD-level true-flow predictors, such as distance and origin or
  destination area covariates.
- `z_ij`: residual observation-bias predictors that are not represented by the
  offset.
- `eta_ij`: true-flow linear predictor.
- `delta_ij`: remaining observation-bias linear predictor.
- `w_ij`: selected log-coverage offset.

The working observation model is:

```text
Y_ij ~ Poisson(mu_obs_ij)
log(mu_obs_ij) = eta_ij + delta_ij + w_ij
eta_ij = alpha + x_ij beta + u_i^O + u_j^D + u_ij
delta_ij = z_ij gamma
```

A negative-binomial likelihood can use the same linear predictor when
overdispersion is needed:

```text
Y_ij ~ NegBin(mu_obs_ij, phi)
log(mu_obs_ij) = eta_ij + delta_ij + w_ij
```

The estimated true-flow mean removes the coverage offset and any explicit
observation-bias terms:

```text
mu_true_ij = exp(eta_ij)
```

The adjusted flow returned to users is a summary of `mu_true_ij`, for example a
posterior mean or median in the Bayesian path, or a fitted mean in the
frequentist development path.

## Offset Choices

The implementation should expose an explicit offset choice rather than burying
the decision in formula construction.

### Origin Offset

Use origin coverage only:

```text
w_ij = log(c_i^O)
log(mu_obs_ij) = log(mu_true_ij) + log(c_i^O) + delta_ij
```

Interpretation: observed outgoing flows from origin `i` are sampled in
proportion to origin active-user coverage. This is the closest match to current
coverage diagnostics when the observed mobile-phone population is anchored to
home or origin-side representation.

Recommended first default: origin offset, because existing `measure_bias()` and
Stage 3 diagnostics already define coverage at the area level and because many
OD sampling narratives treat origin representation as the main exposure.

### Destination Offset

Use destination coverage only:

```text
w_ij = log(c_j^D)
log(mu_obs_ij) = log(mu_true_ij) + log(c_j^D) + delta_ij
```

Interpretation: observed flows into destination `j` are sampled in proportion to
destination-side capture or destination presence. This can be useful when the
phone-derived data source is more plausibly tied to workplace, visited-place, or
destination detection coverage.

This should be supported, but not made the first default unless the input data
or empirical design clearly defines destination-side coverage.

### Both-Sides Offset

Use the geometric mean of origin and destination coverage:

```text
w_ij = 0.5 * (log(c_i^O) + log(c_j^D))
log(mu_obs_ij) = log(mu_true_ij) + 0.5 * (log(c_i^O) + log(c_j^D)) + delta_ij
```

Equivalently, `q_ij = sqrt(c_i^O * c_j^D)`. Interpretation: observation
reflects both sides of the OD process without multiplying two area-level
coverage ratios directly. This is still stronger than the origin-only or
destination-only assumption, but less aggressive than `c_i^O * c_j^D`.

The first implementation should allow this option but warn in documentation that
it is a stronger measurement assumption. It should not be the default without
empirical justification.

### No Offset

Retain a no-offset mode for backward compatibility and comparison:

```text
w_ij = 0
```

This keeps current reduced-form behavior available and gives diagnostics a
baseline for evaluating whether the offset improves benchmark agreement.

## Coverage Preprocessing

Coverage scores must be positive before taking logs. The implementation should:

- require finite non-missing coverage for the selected offset side;
- reject negative coverage;
- either reject zero coverage by default or apply an explicit user-supplied
  floor;
- record the floor, if any, in metadata;
- avoid silently replacing missing coverage with 1.

If both-side offsets are requested, the result should record the origin,
destination, and combined log offsets separately so diagnostics can identify
which side drives extreme adjustments.

## Assumptions and Identifiability

The offset is treated as known measurement exposure, not as a coefficient to be
estimated. This is what makes the true-flow scale identifiable in the first
implementation.

Key assumptions:

- Coverage scores are measured outside the flow likelihood and are on a scale
  where 1 means full or reference coverage.
- The selected offset side is the correct observation process for the data
  source, or at least a useful approximation.
- Residual bias terms in `bias_formula` capture variation not already explained
  by the offset.
- True-flow predictors in `mobility_formula` are not perfectly confounded with
  the offset after origin, destination, source, and time structure are included.
- Complete-grid prediction uses the same coverage mechanism for observed and
  zero-filled source-missing OD rows unless a later design note introduces a
  different missingness model.

Main identifiability risks:

- An intercept plus an offset whose scale is arbitrary can shift the absolute
  level of `F_ij`. Coverage scores should therefore be documented as exposure
  ratios, not arbitrary indices.
- Including the same coverage variable both as an offset and as an estimated
  bias-formula term makes the offset and coefficient hard to interpret.
- Origin fixed effects or rich origin random effects can absorb origin coverage
  patterns, especially in small data. The offset remains algebraically applied,
  but the separability of true-flow structure and residual bias may be weak.
- Both-side offsets can over-correct if origin and destination coverage scores
  describe the same active-user denominator twice.
- Source and time effects in S2-S4 settings can confound source-specific
  coverage shifts unless source/time coverage definitions are explicit.

The first PR should therefore treat the offset model as a transparent
measurement correction, not as proof that latent true flows have been fully
identified.

## Output Contract

The adjusted result should keep the existing list-like result shape where
possible and add narrowly scoped metadata rather than changing downstream
expectations.

Required row-level fields:

- `flow`: original observed or supplied MPD flow.
- `flow_adj`: adjusted true-flow estimate on the count scale.
- `flow_mpd_pred`: fitted MPD-scale expected flow.
- `flow_true_pred`: fitted true-flow expected flow; equal to `flow_adj` in
  true-flow mode.
- `observation_probability`: selected `q_ij` coverage probability.
- `log_observation_probability`: total offset used in the fitted linear
  predictor.
- `coverage_rate_o`: origin coverage used when available or requested.
- `coverage_rate_d`: destination coverage used when available or requested.
- existing row-status fields for complete-grid mode, unchanged.

Required metadata:

- selected `target_scale`;
- selected `observation_model`;
- selected `coverage_scale`;
- offset column used by the fitted model;
- whether coverage was supplied directly or derived from active-user and
  population columns;
- formula components used for the true-flow and residual bias parts;
- likelihood family and backend;
- scenario metadata for S1-S4 source/time inputs;
- prediction scope and row-status summary for complete-grid mode.

Optional outputs:

- draw-level adjusted true-flow estimates when draw output is requested;
- draw-level observed-flow fitted values if useful for diagnostics;
- offset summaries by origin, destination, source, and time.

Backward compatibility:

- `target_scale = "mpd_counterfactual"` and `observation_model = "reduced_form"`
  should preserve the current reduced-form path as
  closely as possible.
- Existing users should not be forced to supply coverage offsets unless they
  request the new true-flow offset mode.

## Diagnostics

The first implementation should produce diagnostics that are cheap, deterministic
where possible, and aligned with existing `validate_flow_*` and Stage 3
coverage-residual tools.

Minimum diagnostics:

- observation-probability summary: min, median, max, and missing/zero/floored
  counts;
- adjusted-to-observed ratio summary by offset type;
- correlation between `log_observation_probability` and raw residuals where a
  benchmark flow is supplied;
- correlation between `log_observation_probability` and adjusted residuals
  where a benchmark flow is supplied;
- origin-total and destination-total comparisons before and after adjustment;
- flags for extreme offsets and extreme adjusted-to-observed ratios;
- scenario/source/time summaries when repeated observations are present.

Recommended validation workflow:

1. Compare no-offset, origin-offset, destination-offset, and both-offset runs on
   simulated data with known true flows.
2. Use `validate_flow_overall()` and `validate_flow_residuals()` to check
   benchmark agreement and residual reduction.
3. Use `validate_flow_residual_structure()` to check whether offset-adjusted
   residuals still align with area coverage, geography, or selected covariates.
4. Use `validate_flow_distribution()` to check whether destination-share
   allocation improves or degrades after offset adjustment.
5. Use Stage 3 `validate_bias_residual_structure()` outputs to explain whether
   the selected offset side matches the observed active-user coverage pattern.

Diagnostics should report evidence, not choose the offset automatically in the
first PR.

## Relation to GitHub Issues

Issue #20 is treated as the implementation ticket for the coverage-offset
true-flow path. This note scopes that issue as a pragmatic first step: add an
explicit coverage exposure offset to the observed-flow model, define supported
origin/destination/both choices, preserve current output behavior, and attach
diagnostics that make the measurement assumption visible.

Issue #18 is broader. It records the planned genuinely latent two-level Bayesian
model where `F_true_ij` is estimated explicitly rather than recovered through a
single reduced-form observed-flow likelihood or a zero-bias counterfactual. The
coverage-offset design can inform #18 by clarifying the observation equation,
coverage inputs, identifiability risks, and diagnostics, but it should not be
presented as closing #18.

In short:

- #20: implement the coverage-offset observed-flow correction.
- #18: later implement the full latent true-flow model.
- First PR: close or substantially advance #20; leave #18 open unless a later
  PR introduces an explicit latent `F_true_ij` layer.

## First-PR Scope

The first PR should be deliberately small.

In scope:

- Add `target_scale`, `observation_model`, and `coverage_scale` arguments.
- Add explicit coverage-column handling for the selected offset side.
- Compute log offsets during data preparation with strict validation.
- Include the offset in the frequentist development engine first.
- Preserve current S1 behavior when `target_scale = "mpd_counterfactual"` and
  `observation_model = "reduced_form"`.
- Return row-level offset fields and metadata.
- Add focused tests on simulated data for offset construction, zero/missing
  coverage handling, metadata, and no-offset backward compatibility.
- Add at least one benchmark-oriented diagnostic summary for offset behavior.

Out of scope for the first PR:

- A new exported function.
- Changes to deterministic `adjust_*` methods.
- A full latent Bayesian `F_true_ij` parameter layer.
- Automatic offset selection.
- New empirical data assets.
- Broad vignette rewrites beyond minimal user-facing documentation needed for
  the new argument.
- Promotion of `adjust_multilevel_bayes()` out of experimental status.

Decision gate for the first PR:

- The implementation is acceptable if the offset algebra is explicit, the
  no-offset path remains stable, simulated tests show the expected direction of
  correction, and diagnostics make it easy to see when origin, destination, or
  both-side offsets are inappropriate.
