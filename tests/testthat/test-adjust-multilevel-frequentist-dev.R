source(testthat::test_path("helper-multilevel-scenarios.R"))

test_that("multilevel scenario resolver maps S1 through S4", {
  s1 <- make_multilevel_scenario_toy()
  s2 <- make_multilevel_scenario_toy(periods = c("t1", "t2"))
  s3 <- make_multilevel_scenario_toy(sources = c("src1", "src2"))
  s4 <- make_multilevel_scenario_toy(sources = c("src1", "src2"), periods = c("t1", "t2"))

  expect_equal(debiasR:::.resolve_multilevel_scenario(s1$mpd_od)$scenario, "s1")
  expect_equal(debiasR:::.resolve_multilevel_scenario(s2$mpd_od)$scenario, "s2")
  expect_equal(debiasR:::.resolve_multilevel_scenario(s3$mpd_od)$scenario, "s3")
  expect_equal(debiasR:::.resolve_multilevel_scenario(s4$mpd_od)$scenario, "s4")

  expect_equal(
    debiasR:::.resolve_multilevel_scenario(s2$mpd_od, scenario = "s2")$repeated_observation,
    "time"
  )
  expect_error(
    debiasR:::.resolve_multilevel_scenario(s3$mpd_od, scenario = "s2"),
    "scenario = 's2'"
  )
})

test_that("prepare helper carries source and time metadata for repeated inputs", {
  toy <- make_multilevel_scenario_toy(periods = c("t1", "t2"))
  scenario_info <- debiasR:::.resolve_multilevel_scenario(
    toy$mpd_od,
    scenario = "s2"
  )

  prep <- debiasR:::.prepare_multilevel_bayes_data(
    mpd_od_df = toy$mpd_od,
    coverage_df = toy$coverage,
    covariates_df = toy$covariates,
    distance_df = toy$distance,
    flow_col = "flow",
    income_col = "income_norm",
    pop_col = "population",
    distance_col = "distance_km",
    scenario_info = scenario_info
  )

  expect_equal(prep$scenario_info$scenario, "s2")
  expect_equal(prep$scenario_info$repeated_observation, "time")
  expect_true(all(c(
    "mpd_source", "mpd_time", "bias_e_origin",
    "rural_pct_o", "rural_pct_d", "deprivation_score_o", "deprivation_score_d"
  ) %in% names(prep$model_df)))
  expect_equal(length(unique(prep$model_df$mpd_time)), 2)
  expect_true(all(is.finite(prep$model_df$bias_e_origin)))
})

test_that("latent two-level contract creates latent flow identifiers and rejects frequentist engine", {
  s1_toy <- make_multilevel_scenario_toy()
  s1_info <- debiasR:::.resolve_multilevel_scenario(
    s1_toy$mpd_od,
    scenario = "s1"
  )
  s1_prep <- debiasR:::.prepare_multilevel_bayes_data(
    mpd_od_df = s1_toy$mpd_od,
    coverage_df = s1_toy$coverage,
    covariates_df = s1_toy$covariates,
    distance_df = s1_toy$distance,
    flow_col = "flow",
    income_col = "income_norm",
    pop_col = "population",
    distance_col = "distance_km",
    scenario_info = s1_info
  )

  s1_latent <- suppressWarnings(
    debiasR:::.prepare_multilevel_latent_state(
      data = s1_prep$model_df,
      scenario_info = s1_info,
      latent_flow_unit = "auto"
    )
  )

  expect_equal(s1_latent$latent_flow_unit, "od")
  expect_true(s1_latent$identifiability$weak_identification_warning)
  expect_equal(
    s1_latent$identifiability$min_observations_per_latent_flow,
    1L
  )

  toy <- make_multilevel_scenario_toy(sources = c("src1", "src2"))
  scenario_info <- debiasR:::.resolve_multilevel_scenario(
    toy$mpd_od,
    scenario = "s3"
  )
  prep <- debiasR:::.prepare_multilevel_bayes_data(
    mpd_od_df = toy$mpd_od,
    coverage_df = toy$coverage,
    covariates_df = toy$covariates,
    distance_df = toy$distance,
    flow_col = "flow",
    income_col = "income_norm",
    pop_col = "population",
    distance_col = "distance_km",
    scenario_info = scenario_info
  )

  latent <- debiasR:::.prepare_multilevel_latent_state(
    data = prep$model_df,
    scenario_info = scenario_info,
    latent_flow_unit = "auto"
  )

  expect_equal(latent$latent_flow_unit, "od")
  expect_true(all(c("latent_flow_id", "latent_flow_unit") %in% names(latent$data)))
  expect_equal(latent$n_latent_flows, length(unique(paste(prep$model_df$origin, prep$model_df$destination))))
  expect_false(latent$identifiability$weak_identification_warning)

  fit_formula <- debiasR:::.add_multilevel_latent_random_intercept(
    flow ~ rural_pct_o + bias_e_origin + offset(log_observation_probability)
  )
  expect_match(
    paste(deparse(fit_formula), collapse = " "),
    "latent_flow_id"
  )

  expect_error(
    adjust_multilevel_bayes(
      mpd_od_df = toy$mpd_od,
      coverage_df = toy$coverage,
      covariates_df = toy$covariates,
      distance_df = toy$distance,
      model_engine = "frequentist",
      target_scale = "true_flow",
      observation_model = "latent_two_level"
    ),
    "requires `model_engine = 'bayesian'`"
  )
})

test_that("internal frequentist default formula contract scales across MSOA-like S1-S4 inputs", {
  scenarios <- list(
    s1 = list(
      sources = "operator_a",
      periods = "2021_q1",
      repeated = "none",
      scenario_terms = character()
    ),
    s2 = list(
      sources = "operator_a",
      periods = c("2021_q1", "2021_q2"),
      repeated = "time",
      scenario_terms = "mpd_time"
    ),
    s3 = list(
      sources = c("operator_a", "operator_b"),
      periods = "2021_q1",
      repeated = "source",
      scenario_terms = "mpd_source"
    ),
    s4 = list(
      sources = c("operator_a", "operator_b"),
      periods = c("2021_q1", "2021_q2"),
      repeated = "source_time",
      scenario_terms = c("mpd_source", "mpd_time")
    )
  )

  for (scenario_name in names(scenarios)) {
    spec <- scenarios[[scenario_name]]
    toy <- make_multilevel_msoa_like_scenario(
      sources = spec$sources,
      periods = spec$periods,
      zero_filled = identical(scenario_name, "s4")
    )

    res <- adjust_multilevel_bayes(
      mpd_od_df = toy$mpd_od,
      coverage_df = toy$coverage,
      covariates_df = toy$covariates,
      distance_df = toy$distance,
      model_engine = "frequentist",
      scenario = scenario_name,
      source_col = "provider_id",
      time_col = "period_id",
      random_intercept = "none",
      prediction_scope = if (identical(scenario_name, "s4")) "complete_grid" else "observed"
    )

    metadata <- attr(res, "result_metadata")
    model_terms <- attr(res, "model_terms")

    expect_equal(attr(res, "scenario"), scenario_name)
    expect_equal(attr(res, "repeated_observation"), spec$repeated)
    expect_equal(attr(res, "source_col"), "provider_id")
    expect_equal(attr(res, "time_col"), "period_id")
    expect_equal(metadata$model_terms$scenario_fixed_effects, spec$scenario_terms)
    expect_equal(model_terms$scenario_fixed_effects, spec$scenario_terms)
    expect_false(model_terms$custom_formula)
    expect_true(all(
      c("income_o", "income_d", "log_distance", "bias_e_origin", "log_pop_o", "log_pop_d") %in%
        model_terms$default_fixed_effects
    ))
    expect_equal(model_terms$requested_random_intercept, "none")
    expect_equal(metadata$n_prediction_rows, nrow(toy$mpd_od))
    expect_true(all(is.finite(res$flow_adj)))
    expect_true(all(res$flow_adj >= 0))
  }
})

test_that("internal frequentist scaffold returns adjusted observed flows", {
  toy <- make_multilevel_scenario_toy()

  res <- adjust_multilevel_bayes(
    mpd_od_df = toy$mpd_od,
    coverage_df = toy$coverage,
    covariates_df = toy$covariates,
    distance_df = toy$distance,
    model_engine = "frequentist",
    scenario = "s1",
    random_intercept = "none",
    mobility_formula = ~ rural_pct_o + rural_pct_d + log_distance,
    bias_formula = ~ bias_e_origin,
    include_flow_adj_draws = TRUE
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(attr(res, "backend"), "frequentist_dev")
  expect_equal(attr(res, "model_engine"), "frequentist")
  expect_equal(attr(res, "scenario"), "s1")
  expect_equal(attr(res, "repeated_observation"), "none")
  expect_true(all(is.finite(res$flow_adj)))
  expect_true(all(res$flow_adj >= 0))
  expect_equal(dim(attr(res, "flow_adj_draws")), c(1L, nrow(res)))
  expect_true("bias_e_origin" %in% attr(res, "coefficients")$term)
  expect_equal(attr(res, "model_terms")$formula_source, "split_formula")
  expect_equal(attr(res, "model_terms")$formula_interface, "split")
  expect_equal(attr(res, "bias_terms"), "bias_e_origin")
  expect_equal(attr(res, "model_terms")$bias_variables, "bias_e_origin")
  expect_true(all(c("rural_pct_o", "rural_pct_d") %in% attr(res, "model_terms")$formula_variables))
})

test_that("internal frequentist scaffold supports coverage-offset true-flow mode", {
  toy <- make_multilevel_scenario_toy()

  res <- adjust_multilevel_bayes(
    mpd_od_df = toy$mpd_od,
    coverage_df = toy$coverage,
    covariates_df = toy$covariates,
    distance_df = toy$distance,
    model_engine = "frequentist",
    scenario = "s1",
    random_intercept = "none",
    target_scale = "true_flow",
    observation_model = "coverage_offset",
    coverage_scale = "origin",
    mobility_formula = ~ rural_pct_o + rural_pct_d + log_distance,
    bias_formula = ~ bias_e_origin,
    include_flow_adj_draws = TRUE
  )

  metadata <- attr(res, "result_metadata")

  expect_s3_class(res, "tbl_df")
  expect_true(all(c(
    "flow_adj", "flow_mpd_pred", "flow_true_pred",
    "observation_probability", "coverage_rate_o", "coverage_rate_d"
  ) %in% names(res)))
  expect_equal(attr(res, "target_scale"), "true_flow")
  expect_equal(attr(res, "observation_model"), "coverage_offset")
  expect_equal(attr(res, "coverage_scale"), "origin")
  expect_equal(metadata$target_scale, "true_flow")
  expect_equal(metadata$observation_model, "coverage_offset")
  expect_equal(metadata$coverage_scale, "origin")
  expect_equal(metadata$offset_column, "log_observation_probability")
  expect_equal(attr(res, "bias_terms"), character())
  expect_equal(attr(res, "model_terms")$formula_interface, "split_true_flow")
  expect_match(paste(attr(res, "formula"), collapse = " "), "offset\\(log_observation_probability\\)")
  expect_equal(as.numeric(res$flow_adj), as.numeric(res$flow_true_pred))
  expect_true(all(is.finite(res$flow_mpd_pred)))
  expect_true(all(is.finite(res$flow_true_pred)))
  expect_true(all(res$observation_probability > 0 & res$observation_probability < 1))
  expect_equal(
    as.numeric(res$flow_mpd_pred),
    as.numeric(res$flow_true_pred * res$observation_probability),
    tolerance = 1e-6
  )
  expect_true(all(res$flow_true_pred > res$flow_mpd_pred))
  expect_equal(dim(attr(res, "flow_adj_draws")), c(1L, nrow(res)))
})

test_that("coverage-offset true-flow mode supports destination and both coverage scales", {
  toy <- make_multilevel_scenario_toy()

  res_destination <- adjust_multilevel_bayes(
    mpd_od_df = toy$mpd_od,
    coverage_df = toy$coverage,
    covariates_df = toy$covariates,
    distance_df = toy$distance,
    model_engine = "frequentist",
    scenario = "s1",
    random_intercept = "none",
    target_scale = "true_flow",
    observation_model = "coverage_offset",
    coverage_scale = "destination",
    mobility_formula = ~ log_distance,
    bias_formula = ~ bias_e_origin
  )
  res_both <- adjust_multilevel_bayes(
    mpd_od_df = toy$mpd_od,
    coverage_df = toy$coverage,
    covariates_df = toy$covariates,
    distance_df = toy$distance,
    model_engine = "frequentist",
    scenario = "s1",
    random_intercept = "none",
    target_scale = "true_flow",
    observation_model = "coverage_offset",
    coverage_scale = "both",
    mobility_formula = ~ log_distance,
    bias_formula = ~ bias_e_origin
  )

  expect_equal(res_destination$observation_probability, res_destination$coverage_rate_d)
  expect_equal(
    res_both$observation_probability,
    sqrt(res_both$coverage_rate_o * res_both$coverage_rate_d)
  )
})

test_that("coverage-offset true-flow mode errors clearly on zero user coverage", {
  toy <- make_multilevel_scenario_toy()
  toy$coverage$user_count[1] <- 0

  expect_error(
    adjust_multilevel_bayes(
      mpd_od_df = toy$mpd_od,
      coverage_df = toy$coverage,
      covariates_df = toy$covariates,
      distance_df = toy$distance,
      model_engine = "frequentist",
      scenario = "s1",
      random_intercept = "none",
      target_scale = "true_flow",
      observation_model = "coverage_offset",
      coverage_scale = "origin",
      mobility_formula = ~ log_distance,
      bias_formula = ~ bias_e_origin
    ),
    "positive finite coverage rates"
  )
})

test_that("internal frequentist scaffold supports S4 complete-grid prediction", {
  toy <- make_multilevel_scenario_toy(
    sources = c("src1", "src2"),
    periods = c("t1", "t2"),
    zero_filled = TRUE
  )

  res <- adjust_multilevel_bayes(
    mpd_od_df = toy$mpd_od,
    coverage_df = toy$coverage,
    covariates_df = toy$covariates,
    distance_df = toy$distance,
    model_engine = "frequentist",
    scenario = "s4",
    random_intercept = "none",
    formula = flow ~ rural_pct_o + rural_pct_d + bias_e_origin + log_distance + mpd_source + mpd_time,
    prediction_scope = "complete_grid"
  )

  metadata <- attr(res, "result_metadata")

  expect_equal(attr(res, "scenario"), "s4")
  expect_equal(attr(res, "repeated_observation"), "source_time")
  expect_equal(nrow(res), nrow(toy$mpd_od))
  expect_equal(metadata$n_fit_rows, nrow(toy$mpd_od) - 1L)
  expect_equal(metadata$n_prediction_rows, nrow(toy$mpd_od))
  expect_equal(metadata$n_zero_filled_prediction_rows, 1L)
  expect_equal(attr(res, "od_audit")$n_scenarios, 4)
  expect_equal(res$model_fit_status[res$mpd_zero_filled], "predicted")
  expect_true(all(is.finite(res$flow_adj)))
})

test_that("internal frequentist scaffold can use lme4 for a mixed model when available", {
  testthat::skip_if_not_installed("lme4")
  toy <- make_multilevel_scenario_toy(periods = c("t1", "t2"))

  res <- suppressWarnings(
    adjust_multilevel_bayes(
      mpd_od_df = toy$mpd_od,
      coverage_df = toy$coverage,
      covariates_df = toy$covariates,
      distance_df = toy$distance,
      model_engine = "frequentist",
      scenario = "s2",
      random_intercept = "origin",
      mobility_formula = ~ log_distance + mpd_time + (1 + log_distance | origin),
      bias_formula = ~ bias_e_origin
    )
  )

  expect_equal(attr(res, "backend"), "frequentist_dev")
  expect_equal(attr(res, "model_engine"), "frequentist")
  expect_equal(attr(res, "random_intercept"), "origin")
  expect_true("(1 + log_distance | origin)" %in% attr(res, "model_terms")$formula_random_effects)
  expect_true(all(is.finite(res$flow_adj)))
})

test_that("formula validation reports missing prepared covariates", {
  toy <- make_multilevel_scenario_toy()

  expect_error(
    adjust_multilevel_bayes(
      mpd_od_df = toy$mpd_od,
      coverage_df = toy$coverage,
      covariates_df = toy$covariates,
      distance_df = toy$distance,
      model_engine = "frequentist",
      scenario = "s1",
      random_intercept = "none",
      mobility_formula = ~ missing_covariate_o,
      bias_formula = ~ bias_e_origin
    ),
    "missing_covariate_o"
  )
})

test_that("split bias formula validates zero-bias counterfactual variables", {
  toy <- make_multilevel_scenario_toy(sources = c("src1", "src2"))

  expect_error(
    adjust_multilevel_bayes(
      mpd_od_df = toy$mpd_od,
      coverage_df = toy$coverage,
      covariates_df = toy$covariates,
      distance_df = toy$distance,
      model_engine = "frequentist",
      scenario = "s3",
      random_intercept = "none",
      mobility_formula = ~ log_distance,
      bias_formula = ~ mpd_source
    ),
    "zero-bias counterfactual"
  )
})

test_that("split formula works when no area covariates are required", {
  data(simulated_mpd.od)
  data(simulated_coverage)

  res <- suppressWarnings(
    adjust_multilevel_bayes(
      mpd_od_df = simulated_mpd.od,
      coverage_df = simulated_coverage,
      covariates_df = NULL,
      model_engine = "frequentist",
      random_intercept = "none",
      mobility_formula = ~ log_distance,
      bias_formula = ~ bias_e_origin
    )
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(attr(res, "model_terms")$formula_source, "split_formula")
  expect_equal(attr(res, "bias_terms"), "bias_e_origin")
  expect_true(all(is.finite(res$flow_adj)))
})

test_that("internal frequentist scaffold rejects unsupported zero-inflated families", {
  toy <- make_multilevel_scenario_toy()

  expect_error(
    adjust_multilevel_bayes(
      mpd_od_df = toy$mpd_od,
      coverage_df = toy$coverage,
      covariates_df = toy$covariates,
      distance_df = toy$distance,
      model_engine = "frequentist",
      scenario = "s1",
      random_intercept = "none",
      model_family = "zip"
    ),
    "supports only Poisson and negative-binomial"
  )
})
