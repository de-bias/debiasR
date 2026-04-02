test_that("adjust_multilevel_bayes errors clearly when rstanarm is unavailable", {
  data(simulated_mpd.od)
  data(simulated_coverage)
  data(simulated_covariates)

  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    expect_error(
      suppressWarnings(
        adjust_multilevel_bayes(
          mpd_od_df = simulated_mpd.od,
          coverage_df = simulated_coverage,
          covariates_df = simulated_covariates,
          iter = 100,
          chains = 2,
          seed = 1
        )
      ),
      "Backend 'rstanarm' requested, but package is not installed."
    )
  } else {
    skip("rstanarm is installed; fallback error path not applicable in this environment.")
  }
})

test_that("prepare helper builds deterministic bias and synthetic distance columns", {
  data(simulated_mpd.od)
  data(simulated_coverage)
  data(simulated_covariates)

  prep1 <- suppressWarnings(
    debiasR:::.prepare_multilevel_bayes_data(
      mpd_od_df = simulated_mpd.od,
      coverage_df = simulated_coverage,
      covariates_df = simulated_covariates,
      distance_df = NULL,
      flow_col = "flow",
      income_col = "income_norm",
      pop_col = "population",
      distance_col = "distance_km"
    )
  )

  prep2 <- suppressWarnings(
    debiasR:::.prepare_multilevel_bayes_data(
      mpd_od_df = simulated_mpd.od,
      coverage_df = simulated_coverage,
      covariates_df = simulated_covariates,
      distance_df = NULL,
      flow_col = "flow",
      income_col = "income_norm",
      pop_col = "population",
      distance_col = "distance_km"
    )
  )

  expect_true(all(c("bias_e_origin", "log_dist_synth") %in% names(prep1$base_df)))
  expect_equal(prep1$base_df$log_dist_synth, prep2$base_df$log_dist_synth)
  expect_equal(prep1$base_df$bias_e_origin, prep2$base_df$bias_e_origin)
})

test_that("adjust_multilevel_bayes validates required schema", {
  data(simulated_mpd.od)
  data(simulated_coverage)
  data(simulated_covariates)

  bad_cov <- simulated_coverage
  bad_cov$population <- NULL
  expect_error(
    adjust_multilevel_bayes(
      mpd_od_df = simulated_mpd.od,
      coverage_df = bad_cov,
      covariates_df = simulated_covariates
    ),
    "coverage_df"
  )

  bad_covars <- simulated_covariates
  bad_covars$income_norm <- NULL
  expect_error(
    adjust_multilevel_bayes(
      mpd_od_df = simulated_mpd.od,
      coverage_df = simulated_coverage,
      covariates_df = bad_covars,
      income_col = "income_norm"
    ),
    "covariates_df"
  )

  bad_mpd <- simulated_mpd.od
  bad_mpd$flow <- NULL
  expect_error(
    adjust_multilevel_bayes(
      mpd_od_df = bad_mpd,
      coverage_df = simulated_coverage,
      covariates_df = simulated_covariates
    ),
    "mpd_od_df"
  )
})

test_that("adjust_multilevel_bayes returns adjusted flows when rstanarm is available", {
  skip_if_not_installed("rstanarm")

  data(simulated_mpd.od)
  data(simulated_coverage)
  data(simulated_covariates)

  res <- adjust_multilevel_bayes(
    mpd_od_df = simulated_mpd.od,
    coverage_df = simulated_coverage,
    covariates_df = simulated_covariates,
    iter = 100,
    chains = 1,
    seed = 123,
    refresh = 0
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("origin", "destination", "flow", "flow_adj") %in% names(res)))
  expect_true(all(c("bias_e_origin", "log_dist_synth") %in% names(res)))

  modeled <- res[is.finite(res$flow_adj), , drop = FALSE]
  expect_gt(nrow(modeled), 0)
  expect_true(all(is.finite(modeled$flow_adj)))
  expect_true(all(modeled$flow_adj >= 0))
  expect_true(any(abs(modeled$flow_adj - modeled$flow) > 1e-8))

  coef_tbl <- attr(res, "coefficients")
  expect_true(is.data.frame(coef_tbl))
  expect_true("term" %in% names(coef_tbl))
  expect_true("bias_e_origin" %in% coef_tbl$term)
})
