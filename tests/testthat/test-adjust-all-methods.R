test_that("adjust_all_methods fits the main adjustment methods", {
  results <- adjust_all_methods(
    mpd_od_df = simulated_mpd.od,
    coverage_df = simulated_coverage,
    benchmark_od_df = simulated_benchmark.od,
    covariates_df = simulated_covariates,
    distance_df = simulated_distance,
    covariate_col = "income_norm",
    multilevel_engine = "frequentist"
  )

  expect_s3_class(results, "debiasR_adjustment_set")
  expect_named(
    results,
    c(
      "inverse_penetration",
      "selection_rate",
      "selection_rate2",
      "raking_ratio",
      "coefficient",
      "multilevel_bayes"
    )
  )
  expect_true(all(vapply(results, function(x) "flow_adj" %in% names(x), logical(1))))
  expect_equal(attr(results$multilevel_bayes, "model_engine"), "frequentist")
})

test_that("adjust_all_methods can fit a selected subset of methods", {
  results <- adjust_all_methods(
    mpd_od_df = simulated_mpd.od,
    coverage_df = simulated_coverage,
    benchmark_od_df = simulated_benchmark.od,
    covariates_df = simulated_covariates,
    methods = c("inverse_penetration", "coefficient"),
    covariate_col = "income_norm"
  )

  expect_named(results, c("inverse_penetration", "coefficient"))
  expect_true(all(vapply(results, function(x) "flow_adj" %in% names(x), logical(1))))
})

test_that("adjust_all_methods requires distances for the multilevel method", {
  expect_error(
    adjust_all_methods(
      mpd_od_df = simulated_mpd.od,
      coverage_df = simulated_coverage,
      benchmark_od_df = simulated_benchmark.od,
      covariates_df = simulated_covariates,
      covariate_col = "income_norm",
      methods = "multilevel_bayes"
    ),
    "`distance_df` is required"
  )
})
