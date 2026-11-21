# tests/testthat/test-adjust-coefficient.R
# Tests for adjust_coefficient()

test_that("adjust_coefficient OLS returns valid structure and constant factor", {

  data(simulated_mpd.od)
  data(simulated_benchmark.od)

  res <- adjust_coefficient(
    mpd_od_df       = simulated_mpd.od,
    benchmark_od_df = simulated_benchmark.od,
    model_family    = "ols",
    level           = "od",
    fit_intercept   = FALSE,
    by_source       = FALSE
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("origin", "destination", "flow", "flow_adj", "coef_factor") %in% names(res)))

  cf <- unique(res$coef_factor[is.finite(res$coef_factor)])
  expect_length(cf, 1L)
  expect_true(is.finite(cf))
  expect_true(all(res$flow_adj == res$flow * res$coef_factor | is.na(res$flow_adj)))

  mod <- attr(res, "model")
  expect_true(is.data.frame(mod))
  expect_true(all(c("beta", "family", "level") %in% names(mod)))
  expect_equal(mod$family[1], "ols")
})

test_that("adjust_coefficient Poisson returns valid structure and positive factor", {

  data(simulated_mpd.od)
  data(simulated_benchmark.od)

  res <- adjust_coefficient(
    mpd_od_df       = simulated_mpd.od,
    benchmark_od_df = simulated_benchmark.od,
    model_family    = "poisson",
    level           = "od",
    by_source       = FALSE
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("origin", "destination", "flow", "flow_adj", "coef_factor") %in% names(res)))

  cf <- unique(res$coef_factor[is.finite(res$coef_factor)])
  expect_length(cf, 1L)
  expect_true(cf > 0)

  mod <- attr(res, "model")
  expect_true(is.data.frame(mod))
  expect_equal(mod$family[1], "poisson")
})

test_that("adjust_coefficient NegBin runs when MASS is available", {

  skip_if_not_installed("MASS")

  data(simulated_mpd.od)
  data(simulated_benchmark.od)

  res <- adjust_coefficient(
    mpd_od_df       = simulated_mpd.od,
    benchmark_od_df = simulated_benchmark.od,
    model_family    = "negbin",
    level           = "od",
    by_source       = FALSE
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("origin", "destination", "flow", "flow_adj", "coef_factor") %in% names(res)))

  cf <- unique(res$coef_factor[is.finite(res$coef_factor)])
  expect_length(cf, 1L)
  expect_true(cf > 0)

  mod <- attr(res, "model")
  expect_true(is.data.frame(mod))
  expect_equal(mod$family[1], "negbin")
})

test_that("adjust_coefficient ZINB runs when pscl is available", {

  skip_if_not_installed("pscl")

  data(simulated_mpd.od)
  data(simulated_benchmark.od)

  res <- adjust_coefficient(
    mpd_od_df       = simulated_mpd.od,
    benchmark_od_df = simulated_benchmark.od,
    model_family    = "zinb",
    level           = "od",
    by_source       = FALSE
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("origin", "destination", "flow", "flow_adj", "coef_factor") %in% names(res)))

  cf <- unique(res$coef_factor[is.finite(res$coef_factor)])
  expect_length(cf, 1L)
  expect_true(cf > 0)

  mod <- attr(res, "model")
  expect_true(is.data.frame(mod))
  expect_equal(mod$family[1], "zinb")
})

test_that("adjust_coefficient errors cleanly on invalid settings", {

  data(simulated_mpd.od)
  data(simulated_benchmark.od)

  # by_source = TRUE without mpd_source in both
  expect_error(
    adjust_coefficient(
      mpd_od_df       = simulated_mpd.od,
      benchmark_od_df = simulated_benchmark.od,
      model_family    = "ols",
      level           = "od",
      by_source       = TRUE
    ),
    "by_source = TRUE"
  )

  # missing required columns in mpd_od_df
  bad_mpd <- simulated_mpd.od
  bad_mpd$origin <- NULL

  expect_error(
    adjust_coefficient(
      mpd_od_df       = bad_mpd,
      benchmark_od_df = simulated_benchmark.od,
      model_family    = "ols",
      level           = "od"
    ),
    "mpd_od_df"
  )

  # missing required columns in benchmark_od_df
  bad_bench <- simulated_benchmark.od
  bad_bench$destination <- NULL

  expect_error(
    adjust_coefficient(
      mpd_od_df       = simulated_mpd.od,
      benchmark_od_df = bad_bench,
      model_family    = "ols",
      level           = "od"
    ),
    "benchmark_od_df"
  )
})

test_that("OLS intercepts predict routes and are apportioned once per fitted margin", {
  raw <- data.frame(origin = LETTERS[1:3], destination = "X", flow = 1:3)
  bench <- transform(raw, flow = 5 + 2 * flow)
  od <- suppressWarnings(adjust_coefficient(raw, bench, fit_intercept = TRUE))
  expect_equal(od$flow_adj, c(7, 9, 11))
  expect_equal(od$coef_factor, od$flow_adj / raw$flow)
  raw <- rbind(raw, transform(raw, destination = "Y", flow = flow * 2))
  bench <- transform(raw, flow = 2 * flow + 2.5)
  src2 <- transform(raw, mpd_source = "two")
  raw <- rbind(transform(raw, mpd_source = "one"), src2)
  bench <- rbind(transform(bench, mpd_source = "one"),
                 transform(src2, flow = 3 * flow + 1))
  pred <- suppressWarnings(adjust_coefficient(raw, bench, level = "origin",
                           fit_intercept = TRUE, by_source = TRUE))
  expect_equal(aggregate(flow_adj ~ origin + mpd_source, pred, sum)$flow_adj,
               aggregate(flow ~ origin + mpd_source, bench, sum)$flow)
})
