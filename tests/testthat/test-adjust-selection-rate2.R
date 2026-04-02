# tests/testthat/test-adjust-selection-rate2.R
# Tests for adjust_selection_rate2()

test_that("adjust_selection_rate2 basic origin weighting works", {

  data(simulated_mpd.od)
  data(simulated_coverage)

  res_o <- adjust_selection_rate2(
    mpd_od_df   = simulated_mpd.od,
    coverage_df = simulated_coverage,
    weight_by   = "origin",
    k           = 1
  )

  expect_s3_class(res_o, "tbl_df")
  expect_true(all(c("origin", "destination", "flow", "flow_adj") %in% names(res_o)))
  expect_true("weight_origin" %in% names(res_o))
  expect_true(all(res_o$flow_adj >= 0 | is.na(res_o$flow_adj)))
})

test_that("adjust_selection_rate2 destination and both-side weighting work", {

  data(simulated_mpd.od)
  data(simulated_coverage)

  # destination-only
  res_d <- adjust_selection_rate2(
    mpd_od_df   = simulated_mpd.od,
    coverage_df = simulated_coverage,
    weight_by   = "destination",
    k           = 1
  )

  expect_s3_class(res_d, "tbl_df")
  expect_true("weight_destination" %in% names(res_d))
  expect_true(all(res_d$flow_adj >= 0 | is.na(res_d$flow_adj)))

  # both origin and destination
  res_b <- adjust_selection_rate2(
    mpd_od_df   = simulated_mpd.od,
    coverage_df = simulated_coverage,
    weight_by   = "both",
    k           = 1
  )

  expect_s3_class(res_b, "tbl_df")
  expect_true(all(c("weight_origin", "weight_destination") %in% names(res_b)))
  expect_true(all(res_b$flow_adj >= 0 | is.na(res_b$flow_adj)))
})

test_that("adjust_selection_rate2 calibrates k when benchmark provided", {

  data(simulated_mpd.od)
  data(simulated_coverage)
  data(simulated_benchmark.od)

  k_grid <- seq(0.5, 2, by = 0.5)

  res_cal <- adjust_selection_rate2(
    mpd_od_df           = simulated_mpd.od,
    coverage_df         = simulated_coverage,
    weight_by           = "origin",
    k                   = NULL,
    k_grid              = k_grid,
    benchmark_od_df     = simulated_benchmark.od,
    flow_col_bench      = "flow",
    calibration_aggregate = "origin"
  )

  # structure
  expect_s3_class(res_cal, "tbl_df")
  expect_true("flow_adj" %in% names(res_cal))

  # calibration attributes
  k_used  <- attr(res_cal, "k")
  k_diag  <- attr(res_cal, "k_calibration")

  expect_false(is.null(k_used))
  expect_true(is.numeric(k_used))
  expect_false(is.null(k_diag))
  expect_true(all(c("k", "loss") %in% names(k_diag)))

  # k_used is within grid range
  expect_true(k_used >= min(k_diag$k) - 1e-12)
  expect_true(k_used <= max(k_diag$k) + 1e-12)

  # k_used corresponds to minimum loss
  best_idx <- which.min(k_diag$loss)
  expect_equal(k_used, k_diag$k[best_idx])

  # Using fixed k_used reproduces same adjusted flows
  res_fixed <- adjust_selection_rate2(
    mpd_od_df   = simulated_mpd.od,
    coverage_df = simulated_coverage,
    weight_by   = "origin",
    k           = k_used
  )

  expect_equal(
    res_fixed$flow_adj,
    res_cal$flow_adj,
    tolerance = 1e-8
  )
})

test_that("adjust_selection_rate2 handles group_cols when present (synthetic check)", {

  data(simulated_mpd.od)
  data(simulated_coverage)

  # Create a simple synthetic grouping to test plumbing
  toy_mpd_g <- simulated_mpd.od
  toy_mpd_g$age_group <- ifelse(as.integer(factor(toy_mpd_g$origin)) %% 2 == 0, "young", "old")

  toy_cov_g <- simulated_coverage
  toy_cov_g$age_group <- ifelse(as.integer(factor(toy_cov_g$origin)) %% 2 == 0, "young", "old")

  res_g <- adjust_selection_rate2(
    mpd_od_df   = toy_mpd_g,
    coverage_df = toy_cov_g,
    weight_by   = "origin",
    group_cols  = "age_group",
    k           = 1
  )

  expect_s3_class(res_g, "tbl_df")
  expect_true("age_group" %in% names(res_g))
  expect_true("weight_origin" %in% names(res_g))
})

test_that("adjust_selection_rate2 errors cleanly with bad inputs", {

  data(simulated_mpd.od)
  data(simulated_coverage)

  # Missing required columns in mpd_od_df
  bad_mpd <- simulated_mpd.od
  bad_mpd$origin <- NULL

  expect_error(
    adjust_selection_rate2(
      mpd_od_df   = bad_mpd,
      coverage_df = simulated_coverage,
      weight_by   = "origin",
      k           = 1
    ),
    "mpd_od_df"
  )

  # Invalid k
  expect_error(
    adjust_selection_rate2(
      mpd_od_df   = simulated_mpd.od,
      coverage_df = simulated_coverage,
      weight_by   = "origin",
      k           = -1
    ),
    "positive scalar"
  )

  # group_cols not found
  expect_error(
    adjust_selection_rate2(
      mpd_od_df   = simulated_mpd.od,
      coverage_df = simulated_coverage,
      weight_by   = "origin",
      group_cols  = "age_group",
      k           = 1
    ),
    "group_cols"
  )
})
