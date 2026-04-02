# tests/testthat/test-adjust-raking-ratio.R
# Tests for adjust_raking_ratio()

test_that("adjust_raking_ratio with benchmark OD (location-only) matches margins", {

  data(simulated_mpd.od)
  data(simulated_benchmark.od)

  res <- adjust_raking_ratio(
    mpd_od_df       = simulated_mpd.od,
    benchmark_od_df = simulated_benchmark.od,
    flow_col_bench  = "flow",
    max_iter        = 500,
    tol             = 1e-8
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("origin", "destination", "flow", "flow_adj", "weight_ipf") %in% names(res)))

  # Origin margins: adjusted vs benchmark
  adj_o <- aggregate(flow_adj ~ origin, data = res, sum)
  bench_o <- aggregate(flow ~ origin, data = simulated_benchmark.od, sum)
  cmp_o <- merge(adj_o, bench_o, by = "origin", all = TRUE)
  expect_equal(cmp_o$flow_adj, cmp_o$flow, tolerance = 1e-6)

  # Destination margins: adjusted vs benchmark
  adj_d <- aggregate(flow_adj ~ destination, data = res, sum)
  bench_d <- aggregate(flow ~ destination, data = simulated_benchmark.od, sum)
  cmp_d <- merge(adj_d, bench_d, by = "destination", all = TRUE)
  expect_equal(cmp_d$flow_adj, cmp_d$flow, tolerance = 1e-6)

  expect_false(is.null(attr(res, "ipf_converged")))
  expect_false(is.null(attr(res, "ipf_iterations")))
})

test_that("adjust_raking_ratio matches explicit origin targets (origin-only)", {

  data(simulated_mpd.od)

  # Origin-only targets: scaled existing margins
  orig_marg <- aggregate(flow ~ origin, data = simulated_mpd.od, sum)

  origin_targets <- data.frame(
    origin = orig_marg$origin,
    target = orig_marg$flow * 1.10
  )

  res <- adjust_raking_ratio(
    mpd_od_df      = simulated_mpd.od,
    origin_targets = origin_targets,
    max_iter       = 500,
    tol            = 1e-8
  )

  # Check origin margins match targets
  adj_o <- aggregate(flow_adj ~ origin, data = res, sum)
  cmp_o <- merge(adj_o, origin_targets, by = "origin", all = TRUE)
  expect_equal(cmp_o$flow_adj, cmp_o$target, tolerance = 1e-6)

  # Destination margins unconstrained in this test
})

test_that("adjust_raking_ratio works with group_cols (origin-only, stratified)", {

  data(simulated_mpd.od)

  # Synthetic grouping based on origin
  toy_mpd_g <- simulated_mpd.od
  toy_mpd_g$age_group <- ifelse(
    as.integer(factor(toy_mpd_g$origin)) %% 2L == 0L,
    "young", "old"
  )

  # Origin targets by (origin, age_group), scaled by 5%
  orig_marg_g <- aggregate(
    flow ~ origin + age_group,
    data = toy_mpd_g,
    sum
  )

  origin_targets <- data.frame(
    origin    = orig_marg_g$origin,
    age_group = orig_marg_g$age_group,
    target    = orig_marg_g$flow * 1.05
  )

  res <- adjust_raking_ratio(
    mpd_od_df      = toy_mpd_g,
    origin_targets = origin_targets,
    group_cols     = "age_group",
    max_iter       = 500,
    tol            = 1e-8
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("age_group", "flow_adj", "weight_ipf") %in% names(res)))

  # For each age_group, origin margins match group-specific targets
  for (g in unique(res$age_group)) {
    sub_res <- subset(res, age_group == g)
    sub_tar <- subset(origin_targets, age_group == g)

    adj_o <- aggregate(flow_adj ~ origin, data = sub_res, sum)
    cmp_o <- merge(adj_o, sub_tar, by = "origin", all = TRUE)

    expect_equal(cmp_o$flow_adj, cmp_o$target, tolerance = 1e-6)
  }
})

test_that("adjust_raking_ratio errors cleanly on bad inputs", {

  data(simulated_mpd.od)

  # 1) Missing required columns in mpd_od_df
  bad <- simulated_mpd.od
  bad$origin <- NULL

  expect_error(
    adjust_raking_ratio(
      mpd_od_df      = bad,
      origin_targets = data.frame(origin = "A", target = 100)
    ),
    "mpd_od_df"
  )

  # 2) No targets and no benchmark
  expect_error(
    adjust_raking_ratio(
      mpd_od_df = simulated_mpd.od
    ),
    "Provide at least one"
  )

  # 3) group_cols specified but not present in mpd_od_df
  expect_error(
    adjust_raking_ratio(
      mpd_od_df      = simulated_mpd.od,
      origin_targets = data.frame(origin = "A", age_group = "x", target = 10),
      group_cols     = "age_group"
    ),
    "group_cols"
  )
})
