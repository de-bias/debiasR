test_that("validate_flow_distribution returns zero divergence for identical distributions", {
  adj_df <- data.frame(
    origin = c("A", "A", "B", "B"),
    destination = c("X", "Y", "X", "Y"),
    flow_adj = c(30, 70, 10, 90)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "A", "B", "B"),
    destination = c("X", "Y", "X", "Y"),
    flow = c(30, 70, 10, 90)
  )

  res <- validate_flow_distribution(
    adj_df,
    benchmark_od_df,
    method_name = "identity",
    weight_by = "benchmark_origin_total"
  )

  expect_true(all(c("summary", "origin_level") %in% names(res)))
  expect_equal(res$summary$method, "identity")
  expect_equal(res$summary$n_origins, 2)
  expect_equal(res$summary$n_origins_used, 2)
  expect_equal(res$origin_level$kl_origin, c(0, 0), tolerance = 1e-12)
  expect_equal(res$origin_level$jsd_origin, c(0, 0), tolerance = 1e-12)
  expect_equal(res$summary$kl_mean, 0, tolerance = 1e-12)
  expect_equal(res$summary$jsd_weighted_mean, 0, tolerance = 1e-12)
})

test_that("validate_flow_distribution uses union support for missing destinations", {
  adj_df <- data.frame(
    origin = c("A", "A"),
    destination = c("X", "Z"),
    flow_adj = c(80, 20)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "A"),
    destination = c("X", "Y"),
    flow = c(50, 50)
  )

  res <- validate_flow_distribution(adj_df, benchmark_od_df)

  expect_equal(res$origin_level$n_destinations, 3)
  expect_true(is.finite(res$origin_level$kl_origin))
  expect_true(is.finite(res$origin_level$jsd_origin))
  expect_gt(res$origin_level$kl_origin, 0)
  expect_gt(res$origin_level$jsd_origin, 0)
})

test_that("validate_flow_distribution handles zero adjusted flows with smoothing", {
  adj_df <- data.frame(
    origin = "A",
    destination = "X",
    flow_adj = 0
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "A"),
    destination = c("X", "Y"),
    flow = c(10, 5)
  )

  res <- validate_flow_distribution(adj_df, benchmark_od_df)

  expect_true(res$origin_level$zero_adj_total)
  expect_false(res$origin_level$zero_benchmark_total)
  expect_true(is.finite(res$origin_level$kl_origin))
  expect_true(is.finite(res$origin_level$jsd_origin))
})

test_that("validate_flow_distribution excludes zero benchmark origins from divergence summaries", {
  adj_df <- data.frame(
    origin = c("A", "B"),
    destination = c("X", "X"),
    flow_adj = c(10, 10)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "B"),
    destination = c("X", "X"),
    flow = c(10, 0)
  )

  res <- validate_flow_distribution(adj_df, benchmark_od_df)

  expect_equal(res$summary$n_origins, 2)
  expect_equal(res$summary$n_origins_used, 1)
  expect_true(is.na(res$origin_level$kl_origin[res$origin_level$origin == "B"]))
  expect_true(res$origin_level$zero_benchmark_total[res$origin_level$origin == "B"])
})

test_that("validate_flow_distribution weighted summaries use benchmark totals", {
  adj_df <- data.frame(
    origin = c("A", "A", "B", "B"),
    destination = c("X", "Y", "X", "Y"),
    flow_adj = c(70, 30, 20, 80)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "A", "B", "B"),
    destination = c("X", "Y", "X", "Y"),
    flow = c(50, 50, 90, 10)
  )

  res <- validate_flow_distribution(
    adj_df,
    benchmark_od_df,
    weight_by = "benchmark_origin_total"
  )

  expected_kl <- stats::weighted.mean(
    res$origin_level$kl_origin,
    res$origin_level$bench_origin_total
  )
  expected_jsd <- stats::weighted.mean(
    res$origin_level$jsd_origin,
    res$origin_level$bench_origin_total
  )

  expect_equal(res$summary$kl_weighted_mean, expected_kl, tolerance = 1e-12)
  expect_equal(res$summary$jsd_weighted_mean, expected_jsd, tolerance = 1e-12)
})

test_that("validate_flow_distribution validates required inputs", {
  adj_df <- data.frame(origin = "A", destination = "X", flow_adj = 1)
  benchmark_od_df <- data.frame(origin = "A", destination = "X", flow = 1)

  expect_error(
    validate_flow_distribution(adj_df, benchmark_od_df, epsilon = 0),
    "`epsilon` must be a single positive finite number."
  )
  expect_error(
    validate_flow_distribution(adj_df[, c("origin", "destination")], benchmark_od_df),
    "`adj_df` must contain:"
  )
})

test_that("validate_flow_distribution computes all requested pairwise comparisons", {
  adj_df <- data.frame(
    origin = c("A", "A", "B", "B"),
    destination = c("X", "Y", "X", "Y"),
    flow = c(20, 80, 50, 50),
    flow_adj = c(30, 70, 10, 90)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "A", "B", "B"),
    destination = c("X", "Y", "X", "Y"),
    flow = c(30, 70, 10, 90)
  )

  res <- validate_flow_distribution(
    adj_df,
    benchmark_od_df,
    comparisons = "all",
    return_od_level = TRUE
  )

  expect_equal(
    sort(res$summary$comparison),
    sort(c("raw_vs_benchmark", "adjusted_vs_benchmark", "raw_vs_adjusted"))
  )
  expect_equal(nrow(res$summary), 3)
  expect_equal(nrow(res$origin_level), 6)
  expect_equal(nrow(res$od_level), 12)

  adjusted_summary <- res$summary[
    res$summary$comparison == "adjusted_vs_benchmark",
  ]
  expect_equal(adjusted_summary$kl_mean, 0, tolerance = 1e-12)
  expect_equal(adjusted_summary$jsd_mean, 0, tolerance = 1e-12)

  raw_summary <- res$summary[
    res$summary$comparison == "raw_vs_benchmark",
  ]
  expect_gt(raw_summary$kl_mean, 0)
  expect_gt(raw_summary$jsd_mean, 0)
})

test_that("validate_flow_distribution requires raw MPD flow only for raw comparisons", {
  adj_df <- data.frame(
    origin = "A",
    destination = "X",
    flow_adj = 1
  )
  benchmark_od_df <- data.frame(origin = "A", destination = "X", flow = 1)

  expect_no_error(
    validate_flow_distribution(adj_df, benchmark_od_df)
  )
  expect_error(
    validate_flow_distribution(
      adj_df,
      benchmark_od_df,
      comparisons = "raw_vs_benchmark"
    ),
    "`adj_df` must contain:"
  )
})

test_that("validate_flow_distribution supports custom raw adjusted and benchmark columns", {
  adj_df <- data.frame(
    origin = c("A", "A"),
    destination = c("X", "Y"),
    raw = c(40, 60),
    adjusted = c(50, 50)
  )
  benchmark_od_df <- data.frame(
    origin = c("A", "A"),
    destination = c("X", "Y"),
    benchmark = c(50, 50)
  )

  res <- validate_flow_distribution(
    adj_df,
    benchmark_od_df,
    flow_col_mpd = "raw",
    flow_col_adj = "adjusted",
    flow_col_bench = "benchmark",
    comparisons = c("raw_vs_benchmark", "adjusted_vs_benchmark")
  )

  expect_equal(nrow(res$summary), 2)
  expect_equal(
    res$summary$kl_mean[res$summary$comparison == "adjusted_vs_benchmark"],
    0,
    tolerance = 1e-12
  )
  expect_gt(
    res$summary$kl_mean[res$summary$comparison == "raw_vs_benchmark"],
    0
  )
})
