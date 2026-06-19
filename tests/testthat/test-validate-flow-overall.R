test_that("validate_flow_overall returns summary metrics and joined data", {
  adj_df <- data.frame(
    origin = c("A", "A", "B"),
    destination = c("B", "C", "C"),
    flow_adj = c(10, 20, 30),
    mpd_source = c("src1", "src1", "src2")
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "A", "B"),
    destination = c("B", "C", "C"),
    flow = c(12, 18, 33)
  )

  res <- validate_flow_overall(
    adj_df = adj_df,
    benchmark_od_df = benchmark_od_df,
    by_source = TRUE,
    method_name = "demo_method"
  )

  x <- adj_df$flow_adj
  y <- benchmark_od_df$flow

  expect_equal(res$method, "demo_method")
  expect_equal(res$comparison, "adjusted_vs_benchmark")
  expect_equal(res$comparison_label, "Adjusted vs benchmark")
  expect_equal(res$x_series, "adjusted")
  expect_equal(res$y_series, "benchmark")
  expect_equal(res$signed_residual, "benchmark - adjusted")
  expect_equal(res$n, 3)
  expect_equal(res$sum_x, sum(x))
  expect_equal(res$sum_y, sum(y))
  expect_equal(res$sum_adj, sum(x))
  expect_equal(res$sum_bench, sum(y))
  expect_equal(res$pearson_r, stats::cor(x, y, method = "pearson"))
  expect_equal(res$spearman_rho, stats::cor(x, y, method = "spearman"))
  expect_equal(res$rmse, sqrt(mean((y - x)^2)))
  expect_equal(res$mae, mean(abs(y - x)))
  expect_equal(res$mape, mean(abs((y - x) / y)))
  expect_true(all(c("origin", "destination", "flow_adj", "flow_bench") %in% names(res$data)))
  expect_equal(nrow(res$by_source), 2)
})

test_that("validate_flow_overall computes requested comparison metrics", {
  adj_df <- data.frame(
    origin = c("A", "A", "B"),
    destination = c("B", "C", "C"),
    flow = c(100, 80, 40),
    flow_adj = c(90, 75, 50)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "A", "B"),
    destination = c("B", "C", "C"),
    flow = c(95, 70, 55)
  )

  res <- validate_flow_overall(
    adj_df,
    benchmark_od_df,
    comparisons = "all",
    return_joined = TRUE
  )

  expect_equal(
    res$summary$comparison,
    c("adjusted_vs_benchmark", "raw_vs_benchmark", "raw_vs_adjusted")
  )
  expect_equal(
    res$summary$signed_residual,
    c("benchmark - adjusted", "benchmark - raw", "adjusted - raw")
  )
  expect_equal(
    res$summary$mean_error,
    c(mean(c(5, -5, 5)), mean(c(-5, -10, 15)), mean(c(-10, -5, 10)))
  )
  expect_equal(
    res$data$residual[res$data$comparison == "raw_vs_adjusted"],
    c(-10, -5, 10)
  )
})

test_that("validate_flow_benchmark remains an alias for validate_flow_overall", {
  adj_df <- data.frame(
    origin = c("A", "B"),
    destination = c("B", "C"),
    flow_adj = c(10, 20)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "B"),
    destination = c("B", "C"),
    flow = c(12, 22)
  )

  expect_equal(
    validate_flow_benchmark(adj_df, benchmark_od_df, method_name = "alias_check"),
    validate_flow_overall(adj_df, benchmark_od_df, method_name = "alias_check")
  )
})
