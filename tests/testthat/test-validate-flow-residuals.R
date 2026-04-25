test_that("validate_flow_residuals returns richer diagnostics and top worst rows", {
  adj_df <- data.frame(
    origin = c("A", "A", "B"),
    destination = c("B", "C", "C"),
    flow = c(100, 50, 10),
    flow_adj = c(90, 40, 30)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "A", "B"),
    destination = c("B", "C", "C"),
    flow = c(80, 80, 20)
  )

  res <- validate_flow_residuals(adj_df, benchmark_od_df, top_n = 2)

  expect_true(all(c("summary", "data", "top_worst") %in% names(res)))
  expect_true(all(c(
    "residual_mpd_benchmark",
    "residual_adj_benchmark",
    "abs_residual_mpd_benchmark",
    "abs_residual_adj_benchmark",
    "pct_residual_mpd_benchmark",
    "pct_residual_adj_benchmark",
    "residual_sd_adj_benchmark",
    "residual_adj_benchmark_sd_score",
    "abs_residual_adj_benchmark_sd_score",
    "residual_adj_over_1sd",
    "residual_adj_over_2sd",
    "residual_adj_over_3sd",
    "abs_residual_improvement",
    "pct_residual_improvement",
    "improvement_flag"
  ) %in% names(res$data)))

  expect_equal(res$data$residual_mpd_benchmark, c(20, -30, -10))
  expect_equal(res$data$residual_adj_benchmark, c(10, -40, 10))
  expect_equal(res$data$abs_residual_mpd_benchmark, c(20, 30, 10))
  expect_equal(res$data$abs_residual_adj_benchmark, c(10, 40, 10))
  expect_equal(res$data$abs_residual_improvement, c(10, -10, 0))
  expect_equal(res$data$improvement_flag, c("improved", "worsened", "unchanged"))

  expect_equal(round(res$data$pct_residual_mpd_benchmark, 1), c(25.0, -37.5, -50.0))
  expect_equal(round(res$data$pct_residual_adj_benchmark, 1), c(12.5, -50.0, 50.0))
  expect_equal(round(res$data$pct_residual_improvement, 1), c(50.0, -33.3, 0.0))
  expect_equal(round(res$data$residual_sd_adj_benchmark, 3), rep(28.868, 3))
  expect_equal(round(res$data$abs_residual_adj_benchmark_sd_score, 3), c(0.346, 1.386, 0.346))
  expect_equal(res$data$residual_adj_over_1sd, c(FALSE, TRUE, FALSE))
  expect_equal(res$data$residual_adj_over_2sd, c(FALSE, FALSE, FALSE))
  expect_equal(res$data$residual_adj_over_3sd, c(FALSE, FALSE, FALSE))

  expect_equal(res$summary$n, 3)
  expect_equal(round(res$summary$mean_abs_residual_mpd_benchmark, 1), 20.0)
  expect_equal(round(res$summary$mean_abs_residual_adj_benchmark, 1), 20.0)
  expect_equal(res$summary$share_improved, 1 / 3, tolerance = 1e-8)
  expect_equal(res$summary$share_worsened, 1 / 3, tolerance = 1e-8)
  expect_equal(res$summary$share_unchanged, 1 / 3, tolerance = 1e-8)
  expect_equal(res$summary$share_residual_adj_over_1sd, 1 / 3, tolerance = 1e-8)
  expect_equal(res$summary$share_residual_adj_over_2sd, 0)
  expect_equal(res$summary$share_residual_adj_over_3sd, 0)

  expect_equal(nrow(res$top_worst), 2)
  expect_equal(res$top_worst$origin[1], "A")
  expect_equal(res$top_worst$destination[1], "C")
  expect_equal(res$top_worst$abs_residual_adj_benchmark[1], 40)
})

test_that("validate_flow_residuals handles zero benchmark flow percentages", {
  adj_df <- data.frame(
    origin = "A",
    destination = "B",
    flow = 10,
    flow_adj = 12
  )

  benchmark_od_df <- data.frame(
    origin = "A",
    destination = "B",
    flow = 0
  )

  res <- validate_flow_residuals(adj_df, benchmark_od_df)

  expect_true(is.na(res$data$pct_residual_mpd_benchmark))
  expect_true(is.na(res$data$pct_residual_adj_benchmark))
})

test_that("validate_flow_residuals validates top_n", {
  adj_df <- data.frame(
    origin = "A",
    destination = "B",
    flow = 10,
    flow_adj = 12
  )

  benchmark_od_df <- data.frame(
    origin = "A",
    destination = "B",
    flow = 11
  )

  expect_error(
    validate_flow_residuals(adj_df, benchmark_od_df, top_n = 0),
    "`top_n` must be a single positive integer."
  )
})
