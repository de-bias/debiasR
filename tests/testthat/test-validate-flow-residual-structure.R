test_that("validate_flow_residual_structure returns correlations and Morans I", {
  adj_df <- data.frame(
    origin = c("A", "B", "C", "D", "A", "B", "C", "D"),
    destination = c("X", "X", "X", "X", "Y", "Y", "Y", "Y"),
    flow = c(9, 21, 31, 41, 10, 20, 30, 40),
    flow_adj = c(11, 21, 29, 39, 10, 20, 30, 40)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "B", "C", "D", "A", "B", "C", "D"),
    destination = c("X", "X", "X", "X", "Y", "Y", "Y", "Y"),
    flow = c(10, 20, 30, 40, 10, 20, 30, 40)
  )

  area_neighbors <- data.frame(
    area = c("A", "B", "C", "D"),
    neighbor = c("B", "A", "D", "C")
  )

  covariates <- data.frame(
    area = c("A", "B", "C", "D"),
    cov = c(1, 2, 3, 4)
  )

  res <- validate_flow_residual_structure(
    adj_df = adj_df,
    benchmark_od_df = benchmark_od_df,
    method_name = "demo_method",
    area_neighbors = area_neighbors,
    covariate_df = covariates,
    covariate_col = "cov"
  )

  expect_true(all(c(
    "summary",
    "flow_correlation",
    "moran_i",
    "covariate_correlation",
    "od_level",
    "area_level",
    "map_data",
    "covariate_data"
  ) %in% names(res)))

  expect_equal(res$summary$method, "demo_method")
  expect_equal(res$summary$comparison, "adjusted_vs_benchmark")
  expect_equal(res$summary$comparison_label, "Adjusted vs benchmark")
  expect_equal(res$summary$n_od_pairs, 8)
  expect_equal(res$summary$n_areas, 4)
  expect_equal(res$area_level$n_od_pairs, c(2, 2, 2, 2))
  expect_equal(res$area_level$selected_residual, c(-0.5, -0.5, 0.5, 0.5))
  expect_equal(res$moran_i$moran_i, 1)
  expect_equal(round(res$flow_correlation$pearson_r, 4), 0.6325)
  expect_equal(round(res$covariate_correlation$pearson_r, 4), 0.8944)
})

test_that("validate_flow_residual_structure supports comparison selection", {
  adj_df <- data.frame(
    origin = c("A", "B"),
    destination = "X",
    flow = c(10, 20),
    flow_adj = c(12, 18)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "B"),
    destination = "X",
    flow = c(11, 19)
  )

  res <- validate_flow_residual_structure(
    adj_df,
    benchmark_od_df,
    comparison = "raw_vs_adjusted"
  )

  expect_equal(res$summary$comparison, "raw_vs_adjusted")
  expect_equal(res$summary$residual_type, "adjustment")
  expect_equal(res$od_level$selected_residual, c(2, -2))
})

test_that("validate_flow_residual_structure can return ggplot diagnostics", {
  testthat::skip_if_not_installed("ggplot2")

  adj_df <- data.frame(
    origin = c("A", "B", "C", "D"),
    destination = "X",
    flow = c(9, 21, 31, 41),
    flow_adj = c(11, 21, 29, 39)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "B", "C", "D"),
    destination = "X",
    flow = c(10, 20, 30, 40)
  )

  coordinates <- data.frame(
    area = c("A", "B", "C", "D"),
    x = c(0, 1, 0, 1),
    y = c(1, 1, 0, 0)
  )

  res <- validate_flow_residual_structure(
    adj_df = adj_df,
    benchmark_od_df = benchmark_od_df,
    geometry_df = coordinates,
    x_col = "x",
    y_col = "y",
    make_plots = TRUE
  )

  expect_s3_class(res$plots$residual_reduction_distribution, "ggplot")
  expect_s3_class(res$plots$residual_vs_benchmark, "ggplot")
  expect_s3_class(res$plots$residual_map, "ggplot")
})

test_that("validate_flow_residual_structure validates optional inputs", {
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
    validate_flow_residual_structure(
      adj_df,
      benchmark_od_df,
      covariate_col = "income"
    ),
    "`covariate_df` is required"
  )

  expect_error(
    validate_flow_residual_structure(
      adj_df,
      benchmark_od_df,
      area_neighbors = data.frame(area = "A")
    ),
    "`area_neighbors` must contain"
  )
})

test_that("validate_flow_residual_structure local Moran is opt-in", {
  adj_df <- data.frame(
    origin = c("A", "B"),
    destination = "X",
    flow = c(10, 10),
    flow_adj = c(11, 9)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "B"),
    destination = "X",
    flow = c(10, 10)
  )

  res <- validate_flow_residual_structure(adj_df, benchmark_od_df)

  expect_false("local_moran" %in% names(res))
  expect_false("lisa_cluster" %in% names(res$map_data))
})

test_that("validate_flow_residual_structure returns Local Moran and LISA clusters", {
  adj_df <- data.frame(
    origin = c("A", "B", "C", "D", "E"),
    destination = "X",
    flow = 10,
    flow_adj = c(12, 12, 8, 8, 10)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "B", "C", "D", "E"),
    destination = "X",
    flow = 10
  )

  area_neighbors <- data.frame(
    area = c("A", "B", "C", "D"),
    neighbor = c("B", "A", "D", "C")
  )

  res <- validate_flow_residual_structure(
    adj_df = adj_df,
    benchmark_od_df = benchmark_od_df,
    area_neighbors = area_neighbors,
    local_moran = TRUE,
    local_moran_nsim = 19,
    local_moran_alpha = 1,
    local_moran_p_adjust = "none",
    local_moran_seed = 123
  )

  expect_true("local_moran" %in% names(res))
  expect_true("lisa_cluster" %in% names(res$map_data))
  expect_equal(res$local_moran$area, c("A", "B", "C", "D", "E"))
  expect_equal(round(res$local_moran$residual_z, 3), c(-1.118, -1.118, 1.118, 1.118, 0))
  expect_equal(round(res$local_moran$spatial_lag_z, 3), c(-1.118, -1.118, 1.118, 1.118, NA))
  expect_equal(res$local_moran$local_moran_i, c(1.25, 1.25, 1.25, 1.25, NA))
  expect_equal(
    res$local_moran$lisa_cluster,
    c("low-low", "low-low", "high-high", "high-high", "no neighbours")
  )
})

test_that("validate_flow_residual_structure Local Moran p-values are reproducible", {
  adj_df <- data.frame(
    origin = c("A", "B", "C", "D"),
    destination = "X",
    flow = 10,
    flow_adj = c(12, 12, 8, 8)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "B", "C", "D"),
    destination = "X",
    flow = 10
  )

  area_neighbors <- data.frame(
    area = c("A", "B", "C", "D"),
    neighbor = c("B", "A", "D", "C")
  )

  res1 <- validate_flow_residual_structure(
    adj_df,
    benchmark_od_df,
    area_neighbors = area_neighbors,
    local_moran = TRUE,
    local_moran_nsim = 31,
    local_moran_seed = 2026
  )
  res2 <- validate_flow_residual_structure(
    adj_df,
    benchmark_od_df,
    area_neighbors = area_neighbors,
    local_moran = TRUE,
    local_moran_nsim = 31,
    local_moran_seed = 2026
  )

  expect_equal(res1$local_moran$p_value, res2$local_moran$p_value)
  expect_equal(res1$local_moran$p_adjusted, res2$local_moran$p_adjusted)
})

test_that("validate_flow_residual_structure Local Moran handles zero variance", {
  adj_df <- data.frame(
    origin = c("A", "B", "C"),
    destination = "X",
    flow = 10,
    flow_adj = c(11, 11, 11)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "B", "C"),
    destination = "X",
    flow = 10
  )

  area_neighbors <- data.frame(
    area = c("A", "B", "C"),
    neighbor = c("B", "C", "A")
  )

  res <- validate_flow_residual_structure(
    adj_df,
    benchmark_od_df,
    area_neighbors = area_neighbors,
    local_moran = TRUE
  )

  expect_true(all(is.na(res$local_moran$local_moran_i)))
  expect_equal(res$local_moran$lisa_cluster, rep("undefined", 3))
})

test_that("validate_flow_residual_structure Local Moran supports weighted links", {
  adj_df <- data.frame(
    origin = c("A", "B", "C"),
    destination = "X",
    flow = 10,
    flow_adj = c(12, 10, 8)
  )

  benchmark_od_df <- data.frame(
    origin = c("A", "B", "C"),
    destination = "X",
    flow = 10
  )

  area_neighbors <- data.frame(
    area = c("A", "A", "B", "C"),
    neighbor = c("B", "C", "A", "A"),
    weight = c(1, 3, 2, 4)
  )

  res <- validate_flow_residual_structure(
    adj_df,
    benchmark_od_df,
    area_neighbors = area_neighbors,
    weight_col = "weight",
    local_moran = TRUE,
    local_moran_nsim = 19,
    local_moran_seed = 99
  )

  a_row <- res$local_moran[res$local_moran$area == "A", ]

  expect_equal(a_row$n_neighbors_used, 2L)
  expect_equal(a_row$local_weight_sum, 4)
  expect_equal(a_row$neighbor_lag_mean, 1.5)
  expect_equal(round(a_row$spatial_lag_z, 3), 0.919)
  expect_equal(a_row$local_moran_i, -4.5)
})

test_that("validate_flow_residual_structure validates Local Moran options", {
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
    validate_flow_residual_structure(
      adj_df,
      benchmark_od_df,
      local_moran = NA
    ),
    "`local_moran` must be `TRUE` or `FALSE`"
  )

  expect_error(
    validate_flow_residual_structure(
      adj_df,
      benchmark_od_df,
      local_moran = TRUE,
      local_moran_nsim = 0
    ),
    "`local_moran_nsim` must be a positive whole number"
  )

  expect_error(
    validate_flow_residual_structure(
      adj_df,
      benchmark_od_df,
      local_moran = TRUE,
      local_moran_p_adjust = "not_a_method"
    ),
    "`local_moran_p_adjust` must be one of"
  )
})
