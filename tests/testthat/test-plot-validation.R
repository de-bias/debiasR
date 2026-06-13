make_validation_plot_fixture <- function() {
  benchmark_od_df <- data.frame(
    origin = c("A", "A", "B", "B", "C", "C"),
    destination = c("B", "C", "A", "C", "A", "B"),
    flow = c(80, 45, 30, 70, 55, 35)
  )

  method_a <- data.frame(
    origin = benchmark_od_df$origin,
    destination = benchmark_od_df$destination,
    flow = c(100, 35, 20, 90, 60, 25),
    flow_adj = c(88, 42, 26, 76, 56, 31)
  )

  method_b <- data.frame(
    origin = benchmark_od_df$origin,
    destination = benchmark_od_df$destination,
    flow = c(100, 35, 20, 90, 60, 25),
    flow_adj = c(94, 39, 22, 83, 58, 28)
  )

  residuals <- list(
    method_a = validate_flow_residuals(
      method_a,
      benchmark_od_df,
      method_name = "method_a"
    ),
    method_b = validate_flow_residuals(
      method_b,
      benchmark_od_df,
      method_name = "method_b"
    )
  )

  overall <- list(
    method_a = validate_flow_overall(
      method_a,
      benchmark_od_df,
      method_name = "method_a",
      return_joined = FALSE
    ),
    method_b = validate_flow_overall(
      method_b,
      benchmark_od_df,
      method_name = "method_b",
      return_joined = FALSE
    )
  )

  overall_all <- list(
    method_a = validate_flow_overall(
      method_a,
      benchmark_od_df,
      method_name = "method_a",
      comparisons = "all",
      return_joined = FALSE
    ),
    method_b = validate_flow_overall(
      method_b,
      benchmark_od_df,
      method_name = "method_b",
      comparisons = "all",
      return_joined = FALSE
    )
  )

  distributions <- list(
    method_a = validate_flow_distribution(
      method_a,
      benchmark_od_df,
      comparisons = "all",
      weight_by = "benchmark_origin_total",
      method_name = "method_a"
    ),
    method_b = validate_flow_distribution(
      method_b,
      benchmark_od_df,
      comparisons = "all",
      weight_by = "benchmark_origin_total",
      method_name = "method_b"
    )
  )

  area_neighbors <- data.frame(
    area = c("A", "B", "B", "C"),
    neighbor = c("B", "A", "C", "B")
  )

  structure <- list(
    method_a = validate_flow_residual_structure(
      method_a,
      benchmark_od_df,
      method_name = "method_a",
      area_neighbors = area_neighbors
    ),
    method_b = validate_flow_residual_structure(
      method_b,
      benchmark_od_df,
      method_name = "method_b",
      area_neighbors = area_neighbors
    )
  )

  structure_lisa <- list(
    method_a = validate_flow_residual_structure(
      method_a,
      benchmark_od_df,
      method_name = "method_a",
      area_neighbors = area_neighbors,
      local_moran = TRUE,
      local_moran_nsim = 19,
      local_moran_alpha = 1,
      local_moran_p_adjust = "none",
      local_moran_seed = 123
    ),
    method_b = validate_flow_residual_structure(
      method_b,
      benchmark_od_df,
      method_name = "method_b",
      area_neighbors = area_neighbors,
      local_moran = TRUE,
      local_moran_nsim = 19,
      local_moran_alpha = 1,
      local_moran_p_adjust = "none",
      local_moran_seed = 123
    )
  )

  list(
    overall = overall,
    overall_all = overall_all,
    residuals = residuals,
    distributions = distributions,
    structure = structure,
    structure_lisa = structure_lisa
  )
}

test_that("validation metric matrix plot returns a ggplot", {
  testthat::skip_if_not_installed("ggplot2")
  fixture <- make_validation_plot_fixture()

  plot <- plot_validation_metrics(fixture$overall)
  all_comparisons_plot <- plot_validation_metrics(
    fixture$overall_all,
    comparisons = "all",
    methods = "method_a"
  )
  selected_plot <- plot_validation_metrics(
    fixture$overall,
    error_measures = c("mae", "rmse"),
    methods = "method_a"
  )
  labelled_measure_plot <- plot_validation_metrics(
    fixture$overall,
    error_measures = c("MAE", "Root mean squared error"),
    methods = "Method B",
    method_labels = c(method_b = "Method B")
  )
  metric_cols_plot <- plot_validation_metrics(
    fixture$overall,
    metric_cols = "mape"
  )
  alias_plot <- plot_validate_flow_metrics(fixture$overall)

  expect_s3_class(plot, "ggplot")
  expect_s3_class(all_comparisons_plot, "ggplot")
  expect_s3_class(selected_plot, "ggplot")
  expect_s3_class(labelled_measure_plot, "ggplot")
  expect_s3_class(metric_cols_plot, "ggplot")
  expect_s3_class(alias_plot, "ggplot")
  expect_equal(unique(as.character(plot$data$comparison)), "adjusted_vs_benchmark")
  expect_equal(unique(as.character(all_comparisons_plot$data$method)), "method_a")
  expect_equal(length(unique(as.character(all_comparisons_plot$data$comparison))), 3)
  expect_equal(unique(as.character(selected_plot$data$metric)), c("mae", "rmse"))
  expect_equal(unique(as.character(selected_plot$data$method)), "method_a")
  expect_equal(unique(as.character(labelled_measure_plot$data$metric)), c("mae", "rmse"))
  expect_equal(unique(as.character(labelled_measure_plot$data$method)), "method_b")
  expect_equal(unique(as.character(metric_cols_plot$data$metric)), "mape")
  expect_error(
    plot_validation_metrics(
      fixture$overall,
      error_measures = "mae",
      metric_cols = "rmse"
    ),
    "Use only one of `error_measures` or `metric_cols`"
  )
  expect_true(any(vapply(plot$layers, function(layer) {
    inherits(layer$geom, "GeomTile")
  }, logical(1))))
})

test_that("validation comparison convention maps x y and signed residuals", {
  fixture <- make_validation_plot_fixture()
  residual_data <- debiasR:::.as_validate_residual_data(fixture$residuals)
  method_a <- residual_data[residual_data$method == "method_a", , drop = FALSE]

  comparison_data <- debiasR:::.validation_residual_long(
    method_a,
    comparisons = "all"
  )

  adjusted <- comparison_data[comparison_data$comparison == "adjusted_vs_benchmark", ]
  raw_benchmark <- comparison_data[comparison_data$comparison == "raw_vs_benchmark", ]
  raw_adjusted <- comparison_data[comparison_data$comparison == "raw_vs_adjusted", ]

  expect_equal(adjusted$x_flow, method_a$adj_flow)
  expect_equal(adjusted$y_flow, method_a$benchmark_flow)
  expect_equal(adjusted$difference, c(-8, 3, 4, -6, -1, 4))
  expect_equal(raw_benchmark$x_flow, method_a$mpd_flow)
  expect_equal(raw_benchmark$y_flow, method_a$benchmark_flow)
  expect_equal(raw_benchmark$difference, c(-20, 10, 10, -20, -5, 10))
  expect_equal(raw_adjusted$x_flow, method_a$mpd_flow)
  expect_equal(raw_adjusted$y_flow, method_a$adj_flow)
  expect_equal(raw_adjusted$difference, c(-12, 7, 6, -14, -4, 6))
  expect_equal(
    unique(as.character(comparison_data$comparison_label)),
    c("Adjusted vs benchmark", "Raw MPD vs benchmark", "Raw MPD vs adjusted")
  )
})

test_that("validation residual violin plot returns a ggplot", {
  testthat::skip_if_not_installed("ggplot2")
  fixture <- make_validation_plot_fixture()

  plot <- plot_validation_residuals(fixture$residuals)
  all_comparisons_plot <- plot_validation_residuals(
    fixture$residuals,
    comparisons = c(
      "adjusted_vs_benchmark",
      "raw_vs_benchmark",
      "raw_vs_adjusted"
    )
  )
  filtered_plot <- plot_validation_residuals(
    fixture$residuals,
    methods = "method_a"
  )
  labelled_filter_plot <- plot_validation_residuals(
    fixture$residuals,
    methods = "Method B",
    method_labels = c(method_b = "Method B")
  )
  absolute_plot <- plot_validation_residuals(
    fixture$residuals,
    residual = "absolute"
  )
  percent_plot <- plot_validation_residuals(
    fixture$residuals,
    residual = "percent"
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(unique(as.character(plot$data$comparison)), "adjusted_vs_benchmark")
  expect_equal(
    plot$labels$y,
    "Flow difference\n(Y - X: Benchmark - Adjusted)"
  )
  expect_equal(
    all_comparisons_plot$labels$y,
    "Flow difference\n(Y - X; X is first and Y second in each facet)"
  )
  expect_equal(
    absolute_plot$labels$y,
    "Absolute flow difference\n(|Y - X|: |Benchmark - Adjusted|)"
  )
  expect_equal(
    percent_plot$labels$y,
    "Flow difference (% of X)\n((Benchmark - Adjusted) / Adjusted)"
  )
  expect_equal(plot$data$difference[plot$data$method == "method_a"], c(-8, 3, 4, -6, -1, 4))
  expect_equal(absolute_plot$data$value[absolute_plot$data$method == "method_a"], c(8, 3, 4, 6, 1, 4))
  expect_equal(
    round(percent_plot$data$value[percent_plot$data$method == "method_a"], 4),
    round(100 * c(-8, 3, 4, -6, -1, 4) / c(88, 42, 26, 76, 56, 31), 4)
  )
  expect_equal(unique(as.character(filtered_plot$data$method)), "method_a")
  expect_equal(unique(as.character(labelled_filter_plot$data$method)), "method_b")
  expect_equal(length(unique(as.character(all_comparisons_plot$data$comparison))), 3)
  layer_geoms <- vapply(plot$layers, function(layer) {
    class(layer$geom)[1]
  }, character(1))
  expect_true("GeomViolin" %in% layer_geoms)
  expect_true("GeomPoint" %in% layer_geoms)
  expect_true("GeomText" %in% layer_geoms)
})

test_that("validation scatter plot returns a ggplot", {
  testthat::skip_if_not_installed("ggplot2")
  fixture <- make_validation_plot_fixture()

  plot <- plot_validation_scatter(fixture$residuals)
  all_comparisons_plot <- plot_validation_scatter(
    fixture$residuals,
    comparisons = c(
      "adjusted_vs_benchmark",
      "raw_vs_benchmark",
      "raw_vs_adjusted"
    )
  )
  raw_adjusted_plot <- plot_validation_scatter(
    fixture$residuals,
    comparisons = "raw_vs_adjusted",
    methods = "method_a"
  )
  raw_benchmark_plot <- plot_validation_scatter(
    fixture$residuals,
    comparisons = "raw_vs_benchmark",
    methods = "method_a"
  )
  plain_plot <- plot_validation_scatter(
    fixture$residuals,
    point_outline = FALSE
  )
  limited_plot <- plot_validation_scatter(
    fixture$residuals,
    difference_limits = c(-10, 10)
  )
  neutral_band_plot <- plot_validation_scatter(
    fixture$residuals,
    white_band = 0.2
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(unique(as.character(plot$data$comparison)), "adjusted_vs_benchmark")
  expect_equal(plot$labels$x, "X-axis: Adjusted flow (people)")
  expect_equal(plot$labels$y, "Y-axis: Benchmark flow (people)")
  expect_equal(raw_adjusted_plot$labels$x, "X-axis: Raw MPD flow (people)")
  expect_equal(raw_adjusted_plot$labels$y, "Y-axis: Adjusted flow (people)")
  expect_equal(raw_benchmark_plot$labels$x, "X-axis: Raw MPD flow (people)")
  expect_equal(raw_benchmark_plot$labels$y, "Y-axis: Benchmark flow (people)")
  expect_equal(
    all_comparisons_plot$labels$x,
    "X-axis flow (people; see facet header)"
  )
  expect_equal(
    sort(unique(as.character(all_comparisons_plot$data$scatter_comparison_label))),
    sort(c(
      "Adjusted vs benchmark\nX: Adjusted | Y: Benchmark",
      "Raw MPD vs adjusted\nX: Raw MPD | Y: Adjusted",
      "Raw MPD vs benchmark\nX: Raw MPD | Y: Benchmark"
    ))
  )
  expect_equal(plot$data$x_flow[plot$data$method == "method_a"], c(88, 42, 26, 76, 56, 31))
  expect_equal(plot$data$y_flow[plot$data$method == "method_a"], c(80, 45, 30, 70, 55, 35))
  expect_equal(plot$data$difference[plot$data$method == "method_a"], c(-8, 3, 4, -6, -1, 4))
  expect_equal(raw_adjusted_plot$data$x_flow, c(100, 35, 20, 90, 60, 25))
  expect_equal(raw_adjusted_plot$data$y_flow, c(88, 42, 26, 76, 56, 31))
  expect_equal(raw_adjusted_plot$data$difference, c(-12, 7, 6, -14, -4, 6))
  expect_equal(raw_benchmark_plot$data$x_flow, c(100, 35, 20, 90, 60, 25))
  expect_equal(raw_benchmark_plot$data$y_flow, c(80, 45, 30, 70, 55, 35))
  expect_equal(raw_benchmark_plot$data$difference, c(-20, 10, 10, -20, -5, 10))
  expect_equal(length(unique(as.character(all_comparisons_plot$data$comparison))), 3)
  expect_s3_class(plain_plot, "ggplot")
  expect_s3_class(limited_plot, "ggplot")
  expect_s3_class(neutral_band_plot, "ggplot")
})

test_that("validation residual band stacked bar plots return ggplots", {
  testthat::skip_if_not_installed("ggplot2")
  fixture <- make_validation_plot_fixture()

  plot <- plot_validation_residual_bands(fixture$residuals)
  quantile_plot <- plot_validation_residual_bands(
    fixture$residuals,
    band_method = "quantile"
  )
  all_comparisons_plot <- plot_validation_residual_bands(
    fixture$residuals,
    comparisons = "all",
    methods = "method_a"
  )
  vertical_plot <- plot_validation_residual_bands(
    fixture$residuals,
    orientation = "vertical"
  )

  expect_s3_class(plot, "ggplot")
  expect_s3_class(quantile_plot, "ggplot")
  expect_s3_class(all_comparisons_plot, "ggplot")
  expect_equal(unique(as.character(plot$data$comparison)), "adjusted_vs_benchmark")
  expect_equal(
    sort(unique(as.character(all_comparisons_plot$data$method_label))),
    sort(c("method_a", "Unadjusted raw MPD"))
  )
  expect_equal(length(unique(as.character(all_comparisons_plot$data$comparison))), 3)
  expect_equal(plot$labels$x, "Share of OD pairs")
  expect_null(plot$labels$y)
  expect_equal(vertical_plot$labels$y, "Share of OD pairs")
  expect_true(any(grepl("P", as.character(quantile_plot$data$residual_band))))
  shares <- as.numeric(round(tapply(quantile_plot$data$share, quantile_plot$data$method_label, sum), 6))
  expect_true(all(shares == 100))
  layer_geoms <- vapply(plot$layers, function(layer) {
    class(layer$geom)[1]
  }, character(1))
  expect_true("GeomCol" %in% layer_geoms)
  expect_false("GeomTile" %in% layer_geoms)
})

test_that("residual band plots normalise finite residual bands", {
  testthat::skip_if_not_installed("ggplot2")
  fixture <- make_validation_plot_fixture()
  residual_data <- debiasR:::.as_validate_residual_data(fixture$residuals)
  residual_data$adj_flow[residual_data$method == "method_a"][1] <- NA_real_

  quantile_plot <- plot_validation_residual_bands(
    residual_data,
    band_method = "quantile"
  )
  shares <- tapply(
    quantile_plot$data$share,
    quantile_plot$data$method_label,
    sum
  )

  expect_equal(
    as.numeric(round(shares[shares > 0], 6)),
    rep(100, sum(shares > 0))
  )
})

test_that("distributional validation heatmaps return ggplots", {
  testthat::skip_if_not_installed("ggplot2")
  fixture <- make_validation_plot_fixture()

  summary_plot <- plot_validation_distribution(
    fixture$distributions
  )
  pairwise_plot <- plot_validation_distribution_pairwise(
    fixture$distributions
  )
  all_comparisons_plot <- plot_validation_distribution(
    fixture$distributions,
    comparisons = "all",
    methods = "method_a"
  )
  full_pairwise_plot <- plot_validation_distribution_pairwise(
    fixture$distributions,
    comparisons = "all",
    methods = "method_a"
  )

  expect_s3_class(summary_plot, "ggplot")
  expect_s3_class(pairwise_plot, "ggplot")
  expect_s3_class(all_comparisons_plot, "ggplot")
  expect_s3_class(full_pairwise_plot, "ggplot")
  expect_equal(unique(as.character(summary_plot$data$comparison_label)), "Adjusted vs benchmark")
  expect_equal(unique(as.character(all_comparisons_plot$data$method)), "method_a")
  expect_equal(length(unique(as.character(all_comparisons_plot$data$comparison_label))), 3)
})

test_that("residual-structure validation plot returns a ggplot", {
  testthat::skip_if_not_installed("ggplot2")
  fixture <- make_validation_plot_fixture()

  plot <- plot_validation_structure(fixture$structure)
  filtered_plot <- plot_validation_structure(
    fixture$structure,
    methods = "method_a"
  )

  expect_s3_class(plot, "ggplot")
  expect_s3_class(filtered_plot, "ggplot")
  expect_equal(unique(as.character(plot$data$comparison)), "adjusted_vs_benchmark")
  expect_equal(unique(as.character(filtered_plot$data$method)), "method_a")
})

test_that("LISA validation map requires Local Moran output", {
  testthat::skip_if_not_installed("ggplot2")
  fixture <- make_validation_plot_fixture()

  expect_error(
    plot_validation_lisa_map(fixture$structure),
    "`structure_results` must contain: lisa_cluster"
  )
})

test_that("LISA validation map plots user-supplied boundaries", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("sf")
  fixture <- make_validation_plot_fixture()
  boundaries <- sf::st_as_sf(
    data.frame(
      area = c("A", "B", "C"),
      wkt = c(
        "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
        "POLYGON ((1 0, 2 0, 2 1, 1 1, 1 0))",
        "POLYGON ((0 1, 1 1, 1 2, 0 2, 0 1))"
      )
    ),
    wkt = "wkt",
    crs = 4326
  )

  plot <- plot_validation_lisa_map(
    fixture$structure_lisa,
    boundaries = boundaries,
    methods = "method_a"
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(unique(as.character(plot$layers[[2]]$data$method)), "method_a")
  expect_true(".lisa_cluster" %in% names(plot$layers[[2]]$data))

  expect_error(
    plot_validation_lisa_map(
      fixture$structure_lisa,
      boundaries = data.frame(area = c("A", "B", "C"))
    ),
    "`boundaries` must be an `sf` object"
  )
})
