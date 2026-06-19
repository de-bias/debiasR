# Validation plotting functions ----------------------------------------------

.require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This plotting function requires the ggplot2 package.", call. = FALSE)
  }
  invisible(TRUE)
}

.validation_okabe_ito <- c(
  "#0072B2", "#E69F00", "#009E73", "#CC79A7",
  "#56B4E9", "#D55E00", "#F0E442", "#000000"
)

# Palette supplied during validation visual review.
.validation_reference_categorical <- c(
  "#ffd700", "#ffb14e", "#fa8775", "#ea5f94",
  "#cd34b5", "#9d02d7", "#0000ff"
)

.validation_metric_matrix_palette <- c(
  "#fff7bc", "#ffd700", "#ffb14e", "#fa8775",
  "#ea5f94", "#cd34b5", "#9d02d7", "#0000ff"
)

.validation_viz_palette_categorical <- c(
  "#ffd700", "#ffb14e", "#fa8775", "#ea5f94",
  "#cd34b5", "#9d02d7", "#0000ff"
)

.validation_residual_band_palette <- c(
  "#ffd700", "#ffb14e", "#fa8775", "#ea5f94",
  "#cd34b5", "#9d02d7", "#0000ff"
)

.validation_lisa_cluster_levels <- c(
  "high-high",
  "low-low",
  "high-low",
  "low-high",
  "not significant",
  "no neighbours",
  "undefined"
)

.validation_lisa_cluster_labels <- c(
  "high-high" = "High-high",
  "low-low" = "Low-low",
  "high-low" = "High-low",
  "low-high" = "Low-high",
  "not significant" = "Not significant",
  "no neighbours" = "No neighbours",
  "undefined" = "Undefined"
)

.validation_lisa_cluster_palette <- c(
  "high-high" = "#fa8775",
  "low-low" = "#0000ff",
  "high-low" = "#ffb14e",
  "low-high" = "#9d02d7",
  "not significant" = "#D1D5DB",
  "no neighbours" = "#F3F4F6",
  "undefined" = "#6B7280"
)

.validation_palette <- function(values, palette = NULL) {
  values <- unique(as.character(stats::na.omit(values)))
  if (length(values) == 0L) {
    return(character())
  }

  if (is.null(palette)) {
    palette <- .validation_reference_categorical
  }

  if (!is.null(names(palette)) && all(values %in% names(palette))) {
    return(palette[values])
  }

  if (is.null(names(palette)) && length(palette) > length(values)) {
    palette <- palette[round(seq.int(1L, length(palette), length.out = length(values)))]
  }

  stats::setNames(rep(palette, length.out = length(values)), values)
}

.validation_theme <- function(base_size = 11,
                              grid = c("y", "xy", "none")) {
  grid <- match.arg(grid)
  theme <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "#A7ADB7", linewidth = 0.35),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "#4B5563"),
      axis.title = ggplot2::element_text(colour = "#111827"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(colour = "#111827", face = "bold"),
      legend.title = ggplot2::element_text(colour = "#111827"),
      legend.text = ggplot2::element_text(colour = "#374151")
    )

  if (grid == "xy") {
    theme <- theme +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(
          colour = "#E5E7EB",
          linewidth = 0.35
        )
      )
  } else if (grid == "y") {
    theme <- theme +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_line(
          colour = "#E5E7EB",
          linewidth = 0.35
        )
      )
  }

  theme
}

.validation_flow_axis_label <- function(x) {
  dplyr::case_when(
    abs(x) >= 1e6 ~ paste0(round(x / 1e6, 1), "m"),
    abs(x) >= 1e3 ~ paste0(round(x / 1e3, 1), "k"),
    TRUE ~ format(round(x, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
  )
}

.validation_symmetric_limits <- function(x, quantile = NULL) {
  finite_values <- abs(x[is.finite(x)])
  if (length(finite_values) == 0L) {
    return(c(-1, 1))
  }

  if (is.null(quantile)) {
    limit <- max(finite_values, na.rm = TRUE)
  } else {
    quantile <- min(max(quantile, 0), 1)
    limit <- stats::quantile(
      finite_values,
      probs = quantile,
      na.rm = TRUE,
      names = FALSE
    )
  }

  if (!is.finite(limit) || limit <= 0) {
    return(c(-1, 1))
  }
  c(-limit, limit)
}

.validation_residual_axis_label <- function(residual, comparisons) {
  comparisons <- .normalise_flow_comparisons(comparisons)
  if (.validation_benchmark_only_comparisons(comparisons)) {
    return(switch(
      residual,
      signed = "Flow difference\n(Benchmark - adjusted/raw MPD)",
      absolute = "Absolute flow difference\n(|Benchmark - adjusted/raw MPD|)",
      percent = paste0(
        "Flow difference (% of adjusted/raw MPD)\n",
        "((Benchmark - adjusted/raw MPD) / adjusted/raw MPD)"
      )
    ))
  }

  if (length(comparisons) == 1L) {
    spec <- .flow_comparison_spec(comparisons)
    difference_label <- paste0(spec$y_label, " - ", spec$x_label)
    return(switch(
      residual,
      signed = paste0("Flow difference\n(", difference_label, ")"),
      absolute = paste0(
        "Absolute flow difference\n(|",
        difference_label,
        "|)"
      ),
      percent = paste0(
        "Flow difference (% of X)\n((",
        difference_label,
        ") / ",
        spec$x_label,
        ")"
      )
    ))
  }

  switch(
    residual,
    signed = "Flow difference\n(second named series - first named series)",
    absolute = "Absolute flow difference\n(|second named series - first named series|)",
    percent = "Flow difference (% of first named series)"
  )
}

.validation_scatter_axis_labels <- function(comparisons) {
  comparisons <- .normalise_flow_comparisons(comparisons)
  if (.validation_benchmark_only_comparisons(comparisons)) {
    return(list(
      x = "Adjusted flow or raw MPD flow (people; see facet)",
      y = "Benchmark flow (people)"
    ))
  }

  if (length(comparisons) == 1L) {
    spec <- .flow_comparison_spec(comparisons)
    return(list(
      x = paste0("X-axis: ", spec$x_label, " flow (people)"),
      y = paste0("Y-axis: ", spec$y_label, " flow (people)")
    ))
  }

  list(
    x = "X-axis flow (people; see facet header)",
    y = "Y-axis flow (people; see facet header)"
  )
}

.validation_difference_label <- function(comparisons) {
  comparisons <- .normalise_flow_comparisons(comparisons)
  if (.validation_benchmark_only_comparisons(comparisons)) {
    return("Benchmark - adjusted/raw MPD")
  }

  if (length(comparisons) == 1L) {
    spec <- .flow_comparison_spec(comparisons)
    return(paste0(spec$y_label, " - ", spec$x_label))
  }

  "Second named series - first named series"
}

.validation_squish_oob <- function(x, range = c(0, 1), ...) {
  x[x < range[1]] <- range[1]
  x[x > range[2]] <- range[2]
  x
}

.validation_diverging_values <- function(limits, white_band = 0.12) {
  zero_position <- (0 - limits[1]) / diff(limits)
  zero_position <- min(max(zero_position, 0), 1)
  half_band <- white_band / 2
  c(
    0,
    max(0, zero_position - half_band),
    min(1, zero_position + half_band),
    1
  )
}

.validation_quantile_label <- function(prob) {
  paste0("P", format(round(prob * 100, 0), trim = TRUE, scientific = FALSE))
}

.validation_quantile_band_labels <- function(probs) {
  labels <- character(length(probs) + 1L)
  labels[1] <- paste0("<= ", .validation_quantile_label(probs[1]))
  if (length(probs) > 1L) {
    labels[2:length(probs)] <- paste0(
      .validation_quantile_label(probs[-length(probs)]),
      " to ",
      .validation_quantile_label(probs[-1])
    )
  }
  labels[length(labels)] <- paste0("> ", .validation_quantile_label(probs[length(probs)]))
  labels
}

.validation_metric_value_label <- function(x) {
  vapply(x, function(value) {
    if (!is.finite(value)) {
      return("")
    }
    abs_value <- abs(value)
    if (abs_value >= 1e6) {
      return(paste0(round(value / 1e6, 1), "m"))
    }
    if (abs_value >= 1e3) {
      return(paste0(round(value / 1e3, 1), "k"))
    }
    if (abs_value >= 100) {
      return(format(round(value, 0), big.mark = ",", scientific = FALSE, trim = TRUE))
    }
    if (abs_value >= 10) {
      return(format(round(value, 1), nsmall = 1, scientific = FALSE, trim = TRUE))
    }
    if (abs_value >= 1) {
      return(format(round(value, 2), nsmall = 2, scientific = FALSE, trim = TRUE))
    }
    format(round(value, 3), nsmall = 3, scientific = FALSE, trim = TRUE)
  }, character(1))
}

.validation_range_labels <- function(breaks) {
  lower <- breaks[-length(breaks)]
  upper <- breaks[-1]
  integer_breaks <- all(abs(breaks - round(breaks)) < sqrt(.Machine$double.eps))

  if (integer_breaks) {
    lower <- as.integer(round(lower))
    upper <- as.integer(round(upper))
    lower[-1] <- lower[-1] + 1L
  }

  paste0(lower, "-", upper)
}

.validation_check_relative_error_breaks <- function(breaks) {
  if (
    !is.numeric(breaks) ||
      length(breaks) < 2L ||
      anyNA(breaks) ||
      any(!is.finite(breaks)) ||
      any(diff(breaks) <= 0) ||
      min(breaks) > 0 ||
      max(breaks) < 100
  ) {
    stop(
      "`relative_error_breaks` must be an increasing numeric vector ",
      "covering 0 to 100.",
      call. = FALSE
    )
  }

  invisible(breaks)
}

.validation_relative_error_band <- function(x, breaks) {
  labels <- .validation_range_labels(breaks)
  score <- 100 * x
  score <- pmin(pmax(score, min(breaks)), max(breaks))
  cut(
    score,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = TRUE,
    ordered_result = TRUE
  )
}

.validation_interpolate_palette <- function(palette, n) {
  if (n <= 0L) {
    return(character())
  }
  if (length(palette) == 0L) {
    stop("`palette` must contain at least one colour.", call. = FALSE)
  }
  if (length(palette) == 1L) {
    return(rep(palette, n))
  }
  if (length(palette) == n) {
    return(palette)
  }
  if (length(palette) > n) {
    return(palette[round(seq.int(1L, length(palette), length.out = n))])
  }

  grDevices::colorRampPalette(unname(palette))(n)
}

.validation_count_label <- function(x) {
  vapply(x, function(value) {
    if (!is.finite(value)) {
      return("")
    }
    if (abs(value) >= 1e6) {
      return(paste0(round(value / 1e6, 1), "m"))
    }
    if (abs(value) >= 1e3) {
      return(paste0(round(value / 1e3), "k"))
    }
    format(round(value, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
  }, character(1))
}

.validation_thin_points <- function(data, max_points = 5000) {
  if (is.null(max_points) || !is.finite(max_points)) {
    return(data)
  }
  max_points <- as.integer(max_points)
  if (max_points <= 0L) {
    return(data[0, , drop = FALSE])
  }
  if (nrow(data) <= max_points) {
    return(data)
  }
  data[unique(round(seq.int(1L, nrow(data), length.out = max_points))), , drop = FALSE]
}

.validation_metric_labels <- c(
  mean_error = "Mean error",
  mae = "Mean absolute\nerror",
  rmse = "Root mean squared\nerror",
  median_absolute_error = "Median absolute error",
  mape = "Mean absolute percentage\nerror",
  pearson_r = "Pearson r",
  spearman_rho = "Spearman rho",
  r_squared = "R-squared"
)

.validation_error_measure_aliases <- c(
  stats::setNames(names(.validation_metric_labels), names(.validation_metric_labels)),
  stats::setNames(names(.validation_metric_labels), unname(.validation_metric_labels)),
  "Mean absolute error" = "mae",
  "Root mean squared error" = "rmse",
  "Mean absolute percentage error" = "mape",
  MAE = "mae",
  RMSE = "rmse",
  MAPE = "mape"
)

.validation_comparison_labels <- c(
  adjusted_vs_benchmark = "Adjusted vs benchmark",
  raw_vs_benchmark = "Raw MPD vs benchmark",
  raw_vs_adjusted = "Raw MPD vs adjusted"
)

.validation_raw_baseline_id <- "raw_mpd_baseline"
.validation_raw_baseline_label <- "Unadjusted raw MPD"
.validation_benchmark_display_id <- "benchmark_comparison"
.validation_benchmark_display_label <- "Adjusted methods and raw MPD vs benchmark"

.validation_distribution_labels <- c(
  benchmark = "Benchmark",
  adjusted_mpd = "Adjusted",
  raw_mpd = "Raw MPD"
)

.validation_integrates_raw_baseline <- function(comparisons) {
  comparisons <- .normalise_flow_comparisons(comparisons)
  all(c("adjusted_vs_benchmark", "raw_vs_benchmark") %in% comparisons)
}

.validation_benchmark_only_comparisons <- function(comparisons) {
  comparisons <- .normalise_flow_comparisons(comparisons)
  .validation_integrates_raw_baseline(comparisons) &&
    all(comparisons %in% c("adjusted_vs_benchmark", "raw_vs_benchmark"))
}

.validation_add_comparison_display <- function(data, comparisons) {
  comparisons <- .normalise_flow_comparisons(comparisons)
  if (!"comparison_label" %in% names(data)) {
    data$comparison_label <- .flow_comparison_label(data$comparison)
  }

  data$comparison_display <- as.character(data$comparison)
  data$comparison_display_label <- as.character(data$comparison_label)

  if (.validation_integrates_raw_baseline(comparisons)) {
    benchmark_rows <- data$comparison %in% c(
      "adjusted_vs_benchmark",
      "raw_vs_benchmark"
    )
    data$comparison_display[benchmark_rows] <- .validation_benchmark_display_id
    data$comparison_display_label[benchmark_rows] <- .validation_benchmark_display_label
  }

  display_levels <- unique(data$comparison_display_label)
  data$comparison_display_label <- factor(
    data$comparison_display_label,
    levels = display_levels
  )
  data
}

.validation_order_raw_baseline <- function(labels,
                                           position = c("last", "first")) {
  position <- match.arg(position)
  labels <- unique(as.character(labels))
  non_baseline <- setdiff(labels, .validation_raw_baseline_label)
  baseline <- intersect(labels, .validation_raw_baseline_label)

  if (position == "first") {
    c(baseline, non_baseline)
  } else {
    c(non_baseline, baseline)
  }
}

.validation_method_factor_levels <- function(labels,
                                             axis = c("x", "y"),
                                             raw_position = c("last", "first")) {
  axis <- match.arg(axis)
  raw_position <- match.arg(raw_position)
  levels <- .validation_order_raw_baseline(labels, position = raw_position)
  if (axis == "y") {
    rev(levels)
  } else {
    levels
  }
}

.validation_distribution_axis_label <- function(metric, value) {
  statistic_label <- c(
    mean = "Mean",
    weighted_mean = "Weighted mean",
    median = "Median"
  )
  paste0(
    statistic_label[[value]],
    " ",
    toupper(metric),
    " (lower = closer allocation)"
  )
}

.validation_check_columns <- function(data, cols, arg = "data") {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    stop(
      "`", arg, "` must contain: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.validation_scalar_row <- function(x) {
  scalar_names <- names(x)[vapply(x, function(value) {
    is.atomic(value) && length(value) == 1L
  }, logical(1))]
  tibble::as_tibble(x[scalar_names])
}

.validation_method_label <- function(data, method_col, method_labels = NULL) {
  method <- as.character(data[[method_col]])
  if (is.null(method_labels)) {
    return(method)
  }
  dplyr::coalesce(unname(method_labels[method]), method)
}

.validation_filter_methods <- function(data,
                                       methods = NULL,
                                       method_col = "method",
                                       method_labels = NULL,
                                       arg = "data") {
  if (is.null(methods)) {
    return(data)
  }
  .validation_check_columns(data, method_col, arg)

  method_values <- as.character(data[[method_col]])
  method_label_values <- .validation_method_label(
    data,
    method_col,
    method_labels
  )
  keep <- method_values %in% methods | method_label_values %in% methods
  out <- data[keep, , drop = FALSE]
  if (nrow(out) == 0L) {
    stop(
      "`methods` did not match any method identifiers or labels in `", arg, "`.",
      call. = FALSE
    )
  }

  out
}

.validation_normalise_error_measures <- function(error_measures) {
  if (!is.character(error_measures) || length(error_measures) == 0L) {
    stop("`error_measures` must be a non-empty character vector.", call. = FALSE)
  }
  dplyr::coalesce(
    unname(.validation_error_measure_aliases[error_measures]),
    error_measures
  )
}

.as_validate_overall_table <- function(metrics, method_col = "method") {
  if (inherits(metrics, "data.frame")) {
    out <- tibble::as_tibble(metrics)
  } else if (is.list(metrics) && "summary" %in% names(metrics) &&
             inherits(metrics$summary, "data.frame")) {
    out <- tibble::as_tibble(metrics$summary)
  } else if (is.list(metrics) && all(c("mae", "rmse", "mape") %in% names(metrics))) {
    out <- .validation_scalar_row(metrics)
  } else if (is.list(metrics)) {
    out <- dplyr::bind_rows(lapply(names(metrics), function(method_name) {
      item <- metrics[[method_name]]
      if (is.list(item) && "summary" %in% names(item) &&
          inherits(item$summary, "data.frame")) {
        rows <- tibble::as_tibble(item$summary)
        if (!method_col %in% names(rows) || all(is.na(rows[[method_col]]))) {
          rows[[method_col]] <- method_name
        }
        return(rows)
      }
      if (!is.list(item) || !all(c("mae", "rmse", "mape") %in% names(item))) {
        stop(
          "`metrics` must be a data frame, one `validate_flow_overall()` result, ",
          "or a named list of `validate_flow_overall()` results.",
          call. = FALSE
        )
      }
      row <- .validation_scalar_row(item)
      if (!method_col %in% names(row) || is.na(row[[method_col]][1])) {
        row[[method_col]] <- method_name
      }
      row
    }))
  } else {
    stop(
      "`metrics` must be a data frame, one `validate_flow_overall()` result, ",
      "or a named list of `validate_flow_overall()` results.",
      call. = FALSE
    )
  }

  if (!method_col %in% names(out)) {
    out[[method_col]] <- "method"
  }

  out
}

.as_validate_residual_data <- function(residuals, method_col = "method") {
  if (inherits(residuals, "data.frame")) {
    out <- tibble::as_tibble(residuals)
  } else if (is.list(residuals) && "data" %in% names(residuals) &&
             inherits(residuals$data, "data.frame")) {
    out <- tibble::as_tibble(residuals$data)
  } else if (is.list(residuals)) {
    out <- dplyr::bind_rows(lapply(names(residuals), function(method_name) {
      item <- residuals[[method_name]]
      if (!is.list(item) || !"data" %in% names(item) ||
          !inherits(item$data, "data.frame")) {
        stop(
          "`residuals` must be a data frame, one `validate_flow_residuals()` ",
          "result, or a named list of `validate_flow_residuals()` results.",
          call. = FALSE
        )
      }
      data <- tibble::as_tibble(item$data)
      if (!method_col %in% names(data) || all(is.na(data[[method_col]]))) {
        data[[method_col]] <- method_name
      }
      data
    }))
  } else {
    stop(
      "`residuals` must be a data frame, one `validate_flow_residuals()` ",
      "result, or a named list of `validate_flow_residuals()` results.",
      call. = FALSE
    )
  }

  if (!method_col %in% names(out)) {
    out[[method_col]] <- "method"
  }

  out
}

.as_validate_distribution_summary <- function(distribution_results,
                                              method_col = "method") {
  if (inherits(distribution_results, "data.frame")) {
    out <- tibble::as_tibble(distribution_results)
  } else if (is.list(distribution_results) &&
             "summary" %in% names(distribution_results) &&
             inherits(distribution_results$summary, "data.frame")) {
    out <- tibble::as_tibble(distribution_results$summary)
  } else if (is.list(distribution_results)) {
    out <- dplyr::bind_rows(lapply(names(distribution_results), function(method_name) {
      item <- distribution_results[[method_name]]
      if (!is.list(item) || !"summary" %in% names(item) ||
          !inherits(item$summary, "data.frame")) {
        stop(
          "`distribution_results` must be a data frame, one ",
          "`validate_flow_distribution()` result, or a named list of results.",
          call. = FALSE
        )
      }
      summary <- tibble::as_tibble(item$summary)
      if (!method_col %in% names(summary) || all(is.na(summary[[method_col]]))) {
        summary[[method_col]] <- method_name
      }
      summary
    }))
  } else {
    stop(
      "`distribution_results` must be a data frame, one ",
      "`validate_flow_distribution()` result, or a named list of results.",
      call. = FALSE
    )
  }

  if (!method_col %in% names(out)) {
    out[[method_col]] <- "method"
  }

  out
}

.as_validate_structure_summary <- function(structure_results,
                                           method_col = "method") {
  if (inherits(structure_results, "data.frame")) {
    out <- tibble::as_tibble(structure_results)
  } else if (is.list(structure_results) &&
             "summary" %in% names(structure_results) &&
             inherits(structure_results$summary, "data.frame")) {
    out <- tibble::as_tibble(structure_results$summary)
  } else if (is.list(structure_results)) {
    out <- dplyr::bind_rows(lapply(names(structure_results), function(method_name) {
      item <- structure_results[[method_name]]
      if (!is.list(item) || !"summary" %in% names(item) ||
          !inherits(item$summary, "data.frame")) {
        stop(
          "`structure_results` must be a data frame, one ",
          "`validate_flow_residual_structure()` result, or a named list of results.",
          call. = FALSE
        )
      }
      summary <- tibble::as_tibble(item$summary)
      if (!method_col %in% names(summary) || all(is.na(summary[[method_col]]))) {
        summary[[method_col]] <- method_name
      }
      summary
    }))
  } else {
    stop(
      "`structure_results` must be a data frame, one ",
      "`validate_flow_residual_structure()` result, or a named list of results.",
      call. = FALSE
    )
  }

  if (!method_col %in% names(out)) {
    out[[method_col]] <- "method"
  }

  out
}

.as_validate_structure_map_data <- function(structure_results,
                                            method_col = "method") {
  if (inherits(structure_results, "data.frame")) {
    out <- tibble::as_tibble(structure_results)
  } else if (is.list(structure_results) &&
             "map_data" %in% names(structure_results) &&
             inherits(structure_results$map_data, "data.frame")) {
    out <- tibble::as_tibble(structure_results$map_data)
  } else if (is.list(structure_results)) {
    result_names <- names(structure_results)
    if (is.null(result_names)) {
      result_names <- rep(NA_character_, length(structure_results))
    }
    out <- dplyr::bind_rows(lapply(seq_along(structure_results), function(i) {
      item <- structure_results[[i]]
      method_name <- result_names[[i]]
      if (!is.list(item) || !"map_data" %in% names(item) ||
          !inherits(item$map_data, "data.frame")) {
        stop(
          "`structure_results` must be a data frame, one ",
          "`validate_flow_residual_structure()` result, or a named list of results.",
          call. = FALSE
        )
      }
      map_data <- tibble::as_tibble(item$map_data)
      if (!method_col %in% names(map_data) || all(is.na(map_data[[method_col]]))) {
        map_data[[method_col]] <- method_name
      }
      map_data
    }))
  } else {
    stop(
      "`structure_results` must be a data frame, one ",
      "`validate_flow_residual_structure()` result, or a named list of results.",
      call. = FALSE
    )
  }

  if (!method_col %in% names(out)) {
    out[[method_col]] <- "method"
  }

  out
}

.validation_residual_long <- function(residual_data,
                                      residual = c("signed", "absolute", "percent"),
                                      comparisons = "adjusted_vs_benchmark",
                                      method_col = "method",
                                      method_labels = NULL) {
  residual <- match.arg(residual)
  comparisons <- .normalise_flow_comparisons(comparisons)
  .validation_check_columns(
    residual_data,
    c(method_col, "mpd_flow", "adj_flow", "benchmark_flow"),
    "residuals"
  )

  builders <- list(
    adjusted_vs_benchmark = function(data) {
      y_minus_x <- data$benchmark_flow - data$adj_flow
      tibble::tibble(
        method = data[[method_col]],
        method_label = .validation_method_label(data, method_col, method_labels),
        comparison = "adjusted_vs_benchmark",
        comparison_label = .flow_comparison_label("adjusted_vs_benchmark"),
        x_series = "adjusted",
        y_series = "benchmark",
        x_flow = data$adj_flow,
        y_flow = data$benchmark_flow,
        difference = y_minus_x,
        abs_difference = abs(y_minus_x),
        pct_difference = dplyr::if_else(
          data$adj_flow == 0,
          NA_real_,
          100 * y_minus_x / data$adj_flow
        )
      )
    },
    raw_vs_adjusted = function(data) {
      y_minus_x <- data$adj_flow - data$mpd_flow
      tibble::tibble(
        method = data[[method_col]],
        method_label = .validation_method_label(data, method_col, method_labels),
        comparison = "raw_vs_adjusted",
        comparison_label = .flow_comparison_label("raw_vs_adjusted"),
        x_series = "raw_mpd",
        y_series = "adjusted",
        x_flow = data$mpd_flow,
        y_flow = data$adj_flow,
        difference = y_minus_x,
        abs_difference = abs(y_minus_x),
        pct_difference = dplyr::if_else(
          data$mpd_flow == 0,
          NA_real_,
          100 * y_minus_x / data$mpd_flow
        )
      )
    },
    raw_vs_benchmark = function(data) {
      raw_data <- if (all(c("origin", "destination") %in% names(data))) {
        dplyr::distinct(data, .data$origin, .data$destination, .keep_all = TRUE)
      } else {
        dplyr::distinct(data, .data$mpd_flow, .data$benchmark_flow, .keep_all = TRUE)
      }
      y_minus_x <- raw_data$benchmark_flow - raw_data$mpd_flow
      tibble::tibble(
        method = .validation_raw_baseline_id,
        method_label = .validation_raw_baseline_label,
        comparison = "raw_vs_benchmark",
        comparison_label = .flow_comparison_label("raw_vs_benchmark"),
        x_series = "raw_mpd",
        y_series = "benchmark",
        x_flow = raw_data$mpd_flow,
        y_flow = raw_data$benchmark_flow,
        difference = y_minus_x,
        abs_difference = abs(y_minus_x),
        pct_difference = dplyr::if_else(
          raw_data$mpd_flow == 0,
          NA_real_,
          100 * y_minus_x / raw_data$mpd_flow
        )
      )
    }
  )

  out <- dplyr::bind_rows(lapply(comparisons, function(comparison) {
    builders[[comparison]](residual_data)
  }))

  value_col <- switch(
    residual,
    signed = "difference",
    absolute = "abs_difference",
    percent = "pct_difference"
  )

  out |>
    dplyr::mutate(
      residual_type = residual,
      value = .data[[value_col]],
      comparison_label = factor(
        .data$comparison_label,
        levels = .flow_comparison_label(comparisons)
      )
    )
}

#' Plot overall validation metrics by adjustment method
#'
#' Builds a method-by-metric matrix from `validate_flow_overall()` results or a
#' data frame containing one row per method. Cell fill is scaled within each
#' metric so unlike metrics are not forced onto a common raw scale, while cell
#' labels show the original metric values.
#'
#' @param metrics A data frame, one `validate_flow_overall()` result, or a named
#'   list of `validate_flow_overall()` results.
#' @param error_measures Character vector of error measure IDs or labels to
#'   plot. Defaults to `c("mae", "rmse", "mape")`. Common labels such as
#'   `"MAE"`, `"RMSE"`, `"MAPE"`, and `"Mean absolute error"` are accepted.
#' @param metric_cols Compatibility alias for `error_measures`.
#' @param comparisons Flow comparison(s) to plot. Defaults to
#'   `"adjusted_vs_benchmark"`. Use `"all"` when `metrics` contains rows for
#'   all supported comparisons. When `"raw_vs_benchmark"` is included, it is
#'   shown as a single unadjusted raw MPD baseline row.
#' @param methods Optional character vector of method identifiers or display
#'   labels to include. Default `NULL` includes all methods.
#' @param method_col Column containing the method identifier. Default
#'   `"method"`.
#' @param method_family_col Optional column containing method-family labels.
#'   Retained for compatibility with earlier prototypes; the metric matrix is
#'   organised by method and metric.
#' @param method_labels Optional named character vector used to relabel methods.
#' @param palette Optional character vector of colours for the relative-error
#'   fill scale. Defaults to a yellow-green-blue sequence suitable for ordered
#'   error intensity.
#' @param relative_error_breaks Numeric percentage cut points used to bin the
#'   within-metric relative-error fill scale. Default `seq(0, 100, by = 10)`
#'   produces legend ranges such as 0-10, 11-20, and 91-100.
#'
#' @return A `ggplot` object.
#' @export
plot_validation_metrics <- function(metrics,
                                    error_measures = c("mae", "rmse", "mape"),
                                    metric_cols = NULL,
                                    comparisons = "adjusted_vs_benchmark",
                                    methods = NULL,
                                    method_col = "method",
                                    method_family_col = NULL,
                                    method_labels = NULL,
                                    palette = NULL,
                                    relative_error_breaks = seq(0, 100, by = 10)) {
  .require_ggplot2()
  .validation_check_relative_error_breaks(relative_error_breaks)
  if (!is.null(metric_cols)) {
    if (!missing(error_measures)) {
      stop(
        "Use only one of `error_measures` or `metric_cols`.",
        call. = FALSE
      )
    }
    error_measures <- metric_cols
  }
  metric_cols <- .validation_normalise_error_measures(error_measures)
  comparisons <- .normalise_flow_comparisons(comparisons)
  metric_tbl <- .as_validate_overall_table(metrics, method_col = method_col)
  if (!"comparison" %in% names(metric_tbl)) {
    metric_tbl$comparison <- "adjusted_vs_benchmark"
  }
  metric_tbl <- .validation_filter_methods(
    metric_tbl,
    methods = methods,
    method_col = method_col,
    method_labels = method_labels,
    arg = "metrics"
  )
  metric_tbl <- metric_tbl[metric_tbl$comparison %in% comparisons, , drop = FALSE]
  if (nrow(metric_tbl) == 0L) {
    stop("`metrics` does not contain the requested comparison(s).", call. = FALSE)
  }
  .validation_check_columns(metric_tbl, c(method_col, metric_cols), "metrics")
  if (!is.null(method_family_col)) {
    .validation_check_columns(metric_tbl, method_family_col, "metrics")
  }

  plot_data <- dplyr::bind_rows(lapply(metric_cols, function(metric_col) {
    tibble::tibble(
      method = metric_tbl[[method_col]],
      method_label = .validation_method_label(metric_tbl, method_col, method_labels),
      method_family = if (is.null(method_family_col)) {
        .validation_method_label(metric_tbl, method_col, method_labels)
      } else {
        as.character(metric_tbl[[method_family_col]])
      },
      metric = metric_col,
      metric_label = dplyr::coalesce(
        unname(.validation_metric_labels[metric_col]),
        metric_col
      ),
      comparison = metric_tbl$comparison,
      comparison_label = dplyr::coalesce(
        unname(.validation_comparison_labels[metric_tbl$comparison]),
        metric_tbl$comparison
      ),
      value = as.numeric(metric_tbl[[metric_col]])
    )
  }))

  plot_data <- plot_data |>
    dplyr::mutate(
      method = dplyr::if_else(
        .data$comparison == "raw_vs_benchmark",
        .validation_raw_baseline_id,
        as.character(.data$method)
      ),
      method_label = dplyr::if_else(
        .data$comparison == "raw_vs_benchmark",
        .validation_raw_baseline_label,
        as.character(.data$method_label)
      ),
      method_family = dplyr::if_else(
        .data$comparison == "raw_vs_benchmark",
        .validation_raw_baseline_label,
        as.character(.data$method_family)
      )
    ) |>
    dplyr::distinct(
      .data$comparison,
      .data$method_label,
      .data$metric,
      .keep_all = TRUE
    )
  plot_data <- .validation_add_comparison_display(plot_data, comparisons)

  plot_data <- dplyr::bind_rows(lapply(metric_cols, function(metric_col) {
    metric_data <- plot_data[plot_data$metric == metric_col, , drop = FALSE]
    finite_value <- is.finite(metric_data$value)
    if (any(finite_value)) {
      metric_range <- range(metric_data$value[finite_value])
      metric_span <- diff(metric_range)
      metric_data$relative_error <- if (is.finite(metric_span) && metric_span > 0) {
        (metric_data$value - metric_range[1]) / metric_span
      } else {
        0
      }
    } else {
      metric_data$relative_error <- NA_real_
    }
    metric_data
  }))

  metric_palette <- if (is.null(palette)) {
    .validation_metric_matrix_palette
  } else {
    palette
  }
  method_order <- .validation_method_factor_levels(
    plot_data$method_label,
    axis = "y",
    raw_position = "last"
  )
  metric_order <- unique(plot_data$metric_label)
  plot_data <- plot_data |>
    dplyr::mutate(
      method_label = factor(.data$method_label, levels = method_order),
      metric_label = factor(.data$metric_label, levels = metric_order),
      value_label = .validation_metric_value_label(.data$value),
      relative_error_band = .validation_relative_error_band(
        .data$relative_error,
        relative_error_breaks
      ),
      label_colour = dplyr::if_else(
        is.finite(.data$relative_error) & .data$relative_error >= 0.55,
        "white",
        "#1f2933"
      )
    )
  relative_error_labels <- .validation_range_labels(relative_error_breaks)
  fill_values <- stats::setNames(
    .validation_interpolate_palette(metric_palette, length(relative_error_labels)),
    relative_error_labels
  )
  legend_data <- data.frame(
    metric_label = factor(metric_order[[1]], levels = metric_order),
    method_label = factor(method_order[[1]], levels = method_order),
    relative_error_band = factor(
      relative_error_labels,
      levels = relative_error_labels,
      ordered = TRUE
    )
  )

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$metric_label,
      y = .data$method_label,
      fill = .data$relative_error_band
    )
  ) +
    ggplot2::geom_tile(
      data = legend_data,
      ggplot2::aes(
        x = .data$metric_label,
        y = .data$method_label,
        fill = .data$relative_error_band
      ),
      alpha = 0,
      colour = NA,
      linewidth = 0,
      inherit.aes = FALSE,
      show.legend = TRUE
    ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 0.6,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$value_label, colour = .data$label_colour),
      size = 3.4,
      fontface = "bold",
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = fill_values,
      limits = relative_error_labels,
      breaks = relative_error_labels,
      drop = FALSE,
      na.translate = FALSE,
      na.value = "#f2f2f2",
      name = "Relative error\nscore (%)",
      guide = ggplot2::guide_legend(
        ncol = 1,
        byrow = FALSE,
        override.aes = list(
          fill = unname(fill_values),
          alpha = 1,
          colour = "#A7ADB7",
          linewidth = 0.25
        )
      )
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::labs(x = NULL, y = NULL) +
    .validation_theme(base_size = 11, grid = "none") +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 9),
      legend.key.width = grid::unit(0.42, "cm"),
      legend.key.height = grid::unit(0.42, "cm"),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(colour = "#344054")
    )

  if (length(unique(plot_data$comparison_display)) > 1L) {
    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(.data$comparison_display_label),
        ncol = 1,
        scales = "free_y"
      )
  }

  plot
}

#' @rdname plot_validation_metrics
#' @export
plot_validate_flow_metrics <- function(...) {
  plot_validation_metrics(...)
}

#' Plot validation flow-difference distributions as violins
#'
#' Builds violin plots from `validate_flow_residuals()` data. Differences are
#' aligned with the scatterplot convention used by
#' `plot_validation_scatter()`: positive values mean the second named series is
#' larger than the first named series. The default style uses translucent
#' violins, jittered OD-pair points, and sample-size labels above each method.
#'
#' @param residuals A data frame, one `validate_flow_residuals()` result, or a
#'   named list of `validate_flow_residuals()` results.
#' @param residual Difference scale: `"signed"`, `"absolute"`, or `"percent"`.
#' @param comparisons Comparisons to plot. Defaults to
#'   `"adjusted_vs_benchmark"`. Use a character vector to compare multiple
#'   flow-pair definitions. When `"raw_vs_benchmark"` is included, it is shown
#'   once as the unadjusted raw MPD baseline.
#' @param methods Optional character vector of method identifiers or display
#'   labels to include, giving users control over which adjustment methods are
#'   compared. Default `NULL` includes all methods.
#' @param method_col Column containing the method identifier. Default
#'   `"method"`.
#' @param method_labels Optional named character vector used to relabel methods.
#' @param palette Optional character vector of colours. Defaults to a bright
#'   colour-blind-aware categorical palette inspired by the shared validation
#'   visual style.
#' @param show_points Logical; show jittered OD-pair values over each violin.
#'   Default `TRUE`.
#' @param show_counts Logical; show the number of plotted OD pairs above each
#'   violin. Default `TRUE`.
#' @param point_alpha Point opacity. Default `0.25`.
#' @param point_size Point size. Default `0.6`.
#' @param jitter_width Horizontal jitter width for OD-pair points. Default
#'   `0.12`.
#' @param violin_alpha Violin fill opacity. Default `0.5`.
#' @param max_points Maximum number of OD-pair points to draw per
#'   method-comparison group. The violin density and count labels still use all
#'   rows. Use `Inf` to draw every point. Default `5000`.
#' @param y_transform Y-axis transformation. `"pseudo_log"` gives a log-like
#'   display that can still show zero and negative signed residuals. Use
#'   `"identity"` for the raw residual scale. Default `"pseudo_log"`.
#'
#' @return A `ggplot` object.
#' @export
plot_validation_residuals <- function(residuals,
                                      residual = c("signed", "absolute", "percent"),
                                      comparisons = "adjusted_vs_benchmark",
                                      methods = NULL,
                                      method_col = "method",
                                      method_labels = NULL,
                                      palette = NULL,
                                      show_points = TRUE,
                                      show_counts = TRUE,
                                      point_alpha = 0.25,
                                      point_size = 0.6,
                                      jitter_width = 0.12,
                                      violin_alpha = 0.5,
                                      max_points = 5000,
                                      y_transform = c("pseudo_log", "identity")) {
  .require_ggplot2()
  residual <- match.arg(residual)
  y_transform <- match.arg(y_transform)
  residual_data <- .as_validate_residual_data(residuals, method_col = method_col)
  residual_data <- .validation_filter_methods(
    residual_data,
    methods = methods,
    method_col = method_col,
    method_labels = method_labels,
    arg = "residuals"
  )
  plot_data <- .validation_residual_long(
    residual_data = residual_data,
    residual = residual,
    comparisons = comparisons,
    method_col = method_col,
    method_labels = method_labels
  )
  plot_data <- .validation_add_comparison_display(plot_data, comparisons)

  method_order <- .validation_method_factor_levels(
    plot_data$method_label,
    axis = "x",
    raw_position = "last"
  )
  plot_data <- plot_data |>
    dplyr::mutate(method_label = factor(.data$method_label, levels = method_order))
  finite_plot_data <- plot_data[is.finite(plot_data$value), , drop = FALSE]
  count_data <- finite_plot_data |>
    dplyr::group_by(.data$comparison_display_label, .data$method_label) |>
    dplyr::summarise(
      n = dplyr::n(),
      y_max = max(.data$value, na.rm = TRUE),
      y_min = min(.data$value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      label_y = .data$y_max + pmax(abs(.data$y_max - .data$y_min) * 0.08, 1),
      n_label = .validation_count_label(.data$n)
    )
  point_data <- dplyr::bind_rows(lapply(
    split(
      finite_plot_data,
      list(finite_plot_data$comparison_display_label, finite_plot_data$method_label),
      drop = TRUE
    ),
    .validation_thin_points,
    max_points = max_points
  ))

  violin_palette <- if (is.null(palette)) {
    .validation_viz_palette_categorical
  } else {
    palette
  }
  fill_values <- .validation_palette(plot_data$method_label, violin_palette)
  y_label <- .validation_residual_axis_label(residual, comparisons)

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$method_label,
      y = .data$value,
      fill = .data$method_label,
      colour = .data$method_label
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#4B5563", linetype = "dashed") +
    ggplot2::geom_violin(
      trim = FALSE,
      alpha = violin_alpha,
      colour = "white",
      linewidth = 0.5,
      scale = "width",
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$comparison_display_label), scales = "free") +
    ggplot2::scale_fill_manual(values = fill_values, name = NULL) +
    ggplot2::scale_colour_manual(values = fill_values, name = NULL) +
    ggplot2::scale_y_continuous(
      labels = .validation_flow_axis_label,
      transform = y_transform
    ) +
    ggplot2::labs(x = NULL, y = y_label) +
    ggplot2::coord_cartesian(clip = "off") +
    .validation_theme(base_size = 11, grid = "y") +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      strip.text = ggplot2::element_text(face = "bold")
    )

  if (isTRUE(show_points)) {
    plot <- plot +
      ggplot2::geom_point(
        data = point_data,
        position = ggplot2::position_jitter(
          width = jitter_width,
          height = 0,
          seed = 123
        ),
        alpha = point_alpha,
        size = point_size,
        stroke = 0,
        na.rm = TRUE
      )
  }

  if (isTRUE(show_counts) && nrow(count_data) > 0L) {
    plot <- plot +
      ggplot2::geom_text(
        data = count_data,
        ggplot2::aes(x = .data$method_label, y = .data$label_y, label = .data$n_label),
        inherit.aes = FALSE,
        fontface = "bold",
        size = 3.4,
        colour = "#111827",
        na.rm = TRUE
      )
  }

  plot
}

#' @rdname plot_validation_residuals
#' @export
plot_validate_flow_residual_violin <- function(...) {
  plot_validation_residuals(...)
}

#' Plot validation scatterplots for raw, adjusted, and benchmark flows
#'
#' Builds faceted scatterplots from `validate_flow_residuals()` data. The
#' default comparison is adjusted versus benchmark, with colour showing the
#' signed difference between the second and first named series. For a single
#' comparison, axis labels name the selected X and Y series directly; for
#' multiple comparisons, facet headers include the compared series.
#'
#' @param residuals A data frame, one `validate_flow_residuals()` result, or a
#'   named list of `validate_flow_residuals()` results.
#' @param comparisons Comparisons to plot. Defaults to
#'   `"adjusted_vs_benchmark"`. Use a character vector to compare multiple
#'   flow-pair definitions. When `"raw_vs_benchmark"` is included, it is shown
#'   once as the unadjusted raw MPD baseline.
#' @param methods Optional character vector of method identifiers or display
#'   labels to include. Default `NULL` includes all methods.
#' @param method_col Column containing the method identifier. Default
#'   `"method"`.
#' @param method_labels Optional named character vector used to relabel methods.
#' @param difference_limits Optional numeric vector of length 2 giving the
#'   colour-scale limits for the signed difference. Values outside the limits
#'   are squished to the end colours.
#' @param difference_quantile Quantile of absolute signed differences used for
#'   robust symmetric colour limits when `difference_limits = NULL`. Set to `1`
#'   to use the full observed range. Default `0.95`.
#' @param white_band Width of the neutral white band around zero as a proportion
#'   of the colour scale. Increase this to make near-zero differences read as
#'   neutral. Default `0.22`.
#' @param point_alpha Point opacity. Default `0.45`.
#' @param point_size Point size. Default `1.1`.
#' @param point_outline Logical; draw points as filled circles with a subtle
#'   neutral outline. Default `TRUE`.
#' @param point_outline_colour Outline colour used when `point_outline = TRUE`.
#'   Default `"#6B7280"`.
#' @param point_stroke Outline stroke width used when `point_outline = TRUE`.
#'   Default `0.12`.
#'
#' @return A `ggplot` object.
#' @export
plot_validation_scatter <- function(residuals,
                                    comparisons = "adjusted_vs_benchmark",
                                    methods = NULL,
                                    method_col = "method",
                                    method_labels = NULL,
                                    difference_limits = NULL,
                                    difference_quantile = 0.95,
                                    white_band = 0.22,
                                    point_alpha = 0.45,
                                    point_size = 1.1,
                                    point_outline = TRUE,
                                    point_outline_colour = "#6B7280",
                                    point_stroke = 0.12) {
  .require_ggplot2()
  residual_data <- .as_validate_residual_data(residuals, method_col = method_col)
  residual_data <- .validation_filter_methods(
    residual_data,
    methods = methods,
    method_col = method_col,
    method_labels = method_labels,
    arg = "residuals"
  )
  plot_data <- .validation_residual_long(
    residual_data = residual_data,
    residual = "signed",
    comparisons = comparisons,
    method_col = method_col,
    method_labels = method_labels
  ) |>
    .validation_add_comparison_display(comparisons = comparisons) |>
    dplyr::mutate(
      x_series_label = dplyr::coalesce(
        unname(stats::setNames(
          .flow_comparison_specs$x_label,
          .flow_comparison_specs$comparison
        )[.data$comparison]),
        .data$x_series
      ),
      y_series_label = dplyr::coalesce(
        unname(stats::setNames(
          .flow_comparison_specs$y_label,
          .flow_comparison_specs$comparison
        )[.data$comparison]),
        .data$y_series
      )
    )
  plot_data$scatter_comparison_label <- if (length(unique(plot_data$comparison)) == 1L) {
    as.character(plot_data$comparison_label)
  } else if (.validation_benchmark_only_comparisons(comparisons)) {
    dplyr::case_when(
      plot_data$comparison == "adjusted_vs_benchmark" ~ "Adjusted flow vs benchmark",
      plot_data$comparison == "raw_vs_benchmark" ~ "Raw MPD flow vs benchmark",
      TRUE ~ as.character(plot_data$comparison_label)
    )
  } else {
    paste0(
      as.character(plot_data$comparison_label),
      "\nX: ",
      plot_data$x_series_label,
      " | Y: ",
      plot_data$y_series_label
    )
  }
  axis_labels <- .validation_scatter_axis_labels(comparisons)
  difference_label <- .validation_difference_label(comparisons)
  method_order <- .validation_method_factor_levels(
    plot_data$method_label,
    axis = "x",
    raw_position = "last"
  )
  plot_data <- plot_data |>
    dplyr::mutate(
      method_label = factor(.data$method_label, levels = method_order)
    )
  if (is.null(difference_limits)) {
    difference_limits <- .validation_symmetric_limits(
      plot_data$difference,
      quantile = difference_quantile
    )
  } else if (
    !is.numeric(difference_limits) ||
      length(difference_limits) != 2L ||
      any(!is.finite(difference_limits)) ||
      difference_limits[1] >= difference_limits[2]
  ) {
    stop(
      "`difference_limits` must be a finite numeric vector of length 2 ",
      "with the lower limit first.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(white_band) ||
      length(white_band) != 1L ||
      !is.finite(white_band) ||
      white_band < 0 ||
      white_band >= 1
  ) {
    stop(
      "`white_band` must be a single finite number in the interval [0, 1).",
      call. = FALSE
    )
  }
  difference_values <- .validation_diverging_values(
    difference_limits,
    white_band = white_band
  )

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$x_flow, y = .data$y_flow)
  )

  if (isTRUE(point_outline)) {
    plot <- plot +
      ggplot2::geom_point(
        ggplot2::aes(fill = .data$difference),
        shape = 21,
        colour = point_outline_colour,
        stroke = point_stroke,
        alpha = point_alpha,
        size = point_size,
        na.rm = TRUE
    )
    difference_scale <- ggplot2::scale_fill_gradientn(
      colours = c("#0000ff", "#FFFFFF", "#FFFFFF", "#fa8775"),
      values = difference_values,
      limits = difference_limits,
      oob = .validation_squish_oob,
      labels = .validation_flow_axis_label,
      name = difference_label
    )
  } else {
    plot <- plot +
      ggplot2::geom_point(
        ggplot2::aes(colour = .data$difference),
        alpha = point_alpha,
        size = point_size,
        na.rm = TRUE
    )
    difference_scale <- ggplot2::scale_colour_gradientn(
      colours = c("#0000ff", "#FFFFFF", "#FFFFFF", "#fa8775"),
      values = difference_values,
      limits = difference_limits,
      oob = .validation_squish_oob,
      labels = .validation_flow_axis_label,
      name = difference_label
    )
  }

  facet_layer <- if (.validation_benchmark_only_comparisons(comparisons)) {
    ggplot2::facet_wrap(
      ggplot2::vars(.data$method_label, .data$scatter_comparison_label),
      scales = "free"
    )
  } else if (length(unique(plot_data$comparison)) > 1L) {
    ggplot2::facet_wrap(
      ggplot2::vars(.data$method_label, .data$scatter_comparison_label),
      scales = "free"
    )
  } else {
    ggplot2::facet_grid(
      ggplot2::vars(.data$method_label),
      ggplot2::vars(.data$scatter_comparison_label),
      scales = "free"
    )
  }

  plot +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      colour = "#4B5563",
      linewidth = 0.5
    ) +
    facet_layer +
    difference_scale +
    ggplot2::scale_x_continuous(labels = .validation_flow_axis_label) +
    ggplot2::scale_y_continuous(labels = .validation_flow_axis_label) +
    ggplot2::labs(x = axis_labels$x, y = axis_labels$y) +
    .validation_theme(base_size = 11, grid = "xy") +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(face = "bold", size = 12.5),
      strip.text.y = ggplot2::element_text(face = "bold", size = 12.5)
    )
}

#' @rdname plot_validation_scatter
#' @export
plot_validate_flow_scatter <- function(...) {
  plot_validation_scatter(...)
}

#' Plot residual outlier bands as stacked bars
#'
#' Summarises absolute flow-comparison differences into residual
#' standard-deviation or quantile bands and plots the share of OD pairs in each
#' band. The default comparison is adjusted versus benchmark.
#'
#' @param residuals A data frame, one `validate_flow_residuals()` result, or a
#'   named list of `validate_flow_residuals()` results.
#' @param method_col Column containing the method identifier. Default
#'   `"method"`.
#' @param method_labels Optional named character vector used to relabel methods.
#' @param comparisons Flow comparison(s) to plot. Defaults to
#'   `"adjusted_vs_benchmark"`. Use `"all"` to facet all supported comparisons.
#'   When `"raw_vs_benchmark"` is included, it is shown once as the unadjusted
#'   raw MPD baseline.
#' @param methods Optional character vector of method identifiers or display
#'   labels to include. Default `NULL` includes all methods.
#' @param band_method Banding strategy. `"sd"` uses standard-deviation bands;
#'   `"quantile"` uses shared absolute-residual quantile cut points. Default
#'   `"sd"`.
#' @param sd_reference Standard-deviation reference. `"selected"` uses the
#'   selected comparison residuals, `"adjusted"` uses the pooled
#'   benchmark-minus-adjusted residuals, and `"pooled"` uses all three canonical
#'   comparison residuals. Used when `band_method = "sd"`.
#' @param quantile_reference Quantile reference. `"selected"` uses the selected
#'   comparison residuals, `"adjusted"` uses pooled benchmark-minus-adjusted
#'   absolute residuals, and `"pooled"` uses all three canonical comparison
#'   residuals. Used when
#'   `band_method = "quantile"`.
#' @param quantile_probs Interior quantile cut points used when
#'   `band_method = "quantile"`. Default `c(0.5, 0.75, 0.9, 0.95)`.
#' @param palette Optional named or unnamed character vector of colours.
#'   Defaults to a colour-blind-aware sequential palette ordered from lower to
#'   higher residual-severity bands.
#' @param label_min_share Minimum segment share, in percentage points, required
#'   for an in-bar label. Default `8`.
#' @param orientation Bar orientation. `"horizontal"` places methods on the
#'   y-axis and shares on the x-axis; `"vertical"` keeps methods on the x-axis.
#'   Default `"horizontal"`.
#'
#' @return A `ggplot` object.
#' @export
plot_validation_residual_bands <- function(residuals,
                                           method_col = "method",
                                           method_labels = NULL,
                                           comparisons = "adjusted_vs_benchmark",
                                           methods = NULL,
                                           band_method = c("sd", "quantile"),
                                           sd_reference = c("selected", "adjusted", "pooled"),
                                           quantile_reference = c("selected", "adjusted", "pooled"),
                                           quantile_probs = c(0.5, 0.75, 0.9, 0.95),
                                           palette = NULL,
                                           label_min_share = 8,
                                           orientation = c("horizontal", "vertical")) {
  .require_ggplot2()
  comparisons <- .normalise_flow_comparisons(comparisons)
  band_method <- match.arg(band_method)
  sd_reference <- match.arg(sd_reference)
  quantile_reference <- match.arg(quantile_reference)
  orientation <- match.arg(orientation)
  residual_data <- .as_validate_residual_data(residuals, method_col = method_col)
  residual_data <- .validation_filter_methods(
    residual_data,
    methods = methods,
    method_col = method_col,
    method_labels = method_labels,
    arg = "residuals"
  )
  .validation_check_columns(
    residual_data,
    c(method_col, "origin", "destination", "mpd_flow", "adj_flow", "benchmark_flow"),
    "residuals"
  )

  adjusted_residual <- residual_data$benchmark_flow - residual_data$adj_flow
  raw_residual <- residual_data$benchmark_flow - residual_data$mpd_flow
  adjustment_residual <- residual_data$adj_flow - residual_data$mpd_flow
  if (
    !is.numeric(quantile_probs) ||
      length(quantile_probs) == 0L ||
      any(!is.finite(quantile_probs)) ||
      any(quantile_probs <= 0 | quantile_probs >= 1) ||
      is.unsorted(quantile_probs, strictly = TRUE)
  ) {
    stop(
      "`quantile_probs` must be a strictly increasing numeric vector with ",
      "values between 0 and 1.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(label_min_share) ||
      length(label_min_share) != 1L ||
      !is.finite(label_min_share) ||
      label_min_share < 0
  ) {
    stop("`label_min_share` must be a single non-negative finite number.", call. = FALSE)
  }

  residual_reference <- dplyr::bind_rows(lapply(comparisons, function(comparison) {
    spec <- .flow_comparison_spec(comparison)
    if (comparison == "raw_vs_benchmark") {
      residual_data |>
        dplyr::select(origin, destination, mpd_flow, benchmark_flow) |>
        dplyr::distinct() |>
        dplyr::transmute(
          comparison = comparison,
          comparison_label = spec$comparison_label,
          method_label = "Unadjusted raw MPD",
          residual = .data$benchmark_flow - .data$mpd_flow
        )
    } else {
      comparison_tbl <- residual_data
      comparison_tbl$residual <- .flow_comparison_residual(
        comparison_tbl,
        comparison
      )
      comparison_tbl |>
        dplyr::mutate(
          comparison = comparison,
          comparison_label = spec$comparison_label,
          method_label = .validation_method_label(
            comparison_tbl,
            method_col,
            method_labels
          )
        ) |>
        dplyr::select(comparison, comparison_label, method_label, residual)
    }
  }))

  if (band_method == "sd") {
    sd_values <- switch(
      sd_reference,
      selected = residual_reference$residual,
      adjusted = adjusted_residual,
      pooled = c(adjusted_residual, raw_residual, adjustment_residual)
    )
    residual_sd <- stats::sd(sd_values, na.rm = TRUE)
    if (!is.finite(residual_sd) || residual_sd <= 0) {
      residual_sd <- 1
    }

    band_levels <- c(
      "Less than 2 SD",
      "2 to 3 SD",
      "3 to 4 SD",
      "Greater than 4 SD"
    )

    residual_reference <- residual_reference |>
      dplyr::mutate(
        residual_sd_score = abs(.data$residual) / residual_sd,
        residual_band = dplyr::case_when(
          .data$residual_sd_score > 4 ~ "Greater than 4 SD",
          .data$residual_sd_score > 3 ~ "3 to 4 SD",
          .data$residual_sd_score > 2 ~ "2 to 3 SD",
          TRUE ~ "Less than 2 SD"
        )
      )
  } else {
    quantile_values <- switch(
      quantile_reference,
      selected = abs(residual_reference$residual),
      adjusted = abs(adjusted_residual),
      pooled = abs(c(adjusted_residual, raw_residual, adjustment_residual))
    )
    quantile_values <- quantile_values[is.finite(quantile_values)]
    if (length(quantile_values) == 0L) {
      stop(
        "`residuals` must contain at least one finite residual for quantile bands.",
        call. = FALSE
      )
    }

    quantile_breaks <- stats::quantile(
      quantile_values,
      probs = quantile_probs,
      na.rm = TRUE,
      names = FALSE
    )
    keep_breaks <- !duplicated(quantile_breaks)
    quantile_breaks <- quantile_breaks[keep_breaks]
    quantile_probs <- quantile_probs[keep_breaks]

    if (length(quantile_breaks) == 0L) {
      band_levels <- "All residuals"
      residual_reference <- residual_reference |>
        dplyr::mutate(residual_band = band_levels)
    } else {
      band_levels <- .validation_quantile_band_labels(quantile_probs)
      residual_reference <- residual_reference |>
        dplyr::mutate(
          residual_band = as.character(cut(
            abs(.data$residual),
            breaks = c(-Inf, quantile_breaks, Inf),
            labels = band_levels,
            include.lowest = TRUE,
            right = TRUE
          ))
        )
    }
  }

  residual_reference <- residual_reference |>
    dplyr::filter(is.finite(.data$residual), !is.na(.data$residual_band))
  residual_reference <- .validation_add_comparison_display(
    residual_reference,
    comparisons
  )

  plot_data <- residual_reference |>
    dplyr::count(
      .data$comparison_display,
      .data$comparison_display_label,
      .data$method_label,
      .data$residual_band,
      name = "n"
    ) |>
    dplyr::group_by(.data$comparison_display, .data$method_label) |>
    dplyr::mutate(share = 100 * .data$n / sum(.data$n)) |>
    dplyr::ungroup()

  method_grid <- residual_reference |>
    dplyr::distinct(
      .data$comparison_display,
      .data$comparison_display_label,
      .data$method_label
    )
  complete_grid <- merge(
    method_grid,
    data.frame(
    residual_band = band_levels,
    stringsAsFactors = FALSE
    ),
    by = NULL
  )

  plot_data <- complete_grid |>
    dplyr::left_join(
      plot_data,
      by = c(
        "comparison_display",
        "comparison_display_label",
        "method_label",
        "residual_band"
      )
    ) |>
    dplyr::mutate(
      n = dplyr::coalesce(.data$n, 0L),
      share = dplyr::coalesce(.data$share, 0),
      comparison = .data$comparison_display,
      comparison_label = .data$comparison_display_label,
      residual_band = factor(.data$residual_band, levels = band_levels),
      label = dplyr::if_else(
        .data$share >= label_min_share,
        paste0(sprintf("%.1f", .data$share), "%"),
        ""
      ),
      label_colour = dplyr::if_else(
        length(band_levels) > 2L &
          as.integer(.data$residual_band) >= ceiling(length(band_levels) * 0.7),
        "white",
        "#111827"
      )
    )

  fill_values <- .validation_palette(
    band_levels,
    if (is.null(palette)) .validation_residual_band_palette else palette
  )

  method_order <- .validation_method_factor_levels(
    plot_data$method_label,
    axis = if (orientation == "horizontal") "y" else "x",
    raw_position = "last"
  )
  plot_data <- plot_data |>
    dplyr::mutate(
      method_label = factor(
        .data$method_label,
        levels = method_order
      ),
      comparison_display_label = factor(.data$comparison_display_label)
    )

  stack_position <- ggplot2::position_stack(reverse = TRUE)

  if (orientation == "horizontal") {
    plot <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(y = .data$method_label, x = .data$share, fill = .data$residual_band)
    ) +
      ggplot2::geom_col(
        width = 0.72,
        colour = "white",
        linewidth = 0.35,
        position = stack_position
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$label, colour = .data$label_colour),
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        size = 3.4,
        na.rm = TRUE
      ) +
      ggplot2::scale_x_continuous(
        breaks = c(0, 25, 50, 75, 100),
        labels = function(x) paste0(x, "%"),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +
      ggplot2::coord_cartesian(xlim = c(0, 100), clip = "off") +
      ggplot2::labs(x = "Share of OD pairs", y = NULL) +
      .validation_theme(base_size = 11, grid = "xy") +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(colour = "#344054")
      )
  } else {
    plot <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data$method_label, y = .data$share, fill = .data$residual_band)
    ) +
      ggplot2::geom_col(
        width = 0.72,
        colour = "white",
        linewidth = 0.35,
        position = stack_position
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$label, colour = .data$label_colour),
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        size = 3.4,
        na.rm = TRUE
      ) +
      ggplot2::scale_y_continuous(
        breaks = c(0, 25, 50, 75, 100),
        labels = function(x) paste0(x, "%"),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +
      ggplot2::coord_cartesian(ylim = c(0, 100), clip = "off") +
      ggplot2::labs(x = NULL, y = "Share of OD pairs") +
      .validation_theme(base_size = 11, grid = "y") +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
      )
  }

  plot <- plot +
    ggplot2::scale_fill_manual(
      values = fill_values,
      name = "Residual band",
      guide = ggplot2::guide_legend(ncol = 1, byrow = FALSE)
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::theme(
      legend.position = "right",
      legend.box = "vertical",
      panel.grid.minor = ggplot2::element_blank()
    )

  if (length(unique(plot_data$comparison_display)) > 1L) {
    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(.data$comparison_display_label),
        scales = "free_y"
      )
  }

  plot
}

#' @rdname plot_validation_residual_bands
#' @export
plot_validate_flow_residual_heatmap <- function(...) {
  plot_validation_residual_bands(...)
}

#' Plot distributional allocation validation as a method heatmap
#'
#' Plots KL or Jensen-Shannon divergence summaries from
#' `validate_flow_distribution()`.
#'
#' @param distribution_results A data frame, one `validate_flow_distribution()`
#'   result, or a named list of results.
#' @param metric Divergence metric to plot: `"jsd"` or `"kl"`.
#' @param value Summary statistic to plot: `"mean"`, `"weighted_mean"`, or
#'   `"median"`.
#' @param comparisons Flow comparison(s) to plot. Defaults to
#'   `"adjusted_vs_benchmark"`. Use `"all"` to show all comparison rows present
#'   in `distribution_results`. When `"raw_vs_benchmark"` is included, it is
#'   shown as a single unadjusted raw MPD baseline row.
#' @param methods Optional character vector of method identifiers or display
#'   labels to include. Default `NULL` includes all methods.
#' @param method_col Column containing the method identifier. Default
#'   `"method"`.
#' @param method_labels Optional named character vector used to relabel methods.
#'
#' @return A `ggplot` object.
#' @export
plot_validation_distribution <- function(distribution_results,
                                         metric = c("jsd", "kl"),
                                         value = c("mean", "weighted_mean", "median"),
                                         comparisons = "adjusted_vs_benchmark",
                                         methods = NULL,
                                         method_col = "method",
                                         method_labels = NULL) {
  .require_ggplot2()
  metric <- match.arg(metric)
  value <- match.arg(value)
  comparisons <- .normalise_flow_comparisons(comparisons)
  summary <- .as_validate_distribution_summary(distribution_results, method_col = method_col)
  value_col <- paste(metric, value, sep = "_")
  .validation_check_columns(summary, c(method_col, "comparison", value_col), "distribution_results")
  summary <- .validation_filter_methods(
    summary,
    methods = methods,
    method_col = method_col,
    method_labels = method_labels,
    arg = "distribution_results"
  )
  summary <- summary[summary$comparison %in% comparisons, , drop = FALSE]
  if (nrow(summary) == 0L) {
    stop(
      "`distribution_results` does not contain the requested comparison(s).",
      call. = FALSE
    )
  }

  plot_data <- summary |>
    dplyr::mutate(
      method_label = .validation_method_label(summary, method_col, method_labels),
      comparison_label = dplyr::coalesce(
        .flow_comparison_label(.data$comparison),
        .data$comparison
      ),
      divergence = .data[[value_col]]
    ) |>
    dplyr::mutate(
      method = dplyr::if_else(
        .data$comparison == "raw_vs_benchmark",
        .validation_raw_baseline_id,
        as.character(.data[[method_col]])
      ),
      method_label = dplyr::if_else(
        .data$comparison == "raw_vs_benchmark",
        .validation_raw_baseline_label,
        as.character(.data$method_label)
      )
    ) |>
    dplyr::distinct(
      .data$comparison,
      .data$method_label,
      .keep_all = TRUE
    )
  plot_data <- .validation_add_comparison_display(plot_data, comparisons)

  method_order <- .validation_method_factor_levels(
    plot_data$method_label,
    axis = "y",
    raw_position = "last"
  )
  plot_data <- plot_data |>
    dplyr::mutate(
      method_label = factor(
        .data$method_label,
        levels = method_order
      )
    )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$comparison_display_label,
      y = .data$method_label,
      fill = .data$divergence
    )
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", .data$divergence)),
      size = 3.3,
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_gradient(low = "#fff7bc", high = "#0000ff", name = toupper(metric)) +
    ggplot2::labs(x = NULL, y = NULL) +
    .validation_theme(base_size = 11, grid = "none") +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
    )
}

#' @rdname plot_validation_distribution
#' @export
plot_validate_flow_distribution_heatmap <- function(...) {
  plot_validation_distribution(...)
}

#' Plot pairwise distributional allocation divergences
#'
#' Builds an intuitive method-by-comparison divergence chart from
#' `validate_flow_distribution()` summary rows. Lower values indicate closer
#' allocation fidelity. A full reference-by-comparison matrix is also available
#' with `plot_type = "heatmap"`; for JSD, this matrix can mirror supplied
#' off-diagonal entries because JSD is symmetric. KL entries remain directional.
#'
#' @param distribution_results A data frame, one `validate_flow_distribution()`
#'   result, or a named list of results.
#' @param metric Divergence metric to plot: `"jsd"` or `"kl"`.
#' @param value Summary statistic to plot: `"mean"`, `"weighted_mean"`, or
#'   `"median"`.
#' @param comparisons Flow comparison(s) to plot. Defaults to
#'   `"adjusted_vs_benchmark"`. Use `"all"` to include all comparison rows
#'   present in `distribution_results`; with `plot_type = "heatmap"` this
#'   produces the full pairwise matrix. When `"raw_vs_benchmark"` is included
#'   in comparison plots, it is shown as a single unadjusted raw MPD baseline
#'   row.
#' @param methods Optional character vector of method identifiers or display
#'   labels to include. Default `NULL` includes all methods.
#' @param method_col Column containing the method identifier. Default
#'   `"method"`.
#' @param method_labels Optional named character vector used to relabel methods.
#' @param plot_type Plot type to return: `"comparison"` for a horizontal
#'   benchmark-comparison chart or `"heatmap"` for the full pairwise matrix.
#' @param sort Sort order for the comparison chart. Use `"none"` to keep method
#'   order, `"ascending"` for smallest-to-largest divergence, or
#'   `"descending"` for largest-to-smallest divergence. Adjusted methods are
#'   sorted; the raw MPD baseline remains at the edge as a reference row.
#'   Default `"none"`.
#' @param mirror_jsd Logical. If `TRUE`, mirror JSD values to show a symmetric
#'   matrix. Default `TRUE`.
#'
#' @return A `ggplot` object.
#' @export
plot_validation_distribution_pairwise <- function(distribution_results,
                                                  metric = c("jsd", "kl"),
                                                  value = c("mean", "weighted_mean", "median"),
                                                  comparisons = "adjusted_vs_benchmark",
                                                  methods = NULL,
                                                  method_col = "method",
                                                  method_labels = NULL,
                                                  plot_type = c("comparison", "heatmap"),
                                                  sort = c("none", "ascending", "descending"),
                                                  mirror_jsd = TRUE) {
  .require_ggplot2()
  metric <- match.arg(metric)
  value <- match.arg(value)
  plot_type <- match.arg(plot_type)
  sort <- match.arg(sort)
  comparisons <- .normalise_flow_comparisons(comparisons)
  summary <- .as_validate_distribution_summary(distribution_results, method_col = method_col)
  value_col <- paste(metric, value, sep = "_")
  .validation_check_columns(
    summary,
    c(method_col, "reference_distribution", "comparison_distribution", value_col),
    "distribution_results"
  )
  summary <- .validation_filter_methods(
    summary,
    methods = methods,
    method_col = method_col,
    method_labels = method_labels,
    arg = "distribution_results"
  )
  summary <- summary[summary$comparison %in% comparisons, , drop = FALSE]
  if (nrow(summary) == 0L) {
    stop(
      "`distribution_results` does not contain the requested comparison(s).",
      call. = FALSE
    )
  }

  plot_data <- summary |>
    dplyr::transmute(
      method = .data[[method_col]],
      method_label = .validation_method_label(summary, method_col, method_labels),
      comparison = .data$comparison,
      reference_distribution = .data$reference_distribution,
      comparison_distribution = .data$comparison_distribution,
      divergence = .data[[value_col]]
    ) |>
    dplyr::mutate(
      method = dplyr::if_else(
        .data$comparison == "raw_vs_benchmark",
        .validation_raw_baseline_id,
        as.character(.data$method)
      ),
      method_label = dplyr::if_else(
        .data$comparison == "raw_vs_benchmark",
        .validation_raw_baseline_label,
        as.character(.data$method_label)
      )
    ) |>
    dplyr::distinct(
      .data$comparison,
      .data$method_label,
      .data$reference_distribution,
      .data$comparison_distribution,
      .keep_all = TRUE
    )
  plot_data <- .validation_add_comparison_display(plot_data, comparisons)

  if (plot_type == "comparison") {
    plot_data <- plot_data |>
      dplyr::mutate(
        comparison_label = dplyr::coalesce(
          .flow_comparison_label(.data$comparison),
          .data$comparison
        ),
        divergence_label = sprintf("%.3f", .data$divergence)
      )
    if (sort == "none") {
      method_order <- .validation_method_factor_levels(
        plot_data$method_label,
        axis = "y",
        raw_position = "last"
      )
    } else {
      baseline_label <- .validation_raw_baseline_label
      baseline_order <- intersect(
        unique(as.character(plot_data$method_label)),
        baseline_label
      )
      method_order <- plot_data |>
        dplyr::filter(.data$method_label != baseline_label) |>
        dplyr::group_by(.data$method_label) |>
        dplyr::summarise(.sort_value = mean(.data$divergence, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(if (sort == "ascending") .data$.sort_value else dplyr::desc(.data$.sort_value)) |>
        dplyr::pull("method_label") |>
        as.character()
      method_order <- rev(c(method_order, baseline_order))
    }
    comparison_order <- unique(as.character(plot_data$comparison_label))
    plot_data <- plot_data |>
      dplyr::mutate(
        method_label = factor(.data$method_label, levels = method_order),
        comparison_label = factor(.data$comparison_label, levels = comparison_order)
      )

    fill_values <- .validation_palette(
      comparison_order,
      .validation_viz_palette_categorical
    )
    dodge <- ggplot2::position_dodge2(width = 0.72, preserve = "single")

    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .data$method_label,
          y = .data$divergence,
          fill = .data$comparison_label
        )
      ) +
        ggplot2::geom_col(
          width = 0.64,
          colour = "white",
          linewidth = 0.35,
          position = dodge
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = .data$divergence_label),
          position = dodge,
          hjust = -0.15,
          size = 3.3,
          na.rm = TRUE
        ) +
        ggplot2::coord_flip(clip = "off") +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.16))
        ) +
        ggplot2::scale_fill_manual(values = fill_values, name = "Comparison") +
        ggplot2::labs(
          x = NULL,
          y = .validation_distribution_axis_label(metric, value)
        ) +
        .validation_theme(base_size = 11, grid = "xy") +
        ggplot2::theme(
          legend.position = "bottom",
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(colour = "#344054")
        )
    )
  }

  if (metric == "jsd" && isTRUE(mirror_jsd)) {
    plot_data <- dplyr::bind_rows(
      plot_data,
      plot_data |>
        dplyr::transmute(
          method = .data$method,
          method_label = .data$method_label,
          reference_distribution = .data$comparison_distribution,
          comparison_distribution = .data$reference_distribution,
          divergence = .data$divergence
        )
    ) |>
      dplyr::distinct()
  }

  diagonal <- dplyr::bind_rows(lapply(split(plot_data, plot_data$method_label), function(data) {
    distributions <- sort(unique(c(data$reference_distribution, data$comparison_distribution)))
    tibble::tibble(
      method = data$method[1],
      method_label = data$method_label[1],
      reference_distribution = distributions,
      comparison_distribution = distributions,
      divergence = 0
    )
  }))

  plot_data <- dplyr::bind_rows(plot_data, diagonal) |>
    dplyr::distinct() |>
    dplyr::mutate(
      reference_label = dplyr::coalesce(
        unname(.validation_distribution_labels[.data$reference_distribution]),
        .data$reference_distribution
      ),
      comparison_label = dplyr::coalesce(
        unname(.validation_distribution_labels[.data$comparison_distribution]),
        .data$comparison_distribution
      )
    )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$comparison_label, y = .data$reference_label, fill = .data$divergence)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", .data$divergence)),
      size = 3.3,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$method_label)) +
    ggplot2::scale_fill_gradient(low = "#fff7bc", high = "#0000ff", name = toupper(metric)) +
    ggplot2::labs(x = "Comparison distribution", y = "Reference distribution") +
    .validation_theme(base_size = 11, grid = "none") +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' @rdname plot_validation_distribution_pairwise
#' @export
plot_validate_flow_distribution_pairwise_heatmap <- function(...,
                                                             plot_type = "heatmap") {
  plot_validation_distribution_pairwise(..., plot_type = plot_type)
}

#' Plot residual-structure validation metrics
#'
#' Plots the scalar diagnostics returned by
#' `validate_flow_residual_structure()`: residual-versus-benchmark-flow
#' correlation, Moran's I, and residual-versus-covariate correlation when
#' available.
#'
#' @param structure_results A data frame, one
#'   `validate_flow_residual_structure()` result, or a named list of results.
#' @param comparisons Flow comparison(s) to plot. Defaults to
#'   `"adjusted_vs_benchmark"`. Use `"all"` when `structure_results` contains
#'   several comparison rows.
#' @param methods Optional character vector of method identifiers or display
#'   labels to include. Default `NULL` includes all methods.
#' @param method_col Column containing the method identifier. Default
#'   `"method"`.
#' @param method_labels Optional named character vector used to relabel methods.
#' @param palette Optional character vector of colours.
#' @param near_zero_band Optional non-negative numeric width around zero to
#'   shade as a near-zero reference band. Use `NULL` to suppress the band.
#'   Default `0.1`.
#' @param show_value_labels Logical; label points with rounded diagnostic
#'   values. Default `TRUE`.
#' @param value_digits Number of decimal places used when
#'   `show_value_labels = TRUE`. Default `2`.
#'
#' @return A `ggplot` object.
#' @export
plot_validation_structure <- function(structure_results,
                                      comparisons = "adjusted_vs_benchmark",
                                      methods = NULL,
                                      method_col = "method",
                                      method_labels = NULL,
                                      palette = NULL,
                                      near_zero_band = 0.1,
                                      show_value_labels = TRUE,
                                      value_digits = 2) {
  .require_ggplot2()
  comparisons <- .normalise_flow_comparisons(comparisons)
  if (
    !is.null(near_zero_band) &&
      (!is.numeric(near_zero_band) ||
        length(near_zero_band) != 1L ||
        !is.finite(near_zero_band) ||
        near_zero_band < 0)
  ) {
    stop("`near_zero_band` must be `NULL` or a single non-negative finite number.", call. = FALSE)
  }
  if (!is.logical(show_value_labels) || length(show_value_labels) != 1L || is.na(show_value_labels)) {
    stop("`show_value_labels` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (
    !is.numeric(value_digits) ||
      length(value_digits) != 1L ||
      !is.finite(value_digits) ||
      value_digits < 0 ||
      value_digits != as.integer(value_digits)
  ) {
    stop("`value_digits` must be a single non-negative whole number.", call. = FALSE)
  }
  summary <- .as_validate_structure_summary(structure_results, method_col = method_col)
  if (!"comparison" %in% names(summary)) {
    residual_type <- if ("residual_type" %in% names(summary)) {
      as.character(summary$residual_type)
    } else {
      rep("adjusted", nrow(summary))
    }
    summary$comparison <- dplyr::case_when(
      residual_type == "mpd" ~ "raw_vs_benchmark",
      residual_type == "adjustment" ~ "raw_vs_adjusted",
      TRUE ~ "adjusted_vs_benchmark"
    )
  }
  summary <- .validation_filter_methods(
    summary,
    methods = methods,
    method_col = method_col,
    method_labels = method_labels,
    arg = "structure_results"
  )
  summary <- summary[summary$comparison %in% comparisons, , drop = FALSE]
  if (nrow(summary) == 0L) {
    stop(
      "`structure_results` does not contain the requested comparison(s).",
      call. = FALSE
    )
  }
  metric_cols <- c(
    "pearson_residual_benchmark_flow",
    "moran_i",
    "pearson_residual_covariate"
  )
  .validation_check_columns(summary, c(method_col, metric_cols), "structure_results")

  metric_labels <- c(
    pearson_residual_benchmark_flow = "Residual-flow\ncorrelation (r)",
    moran_i = "Spatial autocorrelation\n(Moran's I)",
    pearson_residual_covariate = "Residual-covariate\ncorrelation (r)"
  )
  metric_label_levels <- c(
    "Spatial autocorrelation\n(Moran's I)",
    "Residual-flow\ncorrelation (r)",
    "Residual-covariate\ncorrelation (r)"
  )

  plot_data <- dplyr::bind_rows(lapply(metric_cols, function(metric_col) {
    tibble::tibble(
      method = summary[[method_col]],
      method_label = .validation_method_label(summary, method_col, method_labels),
      comparison = summary$comparison,
      comparison_label = .flow_comparison_label(summary$comparison),
      metric = metric_col,
      metric_label = unname(metric_labels[metric_col]),
      value = as.numeric(summary[[metric_col]])
    )
  }))

  method_order <- .validation_method_factor_levels(
    plot_data$method_label,
    axis = "y",
    raw_position = "last"
  )
  plot_data <- plot_data |>
    dplyr::mutate(
      method_label = factor(.data$method_label, levels = method_order),
      metric_label = factor(.data$metric_label, levels = metric_label_levels),
      value_label = dplyr::if_else(
        is.finite(.data$value),
        format(
          round(.data$value, digits = as.integer(value_digits)),
          nsmall = as.integer(value_digits),
          trim = TRUE,
          scientific = FALSE
        ),
        ""
      )
    )

  structure_axis_limits <- dplyr::bind_rows(lapply(
    split(
      plot_data,
      list(plot_data$comparison_label, plot_data$metric_label),
      drop = TRUE
    ),
    function(data) {
      finite_values <- abs(data$value[is.finite(data$value)])
      if (length(finite_values) == 0L) {
        limit <- 0.25
      } else {
        limit <- max(finite_values)
      }
      limit <- max(limit * 1.15, 0.25)
      limit <- min(limit, 1)
      tibble::tibble(
        comparison_label = data$comparison_label[1],
        metric_label = factor(data$metric_label[1], levels = metric_label_levels),
        method_label = factor(method_order[1], levels = method_order),
        x = c(-limit, limit),
        x_limit = limit,
        label_offset = max(limit * 0.04, 0.015)
      )
    }
  ))

  plot_data <- plot_data |>
    dplyr::left_join(
      structure_axis_limits |>
        dplyr::distinct(
          .data$comparison_label,
          .data$metric_label,
          .data$x_limit,
          .data$label_offset
        ),
      by = c("comparison_label", "metric_label")
    ) |>
    dplyr::mutate(
      value_label_x = dplyr::case_when(
        is.finite(.data$value) & .data$value >= .data$x_limit - 2 * .data$label_offset ~
          .data$value - .data$label_offset,
        is.finite(.data$value) & .data$value <= -.data$x_limit + 2 * .data$label_offset ~
          .data$value + .data$label_offset,
        .data$value >= 0 ~ .data$value + .data$label_offset,
        TRUE ~ .data$value - .data$label_offset
      ),
      value_label_hjust = dplyr::case_when(
        is.finite(.data$value) & .data$value >= .data$x_limit - 2 * .data$label_offset ~ 1,
        is.finite(.data$value) & .data$value <= -.data$x_limit + 2 * .data$label_offset ~ 0,
        .data$value >= 0 ~ 0,
        TRUE ~ 1
      )
    )

  if (is.null(near_zero_band) || near_zero_band == 0) {
    band_data <- structure_axis_limits[0, c("comparison_label", "metric_label"), drop = FALSE]
    band_data$xmin <- numeric()
    band_data$xmax <- numeric()
    band_data$ymin <- numeric()
    band_data$ymax <- numeric()
    band_note <- NULL
  } else {
    band_value_label <- format(
      round(near_zero_band, digits = 2),
      nsmall = 2,
      trim = TRUE,
      scientific = FALSE
    )
    band_note <- paste0(
      "Shaded area: near-zero reference band (absolute diagnostic value <= ",
      band_value_label,
      ")."
    )
    band_data <- structure_axis_limits |>
      dplyr::distinct(.data$comparison_label, .data$metric_label) |>
      dplyr::mutate(
        xmin = -near_zero_band,
        xmax = near_zero_band,
        ymin = -Inf,
        ymax = Inf
      )
  }

  colour_values <- .validation_palette(plot_data$method_label, palette)
  facet_layer <- if (length(unique(plot_data$comparison_label)) == 1L) {
    ggplot2::facet_wrap(
      ggplot2::vars(.data$metric_label),
      scales = "free_x"
    )
  } else {
    ggplot2::facet_wrap(
      ggplot2::vars(.data$comparison_label, .data$metric_label),
      scales = "free_x"
    )
  }

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      y = .data$method_label,
      x = .data$value,
      colour = .data$method_label
    )
  ) +
    ggplot2::geom_blank(
      data = structure_axis_limits,
      ggplot2::aes(x = .data$x, y = .data$method_label),
      inherit.aes = FALSE
    )

  if (nrow(band_data) > 0L) {
    plot <- plot +
      ggplot2::geom_rect(
        data = band_data,
        ggplot2::aes(
          xmin = .data$xmin,
          xmax = .data$xmax,
          ymin = .data$ymin,
          ymax = .data$ymax
        ),
        inherit.aes = FALSE,
        fill = "#E5E7EB",
        alpha = 0.35,
        colour = NA
      )
  }

  plot <- plot +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#4B5563") +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = .data$value, yend = .data$method_label),
      linewidth = 0.8,
      alpha = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(size = 2.5, na.rm = TRUE) +
    facet_layer +
    ggplot2::scale_colour_manual(values = colour_values, name = NULL) +
    ggplot2::labs(x = NULL, y = NULL, caption = band_note) +
    .validation_theme(base_size = 11, grid = "xy") +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        colour = "#6B7280",
        size = 8.5,
        margin = ggplot2::margin(t = 6)
      ),
      strip.text = ggplot2::element_text(face = "bold")
    )

  if (isTRUE(show_value_labels)) {
    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = .data$value_label_x,
          label = .data$value_label,
          hjust = .data$value_label_hjust
        ),
        size = 3.1,
        colour = "#374151",
        show.legend = FALSE,
        na.rm = TRUE
      ) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme(
        plot.margin = ggplot2::margin(5.5, 18, 5.5, 5.5)
      )
  }

  plot
}

#' @rdname plot_validation_structure
#' @export
plot_validate_flow_structure <- function(...) {
  plot_validation_structure(...)
}

#' Plot validation LISA indicators on user-supplied boundaries
#'
#' Maps residual-derived Local Moran/LISA cluster classes returned by
#' `validate_flow_residual_structure(local_moran = TRUE)`. The function requires
#' users to supply `sf` boundary objects because `debiasR` does not ship,
#' download, or infer spatial boundaries. Optional outline boundaries can be
#' supplied for larger administrative areas.
#'
#' @param structure_results A data frame containing LISA map data, one
#'   `validate_flow_residual_structure()` result, or a named list of results.
#'   The data must contain `lisa_cluster`, which is produced when
#'   `validate_flow_residual_structure(local_moran = TRUE)`.
#' @param boundaries User-supplied `sf` polygon or multipolygon object with one
#'   row per area to map.
#' @param boundary_area_col Column in `boundaries` identifying areas. Default
#'   `"area"`.
#' @param area_col Column in the validation map data identifying areas. Default
#'   `"area"`.
#' @param comparisons Flow comparison(s) to plot. Defaults to
#'   `"adjusted_vs_benchmark"`. Use `"all"` when `structure_results` contains
#'   several comparison rows.
#' @param methods Optional character vector of method identifiers or display
#'   labels to include. Default `NULL` includes all methods.
#' @param method_col Column containing the method identifier. Default
#'   `"method"`.
#' @param method_labels Optional named character vector used to relabel methods.
#' @param cluster_col Column containing LISA cluster classes. Default
#'   `"lisa_cluster"`.
#' @param p_value_col Column containing Local Moran/LISA p-values. Default
#'   `"p_value"`.
#' @param p_value_threshold Numeric threshold used to display statistically
#'   significant LISA clusters. Areas with p-values above this threshold are
#'   shown as not significant. Set `NULL` to plot `cluster_col` without
#'   p-value masking. Default `0.05`.
#' @param palette Optional named or unnamed character vector of colours for LISA
#'   clusters. Defaults to the supplied validation review palette plus neutral
#'   greys for non-significant or undefined areas.
#' @param missing_fill Fill colour for boundaries that are not present in the
#'   validation results. Default `"#F3F4F6"`.
#' @param boundary_colour Boundary line colour for mapped areas. Default `NA`
#'   avoids drawing small-area internal borders.
#' @param boundary_linewidth Boundary line width for mapped areas. Default `0`.
#' @param outline_boundaries Optional user-supplied `sf` object for larger
#'   administrative boundaries to draw over the mapped areas.
#' @param outline_colour Larger-boundary outline colour. Default `"white"`.
#' @param outline_linewidth Larger-boundary outline width. Default `0.45`.
#'
#' @return A `ggplot` object.
#' @export
plot_validation_lisa_map <- function(structure_results,
                                     boundaries,
                                     boundary_area_col = "area",
                                     area_col = "area",
                                     comparisons = "adjusted_vs_benchmark",
                                     methods = NULL,
                                     method_col = "method",
                                     method_labels = NULL,
                                     cluster_col = "lisa_cluster",
                                     p_value_col = "p_value",
                                     p_value_threshold = 0.05,
                                     palette = NULL,
                                     missing_fill = "#F3F4F6",
                                     boundary_colour = NA,
                                     boundary_linewidth = 0,
                                     outline_boundaries = NULL,
                                     outline_colour = "white",
                                     outline_linewidth = 0.45) {
  .require_ggplot2()
  comparisons <- .normalise_flow_comparisons(comparisons)
  map_data <- .as_validate_structure_map_data(
    structure_results,
    method_col = method_col
  )
  .validation_check_columns(
    map_data,
    c(area_col, method_col, cluster_col),
    "structure_results"
  )
  if (!"comparison" %in% names(map_data)) {
    residual_type <- if ("residual_type" %in% names(map_data)) {
      as.character(map_data$residual_type)
    } else {
      rep("adjusted", nrow(map_data))
    }
    map_data$comparison <- dplyr::case_when(
      residual_type == "mpd" ~ "raw_vs_benchmark",
      residual_type == "adjustment" ~ "raw_vs_adjusted",
      TRUE ~ "adjusted_vs_benchmark"
    )
  }
  map_data <- .validation_filter_methods(
    map_data,
    methods = methods,
    method_col = method_col,
    method_labels = method_labels,
    arg = "structure_results"
  )
  map_data <- map_data[map_data$comparison %in% comparisons, , drop = FALSE]
  if (nrow(map_data) == 0L) {
    stop(
      "`structure_results` does not contain the requested comparison(s).",
      call. = FALSE
    )
  }
  if (missing(boundaries)) {
    stop("`boundaries` must be supplied as an `sf` object.", call. = FALSE)
  }
  if (!inherits(boundaries, "sf")) {
    stop("`boundaries` must be an `sf` object.", call. = FALSE)
  }
  if (!boundary_area_col %in% names(boundaries)) {
    stop("`boundary_area_col` must name a column in `boundaries`.", call. = FALSE)
  }
  if (!is.null(outline_boundaries) && !inherits(outline_boundaries, "sf")) {
    stop("`outline_boundaries` must be an `sf` object.", call. = FALSE)
  }
  if (!is.null(p_value_threshold)) {
    if (!is.numeric(p_value_threshold) ||
        length(p_value_threshold) != 1L ||
        is.na(p_value_threshold) ||
        p_value_threshold < 0 ||
        p_value_threshold > 1) {
      stop("`p_value_threshold` must be a number between 0 and 1, or `NULL`.", call. = FALSE)
    }
    if (!p_value_col %in% names(map_data)) {
      stop("`p_value_col` must name a column in `structure_results`.", call. = FALSE)
    }
    if (!is.numeric(map_data[[p_value_col]])) {
      stop("`p_value_col` must identify a numeric p-value column.", call. = FALSE)
    }
  }

  map_data <- map_data |>
    dplyr::mutate(
      method_label = .validation_method_label(map_data, method_col, method_labels),
      comparison_label = .flow_comparison_label(.data$comparison),
      .lisa_cluster_raw = as.character(.data[[cluster_col]])
    )

  if (is.null(p_value_threshold)) {
    map_data <- map_data |>
      dplyr::mutate(.lisa_cluster = .data$.lisa_cluster_raw)
  } else {
    map_data <- map_data |>
      dplyr::mutate(
        .lisa_p_value = .data[[p_value_col]],
        .lisa_cluster = dplyr::case_when(
          .data$.lisa_cluster_raw %in% c("no neighbours", "undefined") ~
            .data$.lisa_cluster_raw,
          is.finite(.data$.lisa_p_value) &
            .data$.lisa_p_value <= p_value_threshold ~
            .data$.lisa_cluster_raw,
          TRUE ~ "not significant"
        )
      )
  }

  map_join_data <- map_data |>
    dplyr::select(dplyr::any_of(c(
      area_col,
      method_col,
      "method_label",
      "comparison",
      "comparison_label",
      cluster_col,
      ".lisa_cluster_raw",
      ".lisa_cluster",
      "local_moran_i",
      "lisa_quadrant",
      "p_value",
      "p_adjusted",
      "significant",
      "residual_z",
      "spatial_lag_z"
    )))

  join_by <- stats::setNames(area_col, boundary_area_col)
  plot_data <- dplyr::inner_join(boundaries, map_join_data, by = join_by)
  if (nrow(plot_data) == 0L) {
    stop(
      "`boundaries` and `structure_results` do not share any area identifiers.",
      call. = FALSE
    )
  }

  cluster_values <- unique(as.character(stats::na.omit(plot_data$.lisa_cluster)))
  cluster_order <- unique(c(
    .validation_lisa_cluster_levels[
      .validation_lisa_cluster_levels %in% cluster_values
    ],
    setdiff(cluster_values, .validation_lisa_cluster_levels)
  ))
  if (length(cluster_order) == 0L) {
    cluster_order <- "undefined"
  }
  cluster_labels <- dplyr::coalesce(
    unname(.validation_lisa_cluster_labels[cluster_order]),
    cluster_order
  )
  fill_values <- if (is.null(palette)) {
    values <- .validation_lisa_cluster_palette[cluster_order]
    missing_values <- is.na(values)
    if (any(missing_values)) {
      values[missing_values] <- .validation_palette(
        cluster_order[missing_values],
        .validation_viz_palette_categorical
      )
    }
    values
  } else {
    .validation_palette(cluster_order, palette)
  }
  names(fill_values) <- cluster_order

  if (!is.null(outline_boundaries)) {
    boundary_crs <- sf::st_crs(boundaries)
    outline_crs <- sf::st_crs(outline_boundaries)
    if (!is.na(boundary_crs) && !is.na(outline_crs) && boundary_crs != outline_crs) {
      outline_boundaries <- sf::st_transform(outline_boundaries, boundary_crs)
    }
  }
  boundary_bbox <- sf::st_bbox(boundaries)

  plot_data <- plot_data |>
    dplyr::mutate(
      .lisa_cluster = factor(.data$.lisa_cluster, levels = cluster_order),
      method_label = factor(.data$method_label, levels = unique(.data$method_label)),
      comparison_label = factor(
        .data$comparison_label,
        levels = .flow_comparison_label(comparisons)
      )
    )

  outline_layer <- if (is.null(outline_boundaries)) {
    NULL
  } else {
    ggplot2::geom_sf(
      data = outline_boundaries,
      fill = NA,
      colour = outline_colour,
      linewidth = outline_linewidth,
      inherit.aes = FALSE
    )
  }

  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = boundaries,
      fill = missing_fill,
      colour = boundary_colour,
      linewidth = boundary_linewidth,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_sf(
      data = plot_data,
      ggplot2::aes(fill = .data$.lisa_cluster),
      colour = boundary_colour,
      linewidth = boundary_linewidth,
      inherit.aes = FALSE
    ) +
    outline_layer +
    ggplot2::scale_fill_manual(
      values = fill_values,
      breaks = cluster_order,
      labels = cluster_labels,
      drop = FALSE,
      na.value = missing_fill,
      name = "LISA cluster"
    ) +
    ggplot2::coord_sf(
      xlim = c(boundary_bbox[["xmin"]], boundary_bbox[["xmax"]]),
      ylim = c(boundary_bbox[["ymin"]], boundary_bbox[["ymax"]]),
      datum = NA
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(colour = "#111827"),
      legend.text = ggplot2::element_text(colour = "#374151"),
      strip.text = ggplot2::element_text(colour = "#111827", face = "bold")
    )

  if (
    length(unique(as.character(plot_data$method_label))) > 1L ||
      length(unique(as.character(plot_data$comparison_label))) > 1L
  ) {
    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(.data$method_label, .data$comparison_label)
      )
  }

  plot
}
