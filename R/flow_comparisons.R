# Shared flow-comparison convention -----------------------------------------

.flow_comparison_ids <- c(
  "adjusted_vs_benchmark",
  "raw_vs_benchmark",
  "raw_vs_adjusted"
)

.flow_comparison_labels <- c(
  adjusted_vs_benchmark = "Adjusted vs benchmark",
  raw_vs_benchmark = "Raw MPD vs benchmark",
  raw_vs_adjusted = "Raw MPD vs adjusted"
)

.flow_comparison_specs <- data.frame(
  comparison = .flow_comparison_ids,
  comparison_label = unname(.flow_comparison_labels[.flow_comparison_ids]),
  x_series = c("adjusted", "raw_mpd", "raw_mpd"),
  y_series = c("benchmark", "benchmark", "adjusted"),
  x_label = c("Adjusted", "Raw MPD", "Raw MPD"),
  y_label = c("Benchmark", "Benchmark", "Adjusted"),
  signed_residual = c(
    "benchmark - adjusted",
    "benchmark - raw",
    "adjusted - raw"
  ),
  stringsAsFactors = FALSE
)

.normalise_flow_comparisons <- function(comparisons) {
  if (length(comparisons) == 1L && identical(comparisons, "all")) {
    return(.flow_comparison_ids)
  }

  invalid <- setdiff(comparisons, .flow_comparison_ids)
  if (length(invalid) > 0L) {
    stop(
      "`comparisons` must contain only 'adjusted_vs_benchmark', ",
      "'raw_vs_benchmark', 'raw_vs_adjusted', or 'all'.",
      call. = FALSE
    )
  }

  unique(comparisons)
}

.flow_comparison_spec <- function(comparisons) {
  comparisons <- .normalise_flow_comparisons(comparisons)
  .flow_comparison_specs[
    match(comparisons, .flow_comparison_specs$comparison),
    ,
    drop = FALSE
  ]
}

.flow_comparison_label <- function(comparisons) {
  if (length(comparisons) == 1L && identical(comparisons, "all")) {
    comparisons <- .flow_comparison_ids
  } else {
    invalid <- setdiff(comparisons, .flow_comparison_ids)
    if (length(invalid) > 0L) {
      stop(
        "`comparisons` must contain only 'adjusted_vs_benchmark', ",
        "'raw_vs_benchmark', 'raw_vs_adjusted', or 'all'.",
        call. = FALSE
      )
    }
  }
  unname(.flow_comparison_labels[comparisons])
}

.flow_comparison_residual <- function(data, comparison) {
  switch(
    comparison,
    adjusted_vs_benchmark = data$benchmark_flow - data$adj_flow,
    raw_vs_benchmark = data$benchmark_flow - data$mpd_flow,
    raw_vs_adjusted = data$adj_flow - data$mpd_flow,
    stop("Unsupported flow comparison.", call. = FALSE)
  )
}

.flow_comparison_x <- function(data, comparison) {
  switch(
    comparison,
    adjusted_vs_benchmark = data$adj_flow,
    raw_vs_benchmark = data$mpd_flow,
    raw_vs_adjusted = data$mpd_flow,
    stop("Unsupported flow comparison.", call. = FALSE)
  )
}

.flow_comparison_y <- function(data, comparison) {
  switch(
    comparison,
    adjusted_vs_benchmark = data$benchmark_flow,
    raw_vs_benchmark = data$benchmark_flow,
    raw_vs_adjusted = data$adj_flow,
    stop("Unsupported flow comparison.", call. = FALSE)
  )
}
