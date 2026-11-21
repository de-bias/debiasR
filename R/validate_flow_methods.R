#' Prepare adjusted flow outputs for validation
#'
#' `validate_flow_prepare_output()` joins an adjusted-flow table to the raw MPD
#' flow column used by validation comparisons. `validate_flow_prepare_outputs()`
#' applies the same preparation to a named list of adjusted-flow tables.
#'
#' These helpers are useful when outputs come from different adjustment routes
#' or cached files but need the common `debiasR` validation columns:
#' `origin`, `destination`, `flow`, and `flow_adj`.
#'
#' References must have one row per origin and destination, including
#' `mpd_source` and `mpd_time` in the key when present in both tables.
#'
#' @param adj_df Data frame with adjusted flows.
#' @param adjusted_outputs Named list of adjusted-flow data frames.
#' @param mpd_df Raw MPD flow data with `origin`, `destination`, and the raw
#'   flow column.
#' @param flow_col_mpd Name of the raw MPD flow column in `mpd_df`. Default
#'   `"flow"`.
#' @param flow_col_adj Name of the adjusted-flow column in `adj_df`. Default
#'   `"flow_adj"`.
#'
#' @return `validate_flow_prepare_output()` returns a tibble. If `adj_df`
#'   already contains a `flow` column, that column is retained as
#'   `adjusted_input_flow` and the raw MPD flow from `mpd_df` becomes `flow`.
#'   `validate_flow_prepare_outputs()` returns a named list of prepared tibbles.
#' @export
validate_flow_prepare_output <- function(adj_df,
                                         mpd_df,
                                         flow_col_mpd = "flow",
                                         flow_col_adj = "flow_adj") {
  .validate_required_columns(
    mpd_df,
    c("origin", "destination", flow_col_mpd),
    "mpd_df"
  )
  .validate_required_columns(
    adj_df,
    c("origin", "destination", flow_col_adj),
    "adj_df"
  )

  keys <- .flow_reference_keys(adj_df, mpd_df)
  raw_flow <- tibble::as_tibble(mpd_df) |>
    dplyr::select(
      dplyr::all_of(keys),
      raw_flow = dplyr::all_of(flow_col_mpd)
    )

  adj_tbl <- tibble::as_tibble(adj_df)
  if ("flow" %in% names(adj_tbl)) {
    adj_tbl <- adj_tbl |>
      dplyr::rename(adjusted_input_flow = flow)
  }
  if (!identical(flow_col_adj, "flow_adj")) {
    adj_tbl <- adj_tbl |>
      dplyr::rename(flow_adj = dplyr::all_of(flow_col_adj))
  }

  adj_tbl |>
    dplyr::left_join(raw_flow, by = keys) |>
    dplyr::rename(flow = raw_flow)
}

#' @rdname validate_flow_prepare_output
#' @export
validate_flow_prepare_outputs <- function(adjusted_outputs,
                                          mpd_df,
                                          flow_col_mpd = "flow",
                                          flow_col_adj = "flow_adj") {
  method_names <- .validate_flow_method_names(adjusted_outputs)
  stats::setNames(
    lapply(
      adjusted_outputs,
      validate_flow_prepare_output,
      mpd_df = mpd_df,
      flow_col_mpd = flow_col_mpd,
      flow_col_adj = flow_col_adj
    ),
    method_names
  )
}

#' Validate a named list of adjusted flow outputs
#'
#' These functions apply the corresponding single-output validation function to
#' every element of a named list. They are intended for method-comparison
#' workflows where several adjusted outputs are evaluated against the same
#' benchmark OD table.
#'
#' @param adjusted_outputs Named list of adjusted-flow data frames. Each data
#'   frame is passed to the corresponding single-output validator.
#' @param benchmark_od_df Benchmark OD data frame.
#' @param comparisons Flow comparison(s) to compute. Defaults follow the
#'   vignette comparison workflow.
#' @param drop_zeros Passed to [validate_flow_overall()].
#' @param return_joined Passed to [validate_flow_overall()].
#' @param top_n Passed to [validate_flow_residuals()].
#' @param weight_by,return_origin_level,return_od_level Passed to
#'   [validate_flow_distribution()].
#' @param methods Optional method IDs to include for residual-structure
#'   validation. Default uses all elements in `adjusted_outputs`.
#' @param ... Additional arguments passed to the corresponding single-output
#'   validator. Do not pass `method_name`; it is derived from the list names.
#'
#' @return A named list of validation results.
#' @name validate_flow_methods
NULL

#' @rdname validate_flow_methods
#' @export
validate_flow_overall_methods <- function(adjusted_outputs,
                                          benchmark_od_df,
                                          comparisons = "all",
                                          drop_zeros = FALSE,
                                          return_joined = FALSE,
                                          ...) {
  method_names <- .validate_flow_method_names(adjusted_outputs)
  stats::setNames(
    lapply(method_names, function(method_id) {
      validate_flow_overall(
        adj_df = adjusted_outputs[[method_id]],
        benchmark_od_df = benchmark_od_df,
        comparisons = comparisons,
        drop_zeros = drop_zeros,
        return_joined = return_joined,
        method_name = method_id,
        ...
      )
    }),
    method_names
  )
}

#' @rdname validate_flow_methods
#' @export
validate_flow_residual_methods <- function(adjusted_outputs,
                                           benchmark_od_df,
                                           top_n = 10L,
                                           ...) {
  method_names <- .validate_flow_method_names(adjusted_outputs)
  stats::setNames(
    lapply(method_names, function(method_id) {
      validate_flow_residuals(
        adj_df = adjusted_outputs[[method_id]],
        benchmark_od_df = benchmark_od_df,
        top_n = top_n,
        method_name = method_id,
        ...
      )
    }),
    method_names
  )
}

#' @rdname validate_flow_methods
#' @export
validate_flow_distribution_methods <- function(adjusted_outputs,
                                              benchmark_od_df,
                                              comparisons = "all",
                                              weight_by = "benchmark_origin_total",
                                              return_origin_level = TRUE,
                                              return_od_level = FALSE,
                                              ...) {
  method_names <- .validate_flow_method_names(adjusted_outputs)
  stats::setNames(
    lapply(method_names, function(method_id) {
      validate_flow_distribution(
        adj_df = adjusted_outputs[[method_id]],
        benchmark_od_df = benchmark_od_df,
        comparisons = comparisons,
        method_name = method_id,
        weight_by = weight_by,
        return_origin_level = return_origin_level,
        return_od_level = return_od_level,
        ...
      )
    }),
    method_names
  )
}

#' @rdname validate_flow_methods
#' @export
validate_flow_residual_structure_methods <- function(adjusted_outputs,
                                                    benchmark_od_df,
                                                    methods = names(adjusted_outputs),
                                                    comparison = "adjusted_vs_benchmark",
                                                    ...) {
  method_names <- .validate_flow_method_names(adjusted_outputs)
  if (is.null(methods)) {
    methods <- method_names
  }
  missing_methods <- setdiff(methods, method_names)
  if (length(missing_methods) > 0L) {
    stop(
      "`methods` must name elements in `adjusted_outputs`. Missing: ",
      paste(missing_methods, collapse = ", "),
      call. = FALSE
    )
  }

  stats::setNames(
    lapply(methods, function(method_id) {
      validate_flow_residual_structure(
        adj_df = adjusted_outputs[[method_id]],
        benchmark_od_df = benchmark_od_df,
        method_name = method_id,
        comparison = comparison,
        ...
      )
    }),
    methods
  )
}

#' Prepare origin or destination margins for flow validation
#'
#' Aggregates adjusted outputs and the benchmark table to origin or destination
#' totals. The returned tables keep the same `flow` / `flow_adj` convention used
#' by the main `validate_flow_*()` functions, so the result can be passed to
#' [validate_flow_overall_methods()] or [validate_flow_residual_methods()].
#'
#' @param adjusted_outputs Named list of adjusted-flow data frames.
#' @param benchmark_od_df Benchmark OD data frame.
#' @param role Margin to construct: `"origin"` for total outflows or
#'   `"destination"` for total inflows.
#' @param flow_col_mpd Name of the raw MPD flow column in adjusted outputs.
#'   Default `"flow"`.
#' @param flow_col_adj Name of the adjusted-flow column in adjusted outputs.
#'   Default `"flow_adj"`.
#' @param flow_col_bench Name of the benchmark flow column. Default `"flow"`.
#'
#' @return A list with `adjusted_outputs`, a named list of margin-level adjusted
#'   tables, and `benchmark`, the corresponding margin-level benchmark table.
#' @export
validate_flow_margins <- function(adjusted_outputs,
                                  benchmark_od_df,
                                  role = c("origin", "destination"),
                                  flow_col_mpd = "flow",
                                  flow_col_adj = "flow_adj",
                                  flow_col_bench = "flow") {
  role <- match.arg(role)
  method_names <- .validate_flow_method_names(adjusted_outputs)
  .validate_required_columns(
    benchmark_od_df,
    c("origin", "destination", flow_col_bench),
    "benchmark_od_df"
  )

  adjusted_margin <- function(data) {
    .validate_required_columns(
      data,
      c("origin", "destination", flow_col_mpd, flow_col_adj),
      "adjusted output"
    )
    if (role == "origin") {
      tibble::as_tibble(data) |>
        dplyr::group_by(.data$origin) |>
        dplyr::summarise(
          flow = sum(.data[[flow_col_mpd]], na.rm = TRUE),
          flow_adj = sum(.data[[flow_col_adj]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(destination = "origin_total") |>
        dplyr::select(origin, destination, flow, flow_adj)
    } else {
      tibble::as_tibble(data) |>
        dplyr::group_by(.data$destination) |>
        dplyr::summarise(
          flow = sum(.data[[flow_col_mpd]], na.rm = TRUE),
          flow_adj = sum(.data[[flow_col_adj]], na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(origin = "destination_total") |>
        dplyr::select(origin, destination, flow, flow_adj)
    }
  }

  benchmark_tbl <- tibble::as_tibble(benchmark_od_df)
  benchmark_margin <- if (role == "origin") {
    benchmark_tbl |>
      dplyr::group_by(.data$origin) |>
      dplyr::summarise(flow = sum(.data[[flow_col_bench]], na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(destination = "origin_total") |>
      dplyr::select(origin, destination, flow)
  } else {
    benchmark_tbl |>
      dplyr::group_by(.data$destination) |>
      dplyr::summarise(flow = sum(.data[[flow_col_bench]], na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(origin = "destination_total") |>
      dplyr::select(origin, destination, flow)
  }

  list(
    adjusted_outputs = stats::setNames(
      lapply(adjusted_outputs, adjusted_margin),
      method_names
    ),
    benchmark = benchmark_margin
  )
}

.validate_flow_method_names <- function(adjusted_outputs) {
  if (!is.list(adjusted_outputs) || inherits(adjusted_outputs, "data.frame")) {
    stop("`adjusted_outputs` must be a named list of data frames.", call. = FALSE)
  }
  method_names <- names(adjusted_outputs)
  if (
    is.null(method_names) ||
      length(method_names) != length(adjusted_outputs) ||
      any(!nzchar(method_names))
  ) {
    stop("`adjusted_outputs` must be a named list.", call. = FALSE)
  }
  method_names
}

.validate_required_columns <- function(data, cols, arg) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0L) {
    stop(
      "`", arg, "` must contain: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
