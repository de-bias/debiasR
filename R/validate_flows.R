.flow_overall_metric_row <- function(data, comparison, method_name) {
  spec <- .flow_comparison_spec(comparison)
  empty_row <- tibble::tibble(
    method = method_name,
    comparison = comparison,
    comparison_label = spec$comparison_label,
    x_series = spec$x_series,
    y_series = spec$y_series,
    signed_residual = spec$signed_residual,
    n = 0L,
    sum_x = 0,
    sum_y = 0,
    sum_adj = 0,
    sum_bench = 0,
    sum_mpd = 0,
    mean_error = NA_real_,
    pearson_r = NA_real_,
    spearman_rho = NA_real_,
    rmse = NA_real_,
    mae = NA_real_,
    mape = NA_real_,
    ols_intercept = NA_real_,
    ols_slope = NA_real_,
    r_squared = NA_real_
  )

  if (nrow(data) == 0L) {
    return(empty_row)
  }

  x <- data$x_flow
  y <- data$y_flow
  residual <- y - x
  denom <- ifelse(y == 0, NA_real_, y)
  x_sd <- stats::sd(x, na.rm = TRUE)
  y_sd <- stats::sd(y, na.rm = TRUE)
  can_cor <- length(x) >= 2L &&
    is.finite(x_sd) &&
    x_sd > 0 &&
    is.finite(y_sd) &&
    y_sd > 0
  can_lm <- can_cor && all(is.finite(x + y))

  if (can_lm) {
    fit <- stats::lm(y ~ x)
    coefs <- stats::coef(fit)
    ols_intercept <- unname(coefs[1])
    ols_slope <- unname(coefs[2])
    r_squared <- unname(summary(fit)$r.squared)
  } else {
    ols_intercept <- NA_real_
    ols_slope <- NA_real_
    r_squared <- NA_real_
  }

  tibble::tibble(
    method = method_name,
    comparison = comparison,
    comparison_label = spec$comparison_label,
    x_series = spec$x_series,
    y_series = spec$y_series,
    signed_residual = spec$signed_residual,
    n = length(x),
    sum_x = sum(x, na.rm = TRUE),
    sum_y = sum(y, na.rm = TRUE),
    sum_adj = sum(data$flow_adj, na.rm = TRUE),
    sum_bench = sum(data$flow_bench, na.rm = TRUE),
    sum_mpd = sum(data$mpd_flow, na.rm = TRUE),
    mean_error = mean(residual, na.rm = TRUE),
    pearson_r = if (can_cor) {
      suppressWarnings(stats::cor(x, y, method = "pearson"))
    } else {
      NA_real_
    },
    spearman_rho = if (can_cor) {
      suppressWarnings(stats::cor(x, y, method = "spearman"))
    } else {
      NA_real_
    },
    rmse = sqrt(mean(residual^2)),
    mae = mean(abs(residual)),
    mape = mean(abs(residual / denom), na.rm = TRUE),
    ols_intercept = ols_intercept,
    ols_slope = ols_slope,
    r_squared = r_squared
  )
}

#' Validate adjusted OD flows against a benchmark overall
#'
#' Compares bias-adjusted MPD flows to benchmark (e.g., census) OD flows.
#' Uses adjusted flows as estimates (x) and benchmark as targets (y), returning
#' summary fit metrics that capture both concordance (for example, correlation)
#' and aggregate error (for example, RMSE, MAE, MAPE). For OD-level residual
#' auditing, use \code{validate_flow_residuals()} or the lower-level
#' \code{validate_flow_pairs()} table.
#'
#' @param adj_df Data frame with at least: origin, destination, and a column of adjusted flows
#'   (default name "flow_adj"). If present, an mpd_source column is carried through.
#' @param benchmark_od_df Data frame with at least: origin, destination, and a column of benchmark flows
#'   (default name "flow").
#' @param flow_col_adj Name of adjusted flow column in adj_df. Default "flow_adj".
#' @param flow_col_bench Name of benchmark flow column in benchmark_od_df. Default "flow".
#' @param drop_zeros Logical, drop rows where either x or y == 0 before metrics. Default TRUE.
#' @param na_rm Logical, remove non-finite rows before metrics. Default TRUE.
#' @param by_source Logical, if TRUE and mpd_source exists in both inputs (or in adj_df),
#'   compute metrics per mpd_source as well as overall. Default FALSE.
#' @param return_joined Logical, return the joined row-level data in the result list. Default TRUE.
#' @param method_name Optional label for the adjustment method (e.g. "adjust_inverse_penetration",
#'   "adjust_selection_rate"). Stored in the output for comparison workflows.
#' @param comparisons Flow comparison(s) to compute. The package convention is
#'   that the first series in the ID is the x-axis/baseline, the second series
#'   is the y-axis/reference, and signed residuals are \code{Y - X}. Default
#'   \code{"adjusted_vs_benchmark"}. Use \code{"all"} for all supported
#'   comparisons.
#' @param flow_col_mpd Name of raw MPD flow column in \code{adj_df}. Default
#'   \code{"flow"}. Required only when \code{comparisons} includes raw MPD
#'   flows.
#'
#' @return A list with:
#'   \itemize{
#'     \item method (if provided)
#'     \item comparison metadata, n, sum_x, sum_y, sum_adj, sum_bench
#'     \item pearson_r, spearman_rho
#'     \item rmse, mae, mape
#'     \item ols_intercept, ols_slope, r_squared (from lm(y ~ x))
#'     \item (optional) summary: a tibble of metric rows when more than one
#'       comparison is requested
#'     \item (optional) by_source: a tibble of per-source metrics when by_source = TRUE
#'     \item (optional) data: the joined tibble used for the calculations
#'   }
#' @export
validate_flow_overall <- function(adj_df,
                                  benchmark_od_df,
                                  flow_col_adj   = "flow_adj",
                                  flow_col_bench = "flow",
                                  drop_zeros     = TRUE,
                                  na_rm          = TRUE,
                                  by_source      = FALSE,
                                  return_joined  = TRUE,
                                  method_name    = NA_character_,
                                  comparisons    = "adjusted_vs_benchmark",
                                  flow_col_mpd   = "flow") {

  comparisons <- .normalise_flow_comparisons(comparisons)
  raw_requested <- any(comparisons %in% c("raw_vs_benchmark", "raw_vs_adjusted"))

  # --- Required columns
  req_adj   <- c("origin", "destination", flow_col_adj)
  if (raw_requested) {
    req_adj <- unique(c(req_adj, flow_col_mpd))
  }
  req_bench <- c("origin", "destination", flow_col_bench)
  if (!all(req_adj %in% names(adj_df))) {
    stop("`adj_df` must contain: ", paste(req_adj, collapse = ", "))
  }
  if (!all(req_bench %in% names(benchmark_od_df))) {
    stop("`benchmark_od_df` must contain: ", paste(req_bench, collapse = ", "))
  }

  # --- Prepare joined data
  adj_select_cols <- unique(c("mpd_source", "origin", "destination", flow_col_adj))
  if (flow_col_mpd %in% names(adj_df)) {
    adj_select_cols <- unique(c(adj_select_cols, flow_col_mpd))
  }
  joined <- tibble::as_tibble(adj_df) |>
    dplyr::select(dplyr::any_of(adj_select_cols)) |>
    dplyr::rename(flow_adj = dplyr::all_of(flow_col_adj))
  if (flow_col_mpd %in% names(joined)) {
    joined <- joined |>
      dplyr::rename(mpd_flow = dplyr::all_of(flow_col_mpd))
  } else {
    joined$mpd_flow <- NA_real_
  }
  joined <- joined |>
    dplyr::inner_join(
      benchmark_od_df |>
        dplyr::select(origin, destination, !!flow_col_bench) |>
        dplyr::rename(flow_bench = !!flow_col_bench),
      by = c("origin", "destination")
    ) |>
    dplyr::mutate(
      adj_flow = .data$flow_adj,
      benchmark_flow = .data$flow_bench
    )

  comparison_data <- dplyr::bind_rows(lapply(comparisons, function(comparison) {
    spec <- .flow_comparison_spec(comparison)
    comparison_tbl <- joined
    comparison_tbl$x_flow <- .flow_comparison_x(comparison_tbl, comparison)
    comparison_tbl$y_flow <- .flow_comparison_y(comparison_tbl, comparison)
    comparison_tbl$residual <- .flow_comparison_residual(comparison_tbl, comparison)
    comparison_tbl |>
      dplyr::mutate(
        comparison = comparison,
        comparison_label = spec$comparison_label,
        x_series = spec$x_series,
        y_series = spec$y_series,
        signed_residual = spec$signed_residual
      )
  }))

  # --- Clean rows
  if (na_rm) {
    comparison_data <- dplyr::filter(
      comparison_data,
      is.finite(.data$x_flow),
      is.finite(.data$y_flow)
    )
  }
  if (drop_zeros) {
    comparison_data <- dplyr::filter(
      comparison_data,
      .data$x_flow > 0,
      .data$y_flow > 0
    )
  }

  metric_rows <- dplyr::bind_rows(lapply(comparisons, function(comparison) {
    .flow_overall_metric_row(
      comparison_data[comparison_data$comparison == comparison, , drop = FALSE],
      comparison = comparison,
      method_name = method_name
    )
  }))

  # --- Optional: per-source metrics
  by_src <- NULL
  if (by_source && "mpd_source" %in% names(comparison_data)) {
    by_src <- dplyr::bind_rows(lapply(
      split(
        comparison_data,
        list(comparison_data$comparison, comparison_data$mpd_source),
        drop = TRUE
      ),
      function(data) {
        row <- .flow_overall_metric_row(
          data,
          comparison = data$comparison[1],
          method_name = method_name
        )
        row$mpd_source <- data$mpd_source[1]
        row
      }
    ))
  }

  if (length(comparisons) == 1L) {
    out <- as.list(metric_rows[1, , drop = FALSE])
  } else {
    out <- list(
      method = method_name,
      summary = metric_rows
    )
  }

  if (!is.null(by_src)) {
    out$by_source <- by_src
  }
  if (return_joined) {
    out$data <- comparison_data
  }
  out
}

#' Legacy alias for \code{validate_flow_overall()}
#'
#' Retained for backwards compatibility. New code should prefer
#' \code{validate_flow_overall()}.
#'
#' @param ... Arguments passed to \code{validate_flow_overall()}.
#' @rdname validate_flow_overall
#' @export
validate_flow_benchmark <- function(...) {
  validate_flow_overall(...)
}

.build_flow_audit_table <- function(adj_df,
                                    benchmark_od_df,
                                    flow_col_mpd   = "flow",
                                    flow_col_adj   = "flow_adj",
                                    flow_col_bench = "flow") {

  req_adj <- c("origin", "destination", flow_col_mpd, flow_col_adj)
  req_bench <- c("origin", "destination", flow_col_bench)

  if (!all(req_adj %in% names(adj_df))) {
    stop("`adj_df` must contain: ", paste(req_adj, collapse = ", "))
  }
  if (!all(req_bench %in% names(benchmark_od_df))) {
    stop("`benchmark_od_df` must contain: ", paste(req_bench, collapse = ", "))
  }

  out <- adj_df |>
    dplyr::select(dplyr::any_of("mpd_source"),
                  origin,
                  destination,
                  !!flow_col_mpd,
                  !!flow_col_adj) |>
    dplyr::rename(
      mpd_flow = !!flow_col_mpd,
      adj_flow = !!flow_col_adj
    ) |>
    dplyr::inner_join(
      benchmark_od_df |>
        dplyr::select(origin, destination, !!flow_col_bench) |>
        dplyr::rename(benchmark_flow = !!flow_col_bench),
      by = c("origin", "destination")
    ) |>
    dplyr::mutate(
      diff_mpd_benchmark = .data$mpd_flow - .data$benchmark_flow,
      diff_mpd_adj = .data$mpd_flow - .data$adj_flow,
      diff_adj_benchmark = .data$adj_flow - .data$benchmark_flow,
      signed_residual_adjusted_vs_benchmark = .data$benchmark_flow - .data$adj_flow,
      signed_residual_raw_vs_benchmark = .data$benchmark_flow - .data$mpd_flow,
      signed_residual_raw_vs_adjusted = .data$adj_flow - .data$mpd_flow
    )

  tibble::as_tibble(out)
}

#' Build an OD-pair validation table for MPD, adjusted, and benchmark flows
#'
#' Joins adjusted-flow output to benchmark OD flows and returns a tidy table with
#' original MPD flow, adjusted flow, benchmark flow, legacy first-minus-second
#' difference columns, and canonical signed residual columns. This complements
#' \code{validate_flow_overall()}, which summarizes method-level fit, by
#' exposing OD-level differences directly.
#' For richer residual diagnostics (absolute residuals, percentage residuals,
#' improvement flags, and a top-worst table), use
#' \code{validate_flow_residuals()}.
#'
#' @param adj_df Data frame with at least \code{origin}, \code{destination},
#'   an MPD flow column (default \code{"flow"}), and an adjusted flow column
#'   (default \code{"flow_adj"}). If present, \code{mpd_source} is carried through.
#' @param benchmark_od_df Data frame with at least \code{origin},
#'   \code{destination}, and a benchmark flow column (default \code{"flow"}).
#' @param flow_col_mpd Name of MPD flow column in \code{adj_df}. Default \code{"flow"}.
#' @param flow_col_adj Name of adjusted flow column in \code{adj_df}. Default \code{"flow_adj"}.
#' @param flow_col_bench Name of benchmark flow column in \code{benchmark_od_df}.
#'   Default \code{"flow"}.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{origin, destination} (and \code{mpd_source} if present),
#'   \item \code{mpd_flow},
#'   \item \code{benchmark_flow},
#'   \item \code{adj_flow},
#'   \item \code{diff_mpd_benchmark = mpd_flow - benchmark_flow},
#'   \item \code{diff_mpd_adj = mpd_flow - adj_flow},
#'   \item \code{diff_adj_benchmark = adj_flow - benchmark_flow},
#'   \item \code{signed_residual_adjusted_vs_benchmark = benchmark_flow - adj_flow},
#'   \item \code{signed_residual_raw_vs_benchmark = benchmark_flow - mpd_flow},
#'   \item \code{signed_residual_raw_vs_adjusted = adj_flow - mpd_flow}.
#' }
#' @export
validate_flow_pairs <- function(adj_df,
                                benchmark_od_df,
                                flow_col_mpd   = "flow",
                                flow_col_adj   = "flow_adj",
                                flow_col_bench = "flow") {

  .build_flow_audit_table(
    adj_df = adj_df,
    benchmark_od_df = benchmark_od_df,
    flow_col_mpd = flow_col_mpd,
    flow_col_adj = flow_col_adj,
    flow_col_bench = flow_col_bench
  )
}

#' Legacy alias for \code{validate_flow_pairs()}
#'
#' Retained for backwards compatibility. New code should prefer
#' \code{validate_flow_pairs()}.
#'
#' @param ... Arguments passed to \code{validate_flow_pairs()}.
#' @rdname validate_flow_pairs
#' @export
validate_flow_all <- function(...) {
  validate_flow_pairs(...)
}

.weighted_mean_na <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[keep], w[keep])
}

.mean_logical_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

.safe_pearson <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 2L) {
    return(NA_real_)
  }
  if (stats::sd(x[keep]) <= 0 || stats::sd(y[keep]) <= 0) {
    return(NA_real_)
  }
  suppressWarnings(stats::cor(x[keep], y[keep], method = "pearson"))
}

.sd_score <- function(x, residual_sd) {
  residual_sd <- residual_sd[1]
  if (is.na(residual_sd) || residual_sd <= 0) {
    return(rep(NA_real_, length(x)))
  }
  x / residual_sd
}

.residual_over_sd <- function(abs_score, threshold) {
  dplyr::if_else(
    is.na(abs_score),
    NA,
    abs_score > threshold
  )
}

.aggregate_residual <- function(x, aggregation) {
  if (aggregation == "sum") {
    return(sum(x, na.rm = TRUE))
  }
  mean(x, na.rm = TRUE)
}

.prepare_neighbor_links <- function(area_neighbors,
                                    area_col,
                                    neighbor_col,
                                    weight_col) {
  req_neighbors <- c(area_col, neighbor_col)
  if (!all(req_neighbors %in% names(area_neighbors))) {
    stop("`area_neighbors` must contain: ", paste(req_neighbors, collapse = ", "))
  }
  if (!is.null(weight_col) && !weight_col %in% names(area_neighbors)) {
    stop("`weight_col` must name a column in `area_neighbors`.")
  }

  neighbor_tbl <- area_neighbors |>
    dplyr::select(
      area = dplyr::all_of(area_col),
      neighbor = dplyr::all_of(neighbor_col),
      dplyr::any_of(weight_col)
    )

  if (is.null(weight_col)) {
    neighbor_tbl <- dplyr::mutate(neighbor_tbl, weight = 1)
  } else {
    neighbor_tbl <- dplyr::rename(neighbor_tbl, weight = dplyr::all_of(weight_col))
  }

  neighbor_tbl |>
    dplyr::filter(is.finite(.data$weight), .data$weight > 0)
}

.compute_moran_i <- function(area_level,
                             area_neighbors,
                             area_col,
                             neighbor_col,
                             weight_col) {

  neighbor_tbl <- .prepare_neighbor_links(
    area_neighbors = area_neighbors,
    area_col = area_col,
    neighbor_col = neighbor_col,
    weight_col = weight_col
  )

  values <- area_level |>
    dplyr::select(dplyr::all_of(c("area", "selected_residual"))) |>
    dplyr::filter(is.finite(.data$selected_residual))

  n_areas_used <- nrow(values)
  if (n_areas_used < 2L) {
    return(list(
      moran_i = NA_real_,
      n_areas_used = n_areas_used,
      n_links_used = 0L,
      weight_sum = NA_real_
    ))
  }

  x_bar <- mean(values$selected_residual)
  denominator <- sum((values$selected_residual - x_bar)^2)

  links <- neighbor_tbl |>
    dplyr::inner_join(values, by = "area") |>
    dplyr::rename(area_residual = dplyr::all_of("selected_residual")) |>
    dplyr::inner_join(
      dplyr::rename(
        values,
        neighbor = dplyr::all_of("area"),
        neighbor_residual = dplyr::all_of("selected_residual")
      ),
      by = "neighbor"
    ) |>
    dplyr::filter(is.finite(.data$weight), .data$weight > 0)

  n_links_used <- nrow(links)
  weight_sum <- sum(links$weight, na.rm = TRUE)

  if (n_links_used == 0L || weight_sum <= 0 || denominator <= 0) {
    return(list(
      moran_i = NA_real_,
      n_areas_used = n_areas_used,
      n_links_used = n_links_used,
      weight_sum = weight_sum
    ))
  }

  numerator <- sum(
    links$weight *
      (links$area_residual - x_bar) *
      (links$neighbor_residual - x_bar),
    na.rm = TRUE
  )

  list(
    moran_i = (n_areas_used / weight_sum) * (numerator / denominator),
    n_areas_used = n_areas_used,
    n_links_used = n_links_used,
    weight_sum = weight_sum
  )
}

.validate_local_moran_args <- function(local_moran_nsim,
                                       local_moran_alpha,
                                       local_moran_p_adjust,
                                       local_moran_seed) {
  if (!is.numeric(local_moran_nsim) ||
      length(local_moran_nsim) != 1L ||
      is.na(local_moran_nsim) ||
      local_moran_nsim < 1L) {
    stop("`local_moran_nsim` must be a positive whole number.")
  }
  if (local_moran_nsim != as.integer(local_moran_nsim)) {
    stop("`local_moran_nsim` must be a positive whole number.")
  }
  if (!is.numeric(local_moran_alpha) ||
      length(local_moran_alpha) != 1L ||
      !is.finite(local_moran_alpha) ||
      local_moran_alpha < 0 ||
      local_moran_alpha > 1) {
    stop("`local_moran_alpha` must be a number between 0 and 1.")
  }
  if (!is.character(local_moran_p_adjust) ||
      length(local_moran_p_adjust) != 1L ||
      !local_moran_p_adjust %in% stats::p.adjust.methods) {
    stop("`local_moran_p_adjust` must be one of `stats::p.adjust.methods`.")
  }
  if (!is.null(local_moran_seed) &&
      (!is.numeric(local_moran_seed) ||
       length(local_moran_seed) != 1L ||
       !is.finite(local_moran_seed))) {
    stop("`local_moran_seed` must be `NULL` or a single finite number.")
  }
}

.with_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(force(code))
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  force(code)
}

.local_moran_quadrant <- function(centered_residual,
                                  neighbor_lag_mean,
                                  has_neighbors,
                                  defined) {
  dplyr::case_when(
    !has_neighbors ~ "no neighbours",
    !defined ~ "undefined",
    centered_residual > 0 & neighbor_lag_mean > 0 ~ "high-high",
    centered_residual < 0 & neighbor_lag_mean < 0 ~ "low-low",
    centered_residual > 0 & neighbor_lag_mean < 0 ~ "high-low",
    centered_residual < 0 & neighbor_lag_mean > 0 ~ "low-high",
    TRUE ~ "undefined"
  )
}

.local_moran_cluster <- function(lisa_quadrant,
                                 significant,
                                 has_neighbors,
                                 defined) {
  dplyr::case_when(
    !has_neighbors ~ "no neighbours",
    !defined ~ "undefined",
    is.na(significant) | !significant ~ "not significant",
    TRUE ~ lisa_quadrant
  )
}

.compute_local_moran <- function(area_level,
                                 area_neighbors,
                                 area_col,
                                 neighbor_col,
                                 weight_col,
                                 nsim,
                                 alpha,
                                 p_adjust,
                                 seed) {

  local_base <- area_level |>
    dplyr::select(
      area,
      method,
      residual_type,
      dplyr::any_of(c("comparison", "comparison_label")),
      spatial_role,
      residual_aggregation,
      selected_residual
    )

  if (is.null(area_neighbors)) {
    return(local_base |>
      dplyr::mutate(
        centered_residual = NA_real_,
        residual_z = NA_real_,
        neighbor_lag_sum = NA_real_,
        neighbor_lag_mean = NA_real_,
        spatial_lag_z = NA_real_,
        n_neighbors_used = 0L,
        local_weight_sum = NA_real_,
        local_moran_i = NA_real_,
        lisa_quadrant = "no neighbours",
        p_value = NA_real_,
        p_adjusted = NA_real_,
        significant = NA,
        lisa_cluster = "no neighbours"
      ))
  }

  neighbor_tbl <- .prepare_neighbor_links(
    area_neighbors = area_neighbors,
    area_col = area_col,
    neighbor_col = neighbor_col,
    weight_col = weight_col
  )

  values <- local_base |>
    dplyr::filter(is.finite(.data$selected_residual))

  n_areas_used <- nrow(values)
  residual_sd <- stats::sd(values$selected_residual)
  residual_mean <- mean(values$selected_residual)
  centered_denominator <- sum((values$selected_residual - residual_mean)^2)
  if (n_areas_used < 2L ||
      !is.finite(residual_sd) ||
      residual_sd <= 0 ||
      !is.finite(centered_denominator) ||
      centered_denominator <= 0) {
    return(local_base |>
      dplyr::mutate(
        centered_residual = NA_real_,
        residual_z = NA_real_,
        neighbor_lag_sum = NA_real_,
        neighbor_lag_mean = NA_real_,
        spatial_lag_z = NA_real_,
        n_neighbors_used = 0L,
        local_weight_sum = NA_real_,
        local_moran_i = NA_real_,
        lisa_quadrant = "undefined",
        p_value = NA_real_,
        p_adjusted = NA_real_,
        significant = NA,
        lisa_cluster = "undefined"
      ))
  }

  values <- values |>
    dplyr::mutate(
      centered_residual = .data$selected_residual - residual_mean,
      residual_z = .data$centered_residual / sqrt(centered_denominator / n_areas_used)
    )

  links <- neighbor_tbl |>
    dplyr::inner_join(
      values |> dplyr::select(area, centered_residual, residual_z),
      by = "area"
    ) |>
    dplyr::rename(
      area_centered_residual = dplyr::all_of("centered_residual"),
      area_residual_z = dplyr::all_of("residual_z")
    ) |>
    dplyr::inner_join(
      values |>
        dplyr::select(area, centered_residual, residual_z) |>
        dplyr::rename(
          neighbor = dplyr::all_of("area"),
          neighbor_centered_residual = dplyr::all_of("centered_residual"),
          neighbor_residual_z = dplyr::all_of("residual_z")
        ),
      by = "neighbor"
    )

  neighbor_summary <- links |>
    dplyr::group_by(.data$area) |>
    dplyr::summarise(
      n_neighbors_used = dplyr::n(),
      local_weight_sum = sum(.data$weight, na.rm = TRUE),
      neighbor_lag_sum = sum(.data$weight * .data$neighbor_centered_residual, na.rm = TRUE),
      neighbor_lag_mean = sum(.data$weight * .data$neighbor_centered_residual, na.rm = TRUE) /
        sum(.data$weight, na.rm = TRUE),
      spatial_lag_z = sum(.data$weight * .data$neighbor_residual_z, na.rm = TRUE) /
        sum(.data$weight, na.rm = TRUE),
      .groups = "drop"
    )

  local_tbl <- local_base |>
    dplyr::left_join(
      values |> dplyr::select(area, centered_residual, residual_z),
      by = "area"
    ) |>
    dplyr::left_join(neighbor_summary, by = "area") |>
    dplyr::mutate(
      n_neighbors_used = dplyr::if_else(
        is.na(.data$n_neighbors_used),
        0L,
        .data$n_neighbors_used
      ),
      local_moran_i = (.data$centered_residual * .data$neighbor_lag_sum) /
        (centered_denominator / n_areas_used)
    )

  has_neighbors <- local_tbl$n_neighbors_used > 0L &
    is.finite(local_tbl$local_moran_i) &
    is.finite(local_tbl$centered_residual) &
    is.finite(local_tbl$neighbor_lag_mean)
  observed_i <- local_tbl$local_moran_i

  p_value <- rep(NA_real_, nrow(local_tbl))
  if (any(has_neighbors)) {
    link_index <- match(links$area, local_tbl$area)
    neighbor_index <- match(links$neighbor, local_tbl$area)
    weight <- links$weight
    finite_residual_index <- which(is.finite(local_tbl$centered_residual))
    centered_residual <- local_tbl$centered_residual

    simulated_i <- .with_seed(seed, {
      simulation_matrix <- matrix(NA_real_, nrow = nrow(local_tbl), ncol = nsim)
      for (focal_idx in which(has_neighbors)) {
        other_idx <- setdiff(finite_residual_index, focal_idx)
        for (sim_idx in seq_len(nsim)) {
          permuted_centered <- centered_residual
          permuted_centered[other_idx] <- sample(centered_residual[other_idx])
          focal_links <- link_index == focal_idx
          neighbor_lag_sum <- sum(
            weight[focal_links] * permuted_centered[neighbor_index[focal_links]],
            na.rm = TRUE
          )
          simulation_matrix[focal_idx, sim_idx] <-
            centered_residual[focal_idx] * neighbor_lag_sum /
            (centered_denominator / n_areas_used)
        }
      }
      simulation_matrix
    })

    for (idx in which(has_neighbors)) {
      p_value[idx] <- (sum(abs(simulated_i[idx, ]) >= abs(observed_i[idx]), na.rm = TRUE) + 1) /
        (nsim + 1)
    }
  }

  p_adjusted <- rep(NA_real_, length(p_value))
  finite_p <- is.finite(p_value)
  if (any(finite_p)) {
    p_adjusted[finite_p] <- stats::p.adjust(p_value[finite_p], method = p_adjust)
  }
  significant <- finite_p & p_adjusted <= alpha

  local_tbl |>
    dplyr::mutate(
      lisa_quadrant = .local_moran_quadrant(
        centered_residual = .data$centered_residual,
        neighbor_lag_mean = .data$neighbor_lag_mean,
        has_neighbors = .data$n_neighbors_used > 0L,
        defined = is.finite(.data$local_moran_i)
      ),
      p_value = p_value,
      p_adjusted = p_adjusted,
      significant = dplyr::if_else(finite_p, significant, NA),
      lisa_cluster = .local_moran_cluster(
        lisa_quadrant = .data$lisa_quadrant,
        significant = .data$significant,
        has_neighbors = .data$n_neighbors_used > 0L,
        defined = is.finite(.data$local_moran_i)
      )
    )
}

#' Validate origin-conditioned destination-share distributions
#'
#' Compares OD-flow distributions after normalizing each origin's destination
#' flows into shares. This is a distributional allocation validation diagnostic:
#' it checks whether each origin allocates its flows across destinations in a
#' similar way to a reference OD table, rather than checking individual OD-cell
#' magnitudes. By default, the function compares adjusted OD flows with
#' benchmark OD flows and reports \code{KL(benchmark || adjusted)} plus
#' Jensen-Shannon divergence. It can also compare raw MPD versus benchmark and
#' raw MPD versus adjusted flows through \code{comparisons}. For each
#' comparison ID, the first series is the x/baseline distribution, the second
#' series is the y/reference distribution, and directional KL is
#' \code{KL(Y || X)}. Lower values
#' indicate closer destination-allocation fidelity. These metrics assess
#' spatial allocation, not total flow scale or individual OD-pair residual size.
#'
#' @param adj_df Data frame with at least \code{origin}, \code{destination},
#'   and an adjusted flow column (default \code{"flow_adj"}).
#' @param benchmark_od_df Data frame with at least \code{origin},
#'   \code{destination}, and a benchmark flow column (default \code{"flow"}).
#' @param flow_col_adj Name of adjusted flow column in \code{adj_df}. Default
#'   \code{"flow_adj"}.
#' @param flow_col_mpd Name of raw MPD flow column in \code{adj_df}. Default
#'   \code{"flow"}. Required only when \code{comparisons} includes raw MPD
#'   flows.
#' @param flow_col_bench Name of benchmark flow column in
#'   \code{benchmark_od_df}. Default \code{"flow"}.
#' @param epsilon Small positive smoothing constant added to benchmark and
#'   adjusted flows before shares are computed. Default \code{1e-8}.
#' @param method_name Optional label for the adjustment method. Stored in the
#'   summary and origin-level outputs.
#' @param weight_by Weighting rule for weighted summary means. Currently
#'   \code{"none"} or \code{"benchmark_origin_total"}.
#' @param comparisons Distribution comparisons to compute. The default
#'   \code{"adjusted_vs_benchmark"} preserves the original behavior. Use
#'   \code{"all"} to compute \code{"adjusted_vs_benchmark"},
#'   \code{"raw_vs_benchmark"}, and \code{"raw_vs_adjusted"}.
#' @param return_origin_level Logical, return one row per origin in the output.
#'   Default \code{TRUE}.
#' @param return_od_level Logical, return OD-level share and contribution rows.
#'   Default \code{FALSE}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{summary}: one row per requested comparison with origin count,
#'     mean, median, and optionally benchmark-total-weighted mean KL and JSD,
#'   \item \code{origin_level}: origin-level tibble with KL, JSD, destination
#'     count, raw, adjusted, benchmark, reference, and comparison origin totals,
#'     plus zero-total flags when \code{return_origin_level = TRUE},
#'   \item \code{od_level}: OD-level share and contribution rows when
#'     \code{return_od_level = TRUE}.
#' }
#' @export
validate_flow_distribution <- function(adj_df,
                                       benchmark_od_df,
                                       flow_col_adj = "flow_adj",
                                       flow_col_mpd = "flow",
                                       flow_col_bench = "flow",
                                       epsilon = 1e-8,
                                       method_name = NA_character_,
                                       weight_by = c("none", "benchmark_origin_total"),
                                       comparisons = c("adjusted_vs_benchmark"),
                                       return_origin_level = TRUE,
                                       return_od_level = FALSE) {

  weight_by <- match.arg(weight_by)
  comparisons <- .normalise_distribution_comparisons(comparisons)
  .validate_distribution_epsilon(epsilon)

  raw_requested <- any(comparisons %in% c("raw_vs_benchmark", "raw_vs_adjusted"))
  req_adj <- c("origin", "destination", flow_col_adj)
  if (raw_requested) {
    req_adj <- unique(c(req_adj, flow_col_mpd))
  }
  req_bench <- c("origin", "destination", flow_col_bench)

  if (!all(req_adj %in% names(adj_df))) {
    stop("`adj_df` must contain: ", paste(req_adj, collapse = ", "))
  }
  if (!all(req_bench %in% names(benchmark_od_df))) {
    stop("`benchmark_od_df` must contain: ", paste(req_bench, collapse = ", "))
  }

  adj_select_cols <- unique(c("origin", "destination", flow_col_adj, flow_col_mpd))
  adj_select_cols <- adj_select_cols[adj_select_cols %in% names(adj_df)]

  adj_tbl <- adj_df |>
    dplyr::select(dplyr::all_of(adj_select_cols)) |>
    dplyr::rename(adjusted_flow = dplyr::all_of(flow_col_adj))
  if (flow_col_mpd %in% names(adj_tbl)) {
    adj_tbl <- adj_tbl |>
      dplyr::rename(raw_flow = dplyr::all_of(flow_col_mpd))
  } else {
    adj_tbl$raw_flow <- NA_real_
  }

  bench_tbl <- benchmark_od_df |>
    dplyr::select(origin, destination, !!flow_col_bench) |>
    dplyr::rename(benchmark_flow = !!flow_col_bench)

  support_tbl <- dplyr::full_join(
    bench_tbl,
    adj_tbl,
    by = c("origin", "destination")
  ) |>
    dplyr::mutate(
      benchmark_flow = dplyr::coalesce(.data$benchmark_flow, 0),
      adjusted_flow = dplyr::coalesce(.data$adjusted_flow, 0),
      raw_flow = dplyr::coalesce(.data$raw_flow, 0)
    )

  comparison_outputs <- lapply(
    comparisons,
    .compute_flow_distribution_comparison,
    support_tbl = support_tbl,
    epsilon = epsilon,
    method_name = method_name
  )

  origin_level <- dplyr::bind_rows(
    lapply(comparison_outputs, `[[`, "origin_level")
  )

  summary_tbl <- origin_level |>
    dplyr::group_by(
      .data$method,
      .data$comparison,
      .data$comparison_label,
      .data$x_distribution,
      .data$y_distribution,
      .data$reference_distribution,
      .data$comparison_distribution
    ) |>
    dplyr::summarise(
      n_origins = dplyr::n(),
      n_origins_used = sum(!is.na(.data$kl_origin)),
      weight_by = weight_by,
      kl_mean = mean(.data$kl_origin, na.rm = TRUE),
      kl_median = stats::median(.data$kl_origin, na.rm = TRUE),
      kl_weighted_mean = dplyr::if_else(
        weight_by == "benchmark_origin_total",
        .weighted_mean_na(.data$kl_origin, .data$bench_origin_total),
        NA_real_
      ),
      jsd_mean = mean(.data$jsd_origin, na.rm = TRUE),
      jsd_median = stats::median(.data$jsd_origin, na.rm = TRUE),
      jsd_weighted_mean = dplyr::if_else(
        weight_by == "benchmark_origin_total",
        .weighted_mean_na(.data$jsd_origin, .data$bench_origin_total),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(match(.data$comparison, comparisons))

  out <- list(summary = summary_tbl)
  if (return_origin_level) {
    out$origin_level <- origin_level
  }
  if (isTRUE(return_od_level)) {
    out$od_level <- dplyr::bind_rows(
      lapply(comparison_outputs, `[[`, "od_level")
    )
  }
  out
}

.compute_flow_distribution_comparison <- function(comparison,
                                                  support_tbl,
                                                  epsilon,
                                                  method_name) {
  if (comparison == "raw_vs_benchmark") {
    reference_col <- "benchmark_flow"
    comparison_col <- "raw_flow"
    reference_distribution <- "benchmark"
    comparison_distribution <- "raw_mpd"
  } else if (comparison == "adjusted_vs_benchmark") {
    reference_col <- "benchmark_flow"
    comparison_col <- "adjusted_flow"
    reference_distribution <- "benchmark"
    comparison_distribution <- "adjusted_mpd"
  } else if (comparison == "raw_vs_adjusted") {
    reference_col <- "adjusted_flow"
    comparison_col <- "raw_flow"
    reference_distribution <- "adjusted_mpd"
    comparison_distribution <- "raw_mpd"
  } else {
    stop("Unsupported distribution comparison.")
  }
  spec <- .flow_comparison_spec(comparison)

  od_level <- support_tbl |>
    dplyr::mutate(
      reference_flow = .data[[reference_col]],
      comparison_flow = .data[[comparison_col]]
    ) |>
    dplyr::group_by(origin) |>
    dplyr::mutate(
      bench_origin_total = sum(.data$benchmark_flow, na.rm = TRUE),
      raw_origin_total = sum(.data$raw_flow, na.rm = TRUE),
      adjusted_origin_total = sum(.data$adjusted_flow, na.rm = TRUE),
      adj_origin_total = .data$adjusted_origin_total,
      reference_origin_total = sum(.data$reference_flow, na.rm = TRUE),
      comparison_origin_total = sum(.data$comparison_flow, na.rm = TRUE),
      reference_share = .distribution_shares(.data$reference_flow, epsilon),
      comparison_share = .distribution_shares(.data$comparison_flow, epsilon),
      midpoint_share = 0.5 * (.data$reference_share + .data$comparison_share),
      kl_contribution = .data$reference_share *
        log(.data$reference_share / .data$comparison_share),
      jsd_contribution = .distribution_jsd_contributions(
        .data$reference_share,
        .data$comparison_share
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      method = method_name,
      comparison = comparison,
      comparison_label = spec$comparison_label,
      x_distribution = spec$x_series,
      y_distribution = spec$y_series,
      reference_distribution = reference_distribution,
      comparison_distribution = comparison_distribution,
      zero_benchmark_total = .data$bench_origin_total <= 0,
      zero_raw_total = .data$raw_origin_total <= 0,
      zero_adjusted_total = .data$adjusted_origin_total <= 0,
      zero_adj_total = .data$zero_adjusted_total,
      zero_reference_total = .data$reference_origin_total <= 0,
      zero_comparison_total = .data$comparison_origin_total <= 0
    ) |>
    dplyr::select(
      method,
      comparison,
      comparison_label,
      x_distribution,
      y_distribution,
      reference_distribution,
      comparison_distribution,
      origin,
      destination,
      benchmark_flow,
      raw_flow,
      adjusted_flow,
      reference_flow,
      comparison_flow,
      reference_share,
      comparison_share,
      midpoint_share,
      kl_contribution,
      jsd_contribution,
      bench_origin_total,
      raw_origin_total,
      adjusted_origin_total,
      adj_origin_total,
      reference_origin_total,
      comparison_origin_total,
      zero_benchmark_total,
      zero_raw_total,
      zero_adjusted_total,
      zero_adj_total,
      zero_reference_total,
      zero_comparison_total
    )

  origin_level <- od_level |>
    dplyr::group_by(
      .data$method,
      .data$comparison,
      .data$comparison_label,
      .data$x_distribution,
      .data$y_distribution,
      .data$reference_distribution,
      .data$comparison_distribution,
      .data$origin
    ) |>
    dplyr::summarise(
      n_destinations = dplyr::n(),
      bench_origin_total = dplyr::first(.data$bench_origin_total),
      raw_origin_total = dplyr::first(.data$raw_origin_total),
      adjusted_origin_total = dplyr::first(.data$adjusted_origin_total),
      adj_origin_total = dplyr::first(.data$adj_origin_total),
      reference_origin_total = dplyr::first(.data$reference_origin_total),
      comparison_origin_total = dplyr::first(.data$comparison_origin_total),
      zero_benchmark_total = dplyr::first(.data$zero_benchmark_total),
      zero_raw_total = dplyr::first(.data$zero_raw_total),
      zero_adjusted_total = dplyr::first(.data$zero_adjusted_total),
      zero_adj_total = dplyr::first(.data$zero_adj_total),
      zero_reference_total = dplyr::first(.data$zero_reference_total),
      zero_comparison_total = dplyr::first(.data$zero_comparison_total),
      kl_origin = dplyr::if_else(
        dplyr::first(.data$zero_reference_total),
        NA_real_,
        sum(.data$kl_contribution)
      ),
      jsd_origin = dplyr::if_else(
        dplyr::first(.data$zero_reference_total),
        NA_real_,
        sum(.data$jsd_contribution)
      ),
      .groups = "drop"
    )

  list(origin_level = origin_level, od_level = od_level)
}

#' Build richer OD-level residual diagnostics for adjusted versus benchmark flows
#'
#' Extends \code{validate_flow_pairs()} with residual-style aliases, absolute and
#' percentage residuals, improvement diagnostics, standard-deviation flags for
#' large remaining adjusted residuals, and a convenience table of the worst
#' remaining OD pairs after adjustment. This is useful when you want to move
#' beyond method-level fit and inspect where the adjustment helped, did not
#' help, or made residuals worse.
#'
#' @param adj_df Data frame with at least \code{origin}, \code{destination},
#'   an MPD flow column (default \code{"flow"}), and an adjusted flow column
#'   (default \code{"flow_adj"}). If present, \code{mpd_source} is carried through.
#' @param benchmark_od_df Data frame with at least \code{origin},
#'   \code{destination}, and a benchmark flow column (default \code{"flow"}).
#' @param flow_col_mpd Name of MPD flow column in \code{adj_df}. Default \code{"flow"}.
#' @param flow_col_adj Name of adjusted flow column in \code{adj_df}. Default \code{"flow_adj"}.
#' @param flow_col_bench Name of benchmark flow column in \code{benchmark_od_df}.
#'   Default \code{"flow"}.
#' @param top_n Number of OD pairs to retain in the \code{top_worst} table,
#'   ranked by the absolute residual remaining after adjustment. Default \code{10}.
#' @param method_name Optional label for the adjustment method. Stored in the
#'   summary, data, and \code{top_worst} outputs.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{summary}: one-row tibble with mean/median residual magnitudes and
#'     signed and absolute residual-reduction summaries, shares improved,
#'     worsened, unchanged, and MPD versus adjusted residual shares above 1,
#'     2, and 3 standard deviations,
#'   \item \code{data}: OD-level tibble containing original and adjusted signed
#'     residuals using the package convention \code{Y - X}, signed residual
#'     movement, absolute residual reduction, percentage residuals,
#'     standard-deviation diagnostics for MPD and adjusted residuals, and an
#'     \code{improvement_flag},
#'   \item \code{top_worst}: the \code{top_n} OD pairs with the largest absolute
#'     residual remaining after adjustment.
#' }
#'
#' Signed residuals use the shared comparison convention:
#' \code{adjusted_vs_benchmark} is \code{benchmark - adjusted},
#' \code{raw_vs_benchmark} is \code{benchmark - raw}, and
#' \code{raw_vs_adjusted} is \code{adjusted - raw}. The exact signed movement
#' requested by the Stage 2 validation plan is stored as
#' \code{signed_residual_reduction}; algebraically this equals
#' \code{adjusted - raw}. For a direction-free "positive means less benchmark
#' error" comparison, use \code{abs_residual_reduction} or
#' \code{improvement_flag}.
#'
#' @export
validate_flow_residuals <- function(adj_df,
                                    benchmark_od_df,
                                    flow_col_mpd   = "flow",
                                    flow_col_adj   = "flow_adj",
                                    flow_col_bench = "flow",
                                    top_n          = 10L,
                                    method_name    = NA_character_) {

  if (length(top_n) != 1L || is.na(top_n) || top_n < 1) {
    stop("`top_n` must be a single positive integer.")
  }
  top_n <- as.integer(top_n)

  audit_tbl <- validate_flow_pairs(
    adj_df = adj_df,
    benchmark_od_df = benchmark_od_df,
    flow_col_mpd = flow_col_mpd,
    flow_col_adj = flow_col_adj,
    flow_col_bench = flow_col_bench
  )

  residual_sd_mpd_benchmark <- stats::sd(audit_tbl$diff_mpd_benchmark, na.rm = TRUE)
  residual_sd_adj_benchmark <- stats::sd(audit_tbl$diff_adj_benchmark, na.rm = TRUE)

  residuals <- audit_tbl |>
    dplyr::mutate(
      method = method_name,
      residual_mpd_benchmark = .data$signed_residual_raw_vs_benchmark,
      residual_adj_benchmark = .data$signed_residual_adjusted_vs_benchmark,
      residual_mpd_adj = .data$signed_residual_raw_vs_adjusted,
      benchmark_minus_mpd = .data$residual_mpd_benchmark,
      benchmark_minus_adj = .data$residual_adj_benchmark,
      signed_residual_reduction = .data$residual_mpd_adj,
      abs_residual_mpd_benchmark = abs(.data$residual_mpd_benchmark),
      abs_residual_adj_benchmark = abs(.data$residual_adj_benchmark),
      abs_residual_mpd_adj = abs(.data$residual_mpd_adj),
      pct_residual_mpd_benchmark = dplyr::if_else(
        .data$benchmark_flow == 0,
        NA_real_,
        100 * .data$residual_mpd_benchmark / .data$benchmark_flow
      ),
      pct_residual_adj_benchmark = dplyr::if_else(
        .data$benchmark_flow == 0,
        NA_real_,
        100 * .data$residual_adj_benchmark / .data$benchmark_flow
      ),
      abs_residual_improvement = .data$abs_residual_mpd_benchmark - .data$abs_residual_adj_benchmark,
      abs_residual_reduction = .data$abs_residual_improvement,
      pct_residual_improvement = dplyr::if_else(
        .data$abs_residual_mpd_benchmark == 0,
        NA_real_,
        100 * .data$abs_residual_improvement / .data$abs_residual_mpd_benchmark
      ),
      residual_sd_mpd_benchmark = residual_sd_mpd_benchmark,
      residual_mpd_benchmark_sd_score = .sd_score(
        .data$residual_mpd_benchmark,
        .data$residual_sd_mpd_benchmark
      ),
      abs_residual_mpd_benchmark_sd_score = abs(.data$residual_mpd_benchmark_sd_score),
      residual_sd_adj_benchmark = residual_sd_adj_benchmark,
      residual_adj_benchmark_sd_score = .sd_score(
        .data$residual_adj_benchmark,
        .data$residual_sd_adj_benchmark
      ),
      abs_residual_adj_benchmark_sd_score = abs(.data$residual_adj_benchmark_sd_score),
      residual_mpd_over_1sd = .residual_over_sd(.data$abs_residual_mpd_benchmark_sd_score, 1),
      residual_mpd_over_2sd = .residual_over_sd(.data$abs_residual_mpd_benchmark_sd_score, 2),
      residual_mpd_over_3sd = .residual_over_sd(.data$abs_residual_mpd_benchmark_sd_score, 3),
      residual_adj_over_1sd = .residual_over_sd(.data$abs_residual_adj_benchmark_sd_score, 1),
      residual_adj_over_2sd = .residual_over_sd(.data$abs_residual_adj_benchmark_sd_score, 2),
      residual_adj_over_3sd = .residual_over_sd(.data$abs_residual_adj_benchmark_sd_score, 3),
      moved_in_benchmark_direction = dplyr::case_when(
        .data$benchmark_minus_mpd == 0 & .data$signed_residual_reduction == 0 ~ TRUE,
        .data$benchmark_minus_mpd == 0 | .data$signed_residual_reduction == 0 ~ FALSE,
        sign(.data$benchmark_minus_mpd) == sign(.data$signed_residual_reduction) ~ TRUE,
        TRUE ~ FALSE
      ),
      improvement_flag = dplyr::case_when(
        .data$abs_residual_improvement > 0 ~ "improved",
        .data$abs_residual_improvement < 0 ~ "worsened",
        TRUE ~ "unchanged"
      )
    )

  summary_tbl <- residuals |>
    dplyr::summarise(
      method = method_name,
      n = dplyr::n(),
      mean_signed_residual_reduction = mean(.data$signed_residual_reduction, na.rm = TRUE),
      median_signed_residual_reduction = stats::median(.data$signed_residual_reduction, na.rm = TRUE),
      mean_abs_residual_reduction = mean(.data$abs_residual_reduction, na.rm = TRUE),
      median_abs_residual_reduction = stats::median(.data$abs_residual_reduction, na.rm = TRUE),
      mean_abs_residual_mpd_benchmark = mean(.data$abs_residual_mpd_benchmark, na.rm = TRUE),
      mean_abs_residual_adj_benchmark = mean(.data$abs_residual_adj_benchmark, na.rm = TRUE),
      median_abs_residual_mpd_benchmark = stats::median(.data$abs_residual_mpd_benchmark, na.rm = TRUE),
      median_abs_residual_adj_benchmark = stats::median(.data$abs_residual_adj_benchmark, na.rm = TRUE),
      share_improved = .mean_logical_na(.data$improvement_flag == "improved"),
      share_worsened = .mean_logical_na(.data$improvement_flag == "worsened"),
      share_unchanged = .mean_logical_na(.data$improvement_flag == "unchanged"),
      share_moved_in_benchmark_direction = .mean_logical_na(.data$moved_in_benchmark_direction),
      share_residual_mpd_over_1sd = .mean_logical_na(.data$residual_mpd_over_1sd),
      share_residual_mpd_over_2sd = .mean_logical_na(.data$residual_mpd_over_2sd),
      share_residual_mpd_over_3sd = .mean_logical_na(.data$residual_mpd_over_3sd),
      share_residual_adj_over_1sd = .mean_logical_na(.data$residual_adj_over_1sd),
      share_residual_adj_over_2sd = .mean_logical_na(.data$residual_adj_over_2sd),
      share_residual_adj_over_3sd = .mean_logical_na(.data$residual_adj_over_3sd),
      reduction_share_residual_over_2sd =
        .data$share_residual_mpd_over_2sd - .data$share_residual_adj_over_2sd
    )

  top_worst_tbl <- residuals |>
    dplyr::arrange(
      dplyr::desc(.data$abs_residual_adj_benchmark),
      dplyr::desc(.data$abs_residual_mpd_benchmark)
    ) |>
    dplyr::slice_head(n = top_n)

  list(
    summary = summary_tbl,
    data = residuals,
    top_worst = top_worst_tbl
  )
}

#' Validate residual structure and randomness diagnostics
#'
#' Builds residual-structure diagnostics for a selected flow comparison. The
#' function reports residual correlation with benchmark flow,
#' aggregates residuals to origin or destination areas for map-ready output,
#' optionally computes global Moran's I from a user-supplied neighbour table,
#' and optionally relates area-level residuals to a selected covariate.
#'
#' @param adj_df Data frame with at least \code{origin}, \code{destination},
#'   an MPD flow column (default \code{"flow"}), and an adjusted flow column
#'   (default \code{"flow_adj"}).
#' @param benchmark_od_df Data frame with at least \code{origin},
#'   \code{destination}, and a benchmark flow column (default \code{"flow"}).
#' @param flow_col_mpd Name of MPD flow column in \code{adj_df}. Default \code{"flow"}.
#' @param flow_col_adj Name of adjusted flow column in \code{adj_df}. Default \code{"flow_adj"}.
#' @param flow_col_bench Name of benchmark flow column in \code{benchmark_od_df}.
#'   Default \code{"flow"}.
#' @param method_name Optional label for the adjustment method. Stored in outputs.
#' @param residual_type Legacy residual-series shortcut. \code{"adjusted"} maps
#'   to \code{comparison = "adjusted_vs_benchmark"}, \code{"mpd"} maps to
#'   \code{comparison = "raw_vs_benchmark"}, and \code{"adjustment"} maps to
#'   \code{comparison = "raw_vs_adjusted"}.
#' @param comparison Flow comparison to diagnose. The first series in the ID is
#'   the x-axis/baseline, the second series is the y-axis/reference, and signed
#'   residuals are \code{Y - X}. If \code{NULL}, \code{residual_type} is used
#'   for backward compatibility. Default \code{NULL}.
#' @param spatial_role Area role used for area-level residual summaries:
#'   \code{"origin"} or \code{"destination"}.
#' @param residual_aggregation How OD residuals are aggregated to area level:
#'   \code{"mean"} or \code{"sum"}.
#' @param area_neighbors Optional neighbour table for Moran's I.
#' @param area_col Column in \code{area_neighbors} identifying the focal area.
#'   Default \code{"area"}.
#' @param neighbor_col Column in \code{area_neighbors} identifying the neighbouring
#'   area. Default \code{"neighbor"}.
#' @param weight_col Optional positive numeric weight column in
#'   \code{area_neighbors}. If \code{NULL}, all neighbour links receive weight 1.
#' @param local_moran Logical. If \code{TRUE}, compute Local Moran's I and LISA
#'   cluster classes from the selected area-level residuals and
#'   \code{area_neighbors}. Default \code{FALSE}.
#' @param local_moran_nsim Positive whole number of random permutations used for
#'   Local Moran pseudo p-values. Default \code{999}.
#' @param local_moran_alpha Significance threshold used to classify LISA
#'   clusters after p-value adjustment. Default \code{0.05}.
#' @param local_moran_p_adjust P-value adjustment method passed to
#'   \code{stats::p.adjust()}. Default \code{"BH"}.
#' @param local_moran_seed Optional random seed for Local Moran permutation
#'   p-values. Default \code{NULL}.
#' @param covariate_df Optional area-level covariate table.
#' @param covariate_col Optional covariate column to correlate with area-level
#'   residuals. Requires \code{covariate_df}.
#' @param covariate_area_col Area key in \code{covariate_df}. Default \code{"area"}.
#' @param geometry_df Optional area table with coordinates or geometry-like
#'   columns to join onto \code{map_data}.
#' @param geometry_area_col Area key in \code{geometry_df}. Default \code{"area"}.
#' @param x_col Optional x-coordinate column in \code{map_data}, used only when
#'   \code{make_plots = TRUE}.
#' @param y_col Optional y-coordinate column in \code{map_data}, used only when
#'   \code{make_plots = TRUE}.
#' @param make_plots Logical. If \code{TRUE}, return ggplot objects for residual
#'   reduction distribution, residual-versus-benchmark scatter, optional
#'   residual-versus-covariate scatter, and optional coordinate residual map.
#'   Requires \pkg{ggplot2}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{summary}: one-row tibble with flow correlation, Moran's I when
#'     available, and covariate correlation when requested,
#'   \item \code{flow_correlation}: Pearson correlation between selected OD
#'     residuals and benchmark OD flows,
#'   \item \code{moran_i}: Moran's I summary from the neighbour table, or
#'     \code{NA} when no neighbour table is supplied,
#'   \item \code{local_moran}: optional Local Moran's I and LISA cluster table
#'     when \code{local_moran = TRUE},
#'   \item \code{covariate_correlation}: optional Pearson correlation between
#'     area-level residuals and the selected covariate,
#'   \item \code{od_level}: OD-level residual table,
#'   \item \code{area_level}: area-level residual table,
#'   \item \code{map_data}: area-level residual table joined to \code{geometry_df}
#'     when supplied,
#'   \item \code{plots}: optional ggplot objects when \code{make_plots = TRUE}.
#' }
#' @export
validate_flow_residual_structure <- function(adj_df,
                                             benchmark_od_df,
                                             flow_col_mpd   = "flow",
                                             flow_col_adj   = "flow_adj",
                                             flow_col_bench = "flow",
                                             method_name    = NA_character_,
                                             residual_type  = c("adjusted", "mpd", "adjustment"),
                                             comparison     = NULL,
                                             spatial_role   = c("origin", "destination"),
                                             residual_aggregation = c("mean", "sum"),
                                             area_neighbors = NULL,
                                             area_col       = "area",
                                             neighbor_col   = "neighbor",
                                             weight_col     = NULL,
                                             local_moran    = FALSE,
                                             local_moran_nsim = 999,
                                             local_moran_alpha = 0.05,
                                             local_moran_p_adjust = "BH",
                                             local_moran_seed = NULL,
                                             covariate_df   = NULL,
                                             covariate_col  = NULL,
                                             covariate_area_col = "area",
                                             geometry_df    = NULL,
                                             geometry_area_col = "area",
                                             x_col          = NULL,
                                             y_col          = NULL,
                                             make_plots     = FALSE) {

  residual_type <- match.arg(residual_type)
  if (is.null(comparison)) {
    comparison <- switch(
      residual_type,
      adjusted = "adjusted_vs_benchmark",
      mpd = "raw_vs_benchmark",
      adjustment = "raw_vs_adjusted"
    )
  } else {
    comparison <- .normalise_flow_comparisons(comparison)
    if (length(comparison) != 1L) {
      stop("`comparison` must select exactly one flow comparison.", call. = FALSE)
    }
  }
  comparison_spec <- .flow_comparison_spec(comparison)
  spatial_role <- match.arg(spatial_role)
  residual_aggregation <- match.arg(residual_aggregation)
  residual_type_label <- switch(
    comparison,
    adjusted_vs_benchmark = "adjusted",
    raw_vs_benchmark = "mpd",
    raw_vs_adjusted = "adjustment"
  )
  spatial_role_label <- spatial_role
  residual_aggregation_label <- residual_aggregation

  if (make_plots && !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("`make_plots = TRUE` requires the ggplot2 package.")
  }
  if (!is.null(covariate_col) && is.null(covariate_df)) {
    stop("`covariate_df` is required when `covariate_col` is supplied.")
  }
  if (!is.logical(local_moran) || length(local_moran) != 1L || is.na(local_moran)) {
    stop("`local_moran` must be `TRUE` or `FALSE`.")
  }
  if (local_moran) {
    .validate_local_moran_args(
      local_moran_nsim = local_moran_nsim,
      local_moran_alpha = local_moran_alpha,
      local_moran_p_adjust = local_moran_p_adjust,
      local_moran_seed = local_moran_seed
    )
  }

  residual_result <- validate_flow_residuals(
    adj_df = adj_df,
    benchmark_od_df = benchmark_od_df,
    flow_col_mpd = flow_col_mpd,
    flow_col_adj = flow_col_adj,
    flow_col_bench = flow_col_bench,
    method_name = method_name
  )

  residual_col <- switch(
    comparison,
    adjusted_vs_benchmark = "residual_adj_benchmark",
    raw_vs_benchmark = "residual_mpd_benchmark",
    raw_vs_adjusted = "residual_mpd_adj"
  )

  od_level <- residual_result$data |>
    dplyr::mutate(
      selected_residual = .data[[residual_col]],
      residual_type = residual_type_label,
      comparison = comparison,
      comparison_label = comparison_spec$comparison_label,
      x_series = comparison_spec$x_series,
      y_series = comparison_spec$y_series,
      signed_residual = comparison_spec$signed_residual
    )

  flow_correlation <- tibble::tibble(
    method = method_name,
    residual_type = residual_type_label,
    comparison = comparison,
    comparison_label = comparison_spec$comparison_label,
    benchmark_comparison = "benchmark_flow",
    n = sum(is.finite(od_level$selected_residual) & is.finite(od_level$benchmark_flow)),
    pearson_r = .safe_pearson(od_level$selected_residual, od_level$benchmark_flow)
  )

  area_level <- od_level |>
    dplyr::mutate(area = .data[[spatial_role]]) |>
    dplyr::group_by(.data$area) |>
    dplyr::summarise(
      method = method_name,
      residual_type = residual_type_label,
      comparison = dplyr::first(.data$comparison),
      comparison_label = dplyr::first(.data$comparison_label),
      spatial_role = spatial_role_label,
      residual_aggregation = residual_aggregation_label,
      n_od_pairs = dplyr::n(),
      selected_residual = .aggregate_residual(.data$selected_residual, residual_aggregation_label),
      mean_residual = mean(.data$selected_residual, na.rm = TRUE),
      sum_residual = sum(.data$selected_residual, na.rm = TRUE),
      mean_abs_residual = mean(abs(.data$selected_residual), na.rm = TRUE),
      benchmark_flow_sum = sum(.data$benchmark_flow, na.rm = TRUE),
      mpd_flow_sum = sum(.data$mpd_flow, na.rm = TRUE),
      adj_flow_sum = sum(.data$adj_flow, na.rm = TRUE),
      .groups = "drop"
    )

  moran_stats <- list(
    moran_i = NA_real_,
    n_areas_used = sum(is.finite(area_level$selected_residual)),
    n_links_used = NA_integer_,
    weight_sum = NA_real_
  )
  if (!is.null(area_neighbors)) {
    moran_stats <- .compute_moran_i(
      area_level = area_level,
      area_neighbors = area_neighbors,
      area_col = area_col,
      neighbor_col = neighbor_col,
      weight_col = weight_col
    )
  }

  moran_tbl <- tibble::tibble(
    method = method_name,
    residual_type = residual_type_label,
    comparison = comparison,
    comparison_label = comparison_spec$comparison_label,
    spatial_role = spatial_role_label,
    residual_aggregation = residual_aggregation_label,
    n_areas_used = moran_stats$n_areas_used,
    n_links_used = moran_stats$n_links_used,
    weight_sum = moran_stats$weight_sum,
    moran_i = moran_stats$moran_i
  )

  covariate_data <- NULL
  covariate_correlation <- tibble::tibble(
    method = method_name,
    residual_type = residual_type_label,
    comparison = comparison,
    comparison_label = comparison_spec$comparison_label,
    spatial_role = spatial_role_label,
    covariate = NA_character_,
    n = NA_integer_,
    pearson_r = NA_real_
  )

  if (!is.null(covariate_col)) {
    req_covariates <- c(covariate_area_col, covariate_col)
    if (!all(req_covariates %in% names(covariate_df))) {
      stop("`covariate_df` must contain: ", paste(req_covariates, collapse = ", "))
    }

    covariate_data <- covariate_df |>
      dplyr::select(
        area = dplyr::all_of(covariate_area_col),
        covariate_value = dplyr::all_of(covariate_col)
      ) |>
      dplyr::right_join(area_level, by = "area")

    covariate_correlation <- tibble::tibble(
      method = method_name,
      residual_type = residual_type_label,
      comparison = comparison,
      comparison_label = comparison_spec$comparison_label,
      spatial_role = spatial_role_label,
      covariate = covariate_col,
      n = sum(is.finite(covariate_data$selected_residual) & is.finite(covariate_data$covariate_value)),
      pearson_r = .safe_pearson(covariate_data$selected_residual, covariate_data$covariate_value)
    )
  }

  map_data <- area_level
  if (!is.null(geometry_df)) {
    if (!geometry_area_col %in% names(geometry_df)) {
      stop("`geometry_area_col` must name a column in `geometry_df`.")
    }
    geometry_tbl <- geometry_df |>
      dplyr::rename(area = dplyr::all_of(geometry_area_col))
    map_data <- area_level |>
      dplyr::left_join(geometry_tbl, by = "area")
  }

  local_moran_tbl <- NULL
  if (local_moran) {
    local_moran_tbl <- .compute_local_moran(
      area_level = area_level,
      area_neighbors = area_neighbors,
      area_col = area_col,
      neighbor_col = neighbor_col,
      weight_col = weight_col,
      nsim = as.integer(local_moran_nsim),
      alpha = local_moran_alpha,
      p_adjust = local_moran_p_adjust,
      seed = local_moran_seed
    )

    map_data <- map_data |>
      dplyr::left_join(
        local_moran_tbl |>
          dplyr::select(
            area,
            centered_residual,
            residual_z,
            neighbor_lag_sum,
            neighbor_lag_mean,
            spatial_lag_z,
            n_neighbors_used,
            local_weight_sum,
            local_moran_i,
            lisa_quadrant,
            p_value,
            p_adjusted,
            significant,
            lisa_cluster
          ),
        by = "area"
      )
  }

  summary_tbl <- tibble::tibble(
    method = method_name,
    residual_type = residual_type_label,
    comparison = comparison,
    comparison_label = comparison_spec$comparison_label,
    spatial_role = spatial_role_label,
    residual_aggregation = residual_aggregation_label,
    n_od_pairs = nrow(od_level),
    n_areas = nrow(area_level),
    pearson_residual_benchmark_flow = flow_correlation$pearson_r,
    moran_i = moran_tbl$moran_i,
    pearson_residual_covariate = covariate_correlation$pearson_r
  )

  out <- list(
    summary = summary_tbl,
    flow_correlation = flow_correlation,
    moran_i = moran_tbl,
    covariate_correlation = covariate_correlation,
    od_level = od_level,
    area_level = area_level,
    map_data = map_data
  )

  if (!is.null(covariate_data)) {
    out$covariate_data <- covariate_data
  }

  if (!is.null(local_moran_tbl)) {
    out$local_moran <- local_moran_tbl
  }

  if (make_plots) {
    plots <- list(
      residual_reduction_distribution =
        ggplot2::ggplot(od_level, ggplot2::aes(x = .data$abs_residual_reduction)) +
        ggplot2::geom_histogram(bins = 30, fill = "#2B8CBE", color = "white", na.rm = TRUE) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#D95F0E") +
        ggplot2::labs(
          x = "Absolute residual reduction",
          y = "OD pairs",
          title = "Residual reduction distribution"
        ),
      residual_vs_benchmark =
        ggplot2::ggplot(od_level, ggplot2::aes(x = .data$benchmark_flow, y = .data$selected_residual)) +
        ggplot2::geom_point(alpha = 0.7, na.rm = TRUE) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#D95F0E") +
        ggplot2::labs(
          x = "Benchmark OD flow",
          y = paste0(comparison_spec$comparison_label, " residual"),
          title = "Residuals versus benchmark flow"
        )
    )

    if (!is.null(covariate_data)) {
      plots$residual_vs_covariate <-
        ggplot2::ggplot(
          covariate_data,
          ggplot2::aes(x = .data$covariate_value, y = .data$selected_residual)
        ) +
        ggplot2::geom_point(alpha = 0.7, na.rm = TRUE) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#D95F0E") +
        ggplot2::labs(
          x = covariate_col,
          y = paste0(comparison_spec$comparison_label, " area residual"),
          title = "Area residuals versus covariate"
        )
    }

    if (!is.null(x_col) || !is.null(y_col)) {
      if (is.null(x_col) || is.null(y_col)) {
        stop("Both `x_col` and `y_col` are required for a residual map plot.")
      }
      if (!all(c(x_col, y_col) %in% names(map_data))) {
        stop("`x_col` and `y_col` must name columns in `map_data`.")
      }
      map_plot_data <- map_data |>
        dplyr::mutate(
          .map_x = .data[[x_col]],
          .map_y = .data[[y_col]]
        )
      plots$residual_map <-
        ggplot2::ggplot(
          map_plot_data,
          ggplot2::aes(x = .data$.map_x, y = .data$.map_y, color = .data$selected_residual)
        ) +
        ggplot2::geom_point(size = 2.5, alpha = 0.9, na.rm = TRUE) +
        ggplot2::coord_equal() +
        ggplot2::labs(
          x = x_col,
          y = y_col,
          color = "Residual",
          title = "Area-level residual map"
        )
    }

    out$plots <- plots
  }

  out
}
