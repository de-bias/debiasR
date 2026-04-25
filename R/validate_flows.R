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
#'
#' @return A list with:
#'   \itemize{
#'     \item method (if provided)
#'     \item n, sum_adj, sum_bench
#'     \item pearson_r, spearman_rho
#'     \item rmse, mae, mape
#'     \item ols_intercept, ols_slope, r_squared (from lm(y ~ x))
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
                                  method_name    = NA_character_) {

  # --- Required columns
  req_adj   <- c("origin", "destination", flow_col_adj)
  req_bench <- c("origin", "destination", flow_col_bench)
  if (!all(req_adj %in% names(adj_df))) {
    stop("`adj_df` must contain: ", paste(req_adj, collapse = ", "))
  }
  if (!all(req_bench %in% names(benchmark_od_df))) {
    stop("`benchmark_od_df` must contain: ", paste(req_bench, collapse = ", "))
  }

  # --- Prepare joined data
  joined <- adj_df |>
    dplyr::select(dplyr::any_of(c("mpd_source")), origin, destination, !!flow_col_adj) |>
    dplyr::rename(flow_adj = !!flow_col_adj) |>
    dplyr::inner_join(
      benchmark_od_df |>
        dplyr::select(origin, destination, !!flow_col_bench) |>
        dplyr::rename(flow_bench = !!flow_col_bench),
      by = c("origin", "destination")
    )

  # --- Clean rows
  if (na_rm) {
    joined <- dplyr::filter(joined,
                            is.finite(.data$flow_adj),
                            is.finite(.data$flow_bench))
  }
  if (drop_zeros) {
    joined <- dplyr::filter(joined,
                            .data$flow_adj > 0,
                            .data$flow_bench > 0)
  }

  # --- Empty guard
  if (nrow(joined) == 0L) {
    res <- list(
      method        = method_name,
      n             = 0L,
      sum_adj       = 0,
      sum_bench     = 0,
      pearson_r     = NA_real_,
      spearman_rho  = NA_real_,
      rmse          = NA_real_,
      mae           = NA_real_,
      mape          = NA_real_,
      ols_intercept = NA_real_,
      ols_slope     = NA_real_,
      r_squared     = NA_real_
    )
    if (by_source && "mpd_source" %in% names(joined)) {
      res$by_source <- dplyr::tibble()
    }
    if (return_joined) res$data <- joined
    return(res)
  }

  # --- Core metrics (x = adjusted; y = benchmark)
  x <- joined$flow_adj
  y <- joined$flow_bench

  sum_adj   <- sum(x, na.rm = TRUE)
  sum_bench <- sum(y, na.rm = TRUE)

  pearson_r    <- suppressWarnings(stats::cor(x, y, method = "pearson"))
  spearman_rho <- suppressWarnings(stats::cor(x, y, method = "spearman"))

  rmse  <- sqrt(mean((y - x)^2))
  mae   <- mean(abs(y - x))
  denom <- ifelse(y == 0, NA_real_, y)
  mape  <- mean(abs((y - x) / denom), na.rm = TRUE)

  fit <- stats::lm(y ~ x)
  coefs <- stats::coef(fit)
  ols_intercept <- unname(coefs[1])
  ols_slope     <- unname(coefs[2])
  r_squared     <- unname(summary(fit)$r.squared)

  # --- Package result
  out <- list(
    method        = method_name,
    n             = nrow(joined),
    sum_adj       = sum_adj,
    sum_bench     = sum_bench,
    pearson_r     = pearson_r,
    spearman_rho  = spearman_rho,
    rmse          = rmse,
    mae           = mae,
    mape          = mape,
    ols_intercept = ols_intercept,
    ols_slope     = ols_slope,
    r_squared     = r_squared
  )

  # --- Optional: per-source metrics
  if (by_source && "mpd_source" %in% names(joined)) {
    metrics_fun <- function(df) {
      xx <- df$flow_adj
      yy <- df$flow_bench
      data.frame(
        n = length(xx),
        sum_adj = sum(xx),
        sum_bench = sum(yy),
        pearson_r = suppressWarnings(stats::cor(xx, yy, method = "pearson")),
        spearman_rho = suppressWarnings(stats::cor(xx, yy, method = "spearman")),
        rmse = sqrt(mean((yy - xx)^2)),
        mae  = mean(abs(yy - xx)),
        mape = {
          dd <- ifelse(yy == 0, NA_real_, yy)
          mean(abs((yy - xx) / dd), na.rm = TRUE)
        },
        ols_intercept = {
          if (length(xx) >= 2 && all(is.finite(xx + yy))) {
            unname(stats::coef(stats::lm(yy ~ xx))[1])
          } else NA_real_
        },
        ols_slope = {
          if (length(xx) >= 2 && all(is.finite(xx + yy))) {
            unname(stats::coef(stats::lm(yy ~ xx))[2])
          } else NA_real_
        },
        r_squared = {
          if (length(xx) >= 2 && all(is.finite(xx + yy))) {
            unname(summary(stats::lm(yy ~ xx))$r.squared)
          } else NA_real_
        }
      )
    }

    by_src <- joined |>
      dplyr::group_by(mpd_source) |>
      dplyr::group_modify(~ tibble::as_tibble(metrics_fun(.x))) |>
      dplyr::ungroup()

    out$by_source <- by_src
  }

  if (return_joined) out$data <- joined
  out
}

#' Legacy alias for \code{validate_flow_overall()}
#'
#' Retained for backwards compatibility. New code should prefer
#' \code{validate_flow_overall()}.
#'
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
      diff_adj_benchmark = .data$adj_flow - .data$benchmark_flow
    )

  tibble::as_tibble(out)
}

#' Build an OD-pair validation table for MPD, adjusted, and benchmark flows
#'
#' Joins adjusted-flow output to benchmark OD flows and returns a tidy table with
#' original MPD flow, adjusted flow, benchmark flow, and residual-style
#' difference columns. This complements \code{validate_flow_overall()}, which
#' summarizes method-level fit, by exposing OD-level differences directly.
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
#'   \item \code{diff_adj_benchmark = adj_flow - benchmark_flow}.
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

#' Validate origin-conditioned destination-share distributions
#'
#' Compares adjusted OD flows with benchmark OD flows after normalizing each
#' origin's destination flows into shares. This reports Kullback-Leibler
#' divergence using the benchmark distribution as the reference,
#' `KL(benchmark || adjusted)`, and Jensen-Shannon divergence as a symmetric
#' companion metric. Lower values indicate closer destination-allocation
#' fidelity. These metrics assess spatial allocation, not total flow scale.
#'
#' @param adj_df Data frame with at least \code{origin}, \code{destination},
#'   and an adjusted flow column (default \code{"flow_adj"}).
#' @param benchmark_od_df Data frame with at least \code{origin},
#'   \code{destination}, and a benchmark flow column (default \code{"flow"}).
#' @param flow_col_adj Name of adjusted flow column in \code{adj_df}. Default
#'   \code{"flow_adj"}.
#' @param flow_col_bench Name of benchmark flow column in
#'   \code{benchmark_od_df}. Default \code{"flow"}.
#' @param epsilon Small positive smoothing constant added to benchmark and
#'   adjusted flows before shares are computed. Default \code{1e-8}.
#' @param method_name Optional label for the adjustment method. Stored in the
#'   summary and origin-level outputs.
#' @param weight_by Weighting rule for weighted summary means. Currently
#'   \code{"none"} or \code{"benchmark_origin_total"}.
#' @param return_origin_level Logical, return one row per origin in the output.
#'   Default \code{TRUE}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{summary}: one-row tibble with origin count, mean, median, and
#'     optionally benchmark-total-weighted mean KL and JSD,
#'   \item \code{origin_level}: origin-level tibble with KL, JSD, destination
#'     count, benchmark and adjusted origin totals, and zero-total flags when
#'     \code{return_origin_level = TRUE}.
#' }
#' @export
validate_flow_distribution <- function(adj_df,
                                       benchmark_od_df,
                                       flow_col_adj = "flow_adj",
                                       flow_col_bench = "flow",
                                       epsilon = 1e-8,
                                       method_name = NA_character_,
                                       weight_by = c("none", "benchmark_origin_total"),
                                       return_origin_level = TRUE) {

  weight_by <- match.arg(weight_by)

  if (length(epsilon) != 1L || !is.finite(epsilon) || epsilon <= 0) {
    stop("`epsilon` must be a single positive finite number.")
  }

  req_adj <- c("origin", "destination", flow_col_adj)
  req_bench <- c("origin", "destination", flow_col_bench)

  if (!all(req_adj %in% names(adj_df))) {
    stop("`adj_df` must contain: ", paste(req_adj, collapse = ", "))
  }
  if (!all(req_bench %in% names(benchmark_od_df))) {
    stop("`benchmark_od_df` must contain: ", paste(req_bench, collapse = ", "))
  }

  adj_tbl <- adj_df |>
    dplyr::select(origin, destination, !!flow_col_adj) |>
    dplyr::rename(adj_flow = !!flow_col_adj)

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
      adj_flow = dplyr::coalesce(.data$adj_flow, 0)
    )

  origin_level <- support_tbl |>
    dplyr::group_by(origin) |>
    dplyr::mutate(
      bench_origin_total = sum(.data$benchmark_flow, na.rm = TRUE),
      adj_origin_total = sum(.data$adj_flow, na.rm = TRUE),
      p_share_bench = (.data$benchmark_flow + epsilon) /
        sum(.data$benchmark_flow + epsilon, na.rm = TRUE),
      q_share_adj = (.data$adj_flow + epsilon) /
        sum(.data$adj_flow + epsilon, na.rm = TRUE),
      midpoint_share = 0.5 * (.data$p_share_bench + .data$q_share_adj)
    ) |>
    dplyr::summarise(
      method = method_name,
      n_destinations = dplyr::n(),
      bench_origin_total = dplyr::first(.data$bench_origin_total),
      adj_origin_total = dplyr::first(.data$adj_origin_total),
      zero_benchmark_total = dplyr::first(.data$bench_origin_total) <= 0,
      zero_adj_total = dplyr::first(.data$adj_origin_total) <= 0,
      kl_origin = dplyr::if_else(
        dplyr::first(.data$zero_benchmark_total),
        NA_real_,
        sum(.data$p_share_bench * log(.data$p_share_bench / .data$q_share_adj))
      ),
      jsd_origin = dplyr::if_else(
        dplyr::first(.data$zero_benchmark_total),
        NA_real_,
        0.5 * sum(.data$p_share_bench * log(.data$p_share_bench / .data$midpoint_share)) +
          0.5 * sum(.data$q_share_adj * log(.data$q_share_adj / .data$midpoint_share))
      ),
      .groups = "drop"
    )

  summary_tbl <- origin_level |>
    dplyr::summarise(
      method = method_name,
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
      )
    )

  out <- list(summary = summary_tbl)
  if (return_origin_level) {
    out$origin_level <- origin_level
  }
  out
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
#'
#' @return A list with:
#' \itemize{
#'   \item \code{summary}: one-row tibble with mean/median residual magnitudes and
#'     shares improved, worsened, unchanged, and residual shares above 1, 2,
#'     and 3 standard deviations,
#'   \item \code{data}: OD-level tibble containing original and adjusted residuals,
#'     absolute residuals, percentage residuals, improvement measures, standard
#'     deviation diagnostics for adjusted residuals, and an \code{improvement_flag},
#'   \item \code{top_worst}: the \code{top_n} OD pairs with the largest absolute
#'     residual remaining after adjustment.
#' }
#' @export
validate_flow_residuals <- function(adj_df,
                                    benchmark_od_df,
                                    flow_col_mpd   = "flow",
                                    flow_col_adj   = "flow_adj",
                                    flow_col_bench = "flow",
                                    top_n          = 10L) {

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

  residual_sd_adj_benchmark <- stats::sd(audit_tbl$diff_adj_benchmark, na.rm = TRUE)

  residuals <- audit_tbl |>
    dplyr::mutate(
      residual_mpd_benchmark = .data$diff_mpd_benchmark,
      residual_adj_benchmark = .data$diff_adj_benchmark,
      residual_mpd_adj = .data$diff_mpd_adj,
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
      pct_residual_improvement = dplyr::if_else(
        .data$abs_residual_mpd_benchmark == 0,
        NA_real_,
        100 * .data$abs_residual_improvement / .data$abs_residual_mpd_benchmark
      ),
      residual_sd_adj_benchmark = residual_sd_adj_benchmark,
      residual_adj_benchmark_sd_score = dplyr::if_else(
        is.na(.data$residual_sd_adj_benchmark) | .data$residual_sd_adj_benchmark <= 0,
        NA_real_,
        .data$residual_adj_benchmark / .data$residual_sd_adj_benchmark
      ),
      abs_residual_adj_benchmark_sd_score = abs(.data$residual_adj_benchmark_sd_score),
      residual_adj_over_1sd = dplyr::if_else(
        is.na(.data$abs_residual_adj_benchmark_sd_score),
        NA,
        .data$abs_residual_adj_benchmark_sd_score > 1
      ),
      residual_adj_over_2sd = dplyr::if_else(
        is.na(.data$abs_residual_adj_benchmark_sd_score),
        NA,
        .data$abs_residual_adj_benchmark_sd_score > 2
      ),
      residual_adj_over_3sd = dplyr::if_else(
        is.na(.data$abs_residual_adj_benchmark_sd_score),
        NA,
        .data$abs_residual_adj_benchmark_sd_score > 3
      ),
      improvement_flag = dplyr::case_when(
        .data$abs_residual_improvement > 0 ~ "improved",
        .data$abs_residual_improvement < 0 ~ "worsened",
        TRUE ~ "unchanged"
      )
    )

  summary_tbl <- residuals |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_abs_residual_mpd_benchmark = mean(.data$abs_residual_mpd_benchmark, na.rm = TRUE),
      mean_abs_residual_adj_benchmark = mean(.data$abs_residual_adj_benchmark, na.rm = TRUE),
      median_abs_residual_mpd_benchmark = stats::median(.data$abs_residual_mpd_benchmark, na.rm = TRUE),
      median_abs_residual_adj_benchmark = stats::median(.data$abs_residual_adj_benchmark, na.rm = TRUE),
      share_improved = mean(.data$improvement_flag == "improved", na.rm = TRUE),
      share_worsened = mean(.data$improvement_flag == "worsened", na.rm = TRUE),
      share_unchanged = mean(.data$improvement_flag == "unchanged", na.rm = TRUE),
      share_residual_adj_over_1sd = mean(.data$residual_adj_over_1sd, na.rm = TRUE),
      share_residual_adj_over_2sd = mean(.data$residual_adj_over_2sd, na.rm = TRUE),
      share_residual_adj_over_3sd = mean(.data$residual_adj_over_3sd, na.rm = TRUE)
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
