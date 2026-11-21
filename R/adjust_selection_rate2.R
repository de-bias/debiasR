#' Selection Rate II weighting (Zagheni & Weber 2012) with k calibration
#'
#' Notation used throughout:
#' \itemize{
#'   \item \eqn{P_i^{(O)}, P_j^{(D)}}: population at origin \eqn{i} and destination \eqn{j}
#'   \item \eqn{U_i^{(O)}, U_j^{(D)}}: active users at origin \eqn{i} and destination \eqn{j}
#'   \item \eqn{p_i^{(O)} = U_i^{(O)}/P_i^{(O)}} and \eqn{p_j^{(D)} = U_j^{(D)}/P_j^{(D)}}: penetration
#'   \item \eqn{F_{ij}^{mpd}} and \eqn{F_{ij}^{adj}}: observed and adjusted flows
#'   \item \eqn{k > 0}: selection-rate curvature parameter
#' }
#'
#' Implements the "Selection Rate II" correction:
#' \deqn{CF(p; k) = p \frac{e^{-k} - 1}{e^{-k p} - 1}}
#'
#' where \eqn{p} is a penetration rate between 0 and 1 (e.g. Internet or platform
#' penetration), and k > 0 controls how strongly selection bias increases
#' as p decreases.
#'
#' For OD flows:
#'   - \code{weight_by = "origin"}: \eqn{w_{ij} = CF(p_i^{(O)}; k)}
#'   - \code{weight_by = "destination"}: \eqn{w_{ij} = CF(p_j^{(D)}; k)}
#'   - \code{weight_by = "both"}: \eqn{w_{ij} = \sqrt{CF(p_i^{(O)}; k)\,CF(p_j^{(D)}; k)}}
#'
#' Adjusted flows are:
#' \deqn{F_{ij}^{adj} = F_{ij}^{mpd} \times w_{ij}}
#'
#' Supports:
#'   1) Location-only OD (default).
#'   2) Stratified OD (e.g. age/sex) via group_cols; then p and CF are
#'      computed by (area, group_cols).
#'
#' Calibration of k:
#'   - If k is provided (numeric scalar): use it.
#'   - Else if benchmark_od_df is provided:
#'       search k_grid, pick k* minimising sum of absolute errors between
#'       adjusted and benchmark flows (origin-aggregated or OD-level).
#'   - Else: default k = 1.
#'
#' @param mpd_od_df Data frame with at least:
#'   origin, destination, flow, mpd_source; plus group_cols if used.
#' @param coverage_df Data frame with:
#'   NEW: origin, origin_population, origin_user_count,
#'        destination, destination_population, destination_user_count, mpd_source
#'   or LEGACY: origin, population, user_count, mpd_source.
#'   Must contain group_cols if used.
#' @param weight_by "origin", "destination", or "both".
#' @param group_cols Optional character vector of stratification variables
#'   present in both mpd_od_df and coverage_df (e.g. c("age_group","sex")).
#' @param k Optional positive scalar. If NULL and benchmark_od_df supplied,
#'   k is calibrated by grid search. If NULL and no benchmark, k = 1.
#' @param k_grid Grid of candidate k values for calibration when k is NULL.
#'   Default seq(0.1, 5, by = 0.1).
#' @param benchmark_od_df Optional benchmark OD for calibrating k.
#'   Must contain origin, destination, and a flow column.
#' @param flow_col_bench Name of benchmark flow column. Default "flow".
#' @param calibration_aggregate "origin" (default, compare origin totals)
#'   or "od" (compare OD flows directly).
#' @param clip_min Lower bound used to clamp weights. Default 0.
#' @param clip_max Upper bound used to clamp weights. Default Inf.
#' @param keep_cols Extra columns from mpd_od_df to retain.
#'
#' @details Calibration uses the same matched observations with usable coverage for
#' every candidate. An empty comparison raises an error. A candidate that
#' cannot predict all supported observations receives infinite loss.
#'
#' @return Tibble with:
#'   origin, destination, mpd_source, (group_cols), flow,
#'   weight_origin, weight_destination, weight_missing, flow_adj.
#'   Attributes:
#'     - "k" : numeric k used.
#'     - "k_calibration" : candidate k, loss and number of supported observation rows (n_scored).
#' @export
adjust_selection_rate2 <- function(mpd_od_df,
                                     coverage_df,
                                     weight_by = c("origin", "destination", "both"),
                                     group_cols = NULL,
                                     k = NULL,
                                     k_grid = seq(0.1, 5, by = 0.1),
                                     benchmark_od_df = NULL,
                                     flow_col_bench = "flow",
                                     calibration_aggregate = c("origin", "od"),
                                     clip_min = 0,
                                     clip_max = Inf,
                                     keep_cols = character()) {

  weight_by <- match.arg(weight_by)
  calibration_aggregate <- match.arg(calibration_aggregate)

  # ---- basic checks ----
  req_mpd <- c("origin", "destination", "flow", "mpd_source")
  if (!all(req_mpd %in% names(mpd_od_df))) {
    stop("`mpd_od_df` must contain: ", paste(req_mpd, collapse = ", "))
  }

  nms_cov <- names(coverage_df)
  has_new <- all(c(
    "origin",
    "origin_population", "origin_user_count",
    "destination", "destination_population", "destination_user_count",
    "mpd_source"
  ) %in% nms_cov)
  has_legacy <- all(c("origin", "population", "user_count", "mpd_source") %in% nms_cov)

  if (!has_new && !has_legacy) {
    stop(
      "`coverage_df` does not match a supported schema.\n",
      "New:    {origin, origin_population, origin_user_count, ",
      "destination, destination_population, destination_user_count, mpd_source}\n",
      "Legacy: {origin, population, user_count, mpd_source}\n",
      "Got: {", paste(nms_cov, collapse = ", "), "}"
    )
  }

  # handle group_cols
  if (!is.null(group_cols) && length(group_cols) > 0) {
    miss_mpd <- setdiff(group_cols, names(mpd_od_df))
    miss_cov <- setdiff(group_cols, names(coverage_df))
    if (length(miss_mpd)) {
      stop("`group_cols` missing in mpd_od_df: ", paste(miss_mpd, collapse = ", "))
    }
    if (length(miss_cov)) {
      stop("`group_cols` missing in coverage_df: ", paste(miss_cov, collapse = ", "))
    }
  } else {
    group_cols <- character(0)
  }
  group_syms <- rlang::syms(group_cols)

  # ---- helpers ----

  clamp_weight <- function(w) {
    w <- suppressWarnings(as.numeric(w))
    w[!(is.finite(w) & w > 0)] <- NA_real_
    w <- pmax(clip_min, pmin(w, clip_max), na.rm = FALSE)
    w
  }

  cf_fun <- function(p, k_val) {
    p <- suppressWarnings(as.numeric(p))
    p[p < 0] <- 0
    p[p > 1] <- 1

    eps <- 1e-8
    p_safe <- p
    p_safe[p_safe < eps] <- eps
    p_safe[p_safe > 1 - eps] <- 1 - eps

    num <- p_safe * expm1(-k_val)
    den <- expm1(-k_val * p_safe)

    cf <- num / den
    cf[p == 1] <- 1
    cf[p == 0] <- 0
    cf
  }

  # ---- precompute penetrations p_o, p_d that do not depend on k ----

  if (has_new) {
    cov_o_pre <- coverage_df %>%
      dplyr::transmute(
        origin     = as.character(.data$origin),
        mpd_source = .data$mpd_source,
        !!!group_syms,
        P_o        = as.numeric(.data$origin_population),
        U_o        = as.numeric(.data$origin_user_count)
      ) %>%
      dplyr::filter(is.finite(P_o), P_o > 0, is.finite(U_o), U_o >= 0) %>%
      dplyr::group_by(origin, mpd_source, !!!group_syms) %>%
      dplyr::summarise(
        P_o = sum(P_o, na.rm = TRUE),
        U_o = sum(U_o, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(p_o = pmin(pmax(U_o / P_o, 0), 1))

    cov_d_pre <- coverage_df %>%
      dplyr::transmute(
        destination = as.character(.data$destination),
        mpd_source  = .data$mpd_source,
        !!!group_syms,
        P_d         = as.numeric(.data$destination_population),
        U_d         = as.numeric(.data$destination_user_count)
      ) %>%
      dplyr::filter(is.finite(P_d), P_d > 0, is.finite(U_d), U_d >= 0) %>%
      dplyr::group_by(destination, mpd_source, !!!group_syms) %>%
      dplyr::summarise(
        P_d = sum(P_d, na.rm = TRUE),
        U_d = sum(U_d, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(p_d = pmin(pmax(U_d / P_d, 0), 1))

  } else {
    cov_base_pre <- coverage_df %>%
      dplyr::transmute(
        area       = as.character(.data$origin),
        mpd_source = .data$mpd_source,
        !!!group_syms,
        P          = as.numeric(.data$population),
        U          = as.numeric(.data$user_count)
      ) %>%
      dplyr::filter(is.finite(P), P > 0, is.finite(U), U >= 0) %>%
      dplyr::group_by(area, mpd_source, !!!group_syms) %>%
      dplyr::summarise(
        P = sum(P, na.rm = TRUE),
        U = sum(U, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(p = pmin(pmax(U / P, 0), 1))

    cov_o_pre <- cov_base_pre %>%
      dplyr::rename(origin = area, p_o = p)

    cov_d_pre <- cov_base_pre %>%
      dplyr::rename(destination = area, p_d = p)
  }

  rate_weights <- function(rates, side, p_col) {
    keys <- c(side, "mpd_source", group_cols)
    aligned <- dplyr::left_join(mpd_od_df[keys], rates[c(keys, p_col)], by = keys)
    function(k) clamp_weight(cf_fun(aligned[[p_col]], k))
  }
  origin_weight <- if (weight_by != "destination") rate_weights(cov_o_pre, "origin", "p_o")
  destination_weight <- if (weight_by != "origin") rate_weights(cov_d_pre, "destination", "p_d")
  weights <- function(k) .selection_weights(mpd_od_df, k, origin_weight, destination_weight, weight_by)

  # ---- choose k: fixed, calibrated, or default ----

  k_used <- NULL
  k_diag <- NULL

  if (!is.null(k)) {
    if (!is.numeric(k) || length(k) != 1 || !is.finite(k) || k <= 0) {
      stop("`k` must be a positive scalar.")
    }
    k_used <- as.numeric(k)

  } else if (!is.null(benchmark_od_df)) {
    if (!length(k_grid) || any(!is.finite(k_grid) | k_grid <= 0)) {
      stop("Provide finite, positive k_grid candidates.")
    }
    k_diag <- .calibrate_selection(mpd_od_df, benchmark_od_df, flow_col_bench,
                                   calibration_aggregate, k_grid, weights, group_cols)
    names(k_diag)[1] <- "k"
    k_used <- k_diag$k[which.min(k_diag$loss)]
  } else {
    # no k given, no benchmark: default
    k_used <- 1
  }

  # ---- final run with chosen k ----

  out_final <- .selection_output(mpd_od_df, weights(k_used), c(group_cols, keep_cols))
  attr(out_final, "k") <- k_used
  if (!is.null(k_diag)) {
    attr(out_final, "k_calibration") <- k_diag
  }

  out_final
}
