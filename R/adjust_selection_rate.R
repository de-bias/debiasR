#' Selection-rate weighting (Chi et al. with r_t calibration)
#'
#' Implements Chi et al.-style selection weights.
#'
#' Notation used throughout:
#' \itemize{
#'   \item \eqn{P_i^{(O)}, P_j^{(D)}}: population at origin \eqn{i} and destination \eqn{j}
#'   \item \eqn{U_i^{(O)}, U_j^{(D)}}: active users at origin \eqn{i} and destination \eqn{j}
#'   \item \eqn{p_i^{(O)} = U_i^{(O)}/P_i^{(O)}} and \eqn{p_j^{(D)} = U_j^{(D)}/P_j^{(D)}}: penetration
#'   \item \eqn{I_i, I_j \in [0,1]}: normalized selected covariate
#'   \item \eqn{r_t > 0}: global selection parameter
#'   \item \eqn{F_{ij}^{mpd}} and \eqn{F_{ij}^{adj}}: observed and adjusted flows
#' }
#'
#' Origin-side weight:
#' \deqn{w_i^{(O)}(r_t) = \frac{1}{I_i p_i^{(O)} + (1 - I_i) r_t}}
#'
#' Destination-side weight:
#' \deqn{w_j^{(D)}(r_t) = \frac{1}{I_j p_j^{(D)} + (1 - I_j) r_t}}
#'
#' Flow adjustment:
#' \deqn{F_{ij}^{adj} = F_{ij}^{mpd} \times w_{ij}}
#' with \eqn{w_{ij} = w_i^{(O)}} for \code{weight_by = "origin"},
#' \eqn{w_{ij} = w_j^{(D)}} for \code{weight_by = "destination"}, and
#' \eqn{w_{ij} = \sqrt{w_i^{(O)} w_j^{(D)}}} for \code{weight_by = "both"}.
#'
#' with flexible area-level covariates and optional calibration of r_t
#' against benchmark OD flows.
#'
#' Coverage formats:
#'   NEW:
#'     origin, origin_population, origin_user_count,
#'     destination, destination_population, destination_user_count, mpd_source
#'   LEGACY:
#'     origin, population, user_count, mpd_source
#'
#' Optional covariates:
#'   covariates_df with:
#'     area,
#'     a numeric area-level covariate column (specified via `covariate_col`),
#'     optional mpd_source.
#'
#' r_t handling:
#'   - If `r_global` is provided: use it directly.
#'   - Else if `benchmark_od_df` is provided: calibrate r_t by grid search
#'     to minimize the sum of absolute errors, reproducing Chi et al.'s idea.
#'     By default aggregates by origin:
#'       \deqn{r_t^\ast = \arg\min_{r \in \mathcal{R}} \sum_i
#'       \left|\hat{M}_i(r) - M_i^{bench}\right|}
#'   - Else: fall back to descriptive r_t = sum(U) / sum(P) (transparent).
#'
#' @param mpd_od_df Data frame with: origin, destination, flow, mpd_source.
#' @param coverage_df Data frame in NEW or LEGACY schema.
#' @param covariates_df Optional data frame with columns:
#'   - area
#'   - a numeric area-level covariate column
#'   - optional mpd_source
#' @param covariate_col Optional name of the numeric area-level covariate column
#'   in `covariates_df`. The selected covariate is normalized to the 0-1 range
#'   internally before weights are computed. If NULL, auto-detect among legacy
#'   income-like names:
#'   `income_norm, gni_pc, income, inc, gni, income_2000, income_2010`.
#' @param weight_by "origin", "destination", or "both".
#' @param r_global Optional scalar r_t. Overrides calibration if given.
#' @param benchmark_od_df Optional OD benchmark for calibrating r_t.
#'   Must contain: origin, destination, and a flow column.
#' @param flow_col_bench Name of benchmark flow column in `benchmark_od_df`.
#'   Default "flow".
#' @param r_grid Numeric vector of candidate r_t values for calibration.
#'   Default `seq(0, 3, by = 0.01)` as in Chi et al.
#' @param calibration_aggregate "origin" (default, Chi et al. style) or "od".
#' @param clip_min Lower bound used to clamp weights. Default 0.
#' @param clip_max Upper bound used to clamp weights. Default Inf.
#' @param keep_cols Extra columns from `mpd_od_df` to keep.
#' @param ... Deprecated arguments. `income_col` is accepted here as a legacy
#'   alias for `covariate_col`.
#'
#' @details Calibration uses the same matched observations with usable coverage for
#' every candidate. An empty comparison raises an error. A candidate that
#' cannot predict all supported observations receives infinite loss.
#'
#' @return Tibble with:
#'   origin, destination, mpd_source, flow,
#'   weight_origin, weight_destination, weight_missing, flow_adj.
#'   Attributes:
#'     - "r_global": numeric r_t used.
#'     - "r_calibration": candidate r, loss and number of supported observation rows (n_scored).
#' @export
adjust_selection_rate <- function(mpd_od_df,
                                   coverage_df,
                                   covariates_df = NULL,
                                   covariate_col = NULL,
                                   weight_by = c("origin", "destination", "both"),
                                   r_global = NULL,
                                   benchmark_od_df = NULL,
                                   flow_col_bench = "flow",
                                   r_grid = seq(0, 3, by = 0.01),
                                   calibration_aggregate = c("origin", "od"),
                                   clip_min = 0,
                                   clip_max = Inf,
                                   keep_cols = character(),
                                   ...) {
  weight_by <- match.arg(weight_by)
  calibration_aggregate <- match.arg(calibration_aggregate)

  dots <- list(...)
  legacy_income_col <- dots$income_col
  dots$income_col <- NULL
  if (length(dots) > 0L) {
    stop("Unused argument(s): ", paste(names(dots), collapse = ", "))
  }
  if (!is.null(covariate_col) && !is.null(legacy_income_col) &&
      !identical(covariate_col, legacy_income_col)) {
    stop("Use only one of `covariate_col` or legacy `income_col`.")
  }
  if (is.null(covariate_col) && !is.null(legacy_income_col)) {
    covariate_col <- legacy_income_col
  }

  # -------- basic checks --------
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

  # -------- small helpers --------
  clamp_weight <- function(w) {
    w <- suppressWarnings(as.numeric(w))
    w[!(is.finite(w) & w > 0)] <- NA_real_
    w <- pmax(clip_min, pmin(w, clip_max), na.rm = FALSE)
    w
  }

  normalize01 <- function(v) {
    v <- suppressWarnings(as.numeric(v))
    v[!is.finite(v)] <- NA_real_
    if (!any(is.finite(v))) return(rep(NA_real_, length(v)))
    v_min <- min(v, na.rm = TRUE)
    v_max <- max(v, na.rm = TRUE)
    if (v_min >= 0 && v_max <= 1) return(v)
    if (v_max > v_min) (v - v_min) / (v_max - v_min) else rep(NA_real_, length(v))
  }

  infer_covariate_from_coverage <- function(df, prefix = NULL) {
    cand <- c(
      if (!is.null(prefix)) paste0(prefix, c("income", "gni_pc", "gni")),
      "income", "gni_pc", "gni"
    )
    cand <- cand[cand %in% names(df)]
    if (length(cand) == 0) return(rep(1, nrow(df)))
    inc <- normalize01(df[[cand[1]]])
    ifelse(is.finite(inc), inc, 1)
  }

  # -------- build normalized selected covariate (if provided) --------
  covariate_values <- NULL
  if (!is.null(covariates_df)) {
    if (!"area" %in% names(covariates_df)) {
      stop("`covariates_df` must contain column `area`.")
    }

    if (!is.null(covariate_col)) {
      if (!covariate_col %in% names(covariates_df)) {
        stop("`covariate_col` = '", covariate_col, "' not found in `covariates_df`.")
      }
      covariate_col_use <- covariate_col
    } else {
      cand_auto <- c("income_norm", "gni_pc", "income", "inc", "gni",
                     "income_2000", "income_2010")
      covariate_col_use <- cand_auto[cand_auto %in% names(covariates_df)][1]
      if (is.na(covariate_col_use)) covariate_col_use <- NULL
    }

    if (!is.null(covariate_col_use)) {
      covariate_norm <- normalize01(covariates_df[[covariate_col_use]])
      covariate_values <- covariates_df %>%
        dplyr::transmute(
          area = as.character(.data$area),
          covariate_norm = covariate_norm
        )
      if ("mpd_source" %in% names(covariates_df)) {
        covariate_values$mpd_source <- covariates_df$mpd_source
      }
    }
  }

  # =====================================================================
  # Precompute side-specific quantities that do not depend on r_t
  # =====================================================================

  if (has_new) {
    # ORIGIN side
    cov_o_pre <- coverage_df %>%
      dplyr::transmute(
        origin     = as.character(.data$origin),
        mpd_source = .data$mpd_source,
        P_o        = as.numeric(.data$origin_population),
        U_o        = as.numeric(.data$origin_user_count)
      ) %>%
      dplyr::filter(is.finite(P_o), P_o > 0, is.finite(U_o), U_o > 0)

    if (!is.null(covariate_values)) {
      if ("mpd_source" %in% names(covariate_values)) {
        cov_o_pre <- cov_o_pre %>%
          dplyr::left_join(covariate_values,
                           by = c("origin" = "area", "mpd_source"))
      } else {
        cov_o_pre <- cov_o_pre %>%
          dplyr::left_join(covariate_values, by = c("origin" = "area"))
      }
      Income_o <- ifelse(is.finite(cov_o_pre$covariate_norm),
                         cov_o_pre$covariate_norm, NA_real_)
    } else {
      Income_o <- infer_covariate_from_coverage(cov_o_pre, prefix = "origin_")
    }
    Income_o[!is.finite(Income_o)] <- 1
    cov_o_pre$Income_o <- Income_o
    cov_o_pre$pen_o <- cov_o_pre$U_o / cov_o_pre$P_o

    # DESTINATION side
    cov_d_pre <- coverage_df %>%
      dplyr::transmute(
        destination = as.character(.data$destination),
        mpd_source  = .data$mpd_source,
        P_d         = as.numeric(.data$destination_population),
        U_d         = as.numeric(.data$destination_user_count)
      ) %>%
      dplyr::filter(is.finite(P_d), P_d > 0, is.finite(U_d), U_d > 0)

    if (!is.null(covariate_values)) {
      if ("mpd_source" %in% names(covariate_values)) {
        cov_d_pre <- cov_d_pre %>%
          dplyr::left_join(covariate_values,
                           by = c("destination" = "area", "mpd_source"))
      } else {
        cov_d_pre <- cov_d_pre %>%
          dplyr::left_join(covariate_values, by = c("destination" = "area"))
      }
      Income_d <- ifelse(is.finite(cov_d_pre$covariate_norm),
                         cov_d_pre$covariate_norm, NA_real_)
    } else {
      Income_d <- infer_covariate_from_coverage(cov_d_pre, prefix = "destination_")
    }
    Income_d[!is.finite(Income_d)] <- 1
    cov_d_pre$Income_d <- Income_d
    cov_d_pre$pen_d <- cov_d_pre$U_d / cov_d_pre$P_d

  } else {
    # LEGACY schema
    cov_base_pre <- coverage_df %>%
      dplyr::transmute(
        area       = as.character(.data$origin),
        mpd_source = .data$mpd_source,
        P          = as.numeric(.data$population),
        U          = as.numeric(.data$user_count)
      ) %>%
      dplyr::filter(is.finite(P), P > 0, is.finite(U), U > 0)

    if (!is.null(covariate_values)) {
      if ("mpd_source" %in% names(covariate_values)) {
        cov_base_pre <- cov_base_pre %>%
          dplyr::left_join(covariate_values, by = c("area", "mpd_source"))
      } else {
        cov_base_pre <- cov_base_pre %>%
          dplyr::left_join(covariate_values, by = "area")
      }
      Income <- ifelse(is.finite(cov_base_pre$covariate_norm),
                       cov_base_pre$covariate_norm, NA_real_)
    } else {
      Income <- infer_covariate_from_coverage(cov_base_pre, prefix = NULL)
    }
    Income[!is.finite(Income)] <- 1
    cov_base_pre$Income <- Income
    cov_base_pre$pen <- cov_base_pre$U / cov_base_pre$P
  }

  if (has_new) {
    origin_rates <- dplyr::transmute(cov_o_pre, origin, mpd_source, income = .data$Income_o, pen = .data$pen_o)
    destination_rates <- dplyr::transmute(cov_d_pre, destination, mpd_source, income = .data$Income_d, pen = .data$pen_d)
  } else {
    origin_rates <- dplyr::transmute(cov_base_pre, origin = area, mpd_source, income = .data$Income, pen = .data$pen)
    destination_rates <- dplyr::rename(origin_rates, destination = origin)
  }
  rate_weights <- function(rates, side) {
    keys <- c(side, "mpd_source")
    rates <- dplyr::distinct(rates)
    if (anyDuplicated(rates[keys])) stop("Conflicting selection rates for the same ", side, " and source.")
    aligned <- dplyr::left_join(mpd_od_df[keys], rates, by = keys)
    function(r) clamp_weight(1 / (aligned$income * aligned$pen + (1 - aligned$income) * r))
  }
  origin_weight <- if (weight_by != "destination") rate_weights(origin_rates, "origin")
  destination_weight <- if (weight_by != "origin") rate_weights(destination_rates, "destination")
  weights <- function(r) .selection_weights(mpd_od_df, r, origin_weight, destination_weight, weight_by)
  r_used <- NULL
  calib_diag <- NULL

  # Case 1: user supplies r_global directly
  if (!is.null(r_global)) {
    r_used <- as.numeric(r_global[1])

  } else if (!is.null(benchmark_od_df)) {
    if (!length(r_grid) || any(!is.finite(r_grid) | r_grid < 0)) {
      stop("Provide finite, non-negative r_grid candidates.")
    }
    calib_diag <- .calibrate_selection(mpd_od_df, benchmark_od_df, flow_col_bench,
                                       calibration_aggregate, r_grid, weights)
    names(calib_diag)[1] <- "r"
    r_used <- calib_diag$r[which.min(calib_diag$loss)]
  } else {
    # Case 3: fallback descriptive r_t (transparent)
    if (has_new) {
      if (weight_by == "origin") {
        r_used <- sum(cov_o_pre$U_o, na.rm = TRUE) /
          sum(cov_o_pre$P_o, na.rm = TRUE)
      } else if (weight_by == "destination") {
        r_used <- sum(cov_d_pre$U_d, na.rm = TRUE) /
          sum(cov_d_pre$P_d, na.rm = TRUE)
      } else { # both
        r_used <- (sum(cov_o_pre$U_o, na.rm = TRUE) +
                     sum(cov_d_pre$U_d, na.rm = TRUE)) /
          (sum(cov_o_pre$P_o, na.rm = TRUE) +
             sum(cov_d_pre$P_d, na.rm = TRUE))
      }
    } else {
      r_used <- sum(cov_base_pre$U, na.rm = TRUE) /
        sum(cov_base_pre$P, na.rm = TRUE)
    }
  }

  # =====================================================================
  # Final run with chosen r_t
  # =====================================================================

  out_final <- .selection_output(mpd_od_df, weights(r_used), keep_cols)
  attr(out_final, "r_global") <- r_used
  if (!is.null(calib_diag)) {
    attr(out_final, "r_calibration") <- calib_diag
  }

  out_final
}

.selection_weights <- function(data, parameter, origin_weight, destination_weight, weight_by) {
  missing <- rep(NA_real_, nrow(data))
  origin <- if (is.null(origin_weight)) missing else origin_weight(parameter)
  destination <- if (is.null(destination_weight)) missing else destination_weight(parameter)
  applied <- switch(weight_by, origin = origin, destination = destination,
                    both = sqrt(origin * destination))
  list(weight_origin = origin, weight_destination = destination,
       weight_missing = !is.finite(applied), flow_adj = data$flow * applied)
}

.selection_output <- function(data, weights, keep_cols) {
  columns <- intersect(unique(c("origin", "destination", "mpd_source", keep_cols, "flow")), names(data))
  out <- tibble::as_tibble(data[columns])
  for (column in names(weights)) out[[column]] <- weights[[column]]
  out
}

.calibrate_selection <- function(data, benchmark, flow_col, aggregate, grid, weights, strata = character()) {
  .validate_required_columns(benchmark, c("origin", "destination", flow_col), "benchmark_od_df")
  keys <- .flow_reference_keys(data, benchmark, strata)
  reference <- benchmark[keys]
  reference$.benchmark <- as.numeric(benchmark[[flow_col]])
  observations <- data[keys]
  observations$.row <- seq_len(nrow(data))
  paired <- dplyr::inner_join(observations, reference, by = keys)
  # At parameter one both formulas have defined weights wherever coverage is usable.
  supported <- is.finite(weights(1)$flow_adj[paired$.row]) & is.finite(paired$.benchmark)
  paired <- paired[supported, , drop = FALSE]
  if (!nrow(paired)) stop("Calibration failed: no usable overlap between adjusted and benchmark flows.")
  loss <- vapply(grid, function(value) {
    difference <- weights(value)$flow_adj[paired$.row] - paired$.benchmark
    if (any(!is.finite(difference))) return(Inf)
    if (aggregate == "origin") difference <- rowsum(difference, paired$origin, reorder = FALSE)
    sum(abs(difference))
  }, numeric(1))
  if (!any(is.finite(loss))) stop("No candidate has finite predictions on the common calibration support.")
  data.frame(parameter = grid, loss = loss, n_scored = nrow(paired))
}
