#' Fit multiple adjustment methods
#'
#' Runs the main \code{debiasR} adjustment methods on the same set of
#' origin-destination inputs and returns a named list of adjusted-flow tables.
#' This is useful when users want to compare methods with the same MPD flows,
#' coverage table, benchmark flows, covariates, and distances.
#'
#' By default, the function fits the five deterministic adjustment methods and
#' the multilevel adjustment path. The multilevel path uses
#' \code{model_engine = "frequentist"} by default so broad method comparisons
#' are fast to run. Set \code{multilevel_engine = "bayesian"}, or pass
#' \code{multilevel_args = list(model_engine = "bayesian", ...)}, when you want
#' posterior summaries from \code{adjust_multilevel_bayes()}.
#'
#' @param mpd_od_df Data frame of MPD flows with at least
#'   \code{origin}, \code{destination}, and \code{flow}.
#' @param coverage_df Data frame with at least \code{origin},
#'   \code{population}, and \code{user_count}.
#' @param benchmark_od_df Data frame of benchmark OD flows with at least
#'   \code{origin}, \code{destination}, and \code{flow}. Required for the
#'   benchmark-calibrated methods.
#' @param covariates_df Area-level covariates with at least \code{area} and the
#'   selected \code{covariate_col}. Required for selection-rate and multilevel
#'   methods.
#' @param distance_df OD distance data with \code{origin}, \code{destination},
#'   and \code{distance_km}. Required when \code{"multilevel_bayes"} is included.
#' @param methods Character vector of methods to fit. Use \code{"all"} for all
#'   supported methods. Supported values are \code{"inverse_penetration"},
#'   \code{"selection_rate"}, \code{"selection_rate2"},
#'   \code{"raking_ratio"}, \code{"coefficient"}, and
#'   \code{"multilevel_bayes"}.
#' @param covariate_col Name of the area-level covariate used by the selection
#'   and default multilevel formulas. Default \code{"rural_pct"}.
#' @param multilevel_engine Fitting engine for \code{adjust_multilevel_bayes()}
#'   when \code{"multilevel_bayes"} is included. Default \code{"frequentist"}.
#' @param inverse_penetration_args,selection_rate_args,selection_rate2_args,raking_ratio_args,coefficient_args,multilevel_args
#'   Named lists of additional arguments passed to the corresponding
#'   adjustment functions. Values in these lists override the defaults used by
#'   \code{adjust_all_methods()}.
#'
#' @return A named list of tibbles. Each element is the direct output returned
#'   by one adjustment function and contains a standard \code{flow_adj} column.
#'
#' @examples
#' results <- adjust_all_methods(
#'   mpd_od_df = simulated_mpd.od,
#'   coverage_df = simulated_coverage,
#'   benchmark_od_df = simulated_benchmark.od,
#'   covariates_df = simulated_covariates,
#'   distance_df = simulated_distance,
#'   covariate_col = "income_norm",
#'   methods = c("inverse_penetration", "coefficient")
#' )
#' names(results)
#'
#' @export
adjust_all_methods <- function(mpd_od_df,
                               coverage_df,
                               benchmark_od_df = NULL,
                               covariates_df = NULL,
                               distance_df = NULL,
                               methods = "all",
                               covariate_col = "rural_pct",
                               multilevel_engine = c("frequentist", "bayesian"),
                               inverse_penetration_args = list(),
                               selection_rate_args = list(),
                               selection_rate2_args = list(),
                               raking_ratio_args = list(),
                               coefficient_args = list(),
                               multilevel_args = list()) {
  multilevel_engine <- match.arg(multilevel_engine)

  valid_methods <- c(
    "inverse_penetration",
    "selection_rate",
    "selection_rate2",
    "raking_ratio",
    "coefficient",
    "multilevel_bayes"
  )

  if (length(methods) == 1L && identical(methods, "all")) {
    methods <- valid_methods
  }
  bad_methods <- setdiff(methods, valid_methods)
  if (length(bad_methods) > 0L) {
    stop(
      "`methods` contains unsupported value(s): ",
      paste(bad_methods, collapse = ", "),
      call. = FALSE
    )
  }
  methods <- unique(methods)
  if (length(methods) == 0L) {
    stop("`methods` must select at least one adjustment method.", call. = FALSE)
  }

  .adjust_all_require_cols(mpd_od_df, c("origin", "destination", "flow"), "mpd_od_df")
  .adjust_all_require_cols(coverage_df, c("origin", "population", "user_count"), "coverage_df")

  needs_benchmark <- any(methods %in% c(
    "selection_rate", "selection_rate2", "raking_ratio", "coefficient"
  ))
  if (needs_benchmark) {
    .adjust_all_require_cols(
      benchmark_od_df,
      c("origin", "destination", "flow"),
      "benchmark_od_df"
    )
  }

  needs_covariates <- any(methods %in% c("selection_rate", "multilevel_bayes"))
  if (needs_covariates) {
    .adjust_all_require_cols(covariates_df, c("area", covariate_col), "covariates_df")
  }

  if ("multilevel_bayes" %in% methods) {
    if (is.null(distance_df)) {
      stop(
        "`distance_df` is required when `methods` includes \"multilevel_bayes\".",
        call. = FALSE
      )
    }
    .adjust_all_require_cols(
      distance_df,
      c("origin", "destination", "distance_km"),
      "distance_df"
    )
  }

  out <- list()

  if ("inverse_penetration" %in% methods) {
    args <- .adjust_all_merge_args(
      list(
        mpd_od_df = mpd_od_df,
        coverage_df = coverage_df,
        weight_by = "both"
      ),
      inverse_penetration_args
    )
    out$inverse_penetration <- do.call(adjust_inverse_penetration, args)
  }

  if ("selection_rate" %in% methods) {
    args <- .adjust_all_merge_args(
      list(
        mpd_od_df = mpd_od_df,
        coverage_df = coverage_df,
        covariates_df = covariates_df,
        covariate_col = covariate_col,
        weight_by = "origin",
        benchmark_od_df = benchmark_od_df,
        calibration_aggregate = "origin"
      ),
      selection_rate_args
    )
    out$selection_rate <- do.call(adjust_selection_rate, args)
  }

  if ("selection_rate2" %in% methods) {
    args <- .adjust_all_merge_args(
      list(
        mpd_od_df = mpd_od_df,
        coverage_df = coverage_df,
        weight_by = "origin",
        benchmark_od_df = benchmark_od_df,
        calibration_aggregate = "origin"
      ),
      selection_rate2_args
    )
    out$selection_rate2 <- do.call(adjust_selection_rate2, args)
  }

  if ("raking_ratio" %in% methods) {
    args <- .adjust_all_merge_args(
      list(
        mpd_od_df = mpd_od_df,
        benchmark_od_df = benchmark_od_df
      ),
      raking_ratio_args
    )
    out$raking_ratio <- do.call(adjust_raking_ratio, args)
  }

  if ("coefficient" %in% methods) {
    args <- .adjust_all_merge_args(
      list(
        mpd_od_df = mpd_od_df,
        benchmark_od_df = benchmark_od_df,
        model_family = "ols",
        level = "od"
      ),
      coefficient_args
    )
    out$coefficient <- do.call(adjust_coefficient, args)
  }

  if ("multilevel_bayes" %in% methods) {
    default_formula <- stats::as.formula(paste(
      "flow ~",
      paste0(covariate_col, "_o"),
      "+",
      paste0(covariate_col, "_d"),
      "+ log_distance + bias_e_origin"
    ))

    ml_defaults <- list(
      covariates_df = covariates_df,
      distance_df = distance_df,
      formula = default_formula,
      model_engine = multilevel_engine,
      scenario = "s1",
      source_col = "mpd_source",
      time_col = "mpd_time",
      repeated_observation = "none",
      prediction_scope = "complete_grid",
      random_intercept = "none",
      model_family = "poisson",
      flow_adj_summary = "median"
    )
    ml_args <- .adjust_all_merge_args(ml_defaults, multilevel_args)

    ml_inputs <- .adjust_all_prepare_multilevel_inputs(
      mpd_od_df = mpd_od_df,
      coverage_df = coverage_df,
      source_col = ml_args$source_col,
      time_col = ml_args$time_col
    )

    ml_args <- c(
      list(
        mpd_od_df = ml_inputs$mpd_od_df,
        coverage_df = ml_inputs$coverage_df
      ),
      ml_args
    )
    out$multilevel_bayes <- do.call(adjust_multilevel_bayes, ml_args)
  }

  attr(out, "methods") <- names(out)
  class(out) <- c("debiasR_adjustment_set", class(out))
  out
}

.adjust_all_merge_args <- function(defaults, overrides) {
  if (is.null(overrides)) {
    overrides <- list()
  }
  if (!is.list(overrides)) {
    stop("Method-specific argument overrides must be supplied as named lists.", call. = FALSE)
  }
  utils::modifyList(defaults, overrides, keep.null = TRUE)
}

.adjust_all_require_cols <- function(x, cols, arg_name) {
  if (is.null(x) || !is.data.frame(x)) {
    stop("`", arg_name, "` must be a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(cols, names(x))
  if (length(missing_cols) > 0L) {
    stop(
      "`", arg_name, "` must contain: ",
      paste(cols, collapse = ", "),
      ". Missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.adjust_all_prepare_multilevel_inputs <- function(mpd_od_df,
                                                  coverage_df,
                                                  source_col,
                                                  time_col) {
  mpd_out <- mpd_od_df
  coverage_out <- coverage_df

  if (!is.null(source_col)) {
    if (!source_col %in% names(mpd_out)) {
      mpd_out[[source_col]] <- "source_1"
    }
    if (!source_col %in% names(coverage_out)) {
      coverage_out[[source_col]] <- "source_1"
    }
  }

  if (!is.null(time_col)) {
    if (!time_col %in% names(mpd_out)) {
      mpd_out[[time_col]] <- "time_1"
    }
    if (!time_col %in% names(coverage_out)) {
      coverage_out[[time_col]] <- "time_1"
    }
  }

  list(
    mpd_od_df = mpd_out,
    coverage_df = coverage_out
  )
}
