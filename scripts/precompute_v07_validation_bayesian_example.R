#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
})

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the debiasR package root.")
}

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else {
  library(debiasR)
}

if (!requireNamespace("rstanarm", quietly = TRUE)) {
  stop("The `rstanarm` package is required to fit the v07 Bayesian example.")
}

formula_text <- function(x) {
  paste(deparse(x, width.cutoff = 500), collapse = " ")
}

env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(default)
  }
  as.integer(value)
}

env_n_areas <- function(name, default = Inf) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(default)
  }
  if (tolower(value) %in% c("inf", "infinity", "all", "full")) {
    return(Inf)
  }
  parsed <- suppressWarnings(as.numeric(value))
  if (length(parsed) != 1L || is.na(parsed) || parsed < 1L) {
    stop("`", name, "` must be a positive number or `Inf`.")
  }
  if (is.infinite(parsed)) {
    return(Inf)
  }
  as.integer(parsed)
}

env_methods <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (identical(value, "")) {
    return(default)
  }
  methods <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  methods <- methods[nzchar(methods)]
  if (length(methods) == 0L) {
    stop("`", name, "` must name at least one Bayesian method.")
  }
  methods
}

stable_fingerprint <- function(x) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 2)
  unname(tools::md5sum(path))
}

input_fingerprint <- function(mpd_df,
                              coverage_df,
                              covariates_df,
                              distance_df) {
  stable_fingerprint(list(
    mpd_od = tibble::as_tibble(mpd_df) |>
      dplyr::arrange(.data$origin, .data$destination),
    coverage = tibble::as_tibble(coverage_df) |>
      dplyr::arrange(.data$origin, .data$destination),
    covariates = tibble::as_tibble(covariates_df) |>
      dplyr::arrange(.data$area),
    distance = tibble::as_tibble(distance_df) |>
      dplyr::arrange(.data$origin, .data$destination)
  ))
}

iter <- env_int("DEBIAS_V07_BAYES_ITER", 1000L)
chains <- env_int("DEBIAS_V07_BAYES_CHAINS", 4L)
seed_base <- env_int("DEBIAS_V07_BAYES_SEED", 20260626L)
n_areas <- env_n_areas("DEBIAS_V07_BAYES_N_AREAS", Inf)
out_dir <- Sys.getenv(
  "DEBIAS_V07_BAYES_OUT_DIR",
  unset = file.path("inst", "extdata")
)

example_data <- debiasR::debiasR_example_data(
  n_areas = n_areas,
  complete_grid = TRUE,
  geography = "lad"
)

mpd_od <- example_data$mpd_od
benchmark_od <- example_data$benchmark_od
coverage <- example_data$coverage
covariates <- example_data$covariates
distance <- example_data$distance
fingerprint <- input_fingerprint(
  mpd_df = mpd_od,
  coverage_df = coverage,
  covariates_df = covariates,
  distance_df = distance
)

expected_keys <- mpd_od |>
  select(origin, destination) |>
  distinct()
benchmark_keys <- benchmark_od |>
  select(origin, destination) |>
  distinct()

if (nrow(anti_join(expected_keys, benchmark_keys, by = c("origin", "destination"))) > 0L) {
  stop("Benchmark data do not cover all MPD validation rows.")
}
if (!isTRUE(example_data$od_audit$strict_square_support[1])) {
  stop("The v07 example requires a strict square OD grid.")
}

bayesian_specs_all <- list(
  bayes_gravity = list(
    label = "Bayesian gravity",
    role = "Gravity baseline",
    coverage_scale = "origin",
    random_intercept = "none",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance
  ),
  bayes_gravity_rural = list(
    label = "Bayesian gravity + rural",
    role = "Gravity plus one interpretable area characteristic",
    coverage_scale = "origin",
    random_intercept = "none",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d
  ),
  bayes_gravity_education = list(
    label = "Bayesian gravity + education",
    role = "Gravity plus two interpretable area characteristics",
    coverage_scale = "origin",
    random_intercept = "none",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d + per_level4_o + per_level4_d
  ),
  bayes_origin_pool = list(
    label = "Bayesian origin pooling",
    role = "Flexible origin pooling",
    coverage_scale = "origin",
    random_intercept = "origin",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d + per_level4_o + per_level4_d +
      (1 | origin)
  ),
  bayes_corridor_pool = list(
    label = "Bayesian corridor pooling",
    role = "Corridor sensitivity",
    coverage_scale = "origin",
    random_intercept = "od",
    mobility_formula = ~ log_pop_o + log_pop_d + log_distance +
      rural_pct_o + rural_pct_d + (1 | od_id)
  )
)

selected_method_ids <- env_methods(
  "DEBIAS_V07_BAYES_METHODS",
  c(
    "bayes_gravity",
    "bayes_gravity_rural",
    "bayes_gravity_education",
    "bayes_origin_pool"
  )
)
unknown_methods <- setdiff(selected_method_ids, names(bayesian_specs_all))
if (length(unknown_methods) > 0L) {
  stop(
    "Unknown `DEBIAS_V07_BAYES_METHODS` value(s): ",
    paste(unknown_methods, collapse = ", ")
  )
}
bayesian_specs <- bayesian_specs_all[selected_method_ids]

fit_one_spec <- function(method_id, spec, spec_index) {
  message("Fitting ", method_id, "...")
  start <- Sys.time()
  fit <- debiasR::adjust_multilevel_bayes(
    mpd_od_df = mpd_od,
    coverage_df = coverage,
    covariates_df = covariates,
    distance_df = distance,
    observation_model = "coverage_offset",
    target_scale = "true_flow",
    coverage_scale = spec$coverage_scale,
    mobility_formula = spec$mobility_formula,
    bias_formula = ~ 0,
    model_family = "poisson",
    model_engine = "bayesian",
    random_intercept = spec$random_intercept,
    prediction_scope = "complete_grid",
    iter = iter,
    chains = chains,
    seed = seed_base + spec_index,
    refresh = 0
  )
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  out_keys <- fit |>
    select(origin, destination) |>
    distinct()
  if (nrow(anti_join(expected_keys, out_keys, by = c("origin", "destination"))) > 0L ||
      nrow(anti_join(out_keys, expected_keys, by = c("origin", "destination"))) > 0L) {
    stop("Output rows do not match validation rows for ", method_id, ".")
  }
  if (any(!is.finite(fit$flow_adj)) || any(fit$flow_adj < 0)) {
    stop("Non-finite or negative adjusted flows for ", method_id, ".")
  }

  diagnostics <- attr(fit, "diagnostics")
  convergence <- diagnostics$convergence
  result_metadata <- attr(fit, "result_metadata")
  overall <- debiasR::validate_flow_overall(
    adj_df = fit,
    benchmark_od_df = benchmark_od,
    comparisons = "adjusted_vs_benchmark",
    return_joined = FALSE,
    method_name = method_id
  )

  adjusted <- fit |>
    as_tibble() |>
    select(
      any_of(c(
        "origin",
        "destination",
        "flow",
        "flow_adj",
        "flow_mpd_pred",
        "flow_true_pred",
        "flow_adj_mean",
        "flow_adj_median",
        "flow_adj_q2.5",
        "flow_adj_q97.5",
        "observation_probability",
        "coverage_rate_o",
        "coverage_rate_d",
        "log_observation_probability",
        "distance_km",
        "log_distance"
      ))
    ) |>
    mutate(method = method_id, .before = 1)

  display <- adjusted |>
    slice_head(n = 5)

  adjusted_validation <- adjusted |>
    select(
      method,
      origin,
      destination,
      flow,
      flow_adj
    )

  metadata <- tibble(
    method = method_id,
    method_label = spec$label,
    specification_role = spec$role,
    benchmark_used_in_fit = "No",
    coverage_scale = spec$coverage_scale,
    random_intercept = spec$random_intercept,
    mobility_formula = formula_text(spec$mobility_formula),
    resolved_formula = paste(attr(fit, "formula"), collapse = " "),
    backend = attr(fit, "backend"),
    model_engine = attr(fit, "model_engine"),
    model_family = attr(fit, "model_family"),
    target_scale = attr(fit, "target_scale"),
    observation_model = attr(fit, "observation_model"),
    prediction_scope = attr(fit, "prediction_scope"),
    iter = iter,
    chains = chains,
    seed = seed_base + spec_index,
    input_fingerprint = fingerprint,
    area_set = paste(sort(unique(mpd_od$origin)), collapse = ";"),
    geography = example_data$metadata$geography[1],
    n_areas_requested = n_areas,
    n_areas_loaded = example_data$metadata$n_areas[1],
    n_validation_rows = nrow(fit),
    n_fit_rows = result_metadata$n_fit_rows,
    n_prediction_rows = result_metadata$n_prediction_rows,
    n_mpd_zero_filled = example_data$metadata$n_mpd_zero_filled[1],
    n_benchmark_zero_filled = example_data$metadata$n_benchmark_zero_filled[1],
    runtime_seconds = attr(fit, "runtime_seconds"),
    elapsed_sec = elapsed,
    convergence_status = convergence$status,
    rhat_max = convergence$rhat_max,
    n_eff_min = convergence$n_eff_min,
    diagnostic_note = dplyr::case_when(
      is.finite(convergence$rhat_max) && convergence$rhat_max > 1.1 ~ "Inspect: max R-hat above 1.1",
      is.finite(convergence$n_eff_min) && convergence$n_eff_min < 50 ~ "Inspect: low minimum ESS",
      TRUE ~ "No headline warning"
    ),
    mae = overall$mae,
    rmse = overall$rmse,
    pearson_r = overall$pearson_r,
    spearman_rho = overall$spearman_rho
  )

  list(
    adjusted = adjusted_validation,
    display = display,
    metadata = metadata
  )
}

outputs <- Map(
  fit_one_spec,
  names(bayesian_specs),
  bayesian_specs,
  seq_along(bayesian_specs)
)

adjusted_all <- bind_rows(lapply(outputs, `[[`, "adjusted"))
display_all <- bind_rows(lapply(outputs, `[[`, "display"))
metadata_all <- bind_rows(lapply(outputs, `[[`, "metadata"))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  adjusted_all,
  file.path(out_dir, "v07-validation-bayesian-adjusted.csv"),
  row.names = FALSE
)
utils::write.csv(
  display_all,
  file.path(out_dir, "v07-validation-bayesian-display.csv"),
  row.names = FALSE
)
utils::write.csv(
  metadata_all,
  file.path(out_dir, "v07-validation-bayesian-metadata.csv"),
  row.names = FALSE
)

message("Wrote v07 Bayesian validation output files to ", out_dir, ".")
