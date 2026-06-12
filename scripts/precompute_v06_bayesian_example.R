#!/usr/bin/env Rscript

# Regenerate the precomputed Bayesian output displayed in
# vignettes/v06-adjusting-biases.qmd. This script is intentionally separate from
# vignette rendering so routine documentation updates do not rerun MCMC.

repo_root <- normalizePath(getwd(), mustWork = TRUE)
description_file <- file.path(repo_root, "DESCRIPTION")

if (!file.exists(description_file)) {
  stop(
    "Run this script from the debiasR repository root.",
    call. = FALSE
  )
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop(
    "Package 'devtools' is required to load the local debiasR source tree.",
    call. = FALSE
  )
}

if (!requireNamespace("debiasRdata", quietly = TRUE)) {
  stop(
    "Package 'debiasRdata' is required to regenerate the vignette artifact.",
    call. = FALSE
  )
}

devtools::load_all(repo_root, quiet = TRUE)

example_data <- debiasR_example_data(n_areas = 25, complete_grid = TRUE)

mpd_s1 <- example_data$mpd_od |>
  dplyr::mutate(
    mpd_source = "operator_a",
    mpd_time = "2021_q1"
  )

coverage_s1 <- example_data$coverage |>
  dplyr::mutate(
    mpd_source = "operator_a",
    mpd_time = "2021_q1"
  )

adj_multilevel <- adjust_multilevel_bayes(
  mpd_od_df = mpd_s1,
  coverage_df = coverage_s1,
  covariates_df = example_data$covariates,
  distance_df = example_data$distance,
  mobility_formula = ~ rural_pct_o + rural_pct_d + log_distance,
  bias_formula = ~ bias_e_origin,
  target_scale = "true_flow",
  observation_model = "coverage_offset",
  coverage_scale = "origin",
  model_engine = "bayesian",
  scenario = "s1",
  source_col = "mpd_source",
  time_col = "mpd_time",
  repeated_observation = "none",
  prediction_scope = "complete_grid",
  random_intercept = "none",
  model_family = "poisson",
  flow_adj_summary = "median",
  include_flow_adj_draws = TRUE,
  iter = 1000,
  chains = 2,
  seed = 123,
  refresh = 0
)

benchmark_flow_lookup <- example_data$benchmark_od |>
  dplyr::select(
    origin,
    destination,
    flow_benchmark = flow
  )

flow_adj_draws <- attr(adj_multilevel, "flow_adj_draws")

flow_adj_median <- apply(flow_adj_draws, 2, stats::median)
flow_adj_mean <- colMeans(flow_adj_draws)

comparison <- adj_multilevel |>
  dplyr::left_join(
    benchmark_flow_lookup,
    by = c("origin", "destination")
  ) |>
  dplyr::mutate(
    flow_mpd_adjusted_median = flow_adj_median,
    flow_mpd_adjusted_mean = flow_adj_mean,
    flow_mpd_pred_median = flow_mpd_adjusted_median * observation_probability,
    flow_mpd_pred_mean = flow_mpd_adjusted_mean * observation_probability
  ) |>
  dplyr::select(
    origin,
    destination,
    flow_mpd_raw = flow,
    flow_mpd_adjusted_median,
    flow_mpd_adjusted_mean,
    flow_benchmark,
    observation_probability,
    flow_mpd_pred_median,
    flow_mpd_pred_mean
  ) |>
  dplyr::slice_head(n = 5)

output_file <- file.path(
  repo_root,
  "inst",
  "extdata",
  "v06-bayesian-s1-comparison.csv"
)

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(comparison, output_file, row.names = FALSE, na = "")

metadata_fields <- c(
  "model_engine",
  "backend",
  "target_scale",
  "observation_model",
  "coverage_scale",
  "offset_column",
  "scenario",
  "repeated_observation",
  "n_sources",
  "n_time_periods",
  "prediction_scope",
  "flow_adj_summary"
)

metadata <- attr(adj_multilevel, "result_metadata")[metadata_fields]
metadata_table <- data.frame(
  field = names(metadata),
  value = vapply(
    metadata,
    function(x) paste(as.character(x), collapse = ", "),
    character(1)
  ),
  stringsAsFactors = FALSE
)

metadata_file <- file.path(
  repo_root,
  "inst",
  "extdata",
  "v06-bayesian-s1-metadata.csv"
)

utils::write.csv(metadata_table, metadata_file, row.names = FALSE, na = "")

model_terms <- attr(adj_multilevel, "model_terms")
terms_table <- do.call(
  rbind,
  lapply(names(model_terms), function(component) {
    values <- model_terms[[component]]
    if (is.null(values) || length(values) == 0) {
      values <- NA_character_
    }
    data.frame(
      component = component,
      value = paste(as.character(values), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
)

terms_file <- file.path(
  repo_root,
  "inst",
  "extdata",
  "v06-bayesian-s1-model-terms.csv"
)

utils::write.csv(terms_table, terms_file, row.names = FALSE, na = "")

message("Wrote ", output_file)
message("Wrote ", metadata_file)
message("Wrote ", terms_file)
