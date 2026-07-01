#!/usr/bin/env Rscript

# Regenerate the real-data evidence output files displayed in
# vignettes/v09-advanced-bayesian-adjustment.qmd.
#
# This script deliberately does not rerun MCMC. The compact display rows are
# derived from the stored v07 Bayesian coverage-offset output, while the
# full empirical approval summaries are traceable to notes/project-management.

suppressPackageStartupMessages({
  library(dplyr)
})

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the debiasR package root.", call. = FALSE)
}

out_dir <- file.path("inst", "extdata")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

v07_adjusted_file <- file.path(out_dir, "v07-validation-bayesian-adjusted.csv")
v07_metadata_file <- file.path(out_dir, "v07-validation-bayesian-metadata.csv")

if (!file.exists(v07_adjusted_file) || !file.exists(v07_metadata_file)) {
  stop(
    "Missing v07 Bayesian output files. Run ",
    "`scripts/precompute_v07_validation_bayesian_example.R` first.",
    call. = FALSE
  )
}

v07_adjusted <- tibble::as_tibble(
  utils::read.csv(v07_adjusted_file, stringsAsFactors = FALSE)
)
v07_metadata <- tibble::as_tibble(
  utils::read.csv(v07_metadata_file, stringsAsFactors = FALSE)
)

origin_pool_metadata <- v07_metadata |>
  filter(.data$method == "bayes_origin_pool") |>
  slice(1)

if (nrow(origin_pool_metadata) != 1L) {
  stop("Could not find the bayes_origin_pool metadata row.", call. = FALSE)
}

compact_output <- v07_adjusted |>
  filter(.data$method == "bayes_origin_pool") |>
  slice_head(n = 5) |>
  transmute(
    origin = .data$origin,
    destination = .data$destination,
    flow = .data$flow,
    flow_adj = .data$flow_adj,
    flow_mpd_pred = .data$flow_mpd_pred,
    flow_true_pred = .data$flow_true_pred,
    flow_adj_mean = .data$flow_adj_mean,
    flow_adj_median = .data$flow_adj_median,
    flow_adj_q2.5 = .data$flow_adj_q2.5,
    flow_adj_q97.5 = .data$flow_adj_q97.5,
    observation_probability = .data$observation_probability,
    coverage_rate_o = .data$coverage_rate_o,
    coverage_rate_d = .data$coverage_rate_d,
    distance_km = .data$distance_km,
    log_distance = .data$log_distance
  )

metadata_diagnostics <- tibble::tribble(
  ~object, ~attribute, ~field, ~value,
  "coverage_offset_compact_lad", "attr(result, \"result_metadata\")", "model_engine", origin_pool_metadata$model_engine,
  "coverage_offset_compact_lad", "attr(result, \"result_metadata\")", "backend", origin_pool_metadata$backend,
  "coverage_offset_compact_lad", "attr(result, \"result_metadata\")", "model_family", origin_pool_metadata$model_family,
  "coverage_offset_compact_lad", "attr(result, \"result_metadata\")", "target_scale", origin_pool_metadata$target_scale,
  "coverage_offset_compact_lad", "attr(result, \"result_metadata\")", "observation_model", origin_pool_metadata$observation_model,
  "coverage_offset_compact_lad", "attr(result, \"result_metadata\")", "prediction_scope", origin_pool_metadata$prediction_scope,
  "coverage_offset_compact_lad", "attr(result, \"result_metadata\")", "coverage_scale", origin_pool_metadata$coverage_scale,
  "coverage_offset_compact_lad", "attr(result, \"result_metadata\")", "random_intercept", origin_pool_metadata$random_intercept,
  "coverage_offset_compact_lad", "attr(result, \"result_metadata\")", "n_fit_rows", as.character(origin_pool_metadata$n_fit_rows),
  "coverage_offset_compact_lad", "attr(result, \"result_metadata\")", "n_prediction_rows", as.character(origin_pool_metadata$n_prediction_rows),
  "coverage_offset_compact_lad", "attr(result, \"diagnostics\")$convergence", "status", origin_pool_metadata$convergence_status,
  "coverage_offset_compact_lad", "attr(result, \"diagnostics\")$convergence", "rhat_max", formatC(origin_pool_metadata$rhat_max, digits = 3, format = "f"),
  "coverage_offset_compact_lad", "attr(result, \"diagnostics\")$convergence", "n_eff_min", formatC(origin_pool_metadata$n_eff_min, digits = 0, format = "f"),
  "coverage_offset_compact_lad", "validate_flow_overall(result, benchmark_od)", "mae", formatC(origin_pool_metadata$mae, digits = 1, format = "f"),
  "coverage_offset_compact_lad", "validate_flow_overall(result, benchmark_od)", "rmse", formatC(origin_pool_metadata$rmse, digits = 1, format = "f"),
  "coverage_offset_compact_lad", "validate_flow_overall(result, benchmark_od)", "pearson_r", formatC(origin_pool_metadata$pearson_r, digits = 3, format = "f"),
  "coverage_offset_compact_lad", "validate_flow_overall(result, benchmark_od)", "spearman_rho", formatC(origin_pool_metadata$spearman_rho, digits = 3, format = "f"),
  "coverage_offset_full_s4_approval", "attr(result, \"result_metadata\")", "observation_model", "coverage_offset",
  "coverage_offset_full_s4_approval", "attr(result, \"result_metadata\")", "scenario", "s4",
  "coverage_offset_full_s4_approval", "attr(result, \"result_metadata\")", "prediction_scope", "observed",
  "coverage_offset_full_s4_approval", "attr(result, \"result_metadata\")", "n_prediction_rows", "74874",
  "coverage_offset_full_s4_approval", "attr(result, \"diagnostics\")$convergence", "status", "available",
  "coverage_offset_full_s4_approval", "attr(result, \"diagnostics\")$convergence", "rhat_max", "about 1.01",
  "coverage_offset_full_s4_approval", "attr(result, \"diagnostics\")$convergence", "n_eff_min", "485",
  "coverage_offset_full_s4_approval", "attr(result, \"diagnostics\")$convergence", "diagnostic_note", "no failures, no R-hat, ESS, or non-convergence warnings",
  "coverage_offset_full_s4_approval", "provenance", "recorded_decision_date", "2026-06-25",
  "coverage_offset_full_s4_approval", "provenance", "source_note", "TASK_BOARD.md and STATUS.md empirical coverage-offset approval notes",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"result_metadata\")", "observation_model", "latent_two_level",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"result_metadata\")", "scenario", "s3 and s4",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"result_metadata\")", "prediction_scope", "observed",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"result_metadata\")", "backend", "stan_latent",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"result_metadata\")", "latent_max_treedepth", "15",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"diagnostics\")$convergence", "status", "available",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"diagnostics\")$convergence", "divergences", "0",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"diagnostics\")$convergence", "treedepth_hits", "0",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"diagnostics\")$convergence", "ebfmi_min", "> 0.91",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"diagnostics\")$convergence", "rhat_max", "about 1.023",
  "latent_two_level_full_s3_s4_approval", "attr(result, \"diagnostics\")$convergence", "n_eff_min", "about 190",
  "latent_two_level_full_s3_s4_approval", "provenance", "recorded_decision_date", "2026-06-25",
  "latent_two_level_full_s3_s4_approval", "provenance", "source_note", "TASK_BOARD.md and STATUS.md empirical latent approval notes"
)

summary_table <- tibble::tribble(
  ~observation_model, ~real_data_evidence, ~output_interpretation,
  "coverage_offset",
  paste0(
    "The LAD example uses observed MPD flows, active-user coverage, area ",
    "covariates and OD distances to estimate true-flow-scale predictions. ",
    "Larger S4 LAD/LTLA applications use the same returned columns with ",
    "source/time coverage in the observation model."
  ),
  paste0(
    "`flow_adj` is the true-flow-scale prediction and equals ",
    "`flow_true_pred`; `flow_mpd_pred` keeps the fitted MPD observation ",
    "scale with the coverage offset included. Benchmark OD cells are not ",
    "used to fit the Bayesian coverage-offset model."
  ),
  "reduced_form",
  paste0(
    "Real LAD S1 applications use the same OD identifiers, covariates and ",
    "distance inputs, but this model variant is retained as a compatibility ",
    "and sensitivity option rather than as the recommended true-flow model."
  ),
  paste0(
    "`flow_adj` is an MPD-scale counterfactual with the fitted bias term ",
    "neutralised. `flow_true_pred` is not a true-flow quantity for this ",
    "variant."
  ),
  "latent_two_level",
  paste0(
    "Repeated-source S3/S4 applications use source/time MPD rows, active-user ",
    "coverage, Census benchmark validation and LAD centroid distances to ",
    "estimate shared latent OD or OD-time true-flow states."
  ),
  paste0(
    "`latent_flow_id` identifies the shared OD or OD-time state. `flow_adj` ",
    "and `flow_true_pred` summarize the latent true-flow intensity; ",
    "`flow_mpd_pred` remains source/time-specific because it includes the ",
    "observation layer."
  )
)

utils::write.csv(
  compact_output,
  file.path(out_dir, "v09-bayesian-real-data-output.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  summary_table,
  file.path(out_dir, "v09-bayesian-real-data-summary.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  metadata_diagnostics,
  file.path(out_dir, "v09-bayesian-real-data-diagnostics.csv"),
  row.names = FALSE,
  na = ""
)

message("Wrote v09 Bayesian real-data evidence output files to ", out_dir, ".")
