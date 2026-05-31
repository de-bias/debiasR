#!/usr/bin/env Rscript

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the package root so DESCRIPTION is available.")
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("The dev test runner requires the 'devtools' package.")
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("The dev test runner requires the 'testthat' package.")
}

run_bayesian <- identical(Sys.getenv("DEBIASR_RUN_BAYESIAN"), "true")

test_files <- list.files(
  file.path("tests", "testthat"),
  pattern = "^test-.*[.]R$",
  full.names = TRUE
)

bayesian_test_file <- file.path("tests", "testthat", "test-adjust-multilevel-bayes.R")
if (!run_bayesian) {
  test_files <- setdiff(test_files, bayesian_test_file)
}

message("Loading debiasR package context with devtools::load_all().")
devtools::load_all(".", quiet = TRUE)

message("Running ", length(test_files), " test files.")
if (!run_bayesian) {
  message(
    "Skipping optional Bayesian test file. Set DEBIASR_RUN_BAYESIAN=true ",
    "to include it."
  )
}

for (test_file in sort(test_files)) {
  message("Running ", test_file)
  testthat::test_file(test_file, reporter = "summary")
}

message("Development test suite completed successfully.")
