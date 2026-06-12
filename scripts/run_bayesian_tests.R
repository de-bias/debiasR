#!/usr/bin/env Rscript

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the package root so DESCRIPTION is available.")
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("The Bayesian test runner requires the 'devtools' package.")
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("The Bayesian test runner requires the 'testthat' package.")
}

if (!requireNamespace("rstanarm", quietly = TRUE)) {
  stop("The Bayesian test runner requires the optional 'rstanarm' package.")
}

if (!requireNamespace("rstan", quietly = TRUE)) {
  stop("The Bayesian test runner requires the optional 'rstan' package for the latent two-level backend.")
}

options(mc.cores = 1)
Sys.setenv(RSTAN_NUM_THREADS = "1")

start_time <- Sys.time()
message("Bayesian test run started at ", format(start_time, "%Y-%m-%d %H:%M:%S %Z"))
message("R version: ", getRversion())
message("testthat version: ", as.character(utils::packageVersion("testthat")))
message("rstanarm version: ", as.character(utils::packageVersion("rstanarm")))
message("rstan version: ", as.character(utils::packageVersion("rstan")))
message("Loading debiasR package context with devtools::load_all().")

devtools::load_all(".", quiet = TRUE)

test_file <- file.path("tests", "testthat", "test-adjust-multilevel-bayes.R")
message("Running ", test_file)
testthat::test_file(test_file, reporter = "summary")

end_time <- Sys.time()
message("Bayesian test run finished at ", format(end_time, "%Y-%m-%d %H:%M:%S %Z"))
message(
  "Elapsed time: ",
  round(as.numeric(difftime(end_time, start_time, units = "secs")), 1),
  " seconds"
)
