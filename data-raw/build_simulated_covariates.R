# data-raw/build_simulated_covariates.R
# Build simulated covariates for Stage-1 v0.2

suppressPackageStartupMessages({
  library(dplyr)
  library(usethis)
})

if (!file.exists("data/simulated_coverage.rda") || !file.exists("data/simulated_pop.rda")) {
  stop("simulated_coverage.rda and simulated_pop.rda are required. Run data-raw/build_simulated_data.R first.")
}

load("data/simulated_coverage.rda")
load("data/simulated_pop.rda")

simulated_covariates <- simulated_coverage %>%
  distinct(origin) %>%
  arrange(origin) %>%
  rename(area = origin) %>%
  left_join(simulated_pop %>% rename(area = origin), by = "area") %>%
  mutate(
    gni_pc = as.numeric(scale(population)) * 4000 + 24000,
    gni_pc = pmax(gni_pc, 12000),
    income_norm = (gni_pc - min(gni_pc)) / (max(gni_pc) - min(gni_pc)),
    internet_access = pmin(pmax(0.25 + 0.65 * income_norm + rnorm(n(), 0, 0.04), 0.05), 0.98),
    urbanisation_rate = pmin(pmax(0.20 + 0.70 * income_norm + rnorm(n(), 0, 0.05), 0.02), 0.99),
    ageing_index = pmin(pmax(0.85 - 0.35 * income_norm + rnorm(n(), 0, 0.03), 0.15), 1.30),
    population_origin = population,
    population_destination = population
  ) %>%
  select(
    area,
    population,
    population_origin,
    population_destination,
    gni_pc,
    income_norm,
    internet_access,
    urbanisation_rate,
    ageing_index
  )

use_data(simulated_covariates, overwrite = TRUE)
