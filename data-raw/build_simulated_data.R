# data-raw/build_simulated_data.R
# Regenerate simulated datasets for Stage-1 v0.2

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(usethis)
})

std_area <- function(x) {
  x |>
    as.character() |>
    stringr::str_squish()
}

std_source <- function(x) {
  x <- tolower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    stringr::str_detect(x, "facebook|\\bfb\\b") ~ "facebook",
    stringr::str_detect(x, "\\bx\\b|twitter")   ~ "twitter",
    stringr::str_detect(x, "multi.?app.?1")     ~ "multiapp1",
    stringr::str_detect(x, "multi.?app.?2")     ~ "multiapp2",
    TRUE                                         ~ x
  )
}

safe_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!any(is.finite(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

haversine_km <- function(lon1, lat1, lon2, lat2) {
  to_rad <- pi / 180
  dlon <- (lon2 - lon1) * to_rad
  dlat <- (lat2 - lat1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  6371 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

msg <- function(...) message("[build_simulated_data] ", paste0(...))

active_users_path <- "data-raw/active-user-count_data.csv"
pop_bench_path <- "data-raw/benchmark-population_data.csv"
mpd_od_path <- "data-raw/od_df_mar2020_feb2021.csv"
bench_od_path <- "data-raw/internal-migration-benchmark_data.csv"

stopifnot(
  file.exists(active_users_path),
  file.exists(pop_bench_path),
  file.exists(mpd_od_path),
  file.exists(bench_od_path)
)

aup_raw <- readr::read_csv(active_users_path, show_col_types = FALSE)
pop_raw <- readr::read_csv(pop_bench_path, show_col_types = FALSE)
mpd_raw <- readr::read_csv(mpd_od_path, show_col_types = FALSE)
bench_raw <- readr::read_csv(bench_od_path, show_col_types = FALSE)

users_wide <- aup_raw |>
  dplyr::transmute(
    area = std_area(.data$name),
    origin_user_count_raw = suppressWarnings(as.numeric(.data$origin_users)),
    mpd_source = std_source(.data$source_mpd)
  ) |>
  dplyr::group_by(area, mpd_source) |>
  dplyr::summarise(origin_user_count = safe_max(origin_user_count_raw), .groups = "drop") |>
  dplyr::filter(is.finite(origin_user_count), origin_user_count > 0)

pop_wide <- pop_raw |>
  dplyr::transmute(
    area = std_area(.data$lad_name),
    population_raw = suppressWarnings(as.numeric(.data$origin_population))
  ) |>
  dplyr::group_by(area) |>
  dplyr::summarise(population = safe_max(population_raw), .groups = "drop") |>
  dplyr::filter(is.finite(population), population > 0)

bench_norm <- bench_raw |>
  dplyr::transmute(
    origin = std_area(.data$lad_name_2020),
    destination = std_area(.data$lad_name_2021),
    flow = suppressWarnings(as.numeric(.data$Count))
  ) |>
  dplyr::filter(is.finite(flow), flow >= 0) |>
  dplyr::group_by(origin, destination) |>
  dplyr::summarise(flow = sum(flow, na.rm = TRUE), .groups = "drop")

mpd_norm <- mpd_raw |>
  dplyr::transmute(
    origin = std_area(.data$origin),
    destination = std_area(.data$destination),
    flow = suppressWarnings(as.numeric(.data$total_flow)),
    mpd_source = std_source(.data$source_mpd)
  ) |>
  dplyr::filter(is.finite(flow), flow >= 0) |>
  dplyr::group_by(origin, destination, mpd_source) |>
  dplyr::summarise(flow = sum(flow, na.rm = TRUE), .groups = "drop")

top_source <- mpd_norm |>
  dplyr::group_by(mpd_source) |>
  dplyr::summarise(total = sum(flow, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(total)) |>
  dplyr::slice(1) |>
  dplyr::pull(mpd_source)

# Use all common areas available in benchmark/pop/users
S <- Reduce(
  intersect,
  list(
    unique(bench_norm$origin),
    unique(bench_norm$destination),
    unique(pop_wide$area),
    unique(users_wide$area)
  )
) |>
  sort()

if (length(S) < 10L) {
  stop("Need at least 10 common areas for robust simulation.")
}

msg("Using all common areas: ", length(S))

# Square benchmark table across all areas
bench_sq <- tidyr::crossing(origin = S, destination = S) |>
  dplyr::left_join(
    bench_norm |>
      dplyr::filter(origin %in% S, destination %in% S),
    by = c("origin", "destination")
  ) |>
  dplyr::mutate(flow = tidyr::replace_na(flow, 0))

simulated_pop <- pop_wide |>
  dplyr::filter(area %in% S) |>
  dplyr::transmute(origin = area, population = round(population)) |>
  dplyr::arrange(origin)

bench_offdiag_out <- bench_sq |>
  dplyr::filter(origin != destination) |>
  dplyr::group_by(origin) |>
  dplyr::summarise(offdiag_out = sum(flow), .groups = "drop")

bench_diag <- bench_offdiag_out |>
  dplyr::left_join(simulated_pop, by = "origin") |>
  dplyr::mutate(diag_flow = pmax(population - offdiag_out, 0)) |>
  dplyr::select(origin, diag_flow)

bench_sq <- bench_sq |>
  dplyr::left_join(bench_diag, by = "origin") |>
  dplyr::mutate(flow = ifelse(origin == destination, diag_flow, flow)) |>
  dplyr::select(origin, destination, flow)

# Deterministic synthetic centroids (used only to produce realistic distance inputs)
set.seed(2026)
n <- length(S)
grid_dim <- ceiling(sqrt(n))
lon_grid <- seq(-6.5, 1.8, length.out = grid_dim)
lat_grid <- seq(50.1, 57.9, length.out = grid_dim)
coords <- tidyr::crossing(lon = lon_grid, lat = lat_grid) |>
  dplyr::slice(1:n) |>
  dplyr::mutate(area = S) |>
  dplyr::select(area, lon, lat)

simulated_distance <- tidyr::crossing(origin = S, destination = S) |>
  dplyr::left_join(dplyr::rename(coords, origin = area, lon_o = lon, lat_o = lat), by = "origin") |>
  dplyr::left_join(dplyr::rename(coords, destination = area, lon_d = lon, lat_d = lat), by = "destination") |>
  dplyr::mutate(distance_km = haversine_km(lon_o, lat_o, lon_d, lat_d)) |>
  dplyr::select(origin, destination, distance_km)

# Coverage rates from observed users/pop, with regularisation
coverage_by_origin <- simulated_pop |>
  dplyr::left_join(
    users_wide |>
      dplyr::filter(mpd_source == top_source) |>
      dplyr::transmute(origin = area, user_count_raw = origin_user_count),
    by = "origin"
  ) |>
  dplyr::mutate(
    user_count_raw = ifelse(is.finite(.data$user_count_raw), .data$user_count_raw, .data$population * 0.6),
    coverage_rate = pmin(pmax(.data$user_count_raw / .data$population, 0.08), 0.92)
  ) |>
  dplyr::select(origin, population, coverage_rate)

# Simulate MPD with realistic bias channels: coverage + distance + stochastic noise
sim_df <- bench_sq |>
  dplyr::filter(origin != destination) |>
  dplyr::left_join(simulated_distance, by = c("origin", "destination")) |>
  dplyr::left_join(
    dplyr::rename(coverage_by_origin, cov_o = coverage_rate),
    by = "origin"
  ) |>
  dplyr::left_join(
    dplyr::rename(coverage_by_origin, destination = origin, cov_d = coverage_rate),
    by = "destination"
  )

# Build empirical MPD on the same full OD support (off-diagonal only) so we can
# calibrate the simulation to match zero-mass and flow quantiles explicitly.
emp_mpd_sq <- tidyr::crossing(origin = S, destination = S) |>
  dplyr::left_join(
    mpd_norm |>
      dplyr::filter(.data$origin %in% S, .data$destination %in% S, .data$mpd_source == top_source) |>
      dplyr::select(.data$origin, .data$destination, .data$flow),
    by = c("origin", "destination")
  ) |>
  dplyr::mutate(flow = tidyr::replace_na(.data$flow, 0)) |>
  dplyr::filter(.data$origin != .data$destination)

target_zero <- mean(emp_mpd_sq$flow == 0)
target_q <- stats::quantile(log1p(emp_mpd_sq$flow), probs = c(0.5, 0.9, 0.99), na.rm = TRUE)

dmax <- max(sim_df$distance_km, na.rm = TRUE)
dn <- sim_df$distance_km / dmax

simulate_flow_mpd <- function(alpha, beta, scale, suppr_prob, theta, seed = 2026L) {
  set.seed(seed)

  obs_prob <- (sim_df$cov_o * sim_df$cov_d)^alpha * exp(-beta * dn)
  obs_prob <- pmin(pmax(scale * obs_prob, 0.001), 0.98)

  lambda <- pmax(sim_df$flow * obs_prob, 0)

  # Optional gamma-poisson mixing to tune over-dispersion and tail behaviour.
  if (is.finite(theta) && theta > 0) {
    lambda <- lambda * stats::rgamma(length(lambda), shape = theta, rate = theta)
  }

  flow_sim <- stats::rpois(length(lambda), lambda)

  # Privacy-like suppression on very small counts.
  suppress_flag <- flow_sim <= 3 & stats::runif(length(flow_sim)) < suppr_prob
  flow_sim[suppress_flag] <- 0

  # Rare false positives where benchmark has structural zero.
  fp_flag <- sim_df$flow == 0 & stats::runif(length(flow_sim)) < 0.01
  flow_sim[fp_flag] <- pmax(flow_sim[fp_flag], 1)

  flow_sim
}

score_candidate <- function(flow_sim) {
  qs <- stats::quantile(log1p(flow_sim), probs = c(0.5, 0.9, 0.99), na.rm = TRUE)
  zero <- mean(flow_sim == 0)

  # Strongly prioritize matching zero-mass and upper tail.
  zero_term <- (zero - target_zero)^2
  q_rel <- (qs - target_q) / pmax(abs(target_q), 1e-6)
  q_term <- sum(q_rel^2)

  6 * zero_term + 2 * q_term
}

evaluate_grid <- function(grid_df) {
  best <- list(
    idx = NA_integer_,
    loss = Inf,
    flow = NULL,
    zero = NA_real_,
    q = rep(NA_real_, 3)
  )

  for (k in seq_len(nrow(grid_df))) {
    par <- grid_df[k, ]
    flow_k <- simulate_flow_mpd(
      alpha = par$alpha,
      beta = par$beta,
      scale = par$scale,
      suppr_prob = par$suppr_prob,
      theta = par$theta
    )
    loss_k <- score_candidate(flow_k)

    if (loss_k < best$loss) {
      best$idx <- k
      best$loss <- loss_k
      best$flow <- flow_k
      best$zero <- mean(flow_k == 0)
      best$q <- stats::quantile(log1p(flow_k), probs = c(0.5, 0.9, 0.99), na.rm = TRUE)
    }
  }

  best
}

# Stage 1: coarse global search.
param_grid_coarse <- tidyr::crossing(
  alpha = c(0.50, 0.65, 0.80, 0.95),
  beta = c(0.20, 0.35, 0.50, 0.65),
  scale = c(0.75, 0.90, 1.05, 1.20),
  suppr_prob = c(0.35, 0.50, 0.65),
  theta = c(3, 6, Inf)
)

best1 <- evaluate_grid(param_grid_coarse)
best_par1 <- param_grid_coarse[best1$idx, ]

near_theta <- function(theta0) {
  grid <- c(3, 6, Inf)
  if (is.infinite(theta0)) {
    return(c(6, Inf))
  }
  idx <- which(abs(grid - theta0) < 1e-8)
  if (length(idx) == 0L) return(c(theta0, Inf))
  lo <- max(1L, idx - 1L)
  hi <- min(length(grid), idx + 1L)
  unique(grid[lo:hi])
}

# Stage 2: local refinement around coarse optimum.
param_grid_local <- tidyr::crossing(
  alpha = pmin(pmax(best_par1$alpha + c(-0.10, -0.05, 0, 0.05, 0.10), 0.35), 1.25),
  beta = pmin(pmax(best_par1$beta + c(-0.10, -0.05, 0, 0.05, 0.10), 0.05), 1.00),
  scale = pmin(pmax(best_par1$scale + c(-0.12, -0.06, 0, 0.06, 0.12), 0.50), 1.40),
  suppr_prob = pmin(pmax(best_par1$suppr_prob + c(-0.12, -0.06, 0, 0.06, 0.12), 0.05), 0.90),
  theta = near_theta(best_par1$theta)
) |>
  dplyr::distinct()

best2 <- evaluate_grid(param_grid_local)
best_par <- param_grid_local[best2$idx, ]
flow_mpd <- best2$flow
best_loss <- best2$loss
best_zero <- best2$zero
best_q <- best2$q

msg(
  "MPD calibration coarse best: alpha=", best_par1$alpha,
  ", beta=", best_par1$beta,
  ", scale=", best_par1$scale,
  ", suppr_prob=", best_par1$suppr_prob,
  ", theta=", ifelse(is.finite(best_par1$theta), as.character(best_par1$theta), "Inf"),
  ", loss=", signif(best1$loss, 4)
)
msg(
  "MPD calibration refined best: alpha=", best_par$alpha,
  ", beta=", best_par$beta,
  ", scale=", best_par$scale,
  ", suppr_prob=", best_par$suppr_prob,
  ", theta=", ifelse(is.finite(best_par$theta), as.character(best_par$theta), "Inf"),
  ", loss=", signif(best_loss, 4)
)
msg(
  "MPD calibration targets vs simulated: zero=",
  signif(target_zero, 4), " vs ", signif(best_zero, 4),
  "; q50=", signif(target_q[[1]], 4), " vs ", signif(best_q[[1]], 4),
  "; q90=", signif(target_q[[2]], 4), " vs ", signif(best_q[[2]], 4),
  "; q99=", signif(target_q[[3]], 4), " vs ", signif(best_q[[3]], 4)
)

simulated_mpd.od <- sim_df |>
  dplyr::transmute(origin, destination, mpd_source = top_source, flow = as.numeric(flow_mpd)) |>
  dplyr::arrange(origin, destination)

simulated_benchmark.od <- bench_sq |>
  dplyr::filter(origin != destination) |>
  dplyr::arrange(origin, destination)

# Active users and coverage tables
set.seed(2027)
simulated_active.users <- coverage_by_origin |>
  dplyr::mutate(
    user_count = pmax(1, floor(population * pmin(pmax(coverage_rate + stats::rnorm(dplyr::n(), 0, 0.03), 0.06), 0.95))),
    mpd_source = top_source
  ) |>
  dplyr::select(origin, user_count, mpd_source) |>
  dplyr::arrange(origin)

simulated_coverage <- simulated_pop |>
  dplyr::left_join(
    simulated_active.users |>
      dplyr::select(origin, user_count, mpd_source),
    by = "origin"
  ) |>
  dplyr::mutate(
    destination = origin
  ) |>
  dplyr::select(
    origin,
    destination,
    population,
    user_count,
    mpd_source
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(origin)

# Keep an area-level coverage table as primary schema, with destination retained
# for compatibility with functions that optionally join by destination key.
simulated_coverage <- simulated_coverage |>
  dplyr::select(
    origin,
    destination,
    population,
    user_count,
    mpd_source
  ) |>
  dplyr::arrange(origin)

# Consistency checks
stopifnot(nrow(simulated_mpd.od) == length(S) * (length(S) - 1))
stopifnot(nrow(simulated_benchmark.od) == length(S) * (length(S) - 1))
stopifnot(nrow(simulated_distance) == length(S) * length(S))
stopifnot(all(is.finite(simulated_distance$distance_km)))
stopifnot(all(simulated_distance$distance_km >= 0))

msg("Top MPD source used: ", top_source)
msg("Areas used: ", length(S))
msg("simulated_mpd.od rows: ", nrow(simulated_mpd.od))
msg("simulated_benchmark.od rows: ", nrow(simulated_benchmark.od))
msg("simulated_distance rows: ", nrow(simulated_distance))

usethis::use_data(
  simulated_mpd.od,
  simulated_benchmark.od,
  simulated_coverage,
  simulated_active.users,
  simulated_pop,
  simulated_distance,
  overwrite = TRUE
)
