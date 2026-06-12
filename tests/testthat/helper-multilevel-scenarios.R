make_multilevel_scenario_toy <- function(sources = "src1",
                                         periods = "t1",
                                         zero_filled = FALSE) {
  areas <- c("A", "B", "C")
  od <- expand.grid(
    origin = areas,
    destination = areas,
    mpd_source = sources,
    mpd_time = periods,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  origin_i <- match(od$origin, areas)
  dest_i <- match(od$destination, areas)
  source_i <- match(od$mpd_source, sources)
  time_i <- match(od$mpd_time, periods)
  od$flow <- as.integer(3 + origin_i + dest_i + source_i + time_i)
  od$mpd_observed <- TRUE
  od$mpd_zero_filled <- FALSE
  od$mpd_row_status <- "observed"

  if (isTRUE(zero_filled)) {
    od$mpd_observed[nrow(od)] <- FALSE
    od$mpd_zero_filled[nrow(od)] <- TRUE
    od$mpd_row_status[nrow(od)] <- "zero_filled"
    od$flow[nrow(od)] <- 0L
  }

  coverage <- expand.grid(
    origin = areas,
    mpd_source = sources,
    mpd_time = periods,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  coverage$population <- c(100, 120, 150)[match(coverage$origin, areas)]
  coverage$user_count <- pmax(
    2,
    round(coverage$population * (
      0.076 +
        0.014 * match(coverage$mpd_source, sources) +
        0.005 * match(coverage$mpd_time, periods)
    ))
  )

  covariates <- data.frame(
    area = areas,
    income_norm = c(0.2, 0.5, 0.8),
    rural_pct = c(0.7, 0.4, 0.1),
    deprivation_score = c(3.0, 1.5, 2.2),
    population = c(100, 120, 150)
  )

  distance <- expand.grid(
    origin = areas,
    destination = areas,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  distance$distance_km <- abs(match(distance$origin, areas) - match(distance$destination, areas)) + 1

  list(
    mpd_od = od,
    coverage = coverage,
    covariates = covariates,
    distance = distance
  )
}

make_multilevel_msoa_like_scenario <- function(n_areas = 12,
                                               sources = "operator_a",
                                               periods = "2021_q1",
                                               zero_filled = FALSE) {
  areas <- sprintf("E020%05d", seq_len(n_areas))
  od <- expand.grid(
    origin = areas,
    destination = areas,
    provider_id = sources,
    period_id = periods,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  origin_i <- match(od$origin, areas)
  dest_i <- match(od$destination, areas)
  source_i <- match(od$provider_id, sources)
  time_i <- match(od$period_id, periods)
  od$flow <- as.integer(
    15 + (origin_i * 2) + dest_i + (source_i * 4) + (time_i * 3) +
      ifelse(od$origin == od$destination, 6, 0)
  )
  od$mpd_observed <- TRUE
  od$mpd_zero_filled <- FALSE
  od$mpd_row_status <- "observed"

  if (isTRUE(zero_filled)) {
    zero_idx <- nrow(od)
    od$mpd_observed[zero_idx] <- FALSE
    od$mpd_zero_filled[zero_idx] <- TRUE
    od$mpd_row_status[zero_idx] <- "zero_filled"
    od$flow[zero_idx] <- 0L
  }

  coverage <- expand.grid(
    origin = areas,
    provider_id = sources,
    period_id = periods,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  coverage_origin_i <- match(coverage$origin, areas)
  coverage_source_i <- match(coverage$provider_id, sources)
  coverage_time_i <- match(coverage$period_id, periods)
  coverage$population <- 1000 + (coverage_origin_i * 25)
  coverage$user_count <- pmax(
    10,
    round(coverage$population * (0.055 + 0.004 * coverage_source_i + 0.002 * coverage_time_i))
  )

  covariates <- data.frame(
    area = areas,
    income_norm = seq(0.15, 0.85, length.out = n_areas),
    rural_pct = seq(0.75, 0.25, length.out = n_areas),
    deprivation_score = seq(1.2, 3.6, length.out = n_areas),
    population = 1000 + (seq_len(n_areas) * 25)
  )

  distance <- expand.grid(
    origin = areas,
    destination = areas,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  distance$distance_km <- abs(match(distance$origin, areas) - match(distance$destination, areas)) + 1

  list(
    mpd_od = od,
    coverage = coverage,
    covariates = covariates,
    distance = distance
  )
}
