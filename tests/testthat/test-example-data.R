test_that("debiasR_example_data normalises empirical travel-to-work files", {
  mpd_path <- tempfile(fileext = ".csv")
  census_path <- tempfile(fileext = ".csv")

  utils::write.csv(
    data.frame(
      MSOA21CD_home = c("E02000001", "E02000001", "E02000002", "E02000003"),
      county_home = "GREATER_LONDON_AUTHORITY",
      MSOA21CD_work = c("E02000001", "E02000002", "E02000001", "E02000003"),
      county_work = "GREATER_LONDON_AUTHORITY",
      count = c(10, 4, 5, 1)
    ),
    mpd_path,
    row.names = FALSE
  )

  utils::write.csv(
    data.frame(
      `Middle layer Super Output Areas code` = c(
        "E02000001", "E02000001", "E02000002", "E02000003", "E02000001"
      ),
      `Middle layer Super Output Areas label` = "area",
      `MSOA of workplace code` = c(
        "E02000001", "E02000002", "E02000001", "E02000003", "999999999"
      ),
      `MSOA of workplace label` = "work",
      `Place of work indicator (4 categories) code` = c(3, 3, 3, 1, 2),
      `Place of work indicator (4 categories) label` = "label",
      Count = c(100, 25, 50, 200, 999),
      check.names = FALSE
    ),
    census_path,
    row.names = FALSE
  )

  ex <- debiasR_example_data(
    n_areas = Inf,
    mpd_path = mpd_path,
    census_path = census_path
  )

  expect_named(
    ex,
    c(
      "mpd_od",
      "benchmark_od",
      "coverage",
      "active_users",
      "population",
      "covariates",
      "distance",
      "od_audit",
      "msoa_OD_travel2work",
      "census_msoa_OD_travel2work",
      "metadata"
    )
  )
  expect_equal(nrow(ex$mpd_od), 3)
  expect_equal(nrow(ex$benchmark_od), 3)
  expect_true(all(c("origin", "destination", "flow") %in% names(ex$benchmark_od)))
  expect_false("999999999" %in% ex$benchmark_od$destination)

  coverage_1 <- ex$coverage[ex$coverage$origin == "E02000001", ]
  expect_equal(coverage_1$population, 125)
  expect_equal(coverage_1$user_count, 14)
  expect_equal(coverage_1$mpd_source, "locomizer_travel_to_work")
})

test_that("debiasR_example_data can return strict complete square OD support", {
  mpd_path <- tempfile(fileext = ".csv")
  census_path <- tempfile(fileext = ".csv")

  utils::write.csv(
    data.frame(
      MSOA21CD_home = c("E02000001", "E02000001", "E02000002", "E02000003"),
      MSOA21CD_work = c("E02000001", "E02000002", "E02000001", "E02000003"),
      count = c(10, 4, 5, 1)
    ),
    mpd_path,
    row.names = FALSE
  )

  utils::write.csv(
    data.frame(
      `Middle layer Super Output Areas code` = c(
        "E02000001", "E02000001", "E02000002", "E02000003"
      ),
      `MSOA of workplace code` = c(
        "E02000001", "E02000002", "E02000001", "E02000003"
      ),
      `Place of work indicator (4 categories) code` = c(3, 3, 3, 1),
      Count = c(100, 25, 50, 200),
      check.names = FALSE
    ),
    census_path,
    row.names = FALSE
  )

  ex <- debiasR_example_data(
    n_areas = Inf,
    mpd_path = mpd_path,
    census_path = census_path,
    complete_grid = TRUE
  )

  areas_mpd_o <- sort(unique(ex$mpd_od$origin))
  areas_mpd_d <- sort(unique(ex$mpd_od$destination))
  areas_bench_o <- sort(unique(ex$benchmark_od$origin))
  areas_bench_d <- sort(unique(ex$benchmark_od$destination))

  expect_identical(areas_mpd_o, areas_mpd_d)
  expect_identical(areas_mpd_o, areas_bench_o)
  expect_identical(areas_mpd_o, areas_bench_d)
  expect_equal(nrow(ex$mpd_od), length(areas_mpd_o)^2)
  expect_equal(nrow(ex$benchmark_od), length(areas_mpd_o)^2)
  expect_false(any(duplicated(ex$mpd_od[c("origin", "destination")])))
  expect_false(any(duplicated(ex$benchmark_od[c("origin", "destination")])))
  expect_true(all(is.finite(ex$mpd_od$flow) & ex$mpd_od$flow >= 0))
  expect_true(all(is.finite(ex$benchmark_od$flow) & ex$benchmark_od$flow >= 0))
  expect_true(any(ex$mpd_od$mpd_zero_filled))
  expect_true(any(ex$benchmark_od$benchmark_zero_filled))
  expect_true(ex$od_audit$strict_square_support)
  expect_equal(ex$metadata$n_mpd_zero_filled, sum(ex$mpd_od$mpd_zero_filled))
  expect_equal(ex$metadata$mpd_total_flow, sum(ex$mpd_od$flow))
  expect_equal(ex$metadata$benchmark_total_flow, sum(ex$benchmark_od$flow))
  expect_equal(ex$metadata$mpd_balance_diff, 0)
  expect_equal(ex$metadata$benchmark_balance_diff, 0)
})
