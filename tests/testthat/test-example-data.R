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
