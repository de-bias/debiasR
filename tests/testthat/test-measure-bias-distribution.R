test_that("measure_bias_distribution returns zero divergence for proportional shares", {
  coverage_df <- data.frame(
    origin = c("A", "B", "C"),
    population = c(100, 200, 300),
    user_count = c(10, 20, 30)
  )

  res <- measure_bias_distribution(coverage_df)

  expect_true(all(c("summary", "area_level") %in% names(res)))
  expect_equal(res$summary$n_areas, 3)
  expect_equal(res$summary$kl_population_user, 0, tolerance = 1e-12)
  expect_equal(res$summary$jsd_population_user, 0, tolerance = 1e-12)
  expect_equal(
    res$area_level$population_share,
    res$area_level$user_share,
    tolerance = 1e-8
  )
})

test_that("measure_bias_distribution matches manual KL and JSD calculations", {
  coverage_df <- data.frame(
    area_id = c("A", "B"),
    pop = c(300, 100),
    users = c(25, 75)
  )

  res <- measure_bias_distribution(
    coverage_df,
    area_col = "area_id",
    population_col = "pop",
    user_count_col = "users"
  )

  p <- c(300, 100) / 400
  q <- c(25, 75) / 100
  midpoint <- 0.5 * (p + q)
  expected_kl <- sum(p * log(p / q))
  expected_jsd <- 0.5 * sum(p * log(p / midpoint)) +
    0.5 * sum(q * log(q / midpoint))

  expect_equal(res$summary$kl_population_user, expected_kl, tolerance = 1e-8)
  expect_equal(res$summary$jsd_population_user, expected_jsd, tolerance = 1e-8)
  expect_equal(
    res$area_level$share_difference_user_minus_population,
    q - p,
    tolerance = 1e-8
  )
})

test_that("measure_bias_distribution handles zero users through smoothing", {
  coverage_df <- data.frame(
    origin = c("A", "B"),
    population = c(100, 50),
    user_count = c(0, 0)
  )

  res <- measure_bias_distribution(coverage_df)

  expect_true(is.finite(res$summary$kl_population_user))
  expect_true(is.finite(res$summary$jsd_population_user))
  expect_equal(res$summary$total_user_count, 0)
})

test_that("measure_bias_distribution validates inputs", {
  coverage_df <- data.frame(
    origin = c("A", "B"),
    population = c(100, 50),
    user_count = c(10, 5)
  )

  expect_error(
    measure_bias_distribution(coverage_df, epsilon = 0),
    "`epsilon` must be a single positive finite number."
  )
  expect_error(
    measure_bias_distribution(coverage_df[, c("origin", "population")]),
    "`coverage_df` must contain:"
  )
  expect_error(
    measure_bias_distribution(
      data.frame(origin = c("A", "A"), population = c(100, 50), user_count = c(10, 5))
    ),
    "one row per area"
  )
  expect_error(
    measure_bias_distribution(
      data.frame(origin = "A", population = -1, user_count = 1)
    ),
    "`population_col` must be positive"
  )
  expect_error(
    measure_bias_distribution(
      data.frame(origin = "A", population = 1, user_count = -1)
    ),
    "`user_count_col` must be non-negative"
  )
})
