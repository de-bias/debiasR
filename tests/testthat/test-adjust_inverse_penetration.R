# tests/testthat/test-adjust_inverse_penetration.R
# Updated to match the new coverage_df schema:
#   origin, population, user_count

test_that("origin weighting applies correct origin weights on toy data", {

  data(simulated_mpd.od)
  data(simulated_coverage)

  # Run method
  res <- adjust_inverse_penetration(
    mpd_od_df   = simulated_mpd.od,
    coverage_df = simulated_coverage,
    weight_by   = "origin"
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("origin", "destination", "flow_adj", "weight_origin") %in% names(res)))

  # Compute expected weight = population / user_count
  cov <- simulated_coverage
  cov$w <- cov$population / cov$user_count

  # Join weights
  chk <- res |>
    dplyr::left_join(cov |> dplyr::select(origin, w), by = "origin")

  # Verify the applied weights equal expected
  expect_equal(chk$weight_origin, chk$w, tolerance = 1e-6)

  # flow_adj must equal flow * weight_origin
  expect_equal(chk$flow_adj, chk$flow * chk$weight_origin, tolerance = 1e-6)
})


test_that("destination weighting applies correct destination weights", {

  data(simulated_mpd.od)
  data(simulated_coverage)

  res <- adjust_inverse_penetration(
    mpd_od_df   = simulated_mpd.od,
    coverage_df = simulated_coverage,
    weight_by   = "destination"
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("destination", "flow_adj", "weight_destination") %in% names(res)))

  # Expected destination weight
  cov <- simulated_coverage
  cov$w <- cov$population / cov$user_count

  chk <- res |>
    dplyr::left_join(cov |> dplyr::transmute(destination = origin, w), by = "destination")

  expect_equal(chk$weight_destination, chk$w, tolerance = 1e-6)

  expect_equal(chk$flow_adj, chk$flow * chk$weight_destination, tolerance = 1e-6)
})


test_that("both-sides weighting uses geometric mean of origin and destination weights", {

  data(simulated_mpd.od)
  data(simulated_coverage)

  res <- adjust_inverse_penetration(
    mpd_od_df   = simulated_mpd.od,
    coverage_df = simulated_coverage,
    weight_by   = "both"
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("flow_adj", "weight_origin", "weight_destination", "weight_both") %in% names(res)))

  cov <- simulated_coverage |>
    dplyr::mutate(
      w_o = population / user_count,
      w_d = population / user_count,
      w_b = sqrt(w_o * w_d)
    )

  chk <- res |>
    dplyr::left_join(cov |> dplyr::select(origin, w_o), by = "origin") |>
    dplyr::left_join(cov |> dplyr::transmute(destination = origin, w_d), by = "destination") |>
    dplyr::mutate(w_b = sqrt(w_o * w_d))

  # ensure both-side weights are correct
  expect_equal(chk$weight_both, chk$w_b, tolerance = 1e-6)

  # flow_adj must equal flow * weight_both
  expect_equal(chk$flow_adj, chk$flow * chk$weight_both, tolerance = 1e-6)
})
