test_that("adjust_raking_ratio performs a real margin adjustment on a tiny OD table", {
  mpd_od_df <- data.frame(
    origin = c("A", "A", "B", "B"),
    destination = c("X", "Y", "X", "Y"),
    flow = c(10, 20, 30, 40)
  )

  origin_targets <- data.frame(
    origin = c("A", "B"),
    target = c(60, 40)
  )

  destination_targets <- data.frame(
    destination = c("X", "Y"),
    target = c(50, 50)
  )

  res <- adjust_raking_ratio(
    mpd_od_df = mpd_od_df,
    origin_targets = origin_targets,
    destination_targets = destination_targets,
    max_iter = 200,
    tol = 1e-10
  )

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("origin", "destination", "flow", "flow_adj", "weight_ipf") %in% names(res)))
  expect_true(isTRUE(attr(res, "ipf_converged")))
  expect_gt(attr(res, "ipf_iterations"), 0L)

  adj_origin <- aggregate(flow_adj ~ origin, data = res, sum)
  cmp_origin <- merge(adj_origin, origin_targets, by = "origin", all = TRUE)
  expect_equal(cmp_origin$flow_adj, cmp_origin$target, tolerance = 1e-6)

  adj_destination <- aggregate(flow_adj ~ destination, data = res, sum)
  cmp_destination <- merge(adj_destination, destination_targets, by = "destination", all = TRUE)
  expect_equal(cmp_destination$flow_adj, cmp_destination$target, tolerance = 1e-6)
})
