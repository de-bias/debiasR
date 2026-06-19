.validate_distribution_epsilon <- function(epsilon) {
  if (length(epsilon) != 1L || !is.finite(epsilon) || epsilon <= 0) {
    stop("`epsilon` must be a single positive finite number.")
  }
  invisible(epsilon)
}

.distribution_shares <- function(x, epsilon) {
  total <- sum(x + epsilon, na.rm = TRUE)
  if (!is.finite(total) || total <= 0) {
    stop("Cannot compute distribution shares from non-finite or empty totals.")
  }
  (x + epsilon) / total
}

.distribution_jsd_contributions <- function(p_share, q_share) {
  midpoint_share <- 0.5 * (p_share + q_share)
  0.5 * p_share * log(p_share / midpoint_share) +
    0.5 * q_share * log(q_share / midpoint_share)
}

.normalise_distribution_comparisons <- function(comparisons) {
  .normalise_flow_comparisons(comparisons)
}
