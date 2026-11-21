#' Inverse penetration rate weights
#'
#' Adjusts MPD-derived OD flows using inverse penetration weights derived from
#' coverage information on population and active users at origins and/or
#' destinations.
#'
#' Notation used throughout:
#' \itemize{
#'   \item \eqn{P_i^{(O)}, P_j^{(D)}}: population at origin \eqn{i} and destination \eqn{j}
#'   \item \eqn{U_i^{(O)}, U_j^{(D)}}: active users at origin \eqn{i} and destination \eqn{j}
#'   \item \eqn{F_{ij}^{mpd}}: observed MPD flow from \eqn{i} to \eqn{j}
#'   \item \eqn{F_{ij}^{adj}}: adjusted flow
#' }
#'
#' The coverage data frame is expected to contain, at minimum:
#' \itemize{
#'   \item \code{origin}, \code{population}, \code{user_count}
#' }
#' possibly with a \code{mpd_source} column for source-specific coverage.
#'
#' Penetration at origins is defined as
#'
#' \deqn{p^{(O)}_i = \frac{U^{(O)}_i}{P^{(O)}_i}}
#'
#' and the inverse-penetration weight is \eqn{w^{(O)}_i = P^{(O)}_i / U^{(O)}_i}.
#' Analogously for destinations.
#'
#' For \code{weight_by = "origin"}, adjusted flows are:
#'
#' \deqn{F^{adj}_{ij} = w^{(O)}_i F^{mpd}_{ij}.}
#'
#' For \code{weight_by = "destination"}:
#'
#' \deqn{F^{adj}_{ij} = w^{(D)}_j F^{mpd}_{ij}.}
#'
#' For \code{weight_by = "both"}, we use the geometric mean of the two:
#'
#' \deqn{w^{(B)}_{ij} = \sqrt{w^{(O)}_i w^{(D)}_j}, \quad
#'       F^{adj}_{ij} = w^{(B)}_{ij} F^{mpd}_{ij}.}
#'
#' @param mpd_od_df Data frame of MPD flows with at least:
#'   \code{origin, destination,} and a flow column (default \code{"flow"}).
#'   If present, \code{mpd_source} is used to match coverage by source.
#' @param coverage_df Data frame with at least:
#'   \itemize{
#'     \item \code{origin, population, user_count}
#'   }
#'   Only identical coverage records may repeat for an area and applicable source.
#'   Non-positive or missing denominators yield missing weights.
#'   The coverage data may also include \code{mpd_source}.
#' @param flow_col Name of the MPD flow column in \code{mpd_od_df}. Default "flow".
#' @param weight_by One of \code{"origin"}, \code{"destination"}, \code{"both"}.
#'
#' @return A tibble with:
#'   \itemize{
#'     \item \code{origin, destination} (and \code{mpd_source} if present),
#'     \item \code{flow}: original MPD flow,
#'     \item \code{flow_adj}: adjusted flow,
#'     \item \code{weight_missing}: whether the selected correction weight is unavailable,
#'     \item \code{weight_origin}: origin-side inverse-penetration weight (if applicable),
#'     \item \code{weight_destination}: destination-side weight (if applicable),
#'     \item \code{weight_both}: geometric mean of the two (if \code{weight_by = "both"}).
#'   }
#'
#' @export
adjust_inverse_penetration <- function(mpd_od_df,
                                        coverage_df,
                                        flow_col  = "flow",
                                        weight_by = c("origin", "destination", "both")) {

  weight_by <- match.arg(weight_by)

  # --- basic checks -------------------------------------------------------

  req_mpd <- c("origin", "destination", flow_col)
  if (!all(req_mpd %in% names(mpd_od_df))) {
    stop("`mpd_od_df` must contain: ", paste(req_mpd, collapse = ", "))
  }

  has_source <- "mpd_source" %in% names(mpd_od_df) &&
    "mpd_source" %in% names(coverage_df)

  # coverage requirements
  req_cov <- c("origin", "population", "user_count")
  if (!all(req_cov %in% names(coverage_df))) {
    stop("`coverage_df` must contain: ", paste(req_cov, collapse = ", "))
  }

  keys <- c("origin", if (has_source) "mpd_source")
  coverage <- unique(coverage_df[c(keys, "population", "user_count")])
  if (anyDuplicated(coverage[keys])) {
    stop("Conflicting coverage records for the same ", paste(keys, collapse = ", "), ".")
  }
  valid <- with(coverage, is.finite(population) & population > 0 &
                  is.finite(user_count) & user_count > 0)
  coverage$weight <- rep(NA_real_, nrow(coverage))
  coverage$weight[valid] <- coverage$population[valid] / coverage$user_count[valid]
  coverage$weight[!is.finite(coverage$weight)] <- NA_real_
  out <- mpd_od_df
  if (weight_by %in% c("origin", "both")) {
    lookup <- dplyr::select(coverage, dplyr::all_of(keys), weight_origin = dplyr::all_of("weight"))
    out <- dplyr::left_join(out, lookup, by = keys)
  }
  if (weight_by %in% c("destination", "both")) {
    lookup <- dplyr::select(coverage, dplyr::all_of(keys), weight_destination = dplyr::all_of("weight")) |>
      dplyr::rename(destination = origin)
    out <- dplyr::left_join(out, lookup, by = c("destination", setdiff(keys, "origin")))
  }

  # --- compute adjusted flows --------------------------------------------

  flow_vec <- out[[flow_col]]

  if (weight_by == "origin") {
    out$flow_adj <- flow_vec * out$weight_origin

  } else if (weight_by == "destination") {
    out$flow_adj <- flow_vec * out$weight_destination

  } else if (weight_by == "both") {
    # geometric mean when both weights are available and positive
    out$weight_both <- sqrt(out$weight_origin * out$weight_destination)
    out$flow_adj    <- flow_vec * out$weight_both
  }

  out$weight_missing <- !is.finite(switch(weight_by, origin = out$weight_origin,
                                           destination = out$weight_destination, both = out$weight_both))

  # final ordering: keep original flow explicitly
  out <- out |>
    dplyr::rename(flow = !!rlang::sym(flow_col)) |>
    tibble::as_tibble()

  out
}
