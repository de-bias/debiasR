#' Simulated Active User Counts by Origin
#'
#' Simulated active-user counts derived from a mobile-phone data source.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{origin}{Origin area name.}
#'   \item{user_count}{Number of active users at the origin.}
#'   \item{mpd_source}{Mobile-phone data source identifier.}
#' }
"simulated_active.users"

#' Simulated Benchmark Origin-Destination Flows
#'
#' Simulated benchmark OD flow counts used for validation examples.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{origin}{Origin area name.}
#'   \item{destination}{Destination area name.}
#'   \item{flow}{Benchmark flow count from origin to destination.}
#' }
"simulated_benchmark.od"

#' Simulated Area Covariates
#'
#' Simulated area-level covariates for model-based correction methods.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{area}{Area name.}
#'   \item{population}{Area population benchmark.}
#'   \item{population_origin}{Origin population proxy (same as population at area level).}
#'   \item{population_destination}{Destination population proxy (same as population at area level).}
#'   \item{gni_pc}{Simulated GNI per capita.}
#'   \item{income_norm}{Income normalized between 0 and 1.}
#'   \item{internet_access}{Simulated internet access indicator.}
#'   \item{urbanisation_rate}{Simulated urbanization rate.}
#'   \item{ageing_index}{Simulated ageing index.}
#' }
"simulated_covariates"

#' Simulated OD Distance Matrix
#'
#' Simulated origin-destination great-circle distances (km) from deterministic
#' area centroids used in examples.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{origin}{Origin area name.}
#'   \item{destination}{Destination area name.}
#'   \item{distance_km}{Simulated OD distance in kilometers.}
#' }
"simulated_distance"

#' Simulated Coverage Inputs
#'
#' Simulated area-level coverage inputs containing population and active-user
#' counts, keyed by origin area.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{origin}{Origin area name.}
#'   \item{destination}{Destination area name (same as origin; retained for compatibility).}
#'   \item{population}{Benchmark resident population at origin.}
#'   \item{user_count}{Active users at origin.}
#'   \item{mpd_source}{Mobile-phone data source identifier.}
#' }
"simulated_coverage"

#' Simulated MPD Origin-Destination Flows
#'
#' Simulated origin-destination flows derived from mobile-phone data.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{origin}{Origin area name.}
#'   \item{destination}{Destination area name.}
#'   \item{mpd_source}{Mobile-phone data source identifier.}
#'   \item{flow}{Observed MPD flow count from origin to destination.}
#' }
"simulated_mpd.od"

#' Simulated Benchmark Population by Origin
#'
#' Simulated benchmark population counts by origin area.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{origin}{Origin area name.}
#'   \item{population}{Benchmark resident population at origin.}
#' }
"simulated_pop"
