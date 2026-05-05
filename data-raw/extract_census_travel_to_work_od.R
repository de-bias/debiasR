# Extract the Census 2021 MSOA travel-to-work benchmark used by examples.
#
# Input MPD data:
#   Zenodo 10.5281/zenodo.13327082, msoa_OD_travel2work.csv.gz
#
# Census source:
#   Nomis Census 2021 origin-destination workplace data, ODWP01EW.
#   The MSOA file is filtered to place-of-work indicator code 3:
#   "Working in the UK but not working at or from home".
#
# Output:
#   census_msoa_OD_travel2work.csv.gz, normalized as:
#   origin, destination, flow
#
# Configure paths with environment variables:
#   MPD_TRAVEL_TO_WORK_PATH
#   CENSUS_ODWP01EW_ZIP
#   CENSUS_TRAVEL_TO_WORK_OUTPUT

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

zenodo_mpd_url <- "https://zenodo.org/api/records/13327082/files/msoa_OD_travel2work.csv.gz/content"
nomis_odwp01_url <- "https://www.nomisweb.co.uk/output/census/2021/odwp01ew.zip"

cache_dir <- Sys.getenv(
  "DEBIASR_CACHE_DIR",
  file.path(tempdir(), "debiasR-travel-to-work-cache")
)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

mpd_path <- Sys.getenv(
  "MPD_TRAVEL_TO_WORK_PATH",
  file.path(cache_dir, "msoa_OD_travel2work.csv.gz")
)
census_zip <- Sys.getenv(
  "CENSUS_ODWP01EW_ZIP",
  file.path(cache_dir, "odwp01ew.zip")
)
output_path <- Sys.getenv(
  "CENSUS_TRAVEL_TO_WORK_OUTPUT",
  file.path("data-raw", "derived", "census_msoa_OD_travel2work.csv.gz")
)

download_if_missing <- function(url, path) {
  if (file.exists(path)) {
    return(invisible(path))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  message("Downloading ", url)
  utils::download.file(url, path, mode = "wb", quiet = FALSE)
  invisible(path)
}

download_if_missing(zenodo_mpd_url, mpd_path)
download_if_missing(nomis_odwp01_url, census_zip)

message("Reading MPD travel-to-work support: ", mpd_path)
mpd_od <- readr::read_csv(mpd_path, show_col_types = FALSE) |>
  transmute(
    origin = as.character(.data$MSOA21CD_home),
    destination = as.character(.data$MSOA21CD_work),
    flow = as.numeric(.data$count)
  ) |>
  filter(
    grepl("^[EW][0-9]{8}$", .data$origin),
    grepl("^[EW][0-9]{8}$", .data$destination),
    is.finite(.data$flow),
    .data$flow > 0
  )

mpd_origins <- unique(mpd_od$origin)
mpd_destinations <- unique(mpd_od$destination)

message("Reading Census 2021 ODWP01EW_MSOA.csv from: ", census_zip)
census_raw <- readr::read_csv(
  unz(census_zip, "ODWP01EW_MSOA.csv"),
  show_col_types = FALSE
)

message("Extracting matching Census workplace OD matrix")
census_od <- census_raw |>
  transmute(
    origin = as.character(.data$`Middle layer Super Output Areas code`),
    destination = as.character(.data$`MSOA of workplace code`),
    place_of_work_indicator = as.integer(.data$`Place of work indicator (4 categories) code`),
    flow = as.numeric(.data$Count)
  ) |>
  filter(
    .data$place_of_work_indicator == 3L,
    .data$origin %in% mpd_origins,
    .data$destination %in% mpd_destinations,
    grepl("^[EW][0-9]{8}$", .data$origin),
    grepl("^[EW][0-9]{8}$", .data$destination),
    is.finite(.data$flow),
    .data$flow > 0
  ) |>
  group_by(.data$origin, .data$destination) |>
  summarise(flow = sum(.data$flow, na.rm = TRUE), .groups = "drop") |>
  arrange(.data$origin, .data$destination)

if (nrow(census_od) == 0L) {
  stop("No matching Census OD flows found for the MPD MSOA support.")
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(census_od, output_path)

message("Wrote: ", output_path)
message("Rows: ", nrow(census_od))
message("Origins: ", dplyr::n_distinct(census_od$origin))
message("Destinations: ", dplyr::n_distinct(census_od$destination))
