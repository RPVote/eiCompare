#' Latitude-Longitude Coordinates to FIPS Geocode
#'
#' Converts latitude/longitude coordinates to 15-digit FIPS code. Communicates
#' with FCC API.
#'
#'
#' @param latitude Numeric. Latitude coordinate.
#' @param longitude Numeric. Longitude coordinate.
#' @param number Numeric. Usually part of a loop index counter
#' @return Character string 15-digit FIPS code corresponding to Lat/Long entry
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references https://geo.fcc.gov/api/census/block/
#' @examples
#' \dontrun{
#' # EXAMPLE: NOT RUN #
#' # census_block <- list()
#' # num_catch <- rep(NA, nrow(nom_geo))
#'
#' # for (i in 1:nrow(nom_geo)) {
#'
#' #  census_block[[i]] <- latlong2fips(nom_geo$lat[i], nom_geo$lon[i], i)
#' # }
#'
#' # Row Bind the list into a data.frame object #
#' # fips_df <- rbindlist(census_block)
#' }
#'
#' @export latlong2fips
latlong2fips <- function(latitude, longitude, number) {
  if (!requireNamespace("RJSONIO", quietly = TRUE)) {
    stop("This utility requires RJSONIO. Please install and re-run.")
  }
  cat("Communicating with geo.fcc.gov...\n")
  url <- sprintf(
    "https://geo.fcc.gov/api/census/block/find?latitude=%f&longitude=%f&showall=true&format=json",
    as.numeric(latitude), as.numeric(longitude)
  )
  json <- RJSONIO::fromJSON(url)

  if (is.null(json$Block$FIPS) || length(json$Block$FIPS) == 0) { # error here
    warning("Probably Bad LAT/LONG Coordinate. Couldn't calculate.\nRow:", number, )
    return(data.frame(row_id = number, FIP = NA, stringsAsFactors = F))
  }
  if (json$status != "OK") {
    FIP <- "CONVERGE-FAIL"
  } else {
    FIP <- as.character(json$Block["FIPS"])
  }
  return(data.frame(
    row_id = number, FIP = FIP,
    stringsAsFactors = F
  ))
}
