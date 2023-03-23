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
#'
#' @export latlong2fips
latlong2fips <- function(latitude, longitude, number) {
  message("Communicating with geo.fcc.gov...\n")
  url <- paste("https://geo.fcc.gov/api/census/block/find?latitude=", latitude, "&longitude=", longitude, "&showall=true&format=json", sep = "")
  url <- sprintf(url, latitude, longitude)

  if (requireNamespace("RJSONIO", quietly = TRUE)) {
    json <- RJSONIO::fromJSON(url)
  } else {
    message("This utility requires RJSONIO. Please install and re-run.")
  }

  if (length("json$Block$FIPS") == 0 | is.null(json$Block$FIPS)) { # error here
    message(paste("Probably Bad LAT/LONG Coordinate. Couldn't calculate.\nRow:", number, sep = " "))
    return(data.frame(row_id = number, FIP = NA, stringsAsFactors = F))
  } else {
    if (json$status == "OK") {
      number <- number
    } else {
      number <- "CONVERGE-FAIL"
    }
    return(data.frame(
      row_id = number, FIP = as.character(json$Block["FIPS"]),
      stringsAsFactors = F
    ))
  }
}
