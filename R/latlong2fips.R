latlong2fips <- function(latitude, longitude, number) {
  cat("Communicating with geo.fcc.gov...\n")
  url <- paste("https://geo.fcc.gov/api/census/block/find?latitude=", latitude, "&longitude=", longitude, "&showall=true&format=json", sep = "")
  url <- sprintf(url, latitude, longitude)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  if (sjmisc::is_empty(json$Block$FIPS)) { # error here
    cat(paste("Probably Bad LAT/LONG Coordinate. Couldn't calculate.\nRow:", number, sep = " "))
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
