#' Extract relevant geographic units from 15-digit FIPS code
#' 
#' Extract relevant geographic units from 15-digit FIPS code
#' 
#' 
#' @param fips Character vector. 15-digit FIPS code
#' @return List with separate datasets of length fips, including values for
#' fips, state, county, tract, block, state-county, and state-county-tract.
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @examples
#' 
#'   
#' 
#' 
#' @export unit_comb_extract
unit_comb_extract <- function(fips) {

  # NA in FIPS -- just make everything NA
  if (is.na(fips)) {
    return(data.frame(
      fips = NA, st = NA, county = NA, tract = NA, block = NA, st_cty = NA,
      st_cty_tract = NA, stringsAsFactors = F
    ))
  }
  fips <- fips
  unit_extract <- function(fips, keep) {
    unit <- unlist(data.table::tstrsplit(fips, "", keep = keep))
    paste(unit, collapse = "")
  }
  # Place Appropriate elements together:
  st <- unit_extract(fips, 1:2)
  county <- unit_extract(fips, 3:5)
  tract <- unit_extract(fips, 6:11)
  block <- unit_extract(fips, 12:15)
  st_cty <- unit_extract(fips, 1:5)
  st_cty_tract <- unit_extract(fips, 1:11)
  # Return as Data frame suitable for lapply
  return(data.frame(
    fips = fips, st = st, county = county, tract = tract, block = block,
    st_cty = st_cty, st_cty_tract = st_cty_tract, stringsAsFactors = F
  ))
}
