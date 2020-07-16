#' Internal function that checks for adequate closeness between sums
#' of race columns and provided vote totals.
#' 
#' This function checks conditions inputted to clean_race by the user
#' 
#' When this function returns 0, votes do not sum close enough 
#' to the provided vote totals. 
#' 
#' When this function returns 1, votes sum  marginally away 
#' from the provided totals. Restandardization is appropriate
#'  
#' When this function returns 2, votes sum exactly to vote
#' totals 
#' 
#' @param vote_sums A numeric vector containing the sums of votes by each race/ethnicity
#' @param provided_totals A numeric vector containing the totals provided to the clean_race function
#' @param max_dev A flaoting point object passed from clean_race
#' @param avg_dev A floating point object passed from clean_race
#' 
#' @return A numeric vector 0, 1, or 2. 

check_race_diffs <- function(vote_sums, provided_totals, max_dev, avg_dev) {
  
  # check max_dev, avg_dev validity
  if((max_dev < 0) | (avg_dev < 0)) {
    stop(
      "max_dev and avg_dev must be greater than 0"
    )
  }
  
  # get max and avg deviations
  diff_prps <- abs(vote_sums - provided_totals) / provided_totals
  max <- max(diff_prps)
  avg <- mean(diff_prps)
  
  # check conditions
  if (all(diff_prps == 0)) {
    return(2)
  } else if (max < max_dev & avg < avg_dev) {
    return(1)
  } else {
    return(0)
  }
}


#' Computes proportions for returning by clean_race and clean_cand
#' 
#' @param votes A dataframe of raw votes by race or candidates
#' @param totals A numeric vector of totals on which to standardize
#' 
#' @return A dataframe of standardized proportions whose columns sum rowwise to 1

standardize_votes <- function(votes) {
  prps <- votes / rowSums(votes)
  names(prps) <- paste(names(prps), "prp", sep = "_")
  return(prps)
}

#' Converts raw vote totals from different race/ethnicies across precincts 
#' into proportions, checking for problematic differences between known
#' vote totals and sums across race/ethnicities.
#' 
#' If race turnout columns sum row-wise to equal vote_totals, they are 
#' returned as proportions. 
#'  
#' If race turnout columns sum row-wise to sufficiently close to 
#' vote_totals, they are returned as proportions of the sums.
#'  
#' If race turnout columns sum row-wise exceedingly far from 
#' vote_totals, the function stops and returns an error message.
#'
#' @param data A dataframe of election results, where each row represents a precinct or geographic voting unit.
#' @param cols A character vector with the names of the columns indicating total votes cast by each race
#' @param totals_col A character string with the name of the total vote count column in the data. If null, total votes are computed within the function.
#' @param max_dev A numeric type object indicating the maximum allowable deviation of a precinct's vote sum from the totals in totals_col.
#' @param avg_dev A numeric type object indicating the maximum average deviation difference of all precints' vote sums from the totals in totals_col.
#' @param verbose A boolean indicating whether to print warnings and messages.
#'
#' @export
#' 
#' @return A dataframe with proportions corresponding to the turnout of each race/ethnicity group.

clean_race <- function(data,
                       cols, 
                       totals_col = NULL, 
                       max_dev = 0.1, 
                       avg_dev = 0.025, 
                       verbose = TRUE) {

  # get votes by race, sum of votes
  votes <- data[, cols]
  vote_sums <- rowSums(votes)

  if (is.null(totals_col)) {

    # if no vote totals_col passed, use vote_sums for totals
    proportions <- eiCompare::standardize_votes(votes)
    return(proportions)
  } else {
    vote_totals <- data[, totals_col]
    closeness <- eiCompare::check_race_diffs(vote_sums,
                                             vote_totals,
                                             max_dev,
                                             avg_dev)

    if (closeness == 2) {
      if (verbose == TRUE) {
        print(
          "All race columns sum correctly. Computing proportions..."
        )
      }
      proportions <- eiCompare::standardize_votes(votes)
      return(proportions)
      
    } else if (closeness == 1) {
      if (verbose == TRUE) {
        warning(
          "Precinct vote sums deviate from provided totals. 
          Deviations are minor. Restandardizing vote columns..."
        )
      }
      proportions <- eiCompare::standardize_votes(votes)
      return(proportions)
      
    } else if (closeness == 0) {
      stop(
        "Precinct vote sums are too far from the totals. 
        Recheck your data, or leave vote_totals blank. 
        Type ?prep_race for more information."
      )
    }
  }
}