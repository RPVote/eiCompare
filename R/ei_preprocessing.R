#' Internal function that checks for adequate closeness between sums
#' of race/candidate columns and provided vote totals.
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
#' @author Ari Decter-Frain
#' 
#' @param vote_sums A numeric vector containing the sums of votes by each race/ethnicity
#' @param provided_totals A numeric vector containing the totals provided to the clean_race function
#' @param max_dev A flaoting point object passed from clean_race
#' @param avg_dev A floating point object passed from clean_race
#' 
#' @keyword internal
#' 
#' @return A list containing two objects, 'closeness' and 'deviates'. See details for more information
check_diffs <- function(vote_sums, provided_totals, max_dev, avg_dev) {
  
  # check max_dev, avg_dev validity
  if((max_dev < 0) | (avg_dev < 0)) {
    stop(
      "max_dev and avg_dev must be greater than 0"
    )
  }
  
  # get deviation info
  diff_prps <- abs(vote_sums - provided_totals) / provided_totals
  deviates <- diff_prps != 0
  max <- max(diff_prps)
  avg <- mean(diff_prps)
  
  # check conditions
  output <- list(closeness = 2, deviates = deviates)
  if (all(diff_prps == 0)) {
    return(output)
  } else if (max < max_dev & avg < avg_dev) {
    output$closeness <- 1
    return(output)
  } else {
    output$closeness <- 0
    return(output)
  }
}

#' Computes proportions for returning by clean_race and clean_cand
#' 
#' @author Ari Decter-Frain
#' 
#' @param votes A dataframe of raw votes by race or candidates
#' @param totals A numeric vector of totals on which to standardize
#' 
#' @keyword internal
#' 
#' @return A dataframe of standardized proportions whose columns sum rowwise to 1
standardize_votes <- function(votes) {
  prps <- votes / rowSums(votes)
  names(prps) <- paste(names(prps), "p", sep = "_")
  return(prps)
}

#' Converts raw vote totals from different voter groups / 
#' candidates across precincts into proportions, checking 
#' for problematic differences between known vote totals 
#' and sums across race/ethnicities.
#' 
#' If turnout columns sum row-wise to equal vote_totals, they are 
#' returned as proportions. 
#'  
#' If turnout columns sum row-wise to sufficiently close to 
#' vote_totals, they are returned as proportions of the sums.
#'  
#' If turnout columns sum row-wise exceedingly far from 
#' vote_totals, the function stops and returns an error message.
#'
#' @author Ari Decter-Frain
#'
#' @param data A dataframe of election results, where each row represents a precinct or geographic voting unit
#' @param cols A character vector with the names of the columns indicating total votes cast by each race, or for each candidate
#' @param totals_col A character string with the name of the total vote count column in the data. If null, total votes are computed within the function
#' @param max_dev A numeric object setting the max allowable deviation of a precinct's vote sum from totals
#' @param avg_dev A numeric object setting the max allowable average deviation difference of all precints' vote sums from totals
#' @param verbose A boolean indicating whether to print status messages
#' @param diagnostic A boolean. When true, an extra column of booleans is returned indicating whether each row had a deviation from totals
#'
#' @export
#' 
#' @return A dataframe with proportions corresponding to the turnout of each race/ethnicity group
stdize_votes <- function(
    data,
    cols, 
    totals_col = NULL, 
    max_dev = 0.1, 
    avg_dev = 0.025, 
    verbose = TRUE,
    diagnostic = FALSE
) {

  # get votes by race, sum of votes
  votes <- data[, cols]
  vote_sums <- rowSums(votes)

  if (is.null(totals_col)) {

    # if no vote totals_col passed, use vote_sums for totals
    proportions <- standardize_votes(votes)
    return(proportions)
    
  # else check the extent of deviation from provided totals
  } else {
    vote_totals <- data[, totals_col]
    diff_check <- check_diffs(
      vote_sums, vote_totals, max_dev, avg_dev
    )
    closeness <- diff_check$closeness
    if (closeness == 2) {
      if (verbose == TRUE) {
        message(
          "All columns sum correctly. Computing proportions..."
        )
      }
      proportions <- standardize_votes(votes)
      if (diagnostic == TRUE) {
        proportions$deviates <- diff_check$deviates 
      }
      return(proportions)
      
    } else if (closeness == 1) {
      if (verbose == TRUE) {
        message(
          "Vote sums deviate from totals.\nDeviations are minor. Restandardizing vote columns..."
        )
      }
      proportions <- standardize_votes(votes)
      if (diagnostic == TRUE) {
        proportions$deviates <- diff_check$deviates 
      }
      return(proportions)
      
    } else if (closeness == 0) {
      warning(
        "Precinct vote sums are too far from totals.\n  Returning boolean column identifying problematic rows..."
      )
      return(data.frame('deviates' = diff_check$deviates))
    }
  }
}

#' Converts a dataframe with total votes for candidates and total votes
#' by each racial/ethnic group into proportions that can be used for 
#' Ecological Inference analysis
#'
#' @author Ari Decter-Frain
#'
#' @param data A dataframe of election results, where each row represents a precinct or geographic voting unit
#' @param race_cols A character vector of colnames corresponding to turnout counts of each race/ethnicity group
#' @param cand_cols A character vector of colnames corresponding to turnout counts of voters for each candidate
#' @param totals_from A character string, either "cand" or "race" to set whether totals are computed from candidate turnout or race/ethnicity turnout columns. Ignored if totals_col provided.
#' @param totals_col A character string with the name of the total vote count column in the data. If null, total votes are computed within the function
#' @param max_dev_race A numeric object setting the max allowable deviation of any one precincts' sum of race columns from totals
#' @param max_dev_cand A numeric object setting the max allowable deviation of any one precincts' sum of candidate columns from totals
#' @param avg_dev_race A numeric object setting the max allowable mean deviation of all precincts' sum of race columns from totals
#' @param avg_dec_cand A numeric object setting the max allowable mean deviation of all precincts' sum of candidate columns from totals
#' @param verbose A boolean. When true, function returns progress messages.
#' @param diagnostic A boolean. When true, an extra column of booleans is returned indicating whether each row had a deviation from totals
#' 
#' @export
#' 
#' @return A dataframe containing columns for each race and candidate converted to percentages, ready for Ecological Inference
stdize_votes_all <- function(
    data,
    race_cols,
    cand_cols,
    totals_from = "cand",
    totals_col = NULL,
    max_dev_race = 0.1,
    max_dev_cand = 0.1,
    avg_dev_race = 0.025,
    avg_dev_cand = 0.025,
    verbose = T,
    diagnostic = F
) {
  
  if(is.null(totals_col) & totals_from == "cand") {
    data$cand_totals <- rowSums(data[, cand_cols])
    cand_totals_col = NULL
    race_totals_col = "cand_totals"
  } else if (is.null(totals_col) & totals_from == 'race') {
    data$race_totals <- rowSums(data[, race_cols])
    cand_totals_col = "race_totals"
    race_totals_col = NULL
  } else if (!is.null(totals_col)) {
    cand_totals_col = totals_col
    race_totals_col = totals_col
  } else {
    stop("You must either define a totals column with totals_col, 
       or set totals_from equal to 'cand' or 'race'")
  }
        
  cand_prps <- stdize_votes(
    data = data,
    cols = cand_cols, 
    totals_col = cand_totals_col, 
    max_dev = max_dev_cand, 
    avg_dev = avg_dev_cand, 
    verbose = verbose,
    diagnostic = diagnostic  
  )
    
  race_prps <- stdize_votes(
    data = data,
    cols = race_cols, 
    totals_col = race_totals_col, 
    max_dev = max_dev_race, 
    avg_dev = avg_dev_race, 
    verbose = verbose,
    diagnostic = diagnostic
  )
    
  all_proportions <- cbind(cand_prps, race_prps)
  return(all_proportions)
}