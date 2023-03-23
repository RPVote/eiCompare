#' Sum row-wise over columns in a dataframe
#'
#' Simple wrapper of rowSums for checking row sums of race, candidate columns
#'
#' @param data A data.frame() object containing precinct-level turnout data by
#' race and candidate
#' @param cols A set of columns to sum over. Typically, enter cand_cols or
#' race_cols here.
#'
#' @export
#'
#' @return A vector of row-wise sums across the column vector entered as
#' argument.
sum_over_cols <- function(data, cols) {
  return(rowSums(data[, cols]))
}


#' Remove or identify duplicated precincts
#'
#' Removes any rows in the dataset that are fully duplicated. If necessary, adds
#'' duplicates' column indicating where precincts appear duplicated, for manual
#' inspection by the user
#'
#' @author Ari Decter-Frain <agd75@@cornell.edu>
#'
#' @param data A data.frame() object containing precinct-level turnout data by
#' race and candidate
#' @param id_cols The name or index of the column in the data containing
#' unique precinct identifiers. Can pass multiple column names or indices in
#' a vector if precincts are identified over multiple columns
#' (eg. c("precinctid", "countyid")).
#' @param verbose A boolean. If true, messages are returned describing actions
#' taken by the function.
#'
#' @export
#'
#' @return A new dataframe without duplicated rows, and (if any) a boolean
#' column identifying duplicated precincts for further investigation.

dedupe_precincts <- function(data, id_cols, verbose = TRUE) {

  # Remove any fully duplicated rows
  init_rows <- nrow(data)
  data <- unique(data)
  rows <- nrow(data)
  rows_removed <- init_rows - rows

  # Inform user
  if (rows_removed != 0 & verbose) {
    message(paste("Removing", rows_removed, "identical rows..."))
  }

  # Check for duplicate precincts
  # Check both directions to get every duplicate
  dupes <- duplicated(data[, id_cols]) |
    duplicated(data[, id_cols],
      fromLast = TRUE
    )

  # If duplicates found, add boolean column identifying them
  if (sum(dupes) != 0 & verbose) {
    data$duplicate <- dupes

    # Inform user
    warning("Precincts appear duplicated. Returning boolean column identifying
duplicates...")
  }

  # Inform user if no duplicates
  if ((rows_removed == 0) & (sum(dupes) == 0) & verbose) {
    message("Data does not contain duplicates. Proceed...")
  }

  return(data)
}

#' Remove / Impute NAs in an EI dataset
#'
#' @param data A data.frame() object containing precinct-level turnout data by
#' race and candidate
#' @param cand_cols A character vector listing the column names for turnout for
#' each candidate
#' @param race_cols A character vector listing the column names for turnout by
#' race
#' @param totals_col The name of the column containing total votes cast in each
#' precinct
#' @param na_action A string indicating how to handle missing values in EI
#' columns. Possible values are "DROP" and "MEAN". "DROP" drops all rows
#' where variables are missing. "MEAN" imputes missing values as the mean of the
#' column
#' @param verbose A boolean indicating whether to give status updates
#' @return A dataframe of inputs to ecological inference without any missing values.
#' @export
resolve_missing_vals <- function(
                                 data,
                                 cand_cols,
                                 race_cols,
                                 totals_col,
                                 na_action = "DROP",
                                 verbose = TRUE) {
  cols <- c(cand_cols, race_cols, totals_col)
  na_action <- toupper(na_action)

  # If "drop" selected
  if (na_action == "DROP") {
    n_rows <- nrow(data)

    # Drop rows with NAs in any necessary column
    data <- data[rowSums(is.na(data[, cols])) == 0, ]
    n_rows_new <- nrow(data)
    n_dropped <- n_rows - n_rows_new
    if (verbose) {
      if (n_dropped == 0) {
        message(
          "No missing values in key columns. Returning original dataframe..."
        )
      } else {
        message(
          paste(
            "Removing", n_dropped, "rows with missing values in key columns..."
          )
        )
      }
    }

    # If "mean" selected
  } else if (na_action == "MEAN") {
    ei_data <- data[, cols]
    n_missing <- 0

    for (i in 1:ncol(ei_data)) {
      missing <- is.na(ei_data[, i])

      # Update counter
      n_missing <- n_missing + sum(missing)

      # Replace missings with column mean
      ei_data[missing, i] <- mean(ei_data[, i], na.rm = TRUE)
    }

    # Put imputed data back into dataset
    data[, cols] <- ei_data

    if (verbose) {
      if (n_missing == 0) {
        message(
          "No missing values in key columns. Returning original dataframe..."
        )
      } else {
        message(
          paste(
            "Imputing", n_missing, "missing values across key columns..."
          )
        )
      }
    }
  } else {
    stop(
      "Invalid entry for na_action parameter.\nEnter either 'DROP' or 'MEAN'"
    )
  }
  return(data)
}


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
#' @param vote_sums A numeric vector containing the sums of votes by each
#'   race/ethnicity
#' @param provided_totals A numeric vector containing the totals provided to the
#'   clean_race function
#' @param max_dev A floating point object passed from clean_race
#' @param avg_dev A floating point object passed from clean_race
#'
#' @keywords internal
#'
#' @return A list containing two objects, 'closeness' and 'deviates'. See
#'   details for more information.
check_diffs <- function(vote_sums,
                        provided_totals,
                        max_dev = 0.1,
                        avg_dev = 0.25) {

  # Check max_dev, avg_dev validity
  if ((max_dev < 0) | (avg_dev < 0)) {
    stop(
      "max_dev and avg_dev must be greater than 0"
    )
  }

  # Get deviation info
  diff_prps <- abs(vote_sums - provided_totals) / provided_totals
  deviates <- diff_prps != 0
  max <- max(diff_prps)
  avg <- mean(diff_prps)

  # Check conditions
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
#' @param new_names A boolean indicating whether to return cand and race columns
#' with the same names. If FALSE, names returned with "_prop" added on.
#'
#' @keywords internal
#'
#' @return A dataframe of standardized proportions whose columns sum rowwise to
#' 1
standardize_votes <- function(votes, new_names = FALSE) {
  totals <- rowSums(votes)
  prps <- votes / totals
  if (new_names) {
    names(prps) <- paste(names(prps), "prop", sep = "_")
  }
  output <- cbind(prps, data.frame("total" = totals))
  return(output)
}

#' stdize_votes
#'
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
#' @param data A dataframe of election results, where each row represents a
#' precinct or geographic voting unit
#' @param cols A character vector with the names of the columns indicating total
#'  votes cast by each race, or for each candidate
#' @param totals_col A character string with the name of the total vote count
#' column in the data. If null, total votes are computed within the function
#' @param max_dev A numeric object setting the max allowable deviation of a
#' precinct's vote sum from totals
#' @param avg_dev A numeric object setting the max allowable average deviation
#' difference of all precints' vote sums from totals
#' @param new_names A boolean indicating whether to return cand and race columns
#' with the same names. If FALSE, names returned with "_prop" added on.
#' @param verbose A boolean indicating whether to print status messages
#' @param diagnostic A boolean. When true, an extra column of booleans is
#' returned indicating whether each row had a deviation from totals
#'
#' @export
#'
#' @return A dataframe with proportions corresponding to the turnout of each
#' race/ethnicity group
stdize_votes <- function(data,
                         cols,
                         totals_col = NULL,
                         max_dev = 0.1,
                         avg_dev = 0.025,
                         new_names = FALSE,
                         verbose = TRUE,
                         diagnostic = FALSE) {

  # Set data as dataframe
  data <- as.data.frame(data)

  # Get votes by race, sum of votes
  votes <- data[, cols]
  vote_sums <- rowSums(votes)

  if (is.null(totals_col)) {

    # If no vote totals_col passed, use vote_sums for totals
    proportions <- standardize_votes(votes, new_names)
    return(proportions)

    # Else check the extent of deviation from provided totals
  } else {
    vote_totals <- data[, totals_col]
    diff_check <- check_diffs(
      vote_sums, vote_totals, max_dev, avg_dev
    )
    closeness <- diff_check$closeness
    if (closeness == 2) {
      if (verbose) {
        message(
          "All columns sum correctly. Computing proportions..."
        )
      }
      proportions <- standardize_votes(votes, new_names)
      if (diagnostic) {
        proportions$deviates <- diff_check$deviates
      }
      return(proportions)
    } else if (closeness == 1) {
      if (verbose) {
        message(
          paste(
            "Vote sums deviate from totals.\nDeviations are minor.",
            "Restandardizing vote columns..."
          )
        )
      }
      proportions <- standardize_votes(votes, new_names)
      if (diagnostic) {
        proportions$deviates <- diff_check$deviates
      }
      return(proportions)
    } else if (closeness == 0) {
      warning(
        paste(
          "Precinct vote sums are too far from totals.\n",
          "Returning boolean column identifying problematic rows..."
        )
      )
      return(data.frame("deviates" = diff_check$deviates))
    }
  }
}

#' stdize_votes_all
#'
#' Converts a dataframe with total votes for candidates and total votes
#' by each racial/ethnic group into proportions that can be used for
#' Ecological Inference analysis
#'
#' @author Ari Decter-Frain
#'
#' @param data A dataframe of election results, where each row represents a
#' precinct or geographic voting unit
#' @param race_cols A character vector of colnames corresponding to turnout
#' counts of each race/ethnicity group
#' @param cand_cols A character vector of colnames corresponding to turnout
#' counts of voters for each candidate
#' @param totals_from A character string, either "cand" or "race" to set whether
#' totals are computed from candidate turnout or race/ethnicity turnout columns.
#' Ignored if totals_col provided.
#' @param totals_col A character string with the name of the total vote count
#' column in the data. If null, total votes are computed within the function
#' @param max_dev_race A numeric object setting the max allowable deviation of
#' any one precincts' sum of race columns from totals
#' @param max_dev_cand A numeric object setting the max allowable deviation of
#' any one precincts' sum of candidate columns from totals
#' @param avg_dev_race A numeric object setting the max allowable mean deviation
#' of all precincts' sum of race columns from totals
#' @param avg_dev_cand A numeric object setting the max allowable mean deviation
#' of all precincts' sum of candidate columns from totals
#' @param new_names A boolean indicating whether to return cand and race columns
#' with the same names. If FALSE, names returned with "_prop" added on.
#' @param ignore_devs A boolean. When true, columns are standardized ignoring
#' all deviations from totals
#' @param verbose A boolean. When true, function returns progress messages.
#' @param diagnostic A boolean. When true, an extra column of booleans is
#' returned indicating whether each row had a deviation from totals
#'
#' @export
#'
#' @return A dataframe containing columns for each race and candidate converted
#' to percentages and a totals column, ready for Ecological Inference
stdize_votes_all <- function(data,
                             race_cols,
                             cand_cols,
                             totals_from = "cand",
                             totals_col = NULL,
                             max_dev_race = 0.1,
                             max_dev_cand = 0.1,
                             avg_dev_race = 0.025,
                             avg_dev_cand = 0.025,
                             new_names = FALSE,
                             ignore_devs = FALSE,
                             verbose = TRUE,
                             diagnostic = FALSE) {

  # Use different totals depending on user input.
  if (is.null(totals_col) & totals_from == "cand") {
    if (verbose) {
      message("Computing totals from candidate columns...")
    }
    data$cand_totals <- rowSums(data[, cand_cols])
    cand_totals_col <- NULL
    race_totals_col <- "cand_totals"
  } else if (is.null(totals_col) & totals_from == "race") {
    if (verbose) {
      message("Computing totals from race columns...")
    }
    data$race_totals <- rowSums(data[, race_cols])
    cand_totals_col <- "race_totals"
    race_totals_col <- NULL
  } else if (!is.null(totals_col)) {
    if (verbose) {
      message("Using provided totals...")
    }
    cand_totals_col <- totals_col
    race_totals_col <- totals_col
  } else {
    stop(
      paste(
        "You must either define a totals column with totals_col,\n",
        "or set totals_from equal to 'cand' or 'race'"
      )
    )
  }

  # If ignore devs, set to Inf to pass checks
  if (ignore_devs) {
    max_dev_race <- Inf
    max_dev_cand <- Inf
    avg_dev_race <- Inf
    avg_dev_cand <- Inf
    verbose = FALSE
  }

  if (verbose) {
    message("Standardizing candidate columns...")
  }
# Get candidate standardized proportions
  cand_prps <- stdize_votes(
    data = data,
    cols = cand_cols,
    totals_col = cand_totals_col,
    max_dev = max_dev_cand,
    avg_dev = avg_dev_cand,
    new_names = new_names,
    verbose = verbose,
    diagnostic = diagnostic
  )

  if (verbose) {
    message("Standardizing race columns...")
  }
  # Get race standardized proportions
  race_prps <- stdize_votes(
    data = data,
    cols = race_cols,
    totals_col = race_totals_col,
    max_dev = max_dev_race,
    avg_dev = avg_dev_race,
    new_names = new_names,
    verbose = verbose,
    diagnostic = diagnostic
  )

  # Adjust deviation column names to allow for two
  if (names(cand_prps)[1] == "deviates") {
    names(cand_prps)[1] <- "cand_deviates"
  }
  if (names(race_prps)[1] == "deviates") {
    names(race_prps)[1] <- "race_deviates"
  }
  if (diagnostic) {
    names(cand_prps)[length(cand_prps)] <- "cand_deviates"
    names(race_prps)[length(cand_prps)] <- "race_deviates"
  }

  # Set appropriate order of columns in output
  if (totals_from == "cand") {
    all_proportions <- cbind(cand_prps, race_prps)
  } else {
    all_proportions <- cbind(race_prps, cand_prps)
  }

  # Delete extra totals column if there are two
  if (sum(names(all_proportions) == "total") > 1) {
    all_proportions <-
      all_proportions[, -match("total", names(all_proportions))]
  }

  # Rename totals column to the original inputed name
  if (!new_names) {
    if (!is.null(totals_col)) {
      totals_index <- which(colnames(all_proportions) == "total")
      all_proportions$total <- data[, totals_col]
      colnames(all_proportions)[totals_index] <- totals_col
    }
  }

  return(all_proportions)
}
