#' Counts the number of words per row in the column of a dataframe.
#'
#' A "word" is defined as a string of alphabetical characters separated by
#' either spaces or dashes (but not other special characters).
#'
#' @param voter_file The voter file, with each row consisting of a voter.
#' @param surname_col A string denoting the surname column.
#' @param regex A string denoting the regular expression to use for querying the
#'   the word count.
#' @return A vector of word counts.
#'
#' @export get_word_count
#' @importFrom stringr str_count
get_word_count <- function(voter_file,
                           surname_col = "last_name",
                           regex = "[ -]+") {
  word_count <- 1 + stringr::str_count(voter_file[[surname_col]], regex)
  return(word_count)
}


#' Gets special characters in a column of names.
#'
#' Returns a unique list of special characters found in a column of a dataframe.
#' By default, these characters consist of any that are not upper- or lower-case
#' letters. This preference can be overwritten by providing a new regular
#' expression.
#'
#' @param voter_file The voter file, with each row consisting of a voter.
#' @param surname_col A string denoting the surname column.
#' @param regex A string denoting the regular expression to use for identifying
#'  non-special characters (by default, alphabetic characters).
#' @return A vector of unique special characters found in the names.
#'
#' @export get_unique_special_characters
#' @importFrom stringr str_c str_replace_all str_split
get_unique_special_characters <- function(voter_file,
                                          surname_col = "last_name",
                                          regex = "[A-Za-z]") {
  # Replace all alphabetic characters with empty strings
  characters <- stringr::str_replace_all(voter_file[[surname_col]], regex, "")
  # Combine all strings together
  characters <- stringr::str_c(characters, collapse = "")
  # Split up by individual character, taking unique ones
  characters <- stringr::str_split(characters, pattern = "")[[1]]
  characters <- sort(unique(characters))
  return(characters)
}


#' Gets surnames containing special characters.
#'
#' Returns a subsetted voter file whose rows consist of voters that have
#' special characters in their last name.
#'
#' @param voter_file The voter file, with each row consisting of a voter.
#' @param surname_col A string denoting the surname column.
#' @param regex A string denoting the regular expression to use for querying the
#'   the special characters.
#' @return A dataframe of voters whose surname has special characters.
#'
#' @export get_special_character_surnames
#' @importFrom stringr str_detect
get_special_character_surnames <- function(voter_file,
                                           surname_col = "last_name",
                                           regex = "[^A-Za-z]") {
  special_characters <- stringr::str_detect(voter_file[[surname_col]], regex)
  return(voter_file[special_characters, ])
}


#' Strips special characters from a voter file.
#'
#' Given a voter file and a column, returns a voter file with special characters
#' stripped stripped from that column.
#'
#' @param voter_file The voter file, with each row consisting of a voter.
#' @param surname_col A string denoting the surname column.
#' @param regex A string denoting the regular expression to use for denoting the
#'   the special characters.
#' @param replace The replacement string for special characters.
#' @return A dataframe of voters whose surname column is stripped of special
#'  characters.
#'
#' @export strip_special_characters
#' @importFrom stringr str_replace_all
strip_special_characters <- function(voter_file,
                                     surname_col = "last_name",
                                     regex = "[^A-Za-z]+",
                                     replace = " ") {
  # Replace special characters with empty spaces
  voter_file[[surname_col]] <- stringr::str_replace_all(
    voter_file[[surname_col]],
    regex,
    replace
  )
  return(voter_file)
}


#' Gets multi-barreled surnames from a voter file.
#'
#' A multi-barreled surname is one containing a dash or a space. This function
#' finds all multi-barreled surnames in a voter file.
#'
#' @param voter_file The voter file, with each row consisting of a voter.
#' @param surname_col A string denoting the surname column.
#' @param regex A string denoting the regular expression to use for denoting the
#'   the special characters.
#' @return A dataframe of voters whose surnames are multi-barreled.
#'
#' @export get_multi_barreled_surnames
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
get_multi_barreled_surnames <- function(voter_file,
                                        surname_col = "last_name",
                                        regex = "[ -]+") {
  multi_barreled <- dplyr::filter(
    voter_file,
    stringr::str_detect(voter_file[[surname_col]], regex)
  )
  return(multi_barreled)
}


#' Determines which surnames match to the Census list.
#'
#' @param voter_file The voter file, with each row consisting of a voter.
#' @param surname_col A string denoting the surname column.
#' @param strip_special Whether to strip special characters before matching in
#'  the surname database.
#' @return A vector of logicals denoting a match or not.
#'
#' @export surname_match
surname_match <- function(voter_file,
                          surname_col = "last_name",
                          strip_special = FALSE) {
  if (strip_special) {
    voter_file <- eiCompare::strip_special_characters(
      voter_file = voter_file,
      surname_col = surname_col,
      replace = ""
    )
  }
  # Determine if there's a surname match
  surname_match <- voter_file[[surname_col]] %in% wru::surnames2010$surname
  return(surname_match)
}


#' Briefly summarizes the surnames in a voter file.
#'
#' @param voter_file The voter file, with each row consisting of a voter.
#' @param surname_col A string denoting the surname column.
#'
#' @export surname_summary
surname_summary <- function(voter_file, surname_col) {
  # Print number of voters
  n_voters <- nrow(voter_file)
  message(paste("Voter file has", n_voters, "voters."))

  # Print number of voters with no last name
  n_nan_surnames <- sum(is.na(voter_file[[surname_col]]))
  message(paste("Voter files has", n_nan_surnames, "voter(s) with no surname."))

  # Print number of voters with special characters
  special_character_surnames <- eiCompare::get_special_character_surnames(
    voter_file = voter_file,
    surname_col = surname_col
  )
  n_special_character_surnames <- nrow(special_character_surnames)
  message(paste(
    "Voter file has", n_special_character_surnames,
    "voters containing special characters."
  ))

  # Print number of matches by default
  n_organic_matches <- sum(eiCompare::surname_match(
    voter_file = voter_file,
    surname_col = surname_col,
    strip_special = FALSE
  ))
  message(paste(
    "Voter file has", n_organic_matches,
    "voters with surnames matching the database by default."
  ))

  # Print number of matches after stripping special characters
  n_stripped_matches <- sum(eiCompare::surname_match(
    voter_file = voter_file,
    surname_col = surname_col,
    strip_special = TRUE
  ))
  message(paste(
    "Voter file has", n_stripped_matches,
    "voters with surnames matching the database after removing",
    "special characters."
  ))
}



#' Predicts, for one row in a voter file, the probability of a voter having a
#' certain race by averaging over each "barrel" of the surname.
#'
#' @param voter_file The voter file, with each row consisting of a voter.
#' @param surname_col A string denoting the surname column.
#' @param surname_only Whether to obtain probabilities for surnames only.
#' @param census_data A data frame containing Census data corresponding to the
#'  geographic information for units in the voter file.
#' @param census_geo The census level at which to apply BISG. Passed to WRU.
#' @param surname_year Which Census year to use for surname matching. Passed to
#'  WRU.
#' @param use_age Whether to use the age in the BISG calculation. Passed to WRU.
#' @param use_sex Whether to use the sex in the BISG calculation. Passed to WRU.
#' @param state A string denoting the state for which the data is queried.
#' @param county A string denoting the column containing the county FIPS code.
#' @param tract A string denoting the column containing the tract FIPS code.
#' @param block A string denoting the column containing the block FIPS code.
#' @param pattern What pattern to split surnames on. By default, surnames are
#'  split on a space(s), which assumes hyphens have already been removed.
#' @param remove_patterns A list of strings which will be removed from the
#'  list of barrels.
#' @return A vector of probabilities for each surname.
#'
#' @export predict_race_multi_barreled
predict_race_multi_barreled <- function(voter_file,
                                        surname_col = "last_name",
                                        surname_only = TRUE,
                                        census_data = NULL,
                                        census_geo = "block",
                                        surname_year = 2010,
                                        use_age = FALSE,
                                        use_sex = FALSE,
                                        state = NULL,
                                        county = NULL,
                                        tract = NULL,
                                        block = NULL,
                                        pattern = "[ -]+",
                                        remove_patterns = NULL) {
  # Split up multi-barreled surnames
  surnames <- stringr::str_split(
    voter_file[[surname_col]],
    pattern = pattern
  )[[1]]
  # Remove specific barrels
  if (!is.null(remove_patterns)) {
    surnames <- surnames[!(surnames %in% remove_patterns)]
  }

  # Use surname only
  if (surname_only) {
    new_voter_file <- data.frame(surname = surnames)
    # Calculate probabilities using surnames only
    probabilities <- suppressWarnings(
      wru::merge_surnames(
        voter.file = new_voter_file,
        surname.year = 2010,
        clean.surname = FALSE,
        impute.missing = TRUE
      )
    )
    probabilities <- as.numeric(colMeans(probabilities[, c(-1, -2)]))
  } else {
    # Transfer geolocations
    new_voter_file <- data.frame(
      surname = surnames,
      state = state,
      county = voter_file[[county]],
      tract = voter_file[[tract]],
      block = voter_file[[block]]
    )

    # Predict race using full BISG
    invisible(capture.output(
      bisg <- suppressWarnings(
        wru::predict_race(
          voter.file = new_voter_file,
          census.surname = TRUE,
          surname.only = FALSE,
          surname.year = surname_year,
          census.geo = census_geo,
          census.data = census_data,
          age = use_age,
          sex = use_sex,
        )
      )
    ))
    probabilities <- as.numeric(colMeans(bisg[, c(6:10)]))
  }
  return(probabilities)
}
