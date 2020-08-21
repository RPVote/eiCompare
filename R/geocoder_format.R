#' Pre-processes voter file by checking zipcode, and any special
#' characters or typos within the address.
#'
#'
#' @param voter_file A voter file containing the address of the voter.
#' @param address The voter address in the structure of street
#' number-street name-city-state-zipcode
#' @param delimiter The symbol/character used to parse address."space"
#' is used to indicate that the address is parsed by spaces.
#' @return The voter file with pre-processed format for each address
#' variable.
#'
#' @export split_add_nocommas
#'
#' @importFrom utils tail
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Juandalyn Burke <jcburke@@uw.edu>
split_add_nocommas <- function(voter_file,
                               address = "address",
                               delimiter = "space") {

  # If the voter file contains a one-line address (i.e. with street name,
  # city, state, and zipcode) with no commas, parse out the address
  # in columns
  if (!is.na(voter_file[[address]]) & delimiter == "space") {

    # Split address
    split_add <- strsplit(voter_file[[address]])

    # Assign the last value to the zipcode column
    voter_file$zipcode <- voter_file[[address]] %>%
      strsplit(" ") %>%
      sapply(utils::tail, 1)

    # Split the address and assign the second to last value to the state column
    voter_file$state <- sub(
      ".*\\b([A-Z]{2}) \\d{5}.*",
      "\\1",
      voter_file[[address]]
    )

    # Select every part of the address except the city, state, and zipcode
    voter_file$split_add_city <- gsub(
      "[[:alpha:]]{2}[[:space:]][[:digit:]]{5}",
      "",
      address
    )
  }
  # NOTE: Plan to try and come up with a way to split the street and city
}


#' Pre-processes voter file by checking zipcode, and any special
#' characters or typos within the address.
#'
#' @param voter_file A voter file containing the address of the voter.
#' @param address Either "single" or "split". single addresses are one
#' line address that include street, city, state, and zipcode on one line.
#' @param delimiter The type of delimiter (comma, hyphen, dash) that the
#' address parts are split into.
#' @return The voter file with pre-processed format for each address
#' variable.
#'
#' @importFrom utils read.table
#' @export add_split_comma
add_split_comma <- function(voter_file,
                            address = "address",
                            delimiter = "comma") {
  # This function makes separate columns for each part of the address
  # and puts them into columns
  if (delimiter == "comma") {
    # Obtain the address column from the original dataset
    add_col <- voter_file[[address]]

    # Split the column by comma
    add_split <- strsplit(add_col, ",")

    # Get the length of the string to determine column names
    voter_file$num_col <- length(add_split)

    # For 4 columns, include address names and a secondary address
    if (voter_file$num_col == 4) {
      df <- strsplit(utils::read.table(add_col,
        sep = ",", colClasses = "character",
        col.names = c(
          "street_address", "street_address2",
          "city", "state", "zipcode"
        )
      ))
    }

    # For 3 columns, include street address, city, and zipcode
    if (voter_file$num_col == 3) {
      df <- strsplit(read.table(add_col,
        sep = ",", colClasses = "character",
        col.names = c(
          "street_address", "city",
          "state", "zipcode"
        )
      ))
    }
  }
}


#' Pre-processes voter file by checking zipcode, and any special characters
#' or typos within the address.
#'
#'
#' @param voter_file A voter file containing the address of the voter.
#' @param street_number The number attached to the street name. Ex. 1442
#' @param street_name The name of the place in which a voter lives.
#' Ex. Market Street
#' @param street_suffix A directional abbreviation such as NE for northeast or
#'  SW for southwest.
#'
#' @return The voter file with pre-processed format for each address variable.
#'
#' @export concat_streetname
concat_streetname <- function(voter_file,
                              street_number = "street_number",
                              street_name = "street_name",
                              street_suffix = "street_suffix") {

  # Concatenate columns for street address
  if (
    !is.null(voter_file[[street_number]]) & !is.null(voter_file[[street_name]])
  ) {
    voter_file$street_address <- paste(voter_file[[street_number]],
      voter_file[[street_name]],
      voter_file[[street_suffix]],
      sep = " "
    )
    # Remove NAs from street_address column
    voter_file$street_address <- gsub("NA", "", voter_file$street_address)
  }
  return(voter_file)
}


#' This function concatenate the final address
#'
#' @param voter_file A voter file containing the address of the voter.
#' @param street_address The street number and street name of the voters
#' address. Ex. 1442 Market Street
#' @param city The name of the city that the voter lives in.
#' @param state The state (based on the United States 50 states) that
#' the voter lives in.
#' @param zipcode The United States Postal Service (USPS) postal code.
#'
#' @return The voter file with pre-processed format for each address variable.
#'
#' @export concat_final_address
concat_final_address <- function(voter_file,
                                 street_address = "street_address",
                                 city = "city",
                                 state = "state",
                                 zipcode = "zipcode") {

  # Concatenate final address column
  voter_file$final_address <- paste(voter_file[[street_address]],
    voter_file[[city]],
    voter_file[[state]],
    voter_file[[zipcode]],
    sep = ","
  )

  # Remove extra spaces
  voter_file$final_address <- gsub(" ,", ",", voter_file$final_address)

  return(voter_file)
}


#' Pre-processes voter file by checking zipcode, and any special
#' characters or typos within the address.
#'
#' @param voter_file A voter file containing the address of the voter.
#' @param voter_id The unique identifier linked to the voter.
#' @param zipcode The United States Postal Service (USPS) postal code.
#'
#' @return The voter file with pre-processed format for each address
#' variable.
#'
#' @export zip_hyphen
zip_hyphen <- function(voter_file,
                       voter_id = "registration_number",
                       zipcode = "zipcode") {

  # Identify which zipcodes have more than 5 digits (i.e. 9 digits)
  zip9 <- length(which(nchar(voter_file[[zipcode]]) == 9))

  if (zip9 > 0) {

    # Extract the zipcodes with 9 characters, no hyphen
    zip9_ids <- which(nchar(voter_file[[zipcode]]) == 9)

    # Print message stating how many 9-digit zipcodes have been fixed
    # with a hyphen.
    paste(
      "There are",
      zip9_ids,
      "9-digit zipcodes in the dataset that have been formatted.",
      sep = " "
    )

    # Create a dataframe for 9-digit zipcodes that need a hyphen and
    # make zipcode a character variable
    zip9_df <- data.frame(cbind(
      voter_id = voter_file[[voter_id]][zip9_ids],
      zipcode = as.character(voter_file[[zipcode]][zip9_ids])
    ))

    # Add a hyphen to the zipcode using the format XXXXX-XXXX
    zip9_df$new_zipcode <- gsub(
      "(\\d{5})(\\d{4})$", "\\1-\\2", zip9_df$zipcode
    )

    # Merge the hyphenated zipcodes with the exitsing zipcodes in
    # the original dataframe.
    voter_file$zipcode[
      match(zip9_df$voter_id, voter_file[[voter_id]])
    ] <- zip9_df$new_zipcode
  }
  return(voter_file)
}
