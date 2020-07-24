#' Bayesian Improved Surname Geocoding Data Preparation and Race Prediction
#'
#' Preps data for BISG estimation via WRU package
#'
#'
#' @param df data.frame() object, containing voter file information with FIPS
#' code information extracted using unit_comb_extract() function, which brings
#' in relevant columns
#' @param voterid String of voterid column name. Default = NULL. Probably want
#' this.
#' @param precinct String of precinct column name. Default = NULL. Probably
#' want this.
#' @param surname_char String of surname_char column name. This column is
#' passed to wru package.
#' @param state String of two digit state abbreviation, e.g., "NY", or "CA".
#' @param census.geo String of geographic level used for BISG. Default is
#' Census "block". An optional character vector specifying what level of
#' geography to use to merge in U.S. Census 2010 geographic data. Currently
#' "county", "tract", "block", and "place" are supported. Note: sufficient
#' information must be in user-defined voter.file object. If census.geo =
#' "county", then voter.file must have column named county. If census.geo =
#' "tract", then voter.file must have columns named county and tract. And if
#' census.geo = "block", then voter.file must have columns named county, tract,
#' and block. If census.geo = "place", then voter.file must have column named
#' place. Specifying census.geo will call census_helper function to merge
#' Census geographic data at specified level of geography.
#' @param census.key A character object specifying user's Census API key.
#' Required if census.geo is specified, because a valid Census API key is
#' required to download Census geographic data.
#' @param census.data A list indexed by two-letter state abbreviations, which
#' contains pre-saved Census geographic data. Can be generated using
#' get_census_data function from wru package.
#' @param census.surname A TRUE/FALSE object. If TRUE, function will call
#' merge_surnames to merge in Pr(Race | Surname) from U.S. Census Surname List
#' (2000 or 2010) and Spanish Surname List. If FALSE, voter.file object must
#' contain additional fields specifying Pr(Race | Surname), named as follows:
#' p_whi for Whites, p_bla for Blacks, p_his for Hispanics/Latinos, p_asi for
#' Asians, and/or p_oth for Other. Default is TRUE.
#' @param surname.only A TRUE/FALSE object. If TRUE, race predictions will only
#' use surname data and calculate Pr(Race | Surnname). Default is FALSE.
#' @param surname.year A number to specify the year of the census surname
#' statistics. These surname statistics is stored in the data, and will be
#' automatically loaded. The default value is 2010, which means the surname
#' statistics from the 2010 census will be used. Currently, the other available
#' choice is 2000.
#' @param age An optional TRUE/FALSE object specifying whether to condition
#' race predictions on age (in addition to surname and geolocation). Default is
#' FALSE. Must be same as age in census.data object. May only be set to TRUE if
#' census.geo option is specified. If TRUE, voter.file should include a
#' numerical variable age.
#' @param sex optional TRUE/FALSE object specifying whether to condition race
#' predictions on sex (in addition to surname and geolocation). Default is
#' FALSE. Must be same as sex in census.data object. May only be set to TRUE if
#' census.geo option is specified. If TRUE, voter.file should include a
#' numerical variable sex, where sex is coded as 0 for males and 1 for females.
#' @param party An optional character object specifying party registration
#' field in voter.file, e.g., party = "PartyReg". If specified, race/ethnicity
#' predictions will be conditioned on individual's party registration (in
#' addition to geolocation). Whatever the name of the party registration field
#' in voter.file, it should be coded as 1 for Democrat, 2 for Republican, and 0
#' for Other.
#' @param retry The number of retries at the census website if network
#' interruption occurs. Default = 0.
#' @return List object of of two data.frames. List item 1 is initial voter file
#' data.frame. List object 2 (named bisg) includes race predictions.
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references wru R package. Kabir Khanna, Kosuke Imai, Hubert Jin.  Imai and
#' Khanna (2015) "Improving Ecological Inference by Predicting Individual
#' Ethnicity from Voter Registration Records" <DOI:10.1093/pan/mpw001>
#' @import R.utils
#' @examples
#'
#'
#' # EXAMPLE: NOT RUN #
#' # NOTE: You need to have a census key to run this #
#'
#' data(ny_voter)
#' head(ny_voter)
#'
#' # Load ny_census object #
#' # load ( system.file("extdata/ny_census.RData",package="eiCompare") )
#'
#' # Now load your Census key #
#' # [add in here where you would do that]
#' # key_census = ""
#'
#' ####################################################
#' # Prep Data & Perform Bayesian Surname Geolocation #
#' ####################################################
#' # Extract second list object (called bisg, note $bisg)
#' # bisg <- bisg_prep_race_predict(df=ny_voter,
#' #                     voterid = "Voter.ID",
#' #                     precinct = "SD..Poll",
#' #                     surname_char = "Last.Name",
#' #                     state = "NY",
#' #                     census.key = key_census,
#' #                     census.data = ny_census)$bisg
#' @export bisg_prep_race_predict
bisg_prep_race_predict <- function(df, voterid = NULL, precinct = NULL, surname_char, state,
                                   census.geo = "block", census.key, census.data,
                                   census.surname = TRUE, surname.only = FALSE,
                                   surname.year = 2010, age = FALSE, sex = FALSE,
                                   party, retry = 0) {
  if (!is.null(voterid) | !is.null(precinct)) {
    voter.file <- data.frame(
      voterid = df[, voterid],
      precinct = df[, precinct],
      surname = tolower(df[, surname_char]),
      state = "NY",
      county = as.character(df$county),
      tract = as.character(df$tract),
      block = as.character(df$block),
      stringsAsFactors = F
    )
    print(R.utils::str(voter.file))
  } else {
    # Create Data.frame Object to send to predict_race() wru package function #
    voter.file <- data.frame(
      voterid = 1:nrow(df),
      surname = tolower(df[, surname_char]),
      state = "NY",
      county = as.character(df$county),
      tract = as.character(df$tract),
      block = as.character(df$block),
      stringsAsFactors = F
    )
    print(R.utils::str(voter.file))
  }
  # Estimate Voter Race #
  # Suppress the warning that a few people are not race predicted
  bisg <- suppressWarnings(wru::predict_race(voter.file,
    census.geo = census.geo,
    census.key = census.key,
    census.data = census.data,
    census.surname = census.surname, surname.only = surname.only,
    surname.year = surname.year, age = age, sex = sex, party,
    retry = retry
  ))

  # Print out Number of NA names not matching Census Lists #
  no_name <- as.vector(table(is.na(bisg$pred.whi))["TRUE"])
  print(paste("Number of Names not matching Census List: ", no_name, sep = ""))

  ## Convert NA probabilities to 0 ##
  bisg$pred.whi[is.na(bisg$pred.whi)] <- 0 # White
  bisg$pred.bla[is.na(bisg$pred.bla)] <- 0 # Black
  bisg$pred.his[is.na(bisg$pred.his)] <- 0 # Hispanic
  bisg$pred.asi[is.na(bisg$pred.asi)] <- 0 # Asian
  bisg$pred.oth[is.na(bisg$pred.oth)] <- 0 # Race: Other

  return(list(voter.file = voter.file, bisg = bisg))
}
