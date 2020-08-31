#' Geocoding voter file addresses with coordinates (latitude and longitude)
#' and/or census geographies.
#'
#'
#' @param voter_file A data frame contain the voter addresses, separated into
#' columns for street, city, state, and zipcode
#' @param geocoder The options for selecting geocoders are "censusxy" and
#'  "opencage".
#' @param parallel TRUE or FALSE. The option to run parallel processing on the
#'  data. Running parallel processing requires the user to have at least 4 CPU
#'  cores. Use detectCores() to determine the number of CPUs on your device.
#' @param voter_id the unique identifier
#' @param street the street number, street name, and/or street suffix.
#' Ex. 555 Main Street SW
#' @param city the location/town
#' @param state the abbreviated state (U.S. state categories such as "GA")
#' @param zipcode the 5 or 9 digit number in the format XXXXX or XXXXX-XXXX.
#' @param country the abbreviated a nation or territory
#' @param census_return either "locations" or "geographies". "locations" returns
#'  the latitude and longitude coordinates. "geographies" returns the latitude,
#'  longitude, and FIPS codes for county, state, tract, and block.
#' @param census_benchmark a dataset of the snapshot of the US Census data. Data
#'  is collected two times a year. Public_AR_Current is the time period when we
#'  created the snapshot of the data (usually done twice yearly). For example,
#'  Public_AR_Current is the most recent snapshot of our dataset.
#' @param census_vintage a dataset that details the survey or census that the
#' census_benchmark uses.
#' @param census_output "single" or "full". "single" indicates that only
#'  latitude and longitude are returned. "full" indicates that latitude,
#'  longitude, and FIPS codes are returned.
#' @param census_class "sf" indicates returning a shape file in the R class, sf.
#' Other file types like "json" and "csv" can also be used.
#' @param opencage_key the Opencage Geocoder API key needed to run the Opencage
#' Geocoder. The use of the key is
#' limited to the level of membership on Opencage. Only 2500 rquests per day
#' for free membership.
#'
#' @return The geocoded voter file with either added simple
#' (latitude and longitude coordinates) or other geographies.
#'
#' @export run_geocoder
#'
#' @import foreach
#' @import parallel
#' @import doParallel
#' @importFrom censusxy cxy_geocode
#'
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @author Juandalyn Burke <jcburke@@uw.edu>


run_geocoder <- function(voter_file,
                         geocoder = "census",
                         parallel = FALSE,
                         voter_id = "voter_id",
                         street = "street",
                         city = "city",
                         state = "state",
                         zipcode = "residence_zipcode",
                         country = NULL,
                         census_return = NULL,
                         census_benchmark = "Public_AR_Current",
                         census_vintage = 4,
                         census_output = "single",
                         census_class = "sf",
                         opencage_key = NULL) {

  # Determine number of observations in the voter_file.
  num_obs <- nrow(voter_file)

  # Runs ONLY if Census Geocoder API is selected and there are less than 10,000
  # or less observations in the voter file.
  if (geocoder == "census" & parallel == FALSE & num_obs <= 10000) {

    # Start-up message
    message(paste0(
      "Running US Census Geocoding API. This may take several",
      " minutes (i.e. up to 20 minutes) to complete."
    ))

    packageStartupMessage("Initializing...", appendLF = FALSE)

    census_voter_file <- censusxy::cxy_geocode(
      .data = voter_file,
      id = voter_id,
      street = street,
      city = city,
      state = state,
      zip = zipcode,
      return = census_return,
      benchmark = census_benchmark,
      output = census_output,
      class = census_class
    )

    voter_file <- census_voter_file
    packageStartupMessage(" Geocoding complete.")
    return(voter_file)
  }

  #############################################################################
  # Runs ONLY if Census Geocoder API is selected and there are less than 10,000
  # in the voter file.
  #############################################################################
  if (geocoder == "census" & parallel == TRUE & num_obs <= 10000) {
    # Detect the number of cores on user's machine
    n_cores <- detectCores()

    # Send warning message if number of cores is less than 4 cores
    if (n_cores < 4) {
      message(paste0(
        "Your machine is has less than 4 cores and will not be",
        "able to perform parallel processing."
      ))
      packageStartupMessage(
        "Aborting parallel processing option...",
        appendLF = FALSE
      )
      Sys.sleep(1)
      packageStartupMessage(" done")
    }

    if (n_cores >= 4) {

      # Start-up messages
      message(paste0(
        "Running US Census Geocoding API. This may take several",
        " minutes (i.e. up to 20 minutes) to complete."
      ))

      packageStartupMessage("Initializing...", appendLF = FALSE)

      # Run Census Geocoder API
      clust <- makeCluster(detectCores() - 2)
      registerDoParallel(clust)

      start_time_2 <- Sys.time()

      census_voter_file <- foreach(
        i = 1,
        .combine = rbind,
        .packages = c("censusxy", "sf")
      ) %dopar% {
        censusxy::cxy_geocode(
          .data = voter_file,
          id = voter_id,
          street = street,
          city = city,
          state = state,
          zip = zipcode,
          return = census_return,
          benchmark = census_benchmark,
          vintage = census_vintage,
          output = census_output,
          class = census_class
        )
      }

      (end_time_2 <- Sys.time() - start_time_2)

      stopCluster(clust)
      gc()

      voter_file <- census_voter_file

      Sys.sleep(1)
      packageStartupMessage(" Geocoding complete.")

      return(voter_file)
    }
  }


  #############################################################################
  # Runs ONLY if Census Geocoder API is selected, the user is conducting
  # parallel processing and there are more than 10,000 observations/voters.
  #############################################################################

  if (geocoder == "census" & parallel == TRUE & num_obs > 10000) {
    # Detect the number of cores on user's machine
    n_cores <- detectCores()

    # Send warning message if number of cores is less than 4 cores
    if (n_cores < 4) {
      message(paste0(
        "Your machine is has less than 4 cores and will not be",
        "able to perform parallel processing."
      ))
      packageStartupMessage(
        "Aborting parallel processing option...",
        appendLF = FALSE
      )
      Sys.sleep(1)
      packageStartupMessage(" done")
    }

    if (n_cores >= 4) {

      # Start-up messages
      message(paste0(
        "Running US Census Geocoding API. This may take several",
        " minutes (i.e. up to 20 minutes) to complete."
      ))

      packageStartupMessage("Initializing...", appendLF = FALSE)

      # Set up iterations for parallel processing
      # Set number of loops needed to batch every 10000 rows of the voter file
      remain <- num_obs %% 10000
      n_loops <- ifelse(remain == 0, round(num_obs / 10000, digits = 0),
        (round(num_obs / 10000, digits = 0)) + 1
      )
      start_row <- 1
      stop_row <- start_row + 9999
      last_row_stop <- num_obs
      last_row_start <- last_row_stop - (num_obs %% 10000) + 1

      # Create empty list that will hold batches of data by 10K observations.
      dfList <- list()

      # Creates lists of dataframes that holds voter data in batches of 10000
      for (i in 1:seq_len(n_loops)) {
        df <- voter_file[start_row:stop_row, ]
        if (start_row == last_row_start) {
          df <- voter_file[last_row_start:last_row_stop, ]
        }
        dfList <- append(dfList, list(df))
        start_row <- stop_row + 1
        stop_row <- start_row + 9999
      }

      # Run Census Geocoder API
      clust <- makeCluster(detectCores() - 2)
      registerDoParallel(clust)

      start_time_2 <- Sys.time()

      census_voter_file <- foreach(
        i = 1:n_loops,
        .combine = rbind,
        .packages = c("censusxy", "sf")
      ) %dopar% {
        censusxy::cxy_geocode(
          .data = dfList[[i]],
          id = voter_id,
          street = street,
          city = city,
          state = state,
          zip = zipcode,
          return = census_return,
          benchmark = census_benchmark,
          vintage = census_vintage,
          output = census_output,
          class = census_class
        )
      }

      (end_time_2 <- Sys.time() - start_time_2)

      stopCluster(clust)
      gc()

      voter_file <- census_voter_file

      Sys.sleep(1)
      packageStartupMessage("Geocoding Complete")

      return(voter_file)
    }

    ############################################################################
    # Runs ONLY if Opencage geocoder is selected and there is an opencage_key. #
    ############################################################################

    if (geocoder == "opencage" & !is.null(opencage_key)) {

      # format address to opencage best pratice for addresses using commas
      voter_file$opencage_address <- paste(voter_file[[street]],
        voter_file[[city]],
        voter_file[[state]],
        voter_file[[zipcode]],
        sep = ","
      )
      # Geocode using new address format for opencage and create columns for
      # geographies
      for (m in 1:seq_len(num_obs)) {
        tryCatch(
          {
            if (requireNamespace("opencage", quietly = TRUE)) {
              opencage_latlon <- opencage::opencage_forward(
                placename = voter_file$opencage_address[m],
                country = NULL,
                key = opencage_key
              )
            } else {
              message(
                "This function requires opencage. Please install and re-run."
              )
            }
            # longtiude
            voter_file$lon[m] <- opencage_latlon$results$geometry.lng[1]
            # latitude
            voter_file$lat[m] <- opencage_latlon$results$geometry.lat[1]
            # confidence score
            voter_file$confidence[m] <- opencage_latlon$results$confidence[1]
            # opencage address
            voter_file$address[m] <- opencage_latlon$results$formatted[1]
            # county name
            voter_file$county[m] <- opencage_latlon$results$components.county[1]
            # FIPS county code
            voter_file$county_fips[m] <- opencage_latlon$results$annotations.FIPS.county[1]
          },
          error = function(e) {
            print(e)
          }
        )
      }
    }
    return(voter_file)
  }
}
