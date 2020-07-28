#' Checks the voter file for missing geocoding (i.e. latitude and longitude) and
#' uses Opencage API to find the missing latitudes and longitudes.
#'
#' This function requires that the user inputs a voter file and has membership to
#' Opencage with an Opencage key.
#'
#' @param voter_file The voter file with that contains the each voter's
#' address and geocoded latitude and longitude information.
#' @param opencage_key The Opencage access key received once the user has
#' obtained a membership to the Opencage tool at www.opencagedata.com


opencage_id_miss_geo <- function(voter_file, opencage_key) {

  # Subset a dataframe for voters with no longitude or latitude.
  miss_latlon <- which(is.na(voter_file$lat) & is.na(voter_file$lon))
  miss_latlon_df <- voter_file[miss_latlon, ]

  # Check how many voters are missing a longitude and latitude.
  num_miss <- length(miss_latlon)
  total_voters <- nrow(voter_file) - num_miss

  # Get the percentage geocoded for the voter file.
  total_geocoded <- round((total_voters - num_miss) / total_voters * 100, 2)
  message(paste(total_geocoded, "% of your voter file is now geocoded.", sep = " "))

  if (num_miss > 0) {
    message(paste("There are", num_miss, "voters missing latitude and longitude values in 
                  your voter geocoded voter file.\nWe will use Opencage to attempt to find them now.", sep = " "))

    # Let's take out the 47 people that missing geocodes and put in a dataframe
    miss_latlon_df$state <- str_sub(miss_latlon_df$full_addy, -2, -1)

    # Make final address for Opencage to identify

    miss_latlon_df$streetnum_name <- paste(miss_latlon_df$RESIDENCE_HOUSE_NUMBER,
      miss_latlon_df$RESIDENCE_STREET_NAME,
      sep = " "
    )

    miss_latlon_df$final_address <- paste(miss_latlon_df$streetnum_name,
      miss_latlon_df$RESIDENCE_CITY,
      miss_latlon_df$state,
      miss_latlon_df$RESIDENCE_ZIPCODE,
      sep = ", "
    )


    # Run these addresses through Opencage to identify latitude and longitude
    # Use opencage to find lat and lon for voters without geocodes and put back into ram dataframe.
    suppressWarnings(
      for (m in 1:nrow(miss_latlon_df)) {
        opencage_latlon <- opencage_forward(placename = miss_latlon_df$final_address[m], country = "US", key = opencage_key)
        miss_latlon_df$lon[m] <- opencage_latlon$results$geometry.lng
        miss_latlon_df$lat[m] <- opencage_latlon$results$geometry.lat
      }
    )
    # Join the new_lon_lat_df dataframe and the missing_geo_city_zip dataframe.
    voter_file$lon[match(miss_latlon_df$REGISTRATION_NUMBER, voter_file$REGISTRATION_NUMBER)] <- miss_latlon_df$lon
    voter_file$lat[match(miss_latlon_df$REGISTRATION_NUMBER, voter_file$REGISTRATION_NUMBER)] <- miss_latlon_df$lat

    # Check for missing geocodes again
    miss_latlon2 <- which(is.na(voter_file$lat) & is.na(voter_file$lon == 0))
    num_miss2 <- length(miss_latlon2)
    total_geocoded2 <- round((total_voters - num_miss2) / total_voters * 100, 2)

    message(paste(total_geocoded2, "% of your voter file is now geocoded using the Opencage API.", sep = " "))
    new_voter_file <- voter_file

    return(new_voter_file)
  } else {
    message(paste("There are no missing geocodes (i.e. latitudes and longitudes) in the voter file.", sep = " "))
  }
}

#' @return The voter file with new geocoded information using Opencage.
