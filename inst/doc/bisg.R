## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Load libraries
suppressPackageStartupMessages({
  library(eiCompare)
  library(stringr)
  library(sf)
  library(wru)
  library(tidyr)
  library(ggplot2)
  library(dplyr)
})

## -----------------------------------------------------------------------------
# Load Georgia census data
data(georgia_census)

## -----------------------------------------------------------------------------
# Shape file for Gwinnett and Fulton counties
data(gwin_fulton_shape)

## -----------------------------------------------------------------------------
# install.packages("tigris")
# library(tigris)
# gwin_fulton_shape <- blocks(state = "GA", county = c("Gwinnett", "Fulton"))

## -----------------------------------------------------------------------------
# Load geocoded voter registration file
data(ga_geo)

## -----------------------------------------------------------------------------
# Check the first six rows of the voter file
head(ga_geo, 6)

## -----------------------------------------------------------------------------
# Find out names of columns in voter file
names(ga_geo)

## -----------------------------------------------------------------------------
# Get the dimensions of the voter file
dim(ga_geo)

## -----------------------------------------------------------------------------
ga_geo <- ga_geo %>%
  tidyr::extract(geometry, c("lon", "lat"), "\\((.*), (.*)\\)", convert = TRUE)

## -----------------------------------------------------------------------------
# Remove duplicate voter IDs (the unique identifier for each voter)
voter_file_dedupe <- dedupe_voter_file(voter_file = ga_geo, voter_id = "registration_number")

## -----------------------------------------------------------------------------
# Convert the voter_shaped_merged file into a data frame for performing BISG.
voter_file_complete <- as.data.frame(voter_file_dedupe)
class(voter_file_complete)

## -----------------------------------------------------------------------------
georgia_census$GA$year <- 2010

# Perform BISG
bisg_df <- eiCompare::wru_predict_race_wrapper(
  voter_file = voter_file_complete,
  census_data = georgia_census,
  voter_id = "registration_number",
  surname = "last_name",
  state = "GA",
  county = "COUNTYFP10",
  tract = "TRACTCE10",
  block = "BLOCKCE10",
  census_geo = "block",
  use_surname = TRUE,
  surname_only = FALSE,
  surname_year = 2010,
  use_age = FALSE,
  use_sex = FALSE,
  return_surname_flag = TRUE,
  return_geocode_flag = TRUE,
  verbose = TRUE
)

## -----------------------------------------------------------------------------
# Check BISG dataframe
head(bisg_df)

## -----------------------------------------------------------------------------
summary(bisg_df)

## -----------------------------------------------------------------------------
# Obtain aggregate values for the BISG results by county
bisg_agg <- precinct_agg_combine(
  voter_file = bisg_df,
  group_col = "COUNTYFP10",
  race_cols = c("pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth"),
  true_race_col = "race",
  include_total = FALSE
)

# Table with BISG race predictions by county
head(bisg_agg)

## -----------------------------------------------------------------------------
bisg_bar <- bisg_agg %>%
  tidyr::gather("Type", "Value", -COUNTYFP10) %>%
  ggplot(aes(COUNTYFP10, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "BISG Predictions for Fulton and Gwinnett Counties", y = "Proportion", x = "Counties") +
  theme_bw()

bisg_bar + scale_color_discrete(name = "Race/Ethnicity Proportions")

## -----------------------------------------------------------------------------
bisg_dfsub <- bisg_df %>%
  dplyr::select(BLOCKCE10, pred.whi, pred.bla, pred.his, pred.asi, pred.oth)

bisg_dfsub

## -----------------------------------------------------------------------------
# Join bisg and shape file
bisg_sf <- dplyr::left_join(gwin_fulton_shape, bisg_dfsub, by = "BLOCKCE10")

## ---- results=FALSE-----------------------------------------------------------
# Plot choropleth map of race/ethnicity predictions for Fulton and Gwinnett counties
plot(bisg_sf["pred.bla"], main = "Proportion of Black Voters identified by BISG")

## ---- results=FALSE-----------------------------------------------------------
plot(bisg_sf["pred.whi"], main = "Proportion of White Voters identified by BISG")

## ---- results=FALSE-----------------------------------------------------------
plot(bisg_sf["pred.his"], main = "Proportion of Hispanic Voters identified by BISG")

## ---- results=FALSE-----------------------------------------------------------
plot(bisg_sf["pred.asi"], main = "Proportion of Asian Voters identified by BISG")

## ---- results=FALSE-----------------------------------------------------------
plot(bisg_sf["pred.oth"], main = "Proportion of 'Other' Voters identified by BISG")

