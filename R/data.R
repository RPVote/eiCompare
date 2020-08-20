#' Corona 2006 Election Results
#'
#' This dataset contains precinct vote data from a 2006 election in Corona, CA.
#'
#' @name cor_06
#' @format A data frame with 47 observations on the following 8 variables:
#' \describe{
#'  \item{precinct}{Precinct ID number.}
#'  \item{totvote}{The total vote, per precinct.}
#'  \item{pct_latino}{Percent of voters identifying as Latino.}
#'  \item{pct_other}{Percent of voters identifying as non-Latino.}
#'  \item{pct_breitenbucher}{Percent of vote for Breitenbucher.}
#'  \item{pct_montanez}{Percent of voters for Montanez.}
#'  \item{pct_spiegel}{Percent of voters for Spiegel.}
#'  \item{pct_skipworth}{Percent of voters for Skipworth.}
#' }
#' @usage data(cor_06)
#' @source Riverside County, CA Board of Elections.
"cor_06"


#' Corona 2014 Election Results
#'
#' This dataset contains precinct vote data and racial demographics from a 2014
#' election in Corona, CA.
#'
#' @name corona
#' @format A data frame with 46 observations on the following 12 variables:
#' \describe{
#'  \item{precinct}{Precinct ID number.}
#'  \item{totvote}{The total vote, per precinct.}
#'  \item{pct_husted}{Percent of vote for Husted.}
#'  \item{pct_spiegel}{Percent of vote for Spiegel.}
#'  \item{pct_ruth}{Percent of vote for Ruth.}
#'  \item{pct_button}{Percent of vote for Button.}
#'  \item{pct_montanez}{Percent of vote for Montanez.}
#'  \item{pct_fox}{Percent of vote for Fox.}
#'  \item{pct_hisp}{Percent of voters identifying as Hispanic.}
#'  \item{pct_asian}{Percent of voters identifying as Asian.}
#'  \item{pct_white}{Percent of voters identifying as white.}
#'  \item{pct_non_lat}{Percent of voters identifying as non-Latino.}
#' }
#' @usage data(corona)
#' @source Riverside County, CA Board of Elections
"corona"

#' East Ramapo School District Proposed Maps
#'
#' This dataset contains proposed maps and CVAP totals for East Ramapo School
#' District.
#'
#' @name ersd_maps
#' @format A data frame with 46 observations on the following 12 variables:
#' \describe{
#'  \item{WARD}{Precinct ID number.}
#'  \item{TOT_CVAP}{The total vote, per precinct.}
#'  \item{WHI_CVAP}{Percent of vote for Husted.}
#'  \item{BLA_CVAP}{Percent of vote for Spiegel.}
#'  \item{HIS_CVAP}{Percent of vote for Ruth.}
#'  \item{ASI_CVAP}{Percent of vote for Button.}
#'  \item{MIN_AGG_FRAC}{Percent of vote for Montanez.}
#'  \item{geometry}{Percent of vote for Fox.}
#' }
#' @usage data(ersd_maps)
#' @source East Ramapo School District
"ersd_maps"


#' Los Angeles County 2010 Election Results
#'
#' This dataset contains precinct vote data and racial demographics from a 2010
#' election in Los Angeles County.
#'
#' @name lac_10
#' @format A data frame with 4980 observations on the following 10 variables:
#' \describe{
#'  \item{precinct}{Precinct ID number.}
#'  \item{tot_reg}{The total number of registered voters.}
#'  \item{i_jones}{Number of votes for Jones.}
#'  \item{i_delatore}{Number of votes for Delatorre.}
#'  \item{votescast}{The total number of votes cast.}
#'  \item{lat_voters}{Number of Latino voters.}
#'  \item{pct_latino}{Percent of voters identifying as Latino.}
#'  \item{pct_delatorre}{Percent of vote for Delatorre.}
#'  \item{pct_jones}{Percent of vote for Jones.}
#'  \item{pct_other}{Percent of vote for other candidates.}
#' }
#' @usage data(lac_10)
#' @source Los Angeles County
"lac_10"


#' New York State FIPS codes
#'
#' New York State FIPS codes for 500 voters.
#'
#' @name ny_fips
#' @format A data frame with 500 observations on the following 2 variables:
#' \describe{
#'  \item{row_id}{Unique identifier.}
#'  \item{FIP}{The 15-digit FIPS code.}
#' }
#' @usage data(ny_fips)
"ny_fips"


#' New York Voter File Sample
#'
#' This dataset contains a sample of 500 voters in East Ramapo School District,
#' New York.
#'
#' @name ny_voter
#' @format A data frame with 500 observations on the following 10 variables:
#' \describe{
#'  \item{Voter.ID}{Anonymized voter ID.}
#'  \item{SD..Poll}{Precinct ID.}
#'  \item{fips}{The 15-digit FIPS code}
#'  \item{st}{State FIPS code}
#'  \item{county}{County FIPS code}
#'  \item{tract}{Tract FIPS code}
#'  \item{block}{Block FIPS code}
#'  \item{st_cty}{State-county FIPS code}
#'  \item{st_cty_tract}{State-county-tract FIPS code}
#'  \item{Last.Name}{Voter surname.}
#' }
#' @usage data(ny_voter)
#' @source East Ramapo School District Board of Elections.
"ny_voter"


#' Rockland County, NY, Census demographic dataset.
#'
#' This dataset contains the demographic information for Rockland County in New
#' York, which is where East Ramapo School District is located.
#'
#' @format A nested list which can be sent to the `predict_race` function in
#'  WRU. Within "NY", the "block", "tract", and "county" keys contain the
#'  following columns.
#' \describe{
#'  \item{state}{State FIPS code}
#'  \item{county}{County FIPS code}
#'  \item{tract}{Tract FIPS code}
#'  \item{block}{Block FIPS code}
#'  \item{P005003}{White alone population}
#'  \item{P005004}{Black or African American alone population}
#'  \item{P005005}{American Indian and Alaska Native alone population}
#'  \item{P005006}{Asian alone population}
#'  \item{P005007}{Native Hawaiian and Other Pacific Islander alone population}
#'  \item{P005008}{Some other race alone population}
#'  \item{P005009}{Two or more races population}
#'  \item{P005010}{Hispanic or Latino population}
#'  \item{r_whi}{White voters; from Census Bureau.}
#'  \item{r_bla}{Black voters; from Census Bureau.}
#'  \item{r_his}{Hispanic voters; from Census Bureau.}
#'  \item{r_asi}{Asian voters; from Census Bureau.}
#'  \item{r_oth}{Other voters; from Census Bureau.}
#' }
#' @usage data(rockland_census)
#' @source Census Bureau via the WRU package.
"rockland_census"

#' Election results and racial turnout data for Gwinnett County, Georgia, US
#'
#' This dataset contains results of the 2018 Georgia gubernatorial election for
#' precincts in Gwinnett County. Data includes counts of votes cast for each
#' candidate and turnout by racial group.
#'
#' Data contain the following intentional errors mean for illustration in
#' vignettes: Rows 35 and 36 split up election results for the same precinct.
#' These should be collapsed.
#'
#' @format A data frame with 157 rows and 9 columns
#' \describe{
#' \item{precinct}{Unique precinct identifier}
#' \item{turnout}{Count of voter turnout}
#' \item{kemp}{Count of votes cast for Republican candidate Brian Kemp}
#' \item{abrams}{Count of votes cast for Democratic candidate Stacey Abrams}
#' \item{metz}{Count of votes cast for Libertarian candidate Ted Metz}
#' \item{white}{Count of voters self-reporting as white}
#' \item{black}{Count of voters self-reporting as black}
#' \item{hispanic}{Count of voters self-reporting as hispanic}
#' \item{other}{Count of voters self-reporting any other racial/ethnic group}
#' }
#' @usage data(gwinnett)
"gwinnett"

#' Fulton County and Gwinnett County, GA, Census demographic dataset.
#'
#' This dataset contains the demographic information for Fulton and Gwinnett counties
#' in Georgia.
#'
#' @name georgia_census
#' @format A nested list which can be sent to the `wru_predict_race_wrapper` function.
#' Within "GA", the "block", "tract", and "county" keys contain the following columns.
#' \describe{
#'  \item{state}{State FIPS code}
#'  \item{county}{County FIPS code}
#'  \item{tract}{Tract FIPS code}
#'  \item{block}{Block FIPS code}
#'  \item{P005003}{White alone population}
#'  \item{P005004}{Black or African American alone population}
#'  \item{P005005}{American Indian and Alaska Native alone population}
#'  \item{P005006}{Asian alone population}
#'  \item{P005007}{Native Hawaiian and Other Pacific Islander alone population}
#'  \item{P005008}{Some other race alone population}
#'  \item{P005009}{Two or more races population}
#'  \item{P005010}{Hispanic or Latino population}
#'  \item{r_whi}{White voters; from Census Bureau.}
#'  \item{r_bla}{Black voters; from Census Bureau.}
#'  \item{r_his}{Hispanic voters; from Census Bureau.}
#'  \item{r_asi}{Asian voters; from Census Bureau.}
#'  \item{r_oth}{Other voters; from Census Bureau.}
#' }
#' @usage data(georgia_census)
#' @source Census Bureau via the WRU package.
"georgia_census"

#' Voter file information that has been geocoded
#'
#' This dataset contains results from geocoding voter addresses using
#' the U.S. Census Bureau. The geocoded voter file has 12 observations
#' and 25 variables that include a geometry of latitude and longitude
#' points and fips code values for state, county, tract, and block
#' geographies.
#'
#' @name ga_geo
#' @format A data frame with 12 rows and 25 columns
#' \describe{
#' \item{county_code}{Unique identifier for counties in the state of Georgia}
#' \item{county_name}{A list of the county name matching the county_code}
#' \item{registration_number}{Unique identifier for registered voter identification}
#' \item{voter_status}{The registration status of the voter}
#' \item{last_name}{The last name of the voter}
#' \item{first_name}{The first name of the voter}
#' \item{str_num}{The street number of the voter address}
#' \item{str_name}{The name of the street of the voter address}
#' \item{str_suffix}{The suufix of the street that is commonly directional}
#' \item{city}{The city of the voter address}
#' \item{state}{The state of the voter address}
#' \item{zipcode}{The 5 or 9 digit zipcode of the voter address}
#' \item{street_address}{The street number and street name, concatenated}
#' \item{final_address}{The street_address, city, state, and zipcode
#' concatenated}
#' \item{cxy_address}{The address generated and predicted by the
#' US Census Geocoder}
#' \item{cxy_status}{The US Census Geocoder flag for whether an addresses
#' was matched in the US Census Geocoder}
#' \item{cxy_quality}{The determinant of whether the addresses matched exctly}
#' \item{cxy_matched_address}{The address used to compare with the voter
#' address inputted
#' into the Geocoder API to determine whether a match has occurred}
#' \item{cxy_tiger_line_id}{unique identifier from the Tiger line database
#' that captures
#' geographic aras of interests like roads, railroads, rivers, etc.}
#' \item{cxy_tiger_side}{a directional identifier in the Tiger Line database}
#' \item{STATEFP10}{the FIPS code for the state geograhic level}
#' \item{COUNTYFP10}{the FIPS code for the county geographic level}
#' \item{TRACTCE10}{the FIPS code for the tract geographic level}
#' \item{BLOCKCE10}{the FIPS code for the block geographic level}
#' \item{geometry}{latitude and longitude coordinates}
#' }
#' @usage data(ga_geo)
"ga_geo"
