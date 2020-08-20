---
title: "Bayesian Improved Surname Geocoding (BISG)"
author: "null"
date: "8/5/2020"
output:
  pdf_document: default
  html_document: default
---
This vignette demonstrates how to perform Bayesian Improved Surname Geocoding when the race/ethncity of individuals are unknown within a dataset.

## *What is Bayesian Improved Surname Geocoding?*

Bayesian Improved Surname Geocoding (BISG) is a method that applies the Bayes Rule/Theorem to predict the race/ethnicity of an individual using the individual's surname and geocoded location [Elliott et. al 2008, Elliot et al. 2009, Imai and Khanna 2016]. 

Specifically, BISG first calculates the prior probability of *i* individual being of a ceratin *r* racial group given their *s* surname, or p(r~i~|s~i~). The prior probability created from the surname is updated using Bayes Theorem using the probability of the *i* individual *r* race given their *g* geographic location. The following equation describes how BISG calculates race/ethnicity of individuals when race/ethncicty is unknown using Bayes Theorem:

      ![BISG Equation for Predicting Race/Ethnicity.](/github/eiCompare/vignettes/bisg_equation.png)

In R, the package that performs BISG is called, WRU: Who Are You `wru` [cite WRU package]. This vignette will walk you through how to prepare your geocoded voter file for performing BISG by stepping you throught the processing of cleaning your voter file, prepping voter data for running the BISG, and finally, performing BISG to obtain racial/ethnic probailities of individuals in a voter file.

## *Performing BISG on your data*
The first step in performing BISG is to geocode your voter file addresses. For information on geocoding, visit the Geocoding Vignette. 

In this tutorial, we will be using the East Ramapo, New York voter file called, **voter_file_geocoded**, that consists of indivduals registering to vote during the year of 2016. 

Let's begin by loading your geocoded voter data into R/RStudio.

### Step 1: Load R libraries/packages, voter file, and census data

Load the R packages needed to perform BISG. If you have not already downloaded the following packages, please install these packages.
```{r}
# Load libraries/packages

suppressMessages(c(
  library(devtools),
  install.packages("tidyverse"),
  library(tidyverse),
  install.packages("stringr"),
  library(stringr),
  install.packages("sf"),
  library(sf),
  install.packages("eiCompare"),
  library(eiCompare),
  library(wru),
  library(readr)
))
```

Load in East Ramapo, NY School District voter registation data.
```{r}
# Load geocoded voter registration file
voter_file_geocoded <- read_csv("~/shared/east_ramapo/data/ram17_clean.csv")
```

Obtain the first six rows of the voter file to check that the file has downloaded properly.
```{r}
# Check the first six rows of the voter file
head(voter_file_geocoded, 6)
```

View the column names of the voter file. Some of these columns will be used along the journey to performing BISG.
```{r}
# Find out names of columns in voter file
names(voter_file_geocoded)
```

Check the dimensions (the number of rows and columns) of the voter file.
```{r}
# Get the dimensions of the voter file
dim(voter_file_geocoded)
```
There are 14310 voters (or observations) and 14 columns in the voter file.

Make sure to load your census data that details certain geographies (i.e. counties, cities, tracts, blocks, etc.)
```{r}
# Load New York census data
load("~/shared/east_ramapo/data/ny_census2.RData")
census_data <- ny_3
```

Next, load the state shape file using the sf::st_read() function.
```{r}
# Load New York block shape file
ny_shape <- st_read("~/shared/east_ramapo/data/tl_2014_36_tabblock10.shp")
```


### Step 2: De-duplicate the voter file.

The next step involves removing duplicate voter IDs from the voter file, using the `dedupe_voter_file` function. Check the column name for the unique identifier assigned to each voter the voter file. For the East Ramapo voter file we are using, voter_id is the column named `id`.

```{r}
# Remove duplicate voter IDs (the unique identifier for each voter)
voter_file_geocoded <- dedupe_voter_file(voter_file = voter_file_geocoded, voter_id = "voter_id")

# Check new dimensions of voter file after removing duplicate voter IDs
dim(voter_file_geocoded)
```
There are no duplicate voter IDs in the dataset for the East Ramapo school district data set.

### Step 3: Merge voter file and shape files. This function may take a minute to complete.
```{r}
# Load shape file
voter_shape_merged <- merge_voter_file_to_shape(
  voter_file = voter_file_geocoded,
  shape_file = ny_shape,
  crs = "+proj=longlat +ellps=GRS80",
  coords = c("lon", "lat"),
  voter_id = "voter_id"
)

# Check first 6 rows of merged voter file.
head(voter_shape_merged, 6)
```
```{r}
# Obtain dimensions of merged file.
dim(voter_shape_merged)
```

```{r}
# Get column names of merged file.
names(voter_shape_merged)
```


### Step 4: Extracting necessary columns from the voter file to perform BISG
The voter file contains lots of information about the voter. However, all information is not needed and some columns like voter ID and state.

```{r}
voter_file_to_bisg <- tidy_voter_file_wru(
  voter_file = voter_shape_merged,
  voter_id = "voter_id",
  surname = "last_name",
  state = "STATEFP10",
  county = "COUNTYFP10",
  tract = "TRACTCE10",
  block = "BLOCKCE10"
)

# Check the first 6 rows of the voter_file_to_bisg
head(voter_file_to_bisg, 6)
```
```{r}
dim(voter_file_to_bisg)

# Get column names for the voter file that is now prepped to perform BISG.
names(voter_file_to_bisg)
```

### Step 5: Perform BISG and obtain the predicted race/ethnicity of each voter.
```{r}
# Convert the voter_shaped_merged file into a data frame for performing BISG.
voter_file_complete <- as.data.frame(voter_shape_merged)
class(voter_file_complete)
```

```{r}
# Perform BISG
bisg_file <- eiCompare::wru_predict_race_wrapper(
  voter_file = voter_file_complete,
  census_data = census_data,
  voter_id = "voter_id",
  surname = "last_name",
  state = "NY",
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
summary(bisg_file$bisg)
```
The BISG voter data.frame is now split into two tables `bisg_file$voter_file` and `bisg$bisg`. The racial proportions for every indidivual given their last name and location is generated and can now be used to perform ecological inference using eiCompare.


## Summarizing and Plotting BISG output

### Scatterplot



### Voter Turnout



### Choropleth Map
