## ----imports, echo=FALSE, message=FALSE, warning=FALSE------------------------
suppressMessages({
  library(dplyr)
  library(eiCompare)
  library(ggplot2)
  library(sf)
  library(wru)
})

## ----load_data----------------------------------------------------------------
# Load the map
data("ersd_maps")
sf::st_crs(ersd_maps) <- 4326

## ----cvap_map, fig.align = "center", fig.height=6, fig.width=7.2, message=FALSE, warning=FALSE----
# Plot the map, using a fill that depends on Citizen Voting Age Population (CVAP)
options(repr.plot.width = 7.2, repr.plot.height = 6)
cvap_map <- ggplot() +
  geom_sf(data = ersd_maps, aes(fill = MIN_AGG_FRAC)) +
  geom_sf_label(data = ersd_maps, aes(label = WARD), size = 5) +
  scale_fill_continuous(limits = c(0, 1)) +
  xlab("Latitude") +
  ylab("Longitude") +
  theme_bw(base_size = 10) +
  theme(
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 5)),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 5)),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(1, "cm")
  ) +
  guides(fill = guide_legend(
    title = "Fraction\nMinority",
    title.position = "top",
    title.size = 10
  ))
show(cvap_map)

## ----toy_voter_file-----------------------------------------------------------
voter_file <- data.frame(
  voter_id = c(1, 2, 3, 4, 5, 5),
  surname = c(
    "ROSENBERG",
    "JACKSON",
    "HERNANDEZ",
    "LEE",
    "SMITH",
    "SMITH"
  ),
  lat = c(41.168, 41.1243, 41.089, 41.14, 41.12, 41.123),
  lon = c(-74.02, -74.039, -74.08, -74.05, -74.045, -74.046)
)

## ----dedupe_voter_file--------------------------------------------------------
voter_file <- eiCompare::dedupe_voter_file(
  voter_file = voter_file,
  voter_id = "voter_id"
)
print(voter_file)

## ----merge_to_ward------------------------------------------------------------
voter_file_w_ward <- eiCompare::merge_voter_file_to_shape(
  voter_file = voter_file,
  shape_file = ersd_maps,
  coords = c("lon", "lat"),
  voter_id = "voter_id"
)
print(as.data.frame(voter_file_w_ward)[, c("surname", "WARD")])

## ----plot_voter_w_ward, fig.height=6, fig.width=7.2, fig.align = "center", message=FALSE, warning=FALSE----
# Plot the map with no fill and voters as points
options(repr.plot.width = 7.2, repr.plot.height = 6)
map <- ggplot() +
  geom_sf(data = ersd_maps, fill = "white") +
  geom_sf_label(data = ersd_maps, aes(label = WARD), size = 3) +
  geom_sf(data = voter_file_w_ward, size = 4, color = "black") +
  xlab("Latitude") +
  ylab("Longitude") +
  theme_bw(base_size = 10) +
  theme(
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 5)),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 5))
  )
show(map)

## ----merge_to_census----------------------------------------------------------
voter_file_w_ward$state <- rep("36", 5)
voter_file_w_ward$county <- rep("087", 5)
voter_file_w_ward$tract <- c("010801", "012202", "012501", "011502", "012202")
voter_file_w_ward$block <- c("1016", "3002", "1016", "4001", "2004")

## ----apply_bisg---------------------------------------------------------------
# Load Rockland County Census information
data(rockland_census)

rockland_census$NY$year <- 2010

# Apply BISG to the voter file to get race predictions
voter_file_with_race <- eiCompare::wru_predict_race_wrapper(
  voter_file = as.data.frame(voter_file_w_ward),
  census_data = rockland_census,
  voter_id = "voter_id",
  surname = "surname",
  state = "NY",
  county = "county",
  tract = "tract",
  block = "block",
  census_geo = "block",
  use_surname = TRUE,
  surname_only = FALSE,
  surname_year = 2010,
  use_age = FALSE,
  use_sex = FALSE,
  return_surname_flag = TRUE,
  return_geocode_flag = TRUE,
  verbose = FALSE
)

## ----race_probs---------------------------------------------------------------
print(voter_file_with_race[, c(
  "voter_id",
  "surname",
  "pred.whi",
  "pred.bla",
  "pred.his",
  "pred.asi"
)])

## ----voter_file---------------------------------------------------------------
# Load Ramapo 2018 voter file
data("ramapo2018")
print(head(ramapo2018, 10))

## ----performance_analysis-----------------------------------------------------
# Load Ramapo 2018 voter file
data("ramapo2018")

# Run Performance Analysis
results <- eiCompare::performance_analysis(
  voter_file = ramapo2018,
  census_data = rockland_census,
  join_census_shape = FALSE,
  join_district_shape = FALSE,
  state = "NY",
  voter_id = "voter_id",
  surname = "last_name",
  district = "ward",
  census_state_col = "state",
  census_county_col = "county",
  census_tract_col = "tract",
  census_block_col = "block",
  crs = NULL,
  coords = c("lon", "lat"),
  census_geo = "block",
  use_surname = TRUE,
  surname_only = FALSE,
  surname_year = 2010,
  use_age = FALSE,
  use_sex = FALSE,
  normalize = TRUE,
  verbose = TRUE
)

## ----visualize_results, fig.align = "center", fig.height=5, fig.width=5, message=FALSE, warning=FALSE----
options(repr.plot.width = 5, repr.plot.height = 5)

# Get minority aggregate turnout
performance <- results$results
ersd_maps$MIN_AGG_FRAC_TURNOUT <- performance$pred.bla_prop + performance$pred.his_prop

# Run performance analysis
performance_comparison <- ggplot(ersd_maps, aes(x = MIN_AGG_FRAC, y = MIN_AGG_FRAC_TURNOUT)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "red",
    size = 1
  ) +
  geom_text(aes(label = WARD), hjust = 0.45, vjust = 1.75) +
  geom_vline(
    xintercept = 0.5,
    linetype = "dotted",
    color = "purple",
    size = 1
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  ggtitle("Minority Aggregate Voter Share") +
  xlab("CVAP Prediction") +
  ylab("Performance Analysis Prediction") +
  coord_fixed() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 15, face = "bold")
  )
show(performance_comparison)

