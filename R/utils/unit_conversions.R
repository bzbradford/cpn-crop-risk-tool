# Unit conversions ----

f_to_c <- function(x) {
  (x - 32) / 1.8
}

c_to_f <- function(x) {
  x * 1.8 + 32
}

mm_to_in <- function(x) {
  x / 25.4
}

cm_to_in <- function(x) {
  x / 2.54
}

mi_to_km <- function(x) {
  x * 1.609
}

km_to_mi <- function(x) {
  x / 1.609
}

kmh_to_mps <- function(x) {
  x / 3.6
}

mps_to_mph <- function(x) {
  x * 2.237
}

mbar_to_inHg <- function(x) {
  x / 33.864
}

# Wind directions
compass_directions <- setNames(
  seq(0, 337.5, 22.5),
  c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
)

#' Convert wind direction to degrees
#' @param dirs vector of wind direction strings
#' @returns vector of wind direction angles in degrees
wind_dir_to_deg <- function(dirs) {
  sapply(dirs, function(dir) {
    if (dir %in% names(compass_directions)) compass_directions[[dir]] else NA
  })
}


#' List of weather variables, unit suffixes, and conversion functions
#' all derivative columns of each of these will start with the same text
#' e.g. temperature => temperature_min => temperature_min_30ma
measures <- tribble(
  ~measure, ~metric, ~imperial, ~conversion,
  "temperature", "°C", "°F", c_to_f,
  "dew_point", "°C", "°F", c_to_f,
  "relative_humidity", "%", "%", \(x) x, # no conversion
  "precip", "mm", "in", mm_to_in,
  "snow", "cm", "in", cm_to_in,
  "wind_speed", "kmh", "mph", km_to_mi,
  "wind_gust", "kmh", "mph", km_to_mi,
  "wind_direction", "°", "°", \(x) x, # no conversion
  "pressure_mean_sea_level", "mbar", "inHg", mbar_to_inHg,
  "pressure_change", "mbar", "inHg", mbar_to_inHg,
)

#' Converts all measures from default metric to imperial values
#' operates on any of the major datasets
#' @param df data frame from the hourly set or beyond
#' @returns df with column data converted
convert_measures <- function(df) {
  for (i in seq_len(nrow(measures))) {
    m <- measures[i, ]
    df <- mutate(df, across(starts_with(m$measure), m$conversion[[1]]))
  }
  df
}

# saved_weather %>% build_hourly() %>% convert_measures()
# saved_weather %>% build_hourly() %>% build_daily() %>% convert_measures()


#' Returns the unit name for the given column
#' used to append unit suffix in plotly
#' @param col_name data column name that needs a unit suffix
#' @param unit_system
find_unit <- function(col_name, unit_system = c("metric", "imperial")) {
  unit_system <- match.arg(unit_system)
  matched <- measures %>%
    rowwise() %>%
    filter(grepl(measure, col_name))
  if (nrow(matched) == 1) matched[[unit_system]] else ""
}


#' Adds the unit suffix to each column name where appropriate
#' for exporting as CSV so unit is documented
#' @param df from the data pipeline
#' @param unit_system
#' @returns df with updated column names
rename_with_units <- function(df, unit_system = c("metric", "imperial")) {
  unit_system <- match.arg(unit_system)
  for (i in seq_len(nrow(measures))) {
    m <- measures[i, ]
    df <- df %>%
      rename_with(
        .fn = ~ paste(.x, m[[unit_system]], sep = "_", recycle0 = TRUE),
        .cols = starts_with(m$measure)
      )
  }
  clean_names(df)
}
