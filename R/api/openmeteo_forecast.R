# Forecast data from open-meteo.com ----

# align openmeteo names with internal names
# units are the same
openmeteo_vars <- c(
  "temperature" = "temperature_2m", # C
  "dew_point" = "dew_point_2m", # C
  "relative_humidity" = "relative_humidity_2m", # %
  "precip" = "precipitation", # mm
  "snow" = "snowfall", # cm
  "wind_speed" = "wind_speed_10m", # kmh
  "wind_gust" = "wind_gusts_10m", # kmh
  "wind_direction" = "wind_direction_10m", # degrees
  "pressure_mean_sea_level" = "pressure_msl" # hpa = mbar
)

# handle returned hourly data
parse_openmeteo_forecast <- function(resp) {
  resp$hourly %>%
    as_tibble() %>%
    unnest(names(.)) %>%
    mutate(across(time, parse_datetime)) %>%
    # align to IBM time 20 min past the hour
    mutate(across(time, ~.x + minutes(20))) %>%
    select(datetime_utc = time, all_of(openmeteo_vars)) %>%
    mutate(dew_point_depression = abs(temperature - dew_point), .after = dew_point)
}

# request forecast data for lat lng point
get_openmeteo_forecast <- function(lat, lng, parse = TRUE) {
  t <- now()
  vars <- paste(openmeteo_vars, collapse = ",")
  base_url <- sprintf("https://api.open-meteo.com/v1/forecast?latitude=%.3f&longitude=%.3f", lat, lng)
  url <- paste0(base_url, "&forecast_days=14&hourly=", vars)
  df <- tibble()
  tryCatch({
    stopifnot(validate_ll(lat, lng))
    req <- request(url) %>%
      req_timeout(3) %>%
      req_retry(max_tries = 2, retry_on_failure = TRUE, after = \(resp) 0)
    resp <- req_perform(req) %>%
      resp_body_json()
    message(sprintf("GET => '%s[...]' completed in %.5f", base_url, now() - t))
    if (parse) parse_openmeteo_forecast(resp) else resp
  }, error = function(e) {
    message("Failed to get forecast from ", url, ": ", e$message)
    tibble()
  })
}

# get_openmeteo_forecast(45, -89, parse = FALSE)
# get_openmeteo_forecast(45, -89)
# get_openmeteo_forecast(60, -100)
