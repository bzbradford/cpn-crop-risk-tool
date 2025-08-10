# NOAA forecast api ----

# urls for valid lat/lng points
noaa_point_url <- function(lat, lng) {
  sprintf("https://api.weather.gov/points/%.6g,%.6g", lat, lng)
}

# noaa_point_url(45, -89)
# noaa_point_url(c(45, 46), c(-89, -90))
# noaa_point_url(c(1, 46), c(1, -90))


# need to get the hourly forecast url from the first api
noaa_get_forecast_url <- function(lat, lng, url = noaa_point_url(lat, lng)) {
  tryCatch({
    stopifnot(validate_ll(lat, lng))
    req <- request(url) %>%
      req_timeout(.5) %>%
      req_retry(max_tries = 3, retry_on_failure = TRUE, after = \(resp) 0)
    t <- now()
    resp <- req_perform(req) %>% resp_body_json()
    duration <- now() - t
    message(sprintf("GET => '%s' completed in %.5f", url, duration))
    resp$properties$forecastHourly
  }, error = function(e) {
    message("Failed to retrieve ", url, ": ", e$message)
    if ("httr2_http_404" %in% class(e)) "404" else NULL
  })
}

# # should succeed
# noaa_get_forecast_url(45, -89)
# noaa_get_forecast_url(38, -121)
#
# # should fail due to lat/lng
# noaa_get_forecast_url(1, 1)
#
# # should fail due to 404
# noaa_get_forecast_url(50, -90)


# parse NOAA hourly forecast data
# units: temperature = F, wind_speed = mph
noaa_parse_forecast <- function(periods) {
  periods %>%
    lapply(function(p) {
      # hoist the nested values
      p$probabilityOfPrecipitation <- p$probabilityOfPrecipitation$value
      # indicate 1 mm of precip if prob > 50% since I don't know how much will fall
      p$precip <- ifelse(p$probabilityOfPrecipitation > 50, 1, 0)
      # dewpoint is provided in Celsius
      p$dewPoint <- p$dewpoint$value
      p$relativeHumidity <- p$relativeHumidity$value
      p$windSpeed <- as.numeric(str_split_i(p$windSpeed, " ", 1))
      p
    }) %>%
    enframe() %>%
    select(value) %>%
    unnest_wider("value") %>%
    janitor::clean_names() %>%
    mutate(
      across(c(start_time, end_time), parse_datetime),
      across(c(start_time, end_time), ~.x + minutes(20)), # align to IBM time 20 min past the hour
      across(temperature, ~signif(f_to_c(.x), 4)),
      across(wind_speed, mi_to_km),
      across(wind_direction, wind_dir_to_deg)
    ) %>%
    select(
      datetime_utc = start_time,
      temperature,
      precip,
      dew_point,
      relative_humidity,
      wind_speed,
      wind_direction
    ) %>%
    mutate(dew_point_depression = abs(temperature - dew_point), .after = dew_point)
}


# get NOAA hourly forecast data
noaa_get_forecast <- function(url) {
  tryCatch({
    req <- request(url) %>%
      req_timeout(5) %>%
      req_retry(max_tries = 3, retry_on_failure = TRUE)
    t <- now()
    resp <- req_perform(req) %>% resp_body_json()
    message(sprintf("GET => '%s' completed in %.5f", url, now() - t))
    noaa_parse_forecast(resp$properties$periods)
  }, error = function(e) {
    message("Failed to get forecast from ", url, ": ", e$message)
    tibble()
  })
}

# # should succeed
# noaa_get_forecast_url(45, -89) %>% noaa_get_forecast()
# noaa_get_forecast_url(38, -121) %>% noaa_get_forecast()
#
# # should fail
# noaa_get_forecast(NULL)
# noaa_get_forecast("foo")
