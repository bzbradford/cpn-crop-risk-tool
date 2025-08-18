# Weather helpers ----

#' df must have cols `date` and `datetime_local`
add_date_cols <- function(df) {
  df %>%
    mutate(
      yday = yday(date),
      year = year(date),
      month = month(date),
      day = day(date),
      hour = hour(datetime_local),
      night = !between(hour, 7, 19), # night is between 20:00 and 6:00
      date_since_night = as_date(datetime_local + hours(4)),
      .after = date,
      .by = grid_id
    )
}


#' Summarize downloaded weather data by grid cell and creates sf object
#' used to intersect site points with existing weather data
#' @param wx hourly weather data from `clean_ibm` function
#' @param start_date start of expected date range
#' @param end_date end of expected date range
#' @returns tibble
weather_status <- function(wx, start_date = min(wx$date), end_date = max(wx$date)) {
  dates_expected <- seq.Date(start_date, end_date, 1)
  wx <- filter(wx, between(date, start_date, end_date))

  if (nrow(wx) == 0) {
    fallback_df <- tibble(grid_id = NA, grid_lat = NA, grid_lng = NA, needs_download = TRUE)
    return(fallback_df)
  }

  wx %>%
    summarize(
      tz = coalesce(first(time_zone), "UTC"),
      date_min = min(date),
      date_max = max(date),
      days_expected = length(dates_expected),
      days_actual = n_distinct(date),
      days_missing = days_expected - days_actual,
      days_missing_pct = days_missing / days_expected,
      time_min_expected = ymd_hms(paste(start_date, "00:20:00"), tz = tz),
      time_min_actual = min(datetime_local),
      time_max_expected = min(now(tzone = tz), ymd_hms(paste(end_date, "23:20:00"), tz = tz)),
      time_max_actual = max(datetime_local),
      hours_expected = hours_diff(time_max_expected, time_min_expected),
      hours_actual = n(),
      hours_missing = hours_expected - hours_actual,
      hours_missing_pct = hours_missing / hours_expected,
      hours_stale = hours_diff(time_max_expected, time_max_actual),
      stale = hours_stale > OPTS$ibm_stale_hours,
      needs_download = stale | days_missing > 0,
      .by = grid_id
    ) %>%
    select(-tz)
}

# weather_status(saved_weather, start_date = ymd("2025-1-1"), end_date = ymd("2025-2-21"))
# saved_weather %>% weather_status(today() - 7,today())


#' Similar to weather_status but returns number of hours per day
#' to check for any incomplete days
#' @param wx hourly weather data
#' @param tz time
daily_status <- function(wx, tz = "UTC") {
  if (length(unique(wx$grid_id)) > 1) warning("More than 1 gridpoint sent to daily_status")
  wx %>%
    summarize(hours = n(), .by = date) %>%
    mutate(
      start_hour = ymd_hms(paste(date, "00:20:00"), tz = tz),
      end_hour = if_else(
        date == today(tzone = tz),
        now(tzone = tz),
        ymd_hms(paste(date, "23:20:00"), tz = tz)
      ),
      hours_expected = hours_diff(end_hour, start_hour) + 1,
      hours_missing = hours_expected - hours
    )
}

# saved_weather %>% filter(grid_id == sample(grid_id, 1)) %>% daily_status(tz = "America/Chicago") %>% tail()


#' Update weather for sites list and date range
#' @param wx existing weather data
#' @param sites sf with site locs
#' @param start_date
#' @param end_date
fetch_weather <- function(wx, sites, start_date, end_date) {
  all_dates <- seq.Date(start_date, end_date, 1)
  sites <- sites %>% st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE)
  reqs <- list()

  # for each site see how much weather is needed
  for (i in seq_len(nrow(sites))) {
    site <- slice(sites, i)

    # already have some weather? find dates to download
    dates_need <- all_dates
    dates_have <- Date()

    if (nrow(wx) > 0) {
      grids <- build_grids(wx)
      wx_status <- weather_status(wx, start_date, end_date)
      grid_status <- grids %>%
        left_join(wx_status, join_by(grid_id))
      site <- st_join(site, grid_status)

      # if weather is up to date don't download
      if (isFALSE(site$needs_download)) next

      # if there is at least one day already downloaded check each date for completeness
      if (isTruthy(site$days_actual)) {
        tz <- site$time_zone
        date_status <- wx %>%
          filter(grid_id == site$grid_id) %>%
          filter(between(date, start_date, end_date)) %>%
          daily_status(tz = tz)
        dates_have <- date_status %>%
          filter(hours_missing <= 2) %>%
          pull(date)
        dates_need <- as_date(setdiff(all_dates, dates_have))
      }
    }

    # skip if up to date
    if (length(dates_need) == 0) next

    # create requests
    new_reqs <- create_ibm_reqs(
      lat = site$lat,
      lng = site$lng,
      dates_need = dates_need,
      dates_have = dates_have
    )
    reqs <- append(reqs, new_reqs)
  }

  # send requests if any
  if (length(reqs) == 0) {
    message("No weather requests in queue")
    return()
  }

  resp <- get_ibm(reqs)

  # handle response
  if (nrow(resp) == 0) {
    message("Failed to get any weather response")
    return(tibble())
  }

  # process response
  status_msg <- NULL
  new_wx <- resp %>%
    clean_ibm() %>%
    build_hourly()

  wx <- bind_rows(new_wx, wx) %>%
    distinct(grid_id, datetime_utc, .keep_all = TRUE) %>%
    arrange(grid_id, datetime_utc)

  wx
}

# fetch_weather(tibble(), tibble(lat = 45, lng = -89), today() - days(7), today())
