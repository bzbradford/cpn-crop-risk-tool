# OpenMeteo Hourly API ---------------------------------------------------------

# "hourly_units": {
#   "time": "iso8601",
#   "temperature_2m": "°C",
#   "relative_humidity_2m": "%",
#   "dew_point_2m": "°C",
#   "apparent_temperature": "°C",
#   "precipitation": "mm",
#   "rain": "mm",
#   "snowfall": "cm",
#   "snow_depth": "m",
#   "weather_code": "wmo code",
#   "pressure_msl": "hPa",
#   "surface_pressure": "hPa",
#   "cloud_cover": "%",
#   "cloud_cover_low": "%",
#   "cloud_cover_mid": "%",
#   "cloud_cover_high": "%",
#   "et0_fao_evapotranspiration": "mm",
#   "vapour_pressure_deficit": "kPa",
#   "wind_speed_10m": "km/h",
#   "wind_speed_100m": "km/h",
#   "wind_direction_10m": "°",
#   "wind_direction_100m": "°",
#   "wind_gusts_10m": "km/h",
#   "soil_temperature_0_to_7cm": "°C",
#   "soil_temperature_7_to_28cm": "°C",
#   "soil_temperature_28_to_100cm": "°C",
#   "soil_temperature_100_to_255cm": "°C",
#   "soil_moisture_0_to_7cm": "m³/m³",
#   "soil_moisture_7_to_28cm": "m³/m³",
#   "soil_moisture_28_to_100cm": "m³/m³",
#   "soil_moisture_100_to_255cm": "m³/m³"
# }

# list of variables to ask for and possibly rename
openmeteo_vars <- vctrs::vec_c(
  temperature = "temperature_2m",
  # "apparent_temperature",
  dew_point = "dew_point_2m",
  relative_humidity = "relative_humidity_2m",
  evapotranspiration = "et0_fao_evapotranspiration",
  precipitation = "precipitation", # = rain + snowfall
  rain = "rain",
  snowfall = "snowfall",
  snow_depth = "snow_depth",
  pressure_msl = "pressure_msl",
  # "vapour_pressure_deficit",
  # "surface_pressure",
  wind_speed = "wind_speed_10m",
  # "wind_speed_100m",
  wind_direction = "wind_direction_10m",
  # "wind_direction_100m",
  wind_gust = "wind_gusts_10m",
  # "cloud_cover",
  # "cloud_cover_low",
  # "cloud_cover_mid",
  # "cloud_cover_high",
  soil_temp = "soil_temperature_0_to_7cm",
  # "soil_temperature_7_to_28cm",
  # "soil_temperature_28_to_100cm",
  # "soil_temperature_100_to_255cm",
  soil_moisture = "soil_moisture_0_to_7cm",
  # "soil_moisture_7_to_28cm",
  # "soil_moisture_28_to_100cm",
  # "soil_moisture_100_to_255cm",
  # "weather_code",
)

# make sure all of these are in the conversion lookup table
stopifnot(
  setdiff(names(openmeteo_vars), conversion_lookup$measure) |>
    length() ==
    0
)

## Build requests ----

#' request forecast data for lat lng point
#' @param lat latitude of point
#' @param lng longitude of point
#' @param start start date YYYY-MM-DD
#' @param end end date
#' @param vars vector of variables to get
om_build_req <- function(
  lat,
  lng,
  start,
  end,
  vars = openmeteo_vars
) {
  url <- "https://archive-api.open-meteo.com/v1/archive"
  request(url) |>
    req_url_query(
      latitude = lat,
      longitude = lng,
      start_date = start,
      end_date = end,
      timezone = "auto",
      hourly = vars,
      .multi = "comma"
    ) |>
    req_timeout(10) |>
    req_error(is_error = \(resp) FALSE)
}

if (FALSE) {
  om_build_req(45, -89, today() - days(1), today()) |>
    req_perform() |>
    resp_body_json()
}


#' request forecast data for lat lng point
#' @param lat latitude of point
#' @param lng longitude of point
#' @param vars vector of variables to get
#' @param days number of forecast days eg 7, 14, 16
om_build_forecast_req <- function(
  lat,
  lng,
  vars = openmeteo_vars,
  days = 16
) {
  url <- "https://api.open-meteo.com/v1/forecast"
  request(url) |>
    req_url_query(
      latitude = lat,
      longitude = lng,
      forecast_days = days,
      timezone = "auto",
      hourly = vars,
      .multi = "comma"
    ) |>
    req_timeout(10) |>
    req_error(is_error = \(resp) FALSE)
}

if (FALSE) {
  om_build_forecast_req(45, -89) |>
    req_perform() |>
    resp_body_json()
}


## Parse response ----

# Validate a response from req_perform_parallel. Handles both error conditions
# (network failures/timeouts) and HTTP error statuses. Returns FALSE and emits
# a diagnostic message on any failure; TRUE if safe to parse.
om_resp_ok <- function(resp) {
  if (inherits(resp, "error")) {
    message("Network error: ", conditionMessage(resp))
    return(FALSE)
  }
  if (resp_is_error(resp)) {
    detail <- tryCatch(resp_body_json(resp)$reason, error = \(e) NULL)
    msg <- paste(resp_status(resp), resp_status_desc(resp))
    if (!is.null(detail)) msg <- paste0(msg, ": ", detail)
    message("HTTP error [", msg, "] => ", resp$request$url)
    return(FALSE)
  }
  TRUE
}

# Parse a validated response into a tidy tibble (assumes resp passed om_resp_ok)
om_parse_json <- function(resp) {
  json <- resp_body_json(resp)
  attr <- tibble(
    grid_lat = json$latitude,
    grid_lng = json$longitude,
    grid_id = sprintf("%.3f,%.3f", grid_lat, grid_lng),
    elevation = json$elevation,
    timezone = json$timezone,
    tz_offset = json$timezone_abbreviation
  )
  hourly <- json$hourly |>
    as_tibble() %>%
    unnest(names(.)) |>
    mutate(
      datetime_utc = ymd_hm(time),
      datetime_local = with_tz(datetime_utc, json$timezone),
      date = as_date(datetime_local),
      .after = time
    ) |>
    select(-time)
  bind_cols(attr, hourly)
}

#' Validate then parse a single response; returns empty tibble on any failure
om_parse_resp <- function(resp) {
  if (!om_resp_ok(resp)) return(tibble())
  tryCatch(
    om_parse_json(resp),
    error = function(e) {
      message("Parse failed: ", e$message)
      tibble()
    }
  )
}

if (FALSE) {
  req <- om_build_req(45, -89, today() - days(1), today(), "temperature_2m")
  resp <- req_perform(req)
  om_parse_resp(resp)

  om_build_forecast_req(45, -89) |>
    req_perform() |>
    om_parse_resp() |>
    view()
}


## Format response ----

#' Creates the working hourly weather dataset from parsed openmeteo response
#' @param wx hourly weather data from `parse_openmeteo` function
om_build_hourly <- function(wx) {
  if (nrow(wx) == 0) {
    return(tibble())
  }

  wx |>
    select(
      grid_id,
      grid_lat,
      grid_lng,
      elevation,
      timezone,
      tz_offset,
      datetime_utc,
      datetime_local,
      date,
      all_of(openmeteo_vars)
    ) |>
    mutate(
      dew_point_depression = abs(temperature - dew_point),
      .after = dew_point
    ) |>
    add_date_cols() |>
    arrange(grid_lat, grid_lng, datetime_local)
}

if (FALSE) {
  wx <- om_build_req(45, -89, today() - days(1), today()) |>
    req_perform() |>
    om_parse_resp() |>
    om_build_hourly()
  wx

  om_build_forecast_req(45, -89) |>
    req_perform() |>
    om_parse_resp() |>
    om_build_hourly() |>
    view()
}


## Grid and status helpers ----

#' Determine grid size from Open Meteo response centroid coordinates
#' Assumes data comes from ECMWF IFS which uses an O1280 grid
#' @param lat grid centroid latitude vector
#' @param lng grid centroid longitude vector
get_o1280_cells <- function(grid_lat, grid_lng) {
  N <- 1280
  d_lat <- 180 / (2 * N) # constant latitudinal step (~0.0703125)

  # identify the latitude ring index (j)
  # j = 0 is the first ring below the North Pole
  j <- floor((90 - grid_lat) / d_lat)
  j <- pmax(0, pmin(j, (2 * N) - 1))

  # calculate latitude boundaries
  ymax <- 90 - (j * d_lat)
  ymin <- 90 - ((j + 1) * d_lat)

  # determine longitude spacing for each ring
  # k is the distance from the NEAREST pole (1 to N)
  k <- pmin(j + 1, (2 * N) - j)
  n_lng <- 20 + 4 * (k - 1)
  d_lng <- 360 / n_lng

  # find the longitude index (i) and center
  i <- round(grid_lng / d_lng)
  c_lng <- i * d_lng

  # calculate longitude boundaries (center +/- half-width)
  xmin <- c_lng - (d_lng / 2)
  xmax <- c_lng + (d_lng / 2)

  # construct polygons using WKT
  wkt_vec <- sprintf(
    "POLYGON((%.5f %.5f, %.5f %.5f, %.5f %.5f, %.5f %.5f, %.5f %.5f))",
    xmin,
    ymin,
    xmax,
    ymin,
    xmax,
    ymax,
    xmin,
    ymax,
    xmin,
    ymin
  )

  tibble(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    geometry = st_as_sfc(wkt_vec, crs = 4326)
  )
}

#' Builds unique grids from downloaded weather data
#' @param wx weather data from `om_parse_resp` or `build_hourly`
om_build_grids <- function(wx) {
  tz_lookup <- wx |> distinct(grid_id, timezone)

  wx |>
    distinct(grid_id, grid_lat, grid_lng) |>
    mutate(get_o1280_cells(grid_lat, grid_lng)) |>
    st_as_sf() |>
    left_join(tz_lookup, join_by(grid_id))
}


#' Similar to weather_status but returns number of hours per day
#' to check for any incomplete days
#' @param wx hourly weather data
#' @param tz time
om_wx_daily_status <- function(wx) {
  wx |>
    summarize(
      tz = coalesce(first(timezone), "UTC"),
      hours = n(),
      .by = c(grid_id, date)
    ) |>
    mutate(
      start_hour = ymd_hms(paste(date, "00:20:00"), tz = first(tz)),
      end_hour = if_else(
        date == today(tzone = first(tz)),
        now(tzone = tz),
        ymd_hms(paste(date, "23:20:00"), tz = first(tz))
      ),
      hours_expected = hours_diff(start_hour, end_hour) + 1,
      hours_missing = hours_expected - hours
    )
}


#' Summarize downloaded weather data by grid cell and creates sf object
#' used to intersect site points with existing weather data
#' @param wx hourly weather data from `ibm_clean_resp` function
#' @param start_date start of expected date range
#' @param end_date end of expected date range
#' @returns tibble
om_wx_status <- function(wx, start_date, end_date) {
  default <- tibble(
    grid_id = NA,
    needs_download = TRUE
  )

  if (nrow(wx) == 0) {
    return(default)
  }

  selected_wx <- wx |>
    filter(between(date, start_date, end_date))

  if (nrow(selected_wx) == 0) {
    return(default)
  }

  dates_expected <- seq.Date(start_date, end_date, 1)
  stale_timeout <- 2

  # summarize for each grid
  selected_wx |>
    om_wx_daily_status() |>
    summarize(
      tz = first(tz),
      date_min = min(date),
      date_max = max(date),
      time_min = min(start_hour),
      time_max = max(end_hour),
      days_expected = length(dates_expected),
      days_actual = n_distinct(date),
      days_incomplete = sum(hours_missing > stale_timeout),
      days_missing = max(0, days_expected - days_actual),
      dates_have = list(unique(date)),
      dates_missing = list(setdiff(dates_expected, date)),
      hours_expected = sum(hours_expected),
      hours_missing = sum(hours_missing),
      hours_stale = if_else(
        date_max == today(tzone = tz),
        hours_diff(time_max, now(tzone = tz)),
        0
      ),
      stale = hours_stale > stale_timeout,
      needs_download = stale | days_missing > 0 | days_incomplete > 0,
      .by = grid_id
    ) |>
    select(-tz)
}

if (FALSE) {
  test_wx <- read_csv("dev/test_wx.csv")
  om_wx_status(wx)
  om_grid_status(wx)
  om_grid_status(wx, as_date("2026-1-1"), today()) |>
    annotate_grids() |>
    pull(dates_missing)
}

#' Build full grid status by joining grid and weather constructors
#' @param wx hourly weather data
om_grid_status <- function(
  wx,
  start_date = min(wx$date),
  end_date = max(wx$date)
) {
  x <- om_build_grids(wx)
  y <- om_wx_status(wx, start_date, end_date)
  left_join(x, y, join_by(grid_id))
}

if (FALSE) {
  om_grid_status(wx)
  om_wx_status(wx)
}


#' Non-spatial join using site lat/lng and grid extents
#' @param sites sites df with `lat` and `lng` cols
#' @param grid grid df from `om_grid_status()`
om_join_grids <- function(sites, grids) {
  sites |>
    left_join(
      grids,
      join_by(lng >= xmin, lng <= xmax, lat >= ymin, lat <= ymax)
    )
}

if (FALSE) {
  sites1 <- load_sites("dev/hars-aars-msn.csv")
  om_grid_status(wx)
  om_join_grids(sites1, om_grid_status(wx))

  sites2 <- load_sites("dev/wisconet stns.csv")
  om_join_grids(sites2, om_grid_status(wx))
}


## Date range chunker ----

#' Identify runs of dates that need data
#' @param start_date start of range
#' @param end_date end of range
#' @param existing_dates vector of dates for which data already exists
om_build_chunks <- function(start_date, end_date, existing_dates) {
  # Ensure inputs are Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  existing_dates <- as.Date(existing_dates)

  # Create the full sequence of dates requested
  full_seq <- seq.Date(from = start_date, to = end_date, by = "day")

  # Identify dates that are NOT in the existing_dates vector
  missing_dates <- full_seq[!(full_seq %in% existing_dates)]

  # If there are no missing dates, return an empty list
  if (length(missing_dates) == 0) {
    return(tibble())
  }

  # Identify continuous runs
  # We find breaks where the difference between consecutive missing dates is > 1 day
  breaks <- c(0, which(diff(missing_dates) != 1), length(missing_dates))

  # Build the result list
  runs <- lapply(seq_len(length(breaks) - 1), function(i) {
    run_indices <- (breaks[i] + 1):breaks[i + 1]
    run_dates <- missing_dates[run_indices]

    tibble(
      start_date = min(run_dates),
      end_date = max(run_dates),
      days = length(run_dates)
    )
  })

  bind_rows(runs)
}

if (FALSE) {
  om_build_chunks(
    start_date = "2026-01-01",
    end_date = "2026-01-15",
    existing_dates = c(
      "2026-01-02",
      "2026-01-03",
      "2026-01-07",
      "2026-01-08",
      "2026-01-09"
    )
  )
}


## Build requests list ----

#' Generate requests list from sites, date range, and existing data
#' @param sites sites df, must have `lat` and `lng` cols
#' @param start_date start of requested date range, date or "YYYY-MM-DD" string
#' @param end_date end of requested date range
#' @param grids if already have weather, provide grid summary from `om_grid_status()`
om_prep_reqs <- function(sites, start_date, end_date, grids = NULL) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (start_date > end_date) {
    warning(sprintf(
      "Invalid fetch dates, start_date %s must be before end_date %s",
      start_date,
      end_date
    ))
    return(tibble())
  }

  # if there already is some weather data, match sites to grids
  df <- if (!is.null(grids)) {
    sites |>
      om_join_grids(grids) |>
      mutate(
        req_lat = coalesce(grid_lat, lat),
        req_lng = coalesce(grid_lng, lng)
      ) |>
      reframe(
        om_build_chunks(start_date, end_date, unlist(dates_have)),
        .by = c(req_lat, req_lng)
      )
  } else {
    sites |>
      distinct(lat, lng) |>
      select(req_lat = lat, req_lng = lng) |>
      mutate(
        start_date = !!start_date,
        end_date = !!end_date,
        days = as.integer(end_date - start_date) + 1
      )
  }

  # build requests for each row
  df |>
    rowwise() |>
    mutate(
      req = list(om_build_req(
        lat = req_lat,
        lng = req_lng,
        start = start_date,
        end = end_date
      ))
    )
}

if (FALSE) {
  om_prep_reqs(sites1, "2025-12-30", "2026-01-10", om_grid_status(wx))
  om_prep_reqs(sites1, "2025-12-30", "2026-01-10")
}


## Build and execute data requests ----

#' get hourly data for sites from start to end date
#' optionally include existing weather to identify needs
om_fetch_weather <- function(sites, start_date, end_date, wx = tibble()) {
  t0 <- now()
  grids <- if (nrow(wx) > 0) om_grid_status(wx) else NULL
  reqs <- om_prep_reqs(sites, start_date, end_date, grids)

  if (nrow(reqs) == 0) {
    message("No new data needed")
    return(wx)
  }

  message(sprintf(
    "Fetching weather: %d sites, %s to %s (%d requests)",
    nrow(sites), start_date, end_date, nrow(reqs)
  ))

  reqs$resp <- req_perform_parallel(reqs$req, on_error = "continue")

  n_ok <- sum(vapply(reqs$resp, \(r) !inherits(r, "error") && !resp_is_error(r), logical(1L)))
  message(sprintf(
    "Completed in %.1fs: %d/%d succeeded",
    as.numeric(now() - t0, units = "secs"), n_ok, nrow(reqs)
  ))

  parsed <- reqs |>
    reframe(req_lat, req_lng, om_parse_resp(resp))

  if (!"grid_id" %in% names(parsed)) return(tibble())
  om_build_hourly(parsed)
}

if (FALSE) {
  wx1 <- om_fetch_weather(sites1, today() - days(1), today())
  om_wx_status(wx1)
  wx2 <- om_fetch_weather(sites1, today() - days(2), today(), wx1)
  om_wx_status(wx2)
}

#' get forecast data for sites
#' @param sites df with cols `lat`, `lng`, and `id`
om_fetch_forecast <- function(sites) {
  t0 <- now()
  message("Fetching forecasts for ", nrow(sites), " sites")

  reqs <- sites |>
    rowwise() |>
    mutate(req = list(om_build_forecast_req(lat, lng)))

  reqs$resp <- req_perform_parallel(reqs$req, on_error = "continue")

  n_ok <- sum(vapply(reqs$resp, \(r) !inherits(r, "error") && !resp_is_error(r), logical(1L)))
  message(sprintf(
    "Completed in %.1fs: %d/%d succeeded",
    as.numeric(now() - t0, units = "secs"), n_ok, nrow(reqs)
  ))

  parsed <- reqs |>
    reframe(site_id = id, lat, lng, om_parse_resp(resp))

  if (!"grid_id" %in% names(parsed)) return(tibble())

  site_id_lookup <- parsed |> distinct(site_id, grid_id)
  hourly <- om_build_hourly(parsed |> select(-site_id))
  if (nrow(hourly) == 0) return(tibble())

  hourly |> left_join(site_id_lookup, join_by(grid_id))
}

if (FALSE) {
  fc <- om_fetch_forecast(sites1)
  fc
  build_daily(wx)

  wx
}


## Merge weather ----

#' efficiently add downloaded weather to existing weather
#' @param wx1 existing weather data frame
#' @param wx2 new weather to merge onto existing
om_merge_wx <- function(wx1, wx2) {
  if (nrow(wx1) == 0) {
    return(wx2)
  }

  if (nrow(wx2) == 0) {
    return(wx1)
  }

  new_wx <- anti_join(wx2, wx1, join_by(grid_id, datetime_utc))
  bind_rows(wx1, new_wx) |>
    arrange(grid_id, datetime_utc) |>
    drop_na(datetime_utc)
}

if (FALSE) {
  wx <- om_merge_wx(wx1, wx2)
  om_wx_status(wx)
}
