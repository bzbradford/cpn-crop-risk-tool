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

#' handle valid response from open meteo
#' @param resp a response
om_parse_resp <- function(resp) {
  if (resp_is_error(resp)) {
    message("HTTP ERROR ==> ", resp$request$url)
    warning(
      "Request failed with status ",
      resp_status(resp),
      ": ",
      resp_status_desc(resp)
    )
    return(tibble())
  }

  tryCatch(
    {
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
          datetime_local = ymd_hm(time, tz = json$timezone),
          datetime_utc = with_tz(datetime_local, "UTC"),
          date = as_date(datetime_local),
          .after = time
        ) |>
        select(-time)

      bind_cols(attr, hourly)
    },
    error = function(e) {
      message("Parsing response failed: ", e$message)
      tibble()
    }
  )
}

if (FALSE) {
  # historical weather
  req <- om_build_req(45, -89, today() - days(1), today(), "temperature_2m")
  resp <- req_perform(req)
  om_parse_resp(resp)
  range(df$datetime_local)

  # forecast
  om_build_forecast_req(45, -89) |>
    req_perform() |>
    om_parse_resp() |>
    view()
}


## Format response ----

#' Creates the working hourly weather dataset from parsed openmeteo response
#' @param wx hourly weather data from `parse_openmeteo` function
#' @param is_forecast tag rows as forecast (TRUE) or history (FALSE)
om_build_hourly <- function(wx, is_forecast = FALSE) {
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
    mutate(is_forecast = is_forecast, .after = date) |>
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

#' identify unique grids from weather data returned by API
#' @param wx weather data from API, used to construct distinct weather grids
#' @param dx half grid dim in x (lng) direction
#' @param dy half grid dim in y (lat) direction
#' @param eps precision used for rounding centroid coordinates
om_build_grids <- function(wx, dx = 0.07, dy = 0.035, eps = 0.0025) {
  tz_lookup <- wx |> distinct(grid_id, timezone)

  wx |>
    distinct(grid_lat, grid_lng, grid_id) |>
    mutate(
      cx = round_to(grid_lng, eps),
      cy = round_to(grid_lat, eps),
      xmin = cx - dx,
      xmax = cx + dx,
      ymin = cy - dy,
      ymax = cy + dy
    ) |>
    mutate(
      geometry = sprintf(
        "POLYGON((%.4f %.4f, %.4f %.4f, %.4f %.4f, %.4f %.4f, %.4f %.4f))",
        xmin,
        ymax,
        xmax,
        ymax,
        xmax,
        ymin,
        xmin,
        ymin,
        xmin,
        ymax
      )
    ) |>
    st_as_sf(wkt = "geometry", crs = 4326) |>
    left_join(tz_lookup, join_by(grid_id))
}

if (FALSE) {
  om_build_grids(wx)

  wx |>
    om_build_grids() |>
    leaflet() |>
    addTiles() |>
    addPolygons()
}


#' Determine date range and list of dates with complete weather by grid
#' @param wx hourly weather data from API
om_wx_status <- function(wx) {
  per_day <- wx |>
    arrange(grid_id, date, datetime_utc) |>
    summarize(
      hours_actual = n_distinct(datetime_utc),
      .by = c(grid_id, date)
    ) |>
    mutate(
      hours_expected = if_else(date == today("UTC"), hour(now("UTC")), 24L),
      complete = hours_actual >= hours_expected
    )

  complete_dates <- per_day |>
    filter(complete) |>
    summarize(dates_have = list(unique(date)), .by = grid_id)

  latest_utc <- wx |>
    summarize(latest_datetime_utc = max(datetime_utc), .by = grid_id)

  per_day |>
    summarize(
      date_min = min(date),
      date_max = max(date),
      hours_actual = sum(hours_actual),
      hours_expected = sum(hours_expected),
      days_expected = n(),
      days_actual = sum(complete),
      .by = grid_id
    ) |>
    mutate(
      days_missing = days_expected - days_actual,
      days_missing_pct = days_missing / days_expected,
      hours_missing = pmax(0, hours_expected - hours_actual),
      hours_missing_pct = hours_missing / hours_expected
    ) |>
    left_join(latest_utc, join_by(grid_id)) |>
    mutate(
      hours_stale = if_else(
        date_max == today("UTC"),
        as.numeric(difftime(now("UTC"), latest_datetime_utc, units = "hours")),
        0
      ),
      needs_download = days_missing > 0 | hours_stale > 1
    ) |>
    select(-latest_datetime_utc) |>
    left_join(complete_dates, join_by(grid_id))
}

if (FALSE) {
  om_wx_status(wx)
}

#' Build full grid status by joining grid and weather constructors
#' @param wx hourly weather data
om_grid_status <- function(wx) {
  x <- om_build_grids(wx) |>
    st_drop_geometry()
  y <- om_wx_status(wx)
  left_join(x, y, join_by(grid_id))
}

if (FALSE) {
  om_grid_status(wx)
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
  grids <- if (nrow(wx) == 0) {
    NULL
  } else {
    om_grid_status(wx)
  }
  reqs <- om_prep_reqs(sites, start_date, end_date, grids)

  message(sprintf(
    "Getting weather for %s sites from %s to %s with %s requests",
    nrow(sites),
    start_date,
    end_date,
    nrow(reqs)
  ))

  reqs$resp <- req_perform_parallel(reqs$req, on_error = "continue")

  message(sprintf("Requests completed in %.4f", as.numeric(now() - t0)))
  echo(reqs$resp)

  parsed <- reqs |>
    reframe(req_lat, req_lng, om_parse_resp(resp))

  if ("grid_id" %in% names(parsed)) {
    om_build_hourly(parsed)
  } else {
    tibble()
  }
}

if (FALSE) {
  wx1 <- om_fetch_weather(sites1, today() - days(1), today())
  om_wx_status(wx1)
  wx2 <- om_fetch_weather(sites1, today() - days(2), today(), wx1)
  om_wx_status(wx2)
}


## Unified fetch (history + forecast) ----

#' Fetch historical and forecast weather in one parallel batch
#' @param sites sites df with `lat` and `lng` cols
#' @param start_date start of requested date range
#' @param end_date end of requested date range
#' @param wx existing weather tibble (empty tibble if none)
#' @param fc_fetched_at named list keyed by grid_id, value = POSIXct of last forecast fetch
#' @param fc_stale_hours hours before a forecast is considered stale and re-fetched
om_fetch_unified <- function(
  sites,
  start_date,
  end_date,
  wx = tibble(),
  fc_fetched_at = list(),
  fc_stale_hours = 1
) {
  t0 <- now()
  start_date <- as.Date(start_date)
  end_date <- min(as.Date(end_date), today())

  grids <- if (nrow(wx) == 0) NULL else om_grid_status(wx)

  # 1. History requests
  hist_reqs <- om_prep_reqs(sites, start_date, end_date, grids)

  # 2. Forecast requests (only if end_date >= today)
  fc_reqs <- tibble()
  if (end_date >= today()) {
    fc_coords <- if (!is.null(grids)) {
      om_join_grids(sites, grids) |>
        mutate(
          req_lat = coalesce(grid_lat, lat),
          req_lng = coalesce(grid_lng, lng),
          gid = sprintf("%.3f,%.3f", req_lat, req_lng)
        ) |>
        distinct(req_lat, req_lng, gid)
    } else {
      sites |>
        distinct(req_lat = lat, req_lng = lng) |>
        mutate(gid = sprintf("%.3f,%.3f", req_lat, req_lng))
    }

    fc_coords <- fc_coords |>
      rowwise() |>
      filter(
        is.null(fc_fetched_at[[gid]]) ||
          as.numeric(difftime(now(), fc_fetched_at[[gid]], units = "hours")) >
            fc_stale_hours
      ) |>
      ungroup()

    if (nrow(fc_coords) > 0) {
      fc_reqs <- fc_coords |>
        rowwise() |>
        mutate(req = list(om_build_forecast_req(req_lat, req_lng))) |>
        ungroup()
    }
  }

  n_hist <- nrow(hist_reqs)
  n_fc <- nrow(fc_reqs)

  if (n_hist == 0 && n_fc == 0) {
    message("No requests needed")
    return(list(wx = wx, fc_fetched_at = fc_fetched_at))
  }

  message(sprintf("Fetching %s history + %s forecast requests", n_hist, n_fc))

  # 3. Combine and perform all requests in one parallel batch
  all_reqs <- bind_rows(
    if (n_hist > 0) mutate(hist_reqs, .is_fc = FALSE) else NULL,
    if (n_fc > 0) mutate(fc_reqs, .is_fc = TRUE) else NULL
  )

  all_reqs$resp <- req_perform_parallel(all_reqs$req, on_error = "continue")

  message(sprintf(
    "Requests completed in %.2f s",
    as.numeric(difftime(now(), t0, units = "secs"))
  ))

  # 4. Parse history responses
  new_hist <- if (n_hist > 0) {
    filter(all_reqs, !.is_fc) |>
      reframe(req_lat, req_lng, om_parse_resp(resp)) |>
      om_build_hourly(is_forecast = FALSE)
  } else {
    tibble()
  }

  # 5. Parse forecast responses
  new_fc <- if (n_fc > 0) {
    filter(all_reqs, .is_fc) |>
      reframe(req_lat, req_lng, om_parse_resp(resp)) |>
      om_build_hourly(is_forecast = TRUE)
  } else {
    tibble()
  }

  # 6. Merge into existing wx
  refreshed_grids <- if (nrow(new_fc) > 0) {
    unique(new_fc$grid_id)
  } else {
    character(0)
  }

  merged <- wx

  # Drop stale forecast rows for grids that just got a fresh forecast
  if (
    length(refreshed_grids) > 0 &&
      nrow(merged) > 0 &&
      "is_forecast" %in% names(merged)
  ) {
    merged <- merged |> filter(!(is_forecast & grid_id %in% refreshed_grids))
  }

  # Add new history rows (anti-join avoids duplicates)
  if (nrow(new_hist) > 0) {
    merged <- om_merge_wx(merged, new_hist)
  }

  # Append new forecast rows
  if (nrow(new_fc) > 0) {
    merged <- bind_rows(merged, new_fc) |> arrange(grid_id, datetime_utc)
  }

  # 7. Update forecast fetch timestamps for refreshed grids
  updated_fc_fetched_at <- fc_fetched_at
  for (gid in refreshed_grids) {
    updated_fc_fetched_at[[gid]] <- now()
  }

  list(wx = merged, fc_fetched_at = updated_fc_fetched_at)
}

if (FALSE) {
  res <- om_fetch_unified(sites1, today() - days(1), today())
  om_wx_status(res$wx)
  res2 <- om_fetch_unified(
    sites1,
    today() - days(3),
    today() + days(7),
    res$wx,
    res$fc_fetched_at
  )
  # second call should skip all requests
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
    arrange(grid_id, datetime_utc)
}

if (FALSE) {
  wx <- om_merge_wx(wx1, wx2)
  om_wx_status(wx)
}
