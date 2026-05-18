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
  wind_gust = "wind_gusts_10m",
  wind_direction = "wind_direction_10m",
  # "wind_direction_100m",
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
  # url <- "https://archive-api.open-meteo.com/v1/archive"
  url <- "https://customer-archive-api.open-meteo.com/v1/archive"
  request(url) |>
    req_url_query(
      latitude = lat,
      longitude = lng,
      start_date = start,
      end_date = end,
      timezone = "auto",
      hourly = vars,
      apikey = OPTS$open_meteo_key,
      .multi = "comma"
    ) |>
    req_timeout(10) |>
    req_error(is_error = \(resp) FALSE)
}

if (FALSE) {
  resp <- om_build_req(45, -89, today() - days(1), today()) |>
    req_perform()
  resp_body_json(resp)
  om_resp_ok(resp)
  om_parse_json(resp)
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
  # url <- "https://api.open-meteo.com/v1/forecast"
  url <- "https://customer-api.open-meteo.com/v1/forecast"
  request(url) |>
    req_url_query(
      latitude = lat,
      longitude = lng,
      forecast_days = days,
      timezone = "auto",
      hourly = vars,
      apikey = OPTS$open_meteo_key,
      .multi = "comma"
    ) |>
    req_timeout(10) |>
    req_error(is_error = \(resp) FALSE)
}

if (FALSE) {
  resp <- om_build_forecast_req(45, -89) |>
    req_perform()
  resp$request$url
  resp_body_json(resp)
  om_resp_ok(resp)
  om_parse_json(resp)
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
    if (!is.null(detail)) {
      msg <- paste0(msg, ": ", detail)
    }
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
    elevation = json$elevation,
    timezone = json$timezone,
    tz_offset = json$timezone_abbreviation
  )
  # Open-Meteo with timezone=auto returns bare local-time strings (no offset),
  # so we must parse with the location's tz; ymd_hm() without tz defaults to UTC
  # and would shift every timestamp by utc_offset_seconds.
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
}

#' Validate then parse a single response; returns empty tibble on any failure
om_parse_resp <- function(resp) {
  if (!om_resp_ok(resp)) {
    return(tibble())
  }
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
    om_parse_resp()
}


## Format response ----

fmt_grid_id <- function(grid_lat, grid_lng) {
  sprintf("%.3f,%.3f", grid_lat, grid_lng)
}

#' Creates the working hourly weather dataset from parsed openmeteo response
#' @param wx hourly weather data from `parse_openmeteo` function. If it does not
#'   have a grid_id column one will be generated
om_build_hourly <- function(wx) {
  if (nrow(wx) == 0) {
    return(tibble())
  }

  if (!("grid_id") %in% names(wx)) {
    wx <- mutate(wx, grid_id = sprintf("%.3f,%.3f", grid_lat, grid_lng))
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
      dew_point_depression = pmax(0, temperature - dew_point),
      .after = dew_point
    ) |>
    mutate(soil_moisture = soil_moisture * 100) |>
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


## Grid cell generator ----

#' Determine grid size from Open Meteo response centroid coordinates
#' Assumes data comes from ECMWF IFS which uses an O1280 grid

# Build the O1280 grid geometry once, cache in package env / memoise / .GlobalEnv
.o1280_cache <- new.env(parent = emptyenv())
.build_o1280 <- function(N = 1280) {
  if (!is.null(.o1280_cache$gauss_lats)) {
    return(invisible())
  }

  # generate Gauss-Legendre nodes
  gq <- statmod::gauss.quad(2 * N, kind = "legendre")
  # nodes are in [-1, 1] ascending; flip to descending so index 1 = ring nearest N pole
  nodes <- rev(gq$nodes)
  gauss_lats <- asin(nodes) * 180 / pi # length 2N, descending

  # Cell latitude edges: midpoints of adjacent Gaussian lats, with +/-90 at the caps.
  # edges[j]   = north edge of ring j
  # edges[j+1] = south edge of ring j
  edges <- c(90, 0.5 * (gauss_lats[-length(gauss_lats)] + gauss_lats[-1]), -90)

  .o1280_cache$N <- N
  .o1280_cache$gauss_lats <- gauss_lats
  .o1280_cache$edges <- edges
  # Pre-sort descending edges into ascending negatives for fast findInterval
  .o1280_cache$neg_edges_asc <- -edges
  invisible()
}

#' Identify centroid and bounding box for O1280 grid cells given lat/lng
#' @param lat,lng coordinates of target point
#' @return A tibble with grid_lat, grid_lng, grid_id, and an sfc geometry
#'   column in EPSG:4326.
get_o1280_cells <- function(lat, lng) {
  stopifnot(length(lat) == length(lng))
  .build_o1280()
  N <- .o1280_cache$N
  gauss_lats <- .o1280_cache$gauss_lats
  edges <- .o1280_cache$edges

  # Snap each input lat to its ring index (1-based in R).
  # edges is descending: edges[j] > center[j] > edges[j+1].
  # findInterval on -edges (ascending) gives j directly.
  j <- findInterval(-lat, .o1280_cache$neg_edges_asc, all.inside = TRUE)
  j <- pmin(pmax(j, 1L), 2L * N)

  # k = ring distance from nearest pole (1..N)
  k <- pmin(j, 2L * N - j + 1L)
  n_lng <- 20L + 4L * (k - 1L)
  d_lng <- 360 / n_lng

  # Latitude edges (exact)
  ymax <- edges[j]
  ymin <- edges[j + 1L]

  # Snap incoming lon to nearest ring longitude center. Rings start at lon = 0;
  # centers are i * d_lng for i = 0..n_lng-1. Normalize input to [0, 360) first.
  lng_pos <- (lng %% 360 + 360) %% 360
  i <- round(lng_pos / d_lng) %% n_lng
  c_lng <- i * d_lng
  # Back to [-180, 180]
  c_lng <- ifelse(c_lng > 180, c_lng - 360, c_lng)

  xmin <- c_lng - d_lng / 2
  xmax <- c_lng + d_lng / 2

  # Build polygons. NB: cells that straddle the antimeridian (xmin < -180 or
  # xmax > 180) will need splitting if you care about rendering; flag here.
  wkt_vec <- sprintf(
    "POLYGON((%.10f %.10f, %.10f %.10f, %.10f %.10f, %.10f %.10f, %.10f %.10f))",
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
    grid_lat = gauss_lats[j],
    grid_lng = c_lng,
    grid_id = fmt_grid_id(grid_lat, grid_lng),
    geometry = st_as_sfc(wkt_vec, crs = 4326)
  )
}


#' Builds unique grids from downloaded weather data
#' @param wx weather data from `om_parse_resp` or `build_hourly`
om_build_wx_grids <- function(wx) {
  tz_lookup <- wx |>
    summarize(
      across(c(timezone, elevation), first),
      .by = grid_id
    )

  wx |>
    distinct(grid_id, grid_lat, grid_lng) |>
    mutate(get_o1280_cells(grid_lat, grid_lng)) |>
    st_as_sf() |>
    left_join(tz_lookup, join_by(grid_id)) |>
    select(grid_id, grid_lat, grid_lng, timezone, elevation, geometry)
}

#' Build each site's O1280 grid cell (canonical centroid + polygon) as an sf.
#' Geometry travels with the site so its cell can render before any weather is
#' downloaded. get_o1280_cells() already sets grid_id via fmt_grid_id().
#' @param sites sites df with `lat` and `lng` columns
om_build_site_grids <- function(sites) {
  sites |>
    mutate(get_o1280_cells(lat, lng)) |>
    select(id, name, lat, lng, grid_id, grid_lat, grid_lng, geometry) |>
    st_as_sf()
}

if (FALSE) {
  sites1 <- load_sites("data/hars-aars-msn.csv")
  om_build_site_grids(sites1)
  test_wx <- read_csv("dev/test_wx.csv")
  test_wx <- read_csv("data/Hourly data.csv") |>
    janitor::clean_names() |>
    mutate(grid_id = fmt_grid_id(grid_lat, grid_lng))
  test_wx |>
    distinct(grid_id)
  g <- om_build_wx_grids(test_wx)
  leaflet() |>
    addTiles() |>
    addPolygons(data = g)
  round(g$lat_c, 6) - round(g$grid_lat, 6)
}


## Grid and status helpers ----

#' Similar to weather_status but returns number of hours per day
#' to check for any incomplete days
#' @param wx hourly weather data
om_wx_daily_status <- function(wx) {
  wx |>
    summarize(
      tz = first(timezone),
      time_min = min(datetime_utc),
      time_max = max(datetime_utc),
      hours_actual = n(),
      .by = c(grid_id, date)
    ) |>
    mutate(
      start_hour = ymd_hms(paste(date, "00:00:00"), tz = first(tz)),
      end_hour = if_else(
        date == today(tzone = first(tz)),
        now(tzone = first(tz)),
        ymd_hms(paste(date, "23:00:00"), tz = first(tz))
      ),
      hours_expected = hours_diff(start_hour, end_hour) + 1,
      hours_missing = pmax(0, hours_expected - hours_actual),
      .by = grid_id
    )
}

if (FALSE) {
  test_wx <- read_csv("dev/test_wx.csv")
  om_wx_daily_status(test_wx)
}


#' Summarize downloaded weather data by grid cell and creates sf object
#' used to intersect site points with existing weather data
#' @param wx hourly weather data from `om_build_hourly` function
#' @param start_date start of expected date range
#' @param end_date end of expected date range
#' @returns tibble
om_wx_status <- function(
  wx,
  start_date = if (nrow(wx) > 0) min(wx$date),
  end_date = if (nrow(wx) > 0) max(wx$date)
) {
  # Single canonical empty schema — every return path matches this
  status_template <- tibble(
    grid_id = character(),
    date_min = as.Date(character()),
    date_max = as.Date(character()),
    time_min = as.POSIXct(character()),
    time_max = as.POSIXct(character()),
    days_expected = integer(),
    days_actual = integer(),
    days_incomplete = integer(),
    days_missing = integer(),
    hours_expected = integer(),
    hours_missing = integer(),
    hours_actual = integer(),
    hours_stale = integer(),
    needs_download = logical(),
    dates_have = list(),
    dates_missing = list()
  )

  if (nrow(wx) == 0) {
    return(status_template)
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  dates_expected <- seq.Date(start_date, end_date, by = "day")
  n_expected <- length(dates_expected)
  max_hours_missing <- 2L

  daily <- om_wx_daily_status(wx)

  # Every grid that appears anywhere in wx — survives the partial case
  all_grids <- distinct(daily, grid_id)

  # Dates with adequate coverage (across all time, not just selected range)
  dates_have <- daily |>
    filter(hours_missing <= max_hours_missing) |>
    summarize(
      dates_have = list(unique(date)),
      .by = grid_id
    )

  # Per-grid stats restricted to the selected window.
  # Guard the summarize: with zero rows, dplyr still evaluates min()/max()
  # on empty vectors for type inference, which produces warnings.
  sel_daily <- filter(daily, between(date, start_date, end_date))

  sel_status <- if (nrow(sel_daily) > 0L) {
    sel_daily |>
      summarize(
        date_min = min(date),
        date_max = max(date),
        time_min = min(time_min),
        time_max = max(time_max),
        days_actual = n_distinct(date),
        days_incomplete = as.integer(sum(hours_missing > max_hours_missing)),
        hours_expected = as.integer(sum(hours_expected)),
        hours_missing = as.integer(sum(hours_missing)),
        hours_stale = if_else(
          date_max == today(tzone = first(tz)),
          pmax(0L, hours_diff(time_max, now(tzone = first(tz)))),
          0L
        ),
        .by = grid_id
      )
  } else {
    # Zero-row prototype matching the summarize schema.
    # Take time_min/time_max from `daily` so POSIXct tz attributes carry over.
    tibble(
      grid_id = character(),
      date_min = as.Date(character()),
      date_max = as.Date(character()),
      time_min = daily$time_min[0],
      time_max = daily$time_max[0],
      days_actual = integer(),
      days_incomplete = integer(),
      hours_expected = integer(),
      hours_missing = integer(),
      hours_stale = integer()
    )
  }

  all_grids |>
    left_join(sel_status, by = "grid_id") |>
    left_join(dates_have, by = "grid_id") |>
    mutate(
      days_expected = n_expected,
      days_actual = coalesce(days_actual, 0L),
      days_incomplete = coalesce(days_incomplete, 0L),
      days_missing = pmax(0L, days_expected - days_actual),
      hours_expected = coalesce(hours_expected, 24L * n_expected),
      hours_missing = coalesce(hours_missing, hours_expected),
      hours_actual = hours_expected - hours_missing,
      hours_stale = coalesce(hours_stale, 0L),
      needs_download = days_missing > 0L | days_incomplete > 0L,
      # Derive dates_missing from dates_have so list-col NA/NULL is handled cleanly.
      # (setdiff strips the Date class — restore it explicitly.)
      dates_missing = purrr::map(dates_have, \(have) {
        if (is.null(have) || length(have) == 0L) {
          dates_expected
        } else {
          as.Date(setdiff(dates_expected, have), origin = "1970-01-01")
        }
      })
    ) |>
    select(all_of(names(status_template)))
}

if (FALSE) {
  test_wx <- read_csv("dev/test_wx.csv")
  om_wx_status(test_wx)

  # handle no weather
  test_wx |>
    filter(FALSE) |>
    om_wx_status()

  # handle date range with no weather
  range(test_wx$date)
  test_wx |>
    om_wx_status(start_date = ymd("2024-1-1"), end_date = ymd("2024-2-1"))

  om_grid_status(test_wx)
  om_grid_status(test_wx, as_date("2026-1-1"), today()) |>
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
  x <- om_build_wx_grids(wx)
  y <- om_wx_status(wx, start_date, end_date)
  left_join(x, y, join_by(grid_id))
}

if (FALSE) {
  om_grid_status(wx)
  om_wx_status(test_wx)
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

#' Generate the requests list for a set of grid cells and date range
#' @param grids df of grid cells to fetch, must have `grid_id`, `grid_lat`,
#'   `grid_lng` (centroid). Requests are issued at the grid centroid so
#'   Open-Meteo resolves the cell we expect, not its nearest neighbor.
#' @param start_date start of requested date range, date or "YYYY-MM-DD" string
#' @param end_date end of requested date range
#' @param wx_status if weather already exists, the `om_grid_status()` summary
#'   used to fetch only the missing date runs per grid
om_prep_reqs <- function(grids, start_date, end_date, wx_status = NULL) {
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

  grids <- distinct(grids, grid_id, grid_lat, grid_lng)

  # if there already is some weather data, fetch only the missing date runs
  df <- if (!is.null(wx_status)) {
    grids |>
      left_join(
        wx_status |>
          sf::st_drop_geometry() |>
          select(grid_id, dates_have),
        join_by(grid_id)
      ) |>
      reframe(
        om_build_chunks(
          start_date,
          end_date,
          # reduce(c) preserves the Date class (unlist() would coerce to
          # numeric and crash as.Date() inside om_build_chunks)
          purrr::reduce(dates_have, c, .init = as.Date(character()))
        ),
        .by = c(grid_id, grid_lat, grid_lng)
      )
  } else {
    grids |>
      mutate(
        start_date = !!start_date,
        end_date = !!end_date,
        days = as.integer(end_date - start_date) + 1
      )
  }

  if (nrow(df) == 0) {
    return(tibble())
  }

  # one request per (grid, date run), issued at the grid centroid
  df |>
    rowwise() |>
    mutate(
      req = list(om_build_req(
        lat = grid_lat,
        lng = grid_lng,
        start = start_date,
        end = end_date
      ))
    )
}

if (FALSE) {
  g1 <- om_build_site_grids(sites1)
  om_prep_reqs(g1, "2025-12-30", "2026-01-10", om_grid_status(test_wx))
  om_prep_reqs(g1, "2025-12-30", "2026-01-10")
}


## Response collection ----

#' Count successful responses from `req_perform_parallel()`
#' (neither a captured error condition nor an HTTP error status)
#' @param resps list of responses
n_resp_ok <- function(resps) {
  sum(vapply(
    resps,
    \(r) !inherits(r, "error") && !resp_is_error(r),
    logical(1L)
  ))
}

#' Parse parallel responses and stamp them with the caller's canonical grid
#' identity. The API's nearest-centroid (grid_lat/grid_lng from om_parse_json)
#' is discarded and replaced with the grid_id/grid_lat/grid_lng carried on
#' `reqs`, keeping grid identity consistent with `om_build_site_grids()`.
#' @param reqs rowwise df with `grid_id`, `grid_lat`, `grid_lng`, and a `resp`
#'   list column from `req_perform_parallel()`
om_collect_responses <- function(reqs) {
  centroids <- distinct(reqs, grid_id, grid_lat, grid_lng)
  reqs |>
    reframe(grid_id, om_parse_resp(resp)) |>
    select(-any_of(c("grid_lat", "grid_lng"))) |>
    left_join(centroids, join_by(grid_id)) |>
    om_build_hourly()
}


## Build and execute data requests ----

#' Get hourly data for a set of grid cells from start to end date.
#' Requests are issued at each grid centroid and the returned rows are stamped
#' with the caller's canonical grid_id / grid_lat / grid_lng so grid identity
#' stays consistent with `om_build_site_grids()` / `om_build_wx_grids()`.
#' @param grids df with `grid_id`, `grid_lat`, `grid_lng`
#' @param wx optional existing weather, used to fetch only missing dates
om_fetch_weather <- function(grids, start_date, end_date, wx = tibble()) {
  t0 <- now()
  grids <- distinct(grids, grid_id, grid_lat, grid_lng)
  wx_status <- if (nrow(wx) > 0) om_grid_status(wx) else NULL
  reqs <- om_prep_reqs(grids, start_date, end_date, wx_status)

  if (nrow(reqs) == 0) {
    message("No new data needed")
    return(wx)
  }

  message(sprintf(
    "Fetching weather: %d grids, %s to %s (%d requests)",
    nrow(grids),
    start_date,
    end_date,
    nrow(reqs)
  ))

  reqs$resp <- req_perform_parallel(reqs$req, on_error = "continue")
  n_ok <- n_resp_ok(reqs$resp)

  message(sprintf(
    "Completed in %.1fs: %d/%d succeeded",
    as.numeric(now() - t0, units = "secs"),
    n_ok,
    nrow(reqs)
  ))

  if (n_ok == 0) {
    warning("All requests failed")
    return(NULL)
  }

  om_collect_responses(reqs)
}

if (FALSE) {
  g1 <- om_build_site_grids(sites1)
  wx1 <- om_fetch_weather(g1, today() - days(1), today())
  om_wx_status(wx1)
  wx2 <- om_fetch_weather(g1, today() - days(2), today(), wx1)
  om_wx_status(wx2)
}

#' Get forecast data for grids. Unlike `om_fetch_weather`, forecasts are fetched
#' using the grid_id and grid centroid of the historical weather data
#' @param grids df with cols `grid_id`, `grid_lat`, `grid_lng`
om_fetch_forecast <- function(grids) {
  t0 <- now()
  message("Fetching forecasts for ", nrow(grids), " grids")

  reqs <- grids |>
    rowwise() |>
    mutate(req = list(om_build_forecast_req(grid_lat, grid_lng)))

  reqs$resp <- req_perform_parallel(reqs$req, on_error = "continue")
  n_ok <- n_resp_ok(reqs$resp)

  message(sprintf(
    "Completed in %.1fs: %d/%d succeeded",
    as.numeric(now() - t0, units = "secs"),
    n_ok,
    nrow(reqs)
  ))

  if (n_ok == 0) {
    message("All forecast requests failed")
    return(NULL)
  }

  om_collect_responses(reqs)
}

if (FALSE) {
  sites1 <- load_sites("dev/hars-aars-msn.csv")

  wx <- om_build_site_grids(sites1) |>
    om_fetch_weather(today() - days(1), today())
  wx

  fc <- om_build_wx_grids(wx) |>
    om_fetch_forecast()
  fc

  build_daily(wx)
  build_daily(fc)

  # should match
  unique(wx$grid_id)
  unique(fc$grid_id)
}


## Merge weather ----

#' efficiently add downloaded weather to existing weather
#' @param wx1 existing weather data frame
#' @param wx2 new weather to merge onto existing
om_merge_wx <- function(wx1, wx2) {
  bind_rows(wx1, wx2) |>
    arrange(grid_id, datetime_utc) |>
    distinct(grid_id, datetime_utc, .keep_all = TRUE) |>
    drop_na(datetime_utc)
}

if (FALSE) {
  wx <- om_merge_wx(wx1, wx2)
  om_wx_status(wx)
}
