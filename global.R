#-- global.R --#

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor) # name cleaning
  library(sf) # GIS
  library(fst) # file storage
  library(httr2) # requests
  library(markdown)
  library(zoo) # rollmean
  library(future) # async
  library(promises) # async

  library(shiny)
  library(shinythemes)
  library(shinyWidgets)
  # library(shinyBS)
  library(shinyjs)
  library(shinyalert)
  library(htmltools)
  library(shinycssloaders)

  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  library(DT)
  # library(gt)
})


# Dev settings ----

## development mode
# shiny::devmode(TRUE)

## RENV
# renv::update()
# renv::snapshot()
# renv::clean()

# renv::install("httr2@1.1.0")
# renv::install("terra@1.8-42")


## turn warnings into errors
# options(warn = 2)

# disable NOAA forecasts for testing
# options(forecast = FALSE)



# Startup ----------------------------------------------------------------------

# set up a second session for asynchronous tasks
plan(multisession, workers = 2)

# EPSG 4326 for use in Leaflet
service_bounds <- read_rds("data/us_ca_clip.rds")

# transform to EPSG 3857 web mercator for intersecting points
service_bounds_3857 <- st_transform(service_bounds, 3857)



# Functions --------------------------------------------------------------------

## Utility ----

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)), " <", paste(class(x), collapse = ", "), ">")
  print(x)
}

runtime <- function(label = "timestamp", ref = NULL) {
  t <- now()
  message(">> ", label, " [", t, "]")
  if (!is.null(ref))
    message(difftime(t, ref), " elapsed")
  return(t)
}

# swaps names and values in a list or vector
invert <- function(x) {
  y <- as(names(x), class(x))
  names(y) <- x
  y
}

# create named list from sublist elements
build_choices <- function(obj, name, value) {
  setNames(
    sapply(obj, \(x) x[[value]]),
    sapply(obj, \(x) x[[name]])
  )
}

# return the first truthy argument
first_truthy <- function(...) {
  for (arg in list(...)) if (shiny::isTruthy(arg)) return(arg)
  NULL
}

# NA-safe summary functions
calc_sum <- function(x) {
  if (all(is.na(x))) return(NA)
  sum(x, na.rm = TRUE)
}

calc_min <- function(x) {
  if (all(is.na(x))) return(NA)
  min(x, na.rm = TRUE)
}

calc_mean <- function(x) {
  if (all(is.na(x))) return(NA)
  mean(x, na.rm = TRUE)
}

calc_max <- function(x) {
  if (all(is.na(x))) return(NA)
  max(x, na.rm = TRUE)
}

roll_mean <- function(vec, width) {
  zoo::rollapplyr(vec, width, \(x) calc_mean(x), partial = T)
}

roll_sum <- function(vec, width) {
  zoo::rollapplyr(vec, width, \(x) calc_sum(x), partial = T)
}

# counts number consecutive runs of values above a threshold
count_runs <- function(vec, threshold, min_run) {
  runs <- run <- 0
  for (val in vec) {
    run <- if (val >= threshold) run + 1 else 0
    if (run == min_run) runs <- runs + 1
  }
  runs
}


## Unit conversions ----

f_to_c       <- \(x) { (x - 32) / 1.8 }
c_to_f       <- \(x) { x * 1.8 + 32 }
mm_to_in     <- \(x) { x / 25.4 }
cm_to_in     <- \(x) { x / 2.54 }
mi_to_km     <- \(x) { x * 1.609 }
km_to_mi     <- \(x) { x / 1.609 }
kmh_to_mps   <- \(x) { x / 3.6 }
mps_to_mph   <- \(x) { x * 2.237 }
mbar_to_inHg <- \(x) { x / 33.864 }

compass_directions <- list(
  "N"   = 0,
  "NNE" = 22.5,
  "NE"  = 45,
  "ENE" = 67.5,
  "E"   = 90,
  "ESE" = 112.5,
  "SE"  = 135,
  "SSE" = 157.5,
  "S"   = 180,
  "SSW" = 202.5,
  "SW"  = 225,
  "WSW" = 247.5,
  "W"   = 270,
  "WNW" = 292.5,
  "NW"  = 315,
  "NNW" = 337.5
)

wind_dir_to_deg <- function(dirs) {
  sapply(dirs, function(dir) {
    if (dir %in% names(compass_directions)) compass_directions[[dir]] else NA
  })
}


## Location helpers ----

#' parse lat/lng coordinates from string
#' @param str input string containing coordinates to parse in form "lat, lng"
#' @returns named list { lat: numeric, lng: numeric }
parse_coords <- function(str) {
  str <- gsub("[ ,\t°NW]", " ", str)
  parts <- str_split_1(str_squish(str), " ")
  if (length(parts) != 2) stop("Invalid coordinate format.")
  coords <- suppressWarnings(list(
    lat = as.numeric(parts[1]),
    lng = as.numeric(parts[2])
  ))
  if (any(sapply(coords, is.na))) stop("Failed to parse coordinates.")
  coords
}

#' returns TRUE if location is within service boundary shapefile
#' @param lat latitude of point
#' @param lng longitude of point
#' @returns boolean
validate_ll <- function(lat, lng) {
  mapply(function(lat, lng) {
    if (!is.numeric(lat) | !is.numeric(lng)) return(F)
    pt <- st_point(c(lng, lat)) %>%
      st_sfc(crs = 4326) %>%
      st_transform(st_crs(service_bounds_3857))
    length(st_intersection(pt, service_bounds_3857)) == 1
  }, lat, lng)
}



# Settings ----

OPTS <- lst(

  ## general ----
  app_title = "Crop Risk Tool",
  app_header_color = "#00693c",
  app_header_badge = "cpn-badge.png",

  ## google ----
  google_geocoding_key = Sys.getenv("google_geocoding_key"),
  google_places_key = Sys.getenv("google_places_key"),

  ## ibm ----
  ibm_keys = list(
    org_id = Sys.getenv("ibm_org_id"),
    tenant_id = Sys.getenv("ibm_tenant_id"),
    api_key = Sys.getenv("ibm_api_key")
  ),
  ibm_auth_endpoint = "https://api.ibm.com/saascore/run/authentication-retrieve/api-key",
  ibm_weather_endpoint = "https://api.ibm.com/geospatial/run/v3/wx/hod/r1/direct",
  # max hours per api call
  ibm_chunk_size = 1000,
  ibm_ignore_cols = c(
    "requestedLatitude",
    "requestedLongitude",
    "iconCode",
    "iconCodeExtended",
    "drivingDifficultyIndex"
  ),
  # how old should weather be before allowing a refresh?
  ibm_stale_hours = 3,

  ## dates ----
  earliest_date = ymd("2015-1-1"),
  default_start_date = today() - 30,

  ## map ----
  # state_colors = {
  #   pals = RColorBrewer::brewer.pal.info
  #   pal = RColorBrewer::brewer.pal
  #   qual_col_pals = pals[pals$category == 'qual',]
  #   unlist(mapply(pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  # },
  map_bounds_wi = list(
    lat1 = 42.4,
    lat2 = 47.1,
    lng1 = -93.0,
    lng2 = -86.8
  ),
  map_bounds_us = list(
    lat1 = 24.0,
    lat2 = 50.0,
    lng1 = -125.5,
    lng2 = -66.0
  ),
  map_tiles = list(
    "ESRI Topo" = providers$Esri.WorldTopoMap,
    "Satellite" = providers$Esri.WorldImagery,
    "OpenStreetMap" = providers$OpenStreetMap,
    "Grey Canvas" = providers$CartoDB.Positron
  ),
  map_layers = list(
    # counties = "States/Counties"
    grid = "Weather grid"
  ),
  map_click_zoom = 10,


  ## sites ----
  # allowable names for site loading
  site_cols = c(
    name = "name",
    name = "location",
    lat = "lat",
    lat = "latitude",
    lng = "lng",
    lng = "long",
    lng = "longitude"
  ),
  max_sites = 25,
  validation_sites_ready = "No sites selected, click on the map or load sites in the sidebar.",
  validation_weather_ready = "No weather data downloaded yet for the selected dates. Click 'Fetch Weather' to download.",


  ## data tab ----
  data_type_choices = list(
    "Hourly" = "hourly",
    "Daily" = "daily",
    "Moving averages" = "ma",
    "Growing degree days" = "gdd"
    # "Disease models" = "disease"
  ),


  ## disease risk tab ----
  # crop_choices = setNames(names(crops), sapply(crops, \(x) x$name)),


  ## plotting ----
  plot_title_font = list(family = "Redhat Display", size = 14),
  plot_axis_font = list(family = "Redhat Text", size = 12),
  site_attr_cols = c("site_id", "site_name", "site_lat", "site_lng"),
  grid_attr_cols = c("grid_id", "grid_lat", "grid_lng", "time_zone", "date_min", "date_max", "days_expected", "days_actual", "days_missing", "days_missing_pct", "hours_expected", "hours_actual", "hours_missing", "hours_missing_pct", "geometry"),
  date_attr_cols = c("datetime_utc", "time_zone", "datetime_local", "date", "yday", "year", "month", "day", "hour", "night", "date_since_night"),
  daily_attr_cols = c("date", "yday", "year", "month", "day"),
  plot_default_cols = c("temperature", "temperature_mean", "temperature_mean_7day", "base_50_upper_86_cumulative"),
  plot_ignore_cols = c(site_attr_cols, grid_attr_cols, date_attr_cols),
)


# NOAA forecast api ------------------------------------------------------------

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
    message(sprintf("GET => '%s' completed in %.5f", url, now() - t))
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



# IBM API interface ------------------------------------------------------------

#' Get the authorization token from the authentication server
#' @param url IBM authentication endpoint
#' @param keys list with org_id, tenant_id, and api_key
refresh_auth <- function(url = OPTS$ibm_auth_endpoint, keys = OPTS$ibm_keys) {
  ibm_auth <<- tryCatch({
    req <- request(url) %>%
      req_url_query(orgId = keys$org_id) %>%
      req_headers_redacted(
        "x-ibm-client-id" = sprintf("saascore-%s", keys$tenant_id),
        "x-api-key" = keys$api_key
      ) %>%
      req_timeout(5) %>%
      req_error(is_error = \(resp) FALSE)
    resp <- req_perform(req)
    status <- resp_status(resp)

    if (status >= 400) {
      echo(resp)
      stop("HTTP error ", status)
    }

    message("Authorization token refreshed at ", now("UTC"))
    list(
      timestamp = now("UTC"),
      status = status,
      token = resp_body_string(resp)
    )
  }, error = function(e) {
    message("Failed to get authorization token: ", e$message)
    list(
      timestamp = 1,
      status = 500,
      token = NULL
    )
  })
  saveRDS(ibm_auth, "ibm_auth.rds")
}

# refresh_auth()


#' Get the current IBM token or refresh if needed
#' token is valid for 1 hour
#' @returns auth token
get_ibm_token <- function() {
  # look for a stored token if available
  if (!exists("ibm_auth")) {
    if (file.exists("ibm_auth.rds")) {
      ibm_auth <<- read_rds("ibm_auth.rds")
    } else {
      refresh_auth()
    }
  }

  # if token is stale get a new one
  if (ibm_auth$timestamp < now("UTC") - minutes(59)) refresh_auth()

  ibm_auth$token
}

# get_ibm_token()


#' Convert vector of dates to vector of hourly datetimes
dates_to_dttm <- function(dates, tz = "UTC") {
  max_len <- OPTS$ibm_chunk_size
  offset <- minutes(20)
  dttms <- seq(
    from = as_datetime(min(dates), tz = tz) + offset,
    to = as_datetime(max(dates), tz = tz) + hours(23) + offset,
    by = as.difftime(hours(1))
  )
  if (length(dttms) > max_len) warning("Sequence length of ", length(dttms), " longer than maximum ", max_len)
  dttms
}

# dates_to_dttm(seq.Date(today() - days(30), today(), 1))


#' Breaks up longer time periods into 1000 hour chunks
#' API will only return 1000 hours at a time
#' @param dates_need vector of dates needed
#' @param dates_have vector of dates already downloaded
#' @param tz time zone to use
#' @returns list of lists `list(start, end, length)`
ibm_chunks <- function(dates_need, dates_have = NULL, tz = "UTC") {
  if (all(dates_need %in% dates_have)) return(NULL)

  chunk_size <- 40 # days
  dates <- seq.Date(min(dates_need), max(dates_need), 1)
  n_chunks <- ceiling(length(dates) / chunk_size)
  i <- 1
  date_chunks <- list()

  while (length(dates) > 0) {
    # pick chunks from outside in
    if (i %% 2 != 0) {
      picked <- dates[1:min(chunk_size, length(dates))]
    } else {
      picked <- dates[max(1, length(dates) - chunk_size):length(dates)]
    }
    dates <- as_date(setdiff(dates, picked))
    date_chunks[[i]] <- picked
    i <- i + 1
  }

  # remove any chunks we already have covered
  if (length(dates_have) > 0) {
    date_chunks <- lapply(date_chunks, function(chunk) {
      if (all(chunk %in% dates_have)) NULL else chunk
    }) %>% compact()
  }

  dttm_chunks <- lapply(date_chunks, function(chunk) dates_to_dttm(chunk, tz))
  lapply(dttm_chunks, function(chunk) {
    list(
      start = first(chunk),
      end = last(chunk),
      length = length(chunk)
    )
  })
}

# # should show 4 chunks
# ibm_chunks(
#   dates_need = seq.Date(as_date("2024-3-1"), as_date("2024-8-1"), by = 1)
# )
#
# # should show 2 chunks (2 are covered)
# ibm_chunks(
#   dates_need = seq.Date(as_date("2024-3-1"), as_date("2024-8-1"), by = 1),
#   dates_have = seq.Date(as_date("2024-4-1"), as_date("2024-7-1"), by = 1)
# )
#
# # should be null (all dates covered)
# ibm_chunks(
#   dates_need = seq.Date(as_date("2024-5-1"), as_date("2024-7-1"), by = 1),
#   dates_have = seq.Date(as_date("2024-3-1"), as_date("2024-8-1"), by = 1)
# )


#' Create a single request for weather data, maximum length 1000 hours
#' @param lat latitude of point
#' @param lng longitude of point
#' @param start_time dttm
#' @param end_time dttm
#' @param url endpoint, changed only for testing failures
#' @param token IBM authentication token
#' @returns httr2 request
create_ibm_request <- function(lat, lng, start_time, end_time, url = OPTS$ibm_weather_endpoint, token = get_ibm_token()) {
  request(url) %>%
    req_headers_redacted(
      "x-ibm-client-id" = sprintf("geospatial-%s", OPTS$ibm_keys$tenant_id),
      "Authorization" = sprintf("Bearer %s", token)
    ) %>%
    req_url_query(
      format = "json",
      geocode = str_glue("{lat},{lng}"),
      startDateTime = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
      endDateTime = format(end_time, "%Y-%m-%dT%H:%M:%S%z"),
      units = "m"
    ) %>%
    req_timeout(5) %>%
    req_retry(max_tries = 2, failure_timeout = 5, retry_on_failure = TRUE) %>%
    req_throttle(rate = 20)
}

# # test
# create_ibm_request(45, -89, now() - days(10), now() - days(5)) %>%
#   req_perform()


#' Create a list of necessary to send to IBM for weather data
#' Using the date vectors it selects time chunks and creates draft requests
#' API documentation: https://docs.google.com/document/d/13HTLgJDpsb39deFzk_YCQ5GoGoZCO_cRYzIxbwvgJLI/edit?tab=t.0
#' @param lat latitude of point
#' @param lng longitude of point
#' @param dates_need vector of dates needed
#' @param dates_have vector of dates already downloaded
#' @returns httr2 list of requests
create_ibm_reqs <- function(lat, lng, dates_need, dates_have = Date()) {
  t <- now()
  tz <- lutz::tz_lookup_coords(lat, lng, warn = F)
  chunks <- ibm_chunks(dates_need, dates_have, tz)

  if (length(chunks) == 0) {
    message(str_glue("Already had weather for {lat}, {lng}"))
    return(tibble())
  }

  start_date <- min(dates_need)
  end_date <- max(dates_need)

  reqs <- lapply(chunks, function(chunk) {
    create_ibm_request(lat, lng, chunk$start, chunk$end)
  })

  message(sprintf("Built requests for %.3f,%.3f from %s to %s with %s calls in %.05f", lat, lng, start_date, end_date, length(reqs), now() - t))

  reqs
}

# test_ibm_reqs <- create_ibm_reqs(43.0731, -89.4012, dates_need = seq.Date(ymd("2025-1-1"), ymd("2025-4-1"), by = 1))

# str(test_ibm_reqs)



#' Execute the list of IBM weather requests
#' @param `reqs` list of IBM requests created by `create_ibm_reqs`
#' @returns tibble ingestable by `clean_ibm` and ready for the data pipeline
get_ibm <- function(reqs) {
  stime <- Sys.time()

  resps <- tryCatch({
    token <- get_ibm_token()
    if (!is.character(token)) stop("Failed to get IBM token.")

    message(sprintf("GET ==> %s IBM requests", length(reqs)))

    # perform parallel requests for each time chunk
    resps <- req_perform_parallel(reqs, on_error = "continue", progress = F)

    # gather response data
    lapply(resps, function(resp) {
      tryCatch({
        if ("httr2_failure" %in% class(resp)) {
          echo(resp)
          stop("Request failed")
        }
        if (resp_status(resp) != 200) stop(paste0("Received status ", resp_status(resp), " with message ", resp_status_desc(resp)))
        resp_body_json(resp, simplifyVector = T) %>% as_tibble()
      }, error = function(e) {
        message(e$message)
        tibble()
      })
    })
  }, error = function(e) {
    message(e$message)
    tibble()
  })

  wx <- bind_rows(resps)
  msg <- if (nrow(wx) > 0) {
    sprintf("OK ==> Performed %s requests in %s sec", length(reqs), round(Sys.time() - stime, 3))
  } else {
    "FAIL ==> Requests did not succeed"
  }
  message(msg)
  wx
}

# get_ibm(test_ibm_reqs)


#' Does some minimal processing on the IBM response to set local time and date
#' @param ibm_response hourly weather data received from API
#' @returns tibble
clean_ibm <- function(ibm_response) {
  if (nrow(ibm_response) == 0) return(tibble())
  ibm_response %>%
    select(-OPTS$ibm_ignore_cols) %>%
    select(
      grid_id = gridpointId,
      grid_lat = latitude,
      grid_lng = longitude,
      datetime_utc = validTimeUtc,
      everything()
    ) %>%
    clean_names() %>%
    mutate(across(datetime_utc, ~parse_date_time(.x, "YmdHMSz"))) %>%
    mutate(time_zone = lutz::tz_lookup_coords(grid_lat, grid_lng, warn = F), .after = datetime_utc) %>%
    mutate(datetime_local = with_tz(datetime_utc, first(time_zone)), .by = time_zone, .after = time_zone) %>%
    mutate(date = as_date(datetime_local), .after = datetime_local)
}

# test_ibm <- test_ibm_raw %>% clean_ibm()
# ggplot(test_ibm, aes(x = datetime_local, y = temperature)) + geom_line()


#' Update weather for sites list and date range
#' @param wx existing weather data
#' @param sites sf with site locs
#' @param start_date
#' @param end_date
fetch_weather <- function(wx, sites, start_date, end_date) {
  all_dates <- seq.Date(start_date, end_date, 1)
  sites <- sites %>% st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F)
  reqs <- list()

  # for each site see how much weather is needed
  for (i in 1:nrow(sites)) {
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
          daily_status()
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
    return (tibble())
  }

  # process response
  status_msg <- NULL
  new_wx <- resp %>%
    clean_ibm() %>%
    build_hourly()
  wx <- bind_rows(new_wx, wx) %>%
    distinct(grid_id, datetime_utc, .keep_all = T) %>%
    arrange(grid_id, datetime_utc)
  return(wx)
}

# fetch_weather(tibble(), tibble(lat = 45, lng = -89), today() - days(7), today())


# Weather helpers ---------------------------------------------------------

#' All possible and currently enabled weather columns
#' some are renamed for clarity
ibm_vars <- c(
  "temperature",
  # "temperature_change24hour",
  # "temperature_max24hour",
  # "temperature_min24hour",
  "dew_point" = "temperature_dew_point",
  # "temperature_feels_like",
  "relative_humidity",
  "precip" = "precip1hour",
  # "precip6hour",
  # "precip24hour",
  # "precip2day",
  # "precip3day",
  # "precip7day",
  # "precip_mtd",
  # "precip_ytd",
  "snow" = "snow1hour",
  # "snow6hour",
  # "snow24hour",
  # "snow2day",
  # "snow3day",
  # "snow7day",
  # "snow_mtd",
  # "snow_season",
  # "snow_ytd",
  # "uv_index",
  # "visibility",
  "wind_speed",
  "wind_gust",
  "wind_direction",
  "pressure_mean_sea_level",
  "pressure_change"
)


#' List of weather variables, unit suffixes, and conversion functions
#' all derivative columns of each of these will start with the same text
#' e.g. temperature => temperature_min => temperature_min_30ma
measures <- tribble(
  ~measure,                  ~metric, ~imperial, ~conversion,
  "temperature",             "°C",   "°F",   c_to_f,
  "dew_point",               "°C",   "°F",   c_to_f,
  "relative_humidity",       "%",    "%",    \(x) x, # no conversion
  "precip",                  "mm",   "in",   mm_to_in,
  "snow",                    "cm",   "in",   cm_to_in,
  "wind_speed",              "kmh",  "mph",  km_to_mi,
  "wind_gust",               "kmh",  "mph",  km_to_mi,
  "wind_direction",          "°",    "°",    \(x) x, # no conversion
  "pressure_mean_sea_level", "mbar", "inHg", mbar_to_inHg,
  "pressure_change",         "mbar", "inHg", mbar_to_inHg,
)


#' Converts all measures from default metric to imperial values
#' operates on any of the major datasets
#' @param df data frame from the hourly set or beyond
#' @returns df with column data converted
convert_measures <- function(df) {
  for (i in 1:nrow(measures)) {
    m <- measures[i,]
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
  for (i in 1:nrow(measures)) {
    m <- measures[i,]
    df <- df %>%
      rename_with(
        .fn = ~paste(.x, m[[unit_system]], sep = "_", recycle0 = TRUE),
        .cols = starts_with(m$measure)
      )
  }
  clean_names(df)
}


#' Creates an appropriately sized grid polygon based on centroid coordinates
#' @param lat latitude of point
#' @param lng longitude of point
#' @param d decimal degree distance from center to edge of grid
#' @returns sf object
ll_to_grid <- function(lat, lon, d = 1/45.5) {
  m <- list(rbind(
    c(lon - d, lat + d),
    c(lon + d, lat + d),
    c(lon + d, lat - d),
    c(lon - d, lat - d),
    c(lon - d, lat + d)
  ))
  st_sfc(st_polygon(m), crs = 4326)
}

# ll_to_grid(45, -89)


#' Creates grid polygons based on weather data grid centroid
#' @param wx IBM weather data eg `saved_weather`
#' @returns sf object of grid cell polygons
build_grids <- function(wx) {
  wx %>%
    distinct(grid_id, grid_lat, grid_lng, time_zone) %>%
    rowwise() %>%
    mutate(geometry = ll_to_grid(grid_lat, grid_lng)) %>%
    ungroup() %>%
    st_set_geometry("geometry")
}

# saved_weather %>% build_grids()


#' Add some more information for displaying on the map
annotate_grids <- function(grids_with_status) {
  grids_with_status %>%
    mutate(
      title = if_else(needs_download, "Weather grid (download required)", "Weather grid"),
      color = if_else(needs_download, "orange", "darkgreen"),
      label = paste0(
        "<b>", title, "</b><br>",
        "Earliest date: ", date_min, "<br>",
        "Latest date: ", date_max, "<br>",
        if_else(date_max == today(), paste0("Most recent data: ", hours_stale, " hours ago<br>"), ""),
        "Total days: ", days_expected, "<br>",
        "Missing days: ", days_missing, sprintf(" (%.1f%%)", 100 * days_missing_pct), "<br>",
        "Missing hours: ", hours_missing, sprintf(" (%.1f%%)", 100 * hours_missing_pct), "<br>"
      ) %>% lapply(HTML)
    )
}

# test_grids <- saved_weather %>% build_grids()
# test_status <- saved_weather %>% weather_status(today() - days(30), today())
# left_join(test_grids, test_status) %>%
#   annotate_grids()


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
      hours_expected = as.integer(difftime(time_max_expected, time_min_expected, units = "hours")),
      hours_actual = n(),
      hours_missing = hours_expected - hours_actual,
      hours_missing_pct = hours_missing / hours_expected,
      hours_stale = as.integer(difftime(time_max_expected, time_max_actual, units = "hours")),
      stale = hours_stale > OPTS$ibm_stale_hours,
      needs_download = stale | ((days_missing > 0) & (hours_missing > 12)),
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
      end_hour = ymd_hms(paste(date, "23:20:00"), tz = tz),
      hours_expected = if_else(
        date == today(),
        as.integer(difftime(now(tzone = tz), start_hour, "hours")),
        as.integer(difftime(end_hour, start_hour, "hours"))
      ) + 1,
      hours_missing = hours_expected - hours
    )
}

# saved_weather %>% filter(grid_id == sample(grid_id, 1)) %>% daily_status()



# Crop and Disease definitions -------------------------------------------------

Disease <- function(name, info, doc) {
  stopifnot(doc %in% list.files(pattern = "*.md", recursive = TRUE))
  list(name = name, info = info, doc = doc)
}

diseases <- list(
  # Corn
  tar_spot = Disease(
    name = "Tar spot",
    info = "Corn is susceptible to tar spot when in the growth stages V10-R3. Risk is based on proability of spore presence.",
    doc = "docs/tar-spot.md"
  ),
  gray_leaf_spot = Disease(
    name = "Gray leaf spot",
    info = "Corn is susceptible to gray leaf spot when in the growth stages V10-R3. Risk is based on probability of spore presence.",
    doc = "docs/gray-leaf-spot.md"
  ),
  don = Disease(
    name = "Giberella ear rot/DON",
    info = "Corn is susceptible to Giberella ear rot during silking. Infection by this disease may lead to deoxynivalenol (DON) accumulation in the ear to dangerous levels. Risk is based on the probability of deoxynivalenol exceeding 1ppm in harvested grain and silage.",
    doc = "docs/don.md"
  ),

  # Soybean
  white_mold = Disease(
    name = "White mold",
    info = "Soybean is vulnerable to white mold when in the growth stages R1-R3 (flowering). Risk is based on probability of spore presence.",
    doc = "docs/white-mold.md"
  ),
  frogeye = Disease(
    name = "Frogeye leaf spot",
    info = "Cornn is vulnerable to tar spot when in the growth stages V10-R3. Risk is based on probability of spore presence.",
    doc = "docs/frogeye.md"
  ),

  # Solanum
  early_blight = Disease(
    name = "Early blight",
    info = "Early blight may affect potato, tomato, pepper, eggplant, and other Solanaceous plants. Risk depends on the number of potato physiological days (P-days) accumulated since crop emergence.",
    doc = "docs/early-blight.md"
  ),
  late_blight = Disease(
    name = "Late blight",
    info = "Late blight may affect potato, tomato, pepper, eggplant, and other Solanaceous plants. Risk depends on the number of disease severity values generated in the last 14 days and since crop emergence.",
    doc = "docs/late-blight.md"
  ),

  # Carrot
  alternaria = Disease(
    name = "Alternaria leaf blight",
    info = "Carrots are susceptible to Alternaria leaf blight. Risk depends on the number of disease severity values generated in the last 7 days.",
    doc = "docs/alternaria.md"
  ),

  # Carrot + Beet
  cercospora = Disease(
    name = "Cercospora leaf spot",
    info = "Carrots and beets are susceptible to Cercospora leaf blight. Risk depends on the average disease severity values in the past 2 days and 7 days.",
    doc = "docs/cercospora.md"
  ),

  # Onion
  botrytis = Disease(
    name = "Botrytis leaf blight",
    info = "Onions are susceptible to Botrytis leaf blight. Risk depends on cumulative disease severity values since crop emergence.",
    doc = "docs/botrytis.md"
  )
)

# set names as $slug
diseases <- imap(diseases, function(disease, slug) {
  disease$slug <- slug
  disease
})


Crop <- function(name, diseases) {
  list(name = name, diseases = diseases)
}

crops <- list(
  corn = Crop(
    name = "Corn",
    diseases = list(
      diseases$tar_spot,
      diseases$gray_leaf_spot,
      diseases$don
    )
  ),
  soybean = Crop(
    name = "Soybean",
    diseases = list(
      diseases$white_mold,
      diseases$frogeye
    )
  ),
  potato = Crop(
    name = "Potato/tomato",
    diseases = list(
      diseases$early_blight,
      diseases$late_blight
    )
  ),
  carrot = Crop(
    name = "Carrot",
    diseases = list(
      diseases$alternaria,
      diseases$cercospora
    )
  ),
  beet = Crop(
    name = "Beet",
    diseases = list(
      diseases$cercospora
    )
  ),
  onion = Crop(
    name = "Onion",
    diseases = list(
      diseases$botrytis
    )
  )
)

# set names as $slug
crops <- imap(crops, function(crop, slug) {
  crop$slug <- slug
  crop
})

OPTS$crop_choices <- build_choices(crops, "name", "slug")



# Field Crops Disease Models ---------------------------------------------------

# Logistic function to convert logit to probability
logistic <- function(logit) exp(logit) / (1 + exp(logit))


#' Tar spot model
#' Corn growth stage V10-R3
#' Risk criteria: High >=35%, Medium >=20%, Low >0%
#' No risk: Fungicide in last 14 days, temperature <32F
#' @param MeanAT_30ma Mean daily temperature, 30-day moving average, Celsius
#' @param MaxRH_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param HrsRH90Night_14ma Nighttime hours RH > 90%, 14-day moving average, 0-24 hours
#' @returns probability of spore presence
predict_tarspot <- function(MeanAT_30ma, MaxRH_30ma, HrsRH90Night_14ma) {
  mu1 <- 32.06987 - 0.89471 * MeanAT_30ma - 0.14373 * MaxRH_30ma
  mu2 <- 20.35950 - 0.91093 * MeanAT_30ma - 0.29240 * HrsRH90Night_14ma
  (logistic(mu1) + logistic(mu2)) / 2
}

# expand_grid(temp = 10:40, rh = seq(0, 100, 5), hours = 0:24) %>%
#   mutate(prob = predict_tarspot(temp, rh, hours)) %>%
#   ggplot(aes(x = temp, y = rh, fill = prob)) +
#   geom_tile() +
#   facet_wrap(~hours) +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)


#' Gray leaf spot model
#' Corn growth stage V10-R3
#' Risk criteria: High >=60%, Medium >=40%, Low >0%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @returns probability of spore presence
#' @param MinAT_21ma Minimum daily temperature, 21-day moving average, Celsius
#' @param MinDP_30ma Minimum dew point temperature, 30-day moving average, Celsius
#' @returns probability of spore presence
predict_gls <- function(MinAT_21ma, MinDP_30ma) {
  mu = -2.9467 - 0.03729 * MinAT_21ma + 0.6534 * MinDP_30ma
  logistic(mu)
}

# expand_grid(temp = 0:30, dp = 0:30) %>%
#   mutate(prob = predict_gls(temp, dp)) %>%
#   ggplot(aes(x = temp, y = dp)) +
#   geom_tile(aes(fill = prob)) +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)


#' Giberella ear rot / DON model
#' Corn silking
#' Risk criteria: High >= 80%, Med >= 40%, Low >= 20%
#' @param
#'
predict_don <- function(
  w7_max_temp_14ma,
  w7_min_temp_14ma,
  w7_days_temp_over_25_14ma,
  w7_days_precip_14ma,
  w0_mean_rh_14ma,
  w0_days_rh_over_80_14ma
) {
  mu =
    -59.6309 +
    1.3057 * w7_max_temp_14ma +
    0.9090 * w7_min_temp_14ma +
    -1.6158 * w7_days_temp_over_25_14ma +
    -0.9350 * w7_days_precip_14ma +
    0.2255 * w0_mean_rh_14ma +
    -0.9249 * w0_days_rh_over_80_14ma
  logistic(mu)
}

# test <- expand_grid(
#   w7_max_temp_14ma = 10:30,
#   w7_min_temp_14ma = 0:30,
#   w7_days_temp_over_25_14ma = 0:14,
#   w7_days_precip_14ma = 0:14,
#   w0_mean_rh_14ma = 10 * (0:10),
#   w0_days_rh_over_80_14ma = 0:14
# ) %>%
#   filter(w7_max_temp_14ma > w7_min_temp_14ma) %>%
#   mutate(don = predict_don(
#     w7_max_temp_14ma,
#     w7_min_temp_14ma,
#     w7_days_temp_over_25_14ma,
#     w7_days_precip_14ma,
#     w0_mean_rh_14ma,
#     w0_days_rh_over_80_14ma
#   ))
#
# hist(test$don)
#
# test %>%
#   filter(don > .01) %>%
#   pivot_longer(1:6) %>%
#   select(name, value, don) %>%
#   mutate(value = factor(value)) %>%
#   ggplot(aes(x = value, y = don)) +
#   geom_boxplot(aes(fill = after_stat(middle)), outlier.size = .5) +
#   scale_fill_viridis_c() +
#   facet_wrap(~name, scales = "free")



#' White mold, dryland model
#' Soybean growth stage R1-R3
#' Risk criteria: High >=40%, Med >=20%, Low >=5%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @param MaxAT_30ma 30-day moving average of daily maximum temperature, Celsius
#' @param MaxWS_30ma 30-day moving average of daily maximum wind speed, m/s
#' @param MaxRH_30ma 30-day moving average of daily maximum relative humidity, 0-100%
#' @returns probability of spore presence
predict_white_mold_dry <- function(MaxAT_30ma, MaxWS_30ma, MaxRH_30ma) {
  m1 <- -.47 * MaxAT_30ma - 1.01 * MaxWS_30ma + 16.65
  m2 <- -.68 * MaxAT_30ma + 17.19
  m3 <- -.86 * MaxAT_30ma + 0.1 * MaxRH_30ma - 0.75 * MaxWS_30ma + 8.2
  (logistic(m1) + logistic(m2) + logistic(m3)) / 3
}

# expand_grid(temp = 0:40, wind = 0:20, rh = (0:10) * 10) %>%
#   mutate(prob = predict_white_mold_dry(temp, wind, rh)) %>%
#   ggplot(aes(x = temp, y = wind, fill = prob)) +
#   geom_tile() +
#   scale_fill_distiller(palette = "Spectral", limits = c(0, 1)) +
#   coord_cartesian(expand = F) +
#   facet_wrap(~rh, labeller = "label_both")


#' White mold, irrigated model
#' Soybean growth stage R1-R3
#' Risk criteria: High >=40%, Med >=20%, Low >=5%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @param MaxAT_30MA Maximum daily temperature, 30-day moving average, Celsius
#' @param MaxRH_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param spacing Row spacing, either "15" or "30", inches
#' @returns probability of spore presence
predict_white_mold_irrig <- function(MaxAT_30MA, MaxRH_30ma, spacing = c("15", "30")) {
  spacing <- match.arg(spacing)
  mu <- -2.38 * (spacing == "30") + 0.65 * MaxAT_30MA + 0.38 * MaxRH_30ma - 52.65
  logistic(mu)
}

# expand_grid(temp = 15:40, rh = seq(50, 100, 5), spacing = c("15", "30")) %>%
#   rowwise() %>%
#   mutate(prob = predict_white_mold_irrig(temp, rh, spacing)) %>%
#   ggplot(aes(x = temp, y = rh, fill = prob)) +
#   geom_tile() +
#   facet_wrap(~spacing, ncol = 1) +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)


#' Frogeye leaf spot model
#' Soybean growth stage R1-R5
#' Risk criteria: High >=50%, Medium >=40%, Low >0%
#' No risk: Fungicide in last 14 days, temperature <32F
#' @param MaxAT_30ma Maximum daily temperature, 30-day moving average, Celsius
#' @param HrsRH80_30ma Daily hours RH > 80%, 30-day moving average, 0-24 hours
#' @returns probability of spore presence
predict_fls <- function(MaxAT_30ma, HrsRH80_30ma) {
  mu <- -5.92485 + 0.12208 * MaxAT_30ma + 0.17326 * HrsRH80_30ma
  logistic(mu)
}

# expand_grid(temp = 0:40, hours = 0:24) %>%
#   mutate(prob = predict_fls(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = prob)) +
#   geom_tile() +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)






# Vegetable Disease Models -----------------------------------------------------

#' Potato physiological days
#' Potato/tomato
#' Pscheidt and Stevenson 1988 https://link.springer.com/article/10.1007/BF02854357
#' More information: https://vegpath.plantpath.wisc.edu/diseases/potato-early-blight/
#' @param tmin Minimum daily temperature, Celsius
#' @param tmax Maximum daily temperature, Celsius
#' @returns numeric daily potato physiological days, approx 0-10 per day
calc_pdays <- function(tmin, tmax) {
  a = 5 * pday(tmin)
  b = 8 * pday((2 * tmin / 3) + (tmax / 3))
  c = 8 * pday((2 * tmax / 3) + (tmin / 3))
  d = 3 * pday(tmin)
  (a + b + c + d) / 24.0
}


#' P-day function for an individual temperature
#' @param temp temperature in Celsius
#' @returns numeric p-day value
pday <- function(temp) {
  case_when(
    temp < 7 ~ 0,
    between(temp, 7, 21) ~ 10 * (1 - ((temp - 21)^2 / 196)), # 196 = (21-7)^2
    between(temp, 21, 30) ~ 10 * (1 - ((temp - 21)**2 / 81)), # 81 = (30-21)^2
    T ~ 0
  )
}

# expand_grid(tmin = 0:35, tmax = 0:35) %>%
#   mutate(pdays = calculate_pdays(tmin, tmax)) %>%
#   ggplot(aes(x = tmin, y = tmax, fill = pdays)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Late blight disease severity values
#' Potato/tomato
#' based on the Wallins BLITECAST model
#' - https://www.google.com/books/edition/The_Plant_Disease_Reporter/ow9BD6P2KZ4C?hl=en&gbpv=1&pg=PA95&printsec=frontcover
#' - https://ipm.ucanr.edu/DISEASE/DATABASE/potatolateblight.html
#' More information: https://vegpath.plantpath.wisc.edu/diseases/potato-late-blight/
#' @param t Mean temperature during hours where RH > 90%, Celsius
#' @param h Number of hours where RH > 90%
#' @returns numeric 0-4 disease severity values
calc_late_blight_dsv <- function(t, h) {
  case_when(
    is.na(t) | is.na(h) ~ 0,
    t <   7.2 ~ 0,
    t <= 11.6 ~ (h > 21) + (h > 18) + (h > 15),
    t <= 15.0 ~ (h > 21) + (h > 18) + (h > 15) + (h > 12),
    t <= 26.6 ~ (h > 18) + (h > 15) + (h > 12) + (h > 9),
    TRUE ~ 0
  )
}

# expand_grid(temp = 0:30, hours = 0:24) %>%
#   mutate(dsv = late_blight_dsv(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dsv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Alternaria leaf blight disease severity values
#' Carrot
#' based on the Pitblado 1992 TOMCAST model https://atrium.lib.uoguelph.ca/server/api/core/bitstreams/5c7ec712-43ef-4b73-b8b8-e85c29d3d58b/content
#' which is based on the Madden et al 1978 FAST model https://www.apsnet.org/publications/phytopathology/backissues/Documents/1978Articles/Phyto68n09_1354.PDF
#'
#' @param temp Mean temperature during hours where RH > 90%, Celsius
#' @param h Number of hours where RH > 90%
#' @returns numeric 0-4 dsv
calc_alternaria_dsv <- function(temp, h) {
  case_when(
    is.na(temp) | is.na(h) ~ 0,
    temp <  13 ~ 0,
    temp <= 18 ~ (h > 20) + (h > 15) + (h > 7),
    temp <= 21 ~ (h > 22) + (h > 15) + (h > 8) + (h > 4),
    temp <= 26 ~ (h > 20) + (h > 12) + (h > 5) + (h > 2),
    temp >  26 ~ (h > 22) + (h > 15) + (h > 8) + (h > 3)
  )
}

# expand_grid(temp = 0:30, hours = 0:24) %>%
#   mutate(dsv = carrot_foliar_dsv(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dsv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Cercospora leaf blight daily infection values
#' Beet
#' based on https://apsjournals.apsnet.org/doi/abs/10.1094/PDIS.1998.82.7.716
#' more information: https://vegpath.plantpath.wisc.edu/diseases/carrot-alternaria-and-cercospora-leaf-blights/
#' @param t Mean temperature during hours where RH > 90%, Celsius, converted to Fahrenheit internally
#' @param h Number of hours where RH > 90%
#' @returns 0-7 div
calc_cercospora_div <- function(t, h) {
  t <- c_to_f(t)
  case_when(
    is.na(t) | is.na(h) ~ 0,
    t <= 60 ~ 0,
    t <= 61 ~ (h > 21),
    t <= 62 ~ (h > 19) + (h > 22),
    t <= 63 ~ (h > 16) + (h > 19) + (h > 21),
    t <= 64 ~ (h > 13) + (h > 15) + (h > 18) + (h > 20) + (h > 23),
    t <= 65 ~ (h > 6) + (h > 8) + (h > 12) + (h > 18) + (h > 21),
    t <= 71 ~ (h > 3) + (h > 6) + (h > 10) + (h > 14) + (h > 18) + (h > 21),
    t <= 72 ~ (h > 2) + (h > 6) + (h > 9) + (h > 13) + (h > 17) + (h > 20),
    t <= 73 ~ (h > 1) + (h > 6) + (h > 9) + (h > 12) + (h > 16) + (h > 19),
    t <= 76 ~ 1 + (h > 5) + (h > 9) + (h > 11) + (h > 16) + (h > 18) + (h > 23),
    t <= 77 ~ 1 + (h > 5) + (h > 8) + (h > 12) + (h > 15) + (h > 18) + (h > 22),
    t <= 78 ~ 1 + (h > 5) + (h > 8) + (h > 11) + (h > 14) + (h > 17) + (h > 20),
    t <= 79 ~ 1 + (h > 4) + (h > 7) + (h > 9) + (h > 12) + (h > 14) + (h > 17),
    t <= 80 ~ 1 + (h > 3) + (h > 6) + (h > 8) + (h > 10) + (h > 12) + (h > 15),
    t <= 81 ~ 1 + (h > 2) + (h > 4) + (h > 6) + (h > 7) + (h > 9) + (h > 11),
    t <= 82 ~ 1 + (h > 2) + (h > 4) + (h > 5) + (h > 7) + (h > 8) + (h > 10),
    t >  82 ~ 1 + (h > 2) + (h > 4) + (h > 5) + (h > 7) + (h > 8) + (h > 9)
  )
}

# expand_grid(temp = 15:30, hours = 0:24) %>%
#   mutate(dsv = cercospora_div(temp, hours), temp = c_to_f(temp)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dsv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Onion botrytis leaf blight
#' based on Sutton et al 1986 https://doi.org/10.1016/0167-8809(86)90136-2
#' more information: https://vegpath.plantpath.wisc.edu/diseases/onion-botrytis/
#' requires 5 days of hourly weather to compute

#' Botcast daily innoculum value
#' this determines if innoculum is predicted to be present
#' A) if temp > 30 for >= 4 hrs on any of past 5 days => 0
#' B) if h < 5 => 0
#' C) if h > 12 => 1
#' D) if prev day dry (<70% rh for >= 6 hrs) and no rain => 0 else 1
#' @param hot boolean: temp > 30 for >= 4 hrs on any of past 5 days
#' @param h int: hours rh > 90 during past day
#' @param dry boolean: previous day dry
botcast_dinov <- function(hot, hours_rh90, dry) {
  case_when(
    hot ~ 0,
    hours_rh90 < 5 ~ 0,
    hours_rh90 > 12 ~ 1,
    dry ~ 0,
    TRUE ~ 1
  )
}

# expand_grid(hot = c(F, T), lw = 0:24, dry = c(F, T)) %>%
#   mutate(dinov = botcast_dinov(hot, lw, dry)) %>% summary()


#' Botcast daily infection values
#' @param t temperature of wet period
#' @param h duration of leaf wetness
botcast_dinfv <- function(t, h) {
  case_when(
    h <= 6 ~ 0,
    t < 6 | t > 28 ~ 0,
    h <= 12 & t < 9 ~ 0,
    t <= 8 ~ case_when(
      h <= 12 ~ 0,
      h <= 22 ~ 1,
      TRUE ~ 2
    ),
    h <= 15 & t >= 27 ~ 0,
    h <= 7 & t >= 24 ~ 0,
    h >= 15 & between(t, 8, 25) ~ 2,
    h >= 14 & between(t, 11, 16.5) ~ 2,
    h >= 11 & between(t, 13.5, 16.5) ~ 2,
    h - 12 <= 9 - t ~ 0,
    TRUE ~ 1
  )
}

# expand_grid(temp = (60:280) / 10, hours = 0:24) %>%
#   mutate(dinfv = botcast_dinfv(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dinfv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_equal(expand = F)


calc_botrytis_dsi <- function(hot, dry, hours_rh90, mean_temp_rh90) {
  dinov = botcast_dinov(hot, hours_rh90, lag(dry, default = FALSE))
  dinfv = botcast_dinfv(mean_temp_rh90, hours_rh90)
  dinov * dinfv
}

# expand_grid(
#   hot = c(T, F),
#   dry = c(T, F),
#   hours_rh90 = 0:24,
#   mean_temp_rh90 = 0:35
# ) %>%
#   mutate(value = calc_botrytis_dsi(hot, dry, hours_rh90, mean_temp_rh90)) %>%
#   count(value)



# Disease severity --------------------------------------------------------
# Functions for converting model outputs into risk scores (eg low/med/high)

assign_risk <- function(model, value) {
  switch(model,
    "tar_spot_prob"          = risk_from_prob(value, 1, 20, 35),
    "gray_leaf_spot_prob"    = risk_from_prob(value, 1, 40, 60),
    "don_prob"               = risk_from_prob(value, 20, 40, 80),
    "white_mold_dry_prob"    = risk_from_prob(value, .01, 20, 35),
    "white_mold_irrig_prob"  = risk_from_prob(value, .01, 5, 10),
    "frogeye_leaf_spot_prob" = risk_from_prob(value, 1, 40, 50),
    "early_blight_pdays"     = risk_for_early_blight(value),
    "late_blight_dsv"        = risk_for_late_blight(value),
    "alternaria_dsv"         = risk_for_alternaria(value),
    "cercospora_div"         = risk_for_cercospora(value),
    "botrytis_dsi"           = risk_for_botrytis(value),
    stop("'", model, "' is not a valid model type")
  )
}

# assign_risk("foo", 1)


## Field crops ----

#' Assign risk for field crops spore probability models
#' @param prob numeric vector of probabilities between 0 and 1
#' @param low threshold for low risk, 0-100
#' @param med threshold for medium risk, 0-100
#' @param high threhold for high risk, 0-100
risk_from_prob <- function(prob, low, med, high) {
  tibble(
    risk = cut(
      prob * 100,
      breaks = c(0, low, med, high, 100),
      labels = c("Very low risk", "Low risk", "Moderate risk", "High risk"),
      include.lowest = TRUE,
      right = FALSE
    ),
    # severity = as.numeric(risk) - 1,
    risk_color = colorFactor("Spectral", risk, reverse = TRUE)(risk),
    value_label = sprintf("%.0f%% (%s)", prob * 100, risk)
  )
}

# tibble(
#   value = runif(20),
#   risk_from_prob(value, 5, 25, 60)
# )


# reduces spore probability as temperature falls below 10C
attenuate_prob <- function(value, temp) {
  case_when(
    temp > 10 ~ value,
    temp > 0 ~ value * temp / 10,
    TRUE ~ 0
  )
}

# tibble(value = c(10:1, 2:10), temp = c(1:10, 9:1) * 3) %>%
#   mutate(new_value = attenuate_prob(value, temp))



## Vegetables ----

#' Convert severity score 0-4 to a risk word and assign a color
#' @param severity numeric vector of severities
#' @returns tibble
risk_from_severity <- function(severity) {
  # pal <- c("#00cc00", "#7dff23", "#ffd700", "#ff8000", "#cc0000")
  pal <- c("#0082b7", "#00cc00", "#ffd700", "#ff8000", "#cc0000")
  tibble(
    risk = cut(
      severity,
      breaks = 0:5,
      labels = c("Very low", "Low", "Moderate", "High", "Very high"),
      include.lowest = TRUE,
      right = FALSE
    ),
    risk_color = colorFactor(pal, risk)(risk)
  )
}

# tibble(
#   value = round(runif(10, 0, 4)),
#   risk_from_severity(value)
# )


#' Assign risk score for early blight p-day accumulation
#' @param value dsv from `calc_pdays` function
risk_for_early_blight <- function(value) {
  tibble(
    total = cumsum(value),
    avg7 = rollapplyr(value, 7, mean, partial = TRUE),
    severity = case_when(
      total >= 400 ~
        (avg7 >= 1) +
        (avg7 >= 3) +
        (avg7 >= 5) +
        (avg7 >= 9),
      TRUE ~
        (total >= 200) +
        (total >= 250) +
        (total >= 300) +
        (total >= 350)
    ),
    risk_from_severity(severity),
    value_label = sprintf("%.1f P-days, 7-day avg: %.1f, Total: %.0f (%s risk)", value, avg7, total, risk)
  )
}

# tibble(
#   value = runif(100, 5, 10),
#   risk_for_earlyblight(value),
# ) %>%
#   mutate(day = row_number()) %>%
#   ggplot(aes(x = day, color = risk, group = 1)) +
#   geom_line(aes(y = total)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)


#' Assign risk score for late blight dsv accumulation
#' @param value dsv from `calc_late_blight_dsv` function
risk_for_late_blight <- function(value) {
  tibble(
    total14 = rollapplyr(value, 14, sum, partial = TRUE),
    total = cumsum(value),
    severity = case_when(
      total14 >= 21 & total >= 30 ~ 4,
      total14 >= 14 & total >= 30 ~ 3,
      total14 >= 3 | total >= 30 ~ 2,
      total14 >= 1 ~ 1,
      TRUE ~ 0
    ),
    risk_from_severity(severity),
    value_label = sprintf("%s DSV, 14-day: %s, Total: %s (%s risk)", value, total14, total, risk)
  )
}

# tibble(
#   value = runif(100, 0, 3) ,
#   risk_for_lateblight(value)
# ) %>%
#   mutate(day = row_number()) %>%
#   ggplot(aes(x = day, group = 1, color = risk)) +
#   geom_line(aes(y = total14)) +
#   geom_line(aes(y = total)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)


#' Assign risk score for alternaria leaf blight
#' @param value dsv from `calc_alternaria_dsv` function
risk_for_alternaria <- function(value) {
  tibble(
    total7 = rollapplyr(value, 7, sum, partial = TRUE),
    total = cumsum(value),
    severity =
      (total7 >= 5) +
      (total7 >= 10) +
      (total7 >= 15) +
      (total7 >= 20),
    risk_from_severity(severity),
    value_label = sprintf("%s DSV, 7-day: %s, Total: %s (%s risk)", value, total7, total, risk)
  )
}

# tibble(
#   value = runif(100, 0, 5),
#   risk_for_alternaria(value)
# ) %>%
#   mutate(day = row_number()) %>%
#   ggplot(aes(x = day, color = risk, group = 1)) +
#   geom_line(aes(y = total7)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)


#' Cercospora leaf blight
#' Windels, C. E., Lamey, H. A., Hilde, D., Widner, J., & Knudsen, T. (1998). A Cerospora leaf spot model for sugar beet: in practice by an industry. Plant disease, 82(7), 716-726.
#' https://apsjournals.apsnet.org/doi/abs/10.1094/PDIS.1998.82.7.716
#' @param value dsv from `calc_cercospora_div` function
risk_for_cercospora <- function(value) {
  tibble(
    avg2 = rollapplyr(value, 2, mean, partial = TRUE),
    avg7 = rollapplyr(value, 7, mean, partial = TRUE),
    total = cumsum(value),
    severity = case_when(
      avg7 >= 5 | avg2 >= 5.5 ~ 4,
      avg7 >= 3 | avg2 >= 3.5 ~ 3,
      avg7 >= 1.5 | avg2 >= 2 ~ 2,
      avg7 >= .5 | avg2 >= 1 ~ 1,
      TRUE ~ 0
    ),
    risk_from_severity(severity),
    value_label = sprintf("%s DIV, 2-day avg: %.1f, 7-day avg: %.1f, Total: %s (%s risk)", value, avg2, avg7, total, risk),
  )
}

# tibble(
#   value = runif(100, 0, 4),
#   risk_for_cercospora(value)
# ) %>%
#   mutate(day = row_number()) %>%
#   ggplot(aes(x = day, color = risk, group = 1)) +
#   geom_line(aes(y = avg2)) +
#   geom_line(aes(y = avg7)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)



# Botrytis (botcast) - onion
risk_for_botrytis <- function(value) {
  tibble(
    total = cumsum(value),
    severity =
      (total > 10) +
      (total > 20) +
      (total > 30) +
      (total > 40),
    risk_from_severity(severity),
    value_label = sprintf("%.0f daily, %.0f total DSI (%s risk)", value, total, risk)
  )
}

# tibble(
#   value = round(runif(100, 0, 4), 0),
#   risk_for_botrytis(value)
# ) %>%
#   mutate(day = row_number()) %>%
#   ggplot(aes(x = day, color = risk, group = 1)) +
#   geom_col(aes(y = value)) +
#   geom_line(aes(y = total)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)



# Growing degree days ----------------------------------------------------------

#' Single sine method
#' to create GDDs with an upper threshold, calculate GDDs with the upper threshold
#' as the base temperature and subtract that value from the GDDs for the base temp
#' to implement a horizontal cutoff.
#' @param tmin minimum daily temperature
#' @param tmax maximum daily temperature
#' @param base base/lower temperature threshold
#' @returns single sine growing degree days for one day
gdd_sine <- function(tmin, tmax, base) {
  mapply(function(tmin, tmax, base) {
    if (is.na(tmin) || is.na(tmax)) return(NA)

    # swap min and max if in wrong order for some reason
    if (tmin > tmax) { t = tmin; tmin = tmax; tmax = t }

    # min and max < lower
    if (tmax <= base) return(0)

    average = (tmin + tmax) / 2

    # tmin > lower = simple average gdds
    if (tmin >= base) return(average - base)

    # tmin < lower, tmax > lower = sine gdds
    alpha = (tmax - tmin) / 2
    base_radians = asin((base - average) / alpha)
    a = average - base
    b = pi / 2 - base_radians
    c = alpha * cos(base_radians)
    (1 / pi) * (a * b + c)
  }, tmin, tmax, base)
}

# expand_grid(tmin = 0:30, tmax = 0:30) %>%
#   filter(tmax >= tmin) %>%
#   mutate(gdd = gdd_sine(tmin, tmax, 10)) %>%
#   ggplot(aes(x = tmin, y = tmax, fill = gdd)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)



# Data pipeline ----------------------------------------------------------------

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


#' Creates the working hourly weather dataset from cleaned ibm response
#' @param ibm_hourly hourly weather data from `clean_ibm` function
#' @returns tibble
build_hourly <- function(ibm_hourly) {
  ibm_hourly %>%
    select(
      grid_id, grid_lat, grid_lng,
      datetime_utc, time_zone, datetime_local, date,
      all_of(ibm_vars)
    ) %>%
    add_date_cols() %>%
    arrange(grid_lat, grid_lng, datetime_local) %>%
    mutate(
      dew_point_depression = abs(temperature - dew_point),
      .after = dew_point
    )
}

# saved_weather %>% build_hourly()



#' Generate daily summary data from hourly weather
#' @param hourly accepts the cleaned hourly data from `build_hourly()`
#' @returns tibble
build_daily <- function(hourly) {
  # grid attributes to be retained
  lat_lng <- hourly %>%
    distinct(grid_id, grid_lat, grid_lng)

  # summarized by calendar date
  summary_fns <- c("min" = calc_min, "mean" = calc_mean, "max" = calc_max)
  by_date <- hourly %>%
    summarize(
      hours = n(),
      across(c(temperature, dew_point, dew_point_depression, relative_humidity), summary_fns),
      across(c(precip, snow), c("daily" = calc_sum, "max_hourly" = calc_max)),
      across(c(pressure_mean_sea_level, wind_speed), summary_fns),
      wind_gust_max = calc_max(wind_gust),
      across(c(wind_direction), summary_fns),
      hours_temp_over_20 = sum(temperature >= 20),
      hours_temp_over_30 = sum(temperature >= 30),
      hours_rh_under_70 = sum(relative_humidity < 70),
      hours_rh_over_80 = sum(relative_humidity >= 80),
      hours_rh_over_90 = sum(relative_humidity >= 90),
      .by = c(grid_id, date, yday, year, month, day)
    ) %>%
    mutate(precip_cumulative = cumsum(precip_daily), .after = precip_max_hourly, .by = grid_id ) %>%
    mutate(snow_cumulative = cumsum(snow_daily), .after = snow_max_hourly, .by = grid_id) %>%
    mutate(
      # for botcast
      hot_past_5_days = rollapplyr(hours_temp_over_30, width = 5, FUN = \(x) any(x >= 4), partial = TRUE),
      dry = (hours_rh_under_70 >= 6) & (precip_daily < 1)
    )

  # summarized by "date since night" eg since 8 pm the day before through 7 pm
  by_night <- hourly %>%
    mutate(
      rh80 = relative_humidity >= 80,
      rh90 = relative_humidity >= 90
    ) %>%
    summarize(
      hours_rh_over_80_night = sum(night & rh80),
      hours_rh_over_90_night = sum(night & rh90),
      temperature_mean_rh_over_80 = if_else(sum(rh80) > 0, sum(temperature * (rh80)) / sum(rh80), NA),
      temperature_mean_rh_over_90 = if_else(sum(rh90) > 0, sum(temperature * (rh90)) / sum(rh90), NA),
      .by = c(grid_id, date_since_night)
    )

  # assemble the data
  by_date %>%
    filter((hours >= 12) | (date == today())) %>%
    left_join(by_night, join_by(grid_id, date == date_since_night)) %>%
    left_join(lat_lng, join_by(grid_id)) %>%
    relocate(grid_lat, grid_lng, .after = grid_id) %>%
    arrange(grid_id, date)
}

# saved_weather %>%
#   filter(grid_id == sample(grid_id, 1)) %>%
#   build_hourly() %>%
#   build_daily() %>%
#   pivot_longer(-c(grid_id:day)) %>%
#   ggplot(aes(x = date, y = value, color = name)) +
#   geom_line(show.legend = F) +
#   facet_wrap(~name, scales = "free")



#' Generate several moving average periods from daily data
#' @param daily accepts daily data from `build_daily()`
#' @param align moving average alignment
#' @returns tibble
build_ma_from_daily <- function(daily, align = c("center", "right")) {
  align <- match.arg(align)

  # retain attribute cols
  attr <- daily %>% select(grid_id, any_of(OPTS$date_attr_cols))

  # define moving average functions
  roll_mean <- function(vec, width) rollapply(vec, width, \(x) calc_mean(x), partial = TRUE, align = align)
  fns <- c(
    "7day" = ~roll_mean(.x, 7),
    "14day" = ~roll_mean(.x, 14),
    "21day" = ~roll_mean(.x, 21),
    "30day" = ~roll_mean(.x, 30)
  )

  # apply moving average functions to each primary data column
  ma <- daily %>%
    select(-hours) %>%
    mutate(
      across(starts_with(c("temperature", "dew_point", "relative_humidity", "wind", "pressure", "hours")), fns),
      .by = grid_id,
      .keep = "none"
    ) %>%
    select(-grid_id)

  # bind attributes
  bind_cols(attr, ma)
}

# test <- saved_weather %>% build_daily() %>% filter(grid_id == sample(grid_id, 1)) %>% build_ma_from_daily()
# test <- saved_weather %>% build_daily() %>% build_ma_from_daily()
# ggplot(test, aes(x = date, color = grid_id)) +
#   geom_line(aes(y = dew_point_min_7day))



#' Plant diseases that use daily values as inputs
#' units must be metric: temperature degC, wind speed km/hr
#' @param daily accepts daily dataset from `build_daily()`
#' @returns tibble
# build_disease_from_daily <- function(daily) {
#   # retain attribute cols
#   attr <- daily %>% select(grid_id, date)
#
#   # generate disease models and add cumulative sums where appropriate
#   disease <- daily %>%
#     mutate(
#       potato_pdays = calc_pdays(temperature_min, temperature_max),
#       late_blight_dsv = calc_late_blight_dsv(temperature_mean_rh_over_90, hours_rh_over_90),
#       alternaria_dsv = calc_alternaria_dsv(temperature_mean_rh_over_90, hours_rh_over_90),
#       cercospora_div = calc_cercospora_div(temperature_mean_rh_over_90, hours_rh_over_90),
#       botrytis_dsi = calc_botrytis_dsi(hot_past_5_days, dry, hours_rh_over_90, temperature_mean_rh_over_90),
#       .by = grid_id,
#       .keep = "none"
#     ) %>%
#     # mutate(across(everything(), c(cumulative = cumsum)), .by = grid_id) %>%
#     # select(sort(names(.))) %>%
#     select(-grid_id)
#
#   # bind attributes
#   bind_cols(attr, disease)
# }

# saved_weather %>% build_hourly() %>% build_daily() %>% build_disease_from_daily()



#' Plant diseases that use moving averages as inputs
#' units must be metric: temperature degC, wind speed km/hr
#' @param daily accepts daily dataset from `build_daily()` but should start 30 days before first disease estimate is desired
#' @returns tibble
# build_disease_from_ma <- function(daily) {
#
#   # retain attribute cols
#   attr <- daily %>% select(grid_id, date)
#
#   # generate disease models and add cumulative sums where appropriate
#   disease <- daily %>%
#     mutate(
#       temperature_max_30day = roll_mean(temperature_max, 30),
#       temperature_mean_30day = roll_mean(temperature_mean, 30),
#       temperature_min_21day = roll_mean(temperature_min, 21),
#       wind_speed_max_30day = roll_mean(wind_speed_max, 30),
#       relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
#       dew_point_min_30day = roll_mean(dew_point_min, 30),
#       hours_rh_over_80_30day = roll_mean(hours_rh_over_80, 30),
#       hours_rh_over_90_night_14day = roll_mean(hours_rh_over_90_night, 14),
#       .by = grid_id
#     ) %>%
#     mutate(
#       white_mold_dry_prob = predict_whitemold_dry(temperature_max_30day, kmh_to_mps(wind_speed_max_30day), relative_humidity_max_30day) %>%
#         attenuate_prob(temperature_min_21day),
#       white_mold_irrig_30_prob =
#         predict_whitemold_irrig(temperature_max_30day, relative_humidity_max_30day, "30"),
#       white_mold_irrig_15_prob =
#         predict_whitemold_irrig(temperature_max_30day, relative_humidity_max_30day, "15"),
#       gray_leaf_spot_prob =
#         predict_gls(temperature_min_21day, dew_point_min_30day),
#       tar_spot_prob =
#         predict_tarspot(temperature_mean_30day, relative_humidity_max_30day, hours_rh_over_90_night_14day) %>%
#         attenuate_prob(temperature_min_21day),
#       frogeye_leaf_spot_prob =
#         predict_fls(temperature_max_30day, hours_rh_over_80_30day),
#       .by = grid_id,
#       .keep = "none"
#     ) %>%
#     select(sort(names(.))) %>%
#     select(-grid_id)
#
#   # bind attributes
#   bind_cols(attr, disease)
# }

# saved_weather %>% filter(grid_id == sample(grid_id, 1)) %>% build_daily() %>% build_disease_from_ma()


test_plot <- function(df) {
  df %>%
    ggplot(aes(x = date, y = model_value)) +
    geom_col(aes(fill = risk_color), lwd = 0, width = 1) +
    geom_line(aes(group = grid_id)) +
    scale_fill_identity() +
    facet_wrap(~grid_id)
}

test_plot_severity <- function(df) {
  df %>%
    ggplot(aes(x = date, y = severity)) +
    geom_col(aes(fill = risk_color), lwd = 0, width = 1) +
    geom_line(aes(group = grid_id)) +
    scale_fill_identity() +
    facet_wrap(~grid_id)
}


build_tar_spot <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      temperature_mean_30day = roll_mean(temperature_mean, 30),
      temperature_min_21day = roll_mean(temperature_min, 21),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      hours_rh_over_90_night_14day = roll_mean(hours_rh_over_90_night, 14),
      model_value = predict_tarspot(
        temperature_mean_30day,
        relative_humidity_max_30day,
        hours_rh_over_90_night_14day
      ) %>% attenuate_prob(temperature_min_21day),
      assign_risk("tar_spot_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>% select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_tar_spot() %>% test_plot()


build_gray_leaf_spot <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      temperature_min_21day = roll_mean(temperature_min, 21),
      dew_point_min_30day = roll_mean(dew_point_min, 30),
      model_value = predict_gls(temperature_min_21day, dew_point_min_30day),
      assign_risk("gray_leaf_spot_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>% select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_gray_leaf_spot() %>%


build_don <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    replace_na(list(precip_daily = 0)) %>%
    mutate(
      temp_max_14day = roll_mean(temperature_max, 14),
      temp_min_14day = roll_mean(temperature_min, 14),
      days_temp_over_25_14day = roll_sum(temperature_mean >= 25, 14),
      days_precip_14day = roll_sum(precip_daily > 0, 14),
      rh_mean_14day = roll_mean(relative_humidity_mean, 14),
      days_rh_over_80_14day = roll_sum(relative_humidity_mean > 80, 14),
      model_value = predict_don(
        lag(temp_max_14day, 7),
        lag(temp_min_14day, 7),
        lag(days_temp_over_25_14day, 7),
        lag(days_precip_14day, 7),
        rh_mean_14day,
        days_rh_over_80_14day
      ),
      assign_risk("don_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>% select(-grid_id)
  bind_cols(attr, disease) %>%
    drop_na(model_value)
}

# saved_weather %>% build_daily() %>% build_don() %>% test_plot()


build_white_mold_dry <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      temperature_max_30day = roll_mean(temperature_max, 30),
      temperature_min_21day = roll_mean(temperature_min, 21),
      wind_speed_max_30day = roll_mean(wind_speed_max, 30),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      model_value = predict_white_mold_dry(
        temperature_max_30day,
        kmh_to_mps(wind_speed_max_30day),
        relative_humidity_max_30day
      ) %>% attenuate_prob(temperature_min_21day),
      assign_risk("white_mold_dry_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_white_mold_dry() %>% test_plot()


build_white_mold_irrig <- function(daily, spacing = c("30", "15")) {
  spacing <- match.arg(spacing)
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      temperature_max_30day = roll_mean(temperature_max, 30),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      model_value = predict_white_mold_irrig(
        temperature_max_30day,
        relative_humidity_max_30day,
        spacing
      ),
      assign_risk("white_mold_irrig_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_white_mold_irrig("30") %>% test_plot()
# saved_weather %>% build_daily() %>% build_white_mold_irrig("15") %>% test_plot()


build_frogeye_leaf_spot <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      temperature_max_30day = roll_mean(temperature_max, 30),
      temperature_mean_30day = roll_mean(temperature_mean, 30),
      temperature_min_21day = roll_mean(temperature_min, 21),
      wind_speed_max_30day = roll_mean(wind_speed_max, 30),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      dew_point_min_30day = roll_mean(dew_point_min, 30),
      hours_rh_over_80_30day = roll_mean(hours_rh_over_80, 30),
      hours_rh_over_90_night_14day = roll_mean(hours_rh_over_90_night, 14),
      model_value = predict_fls(temperature_max_30day, hours_rh_over_80_30day),
      assign_risk("frogeye_leaf_spot_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_frogeye_leaf_spot() %>% test_plot()


build_early_blight <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      model_value = calc_pdays(
        temperature_min,
        temperature_max
      ),
      assign_risk("early_blight_pdays", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_early_blight() %>% test_plot()
# saved_weather %>% build_daily() %>% build_early_blight() %>% test_plot_severity()


build_late_blight <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      model_value = calc_late_blight_dsv(
        temperature_mean_rh_over_90,
        hours_rh_over_90
      ),
      assign_risk("late_blight_dsv", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_late_blight() %>% test_plot()
# saved_weather %>% build_daily() %>% build_late_blight() %>% test_plot_severity()


build_alternaria <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      model_value = calc_alternaria_dsv(
        temperature_mean_rh_over_90,
        hours_rh_over_90
      ),
      assign_risk("alternaria_dsv", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_alternaria() %>% test_plot()
# saved_weather %>% build_daily() %>% build_alternaria() %>% test_plot_severity()


build_cercospora <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      model_value = calc_cercospora_div(temperature_mean_rh_over_90, hours_rh_over_90),
      assign_risk("cercospora_div", model_value),
      .by = grid_id, .keep = "used",
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_cercospora() %>% test_plot()
# saved_weather %>% build_daily() %>% build_cercospora() %>% test_plot_severity()


build_botrytis <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      model_value = calc_botrytis_dsi(hot_past_5_days, dry, hours_rh_over_90, temperature_mean_rh_over_90),
      assign_risk("botrytis_dsi", model_value),
      .by = grid_id, .keep = "used",
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_botrytis() %>% test_plot()
# saved_weather %>% build_daily() %>% build_botrytis() %>% test_plot_severity()




#' Generate various growing degree day models with and without an 86F upper threshold
#' input temperatures must be Celsius and will be converted to Fahrenheit GDDs
#' @param daily accepts daily dataset from `build_daily()`
#' @returns tibble
build_gdd_from_daily <- function(daily) {
  # retain attribute cols
  attr <- daily %>% select(grid_id, date)

  # convert temperatures
  tmin <- c_to_f(daily$temperature_min)
  tmax <- c_to_f(daily$temperature_max)

  # start with a base 86F model to chopping off the upper thresholds
  gdd <- tibble(base_86 = gdd_sine(tmin, tmax, 86))

  # generate each of the base temperature models with and without the upper threshold
  for (base in c(32, 39.2, 41, 45, 48, 50, 52, 55)) {
    name <- str_replace_all(paste0("base_", base), "\\.", "p")
    gdd[[name]] <- gdd_sine(tmin, tmax, base)
    gdd[[paste0(name, "_upper_86")]] <- gdd[[name]] - gdd$base_86
  }

  # remove the upper threshold model
  gdd$base_86 <- NULL

  # assemble, add cumulative cols, sort names
  bind_cols(attr, gdd) %>%
    mutate(
      across(starts_with("base_"), c(cumulative = cumsum)),
      .by = grid_id
    ) %>%
    select(
      all_of(names(attr)),
      all_of(sort(names(.)))
    )
}

# saved_weather %>% build_hourly() %>% build_daily() %>% build_gdd_from_daily()



# Site management ---------------------------------------------------------

sites_template <- tibble(
  id = integer(),
  name = character(),
  lat = numeric(),
  lng = numeric()
)

Site <- function(name, lat, lng, id = 999) {
  site <- as.list(environment())
}

# Site("foo", 1, 2)


#' Try read sites list from csv
#' @param fpath location of csv to read
load_sites <- function(fpath) {
  df <- read_csv(fpath, col_types = "c", show_col_types = F)
  if (nrow(df) == 0) stop("File was empty")
  df <- df %>%
    clean_names() %>%
    select(any_of(OPTS$site_cols)) %>%
    drop_na()
  if (!(all(c("name", "lat", "lng") %in% names(df)))) stop("File did not contain [name] [lat] [lng] columns.")
  df <- df %>%
    mutate(name = sanitize_loc_names(name)) %>%
    distinct(name, lat, lng) %>%
    filter(validate_ll(lat, lng))
  if (nrow(df) == 0) stop("No valid locations within service area.")
  df %>%
    mutate(id = row_number(), .before = 1) %>%
    head(OPTS$max_sites)
}

# # should succeed
# load_sites("test/example-sites.csv")
# load_sites("test/wisconet stns.csv")


#' Make sure names read from csv are valid and safe to display
#' adds a counter after any duplicate names
#' @param str character vector of names
sanitize_loc_names <- function(str) {
  str <- trimws(gsub("<[^>]+>", "", str))
  str <- str_trunc(str, 30)
  Encoding(str) <- "UTF-8"
  tibble(name = str) %>%
    mutate(count = row_number(), .by = name) %>%
    mutate(name = if_else(count > 1, paste0(name, " (", count, ")"), name)) %>%
    pull(name)
}

# # should include "foo (2)"
# sanitize_loc_names(c("foo", "foo", "bar"))
#
# # should strip html
# sanitize_loc_names(c("foo", "bar", "<a href='bad'>baz</a>"))







# UI builders -------------------------------------------------------------

#' Create the missing data element based on number of sites missing
missing_weather_ui <- function(n = 1) {
  msg <- ifelse(
    n == 1,
    "This site is missing data based on your date selections.",
    "One or more sites are missing data based on your date selections."
  )

  div(
    class = "missing-weather-notice",
    div(style = "color: orange; padding: 10px; font-size: 1.5em;", icon("warning")),
    div(em(msg, "Press", strong("Fetch weather"), "on the sidebar to download any missing data."))
  )
}

# missing_weather_ui(1)
# missing_weather_ui(2)


site_action_link <- function(action = c("edit", "save", "trash"), site_id, site_name = "") {
  action <- match.arg(action)
  hovertext = switch(action,
    edit = "Rename this site",
    save = "Pin this site to list",
    trash = "Delete this site"
  )
  onclick = switch(action,
    edit = sprintf("editSite(%s, \"%s\")", site_id, site_name),
    save = sprintf("saveSite(%s)", site_id),
    trash = sprintf("trashSite(%s)", site_id)
  )
  content <- as.character(switch(action,
    edit = icon("pen"),
    save = icon("thumbtack"),
    trash = icon("trash")
  ))
  sprintf("<a style='cursor:pointer' title='%s' onclick='%s'>%s</a>", hovertext, onclick, content)
}

# site_action_link("edit", 1, "foo")
# site_action_link("save", 1, "foo")
# site_action_link("trash", 1, "foo")


disease_modal_link <- function(disease) {
  md <- disease$doc
  name <- disease$name
  title <- paste(name, "information")
  onclick <- sprintf("sendShiny('show_modal', {md: '%s', title: '%s'})", md, title)
  shiny::HTML(sprintf("<a style='cursor:pointer' title='%s' onclick=\"%s\">More information</a>.", title, onclick))
}

# disease_modal_link(diseases$white_mold)


show_modal <- function(md, title = NULL) {
  if (!file.exists(md)) warning(md, " does not exist")
  modalDialog(
    includeMarkdown(md),
    title = title,
    footer = modalButton("Close"),
    easyClose = TRUE
  ) %>% showModal()
}




# Shutdown ----------------------------------------------------------------

clean_old_caches <- function(max_age_days = 30) {
  cache_files <- list.files(path = "cache", pattern = ".*\\.fst", full.names = TRUE)
  old_files <- cache_files[file.mtime(cache_files) < Sys.Date() - max_age_days]
  if (length(old_files) > 0) {
    file.remove(old_files)
    message("Cleaned ", length(old_files), " old cache files")
  }
}



# Extra/unused ----

## Data structures ----

# cat_names <- function(df) {
#   message(deparse(substitute(df)))
#   cat("c(")
#   cat(paste(paste0("\"", names(df), "\""), collapse = ", "))
#   cat(")\n")
# }

# get_specs <- function() {
#   ibm <- get_ibm(45, -89, today() - 1, today())
#   cat_names(ibm)

#   ibm_clean <- clean_ibm(ibm)
#   cat_names(ibm_clean)

#   hourly <- build_hourly(ibm_clean)
#   cat_names(hourly)

#   daily <- build_daily(hourly)
#   cat_names(daily)
# }

# get_specs()


## County shapefile ----

# prep_counties <- function() {
#   states <- read_sf("prep/cb-2018-conus-state-20m.geojson") %>%
#     clean_names() %>%
#     st_drop_geometry() %>%
#     select(statefp, state_name = name)

#   counties_sf <- read_sf("prep/cb-2018-conus-county-5m.geojson") %>%
#     clean_names() %>%
#     select(statefp, countyfp, county_name = name, geometry) %>%
#     left_join(states) %>%
#     relocate(state_name, .after = statefp)

#   counties_sf %>% write_rds("data/counties-conus.rds")
# }

# prep_counties()
