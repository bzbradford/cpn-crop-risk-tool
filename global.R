#-- global.R --#

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor) # name cleaning
  library(sf) # GIS
  library(fst) # file storage
  library(httr2) # requests
  library(markdown)
  library(zoo) # rollmean

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

## development mode
# shiny::devmode(TRUE)

## RENV
# renv::update()
# renv::snapshot()
# renv::clean()

## turn warnings into errors
# options(warn = 2)



# Startup ----------------------------------------------------------------------

## Load files ----

saved_weather <- if (file.exists("data/saved_weather.fst")) {
  as_tibble(read_fst("data/saved_weather.fst"))
} else {
  tibble()
}

# EPSG 4326 for use in Leaflet
service_bounds <- read_rds("data/us_ca_clip.rds")

# transform to EPSG 3857 web mercator for intersecting points
service_bounds_3857 <- st_transform(service_bounds, 3857)

## Sites template ----
sites_template <- tibble(
  id = integer(),
  name = character(),
  lat = numeric(),
  lng = numeric(),
  temp = logical()
)



# Functions --------------------------------------------------------------------

## Utility ----

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)), " <", paste(class(x), collapse = ", "), ">")
  print(x)
}

runtime <- function(label = "", ref = now()) {
  message(">> ", label, " [", now(), "]")
  message(difftime(now(), ref), " since last timestamp")
}

# swaps names and values in a list or vector
invert <- function(x) {
  y <- as(names(x), class(x))
  names(y) <- x
  y
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

## test the CPN skin
# Sys.setenv("CPN_MODE" = TRUE)
# Sys.unsetenv("CPN_MODE")

OPTS <- lst(
  cpn_mode = Sys.getenv("CPN_MODE") == "TRUE",
  app_title = ifelse(cpn_mode, "Crop Risk Tool", "Researcher's Weather Data Tool"),
  app_header_color = ifelse(cpn_mode, "#00693c", "#c5050c"),
  app_header_badge = ifelse(cpn_mode, "cpn-badge.png", "uw-crest.svg"),
  app_footer_badge = if (cpn_mode) {
    div(
      style = "display: inline-flex; gap: 10px; align-items: center;",
      a(img(title = "Crop Protection Network", src = "cpn-logo.png", height = "50px"), href = "https://cropprotectionnetwork.org/", target = "_blank"),
      a(img(title = "National Predictive Modeling Tool Initiative", src = "npmti-logo.png", height = "35px"), href = "https://agpmt.org//", target = "_blank")
    )
  } else {
    a(img(title = "University of Wisconsin-Madison", src = "uw-logo.svg", height = "40px"), href = "https://cals.wisc.edu/", target = "_blank")
  },

  ibm_keys = list(
    org_id = Sys.getenv("ibm_org_id"),
    tenant_id = Sys.getenv("ibm_tenant_id"),
    api_key = Sys.getenv("ibm_api_key")
  ),
  ibm_auth_endpoint = "https://api.ibm.com/saascore/run/authentication-retrieve/api-key",
  ibm_weather_endpoint = "https://api.ibm.com/geospatial/run/v3/wx/hod/r1/direct",
  # max hours per api call
  ibm_chunk_size = 1000,

  google_key = Sys.getenv("google_places_key"),

  ibm_ignore_cols = c(
    "requestedLatitude",
    "requestedLongitude",
    "iconCode",
    "iconCodeExtended",
    "drivingDifficultyIndex"
  ),
  # how old should weather be before allowing a refresh?
  ibm_stale_hours = 3,

  # dates
  earliest_date = ymd("2015-1-1"),
  default_start_date = today() - 30,

  # map
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
  ),
  map_click_zoom = 10,

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
  max_sites = 10,


  #-- Data tab

  # validation messages
  validation_sites_ready = "No sites selected, click on the map or load sites in the sidebar.",
  validation_weather_ready = "No weather data downloaded yet for the selected dates. Click 'Fetch Weather' to download.",

  # data types
  data_type_choices = as.list(c(
    "Hourly" = "hourly",
    "Daily" = "daily",
    "Moving averages" = "ma",
    "Growing degree days" = "gdd",
    "Disease models" = { if (!cpn_mode) "disease" }
  )),


  #-- Disease risk tab

  risk_crop_choices = list(
    "Corn" = "corn",
    "Soybean" = "soybean",
    "Dry bean" = "drybean",
    "Potato/tomato" = "potato",
    "Carrot" = "carrot",
    "Beet" = "beet"
  ),

  crop_diseases = list(
    white_mold = c("soybean", "drybean"),
    gray_leaf_spot = "corn",
    tarspot = "corn",
    frogeye = "soybean",
    early_blight = "potato",
    late_blight = "potato",
    alternaria = "carrot",
    cercospora = "beet"
  ),

  risk_info = list(
    general = "Field crops disease risk assessments are based on probability of spore presence, while algorithms for vegetable diseases vary. Risk model is only valid when the crop is present and in a vulnerable growth stage (if applicable). Risk may be mitigated in commercial production by application of a protective fungicide with the last 14 days. Set the start date to the approximate date of crop emergence for accurate risk assessments.",
    corn = "Corn diseases include tarspot and gray leaf spot. Corn is vulnerable to these diseases when in the growth stages V10-R3.",
    soybean = "Soybean diseases include white mold and frogeye leaf spot. Soybean is vulnerable to white mold when in the growth stages R1-R3 (flowering), and vulnerable to frogeye leaf spot when in R1-R5.",
    drybean = "Dry bean diseases include white mold. The crop is vulnerable to white mold when in the growth stages R1-R3.",
    potato = "Potato, tomato, eggplant, and other Solanaceous plants are susceptible to white mold, early blight and late blight. Early blight risk depends on the number of potato physiological days (P-days) accumulated since crop emergence, while late blight risk depends on the number of disease severity values generated in the last 14 days and since crop emergence.",
    carrot = "Carrots are susceptible to the foliar disease Alternaria leaf blight. Alternaria risk depends on the number of disease severity values generated in the last 7 days.",
    beet = "Beets are susceptible to Cercospora leaf blight. Cercospora risk depends on the average disease severity values in the past 2 days and 7 days."
  ),

  # add site_ before some columns in the sites table
  # site_attr_rename = {
  #   cols <- c("id", "name", "lat", "lng")
  #   names(cols) <- paste0("site_", cols)
  #   cols
  # },

  # plotting
  site_attr_cols = c("site_id", "site_name", "site_lat", "site_lng", "temp"),
  grid_attr_cols = c("grid_id", "grid_lat", "grid_lng", "date_min", "date_max", "days_expected", "days_actual", "days_missing", "days_missing_pct", "hours_expected", "hours_actual", "hours_missing", "hours_missing_pct", "geometry"),
  date_attr_cols = c("datetime_utc", "time_zone", "datetime_local", "date", "yday", "year", "month", "day", "hour", "night", "date_since_night"),
  daily_attr_cols = c("date", "yday", "year", "month", "day"),
  plot_default_cols = c("temperature", "temperature_mean", "temperature_mean_7day", "base_50_upper_86_cumulative"),
  plot_ignore_cols = c(site_attr_cols, grid_attr_cols, date_attr_cols),
)


# NOAA forecast api ------------------------------------------------------------

noaa_point_url <- function(lat, lng) {
  sprintf("https://api.weather.gov/points/%.6g,%.6g", lat, lng)
}

# noaa_point_url(45, -89)


noaa_get_forecast_url <- function(lat, lng, url = noaa_point_url(lat, lng)) {
  tryCatch({
    stopifnot(validate_ll(lat, lng))
    req <- request(url) %>%
      req_timeout(5) %>%
      req_retry(max_tries = 2, retry_on_failure = TRUE)
    t <- now()
    resp <- req_perform(req) %>% resp_body_json()
    message("GET => '", url, "' completed in ", as.numeric(now() - t))
    resp$properties$forecastHourly
  }, error = function(e) {
    message("Failed to retrieve ", url, ": ", e$message)
    echo(e)
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

# units: temperature = F, wind_speed = mph
noaa_parse_forecast <- function(periods) {
  periods %>%
    lapply(function(p) {
      # hoist the nested values
      p$probabilityOfPrecipitation <- p$probabilityOfPrecipitation$value
      # dewpoint is provided in Celsius
      p$dewpoint <- p$dewpoint$value
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
      across(c(start_time, end_time), ~.x + minutes(20)),
      across(temperature, f_to_c),
      across(wind_speed, mi_to_km),
      across(wind_direction, wind_dir_to_deg)
    ) %>%
    select(
      datetime_utc = start_time,
      temperature,
      temperature_dew_point = dewpoint,
      relative_humidity,
      wind_speed,
      wind_direction
    )
}

noaa_get_forecast <- function(lat = NULL, lng = NULL, url = noaa_get_forecast_url(lat, lng)) {
  tryCatch({
    req <- request(url) %>%
      req_timeout(5) %>%
      req_retry(max_tries = 2, retry_on_failure = TRUE)
    t <- now()
    resp <- req_perform(req) %>% resp_body_json()
    message("GET => '", url, "' completed in ", as.numeric(now() - t))
    noaa_parse_forecast(resp$properties$periods)
  }, error = function(e) {
    message("Failed to get forecast from ", url, ": ", e$message)
    echo(e)
    tibble()
  })
}

# # should succeed
# noaa_get_forecast(45, -89)
# noaa_get_forecast(38, -121)
#
# # should fail
# noaa_get_forecast(1, 1)
# noaa_get_forecast()



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
      req_timeout(5)
    resp <- req_perform(req)
    message("Authorization token refreshed at ", now("UTC"))
    list(
      timestamp = now("UTC"),
      status = resp_status(resp),
      token = resp_body_string(resp)
    )
  }, catch = function(e) {
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
      start = format(first(chunk), "%Y-%m-%dT%H:%M:%S%z"),
      end = format(last(chunk), "%Y-%m-%dT%H:%M:%S%z"),
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



#' Fetch hourly weather data from IBM
#' API documentation: https://docs.google.com/document/d/13HTLgJDpsb39deFzk_YCQ5GoGoZCO_cRYzIxbwvgJLI/edit?tab=t.0
#' @param lat latitude of point
#' @param lng longitude of point
#' @param dates_need vector of dates needed
#' @param dates_have vector of dates already downloaded
#' @param url endpoint, changed only for testing failures
#' @returns tibble, either with hourly data if successful or empty if failed
get_ibm <- function(lat, lng, dates_need, dates_have, url = OPTS$ibm_weather_endpoint) {
  stime <- Sys.time()
  tz <- lutz::tz_lookup_coords(lat, lng, warn = F)
  chunks <- ibm_chunks(dates_need, dates_have, tz)

  if (length(chunks) == 0) {
    message(str_glue("Already had weather for {lat}, {lng}"))
    return(tibble())
  }

  start_date <- min(dates_need)
  end_date <- max(dates_need)

  responses <- tryCatch({
    token <- get_ibm_token()
    if (!is.character(token)) stop("Failed to get IBM token.")

    reqs <- lapply(chunks, function(chunk) {
      request(url) %>%
        req_headers_redacted(
          "x-ibm-client-id" = sprintf("geospatial-%s", OPTS$ibm_keys$tenant_id),
          "Authorization" = sprintf("Bearer %s", token)
        ) %>%
        req_url_query(
          format = "json",
          geocode = str_glue("{lat},{lng}"),
          startDateTime = chunk$start,
          endDateTime = chunk$end,
          units = "m"
        ) %>%
        req_timeout(10)
    })

    # perform parallel requests for each time chunk
    resps <- req_perform_parallel(reqs, on_error = "continue", progress = F)

    # gather response data
    lapply(resps, function(resp) {
      tryCatch({
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

  wx <- bind_rows(responses)
  msg <- if (nrow(wx) > 0) {
    str_glue("OK ==> Got weather for {lat}, {lng} from {start_date} to {end_date} in {round(Sys.time() - stime, 3)} using {length(chunks)} calls")
  } else {
    str_glue("FAIL ==> Could not get weather for {lat}, {lng} from {start_date} to {end_date}")
  }
  message(msg)
  wx
}

# should succeed
# get_ibm(43.0731, -89.4012, "2024-1-1", "2024-1-2")
# df <- get_ibm(43.0731, -89.4012, "2024-1-1", "2024-12-31") %>% clean_ibm()
# ggplot(df, aes(x = datetime_local, y = temperature)) + geom_line()

# should fail
# get_ibm(43.0731, -89.4012, "2024-1-1", "2024-12-31", url = 'foo')

# should partially fail because start date is before earliest data 2015-6-29
# get_ibm(43.0731, -89.4012, "2015-1-1", "2015-8-1")



#' Update weather for sites list and date range
#' @param sites sf with site locs
#' @param start_date
#' @param end_date
fetch_weather <- function(sites, start_date, end_date) {
  status_msg <- NULL
  all_dates <- seq.Date(start_date, end_date, 1)
  wx <- saved_weather
  sites <- sites %>% st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F)

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

    # get weather if needed
    if (length(dates_need) > 0) {
      resp <- get_ibm(site$lat, site$lng, dates_need, dates_have)
      incProgress(1)
      if (nrow(resp) == 0) {
        status_msg <- sprintf("Unable to get some/all weather for %.2f, %.2f from %s to %s.", site$lat, site$lng, start_date, end_date)
        next
      }
      new_wx <- clean_ibm(resp)
      wx <- bind_rows(new_wx, wx) %>%
        distinct(grid_id, datetime_utc, .keep_all = T) %>%
        arrange(grid_lat, grid_lng, datetime_utc)
    }
  }

  saved_weather <<- wx
  write_fst(saved_weather, "data/saved_weather.fst", compress = 99)
  return(status_msg)
}



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
  "wind_speed",              "m/s",  "mph",  mps_to_mph,
  "wind_gust",               "m/s",  "mph",  mps_to_mph,
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


#' Summarize downloaded weather data by grid cell and creates sf object
#' used to intersect site points with existing weather data
#' @param wx hourly weather data from `clean_ibm` function
#' @param start_date start of expected date range
#' @param end_date end of expected date range
#' @returns tibble
weather_status <- function(wx, start_date = min(wx$date), end_date = max(wx$date)) {
  dates_expected <- seq.Date(start_date, end_date, 1)
  wx <- filter(wx, between(date, start_date, end_date))
  if (nrow(wx) == 0) return(tibble(grid_id = NA, needs_download = TRUE))
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
      hours_stale = as.integer(difftime(now(tzone = tz), time_max_actual, units = "hours")),
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




# Field Crops Disease Models ---------------------------------------------------

# Logistic function to convert logit to probability
logistic <- function(logit) exp(logit) / (1 + exp(logit))


## Corn/Bean ----

#' White mold, dryland model - Corn & Bean
#' Growth stage: Soy R1-R3
#' Risk criteria: High >=40%, Med >=20%, Low >=5%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @param MaxAT_30ma 30-day moving average of daily maximum temperature, Celsius
#' @param MaxWS_30ma 30-day moving average of daily maximum wind speed, m/s
#' @param MaxRH_30ma 30-day moving average of daily maximum relative humidity, 0-100%
#' @returns probability of spore presence
predict_whitemold_dry <- function(MaxAT_30ma, MaxWS_30ma, MaxRH_30ma) {
  m1 <- -.47 * MaxAT_30ma - 1.01 * MaxWS_30ma + 16.65
  m2 <- -.68 * MaxAT_30ma + 17.19
  m3 <- -.86 * MaxAT_30ma + 0.1 * MaxRH_30ma - 0.75 * MaxWS_30ma + 8.2
  (logistic(m1) + logistic(m2) + logistic(m3)) / 3
}

# expand_grid(temp = 0:40, wind = 0:20, rh = (0:10) * 10) %>%
#   mutate(prob = predict_whitemold_dry(temp, wind, rh)) %>%
#   ggplot(aes(x = temp, y = wind, fill = prob)) +
#   geom_tile() +
#   scale_fill_distiller(palette = "Spectral", limits = c(0, 1)) +
#   coord_cartesian(expand = F) +
#   facet_wrap(~rh, labeller = "label_both")


#' White mold, irrigated model - Corn & Bean
#' Risk criteria: High >=40%, Med >=20%, Low >=5%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @param MaxAT_30MA Maximum daily temperature, 30-day moving average, Celsius
#' @param MaxRH_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param spacing Row spacing, either "15" or "30", inches
#' @returns probability of spore presence
predict_whitemold_irrig <- function(MaxAT_30MA, MaxRH_30ma, spacing = c("15", "30")) {
  spacing <- match.arg(spacing)
  mu <- -2.38 * (spacing == "30") + 0.65 * MaxAT_30MA + 0.38 * MaxRH_30ma - 52.65
  logistic(mu)
}

# expand_grid(temp = 15:40, rh = seq(50, 100, 5), spacing = c("15", "30")) %>%
#   rowwise() %>%
#   mutate(prob = predict_whitemold_irrig(temp, rh, spacing)) %>%
#   ggplot(aes(x = temp, y = rh, fill = prob)) +
#   geom_tile() +
#   facet_wrap(~spacing, ncol = 1) +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)


## Corn ----

#' Gray leaf spot model - Corn
#' Use when growth stage V10-R3
#' Risk criteria: High >=60%, Medium >=40%, Low >0%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @returns probability of spore presence
#' @param MinAT_21ma Minimum daily temperature, 21-day moving average, Celsius
#' @param MinDP_30ma Minimum dew point temperature, 30-day moving average, Celsius
#' @returns probability of spore presence
predict_gls <- function(MinAT_21ma, MinDP_30ma) {
  mu <- -2.9467 - 0.03729 * MinAT_21ma + 0.6534 * MinDP_30ma
  logistic(mu)
}

# expand_grid(temp = 0:40, dp = 0:15) %>%
#   mutate(prob = predict_gls(temp, dp)) %>%
#   ggplot(aes(x = temp, y = dp, fill = prob)) +
#   geom_tile() +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)


#' Tarspot 'tarspotter' - Corn
#' Use when growth stage V10-R3
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


## Soy ----

#' Frogeye leaf spot model - Soy
#' Use when growth stage R1-R5
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
#' - https://www.google.com/books/edition/The_Plant_Disease_Reporter/ow9BD6P2KZ4C?hl=en&gbpv=1&dq=wallins%20blitecast&pg=PA95&printsec=frontcover
#' - https://ipm.ucanr.edu/DISEASE/DATABASE/potatolateblight.html
#' More information: https://vegpath.plantpath.wisc.edu/diseases/potato-late-blight/
#' @param temp Mean temperature during hours where RH > 90%, Celsius
#' @param h Number of hours where RH > 90%
#' @returns numeric 0-4 disease severity values
calc_late_blight_dsv <- function(temp, h) {
  case_when(
    is.na(temp) | is.na(h) ~ 0,
    temp <   7.2 ~ 0,
    temp <= 11.6 ~ (h > 21) + (h > 18) + (h > 15),
    temp <= 15.0 ~ (h > 21) + (h > 18) + (h > 15) + (h > 12),
    temp <= 26.6 ~ (h > 18) + (h > 15) + (h > 12) + (h > 9),
    temp >  26.6 ~ 0
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
#' @param temp Mean temperature during hours where RH > 90%, Celsius, converted to Fahrenheit internally
#' @param h Number of hours where RH > 90%
#' @returns 0-7 div
calc_cercospora_div <- function(temp, h) {
  temp <- c_to_f(temp)
  case_when(
    is.na(temp) | is.na(h) ~ 0,
    temp <= 60 ~ 0,
    temp <= 61 ~ (h > 21),
    temp <= 62 ~ (h > 19) + (h > 22),
    temp <= 63 ~ (h > 16) + (h > 19) + (h > 21),
    temp <= 64 ~ (h > 13) + (h > 15) + (h > 18) + (h > 20) + (h > 23),
    temp <= 65 ~ (h > 6) + (h > 8) + (h > 12) + (h > 18) + (h > 21),
    temp <= 71 ~ (h > 3) + (h > 6) + (h > 10) + (h > 14) + (h > 18) + (h > 21),
    temp <= 72 ~ (h > 2) + (h > 6) + (h > 9) + (h > 13) + (h > 17) + (h > 20),
    temp <= 73 ~ (h > 1) + (h > 6) + (h > 9) + (h > 12) + (h > 16) + (h > 19),
    temp <= 76 ~ 1 + (h > 5) + (h > 9) + (h > 11) + (h > 16) + (h > 18) + (h > 23),
    temp <= 77 ~ 1 + (h > 5) + (h > 8) + (h > 12) + (h > 15) + (h > 18) + (h > 22),
    temp <= 78 ~ 1 + (h > 5) + (h > 8) + (h > 11) + (h > 14) + (h > 17) + (h > 20),
    temp <= 79 ~ 1 + (h > 4) + (h > 7) + (h > 9) + (h > 12) + (h > 14) + (h > 17),
    temp <= 80 ~ 1 + (h > 3) + (h > 6) + (h > 8) + (h > 10) + (h > 12) + (h > 15),
    temp <= 81 ~ 1 + (h > 2) + (h > 4) + (h > 6) + (h > 7) + (h > 9) + (h > 11),
    temp <= 82 ~ 1 + (h > 2) + (h > 4) + (h > 5) + (h > 7) + (h > 8) + (h > 10),
    temp >  82 ~ 1 + (h > 2) + (h > 4) + (h > 5) + (h > 7) + (h > 8) + (h > 9)
  )
}

# expand_grid(temp = 15:30, hours = 0:24) %>%
#   mutate(dsv = cercospora_div(temp, hours), temp = c_to_f(temp)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dsv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)



# Disease severity --------------------------------------------------------
# Functions for converting model outputs into risk scores (eg low/med/high)

assign_risk <- function(model, value) {
  switch(model,
    "white_mold_dry_prob"      = risk_from_prob(value, .01, 20, 35),
    "white_mold_irrig_30_prob" = risk_from_prob(value, .01, 5, 10),
    "white_mold_irrig_15_prob" = risk_from_prob(value, .01, 5, 10),
    "gray_leaf_spot_prob"      = risk_from_prob(value, 1, 40, 60),
    "tarspot_prob"             = risk_from_prob(value, 1, 20, 35),
    "frogeye_leaf_spot_prob"   = risk_from_prob(value, 1, 40, 50),
    "potato_pdays"             = risk_for_earlyblight(value),
    "late_blight_dsv"          = risk_for_lateblight(value),
    "alternaria_dsv"           = risk_for_alternaria(value),
    "cercospora_div"           = risk_for_cercospora(value)
  )
}


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
      labels = c("Very low risk", "Low risk", "Medium risk", "High risk"),
      include.lowest = TRUE,
      right = FALSE
    ),
    risk_color = colorFactor("Spectral", risk, reverse = TRUE)(risk),
    value_label = sprintf("%.0f%% (%s)", prob * 100, risk)
  )
}

# tibble(
#   value = runif(10),
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
  tibble(
    risk = cut(
      severity,
      breaks = 0:5,
      labels = c("Very low", "Low", "Medium", "High", "Very high"),
      include.lowest = TRUE,
      right = FALSE
    ),
    risk_color = colorFactor("Spectral", risk, reverse = TRUE)(risk)
  )
}

# tibble(
#   value = round(runif(10, 0, 4)),
#   risk_from_severity(value)
# )


#' Assign risk score for early blight p-day accumulation
#' @param value dsv from `calc_pdays` function
risk_for_earlyblight <- function(value) {
  tibble(
    total = cumsum(value),
    avg7 = rollapplyr(value, 7, mean, partial = TRUE),
    severity = case_when(
      total >= 300 ~
        (avg7 >= 1) +
        (avg7 >= 3) +
        (avg7 >= 5) +
        (avg7 >= 8),
      TRUE ~
        (total >= 150) +
        (total >= 200) +
        (total >= 250)
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
risk_for_lateblight <- function(value) {
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



# Botcast - onion



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
    ) %>%
    arrange(grid_lat, grid_lng, datetime_local) %>%
    mutate(precip_cumulative = cumsum(precip), .after = precip) %>%
    mutate(snow_cumulative = cumsum(snow), .after = snow) %>%
    mutate(dew_point_depression = abs(temperature - dew_point), .after = dew_point)
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
      across(c(temperature, dew_point, dew_point_depression, relative_humidity), summary_fns),
      across(c(precip, snow), c("daily" = calc_sum, "max_hourly" = calc_max)),
      across(c(pressure_mean_sea_level, wind_speed), summary_fns),
      wind_gust_max = calc_max(wind_gust),
      across(c(wind_direction), summary_fns),
      hours_rh_over_80 = sum(relative_humidity >= 80),
      hours_rh_over_90 = sum(relative_humidity >= 90),
      .by = c(grid_id, date, yday, year, month, day)
    ) %>%
    mutate(precip_cumulative = cumsum(precip_daily), .after = precip_max_hourly, .by = grid_id ) %>%
    mutate(snow_cumulative = cumsum(snow_daily), .after = snow_max_hourly, .by = grid_id)

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
    left_join(by_night, join_by(grid_id, date == date_since_night)) %>%
    left_join(lat_lng, join_by(grid_id)) %>%
    relocate(grid_lat, grid_lng, .after = grid_id) %>%
    arrange(grid_id, date)
}

# saved_weather %>% build_hourly() %>% build_daily()


#' Generate several moving average periods from daily data
#' @param daily accepts daily data from `build_daily()`
#' @param align moving average alignment
#' @returns tibble
build_ma_from_daily <- function(daily, align = c("center", "right")) {
  align <- match.arg(align)

  # retain attribute cols
  attr <- daily %>% select(grid_id, any_of(OPTS$date_attr_cols))

  # define moving average functions
  roll_mean <- function(vec, width) rollapply(vec, width, \(x) mean(x, na.rm = T), fill = NA, partial = T, align = align)
  fns <- c(
    "7day" = ~roll_mean(.x, 7),
    "14day" = ~roll_mean(.x, 14),
    "21day" = ~roll_mean(.x, 21),
    "30day" = ~roll_mean(.x, 30)
  )

  # apply moving average functions to each primary data column
  ma <- daily %>%
    mutate(
      across(starts_with(c("temperature", "dew_point", "relative_humidity", "wind", "pressure", "hours")), fns),
      .keep = "none"
    )

  # bind attributes
  bind_cols(attr, ma)
}

# saved_weather %>% build_hourly() %>% build_daily() %>% build_ma_from_daily()


#' Plant diseases that use daily values as inputs
#' units must be metric: temperature degC, wind speed km/hr
#' @param daily accepts daily dataset from `build_daily()` but should start 30 days before first disease estimate is desired
#' @returns tibble
build_disease_from_ma <- function(daily) {
  roll_mean <- function(vec, width) rollapplyr(vec, width, \(x) mean(x, na.rm = T), fill = NA, partial = T)
  # retain attribute cols
  attr <- daily %>% select(grid_id, date)

  # generate disease models and add cumulative sums where appropriate
  disease <- daily %>%
    mutate(
      temperature_max_30day = roll_mean(temperature_max, 30),
      temperature_mean_30day = roll_mean(temperature_mean, 30),
      temperature_min_21day = roll_mean(temperature_min, 21),
      wind_speed_max_30day = roll_mean(wind_speed_max, 30),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      dew_point_min_30day = roll_mean(dew_point_min, 30),
      hours_rh_over_80_30day = roll_mean(hours_rh_over_80, 30),
      hours_rh_over_90_night_14day = roll_mean(hours_rh_over_90_night, 14)
    ) %>%
    mutate(
      # corn/soybean/drybean
      white_mold_dry_prob =
        predict_whitemold_dry(temperature_max_30day, kmh_to_mps(wind_speed_max_30day), relative_humidity_max_30day) %>%
        attenuate_prob(temperature_min_21day),
      white_mold_irrig_30_prob =
        predict_whitemold_irrig(temperature_max_30day, relative_humidity_max_30day, "30"),
      white_mold_irrig_15_prob =
        predict_whitemold_irrig(temperature_max_30day, relative_humidity_max_30day, "15"),
      # corn
      gray_leaf_spot_prob =
        predict_gls(temperature_min_21day, dew_point_min_30day),
      tarspot_prob =
        predict_tarspot(temperature_mean_30day, relative_humidity_max_30day, hours_rh_over_90_night_14day) %>%
        attenuate_prob(temperature_min_21day),
      # soybean
      frogeye_leaf_spot_prob =
        predict_fls(temperature_max_30day, hours_rh_over_80_30day),
      .keep = "none", .by = grid_id
    ) %>%
    select(sort(names(.))) %>%
    select(-grid_id)

  # bind attributes
  bind_cols(attr, disease)
}

# saved_weather %>% build_hourly() %>% build_daily() %>% build_disease_from_ma()


#' Plant diseases that use daily values as inputs
#' units must be metric: temperature degC, wind speed km/hr
#' @param daily accepts daily dataset from `build_daily()`
#' @returns tibble
build_disease_from_daily <- function(daily) {
  roll_mean <- function(vec, width) rollapplyr(vec, width, \(x) mean(x, na.rm = T), fill = NA, partial = T)
  # retain attribute cols
  attr <- daily %>% select(grid_id, date)

  # generate disease models and add cumulative sums where appropriate
  disease <- daily %>%
    mutate(
      # potato/tomato
      potato_pdays =
        calc_pdays(temperature_min, temperature_max),
      late_blight_dsv =
        calc_late_blight_dsv(temperature_mean_rh_over_90, hours_rh_over_90),
      # carrot
      alternaria_dsv =
        calc_alternaria_dsv(temperature_mean_rh_over_90, hours_rh_over_90),
      # beet
      cercospora_div =
        calc_cercospora_div(temperature_mean_rh_over_90, hours_rh_over_90),
      .keep = "none", .by = grid_id
    ) %>%
    mutate(
      across(
        c(potato_pdays, late_blight_dsv, alternaria_dsv, cercospora_div),
        c(cumulative = cumsum)
      ),
      .by = grid_id
    ) %>%
    select(sort(names(.))) %>%
    select(-grid_id)

  # bind attributes
  bind_cols(attr, disease)
}

# saved_weather %>% build_hourly() %>% build_daily() %>% build_disease_from_daily()


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
    mutate(across(starts_with("base_"), c(cumulative = cumsum)), .by = grid_id) %>%
    select(
      all_of(names(attr)),
      all_of(sort(names(.)))
    )
}

# saved_weather %>% build_hourly() %>% build_daily() %>% build_gdd_from_daily()



# Site management ---------------------------------------------------------

#' Return the next available id number for creating a new site
#' @param ids vector of numeric ids that are already in use
create_id <- function(ids) {
  ids <- as.integer(ids)
  possible_ids <- 1:(length(ids) + 1)
  setdiff(possible_ids, ids)
}

# # should return 4
# create_id(c(1, 2, 3, 5))


#' Validate and fill in defaults for new sites
#' @param loc named list with location attributes
create_site <- function(loc, sites) {
  loc$id <- create_id(sites$id)
  loc$temp <- !isTruthy(loc$temp)

  # make sure it has all the attributes
  missing_attr <- setdiff(names(sites_template), names(loc))
  if (isTruthy(missing_attr)) stop("Loc is missing ", missing_attr)

  # validate lat/lng
  req(validate_ll(loc$lat, loc$lng))

  # keep only approved names
  loc <- loc[names(loc) %in% names(sites_template)]

  loc
}

# # should succeed and assign id 1
# create_site(list(lat = 45, lng = -89, name = "foo"), sites_template)
#
# # should fail, invalid lat/lng
# create_site(list(lat = -45, lng = -89, name = "foo"), sites_template)
#
# # should fail, loc is missing keys
# create_site(list(), sites_template)


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
    mutate(temp = FALSE) %>%
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
    style = "width: 100%; display: inline-flex; align-items: center; border: 1px solid orange; border-radius: 5px; background-color: white; padding; 5px;",
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



# Plotly ------------------------------------------------------------------

# add forecast annotation
plotly_show_forecast <- function(plt, xmax) {
  if ("Date" %in% class(xmax)) {
    x <- today()
    label <- "Today"
  } else {
    x <- now()
    label <- "Now"
  }

  text <- list(list(
    yref = "paper",
    x = x, y = 1,
    text = label,
    showarrow = F,
    opacity = .5
  ))

  vline <- list(list(
    type = "line", yref = "paper",
    x0 = x, x1 = x, y0 = 0, y1 = .95,
    line = list(color = "black", dash = "dash"),
    opacity = .25
  ))

  area <- list(list(
    type = "rect",
    fillcolor = "orange",
    line = list(opacity = 0),
    opacity = 0.05,
    yref = "paper",
    x0 = x, x1 = xmax,
    y0 = 0, y1 = 1,
    layer = "below"
  ))

  plt %>% layout(shapes = c(vline, area), annotations = text)
}


# data should have cols: date, name, value, value_label, risk
disease_plot <- function(data, xrange = NULL) {
  # expand the range by amt %
  yrange <- function(lo, hi, amt = .05) {
    c(lo - abs(hi - lo) * amt, hi + abs(hi - lo) * amt)
  }

  title_font <- list(family = "Redhat Display", size = 14)
  axis_font <- list(family = "Redhat Text", size = 12)

  # axis config
  x <- y1 <- y2 <- list(
    title = "",
    showticklabels = FALSE,
    showgrid = FALSE,
    showline = FALSE,
    zeroline = FALSE,
    fixedrange = TRUE,
    tickfont = axis_font
  )
  x$showticklabels <- TRUE
  x$range <- xrange
  x$hoverformat <- "<b>%b %d, %Y</b>"
  y1$range <- yrange(0, 1)
  y2$range <- yrange(0, 4)
  y2$overlaying <- "y"

  if (!("severity" %in% names(data))) data$severity <- NA
  data <- data %>%
    mutate(
      yaxis = if_else(grepl("_prob", model), "y1", "y2"),
      value = coalesce(severity, value)
    )

  models <- unique(data$model)
  lapply(models, function(model) {
    df <- data %>% filter(model == !!model)
    yaxis <- first(df$yaxis)
    plot_ly(df, x = ~date, y = ~value, height = 100) %>%
      add_trace(
        name = ~name,
        text = ~value_label,
        type = "scatter",
        mode = "lines+markers",
        marker = list(color = ~risk_color),
        line = list(width = 2),
        hovertemplate = "%{text}",
        hoverinfo = "text",
        yaxis = yaxis
      ) %>%
      layout(
        title = list(
          text = ~sprintf("<b>%s</b>", first(name)),
          x = 0,
          y = .99,
          yanchor = "top",
          font = title_font
        ),
        margin = list(l = 5, r = 5, b = 5, t = 5, pad = 5),
        xaxis = x,
        yaxis = y1,
        yaxis2 = y2,
        hovermode = "x unified",
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE) %>%
      plotly_show_forecast(xmax = xrange[2])
  })
}




# Archive ------------------------------------------------------------------------

## County shapefile ----

# prep_counties <- function() {
#   states <- read_sf("prep/cb-2018-conus-state-20m.geojson") %>%
#     clean_names() %>%
#     st_drop_geometry() %>%
#     select(statefp, state_name = name)
#
#   counties_sf <- read_sf("prep/cb-2018-conus-county-5m.geojson") %>%
#     clean_names() %>%
#     select(statefp, countyfp, county_name = name, geometry) %>%
#     left_join(states) %>%
#     relocate(state_name, .after = statefp)
#
#   counties_sf %>% write_rds("data/counties-conus.rds")
# }
#
# prep_counties()


## Data structures ----

# cat_names <- function(df) {
#   message(deparse(substitute(df)))
#   cat("c(")
#   cat(paste(paste0("\"", names(df), "\""), collapse = ", "))
#   cat(")\n")
# }
#
# get_specs <- function() {
#   ibm <- get_ibm(45, -89, today() - 1, today())
#   cat_names(ibm)
#
#   ibm_clean <- clean_ibm(ibm)
#   cat_names(ibm_clean)
#
#   hourly <- build_hourly(ibm_clean)
#   cat_names(hourly)
#
#   daily <- build_daily(hourly)
#   cat_names(daily)
# }
#
# get_specs()
