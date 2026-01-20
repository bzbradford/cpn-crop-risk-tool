#-- global.R --#

suppressPackageStartupMessages({
  # core
  library(tidyverse)
  library(janitor) # name cleaning
  library(sf) # GIS
  library(fst) # file storage
  library(httr2) # requests
  library(markdown)
  library(zoo) # rollmean
  library(future) # async
  library(promises) # async

  # shiny
  library(shiny)
  library(shinythemes)
  library(shinyWidgets)
  library(shinyjs)
  library(shinyalert)
  library(htmltools)
  library(shinycssloaders)

  # components
  library(leaflet)
  library(leaflet.extras)
  library(htmlwidgets)
  library(plotly)
  library(DT)
})


# Dev settings -----------------------------------------------------------------

# add to renv without loading
if (FALSE) {
  library(languageserver)
  library(styler)
  library(miniUI)
  library(watcher)
}


## development mode
# shiny::devmode(TRUE)

## RENV
# renv::status()
# renv::restore()
# renv::update()
# renv::snapshot()
# renv::clean()

# renv::install("httr2@1.1.0")
# renv::install("ragg@1.4.0") # -> 1.5.0
# renv::install("terra@1.8-60") # -> 1.8-80

## turn warnings into errors
# options(warn = 2)

# disable forecasts for testing
# options(forecast = FALSE)


# Async tasks ------------------------------------------------------------------

# set up a second session for asynchronous tasks
plan(multisession, workers = 2)


# Settings ---------------------------------------------------------------------

OPTS <- lst(
  ## general ----
  app_title = "Crop Risk Tool",
  app_header_color = "#00693c",
  app_header_badge = "cpn-badge.png",
  contact_email = "bbradford@wisc.edu",

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
    grid = "Weather data grids"
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
  grid_attr_cols = c(
    "grid_id",
    "grid_lat",
    "grid_lng",
    "time_zone",
    "date_min",
    "date_max",
    "days_expected",
    "days_actual",
    "days_missing",
    "days_missing_pct",
    "hours_expected",
    "hours_actual",
    "hours_missing",
    "hours_missing_pct",
    "geometry"
  ),
  date_attr_cols = c(
    "datetime_utc",
    "time_zone",
    "datetime_local",
    "date",
    "yday",
    "year",
    "month",
    "day",
    "hour",
    "night",
    "date_since_night"
  ),
  daily_attr_cols = c("date", "yday", "year", "month", "day"),
  plot_default_cols = c(
    "temperature",
    "temperature_mean",
    "temperature_mean_7day",
    "base_50_upper_86_cumulative"
  ),
  plot_ignore_cols = c(site_attr_cols, grid_attr_cols, date_attr_cols),
)


# Utility functions ------------------------------------------------------------

source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  for (file in files) {
    source(file)
  }
}


# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)), " <", paste(class(x), collapse = ", "), ">")
  print(x)
}


runtime <- function(label = "timestamp", ref = NULL) {
  t <- now()
  message(">> ", label, " [", t, "]")
  if (!is.null(ref)) {
    message(difftime(t, ref), " elapsed")
  }
  t
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
  for (arg in list(...)) {
    if (shiny::isTruthy(arg)) {
      return(arg)
    }
  }
  NULL
}


# restrict a value to between two extremes
clamp <- function(x, min, max) {
  pmax(min, pmin(max, x))
}


# calculate the difference in hours between two timestamps
hours_diff <- function(start, end) {
  as.integer(ceiling(difftime(end, start, units = "hours")))
}

# hours_diff(now(), now())
# hours_diff(now() - hours(6), now())
# hours_diff(now() - days(1), now())


#' @param ... partial dates like 'aug 1' to convert to yday using current year
get_yday <- function(...) {
  args <- list(...)
  sapply(args, function(v) {
    paste(year(Sys.Date()), v) |>
      ymd() |>
      yday()
  })
}

# get_yday("jun 1", "aug 2")


check_date_overlap <- function(dates_actual, dates_partial) {
  dates_actual <- as_date(dates_actual)
  date_seq <- seq.Date(dates_actual[1], dates_actual[2], 1)
  yrs <- unique(year(date_seq))
  sapply(set_names(yrs), function(yr) {
    dt <- ymd(paste(yr, dates_partial))
    test_seq <- seq.Date(dt[1], dt[2])
    any(date_seq %in% test_seq)
  })
}

# check_date_overlap(c("2025-4-1", "2025-7-1"), c("May 1", "Aug 1"))
# check_date_overlap(c("2025-4-1", "2025-7-1"), c("Jan 1", "Feb 1"))
# check_date_overlap(c("2024-10-1", "2025-7-1"), c("Jun 1", "Aug 1"))



## NA-safe summary functions ----

calc_sum <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  sum(x, na.rm = TRUE)
}

calc_min <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  min(x, na.rm = TRUE)
}

calc_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  mean(x, na.rm = TRUE)
}

calc_max <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  max(x, na.rm = TRUE)
}


## Rolling functions ----

roll_mean <- function(vec, width) {
  zoo::rollapplyr(vec, width, \(x) calc_mean(x), partial = TRUE)
}

roll_sum <- function(vec, width) {
  zoo::rollapplyr(vec, width, \(x) calc_sum(x), partial = TRUE)
}


## Sliding window functions ----

# counts number consecutive runs of values above a threshold
# count_runs <- function(vec, threshold, min_run) {
#   runs <- run <- 0
#   for (val in vec) {
#     run <- if (val >= threshold) run + 1 else 0
#     if (run == min_run) runs <- runs + 1
#   }
#   runs
# }


## Unit conversions ----

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
  c(
    "N",
    "NNE",
    "NE",
    "ENE",
    "E",
    "ESE",
    "SE",
    "SSE",
    "S",
    "SSW",
    "SW",
    "WSW",
    "W",
    "WNW",
    "NW",
    "NNW"
  )
)

#' Convert wind direction to degrees
#' @param dirs vector of wind direction strings
#' @returns vector of wind direction angles in degrees
wind_dir_to_deg <- function(dirs) {
  sapply(dirs, function(dir) {
    if (dir %in% names(compass_directions)) compass_directions[[dir]] else NA
  })
}

# lapply(names(compass_directions), wind_dir_to_deg)



#' List of weather variables, unit suffixes, and conversion functions
#' all derivative columns of each of these will start with the same text
#' e.g. temperature => temperature_min => temperature_min_30ma
measures <- tribble(
  ~measure                  , ~metric , ~imperial , ~conversion  ,
  "temperature"             , "°C"    , "°F"      , c_to_f       ,
  "dew_point"               , "°C"    , "°F"      , c_to_f       ,
  "relative_humidity"       , "%"     , "%"       , \(x) x       , # no conversion
  "precip"                  , "mm"    , "in"      , mm_to_in     ,
  "snow"                    , "cm"    , "in"      , cm_to_in     ,
  "wind_speed"              , "kmh"   , "mph"     , km_to_mi     ,
  "wind_gust"               , "kmh"   , "mph"     , km_to_mi     ,
  "wind_direction"          , "°"     , "°"       , \(x) x       , # no conversion
  "pressure_mean_sea_level" , "mbar"  , "inHg"    , mbar_to_inHg ,
  "pressure_change"         , "mbar"  , "inHg"    , mbar_to_inHg ,
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

# saved_weather |> convert_measures()
# saved_weather |> build_daily() |> convert_measures()



#' Returns the unit name for the given column
#' used to append unit suffix in plotly
#' @param col_name data column name that needs a unit suffix
#' @param unit_system
find_unit <- function(col_name, unit_system = c("metric", "imperial")) {
  unit_system <- match.arg(unit_system)
  matched <- measures |>
    rowwise() |>
    filter(grepl(measure, col_name))
  if (nrow(matched) == 1) matched[[unit_system]] else ""
}

# find_unit("temperature", "metric")


#' Adds the unit suffix to each column name where appropriate
#' for exporting as CSV so unit is documented
#' @param df from the data pipeline
#' @param unit_system
#' @returns df with updated column names
rename_with_units <- function(df, unit_system = c("metric", "imperial")) {
  unit_system <- match.arg(unit_system)
  for (i in seq_len(nrow(measures))) {
    m <- measures[i, ]
    df <- df |>
      rename_with(
        .fn = ~ paste(.x, m[[unit_system]], sep = "_", recycle0 = TRUE),
        .cols = starts_with(m$measure)
      )
  }
  clean_names(df)
}

# tibble(temperature = 1) |> rename_with_units("metric")
# tibble(temperature = 1) |> rename_with_units("imperial")


## Cache ----

clean_old_caches <- function(max_age_days = 30) {
  cache_files <- list.files(
    path = "cache",
    pattern = ".*\\.fst",
    full.names = TRUE
  )
  old_files <- cache_files[file.mtime(cache_files) < Sys.Date() - max_age_days]
  if (length(old_files) > 0) {
    file.remove(old_files)
    message("Cleaned ", length(old_files), " old cache files")
  }
}

# clean_old_caches()



# Crop and Disease definitions -------------------------------------------------

#' @param name display name
#' @param info model info
#' @param doc markdown file for More Information
#' @param risk_period NULL or length two character vector eg 'Jul 1'
Disease <- function(name, info, doc, risk_period = NULL, ...) {
  stopifnot(
    "Invalid parameter provided" = length(list(...)) == 0,
    "Missing doc file" = doc %in% list.files(pattern = "*.md", recursive = TRUE)
  )

  # check that all risk period dates are valid
  if (!is.null(risk_period)) {
    withCallingHandlers(
      {
        ymd(paste(year(Sys.Date()), risk_period))
      },
      warning = function(w) {
        stop("Invalid date format for risk_period: ", risk_period)
      }
    )
  }

  list(name = name, info = info, doc = doc, risk_period = risk_period)
}

diseases <- list(
  # Corn
  tar_spot = Disease(
    name = "Tar spot",
    info = HTML(
      "<b>Corn is susceptible to tar spot when in the growth stages V10-R3 (10th leaf - milk).</b> Risk is based on probability of spore presence. Model depends on temperature and relative humidity."
    ),
    doc = "docs/tar-spot.md",
    risk_period = c("Jul 1", "Aug 15")
  ),
  gray_leaf_spot = Disease(
    name = "Gray leaf spot",
    info = HTML(
      "<b>Corn is susceptible to gray leaf spot when in the growth stages V10-R3 (10th leaf - milk)</b>. Risk is based on probability of spore presence. Model depends on minimum temperature and dew point."
    ),
    doc = "docs/gray-leaf-spot.md",
    risk_period = c("Jul 1", "Aug 15")
  ),
  don = Disease(
    name = "Gibberella ear rot/DON",
    info = HTML(
      "<b>Corn is susceptible to Gibberella ear rot during silking.</b> Infection by this disease may lead to deoxynivalenol (DON) accumulation in the ear to dangerous levels. Risk is based on the probability of deoxynivalenol exceeding 1ppm in harvested grain and silage. Model depends on temperature, precipitation, and relative humidity during the 3 weeks prior to silking."
    ),
    doc = "docs/don.md",
    risk_period = c("Jul 15", "Aug 7")
  ),

  # Soybean
  white_mold = Disease(
    name = "White mold",
    info = HTML(
      "<b>Soybean is vulnerable to white mold when in the growth stages R1-R3 (flowering - beginning pod).</b> Risk is based on probability of spore presence. Model depends on 30-day moving average maximum temperature, relative humidity, and wind speed (non-irrigated model only)."
    ),
    doc = "docs/white-mold.md",
    risk_period = c("Jun 15", "Aug 7")
  ),
  frogeye = Disease(
    name = "Frogeye leaf spot",
    info = HTML(
      "<b>Soybean is vulnerable to frogeye leaf spot when in the growth stages R1-R5 (flowering - beginning seed).</b> Risk is based on probability of spore presence. Model depends on 30-day moving average maximum temperature and daily hours of high humidity."
    ),
    doc = "docs/frogeye.md",
    risk_period = c("Jun 15", "Sep 7")
  ),

  # Solanum
  early_blight = Disease(
    name = "Early blight",
    info = "Early blight may affect potato, tomato, pepper, eggplant, and other Solanaceous plants. Risk depends on the number of potato physiological days (P-days) accumulated since crop emergence, which are generated based on daily min/max temperatures.",
    doc = "docs/early-blight.md"
  ),
  late_blight = Disease(
    name = "Late blight",
    info = "Late blight may affect potato, tomato, pepper, eggplant, and other Solanaceous plants. Risk depends on the number of disease severity values generated in the last 14 days and since crop emergence. Model depends on temperature and hours of high humidity.",
    doc = "docs/late-blight.md"
  ),

  # Carrot
  alternaria = Disease(
    name = "Alternaria/Cercospora leaf blight",
    info = "Alternaria and Cercospora leaf blights are a common fungal disease of carrot leaves and petioles. Risk depends on the number of disease severity values generated in the last 7 days. Model depends on temperature and hours of high humidity.",
    doc = "docs/alternaria.md"
  ),

  # Carrot + Beet
  cercospora = Disease(
    name = "Cercospora leaf spot",
    info = "Cercospora leaf spot is a damaging fungal disease affecting beets. Risk depends on the average disease severity values in the past 2 days and 7 days. Model depends on temperature and hours of high humidity.",
    doc = "docs/cercospora.md"
  ),

  # Onion
  botrytis = Disease(
    name = "Botrytis leaf blight",
    info = "Onions are susceptible to Botrytis leaf blight. Risk depends on cumulative disease severity values since crop emergence. Model depends on temperature, hours of high humidity, and precipitation.",
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
      diseases$alternaria
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



# Location helpers -------------------------------------------------------------

# EPSG 4326 for use in Leaflet
service_bounds <- read_rds("data/us_ca_clip.rds")

# transform to EPSG 3857 web mercator for intersecting points
service_bounds_3857 <- st_transform(service_bounds, 3857)

#' returns TRUE if location is within service boundary shapefile
#' @param lat latitude of point
#' @param lng longitude of point
#' @returns boolean
validate_ll <- function(lat, lng) {
  mapply(
    function(lat, lng) {
      if (!is.numeric(lat) | !is.numeric(lng)) {
        return(FALSE)
      }
      pt <- st_point(c(lng, lat)) |>
        st_sfc(crs = 4326) |>
        st_transform(st_crs(service_bounds_3857))
      length(st_intersection(pt, service_bounds_3857)) == 1
    },
    lat,
    lng
  )
}

# validate_ll(45, -89)
# validate_ll(0, 0)


#' Creates an appropriately sized grid polygon based on centroid coordinates
#' @param lat latitude of point
#' @param lng longitude of point
#' @param d decimal degree distance from center to edge of grid
#' @returns sf object
ll_to_grid <- function(lat, lon, d = 1 / 45.5) {
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
  wx |>
    distinct(grid_id, grid_lat, grid_lng, time_zone) |>
    rowwise() |>
    mutate(geometry = ll_to_grid(grid_lat, grid_lng)) |>
    ungroup() |>
    st_set_geometry("geometry")
}

# saved_weather |> build_grids()


#' Add some more information for displaying on the map
annotate_grids <- function(grids_with_status) {
  grids_with_status |>
    mutate(
      title = if_else(
        needs_download,
        "Weather grid (download required)",
        "Weather grid"
      ),
      color = if_else(needs_download, "orange", "darkgreen"),
      label = paste0(
        "<b>",
        title,
        "</b><br>",
        "Earliest date: ",
        date_min,
        "<br>",
        "Latest date: ",
        date_max,
        "<br>",
        if_else(
          date_max == today(),
          paste0("Most recent data: ", hours_stale, " hours ago<br>"),
          ""
        ),
        "Total days: ",
        days_expected,
        "<br>",
        "Missing days: ",
        days_missing,
        sprintf(" (%.1f%%)", 100 * days_missing_pct),
        "<br>",
        "Missing hours: ",
        hours_missing,
        sprintf(" (%.1f%%)", 100 * hours_missing_pct),
        "<br>"
      ) |>
        lapply(HTML)
    )
}

# test_grids <- saved_weather |> build_grids()
# test_status <- saved_weather |> weather_status(today() - days(30), today())
# left_join(test_grids, test_status) |>
#   annotate_grids()


# Sites ------------------------------------------------------------------------

sites_template <- tibble(
  id = integer(),
  name = character(),
  lat = numeric(),
  lng = numeric()
)


# Site builder
Site <- function(name, lat, lng, id = 999) {
  as.list(environment())
}

# Site("foo", 1, 2)


#' parse lat/lng coordinates from string
#' @param str input string containing coordinates to parse in form "lat, lng"
#' @returns named list { lat: numeric, lng: numeric }
parse_coords <- function(str) {
  str <- gsub("[ ,\t°NW]", " ", str)
  parts <- str_split_1(str_squish(str), " ")
  if (length(parts) != 2) {
    stop("Invalid coordinate format.")
  }
  coords <- suppressWarnings(list(
    lat = as.numeric(parts[1]),
    lng = as.numeric(parts[2])
  ))
  if (any(sapply(coords, is.na))) {
    stop("Failed to parse coordinates.")
  }
  coords
}

# parse_coords("45, -89")
# parse_coords("foo")


#' Make sure names read from csv are valid and safe to display
#' adds a counter after any duplicate names
#' @param str character vector of names
sanitize_loc_names <- function(str) {
  str <- trimws(gsub("<[^>]+>", "", str))
  str <- str_trunc(str, 30)
  Encoding(str) <- "UTF-8"
  tibble(name = str) |>
    mutate(count = row_number(), .by = name) |>
    mutate(name = if_else(count > 1, paste0(name, " (", count, ")"), name)) |>
    pull(name)
}

# should include "foo (2)"
# sanitize_loc_names(c("foo", "foo", "bar"))
# should strip html
# sanitize_loc_names(c("foo", "bar", "<a href='bad'>baz</a>"))


#' Try read sites list from csv
#' @param fpath location of csv to read
load_sites <- function(fpath) {
  df <- read_csv(fpath, col_types = "c", show_col_types = FALSE)
  if (nrow(df) == 0) {
    stop("File was empty")
  }
  df <- df |>
    clean_names() |>
    select(any_of(OPTS$site_cols)) |>
    drop_na()
  if (!(all(c("name", "lat", "lng") %in% names(df)))) {
    stop("File did not contain [name] [lat] [lng] columns.")
  }
  df <- df |>
    mutate(name = sanitize_loc_names(name)) |>
    distinct(name, lat, lng) |>
    filter(validate_ll(lat, lng))
  if (nrow(df) == 0) {
    stop("No valid locations within service area.")
  }
  df |>
    mutate(id = row_number(), .before = 1) |>
    head(OPTS$max_sites)
}

# load_sites("data/example-sites.csv")
# load_sites("data/wisconet stns.csv")



# UI builders ------------------------------------------------------------------

warning_box <- function(html) {
  div(
    class = "warning-box-container",
    div(
      style = "color: orange; padding: 10px; font-size: 1.5em;",
      icon("warning")
    ),
    div(HTML(html))
  )
}

#' Create the missing data element based on number of sites missing
missing_weather_ui <- function(n = 1) {
  str <- ifelse(n == 1, "This site is ", "One or more sites are")
  html <- paste0(
    "<i>",
    str,
    " missing data based on your date selections. Press <b>Fetch weather</b> on the sidebar to download any missing data.</i>"
  )
  warning_box(html)
}

# missing_weather_ui(1)
# missing_weather_ui(2)

site_action_link <- function(
    action = c("edit", "save", "trash"),
  site_id,
  site_name = ""
) {
  action <- match.arg(action)
  hovertext <- switch(
    action,
    edit = "Rename this site",
    save = "Pin this site to list",
    trash = "Delete this site"
  )
  onclick <- switch(
    action,
    edit = sprintf("editSite(%s, \"%s\")", site_id, site_name),
    save = sprintf("saveSite(%s)", site_id),
    trash = sprintf("trashSite(%s)", site_id)
  )
  content <- as.character(switch(
    action,
    edit = icon("pen"),
    save = icon("thumbtack"),
    trash = icon("trash")
  ))
  sprintf(
    "<a style='cursor:pointer' title='%s' onclick='%s'>%s</a>",
    hovertext,
    onclick,
    content
  )
}

# site_action_link("edit", 1, "foo")
# site_action_link("save", 1, "foo")
# site_action_link("trash", 1, "foo")


disease_modal_link <- function(disease) {
  md <- disease$doc
  name <- disease$name
  title <- paste(name, "information")
  onclick <- sprintf(
    "sendShiny('show_modal', {md: '%s', title: '%s'})",
    md,
    title
  )
  shiny::HTML(sprintf(
    "<a style='cursor:pointer' title='%s' onclick=\"%s\">More information</a>.",
    title,
    onclick
  ))
}

# disease_modal_link(diseases$white_mold)


show_modal <- function(md, title = NULL) {
  if (!file.exists(md)) {
    warning(md, " does not exist")
  }
  modalDialog(
    includeMarkdown(md),
    title = title,
    footer = modalButton("Close"),
    easyClose = TRUE
  ) |>
    showModal()
}



# Load remaining code ----

source_dir("src")
