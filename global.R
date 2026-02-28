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

if (FALSE) {
  # add to renv without loading
  library(devtools)
  library(testthat)

  # this got removed from CRAN
  devtools::install_github("trafficonese/leaflet.extras")

  # RENV
  renv::init()
  renv::status()
  renv::restore()
  renv::update()
  renv::snapshot()
  renv::clean()

  # enable development mode
  shiny::devmode(TRUE)

  # turn warnings into errors
  options(warn = 2)

  # disable forecasts for testing
  options(forecast = FALSE)

  # Run unit tests
  testthat::test_dir("tests/testthat")
}

# load test weather
test_hourly_wx <- readRDS("tests/testthat/test_hourly_wx.rds")


# Async tasks ------------------------------------------------------------------

# set up a second session for asynchronous tasks but not in tests
if (!identical(Sys.getenv("TESTTHAT"), "true")) {
  plan(multisession, workers = 2)
}


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
  ibm_auth_timeout = 5,
  ibm_req_timeout = 10,
  ibm_chunk_size = 1000, # max hours per api call
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
    # "Model models" = "Model"
  ),

  ## Model risk tab ----
  model_group_choices = list(
    "Field crops" = "field",
    "Vegetable crops" = "vegetable"
  ),

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

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)), " <", paste(class(x), collapse = ", "), ">")
  print(x)
}

# display a message showing elapsed time since last timestamp
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

# return the first non-empty argument
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

# return 2 element vector of min, max
minmax <- function(x) {
  c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
}

# Date/time functions ----------------------------------------------------------

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

check_date_overlap <- function(date_range, dates_partial) {
  date_range <- as_date(date_range)
  date_seq <- seq.Date(date_range[1], date_range[2], 1)
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

# Summary functions ------------------------------------------------------------

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

roll_mean <- function(vec, width) {
  zoo::rollapplyr(vec, width, \(x) calc_mean(x), partial = TRUE)
}

roll_sum <- function(vec, width) {
  zoo::rollapplyr(vec, width, \(x) calc_sum(x), partial = TRUE)
}

# counts number consecutive runs of values above a threshold
# count_runs <- function(vec, threshold, min_run) {
#   runs <- run <- 0
#   for (val in vec) {
#     run <- if (val >= threshold) run + 1 else 0
#     if (run == min_run) runs <- runs + 1
#   }
#   runs
# }

# Unit conversions -------------------------------------------------------------

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

#' Convert wind direction to degrees
#' @param dirs vector of wind direction strings
#' @returns vector of wind direction angles in degrees (unnamed)
wind_dir_to_deg <- function(dirs) {
  compass_dirs <- c(
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
  degrees <- seq(0, 337.5, 22.5)

  degrees[match(dirs, compass_dirs)]
}

# wind_dir_to_deg(c("N", "ENE", "NNW", "foo"))

# Unit lookup functions --------------------------------------------------------

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

# test_hourly_wx |> convert_measures()
# test_hourly_wx |> build_daily() |> convert_measures()

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
  mapply(
    function(tmin, tmax, base) {
      if (is.na(tmin) || is.na(tmax)) {
        return(NA)
      }

      # swap min and max if in wrong order for some reason
      if (tmin > tmax) {
        t = tmin
        tmin = tmax
        tmax = t
      }

      # min and max < lower
      if (tmax <= base) {
        return(0)
      }

      average = (tmin + tmax) / 2

      # tmin > lower = simple average gdds
      if (tmin >= base) {
        return(average - base)
      }

      # tmin < lower, tmax > lower = sine gdds
      alpha = (tmax - tmin) / 2
      base_radians = asin((base - average) / alpha)
      a = average - base
      b = pi / 2 - base_radians
      c = alpha * cos(base_radians)
      (1 / pi) * (a * b + c)
    },
    tmin,
    tmax,
    base
  )
}

# gdd_sine(10:40, 0:30, 0)
# gdd_sine(5:35, 0:30, 10)

# Color helpers ----------------------------------------------------------------

# Define CSS named colors with their hex values
css_colors <- list(
  "red" = "#FF0000",
  "darkred" = "#8B0000",
  "lightred" = "#FFB6C1", # Using light pink as proxy
  "orange" = "#FFA500",
  "beige" = "#F5F5DC",
  "green" = "#008000",
  "darkgreen" = "#006400",
  "lightgreen" = "#90EE90",
  "blue" = "#0000FF",
  "darkblue" = "#00008B",
  "lightblue" = "#ADD8E6",
  "purple" = "#800080",
  "darkpurple" = "#483D8B", # Using dark slate blue as proxy
  "pink" = "#FFC0CB",
  "cadetblue" = "#5F9EA0",
  "white" = "#FFFFFF",
  "gray" = "#808080",
  "lightgray" = "#D3D3D3",
  "black" = "#000000"
)

# Function to convert hex to RGB
hex_to_rgb <- function(hex) {
  hex <- gsub("#", "", hex)
  if (nchar(hex) == 3) {
    hex <- paste0(
      substr(hex, 1, 1),
      substr(hex, 1, 1),
      substr(hex, 2, 2),
      substr(hex, 2, 2),
      substr(hex, 3, 3),
      substr(hex, 3, 3)
    )
  }
  r <- as.numeric(paste0("0x", substr(hex, 1, 2)))
  g <- as.numeric(paste0("0x", substr(hex, 3, 4)))
  b <- as.numeric(paste0("0x", substr(hex, 5, 6)))
  c(r, g, b)
}

# Function to calculate Euclidean distance in RGB space
color_distance <- function(rgb1, rgb2) {
  sqrt(sum((rgb1 - rgb2)^2))
}

# Function to calculate luminance for contrast ratio
get_luminance <- function(rgb) {
  # Convert RGB to relative luminance
  rgb_norm <- rgb / 255
  rgb_linear <- ifelse(
    rgb_norm <= 0.03928,
    rgb_norm / 12.92,
    ((rgb_norm + 0.055) / 1.055)^2.4
  )
  luminance <- 0.2126 *
    rgb_linear[1] +
    0.7152 * rgb_linear[2] +
    0.0722 * rgb_linear[3]
  luminance
}

# Function to determine text color based on contrast
get_text_color <- function(bg_luminance) {
  # Use a luminance threshold of 0.5 for better visual results
  # Colors darker than this threshold get white text, lighter colors get black text
  if (bg_luminance < 0.5) "#fff" else "#000"
}

#' Find the closest CSS color name for a given hex color
#' @param hex_color A hex color string (e.g., "#FF5733")
#' @returns A list containing the closest CSS color name, hex value, and contrast text color
find_closest_css_color <- function(hex_color) {
  # Validate and clean input hex color
  hex_color <- toupper(gsub("#", "", hex_color))
  if (!grepl("^[0-9A-F]{3}$|^[0-9A-F]{6}$", hex_color)) {
    warning(sprintf(
      "Invalid hex color format '%s'. Use format like '#FF0000' or '#F00'",
      hex_color
    ))
    return(list())
  }

  # Convert input hex to RGB
  input_rgb <- hex_to_rgb(paste0("#", hex_color))

  # Find closest color
  min_distance <- Inf
  closest_color <- NULL

  for (color_name in names(css_colors)) {
    css_rgb <- hex_to_rgb(css_colors[[color_name]])
    distance <- color_distance(input_rgb, css_rgb)

    if (distance < min_distance) {
      min_distance <- distance
      closest_color <- color_name
    }
  }

  # Calculate luminance of the input color for text contrast
  input_luminance <- get_luminance(input_rgb)
  text_color <- get_text_color(input_luminance)

  # Return results
  list(
    input_hex = paste0("#", hex_color),
    css_color = closest_color,
    css_hex_value = css_colors[[closest_color]],
    distance = round(min_distance, 2),
    text_color = text_color
  )
}


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
#' @param wx hourly weather data
#' @returns sf object of grid cell polygons
build_grids <- function(wx) {
  wx |>
    distinct(grid_id, grid_lat, grid_lng, time_zone) |>
    rowwise() |>
    mutate(geometry = ll_to_grid(grid_lat, grid_lng)) |>
    ungroup() |>
    st_set_geometry("geometry")
}

# test_hourly_wx |> build_grids()

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


# UI builders ------------------------------------------------------------------

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

# create a date button element
build_date_btn <- function(value, label, btn_class = c("default", "primary")) {
  btn_class <- match.arg(btn_class)
  HTML(
    str_glue(
      "<button class='btn btn-{btn_class} btn-xs action-button' onclick=\"this.blur(); Shiny.setInputValue('date_preset', '{value}', {{priority: 'event'}});\">{label}</button>"
    )
  )
}

#' @param model object created by Model() call
#' @returns HTML
build_modal_link <- function(model) {
  doc <- model$doc
  if (is.null(doc)) {
    return()
  }
  name <- model$name
  title <- paste(name, "information")
  onclick <- sprintf(
    "sendShiny('show_modal', {md: '%s', title: '%s'})",
    doc,
    title
  )
  shiny::HTML(
    sprintf(
      "<b><a style='cursor:pointer' title='%s' onclick=\"%s\">More&nbsp;information.</a></b>",
      title,
      onclick
    )
  )
}
# build_modal_link(model_list$whitemold)

#' @param md markdown file to display
#' @param title optional modal title
show_modal <- function(md, title = NULL) {
  if (!file.exists(md)) {
    warning(md, " does not exist")
    return()
  }

  m <- modalDialog(
    includeMarkdown(md),
    title = title,
    footer = modalButton("Close"),
    easyClose = TRUE
  )
  showModal(m)
}

#' @param content text or tags to display in the warning box
build_warning_box <- function(content) {
  if (is.null(content)) {
    return()
  }
  div(
    class = "warning-box-container",
    div(
      style = "color: orange; font-size: 1.5em;",
      icon("warning")
    ),
    div(
      style = "font-size: small;",
      content
    )
  )
}

#' @param sites sites df with needs_download column
weather_warning_for_sites <- function(sites) {
  if (nrow(sites) > 0 & any(sites[["needs_download"]])) {
    span(
      ifelse(nrow(sites) == 1, "This site is", "One or more sites are"),
      "missing data based on your date selections. Press",
      strong("Fetch weather"),
      "on the sidebar to download any missing data."
    )
  }
}


# Site constructor -------------------------------------------------------------

sites_template <- tibble(
  id = integer(),
  name = character(),
  lat = numeric(),
  lng = numeric()
)

# Site constructor
Site <- function(name, lat, lng, id = 999) {
  as.list(environment())
}

# Site("foo", 1, 2)

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

# Cookie helpers ---------------------------------------------------------------

#' Parse and validate sites from browser cookie data
#' @param cookie_sites the $sites value from the parsed cookie (list of site records)
#' @returns tibble of valid sites with sequential IDs, or NULL if none
parse_cookie_sites <- function(cookie_sites) {
  if (length(cookie_sites) == 0) {
    return(NULL)
  }

  tryCatch(
    {
      cookie_sites |>
        bind_rows() |>
        select(all_of(names(sites_template))) |>
        filter(validate_ll(lat, lng)) |>
        distinct() |>
        head(OPTS$max_sites) |>
        mutate(id = row_number())
    },
    error = function(e) {
      message("Failed to read sites from cookie: ", e)
      NULL
    }
  )
}

#' Get the cache file path for a user ID, creating the cache directory if needed
#' @param user_id character user ID from the browser cookie
#' @returns file path string, or NULL if user_id is empty/invalid
get_cache_file <- function(user_id) {
  if (!isTruthy(user_id)) {
    return(NULL)
  }
  cache_path <- "cache"
  if (!dir.exists(cache_path)) {
    dir.create(cache_path)
  }
  file.path(cache_path, paste0(user_id, ".fst"))
}


# Cache cleaner ----------------------------------------------------------------

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

# Load remaining code ----------------------------------------------------------

# source_dir <- function(path) {
#   files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
#   for (file in files) {
#     source(file)
#   }
# }

# source_dir("src")

list.files("src", pattern = "\\.[Rr]$", full.names = TRUE) |>
  lapply(source)
