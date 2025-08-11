# config.R ----

get_app_settings <- function() {
  dplyr::lst(

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
}
