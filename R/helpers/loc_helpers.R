# Location helpers ----

# EPSG 4326 for use in Leaflet
service_bounds <- read_rds("data/us_ca_clip.rds")

# transform to EPSG 3857 web mercator for intersecting points
service_bounds_3857 <- st_transform(service_bounds, 3857)

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
