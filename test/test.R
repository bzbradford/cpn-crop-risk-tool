
## Data structures ----

cat_names <- function(df) {
  message(deparse(substitute(df)))
  cat("c(")
  cat(paste(paste0("\"", names(df), "\""), collapse = ", "))
  cat(")\n")
}

get_specs <- function() {
  ibm <- get_ibm(45, -89, today() - 1, today())
  cat_names(ibm)

  ibm_clean <- clean_ibm(ibm)
  cat_names(ibm_clean)

  hourly <- build_hourly(ibm_clean)
  cat_names(hourly)

  daily <- build_daily(hourly)
  cat_names(daily)
}

get_specs()


# Archive ------------------------------------------------------------------------

## County shapefile ----

prep_counties <- function() {
  states <- read_sf("prep/cb-2018-conus-state-20m.geojson") %>%
    clean_names() %>%
    st_drop_geometry() %>%
    select(statefp, state_name = name)

  counties_sf <- read_sf("prep/cb-2018-conus-county-5m.geojson") %>%
    clean_names() %>%
    select(statefp, countyfp, county_name = name, geometry) %>%
    left_join(states) %>%
    relocate(state_name, .after = statefp)

  counties_sf %>% write_rds("data/counties-conus.rds")
}

prep_counties()
