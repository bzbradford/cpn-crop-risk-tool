# Setup file for testthat - runs before all tests

# Source global.R from the app root directory
# chdir = TRUE ensures relative paths in global.R (like source_dir) work correctly
source("../../global.R", chdir = TRUE)

# save a response example for tests
if (FALSE) {
  # load 3 example sites
  test_sites <- load_sites("tests/testthat/example-sites.csv")

  # get and store weather data for testing
  test_hourly_wx <- om_fetch_weather(
    om_build_site_grids(test_sites),
    ymd("2025-1-1"),
    ymd("2025-12-31")
  )
  # mirror what the app actually stores in rv$weather: om_merge_wx drops NA
  # datetime_utc parse artifacts, then sorts and dedups by grid + time
  test_hourly_wx <- om_merge_wx(tibble(), test_hourly_wx)
  saveRDS(test_hourly_wx, "tests/testthat/test_hourly_wx.rds")
  test_hourly_wx <- readRDS("tests/testthat/test_hourly_wx.rds")
}

#' Load example data for tests
#' Note working directory for tests is relative to tests/testthat
test_sites <- load_sites("example-sites.csv")
test_hourly_wx <- readRDS("test_hourly_wx.rds")
test_daily_wx <- build_daily(test_hourly_wx)
