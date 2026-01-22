# Setup file for testthat - runs before all tests

# Source global.R from the app root directory
# chdir = TRUE ensures relative paths in global.R (like source_dir) work correctly
source("../../global.R", chdir = TRUE)

# save a response example for tests
if (FALSE) {
  # two sites, semi-overlapping 2-month windows
  test_ibm_response <- bind_rows(
    ibm_create_reqs(
      lat = 45,
      lng = -89,
      dates_need = seq.Date(ymd("2025-5-1"), ymd("2025-7-1"))
    ) |>
      get_ibm(),
    ibm_create_reqs(
      lat = 50,
      lng = -90,
      dates_need = seq.Date(ymd("2025-6-1"), ymd("2025-8-1"))
    ) |>
      get_ibm(),
  )
  saveRDS(test_ibm_response, "tests/testthat/test_ibm_response.rds")

  # two sites, full year
  test_hourly_wx <- fetch_weather(
    tibble(),
    tribble(
      ~lat , ~lng ,
        45 ,  -89 ,
        30 ,  -95 ,
    ),
    start_date = ymd("2025-1-1"),
    end_date = ymd("2025-12-31")
  )
  saveRDS(test_hourly_wx, "tests/testthat/test_hourly_wx.rds")
}

# load example ibm response data for tests
test_ibm_response <- readRDS("test_ibm_response.rds")
test_hourly_wx <- readRDS("test_hourly_wx.rds")
