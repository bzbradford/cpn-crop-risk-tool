# tests for ibm api interface and associated helpers

# Helpers -----------------------------------------------------------------

test_that("dates_to_dttm works", {
  date_seq <- seq.Date(today() - days(30), today())
  hr_seq <- dates_to_dttm(date_seq)

  # should have class datetime
  expect_s3_class(hr_seq, "POSIXct")

  # should create hourly sequence from date sequence
  expect_equal(length(hr_seq), length(date_seq) * 24)
})

test_that("ibm_chunks works", {
  # should show 4 chunks of up to 1000 hours are needed
  x <- ibm_chunks(
    dates_need = seq.Date(as_date("2024-3-1"), as_date("2024-8-1"), by = 1)
  )
  expect_length(x, 4)
  expect_named(x[[1]], c("start", "end", "length"))

  # should show 2 chunks needed (already have 2 covered)
  x <- ibm_chunks(
    dates_need = seq.Date(as_date("2024-3-1"), as_date("2024-8-1"), by = 1),
    dates_have = seq.Date(as_date("2024-4-1"), as_date("2024-7-1"), by = 1)
  )
  expect_length(x, 2)

  # should be null (all dates covered)
  x <- ibm_chunks(
    dates_need = seq.Date(as_date("2024-5-1"), as_date("2024-7-1"), by = 1),
    dates_have = seq.Date(as_date("2024-3-1"), as_date("2024-8-1"), by = 1)
  )
  expect_null(x)
})


# API response ------------------------------------------------------------

test_that("ibm response formatted correctly", {
  expect_s3_class(test_ibm_response, "data.frame")
})


# Post-processing ---------------------------------------------------------

test_that("ibm_clean_resp works", {
  expect_silent({
    test_ibm_response |> ibm_clean_resp()
  })

  # graph the temperature
  expect_silent({
    test_ibm_response |>
      ibm_clean_resp() |>
      ggplot(aes(x = datetime_local, y = temperature)) +
      geom_line() +
      facet_wrap(~ sprintf("%.2f, %.2f", grid_lat, grid_lng), ncol = 1)
  })
})

test_that("build_hourly works", {
  expect_silent({
    test_ibm_response |>
      ibm_clean_resp() |>
      build_hourly()
  })
})

test_that("daily_status works", {
  expect_silent({
    test_ibm_response |>
      ibm_clean_resp() |>
      build_hourly() |>
      filter(grid_id == sample(grid_id, 1)) |>
      daily_status() |>
      tail()
  })
})

test_that("weather_status works", {
  wx <- test_ibm_response |>
    ibm_clean_resp() |>
    build_hourly()

  # summarizes from min date to max date by default
  x <- weather_status(wx)

  # returns one row per grid id
  expect_equal(length(unique(wx$grid_id)), nrow(x))

  # check a specific date range
  expect_silent({
    wx |>
      weather_status(start_date = ymd("2025-1-1"), end_date = ymd("2025-2-21"))
  })

  # works when no weather data
  x <- wx |>
    filter(FALSE) |>
    weather_status()
  expect_all_true(x$needs_download)

  # works when no weather in range
  x <- wx |>
    weather_status(start_date = ymd("2020-1-1"), end_date = ymd("2020-2-1"))
  expect_all_true(x$needs_download)
})
