# tests for src/api_openmeteo.R
#
# Strategy:
# - Smoke-test the data-pipeline functions (request builders, parsers, status
#   summarizers) so regressions in the chained transforms surface early.
# - More explicit assertions on helpers with edge cases (grid snapping, date
#   chunking, merge dedup).
# - Network-bound fetchers (om_fetch_weather, om_fetch_forecast) are exercised
#   indirectly via test_hourly_wx and not re-tested here.

# Helpers ----------------------------------------------------------------------

# Build a fake httr2 response from a JSON file path or raw JSON string
fake_om_resp <- function(body, status = 200L) {
  body_raw <- if (length(body) == 1 && file.exists(body)) {
    readBin(body, "raw", n = file.size(body))
  } else {
    charToRaw(body)
  }
  httr2::response(
    status_code = status,
    headers = list("content-type" = "application/json"),
    body = body_raw,
    url = "https://archive-api.open-meteo.com/v1/archive"
  )
}

example_resp <- fake_om_resp("open_meteo_example.json")


# Variable list ----------------------------------------------------------------

test_that("openmeteo_vars names are all in conversion_lookup", {
  missing <- setdiff(names(openmeteo_vars), conversion_lookup$measure)
  expect_length(missing, 0)
})


# Request builders -------------------------------------------------------------

test_that("om_build_req composes archive URL with expected query params", {
  req <- om_build_req(45, -89, "2026-05-01", "2026-05-04")
  expect_s3_class(req, "httr2_request")
  expect_match(
    req$url,
    "^https://customer-archive-api\\.open-meteo\\.com/v1/archive"
  )
  expect_match(req$url, "latitude=45")
  expect_match(req$url, "longitude=-89")
  expect_match(req$url, "start_date=2026-05-01")
  expect_match(req$url, "end_date=2026-05-04")
  expect_match(req$url, "timezone=auto")
  # all requested vars are present (comma-joined via .multi="comma")
  for (v in openmeteo_vars) {
    expect_match(req$url, v, fixed = TRUE)
  }
})

test_that("om_build_forecast_req hits forecast endpoint with forecast_days", {
  req <- om_build_forecast_req(45, -89, days = 7)
  expect_match(req$url, "^https://customer-api\\.open-meteo\\.com/v1/forecast")
  expect_match(req$url, "forecast_days=7")
  expect_match(req$url, "latitude=45")
  expect_match(req$url, "longitude=-89")
})


# Response validation ----------------------------------------------------------

test_that("om_resp_ok returns TRUE on a healthy response", {
  expect_true(om_resp_ok(example_resp))
})

test_that("om_resp_ok returns FALSE and messages on an error object", {
  err <- simpleError("boom")
  class(err) <- c("error", "condition")
  expect_message(out <- om_resp_ok(err), "Network error")
  expect_false(out)
})

test_that("om_resp_ok returns FALSE and messages on HTTP error status", {
  bad <- fake_om_resp('{"reason":"bad query"}', status = 400L)
  expect_message(out <- om_resp_ok(bad), "HTTP error")
  expect_false(out)
})


# JSON parsing -----------------------------------------------------------------

test_that("om_parse_json builds a tidy tibble with expected columns", {
  parsed <- om_parse_json(example_resp)
  expect_s3_class(parsed, "tbl_df")
  # metadata columns appear before hourly cols
  expect_true(all(
    c("grid_lat", "grid_lng", "elevation", "timezone", "tz_offset") %in%
      names(parsed)
  ))
  # parsed time cols
  expect_true(all(
    c("datetime_local", "datetime_utc", "date") %in% names(parsed)
  ))
  expect_false("time" %in% names(parsed))
  # at least one requested variable is present (renaming happens later)
  expect_true("temperature_2m" %in% names(parsed))
})

test_that("om_parse_json parses local times in the response timezone, not UTC", {
  parsed <- om_parse_json(example_resp)
  # example response is America/Chicago. First row in the JSON is
  # "2026-05-01T00:00" which is midnight Chicago = 05:00 UTC (CDT). If parsed
  # incorrectly as UTC, datetime_local would equal "2026-05-01 00:00" in UTC.
  expect_equal(unique(parsed$timezone), "America/Chicago")
  expect_equal(tz(parsed$datetime_local), "America/Chicago")
  expect_equal(
    format(parsed$datetime_utc[1], "%Y-%m-%d %H:%M", tz = "UTC"),
    "2026-05-01 05:00"
  )
})

test_that("om_parse_resp returns empty tibble on bad response", {
  err <- simpleError("nope")
  class(err) <- c("error", "condition")
  expect_message(out <- om_parse_resp(err), "Network error")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)

  bad <- fake_om_resp('{"reason":"bad"}', status = 500L)
  expect_message(out2 <- om_parse_resp(bad), "HTTP error")
  expect_equal(nrow(out2), 0)
})

test_that("om_parse_resp returns parsed tibble on good response", {
  out <- om_parse_resp(example_resp)
  expect_gt(nrow(out), 0)
  expect_true("datetime_local" %in% names(out))
})


# Format hourly ----------------------------------------------------------------

test_that("om_build_hourly returns empty tibble for empty input", {
  out <- om_build_hourly(tibble())
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})

test_that("om_build_hourly produces the canonical hourly schema", {
  out <- om_parse_resp(example_resp) |> om_build_hourly()
  expected_cols <- c(
    "grid_id",
    "grid_lat",
    "grid_lng",
    "elevation",
    "timezone",
    "tz_offset",
    "datetime_utc",
    "datetime_local",
    "date",
    names(openmeteo_vars),
    "dew_point_depression"
  )
  expect_true(all(expected_cols %in% names(out)))
  # grid_id is auto-generated from lat/lng
  expect_true(all(grepl("^-?\\d+\\.\\d{3},-?\\d+\\.\\d{3}$", out$grid_id)))
  # dew_point_depression is non-negative
  expect_true(all(out$dew_point_depression >= 0, na.rm = TRUE))
})

test_that("om_build_hourly preserves caller-supplied grid_id", {
  parsed <- om_parse_resp(example_resp) |>
    mutate(grid_id = "custom-id")
  out <- om_build_hourly(parsed)
  expect_true(all(out$grid_id == "custom-id"))
})


# O1280 grid cells -------------------------------------------------------------

test_that("get_o1280_cells returns cells whose bbox contains the centroid", {
  # use a handful of well-spread centroids
  lats <- c(45.02636, 30, -45, 0.5)
  lngs <- c(-89.02173, 0, 100, -179.5)
  cells <- get_o1280_cells(lats, lngs)
  expect_equal(nrow(cells), length(lats))
  expect_true(all(
    c("xmin", "xmax", "ymin", "ymax", "geometry") %in% names(cells)
  ))
  # centroids fall within their assigned bbox
  expect_true(all(lats >= cells$ymin & lats <= cells$ymax))
  expect_true(all(lngs >= cells$xmin & lngs <= cells$xmax))
})

test_that("get_o1280_cells errors on mismatched input lengths", {
  expect_error(get_o1280_cells(c(45, 46), -89))
})

test_that("get_o1280_cells produces narrower longitude bands near the poles", {
  # rings near the equator have many lng cells (small d_lng);
  # rings near the pole have few (large d_lng)
  eq <- get_o1280_cells(0, 0)
  pole <- get_o1280_cells(89.9, 0)
  expect_lt(eq$xmax - eq$xmin, pole$xmax - pole$xmin)
})


# Grid + status summaries ------------------------------------------------------

test_that("om_build_grids returns one sf row per unique grid_id", {
  grids <- om_build_grids(test_hourly_wx)
  expect_s3_class(grids, "sf")
  expect_equal(
    nrow(grids),
    length(unique(test_hourly_wx$grid_id))
  )
  expect_true(all(
    c("grid_id", "timezone", "elevation", "geometry") %in% names(grids)
  ))
})

test_that("om_wx_daily_status reports hours per grid-date", {
  daily <- om_wx_daily_status(test_hourly_wx)
  expect_s3_class(daily, "tbl_df")
  expect_true(all(
    c(
      "grid_id",
      "date",
      "hours_actual",
      "hours_expected",
      "hours_missing"
    ) %in%
      names(daily)
  ))
  # hours_missing was clamped at >= 0
  expect_true(all(daily$hours_missing >= 0))
  # most complete (non-today) days should report 24 hours
  full_days <- daily |> filter(date < today())
  expect_true(median(full_days$hours_actual) == 24)
})

test_that("om_wx_status returns the default sentinel for empty input", {
  out <- om_wx_status(tibble())
  expect_true(all(is.na(out$grid_id)))
  expect_true(all(out$needs_download))
})

test_that("om_wx_status summarizes coverage per grid", {
  out <- om_wx_status(test_hourly_wx)
  expect_s3_class(out, "tbl_df")
  expect_true(all(
    c(
      "grid_id",
      "date_min",
      "date_max",
      "days_expected",
      "days_actual",
      "days_missing",
      "hours_expected",
      "hours_missing",
      "needs_download",
      "dates_have"
    ) %in%
      names(out)
  ))
  expect_equal(nrow(out), length(unique(test_hourly_wx$grid_id)))
})

test_that("om_wx_status flags needs_download when dates outside fixture range requested", {
  far_future <- today() + days(30)
  out <- om_wx_status(
    test_hourly_wx,
    start_date = far_future,
    end_date = far_future
  )
  # no rows in selected_wx for that future date -> default sentinel
  expect_true(all(out$needs_download))
})

test_that("om_grid_status joins grids and status by grid_id", {
  out <- om_grid_status(test_hourly_wx)
  expect_s3_class(out, "sf")
  expect_true(all(
    c("grid_id", "needs_download", "dates_have", "geometry") %in% names(out)
  ))
})

test_that("om_join_grids matches site lat/lng to grid bbox", {
  grids <- om_grid_status(test_hourly_wx)
  joined <- om_join_grids(test_sites, grids)
  expect_equal(nrow(joined), nrow(test_sites))
  # every test site should land in some grid
  expect_true(all(!is.na(joined$grid_id)))
})


# Date chunker -----------------------------------------------------------------

test_that("om_build_chunks returns empty tibble when nothing is missing", {
  full <- seq.Date(as.Date("2026-01-01"), as.Date("2026-01-05"), by = 1)
  out <- om_build_chunks("2026-01-01", "2026-01-05", full)
  expect_equal(nrow(out), 0)
})

test_that("om_build_chunks returns the entire range when no existing dates", {
  out <- om_build_chunks("2026-01-01", "2026-01-05", as.Date(character(0)))
  expect_equal(nrow(out), 1)
  expect_equal(out$start_date, as.Date("2026-01-01"))
  expect_equal(out$end_date, as.Date("2026-01-05"))
  expect_equal(out$days, 5)
})

test_that("om_build_chunks splits missing dates into contiguous runs", {
  out <- om_build_chunks(
    "2026-01-01",
    "2026-01-15",
    as.Date(c(
      "2026-01-02",
      "2026-01-03",
      "2026-01-07",
      "2026-01-08",
      "2026-01-09"
    ))
  )
  expect_equal(nrow(out), 3)
  expect_equal(
    out$start_date,
    as.Date(c("2026-01-01", "2026-01-04", "2026-01-10"))
  )
  expect_equal(
    out$end_date,
    as.Date(c("2026-01-01", "2026-01-06", "2026-01-15"))
  )
  expect_equal(out$days, c(1, 3, 6))
})


# Request preparation ----------------------------------------------------------

test_that("om_prep_reqs returns one request per site when no existing grids", {
  out <- om_prep_reqs(test_sites, "2026-01-01", "2026-01-03")
  expect_equal(nrow(out), nrow(distinct(test_sites, lat, lng)))
  expect_true("req" %in% names(out))
  expect_s3_class(out$req[[1]], "httr2_request")
  expect_true(all(out$days == 3))
})

test_that("om_prep_reqs warns and returns empty when start > end", {
  expect_warning(out <- om_prep_reqs(test_sites, "2026-02-01", "2026-01-01"))
  expect_equal(nrow(out), 0)
})

test_that("om_prep_reqs produces requests for ranges extending beyond fixture", {
  grids <- om_grid_status(test_hourly_wx)
  fixture_dates <- sort(unique(test_hourly_wx$date))
  out <- om_prep_reqs(
    test_sites,
    max(fixture_dates) - 1,
    max(fixture_dates) + 5,
    grids
  )
  expect_gt(nrow(out), 0)
  expect_s3_class(out$req[[1]], "httr2_request")
})


# Merge ------------------------------------------------------------------------

test_that("om_merge_wx dedupes on (grid_id, datetime_utc) keeping first", {
  wx <- test_hourly_wx |> slice_head(n = 50)
  merged <- om_merge_wx(wx, wx)
  # full duplicate of itself collapses back to the original row count
  expect_equal(nrow(merged), nrow(wx))
  # result is sorted
  expect_equal(merged$datetime_utc, sort(merged$datetime_utc))
})

test_that("om_merge_wx drops rows with NA datetime_utc", {
  wx <- test_hourly_wx |> slice_head(n = 5)
  bad <- wx |> mutate(datetime_utc = NA)
  merged <- om_merge_wx(wx, bad)
  expect_true(all(!is.na(merged$datetime_utc)))
})
