# Tests for functions in global.R

# Utility functions ----

test_that("invert swaps names and values", {
  x <- c(a = "1", b = "2", c = "3")
  result <- invert(x)
  expect_equal(names(result), c("1", "2", "3"))
  expect_equal(as.character(result), c("a", "b", "c"))
})

test_that("build_choices creates named list from sublist elements", {
  obj <- list(
    item1 = list(name = "First", value = "v1"),
    item2 = list(name = "Second", value = "v2")
  )
  result <- build_choices(obj, "name", "value")
  expect_equal(names(result), c("First", "Second"))
  expect_equal(as.character(result), c("v1", "v2"))
})

test_that("first_truthy returns first truthy argument", {
  expect_equal(first_truthy(NULL, FALSE, "hello", "world"), "hello")
  expect_equal(first_truthy(1, 2, 3), 1)
  expect_equal(first_truthy(NULL, NULL), NULL)
  expect_equal(first_truthy("", "foo"), "foo")
  expect_equal(first_truthy(0, 1), 0)
})

test_that("clamp restricts value to between two extremes", {
  expect_equal(clamp(5, 0, 10), 5)
  expect_equal(clamp(-5, 0, 10), 0)
  expect_equal(clamp(15, 0, 10), 10)
  expect_equal(clamp(c(-1, 5, 15), 0, 10), c(0, 5, 10))
})


# Date/time functions ----

test_that("hours_diff calculates difference in hours", {
  t <- now()
  expect_equal(hours_diff(t, t), 0)
  expect_equal(hours_diff(now() - hours(6), now()), 6)
  expect_equal(hours_diff(now() - days(1), now()), 24)
})


# Tibble helpers ----

test_that("add_cumsum adds a cumulative sum column", {
  expect_silent({
    test_hourly_wx |>
      add_cumsum("precipitation") |>
      pull(precipitation_cumulative)
  })
})

test_that("add_date_cols builds additional cols from date and datetime", {
  expect_silent({
    tibble(
      grid_id = "foo",
      datetime_local = now() - days(0:10),
      date = as_date(datetime_local)
    ) |>
      add_date_cols()
  })
})


# Summary functions ----

test_that("calc_sum handles NA values correctly", {
  expect_equal(calc_sum(c(1, 2, 3)), 6)
  expect_equal(calc_sum(c(1, 2, NA)), 3)
  expect_true(is.na(calc_sum(c(NA, NA, NA))))
})

test_that("calc_min handles NA values correctly", {
  expect_equal(calc_min(c(1, 2, 3)), 1)
  expect_equal(calc_min(c(5, 2, NA)), 2)
  expect_true(is.na(calc_min(c(NA, NA, NA))))
})

test_that("calc_mean handles NA values correctly", {
  expect_equal(calc_mean(c(1, 2, 3)), 2)
  expect_equal(calc_mean(c(1, 2, NA)), 1.5)
  expect_true(is.na(calc_mean(c(NA, NA, NA))))
})

test_that("calc_max handles NA values correctly", {
  expect_equal(calc_max(c(1, 2, 3)), 3)
  expect_equal(calc_max(c(5, 2, NA)), 5)
  expect_true(is.na(calc_max(c(NA, NA, NA))))
})

test_that("roll_mean calculates rolling mean", {
  vec <- c(1, 2, 3, 4, 5)
  result <- roll_mean(vec, 3)
  expect_equal(length(result), 5)
  # first two values are partial windows
  expect_equal(result[3], 2) # mean(1,2,3)
  expect_equal(result[4], 3) # mean(2,3,4)
  expect_equal(result[5], 4) # mean(3,4,5)
})

test_that("roll_sum calculates rolling sum", {
  vec <- c(1, 2, 3, 4, 5)
  result <- roll_sum(vec, 3)
  expect_equal(length(result), 5)
  expect_equal(result[3], 6) # sum(1,2,3)
  expect_equal(result[4], 9) # sum(2,3,4)
  expect_equal(result[5], 12) # sum(3,4,5)
})

test_that("roll_mean handles NA values", {
  vec <- c(1, NA, 3, 4, 5)
  result <- roll_mean(vec, 3)
  expect_equal(result[3], 2) # mean(1,3) ignoring NA
})


# Unit conversions ----

test_that("temperature conversions are correct", {
  # freezing point
  expect_equal(c_to_f(0), 32)
  expect_equal(f_to_c(32), 0)

  # boiling point
  expect_equal(c_to_f(100), 212)
  expect_equal(f_to_c(212), 100)

  # round trip
  expect_equal(f_to_c(c_to_f(25)), 25)
})

test_that("length conversions are correct", {
  # mm to inches
  expect_equal(mm_to_in(25.4), 1)
  expect_equal(mm_to_in(50.8), 2)

  # cm to inches
  expect_equal(cm_to_in(2.54), 1)
  expect_equal(cm_to_in(5.08), 2)
})

test_that("distance conversions are correct", {
  # miles to km (approximately)
  expect_equal(mi_to_km(1), 1.609)
  expect_equal(round(km_to_mi(1.609), 3), 1)
})

test_that("speed conversions are correct", {
  # km/h to m/s
  expect_equal(kmh_to_mps(3.6), 1)
  expect_equal(kmh_to_mps(36), 10)

  # m/s to mph
  expect_equal(round(mps_to_mph(1), 3), 2.237)
})

test_that("pressure conversion is correct", {
  # standard atmospheric pressure
  expect_equal(round(kPa_to_inHg(1013.25), 1), 299.2)
  expect_equal(round(mbar_to_inHg(1013.25), 2), 29.92)
})

test_that("wind_dir_to_deg converts compass directions", {
  expect_equal(wind_dir_to_deg("N"), 0)
  expect_equal(wind_dir_to_deg("E"), 90)
  expect_equal(wind_dir_to_deg("S"), 180)
  expect_equal(wind_dir_to_deg("W"), 270)
  expect_equal(wind_dir_to_deg("NE"), 45)
  expect_equal(wind_dir_to_deg("SW"), 225)
  expect_true(is.na(wind_dir_to_deg("invalid")))
})


# Unit lookup functions ----

test_that("find_unit returns correct units", {
  expect_equal(find_unit("temperature", "metric"), "°C")
  expect_equal(find_unit("temperature", "imperial"), "°F")
  expect_equal(find_unit("temperature_mean", "metric"), "°C")
  expect_equal(find_unit("precipitation", "metric"), "mm")
  expect_equal(find_unit("precipitation", "imperial"), "in")
  expect_equal(find_unit("relative_humidity", "metric"), "%")
  expect_equal(find_unit("unknown_column", "metric"), "")
})

test_that("rename_with_units works", {
  tibble(temperature = 1) |>
    rename_with_units("metric") |>
    expect_named("temperature_c")

  tibble(temperature = 1) |>
    rename_with_units("imperial") |>
    expect_named("temperature_f")

  expect_silent({
    test_hourly_wx |>
      rename_with_units()
  })

  expect_silent({
    test_daily_wx |>
      rename_with_units()
  })
})


# Growing degree days ----

test_that("gdd_sine", {
  expect_silent({
    expand_grid(tmin = 0:30, tmax = 0:30) |>
      filter(tmax >= tmin) |>
      mutate(gdd = gdd_sine(tmin, tmax, 10)) |>
      ggplot(aes(x = tmin, y = tmax, fill = gdd)) +
      geom_tile() +
      scale_fill_viridis_c() +
      coord_cartesian(expand = F)
  })
})

test_that("gdd_sine numerical parity — all 6 branches", {
  # NA propagation
  expect_true(is.na(gdd_sine(NA_real_, 20, 10)))
  # tmax <= base → 0
  expect_equal(gdd_sine(0, 5, 10), 0)
  # tmin >= base → simple average
  expect_equal(gdd_sine(15, 25, 10), 10)
  # tmin < base, tmax <= upper → sine formula
  expect_equal(gdd_sine(5, 25, 10), 6.089978, tolerance = 1e-6)
  # tmax > upper, tmin >= base → upper-clamp formula
  expect_equal(gdd_sine(15, 200, 10), 97.5, tolerance = 1e-6)
  # tmin < base, tmax > upper → both-threshold formula
  expect_equal(gdd_sine(5, 200, 10), 81.793794, tolerance = 1e-6)
  # custom upper threshold
  expect_equal(gdd_sine(5, 200, 10, upper = 100), 60.545264, tolerance = 1e-6)
  # out-of-order tmin/tmax → same result as corrected order
  expect_equal(gdd_sine(25, 5, 10), gdd_sine(5, 25, 10))
})

test_that("gdd_sine grid parity (base=10)", {
  grid <- expand.grid(tmin = 0:50, tmax = 0:50)
  grid <- grid[grid$tmax >= grid$tmin, ]
  vals <- gdd_sine(grid$tmin, grid$tmax, 10)
  expect_equal(length(vals), 1326)
  expect_equal(sum(vals, na.rm = TRUE), 20743.31, tolerance = 1e-2)
  expect_equal(
    vals[c(100, 200, 300, 400, 500, 700, 900, 1100)],
    c(1.061744, 4.135599, 13, 14, 8.525969, 24.5, 29.5, 22),
    tolerance = 1e-5
  )
})


# Location helpers ----

test_that("validate_ll works", {
  expect_true(validate_ll(45, -89))
  expect_false(validate_ll(300, 400))
})


# UI builders ----

test_that("site_action_link works", {
  expect_silent({
    site_action_link("edit", 1, "foo")
    site_action_link("save", 1, "foo")
    site_action_link("trash", 1, "foo")
    site_action_link("hide", 1)
    site_action_link("show", 1)
  })
})

test_that("build_date_btn works", {
  x <- build_date_btn("past_week", "Past week", "default")
  expect_s3_class(x, "html")
})

test_that("build_modal_link works", {
  x <- build_modal_link(model_list$whitemold)
  expect_s3_class(x, "html")
})

test_that("build_warning_box works", {
  x <- build_warning_box("alert")
  expect_s3_class(x, "shiny.tag")
})


# Site constructor ----

test_that("Site creates site object", {
  site <- Site(id = 1, name = "foo", lat = 1, lng = 2)
  expect_equal(site$id, 1)
  expect_equal(site$name, "foo")
  expect_equal(site$lat, 1)
  expect_equal(site$lng, 2)
  expect_equal(site$hidden, FALSE)
})

test_that("sanitize_loc_names handles duplicates", {
  result <- sanitize_loc_names(c("foo", "foo", "bar"))
  expect_equal(result, c("foo", "foo (2)", "bar"))
})

test_that("sanitize_loc_names strips HTML", {
  result <- sanitize_loc_names(c("foo", "bar", "<a href='bad'>baz</a>"))
  expect_equal(result, c("foo", "bar", "baz"))
})

test_that("sanitize_loc_names truncates long names", {
  long_name <- paste(rep("a", 50), collapse = "")
  result <- sanitize_loc_names(long_name)
  expect_lte(nchar(result), 30)
})

test_that("load_sites loads valid CSV", {
  result <- load_sites("example-sites.csv")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "name", "lat", "lng") %in% names(result)))
  expect_gt(nrow(result), 0)
})


# Cookie helpers ----

test_that("parse_cookie_sites returns NULL for empty/missing input", {
  expect_null(parse_cookie_sites(list()))
  expect_null(parse_cookie_sites(NULL))
})

test_that("parse_cookie_sites parses valid sites", {
  cookie_sites <- list(
    list(id = 1, name = "Site A", lat = 45, lng = -89),
    list(id = 2, name = "Site B", lat = 44, lng = -88)
  )
  result <- parse_cookie_sites(cookie_sites)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("id", "name", "lat", "lng") %in% names(result)))
})

test_that("parse_cookie_sites filters out invalid coordinates", {
  cookie_sites <- list(
    list(id = 1, name = "Valid", lat = 45, lng = -89),
    list(id = 2, name = "Invalid", lat = 999, lng = 999)
  )
  result <- parse_cookie_sites(cookie_sites)
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "Valid")
})

test_that("parse_cookie_sites reassigns sequential IDs", {
  cookie_sites <- list(
    list(id = 5, name = "A", lat = 45, lng = -89),
    list(id = 10, name = "B", lat = 44, lng = -88)
  )
  result <- parse_cookie_sites(cookie_sites)
  expect_equal(result$id, 1:2)
})

test_that("parse_cookie_sites returns NULL when columns are missing", {
  expect_null(parse_cookie_sites(list(list(bad_col = "data"))))
})


# Cache cleaner ----

test_that("get_cache_file returns correct path", {
  path <- get_cache_file("abc123")
  expect_equal(path, file.path("cache", "abc123.fst"))
})

test_that("get_cache_file returns NULL for empty/missing user_id", {
  expect_null(get_cache_file(NULL))
  expect_null(get_cache_file(""))
})
