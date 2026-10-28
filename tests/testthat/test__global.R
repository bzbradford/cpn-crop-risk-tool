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

# Independent reference: area above base and below upper under the sine curve
# through tmin and tmax, integrated numerically over one daily cycle
gdd_sine_ref <- function(tmin, tmax, base, upper = 150) {
  avg <- (tmin + tmax) / 2
  alpha <- (tmax - tmin) / 2
  f <- function(t) pmin(pmax(avg + alpha * sin(t), base), upper) - base
  integrate(
    f,
    -pi / 2,
    3 * pi / 2,
    subdivisions = 1000L,
    rel.tol = 1e-10,
    abs.tol = 1e-10
  )$value /
    (2 * pi)
}

test_that("gdd_sine matches numerical integration", {
  grid <- expand.grid(
    tmin = c(-5, 0, 5, 10, 15, 20, 30),
    tmax = c(0, 10, 12, 20, 25, 35, 45),
    base = c(0, 10),
    upper = c(30, 86)
  )
  grid <- grid[grid$tmax >= grid$tmin, ]
  expected <- mapply(gdd_sine_ref, grid$tmin, grid$tmax, grid$base, grid$upper)
  actual <- gdd_sine(grid$tmin, grid$tmax, grid$base, grid$upper)
  expect_equal(actual, expected, tolerance = 1e-6)
})

test_that("gdd_sine hand-derivable cases", {
  expect_true(is.na(gdd_sine(NA_real_, 20, 10)))
  expect_true(is.na(gdd_sine(5, NA_real_, 10)))
  # entirely below base
  expect_equal(gdd_sine(0, 5, 10), 0)
  expect_equal(gdd_sine(0, 10, 10), 0)
  # entirely between thresholds -> plain average minus base
  expect_equal(gdd_sine(15, 25, 10), 10)
  # entirely above upper -> capped at upper - base
  expect_equal(gdd_sine(30, 40, 10, upper = 25), 15)
  # constant temperature
  expect_equal(gdd_sine(20, 20, 10), 10)
  expect_equal(gdd_sine(5, 5, 10), 0)
  expect_equal(gdd_sine(200, 200, 10), 140)
  # symmetric curve straddling base (avg == base) -> alpha / pi
  expect_equal(gdd_sine(0, 20, 10), 10 / pi)
  # tmin >= base, tmax > upper must be capped (regression)
  expect_lt(gdd_sine(15, 30, 10, upper = 25), gdd_sine(15, 30, 10, upper = 150))
  # out-of-order tmin/tmax
  expect_equal(gdd_sine(25, 5, 10), gdd_sine(5, 25, 10))
})

test_that("gdd_sine properties", {
  grid <- expand.grid(tmin = -5:40, tmax = -5:40)
  grid <- grid[grid$tmax >= grid$tmin, ]
  base <- 10
  upper <- 30
  vals <- gdd_sine(grid$tmin, grid$tmax, base, upper)

  # bounded
  expect_true(all(vals >= 0 & vals <= upper - base + 1e-9))
  # vectorized, no NA introduced
  expect_length(vals, nrow(grid))
  expect_false(anyNA(vals))
  # monotonic in tmin, tmax, and upper
  expect_true(all(gdd_sine(grid$tmin + 1, grid$tmax + 1, base, upper) >= vals - 1e-9))
  expect_true(all(gdd_sine(grid$tmin, grid$tmax + 1, base, upper) >= vals - 1e-9))
  expect_true(all(gdd_sine(grid$tmin, grid$tmax, base, upper + 5) >= vals - 1e-9))
  # very high upper -> same as no cap
  expect_equal(
    gdd_sine(grid$tmin, grid$tmax, base, 1e6),
    gdd_sine(grid$tmin, grid$tmax, base)
  )
})

test_that("gdd_sine is continuous across branch boundaries", {
  eps <- 1e-8
  base <- 10
  upper <- 30
  # tmin crossing base
  expect_equal(
    gdd_sine(base - eps, 25, base, upper),
    gdd_sine(base + eps, 25, base, upper),
    tolerance = 1e-6
  )
  # tmax crossing upper
  expect_equal(
    gdd_sine(15, upper - eps, base, upper),
    gdd_sine(15, upper + eps, base, upper),
    tolerance = 1e-6
  )
  expect_equal(
    gdd_sine(5, upper - eps, base, upper),
    gdd_sine(5, upper + eps, base, upper),
    tolerance = 1e-6
  )
  # tmax crossing base
  expect_equal(
    gdd_sine(0, base - eps, base, upper),
    gdd_sine(0, base + eps, base, upper),
    tolerance = 1e-6
  )
  # tmin crossing upper
  expect_equal(
    gdd_sine(upper - eps, 40, base, upper),
    gdd_sine(upper + eps, 40, base, upper),
    tolerance = 1e-6
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
