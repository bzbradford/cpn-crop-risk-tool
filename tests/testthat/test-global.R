# Tests for functions in global.R

# Utility functions ------------------------------------------------------------

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


# Date/time functions ----------------------------------------------------------

test_that("hours_diff calculates difference in hours", {
  # same time should be 0

  t <- now()
  expect_equal(hours_diff(t, t), 0)

  # 6 hours difference
  expect_equal(hours_diff(now() - hours(6), now()), 6)

  # 1 day = 24 hours
  expect_equal(hours_diff(now() - days(1), now()), 24)
})

test_that("get_yday converts partial dates to day of year", {
  result <- get_yday("jun 1", "aug 2")
  expect_equal(length(result), 2)
  expect_equal(result[[1]], yday(ymd(paste(year(Sys.Date()), "jun 1"))))
  expect_equal(result[[2]], yday(ymd(paste(year(Sys.Date()), "aug 2"))))

  # jun 1 should be around day 152 (varies slightly in leap years)
  expect_true(result[[1]] >= 152 && result[[1]] <= 153)
})

test_that("check_date_overlap detects overlapping date ranges", {
  # overlap: Apr-Jul with May-Aug

  result1 <- check_date_overlap(c("2025-4-1", "2025-7-1"), c("May 1", "Aug 1"))
  expect_true(result1[["2025"]])

  # no overlap: Apr-Jul with Jan-Feb
  result2 <- check_date_overlap(c("2025-4-1", "2025-7-1"), c("Jan 1", "Feb 1"))
  expect_false(result2[["2025"]])

  # cross-year range
  result3 <- check_date_overlap(c("2024-10-1", "2025-7-1"), c("Jun 1", "Aug 1"))
  expect_true(result3[["2025"]])
})


# Summary functions ------------------------------------------------------------

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


# Unit conversions -------------------------------------------------------------

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


# Unit lookup functions --------------------------------------------------------

test_that("find_unit returns correct units", {
  expect_equal(find_unit("temperature", "metric"), "°C")
  expect_equal(find_unit("temperature", "imperial"), "°F")
  expect_equal(find_unit("temperature_mean", "metric"), "°C")
  expect_equal(find_unit("precip", "metric"), "mm")
  expect_equal(find_unit("precip", "imperial"), "in")
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
    test_hourly_wx |>
      build_daily() |>
      rename_with_units()
  })
})


# Color helpers ----------------------------------------------------------------

## hex_to_rgb() ----

test_that("hex_to_rgb converts 6-digit hex colors correctly", {
  expect_equal(hex_to_rgb("#FF0000"), c(255, 0, 0))
  expect_equal(hex_to_rgb("#00FF00"), c(0, 255, 0))
  expect_equal(hex_to_rgb("#0000FF"), c(0, 0, 255))
  expect_equal(hex_to_rgb("#FFFFFF"), c(255, 255, 255))
  expect_equal(hex_to_rgb("#000000"), c(0, 0, 0))
})

test_that("hex_to_rgb handles hex without hash", {
  expect_equal(hex_to_rgb("FF0000"), c(255, 0, 0))
  expect_equal(hex_to_rgb("808080"), c(128, 128, 128))
})

test_that("hex_to_rgb expands 3-digit shorthand hex correctly", {
  expect_equal(hex_to_rgb("#F00"), c(255, 0, 0))
  expect_equal(hex_to_rgb("#0F0"), c(0, 255, 0))
  expect_equal(hex_to_rgb("#00F"), c(0, 0, 255))
  expect_equal(hex_to_rgb("#FFF"), c(255, 255, 255))
  expect_equal(hex_to_rgb("#000"), c(0, 0, 0))
  expect_equal(hex_to_rgb("#ABC"), c(170, 187, 204))
})

test_that("hex_to_rgb handles lowercase hex values", {
  expect_equal(hex_to_rgb("#ff0000"), c(255, 0, 0))
  expect_equal(hex_to_rgb("#aabbcc"), c(170, 187, 204))
})

test_that("hex_to_rgb handles mixed case hex values", {
  expect_equal(hex_to_rgb("#FfAa00"), c(255, 170, 0))
})


## color_distance() ----

test_that("color_distance returns 0 for identical colors", {
  expect_equal(color_distance(c(255, 0, 0), c(255, 0, 0)), 0)
  expect_equal(color_distance(c(0, 0, 0), c(0, 0, 0)), 0)
  expect_equal(color_distance(c(128, 64, 32), c(128, 64, 32)), 0)
})

test_that("color_distance calculates Euclidean distance correctly", {
  # Distance between black and white
  expect_equal(
    color_distance(c(0, 0, 0), c(255, 255, 255)),
    sqrt(255^2 + 255^2 + 255^2)
  )

  # Distance along single axis
  expect_equal(color_distance(c(0, 0, 0), c(100, 0, 0)), 100)
  expect_equal(color_distance(c(0, 0, 0), c(0, 100, 0)), 100)
  expect_equal(color_distance(c(0, 0, 0), c(0, 0, 100)), 100)
})

test_that("color_distance is symmetric", {
  rgb1 <- c(100, 150, 200)
  rgb2 <- c(50, 75, 100)
  expect_equal(color_distance(rgb1, rgb2), color_distance(rgb2, rgb1))
})

test_that("color_distance handles edge cases", {
  # Maximum possible distance (black to white)
  max_dist <- color_distance(c(0, 0, 0), c(255, 255, 255))
  expect_equal(max_dist, sqrt(3 * 255^2))

  # Small differences
  expect_equal(color_distance(c(100, 100, 100), c(101, 100, 100)), 1)
})


## get_luminance() ----

test_that("get_luminance returns 0 for black", {
  expect_equal(get_luminance(c(0, 0, 0)), 0)
})

test_that("get_luminance returns 1 for white", {
  expect_equal(get_luminance(c(255, 255, 255)), 1)
})

test_that("get_luminance is higher for green than red or blue", {
  # Green has the highest weight (0.7152) in the luminance formula
  red_lum <- get_luminance(c(255, 0, 0))
  green_lum <- get_luminance(c(0, 255, 0))
  blue_lum <- get_luminance(c(0, 0, 255))

  expect_gt(green_lum, red_lum)
  expect_gt(green_lum, blue_lum)
})

test_that("get_luminance returns values between 0 and 1", {
  test_colors <- list(
    c(128, 128, 128),
    c(255, 0, 0),
    c(0, 255, 0),
    c(0, 0, 255),
    c(100, 150, 200)
  )

  for (rgb in test_colors) {
    lum <- get_luminance(rgb)
    expect_gte(lum, 0)
    expect_lte(lum, 1)
  }
})

test_that("get_luminance handles the sRGB linear threshold correctly", {
  # Values below ~10.3 (0.03928 * 255) use linear formula
  low_rgb <- c(10, 10, 10)
  lum_low <- get_luminance(low_rgb)
  expect_type(lum_low, "double")
  expect_gte(lum_low, 0)
})


## Tests for get_text_color() ----

test_that("get_text_color returns white for dark backgrounds", {
  expect_equal(get_text_color(0), "#fff")
  expect_equal(get_text_color(0.1), "#fff")
  expect_equal(get_text_color(0.49), "#fff")
})

test_that("get_text_color returns black for light backgrounds", {
  expect_equal(get_text_color(0.5), "#000")
  expect_equal(get_text_color(0.75), "#000")
  expect_equal(get_text_color(1), "#000")
})

test_that("get_text_color uses 0.5 as threshold", {
  expect_equal(get_text_color(0.4999), "#fff")
  expect_equal(get_text_color(0.5), "#000")
})


## find_closest_css_color() ----

test_that("find_closest_css_color returns exact matches", {
  result <- find_closest_css_color("#FF0000")
  expect_equal(result$css_color, "red")
  expect_equal(result$distance, 0)

  result <- find_closest_css_color("#008000")
  expect_equal(result$css_color, "green")
  expect_equal(result$distance, 0)

  result <- find_closest_css_color("#0000FF")
  expect_equal(result$css_color, "blue")
  expect_equal(result$distance, 0)
})

test_that("find_closest_css_color returns correct structure", {
  result <- find_closest_css_color("#FF5733")

  expect_type(result, "list")
  expect_named(
    result,
    c("input_hex", "css_color", "css_hex_value", "distance", "text_color")
  )
  expect_type(result$input_hex, "character")
  expect_type(result$css_color, "character")
  expect_type(result$css_hex_value, "character")
  expect_type(result$distance, "double")
  expect_type(result$text_color, "character")
})

test_that("find_closest_css_color handles various input formats", {
  # With hash
  result1 <- find_closest_css_color("#FF0000")
  expect_equal(result1$css_color, "red")

  # Without hash
  result2 <- find_closest_css_color("FF0000")
  expect_equal(result2$css_color, "red")

  # Lowercase
  result3 <- find_closest_css_color("#ff0000")
  expect_equal(result3$css_color, "red")

  # 3-digit shorthand
  result4 <- find_closest_css_color("#F00")
  expect_equal(result4$css_color, "red")
})

test_that("find_closest_css_color normalizes input_hex to uppercase", {
  result <- find_closest_css_color("#aabbcc")
  expect_equal(result$input_hex, "#AABBCC")
})

test_that("find_closest_css_color finds appropriate closest colors", {
  # Near-red should match red or darkred
  result <- find_closest_css_color("#FF3333")
  expect_true(result$css_color %in% c("red", "darkred", "orange"))

  # Dark blue-ish should match darkblue or blue
  result <- find_closest_css_color("#000080")
  expect_true(result$css_color %in% c("blue", "darkblue"))

  # Light gray should match lightgray or gray or white
  result <- find_closest_css_color("#CCCCCC")
  expect_true(result$css_color %in% c("lightgray", "gray", "white"))
})

test_that("find_closest_css_color returns appropriate text colors", {
  # Dark colors should get white text
  result_dark <- find_closest_css_color("#000000")
  expect_equal(result_dark$text_color, "#fff")

  result_darkblue <- find_closest_css_color("#00008B")
  expect_equal(result_darkblue$text_color, "#fff")

  # Light colors should get black text
  result_white <- find_closest_css_color("#FFFFFF")
  expect_equal(result_white$text_color, "#000")

  result_lightgray <- find_closest_css_color("#D3D3D3")
  expect_equal(result_lightgray$text_color, "#000")
})

test_that("find_closest_css_color warns on invalid hex format", {
  expect_warning(find_closest_css_color("invalid"), "Invalid hex color format")
  expect_warning(find_closest_css_color("#GGG"), "Invalid hex color format")
  expect_warning(find_closest_css_color("#12345"), "Invalid hex color format")
})

test_that("find_closest_css_color distance is non-negative", {
  test_colors <- c("#123456", "#ABCDEF", "#FED321", "#010101")

  for (color in test_colors) {
    result <- find_closest_css_color(color)
    expect_gte(result$distance, 0)
  }
})

test_that("find_closest_css_color css_hex_value matches css_colors list", {
  result <- find_closest_css_color("#FF6B6B")
  expect_equal(result$css_hex_value, css_colors[[result$css_color]])
})


# Location helpers -------------------------------------------------------------

test_that("validate_ll works", {
  validate_ll(45, -89) |> expect_true()
  validate_ll(0, 0) |> expect_false()
})

test_that("parse_coords works", {
  # parses valid coordinates
  result <- parse_coords("45, -89")
  expect_equal(result$lat, 45)
  expect_equal(result$lng, -89)

  # with spaces
  result2 <- parse_coords("45.5  -89.5")
  expect_equal(result2$lat, 45.5)
  expect_equal(result2$lng, -89.5)

  # errors on invalid inputs
  expect_error(parse_coords("foo"), "Invalid coordinate format")
  expect_error(parse_coords("45"), "Invalid coordinate format")
  expect_error(parse_coords("abc, def"), "Failed to parse coordinates")
})

test_that("ll_to_grid works", {
  g <- ll_to_grid(45, -89)
  expect_length(g, 1)
  expect_s3_class(g, "sfc_POLYGON")
})

test_that("build_grids works", {
  wx <- test_hourly_wx
  g <- wx |> build_grids()

  # should have 1 row per grid
  expect_equal(length(unique(wx$grid_id)), nrow(g))

  # should be sf
  expect_s3_class(g, "sf")
})

test_that("annotate_grids works", {
  expect_silent({
    grids <- test_hourly_wx |> build_grids()
    status <- test_hourly_wx |> weather_status()

    left_join(grids, status, join_by(grid_id)) |>
      annotate_grids() |>
      pull(label)
  })
})


# Site constructor -------------------------------------------------------------

test_that("Site creates site object", {
  site <- Site("foo", 1, 2)
  expect_equal(site$name, "foo")
  expect_equal(site$lat, 1)
  expect_equal(site$lng, 2)
  expect_equal(site$id, 999)

  site2 <- Site("bar", 3, 4, id = 1)
  expect_equal(site2$id, 1)
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


# Model definitions ----------------------------------------------------------

example_doc <- "example.md"

test_that("model_list list is properly structured", {
  expect_true(length(model_list) > 0)
  for (model in model_list) {
    expect_named(
      model,
      c("name", "crop", "model_name", "info", "doc", "risk_period", "slug")
    )
  }
})

test_that("Model creates valid model config object", {
  m <- Model(
    name = "Test Model",
    crop = "Any",
    info = "Test info",
    doc = example_doc
  )
  expect_equal(m$name, "Test Model")
  expect_equal(m$crop, "Any")
  expect_equal(m$info, "Test info")
  expect_equal(m$doc, example_doc)
  expect_null(m$risk_period)
})

test_that("Model validates risk_period dates", {
  m <- Model(
    name = "Test",
    info = "Test",
    doc = example_doc,
    risk_period = c("Jul 1", "Aug 15")
  )
  expect_equal(m$risk_period, c("Jul 1", "Aug 15"))
})

test_that("Model errors on invalid doc file", {
  expect_error(
    Model(name = "Test", info = "Test", doc = "nonexistent.md"),
    "Missing doc file"
  )
})

test_that("Model errors on invalid parameters", {
  expect_error(
    Model(
      name = "Test",
      info = "Test",
      doc = example_doc,
      invalid_param = TRUE
    ),
    "Invalid model config: invalid_param"
  )
})
