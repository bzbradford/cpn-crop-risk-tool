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

test_that("annotate_grids works", {
  expect_silent({
    grids <- test_hourly_wx |> om_build_wx_grids()
    status <- test_hourly_wx |> om_wx_status()

    left_join(grids, status, join_by(grid_id)) |>
      annotate_grids() |>
      pull(label)
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
