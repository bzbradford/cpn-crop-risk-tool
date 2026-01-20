#' Find the closest CSS color name for a given hex color
#' @param hex_color A hex color string (e.g., "#FF5733")
#' @returns A list containing the closest CSS color name, hex value, and contrast text color
find_closest_css_color <- function(hex_color) {
  # Define CSS named colors with their hex values
  css_colors <- list(
    "red" = "#FF0000",
    "darkred" = "#8B0000",
    "lightred" = "#FFB6C1", # Using light pink as proxy
    "orange" = "#FFA500",
    "beige" = "#F5F5DC",
    "green" = "#008000",
    "darkgreen" = "#006400",
    "lightgreen" = "#90EE90",
    "blue" = "#0000FF",
    "darkblue" = "#00008B",
    "lightblue" = "#ADD8E6",
    "purple" = "#800080",
    "darkpurple" = "#483D8B", # Using dark slate blue as proxy
    "pink" = "#FFC0CB",
    "cadetblue" = "#5F9EA0",
    "white" = "#FFFFFF",
    "gray" = "#808080",
    "lightgray" = "#D3D3D3",
    "black" = "#000000"
  )

  # Function to convert hex to RGB
  hex_to_rgb <- function(hex) {
    hex <- gsub("#", "", hex)
    if (nchar(hex) == 3) {
      hex <- paste0(
        substr(hex, 1, 1),
        substr(hex, 1, 1),
        substr(hex, 2, 2),
        substr(hex, 2, 2),
        substr(hex, 3, 3),
        substr(hex, 3, 3)
      )
    }
    r <- as.numeric(paste0("0x", substr(hex, 1, 2)))
    g <- as.numeric(paste0("0x", substr(hex, 3, 4)))
    b <- as.numeric(paste0("0x", substr(hex, 5, 6)))
    c(r, g, b)
  }

  # Function to calculate Euclidean distance in RGB space
  color_distance <- function(rgb1, rgb2) {
    sqrt(sum((rgb1 - rgb2)^2))
  }

  # Function to calculate luminance for contrast ratio
  get_luminance <- function(rgb) {
    # Convert RGB to relative luminance
    rgb_norm <- rgb / 255
    rgb_linear <- ifelse(
      rgb_norm <= 0.03928,
      rgb_norm / 12.92,
      ((rgb_norm + 0.055) / 1.055)^2.4
    )
    luminance <- 0.2126 *
      rgb_linear[1] +
      0.7152 * rgb_linear[2] +
      0.0722 * rgb_linear[3]
    luminance
  }

  # Function to determine text color based on contrast
  get_text_color <- function(bg_luminance) {
    # Use a luminance threshold of 0.5 for better visual results
    # Colors darker than this threshold get white text, lighter colors get black text
    if (bg_luminance < 0.5) "#fff" else "#000"
  }

  # Validate and clean input hex color
  hex_color <- toupper(gsub("#", "", hex_color))
  if (!grepl("^[0-9A-F]{3}$|^[0-9A-F]{6}$", hex_color)) {
    warning(sprintf(
      "Invalid hex color format '%s'. Use format like '#FF0000' or '#F00'",
      hex_color
    ))
  }

  # Convert input hex to RGB
  input_rgb <- hex_to_rgb(paste0("#", hex_color))

  # Find closest color
  min_distance <- Inf
  closest_color <- NULL

  for (color_name in names(css_colors)) {
    css_rgb <- hex_to_rgb(css_colors[[color_name]])
    distance <- color_distance(input_rgb, css_rgb)

    if (distance < min_distance) {
      min_distance <- distance
      closest_color <- color_name
    }
  }

  # Calculate luminance of the input color for text contrast
  input_luminance <- get_luminance(input_rgb)
  text_color <- get_text_color(input_luminance)

  # Return results
  list(
    input_hex = paste0("#", hex_color),
    css_color = closest_color,
    css_hex_value = css_colors[[closest_color]],
    distance = round(min_distance, 2),
    text_color = text_color
  )
}

# Example usage:
# result <- find_closest_css_color("#FF6B6B")
# print(result)

# Test with a few examples
# cat("Testing the function:\n\n")
#
# test_colors <- c("#FF6B6B", "#2E8B57", "#4169E1", "#FFD700", "#8A2BE2")
# for (color in test_colors) {
#   result <- find_closest_css_color(color)
#   cat(sprintf("Input: %s -> Closest: %s (%s), Text: %s\n",
#     result$input_hex,
#     result$closest_css_color,
#     result$css_hex_value,
#     result$text_color))
# }
