# Color helpers ----

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

# Location helpers ----

# EPSG 4326 for use in Leaflet
service_bounds <- read_rds("data/us_ca_clip.rds")

# transform to EPSG 3857 web mercator for intersecting points
service_bounds_3857 <- st_transform(service_bounds, 3857)

#' returns TRUE if location is within service boundary shapefile
#' @param lat latitude of point
#' @param lng longitude of point
#' @returns boolean
validate_ll <- function(lat, lng) {
  mapply(
    function(lat, lng) {
      if (!is.numeric(lat) | !is.numeric(lng)) {
        return(FALSE)
      }
      pt <- st_point(c(lng, lat)) |>
        st_sfc(crs = 4326) |>
        st_transform(st_crs(service_bounds_3857))
      length(st_intersection(pt, service_bounds_3857)) == 1
    },
    lat,
    lng
  )
}

#' Creates an appropriately sized grid polygon based on centroid coordinates
#' @param lat latitude of point
#' @param lng longitude of point
#' @param d decimal degree distance from center to edge of grid
#' @returns sf object
ll_to_grid <- function(lat, lon, d = 1 / 45.5) {
  m <- list(rbind(
    c(lon - d, lat + d),
    c(lon + d, lat + d),
    c(lon + d, lat - d),
    c(lon - d, lat - d),
    c(lon - d, lat + d)
  ))
  st_sfc(st_polygon(m), crs = 4326)
}

# ll_to_grid(45, -89)

#' Creates grid polygons based on weather data grid centroid
#' @param wx IBM weather data eg `saved_weather`
#' @returns sf object of grid cell polygons
build_grids <- function(wx) {
  wx |>
    distinct(grid_id, grid_lat, grid_lng, time_zone) |>
    rowwise() |>
    mutate(geometry = ll_to_grid(grid_lat, grid_lng)) |>
    ungroup() |>
    st_set_geometry("geometry")
}

# saved_weather |> build_grids()

#' Add some more information for displaying on the map
annotate_grids <- function(grids_with_status) {
  grids_with_status |>
    mutate(
      title = if_else(
        needs_download,
        "Weather grid (download required)",
        "Weather grid"
      ),
      color = if_else(needs_download, "orange", "darkgreen"),
      label = paste0(
        "<b>",
        title,
        "</b><br>",
        "Earliest date: ",
        date_min,
        "<br>",
        "Latest date: ",
        date_max,
        "<br>",
        if_else(
          date_max == today(),
          paste0("Most recent data: ", hours_stale, " hours ago<br>"),
          ""
        ),
        "Total days: ",
        days_expected,
        "<br>",
        "Missing days: ",
        days_missing,
        sprintf(" (%.1f%%)", 100 * days_missing_pct),
        "<br>",
        "Missing hours: ",
        hours_missing,
        sprintf(" (%.1f%%)", 100 * hours_missing_pct),
        "<br>"
      ) |>
        lapply(HTML)
    )
}

# test_grids <- saved_weather |> build_grids()
# test_status <- saved_weather |> weather_status(today() - days(30), today())
# left_join(test_grids, test_status) |>
#   annotate_grids()

# UI builders ----

warning_box <- function(html) {
  div(
    class = "warning-box-container",
    div(
      style = "color: orange; padding: 10px; font-size: 1.5em;",
      icon("warning")
    ),
    div(HTML(html))
  )
}

#' Create the missing data element based on number of sites missing
missing_weather_ui <- function(n = 1) {
  str <- ifelse(n == 1, "This site is ", "One or more sites are")
  html <- paste0(
    "<i>",
    str,
    " missing data based on your date selections. Press <b>Fetch weather</b> on the sidebar to download any missing data.</i>"
  )
  warning_box(html)
}

# missing_weather_ui(1)
# missing_weather_ui(2)

site_action_link <- function(
  action = c("edit", "save", "trash"),
  site_id,
  site_name = ""
) {
  action <- match.arg(action)
  hovertext <- switch(
    action,
    edit = "Rename this site",
    save = "Pin this site to list",
    trash = "Delete this site"
  )
  onclick <- switch(
    action,
    edit = sprintf("editSite(%s, \"%s\")", site_id, site_name),
    save = sprintf("saveSite(%s)", site_id),
    trash = sprintf("trashSite(%s)", site_id)
  )
  content <- as.character(switch(
    action,
    edit = icon("pen"),
    save = icon("thumbtack"),
    trash = icon("trash")
  ))
  sprintf(
    "<a style='cursor:pointer' title='%s' onclick='%s'>%s</a>",
    hovertext,
    onclick,
    content
  )
}

# site_action_link("edit", 1, "foo")
# site_action_link("save", 1, "foo")
# site_action_link("trash", 1, "foo")

disease_modal_link <- function(disease) {
  md <- disease$doc
  name <- disease$name
  title <- paste(name, "information")
  onclick <- sprintf(
    "sendShiny('show_modal', {md: '%s', title: '%s'})",
    md,
    title
  )
  shiny::HTML(sprintf(
    "<a style='cursor:pointer' title='%s' onclick=\"%s\">More information</a>.",
    title,
    onclick
  ))
}

# disease_modal_link(diseases$white_mold)

show_modal <- function(md, title = NULL) {
  if (!file.exists(md)) {
    warning(md, " does not exist")
  }
  modalDialog(
    includeMarkdown(md),
    title = title,
    footer = modalButton("Close"),
    easyClose = TRUE
  ) |>
    showModal()
}
