#--- MAP ---#

# Local functions --------------------------------------------------------------

#' parse lat/lng coordinates from string
#' @param str input string containing coordinates to parse in form "lat, lng"
#' @returns named list { lat: numeric, lng: numeric }
parse_coords <- function(str) {
  str <- gsub("[ ,\t°NW]", " ", str)
  parts <- str_split_1(str_squish(str), " ")
  if (length(parts) != 2) {
    stop("Invalid coordinate format.")
  }
  coords <- suppressWarnings(list(
    lat = as.numeric(parts[1]),
    lng = as.numeric(parts[2])
  ))
  if (any(sapply(coords, is.na))) {
    stop("Failed to parse coordinates.")
  }
  coords
}

if (FALSE) {
  parse_coords("45, -89")
  parse_coords("foo")
}


#' Add some more information for displaying on the map
#' @param grids_with_status:tibble constructed by `om_grid_status()`
#' @returns tibble with additional columns
annotate_grids <- function(grids_with_status) {
  grids_with_status |>
    mutate(
      color = if_else(needs_download, "orange", "darkgreen"),
      label = paste0(
        "<b>Weather grid</b><br>",
        sprintf("Centroid: %.4f, %.4f<br>", grid_lat, grid_lng),
        # sprintf("Earliest date: %s<br>", date_min),
        # sprintf("Latest date: %s<br>", date_max),
        if_else(
          date_max == today(),
          sprintf("Most recent data: %s hours ago<br>", hours_stale),
          ""
        ),
        sprintf("Total days: %s<br>", days_expected),
        if_else(
          days_incomplete > 0,
          sprintf(
            "Days incomplete: %s (%.1f%%)<br>",
            days_incomplete,
            100 * (days_incomplete / days_expected)
          ),
          ""
        ),
        if_else(
          days_missing > 0,
          sprintf(
            "Days missing: %s (%.1f%%)<br>",
            days_missing,
            100 * (days_missing / days_expected)
          ),
          ""
        ),
        if_else(
          hours_missing > 0,
          sprintf(
            "Hours missing: %s (%.1f%%)<br>",
            hours_missing,
            100 * (hours_missing / hours_expected)
          ),
          ""
        )
        # lapply(dates_missing, function(dt) {
        #   if (length(dt) == 0) {
        #     return(NULL)
        #   }
        #   print(dt)
        #   sprintf(
        #     "Dates missing: %s",
        #     paste(dt, collapse = ", ") |>
        #       str_trunc(40)
        #   )
        # })
      ) |>
        lapply(HTML)
    )
}

if (FALSE) {
  om_grid_status(wx, today() - days(7)) |>
    annotate_grids() |>
    leaflet() |>
    addTiles() |>
    addPolygons(label = ~label)
}


## Color helpers ----

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

#' Find the closest CSS color name for a given hex color
#' @param hex_color A hex color string (e.g., "#FF5733")
#' @returns A list containing the closest CSS color name, hex value, and contrast text color
find_closest_css_color <- function(hex_color) {
  # Validate and clean input hex color
  hex_color <- toupper(gsub("#", "", hex_color))
  if (!grepl("^[0-9A-F]{3}$|^[0-9A-F]{6}$", hex_color)) {
    warning(sprintf(
      "Invalid hex color format '%s'. Use format like '#FF0000' or '#F00'",
      hex_color
    ))
    return(list())
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


# Static UI --------------------------------------------------------------------

mapUI <- function() {
  ns <- NS("map")

  tagList(
    div(
      class = "map-container",
      div(class = "map-title-container", uiOutput(ns("map_title"))),
      leafletOutput(ns("map"), height = "100%"),
      div(
        class = "search-overlay",
        uiOutput(ns("searchbox_ui")),
        uiOutput(ns("coord_search_ui"))
      )
    )
  )
}


# Module server ----------------------------------------------------------------

mapServer <- function(rv, map_data) {
  moduleServer(
    id = "map",
    function(input, output, session) {
      ns <- session$ns

      # Local reactives ----

      ## visible_sites ----
      # rv$sites filtered to non-hidden; always a tibble (possibly empty)
      visible_sites <- reactive({
        sites <- rv$sites
        if (is.null(sites) || nrow(sites) == 0) {
          return(sites_template)
        }
        sites |> filter(!hidden)
      })

      ## visible_sites_with_status ----
      # map_data()$sites_with_status filtered to non-hidden; NULL if unavailable
      visible_sites_with_status <- reactive({
        sites <- map_data()$sites_with_status
        if (is.null(sites) || nrow(sites) == 0) {
          return(NULL)
        }
        sites |> filter(!hidden)
      })

      # Helper functions ----

      proxy_map <- leafletProxy(ns("map"))

      # wrapper for leaflet flyTo
      fly_to <- function(loc) {
        proxy_map |>
          flyTo(loc$lng, loc$lat, max(10, isolate(input$map_zoom)))
      }

      #' wrapper for leaflet fitBounds
      #' @param map leaflet proxy object
      #' @param bounds named list { lat1, lat2, lng1, lng2 }
      #' @param options leaflet zoom/pan options
      fit_bounds <- function(map = proxy_map, bounds, options = NULL) {
        args <- as.list(bounds)
        args$map <- map
        args$options <- options
        do.call(fitBounds, args)
      }

      # fits all sites on the map
      fit_sites <- function() {
        sites <- visible_sites()
        req(nrow(sites) > 0)

        bounds <- list(
          lat1 = min(sites$lat),
          lat2 = max(sites$lat),
          lng1 = min(sites$lng),
          lng2 = max(sites$lng)
        )

        fit_bounds(
          bounds = bounds,
          options = list(padding = c(100, 100), maxZoom = 14)
        )
      }

      # validates a potential new site then offers a popup to rename and finalize
      save_site <- function(site) {
        sites <- rv$sites

        if (nrow(sites) == OPTS$max_sites) {
          return()
        }

        # if (!validate_ll(site$lat, site$lng)) {
        #   show_toast(
        #     "Invalid location",
        #     text = sprintf(
        #       "The location %s, %s is not valid or is outside of our service area.",
        #       site$lat,
        #       site$lng
        #     ),
        #     position = "center"
        #   )
        #   req(FALSE)
        # }

        # if several sites already, confirm each new map click
        # value may be FALSE if cancelled or string (name of site from modal)
        finalize <- function(value) {
          req(value)

          value <- sanitize_loc_names(value)
          req(value)

          site$name <- value
          sites <- sites |>
            bind_rows(as_tibble(site)) |>
            distinct(lat, lng, .keep_all = TRUE) |>
            mutate(id = row_number())

          rv$sites <- sites
          rv$selected_site <- last(sites$id)
        }

        shinyalert(
          text = sprintf("Add new site at %.2f, %.2f?", site$lat, site$lng),
          type = "input",
          inputType = "text",
          inputValue = site$name,
          closeOnClickOutside = FALSE,
          showCancelButton = TRUE,
          confirmButtonText = "Save",
          confirmButtonCol = "#008bb6",
          cancelButtonText = "Cancel",
          callbackR = finalize
        )
      }

      # calls google geocoding api to get locality name
      get_loc_name <- function(lat, lng, name) {
        cmd <- sprintf(
          "getLocalityName(%s, %s, '%s', '%s')",
          lat,
          lng,
          name,
          OPTS$google_geocoding_key
        )
        runjs(cmd)
      }

      # Cross-module comms ----
      # handle commands sent from other modules as set in the reactive value
      observe({
        cmd <- req(rv$map_cmd)
        switch(
          cmd,
          "fit_sites" = fit_sites(),
          warning(sprintf("Unrecognized message '%s'", cmd))
        )
      })

      # UI components ----

      ## map - renderLeaflet ----
      output$map <- renderLeaflet({
        btn_js <- function(id) {
          JS(paste0("(btn, map) => { sendShiny('map-map_btn', '", id, "') };"))
        }

        map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
          addMapPane("extent", 501) |>
          # addMapPane("counties", 410) |>
          addMapPane("grid", 502) |>
          addMapPane("sites", 503) |>
          # addPolygons(
          #   data = service_bounds,
          #   color = "black",
          #   weight = 2,
          #   fill = FALSE,
          #   options = pathOptions(pane = "extent", interactive = FALSE)
          # ) |>
          fit_bounds(OPTS$map_bounds_wi)

        # add basemaps
        basemaps <- OPTS$map_tiles
        for (name in names(basemaps)) {
          map <- addProviderTiles(map, basemaps[[name]], group = name)
        }

        # set up the js callback for cropland data layer (CDL)
        # assume CDL is available from prev year 60 days into the year
        # assigns leaflet map object to global var 'map' so it can be accessed
        yr <- year(Sys.Date()) - ifelse(yday(Sys.Date()) > 60, 1, 2)
        cdl_years <- seq(yr, yr - 3, by = -1)
        callback <- sprintf(
          "() => {
            map = this.getMap();
            const years = [%s];
            createCDLLayers(map, years);
          }",
          paste(cdl_years, collapse = ", ")
        )

        # finalize map
        map |>
          addLayersControl(
            baseGroups = names(basemaps),
            overlayGroups = OPTS$map_layers |> set_names(NULL),
            options = layersControlOptions(collapsed = TRUE)
          ) |>
          addEasyButtonBar(
            easyButton(
              title = "Get my location",
              icon = "fa-location",
              position = "topleft",
              onClick = btn_js("user_loc")
            ),
            easyButton(
              title = "Fit all sites on the map",
              icon = "fa-expand",
              position = "topleft",
              onClick = btn_js("zoom_sites")
            ),
            easyButton(
              title = "Show full map",
              icon = "fa-globe",
              position = "topleft",
              onClick = btn_js("zoom_extent")
            )
          ) |>
          addFullscreenControl() |>
          suspendScroll(
            sleepTime = 0,
            wakeTime = 1000,
            hoverToWake = FALSE,
            sleepNote = FALSE,
            sleepOpacity = 1
          ) |>
          onRender(callback)
      })

      ## map_title - renderUI ----
      output$map_title <- renderUI({
        req(rv$map_risk_data)
        title <- req(rv$map_title)

        div(class = "map-title", title)
      })

      ## searchbox_ui - renderUI ----
      # google places autocomplete will be attached to this
      output$searchbox_ui <- renderUI({
        div(
          title = "Search by name for a city or place",
          textInput(
            ns("searchbox"),
            label = NULL,
            placeholder = "Find a location"
          )
        )
      })

      # initialize google places autocomplete
      session$sendCustomMessage(
        "google-places-init",
        list(
          apiKey = OPTS$google_places_key,
          inputId = ns("searchbox"),
          outputId = ns("searched_loc")
        )
      )

      ## coord_search_ui // renderUI ----
      # Coordinate searchbox under map
      output$coord_search_ui <- renderUI({
        # treat pressing Enter as clicking "go"
        runjs(
          "$(document).keyup((event) => {
            if ($('#map-coord_search').is(':focus') && (event.key == 'Enter')) {
              $('#map-coord_search_go').click();
            }
          });"
        )

        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "display: inline-flex; gap: 5px;",
            div(
              style = "flex: 1;",
              title = "Enter coordinates as decimal degrees eg '45.12, -89.34'",
              textInput(
                inputId = ns("coord_search"),
                label = NULL,
                placeholder = "Enter coordinates",
              )
            ),
            div(
              style = "margin-bottom: 10px;",
              actionButton(ns("coord_search_go"), "Go")
            )
          )
        )
      })

      # Observers ----

      ## Add counties to map ----
      # observe({
      #   delay(100, {
      #     leafletProxy("map") |>
      #       addPolygons(
      #         data = counties_sf,
      #         group = OPTS$map_layers$counties,
      #         label = ~paste0("<b>", state_name, "</b></br>", county_name, " County") |>
      #           lapply(HTML),
      #         color = "black", weight = .2, opacity = .2,
      #         fillColor = ~colorFactor(OPTS$state_colors, state_name)(state_name),
      #         fillOpacity = .1,
      #         options = pathOptions(pane = "counties")
      #       )
      #   })
      # })

      ## Show site markers ----
      observe({
        wx <- rv$weather
        sites <- visible_sites()

        proxy_map |> clearGroup("sites")
        req(nrow(sites) > 0)

        # determine site icons
        sites <- if (nrow(wx) == 0) {
          sites |> mutate(needs_download = TRUE)
        } else {
          req(visible_sites_with_status())
        }

        color_by_risk <- FALSE

        sites <- sites |>
          mutate(
            selection_color = if_else(id == rv$selected_site, "red", "blue"),
            marker_color = selection_color,
            text_color = "#fff",
            label = paste0(
              "<b>Site ",
              id,
              ": ",
              name,
              if_else(
                (nrow(sites) > 1) & id == rv$selected_site,
                " [Selected]",
                ""
              ),
              "</b><br>",
              sprintf("%.3f°N, %.3f°W", lat, lng),
              if_else(needs_download, "<br>Download required", "")
            )
          )

        # color by risk value if available
        risk_values <- rv$map_risk_data
        try({
          if (!is.null(risk_values) && nrow(risk_values) > 0) {
            sites <- sites |>
              left_join(risk_values, join_by(id)) |>
              rowwise() |>
              mutate(risk_color = coalesce(risk_color, "#aaa")) |>
              mutate(as_tibble(find_closest_css_color(risk_color))) |>
              mutate(marker_color = css_color) |>
              mutate(
                model_text = if_else(
                  is.na(model_name),
                  "No model data",
                  sprintf("%s: %s", model_name, value_label)
                )
              ) |>
              mutate(label = paste0(label, "<br>", model_text))
            color_by_risk <- TRUE
          }
        })

        # echo(sites |> select(id, name, marker_color, any_of(c("risk_color", "css_color", "text_color"))))

        proxy_map |>
          addAwesomeMarkers(
            data = sites,
            lat = ~lat,
            lng = ~lng,
            label = ~ lapply(label, HTML),
            layerId = ~id,
            group = "sites",
            icon = ~ makeAwesomeIcon(
              library = "fa",
              # icon = icon,
              markerColor = marker_color,
              iconColor = text_color,
              text = if_else(
                id == rv$selected_site,
                sprintf("(%s)", id),
                as.character(id)
              )
            ),
            options = markerOptions(pane = "sites")
          )
      })

      ## Show user weather data grids ----
      # will only show grids that the user has interacted with in the session
      observe({
        # display any cached weather data for user
        grids <- req(map_data()$grids_with_status)
        if (nrow(grids) > 0) {
          grids <- annotate_grids(grids)
          proxy_map |>
            clearGroup(OPTS$map_layers$grid) |>
            addPolygons(
              data = grids,
              weight = 0.5,
              label = ~label,
              layerId = ~grid_id,
              group = OPTS$map_layers$grid,
              color = "grey",
              opacity = 0.25,
              # fillColor = ~color,
              fillOpacity = 0,
              options = pathOptions(pane = "grid")
            )
        }

        # display grids linked to sites more prominently
        sites <- req(visible_sites_with_status())
        linked_sites <- sites |>
          drop_na(grid_id)
        if (nrow(linked_sites) > 0) {
          sites <- linked_sites |>
            st_as_sf() |>
            annotate_grids()
          proxy_map |>
            addPolygons(
              data = sites,
              weight = 1,
              label = ~label,
              layerId = ~grid_id,
              group = OPTS$map_layers$grid,
              color = ~color,
              opacity = 1,
              # fillColor = ~color,
              # fillOpacity = 0.025,
              fillOpacity = 0,
              options = pathOptions(pane = "grid")
            )
        }
      })

      ## Show all weather data grids ----
      # these are any grids in the saved weather data
      observe({
        # req(session$clientData$url_hostname == "127.0.0.1")

        grids <- map_data()$grids

        proxy_map |>
          clearGroup("grid") |>
          addPolylines(
            data = grids,
            color = "black",
            weight = 0.25,
            opacity = 1,
            group = OPTS$map_layers$grid,
            options = pathOptions(pane = "grid")
          )
      })

      ## Handle EasyButton clicks ----
      observe({
        btn <- req(input$map_btn)

        if (btn == "user_loc") {
          runjs(
            "
            map.locate({ setView: false }).on('locationfound', (event) => {
              sendShiny('map-user_loc', event.latlng)
            })
          "
          )
        } else if (btn == "zoom_sites") {
          fit_sites()
        } else if (btn == "zoom_extent") {
          fit_bounds(bounds = OPTS$map_bounds_us)
        }
      })

      ## Handle searched from google autocomplete ----
      observe({
        loc <- req(input$searched_loc)
        site <- Site(loc$name, loc$lat, loc$lng)
        save_site(site)
        fly_to(site)
        runjs("$('#map-searchbox').val(null);")
      }) |>
        bindEvent(input$searched_loc)

      ## Handle coord search button ----
      # try to parse coords and save if it works
      observe({
        str <- req(input$coord_search)
        tryCatch(
          {
            coords <- parse_coords(str)
            runjs("$('#map-coord_search').val(null);")
            get_loc_name(coords$lat, coords$lng, "Searched point")
          },
          error = function(e) {
            show_toast(
              "Invalid coordinate search",
              text = sprintf(
                "'%s' could not be parsed as valid coordinates. Expected something like '45.12, -89.34'",
                str
              ),
              position = "center"
            )
          }
        )
      }) |>
        bindEvent(input$coord_search_go)

      ## Handle geolocation ----
      observe({
        loc <- req(input$user_loc)
        get_loc_name(loc$lat, loc$lng, "Your location")
      }) |>
        bindEvent(input$user_loc)

      ## Handle location from click ----
      observe({
        loc <- req(input$map_click)
        get_loc_name(loc$lat, loc$lng, "Clicked point")
      }) |>
        bindEvent(input$map_click$.nonce)

      ## Save site after getting locality name from geocoding api
      observe({
        loc <- req(input$locality_name)
        site <- Site(loc$name, loc$lat, loc$lng)
        save_site(site)
        fly_to(site)
        runjs("sendShiny('map-locality_name', null);")
      }) |>
        bindEvent(input$locality_name)

      ## Handle marker click ----
      observe({
        marker <- req(input$map_marker_click)
        id <- marker$id
        sites <- rv$sites
        site <- sites[id, ]
        if (rv$selected_site != id) {
          rv$selected_site <- id
        }
        rv$sites <- sites
        # fly_to(marker)
      }) |>
        bindEvent(input$map_marker_click$.nonce)
    } # end module
  )
}
