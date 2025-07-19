#--- map.R ---#

mapUI <- function() {
  ns <- NS("map")

  tagList(
    # div(
    #   style = "padding: 1rem;",
    #   h2("Site map"),
    # ),
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

mapServer <- function(rv, map_data) {
  moduleServer(
    id = "map",
    function(input, output, session) {

      ns <- session$ns


      # Helper functions ----

      proxy_map <- leafletProxy(ns("map"))

      # wrapper for leaflet flyTo
      fly_to <- function(loc) {
        proxy_map %>%
          flyTo(loc$lng, loc$lat, max(10, isolate(input$map_zoom)))
      }

      #' wrapper for leaflet fitBounds
      #' @param map leaflet proxy object
      #' @param bounds named list { lat1, lat2, lng1, lng2 }
      #' @param options leaflet zoom/pan options
      fit_bounds <- function(map = proxy_map, bounds, options = NULL) {
        args <- as.list(bounds)
        args$map <- map
        args$options = options
        do.call(fitBounds, args)
      }

      # fits all sites on the map
      fit_sites <- function() {
        sites <- rv$sites
        req(nrow(sites) > 0)

        bounds <- list(
          lat1 = min(sites$lat), lat2 = max(sites$lat),
          lng1 = min(sites$lng), lng2 = max(sites$lng)
        )

        fit_bounds(bounds = bounds, options = list(padding = c(100, 100), maxZoom = 10))
      }

      # wrapper for leaflet addProviderTiles
      add_basemaps <- function(map) {
        basemaps <- OPTS$map_tiles
        for (name in names(basemaps)) {
          map <- addProviderTiles(map, basemaps[[name]], group = name)
        }
        map
      }

      save_site <- function(site) {
        sites <- rv$sites

        if (nrow(sites) == OPTS$max_sites) return()

        if (!validate_ll(site$lat, site$lng)) {
          show_toast(
            "Invalid location",
            text = sprintf("The location %s, %s is not valid or is outside of our service area.", site$lat, site$lng),
            position = "center"
          )
          req(FALSE)
        }

        # if several sites already, confirm each new map click
        # value may be FALSE if cancelled or string (name of site from modal)
        finalize <- function(value) {
          req(value)

          value <- sanitize_loc_names(value)
          req(value)

          site$name <- value
          sites <- sites %>%
            bind_rows(as_tibble(site)) %>%
            distinct(lat, lng, .keep_all = T) %>%
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

      get_loc_name <- function(lat, lng, name) {
        cmd <- sprintf("getLocalityName(%s, %s, '%s', '%s')", lat, lng, name, OPTS$google_geocoding_key)
        runjs(cmd)
      }

      # trigger local functions from the main server
      observe({
        cmd <- req(rv$map_cmd)
        switch(
          cmd,
          "fit_sites" = fit_sites(),
          warning(sprintf("Unrecognized message '%s'", cmd))
        )
      })


      # UI components ----

      ## map // renderLeaflet ----
      output$map <- renderLeaflet({

        btn_js <- function(id) {
          JS(paste0("(btn, map) => { sendShiny('map-map_btn', '", id, "') };"))
        }

        leaflet(options = leafletOptions(preferCanvas = T)) %>%
          addMapPane("extent", 501) %>%
          # addMapPane("counties", 410) %>%
          addMapPane("grid", 502) %>%
          addMapPane("sites", 503) %>%
          add_basemaps() %>%
          addLayersControl(
            baseGroups = names(OPTS$map_tiles),
            overlayGroups = unlist(OPTS$map_layers, use.names = F),
            options = layersControlOptions(collapsed = T)
          ) %>%
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
          ) %>%
          # assign leaflet map object to global var 'map'
          htmlwidgets::onRender("() => { map = this; }") %>%
          suspendScroll(
            sleepTime = 0,
            wakeTime = 1000,
            hoverToWake = F,
            sleepNote = F,
            sleepOpacity = 1
          ) %>%
          addPolygons(
            data = service_bounds,
            color = "black",
            weight = 2,
            fill = FALSE,
            options = pathOptions(pane = "extent", interactive = FALSE)
          ) %>%
          fit_bounds(OPTS$map_bounds_wi)
      })


      ## Map title ----

      output$map_title <- renderUI({
        title <- req(rv$map_title)

        div(class = "map-title", title)
      })


      ## searchbox_ui // renderUI ----
      output$searchbox_ui <- renderUI({
        div(
          title = "Search by name for a city or place",
          HTML(paste0("<script async src='https://maps.googleapis.com/maps/api/js?key=", OPTS$google_places_key, "&loading=async&libraries=places&callback=initAutocomplete'></script>")),
          # textInput("searchbox", "Find a location by name")
          textInput(ns("searchbox"), NULL)
        )
      })


      ## coord_search_ui // renderUI ----
      # Coordinate searchbox under map
      output$coord_search_ui <- renderUI({

        # treat pressing Enter as clicking "go"
        runjs("
          $(document).keyup((event) => {
            if ($('#map-coord_search').is(':focus') && (event.key == 'Enter')) {
              $('#map-coord_search_go').click();
            }
          });
        ")

        div(
          style = "display: flex; flex-direction: column;",
          # div(tags$label("Find a location by coordinates")),
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
      #     leafletProxy("map") %>%
      #       addPolygons(
      #         data = counties_sf,
      #         group = OPTS$map_layers$counties,
      #         label = ~paste0("<b>", state_name, "</b></br>", county_name, " County") %>%
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
        sites <- rv$sites

        proxy_map %>% clearGroup("sites")
        req(nrow(sites) > 0)

        # determine site icons
        sites <- if (nrow(wx) == 0) {
          rv$sites %>% mutate(needs_download = TRUE)
        } else {
          map_data()$sites_with_status
        }

        color_by_risk <- FALSE
        risk_values <- rv$risk_last_value

        sites <- sites %>%
          mutate(
            selection_color = if_else(id == rv$selected_site, "red", "blue"),
            marker_color = selection_color,
            text_color = "#fff",
            label = paste0(
              "<b>Site ", id, ": ", name,
              if_else((nrow(sites) > 1) & id == rv$selected_site, " [Selected]", ""),
              "</b><br>",
              sprintf("%.3f°N, %.3f°W", lat, lng),
              if_else(needs_download, "<br>Download required", "")
            )
          )

        # color by risk value if available
        try({
          if (nrow(risk_values) > 0) {
            sites <- sites %>%
              left_join(risk_values, join_by(id)) %>%
              rowwise() %>%
              mutate(
                risk_color = coalesce(risk_color, "#aaa"),
                as_tibble(find_closest_css_color(risk_color))
              ) %>%
              mutate(marker_color = css_color) %>%
              mutate(label = paste(label, sprintf("<br>%s: %s", model_name, value_label)))
            color_by_risk <- TRUE
          }
        })

        # echo(sites %>% select(id, name, marker_color, any_of(c("risk_color", "css_color", "text_color"))))

        proxy_map %>%
          addAwesomeMarkers(
            data = sites,
            lat = ~lat,
            lng = ~lng,
            label = ~lapply(label, HTML),
            layerId = ~id,
            group = "sites",
            icon = ~makeAwesomeIcon(
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
        proxy_map %>%
          clearGroup(OPTS$map_layers$grid)

        # user-selected grids this session
        grids <- map_data()$grids_with_status %>%
          filter(grid_id %in% rv$grids[["grid_id"]]) # null safe column ref

        # if any, display them
        if (nrow(grids) > 0) {
          proxy_map %>%
            addPolygons(
              data = annotate_grids(grids),
              weight = 1,
              label = ~label,
              layerId = ~grid_id,
              group = OPTS$map_layers$grid,
              color = ~color,
              opacity = 1,
              # fillColor = ~color,
              fillOpacity = 0,
              options = pathOptions(pane = "grid")
            )
        }
      })


      ## Show all weather data grids ----
      # only in development
      observe({
        req(session$clientData$url_hostname == "127.0.0.1")

        grids <- map_data()$grids

        proxy_map %>%
          clearGroup("grid") %>%
          addPolylines(
            data = grids,
            color = "black",
            weight = 0.25,
            opacity = 1,
            group = "grid",
            options = pathOptions(pane = "grid")
          )
      })


      ## Handle EasyButton clicks ----
      observe({
        btn <- req(input$map_btn)

        if (btn == "user_loc") {
          runjs("
            map.getMap().locate({ setView: false }).on('locationfound', (event) => {
              sendShiny('map-user_loc', event.latlng)
            })
          ")
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
      }) %>%
        bindEvent(input$searched_loc)


      ## Handle coord search button ----
      # try to parse coords and save if it works
      observe({
        str <- req(input$coord_search)
        tryCatch({
          coords <- parse_coords(str)
          runjs("$('#map-coord_search').val(null);")
          get_loc_name(coords$lat, coords$lng, "Searched point")
        }, error = function(e) {
          show_toast("Invalid coordinate search", text = sprintf("'%s' could not be parsed as valid coordinates. Expected something like '45.12, -89.34'", str), position = "center")
        })
      }) %>%
        bindEvent(input$coord_search_go)


      ## Handle geolocation ----
      observe({
        loc <- req(input$user_loc)
        get_loc_name(loc$lat, loc$lng, "Your location")
      }) %>%
        bindEvent(input$user_loc)


      ## Handle location from click ----
      observe({
        loc <- req(input$map_click)
        get_loc_name(loc$lat, loc$lng, "Clicked point")
      }) %>%
        bindEvent(input$map_click$.nonce)


      ## Save site after getting locality name from geocoding api
      observe({
        loc <- req(input$locality_name)
        site <- Site(loc$name, loc$lat, loc$lng)
        save_site(site)
        fly_to(site)
        runjs("sendShiny('map-locality_name', null);")
      }) %>%
        bindEvent(input$locality_name)


      ## Handle marker click ----
      observe({
        marker <- req(input$map_marker_click)
        id <- marker$id
        sites <- rv$sites
        site <- sites[id,]
        if (rv$selected_site != id) rv$selected_site <- id
        rv$sites <- sites
        # fly_to(marker)
      }) %>%
        bindEvent(input$map_marker_click$.nonce)


    } # end module
  )
}
