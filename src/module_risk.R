#--- crop risk module ---#

riskUI <- function() {
  ns <- NS("risk")
  div(
    style = "margin-top: 10px; min-height: 300px;",
    uiOutput(ns("main_ui")) |>
      withSpinner(
        type = 8,
        size = 0.5,
        proxy.height = 300,
        caption = "Loading..."
      )
  )
}

riskServer <- function(rv, wx_data) {
  moduleServer(
    id = "risk",
    function(input, output, session) {
      ns <- session$ns

      # Interface ----

      ## main_ui ----
      # handle validation messages
      output$main_ui <- renderUI({
        validate(
          need(rv$sites_ready, OPTS$validation_sites_ready),
          need(rv$weather_ready, OPTS$validation_weather_ready)
        )

        tagList(
          uiOutput(ns("model_picker"), style = "margin-bottom: 1rem;"),
          uiOutput(ns("model_ui"), style = "margin: 1rem 0;"),
          uiOutput(ns("model_warnings"), style = "margin: 1rem 0;"),
          uiOutput(ns("results_ui"), style = "margin-top: 1rem;"),
        )
      })

      ## model_picker ----
      output$model_picker <- renderUI({
        choices <- build_choices(model_list, "model_name", "slug")

        tags$div(
          style = "display: flex; align-items: center; gap: 10px;",
          tags$label(
            "Select model:",
            `for` = ns("model"),
            class = "form-group",
          ),
          tags$div(
            style = "flex: 1;",
            selectInput(
              inputId = ns("model"),
              label = NULL,
              choices = choices,
              selected = first_truthy(input$model, first(choices))
            )
          )
        )
      })

      ## selected_model() - reactive ----
      selected_model <- reactive({
        model_slug <- req(input$model)
        model_list[[model_slug]]
      })

      ## model_ui ----
      output$model_ui <- renderUI({
        model <- selected_model()

        div(
          style = "padding: 5px 10px; border: 1px solid lightsteelblue; border-radius: 5px; background: white;",
          div(
            HTML(model$info),
            "Risk models are only valid when the crop is present and in a vulnerable growth stage. Risk may be mitigated in commercial production by application of a protective fungicide.",
            build_modal_link(model)
          ),
          uiOutput(ns("whitemold_opts")),
          uiOutput(ns("wheatscab_opts")),
        )
      })

      ## white_mold_opts ----
      # irrigation and crop spacing picker for white mold model
      output$whitemold_opts <- renderUI({
        req(identical(input$model, model_list$whitemold$slug))

        irrigation_choices <- list("Dry" = FALSE, "Irrigated" = TRUE)
        spacing_choices <- list("30-inch" = "30", "15-inch" = "15")

        div(
          class = "flex-across",
          style = "margin-top: 1rem; column-gap: 2rem;",
          div(
            class = "label-inline",
            tags$label("Irrigation:", `for` = ns("irrigation")),
            radioButtons(
              inputId = ns("irrigation"),
              label = NULL,
              choices = irrigation_choices,
              selected = isolate(input$irrigation) %||%
                first(irrigation_choices),
              inline = TRUE
            ),
          ),
          conditionalPanel(
            "input['irrigation'] == 'TRUE'",
            ns = ns,
            div(
              class = "label-inline",
              tags$label("Row spacing:", `for` = ns("spacing")),
              radioButtons(
                inputId = ns("spacing"),
                label = NULL,
                choices = spacing_choices,
                selected = isolate(input$spacing) %||% first(spacing_choices),
                inline = TRUE
              )
            )
          )
        )
      })

      ## wheat_scab_opts ----
      output$wheatscab_opts <- renderUI({
        req(identical(input$model, model_list$wheatscab$slug))

        choices <- list(
          "Very susceptible" = "VS",
          "Susceptible" = "S",
          "Moderately susceptible" = "MS",
          "Moderately resistant" = "MR"
        )

        div(
          style = "margin-top: 1rem;",
          class = "label-inline",
          tags$label("FHB resistance:", `for` = ns("resistance")),
          radioButtons(
            inputId = ns("resistance"),
            label = NULL,
            choices = choices,
            selected = isolate(input$resistance) %||%
              first(choices),
            inline = TRUE
          )
        )
      })

      ## model_warning_ui ----
      # Display warnings for various conditions
      output$model_warnings <- renderUI({
        req(rv$weather_ready)
        model <- selected_model()
        sites <- selected_sites()
        selected_dates <- req(wx_data()$dates)
        warnings <- list()

        # warning for missing weather
        warnings$wx <- weather_warning_for_sites(sites)

        # warning when outside of risk period
        if (!is.null(model$risk_period)) {
          dates <- c(selected_dates$start, selected_dates$end)
          risk_dates <- model$risk_period
          does_overlap <- check_date_overlap(dates, risk_dates)
          if (!any(does_overlap)) {
            warnings$risk_period <- "The crop is not likely to be in a vulnerable growth stage during the dates you have selected. Risk estimates are only valid when they overlap with the susceptible period of the crop's lifecycle."
          }
        }

        req(length(warnings) > 0)

        content <- if (length(warnings) > 1) {
          tagList(
            strong("Warnings:"),
            br(),
            tags$ul(
              lapply(warnings, tags$li)
            )
          )
        } else {
          tagList(
            strong("Warning:"),
            warnings
          )
        }

        build_warning_box(content)
      })

      # results_ui ----
      output$results_ui <- renderUI({
        validate(
          need(
            rv$weather_ready,
            "No weather data downloaded yet for selected dates."
          )
        )

        tagList(
          uiOutput(ns("plot_feed_opts")),
          uiOutput(ns("plots_ui"))
        )
      })

      ## plot_feed_opts ----
      # shown when more than one site
      output$plot_feed_opts <- renderUI({
        sites <- wx_data()$sites
        req(nrow(sites) > 1)

        div(
          style = "padding: 0 1rem;",
          class = "label-inline",
          tags$label("Show results for:", `for` = ns("show_all_sites")),
          radioButtons(
            inputId = ns("show_all_sites"),
            label = NULL,
            choices = list(
              "All sites" = TRUE,
              "Selected site" = FALSE
            ),
            selected = isolate(input$show_all_sites) %||% TRUE,
            inline = TRUE
          )
        )
      })

      ## selected_sites - reactive ----
      selected_sites <- reactive({
        sites <- wx_data()$sites

        if (nrow(sites) > 1) {
          i <- req(input$show_all_sites)
          if (i == FALSE) {
            sites <- subset(sites, id == rv$selected_site)
          }
        }

        sites
      })

      # Generate model data ----

      ## model_data - reactive ----
      model_data <- reactive({
        model <- req(input$model)
        wx_data <- wx_data()
        wx <- wx_data$daily_full
        req(nrow(wx) > 0)
        date_range <- wx_data$dates

        df <- switch(
          model,
          "tarspot" = build_tar_spot(wx),
          "gls" = build_gray_leaf_spot(wx),
          "don" = build_don(wx),
          "whitemold" = if (req(input$irrigation)) {
            build_white_mold_irrig(wx, req(input$spacing))
          } else {
            build_white_mold_dry(wx)
          },
          "frogeye" = build_frogeye_leaf_spot(wx),
          "wheatscab" = build_wheat_scab(
            wx,
            resistance = req(input$resistance)
          ),
          "earlyblight" = build_early_blight(wx),
          "lateblight" = build_late_blight(wx),
          "alternaria" = build_alternaria(wx),
          "cercospora" = build_cercospora(wx),
          "botrytis" = build_botrytis(wx),
          warning("Don't know how to build data for model '", model, "'")
        )

        filter(df, date >= date_range$start)
      })

      # observe(echo(model_data()))

      ## joined_data - reactive ----
      joined_data <- reactive({
        selected_sites() |>
          st_drop_geometry() |>
          select(id, name, lat, lng, grid_id, grid_lat, grid_lng, time_zone) |>
          left_join(
            model_data(),
            join_by(grid_id),
            relationship = "many-to-many"
          ) |>
          mutate(site_label = sprintf("Site %s: %s", id, name), .after = name)
      })

      # observe(echo(joined_data()))

      # Build risk plots ----

      ## plots_ui ----
      # generate the feed of mini plots by site
      output$plots_ui <- renderUI({
        model <- selected_model()
        model_data <- joined_data()
        wx <- wx_data()
        sites <- wx$sites
        dates <- wx$dates

        req(nrow(model_data) > 0)

        last_values <- model_data |>
          filter(date == min(today(), max(date)), .by = id)

        # write values for map
        rv$map_risk_data <- last_values |>
          select(id, model_value, value_label, risk, risk_color) |>
          mutate(model_name = model$name)
        rv$map_title <- paste(
          model$name,
          "risk,",
          format(dates$end, "%b %d, %Y")
        )

        site_labels <- unique(model_data$site_label)

        # generate plots
        elems <- lapply(site_labels, function(label) {
          df <- model_data |>
            filter(site_label == !!label) |>
            drop_na(grid_id, date, model_value)

          # to show in site feed
          content <- if (nrow(df) > 0) {
            date_range <- c(dates$start, max(dates$end, max(df$date)))
            last_value <- last_values |> filter(site_label == !!label)

            risk_info <- sprintf(
              "For %s: %s",
              format(last_value$date, "%b %d, %Y"),
              last_value$value_label
            )
            plt <- plot_risk(
              df,
              name = model$name,
              xrange = date_range,
              risk_period = model$risk_period
            )

            div(
              strong(model$name),
              span(style = "font-size: small", em(risk_info)),
              plt
            )
          } else {
            strong("No data downloaded yet for this site.")
          }

          div(
            class = "site-data",
            div(class = "site-data-header", label),
            div(class = "site-data-content", content)
          )
        })

        div(
          class = "flex-down",
          elems,
          div(
            style = "text-align: right;",
            downloadButton(
              ns("download_data"),
              "Download dataset",
              class = "btn-sm"
            )
          )
        )
      })

      # Download handler -------------------------------------------------------

      ## download_data ----
      output$download_data <- downloadHandler(
        filename = function() {
          data <- wx_data()
          model <- selected_model()
          fname <- sprintf(
            "%s - %s to %s.csv",
            model$name,
            data$date$start,
            data$date$end
          )
          fname <- gsub("[^A-Za-z0-9 \\.\\-]", " ", fname)
          str_squish(fname)
        },
        content = function(file) {
          data <- wx_data()
          model <- selected_model()
          model_data <- joined_data() |>
            select(-any_of(OPTS$grid_attr_cols)) |>
            select(-c(risk_color, value_label, site_label)) |>
            rename_with_units("metric")

          header <- tibble(
            line = c(
              sprintf("=== Crop Risk Tool - %s model ===", model$name),
              sprintf("Generated: %s", Sys.time()),
              sprintf("Host: %s", session$clientData$url_hostname)
            )
          )

          sites <- selected_sites() |>
            st_drop_geometry() |>
            select(id:days_missing)

          write_excel_csv(header, file, na = "", col_names = FALSE)
          tibble(line = c("", "=== Site list ===")) |>
            write_excel_csv(file, append = TRUE)
          write_excel_csv(sites, file, na = "", col_names = TRUE, append = TRUE)
          tibble(line = c("", "=== Model data ===")) |>
            write_excel_csv(file, append = TRUE)
          write_excel_csv(
            model_data,
            file,
            na = "",
            col_names = TRUE,
            append = TRUE
          )
        }
      )
    } # end module
  )
}
