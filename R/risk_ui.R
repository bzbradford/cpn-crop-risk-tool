riskUI <- function() {
  ns <- NS("risk")
  div(
    style = "margin-top: 10px;",
    p("Risk models are only valid when the crop is present and in a vulnerable growth stage. Risk may be mitigated in commercial production by application of a protective fungicide. Set the start date to the approximate date of crop emergence for accurate risk assessments."),
    uiOutput(ns("main_ui")) %>% withSpinner(type = 8, size = .5, proxy.height = 70, caption = "Loading...")
  )
}

riskServer <- function(rv, wx_data) {
  moduleServer(
    id = "risk",
    function(input, output, session) {
      ns <- session$ns

      ## main_ui ----
      # shown when sites/weather validation pass
      output$main_ui <- renderUI({
        validate(
          need(rv$sites_ready, OPTS$validation_sites_ready),
          need(rv$weather_ready, OPTS$validation_weather_ready)
        )

        tagList(
          uiOutput(ns("crop_picker")),
          uiOutput(ns("model_picker")),
          uiOutput(ns("model_ui")),
          uiOutput(ns("selected_site_ui")),
          uiOutput(ns("weather_missing_ui")),
          uiOutput(ns("plots_ui"))
        )
      })


      ## crop_picker ----
      output$crop_picker <- renderUI({
        choices <- OPTS$crop_choices

        radioGroupButtons(
          inputId = ns("crop"),
          label = "Crop type:",
          choices = choices,
          selected = first_truthy(input$crops, first(choices)),
          individual = TRUE,
          size = "sm"
        )
      })


      ## model_picker ----
      output$model_picker <- renderUI({
        crop_slug <- req(input$crop)
        crop <- crops[[crop_slug]]
        models <- crop$diseases
        choices <- sapply(models, function(x) setNames(x[["slug"]], x[["name"]]))

        radioGroupButtons(
          inputId = ns("model"),
          label = "Risk model:",
          choices = choices,
          selected = first_truthy(input$model, first(choices)),
          individual = TRUE,
          size = "sm"
        )
      })


      ## selected_model() - reactive ----
      selected_model <- reactive({
        model_slug <- req(input$model)
        diseases[[model_slug]]
      })


      ## model_ui ----
      output$model_ui <- renderUI({
        model <- selected_model()

        tagList(
          div(
            style = "margin: 10px 0; font-style: italic;",
            model$info,
            disease_modal_link(model)
          ),

          # model-specific additional ui elements
          uiOutput(ns("white_mold_ui"))
        )
      })

      ## white_mold_ui ----
      # irrigation and crop spacing picker for white mold model
      output$white_mold_ui <- renderUI({
        model <- req(input$model)
        req(model == "white_mold")

        uiOutput(ns("white_mold_opts"))
      })

      output$white_mold_opts <- renderUI({
        irrigation_choices <- list("Dry" = FALSE, "Irrigated" = TRUE)
        spacing_choices <- list("30-inch" = "30", "15-inch" = "15")

        div(
          class = "flex-across",
          style = "row-gap: 0px; column-gap: 30px;",
          div(
            class = "flex-across",
            tags$label("Irrigation:"),
            radioButtons(
              inputId = ns("irrigation"),
              label = NULL,
              choices = irrigation_choices,
              selected = isolate(input$irrigation) %||% first(irrigation_choices),
              inline = TRUE
            ),
          ),
          conditionalPanel(
            "input['risk-irrigation'] == 'TRUE'",
            div(
              class = "flex-across",
              tags$label("Row spacing:"),
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


      ## show_all_sites_ui ----
      # shown when more than one site
      output$selected_site_ui <- renderUI({
        sites <- wx_data()$sites
        req(nrow(sites) > 1)

        uiOutput(ns("selected_site_opts"))
      })

      output$selected_site_opts <- renderUI({
        div(
          class = "flex-across",
          tags$label("Show results for:"),
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


      ## weather_missing_ui ----
      # display warning when days_missing > 0 for any site
      output$weather_missing_ui <- renderUI({
        sites <- wx_data()$sites
        req(rv$weather_ready && nrow(sites) > 0 && any(sites$needs_download))
        div(
          style = "margin-bottom: 15px;",
          missing_weather_ui(n = nrow(sites))
        )
      })


      # Generate model data ----

      ## model_data() - reactive ----
      model_data <- reactive({
        model <- req(input$model)
        wx_data <- wx_data()
        wx <- wx_data$daily_full
        req(nrow(wx) > 0)
        date_range <- wx_data$dates

        df <- switch(model,
          "tar_spot" = build_tar_spot(wx),
          "gray_leaf_spot" = build_gray_leaf_spot(wx),
          "don" = build_don(wx),
          "white_mold" = if (req(input$irrigation)) {
            build_white_mold_irrig(wx, req(input$spacing))
          } else {
            build_white_mold_dry(wx)
          },
          "frogeye" = build_frogeye_leaf_spot(wx),
          "early_blight" = build_early_blight(wx),
          "late_blight" = build_late_blight(wx),
          "alternaria" = build_alternaria(wx),
          "cercospora" = build_cercospora(wx),
          "botrytis" = build_botrytis(wx)
        )

        filter(df, date >= date_range$start)
      })

      # observe(echo(model_data()))


      joined_data <- reactive({
        sites <- wx_data()$sites

        sites %>%
          st_drop_geometry() %>%
          select(id, name, lat, lng, grid_id, grid_lat, grid_lng, time_zone) %>%
          left_join(model_data(), join_by(grid_id), relationship = "many-to-many")
      })

      # observe(echo(joined_data()))


      ## plots_ui ----
      # generate the feed of mini plots by site
      output$plots_ui <- renderUI({
        model <- selected_model()
        model_data <- joined_data() %>%
          mutate(site_label = sprintf("Site %s: %s", id, name), .after = name)
        req(nrow(model_data) > 0)

        wx <- wx_data()
        sites <- wx$sites
        dates <- wx$dates

        # show only selected site option
        if (nrow(sites) > 1) {
          i <- req(input$show_all_sites)
          if (i == FALSE) {
            model_data <- model_data %>% filter(id == rv$selected_site)
          }
        }

        last_values <- model_data %>%
          filter(date == min(today(), max(date)), .by = id)

        # write values for map
        rv$risk_last_value <- last_values %>%
          select(id, model_value, value_label, risk, risk_color) %>%
          mutate(model_name = model$name)
        rv$map_title <- paste(model$name, "risk,", format(dates$end, "%b %d, %Y"))

        site_labels <- unique(model_data$site_label)

        # generate plots
        elems <- lapply(site_labels, function(label) {
          df <- model_data %>%
            filter(site_label == !!label) %>%
            rename(value = model_value) %>%
            drop_na(grid_id, date, value)

          # to show in site feed
          content <- if (nrow(df) > 0) {
            date_range <- c(dates$start, max(dates$end, max(df$date)))
            last_value <- last_values %>% filter(site_label == !!label)

            risk_info <- sprintf(
              "For %s: %s",
              format(last_value$date, "%b %d, %Y"),
              last_value$value_label
            )
            plt <- plot_risk(df, name = model$name, xrange = date_range)

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
            downloadButton(ns("download_data"), "Download dataset", class = "btn-sm")
          )
        )
      })

      ## Download handler ----
      output$download_data <- downloadHandler(
        filename = function() {
          data <- wx_data()
          model <- selected_model()
          fname <- sprintf("%s - %s to %s.csv", model$name, data$date$start, data$date$end)
          fname <- gsub("[^A-Za-z0-9 \\.\\-]", " ", fname)
          str_squish(fname)
        },
        content = function(file) {
          data <- wx_data()
          model <- selected_model()
          model_data <- joined_data() %>%
            select(-any_of(OPTS$grid_attr_cols)) %>%
            select(-c(risk_color, value_label)) %>%
            rename_with_units("metric")

          header <- tibble(line = c(
            sprintf("=== Crop Risk Tool - %s model ===", model$name),
            sprintf("Generated: %s", Sys.time()),
            sprintf("Host: %s", session$clientData$url_hostname)
          ))

          sites <- data$sites %>%
            st_drop_geometry() %>%
            select(id:days_missing)

          write_excel_csv(header, file, na = "", col_names = F)
          tibble(line = c("", "=== Site list ===")) %>%
            write_excel_csv(file, append = T)
          write_excel_csv(sites, file, na = "", col_names = T, append = T)
          tibble(line = c("", "=== Model data ===")) %>%
            write_excel_csv(file, append = T)
          write_excel_csv(model_data, file, na = "", col_names = T, append = T)
        }
      )

    } # end module
  )
}
