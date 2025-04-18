
riskUI <- function() {
  ns <- NS("risk")
  div(
    style = "margin-top: 10px;",
    p(OPTS$risk_info$general),
    uiOutput(ns("crop_ui")),
    uiOutput(ns("crop_info_ui")),
    uiOutput(ns("main_ui"))
  )
}

riskServer <- function(wx_data, selected_site, sites_ready) {
  moduleServer(
    id = "risk",
    function(input, output, session) {
      ns <- session$ns

      # Reactives ----

      ## rv ----
      rv <- reactiveValues(
        weather_ready = FALSE
      )

      observe({
        wr <- nrow(wx_data()$hourly) > 0
        if (rv$weather_ready != wr) rv$weather_ready <- wr
      })

      ## selected_models() ----
      # based on crop, select and name disease models
      selected_models <- reactive({
        crop <- req(input$crop)
        crops <- OPTS$crop_diseases
        c(
          # White mold
          if (crop %in% crops$white_mold) c(
            "White mold" = switch(req(input$irrigation),
              "dry" = "white_mold_dry_prob",
              "irrig" = switch(req(input$spacing),
                "30" = "white_mold_irrig_30_prob",
                "15" = "white_mold_irrig_15_prob"
              )
            )
          ),
          if (crop %in% crops$gray_leaf_spot) c("Gray leaf spot" = "gray_leaf_spot_prob"),
          if (crop %in% crops$tarspot) c("Tarspot" = "tarspot_prob"),
          if (crop %in% crops$frogeye) c("Frogeye leaf spot" = "frogeye_leaf_spot_prob"),
          if (crop %in% crops$early_blight) c("Early blight" = "potato_pdays"),
          if (crop %in% crops$late_blight) c("Late blight" = "late_blight_dsv"),
          if (crop %in% crops$alternaria) c("Alternaria leaf blight" = "alternaria_dsv"),
          if (crop %in% crops$cercospora) c("Cercospora leaf blight" = "cercospora_div")
        )
      })


      # UI Elements ----

      ## crop_ui ----
      # crop picker
      output$crop_ui <- renderUI({
        crop_choices <- OPTS$risk_crop_choices

        tagList(
          radioGroupButtons(
            inputId = ns("crop"),
            label = "Crop type",
            choices = crop_choices,
            selected = first_truthy(input$crops, first(crop_choices)),
            individual = TRUE,
            size = "sm"
          )
        )
      })

      ## crop_info_ui ----
      # displays some text about diseases afflicting a crop
      output$crop_info_ui <- renderUI({
        crop <- req(input$crop)
        div(
          style = "margin: 10px 0; font-style: italic;",
          OPTS$risk_info[[crop]]
        )
      })

      ## main_ui ----
      # shown when sites/weather validation pass
      output$main_ui <- renderUI({
        validate(need(sites_ready(), OPTS$validation_sites_ready))
        validate(need(rv$weather_ready, OPTS$validation_weather_ready))

        tagList(
          div(
            style = "display: flex; flex-direction: row;",
            uiOutput(ns("show_all_sites_ui")),
            uiOutput(ns("white_mold_ui"))
          ),
          uiOutput(ns("selected_site_ui")),
          uiOutput(ns("weather_missing_ui")),
          uiOutput(ns("plots_ui"))
        )
      })

      ## white_mold_ui ----
      # irrigation and crop spacing picker for white mold model
      output$white_mold_ui <- renderUI({
        crop <- req(input$crop)
        crops <- OPTS$crop_diseases
        req(crop %in% crops$white_mold)

        irrig_choices <- list("Dry" = "dry", "Irrigated" = "irrig")
        spacing_choices <- list("30-inch" = "30", "15-inch" = "15")

        div(
          class = "inline-flex",
          style = "margin: 0 10px;",
          radioButtons(
            inputId = ns("irrigation"),
            label = "Irrigation:",
            choices = irrig_choices,
            selected = input$irrigation %||% "irrig",
            inline = TRUE
          ),
          conditionalPanel(
            "input['risk-irrigation'] == 'irrig'",
            radioButtons(
              inputId = ns("spacing"),
              label = "Row spacing:",
              choices = spacing_choices,
              selected = input$spacing %||% "30",
              inline = TRUE
            )
          )
        )
      })

      ## show_all_sites_ui ----
      # shown when more than one site
      output$show_all_sites_ui <- renderUI({
        sites <- wx_data()$sites
        req(nrow(sites) > 1)
        radioButtons(
          inputId = ns("show_all_sites"),
          label = "Show results for:",
          choices = list(
            "All sites" = TRUE,
            "Selected site" = FALSE
          ),
          selected = input$show_all_sites %||% TRUE,
          inline = TRUE
        )
      })

      ## weather_missing_ui ----
      # display warning when days_missing > 0 for any site
      output$weather_missing_ui <- renderUI({
        sites <- wx_data()$sites
        req(rv$weather_ready && nrow(sites) > 0 && any(sites$days_missing > 0))
        div(
          style = "margin-bottom: 15px;",
          missing_weather_ui(n = nrow(sites))
        )
      })

      ## plots_ui ----
      # generate the feed of mini plots by site
      output$plots_ui <- renderUI({
        models <- selected_models()
        wx <- wx_data()
        req(wx$disease)
        req(nrow(wx$disease) > 0)
        dates <- wx$dates
        sites <- wx$sites %>% mutate(site_label = sprintf("Site %s: %s", id, name))

        # optionally filter only selected site
        if (nrow(sites) > 1) {
          if (req(!is.null(input$show_all_sites)) & input$show_all_sites == FALSE) {
            sites <- sites %>% filter(id == selected_site())
          }
        }

        disease_data <- wx$disease %>%
          select(grid_id, date, all_of(models)) %>%
          pivot_longer(cols = -c(grid_id, date)) %>%
          left_join(enframe(models, value = "model"), join_by(name))

        site_data <- sites %>%
          st_drop_geometry() %>%
          select(site_label, grid_id) %>%
          left_join(disease_data, join_by(grid_id)) %>%
          mutate(name = fct_inorder(name))

        site_labels <- unique(sites$site_label)

        # generate plots
        elems <- lapply(site_labels, function(label) {
          df <- site_data %>%
            filter(site_label == !!label) %>%
            drop_na(grid_id, date, value)

          content <- if (nrow(df) > 0) {
            df <- df %>%
              mutate(assign_risk(first(model), value), .by = model)
            last_value <- df %>%
              filter(date == min(today(), max(date))) %>%
              mutate(risk_label = sprintf("%s: %s (%.0f%%)", name, risk, value * 100))
            risk_date <- first(last_value$date)
            risk_info <- paste(last_value$risk_label, collapse = ", ")
            date_range <- c(dates$start, max(dates$end, max(df$date)))
            plts <- disease_plot(df, xrange = date_range)

            div(
              class = "flex-down",
              plts,
              em(strong(paste0("For ", format(risk_date, "%b %d, %Y"), ":")), risk_info)
            )
          } else {
            strong("No data downloaded yet for this site.")
          }

          div(style = "border: 1px solid hsl(210, 40%, 80%); border-radius: 5px;",
            div(label, style = "background: hsl(210, 40%, 95%); padding: 5px 10px; font-size: large; font-weight: bold; border-radius: 5px;"),
            div(content, style = "background: white; padding: 5px 10px; border-radius: 5px;")
          )
        })

        div(
          style = "display: flex; flex-direction: column; gap: 10px;",
          elems
        )
      })

    } # end module
  )
}
