
riskUI <- function() {
  ns <- NS("risk")
  div(
    style = "margin-top: 10px;",
    p("Risk models are only valid when the crop is present and in a vulnerable growth stage. Risk may be mitigated in commercial production by application of a protective fungicide. Set the start date to the approximate date of crop emergence for accurate risk assessments."),
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


      # Reactive ----

      ## rv ----
      rv <- reactiveValues(
        weather_ready = FALSE
      )

      # fill local data when available
      observe({
        wr <- nrow(wx_data()$hourly) > 0
        if (rv$weather_ready != wr) rv$weather_ready <- wr
      })

      ## white_mold_model // reactive ----
      # based on crop, select and name disease models
      white_mold_model <- reactive({
        switch(req(input$irrigation),
          "dry" = c("White mold (non-irrigated)" = "white_mold_dry_prob"),
          "irrig" = switch(req(input$spacing),
            "30" = c("White mold (irrigated, 30-in rows)" = "white_mold_irrig_30_prob"),
            "15" = c("White mold (irrigated, 15-in rows)" = "white_mold_irrig_15_prob")
          )
        )
      })

      ## selected_models // reactive ----
      selected_models <- reactive({
        crop <- req(input$crop)
        disease_slugs <- crops[[crop]][["diseases"]]

        # prepare list with display names and column names
        # white mold has 3 possible data columns and names depending on inputs
        sapply(disease_slugs, function(slug) {
          d <- diseases[[slug]]
          switch(slug,
            "white_mold" = white_mold_model(),
            set_names(d$colname, d$name)
          )
        }, USE.NAMES = FALSE)
      })


      # UI Elements ----

      ## crop_ui ----
      # crop picker
      output$crop_ui <- renderUI({
        crop_choices <- OPTS$crop_choices

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
        crop_slug <- req(input$crop)
        crop <- crops[[crop_slug]]
        crop_info <- crop$info
        crop_diseases <- diseases[names(diseases) %in% crop$diseases]
        info_links <- map(crop_diseases, ~disease_modal_link(.x)) %>%
          paste(collapse = ", ")

        div(
          style = "margin: 10px 0; font-style: italic;",
          crop_info,
          HTML(paste0("More information: ", info_links, "."))
        )
      })

      ## main_ui ----
      # shown when sites/weather validation pass
      output$main_ui <- renderUI({
        validate(need(sites_ready(), OPTS$validation_sites_ready))
        validate(need(rv$weather_ready, OPTS$validation_weather_ready))

        tagList(
          uiOutput(ns("white_mold_ui")),
          uiOutput(ns("show_all_sites_ui")),
          uiOutput(ns("selected_site_ui")),
          uiOutput(ns("weather_missing_ui")),
          uiOutput(ns("plots_ui"))
        )
      })

      ## white_mold_ui ----
      # irrigation and crop spacing picker for white mold model
      output$white_mold_ui <- renderUI({
        crop <- req(input$crop)
        req("white_mold" %in% crops[[crop]][["diseases"]])

        irrig_choices <- list("Dry" = "dry", "Irrigated" = "irrig")
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
              choices = irrig_choices,
              selected = isolate(input$irrigation) %||% "dry",
              inline = TRUE
            ),
          ),
          conditionalPanel(
            "input['risk-irrigation'] == 'irrig'",
            div(
              class = "flex-across",
              tags$label("Row spacing:"),
              radioButtons(
                inputId = ns("spacing"),
                label = NULL,
                choices = spacing_choices,
                selected = isolate(input$spacing) %||% "30",
                inline = TRUE
              )
            )
          )
        )
      })

      ## show_all_sites_ui ----
      # shown when more than one site
      output$show_all_sites_ui <- renderUI({
        sites <- wx_data()$sites
        req(nrow(sites) > 1)

        div(
          class = "flex-across",
          tags$label("Show results for:"),
          radioButtons(
            inputId = ns("show_all_sites"),
            # label = "Show results for:",
            label = NULL,
            choices = list(
              "All sites" = TRUE,
              "Selected site" = FALSE
            ),
            selected = input$show_all_sites %||% TRUE,
            inline = TRUE
          )
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

          # to show in site feed
          content <- if (nrow(df) > 0) {
            date_range <- c(dates$start, max(dates$end, max(df$date)))

            # generate individual disease plots
            plts <- imap(models, function(model_col, model_name) {
              df <- df %>%
                filter(model == !!model_col) %>%
                mutate(assign_risk(model_col, value))
              last_value <- df %>% filter(date == min(today(), max(date)))
              risk_info <- sprintf(
                "For %s: %s",
                format(last_value$date, "%b %d, %Y"),
                last_value$value_label
              )
              plt <- plot_risk(df, name = model_name, xrange = date_range)

              div(
                strong(model_name),
                span(style = "font-size: small", em(risk_info)),
                plt
              )
            })

            div(class = "flex-down", plts)
          } else {
            strong("No data downloaded yet for this site.")
          }

          div(
            class = "site-data",
            div(
              class = "site-data-header",
              label
            ),
            div(
              class = "site-data-content",
              content
            )
          )
        })

        div(class = "flex-down", elems)
      })

    } # end module
  )
}
