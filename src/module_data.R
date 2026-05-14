# Data module

# Helpers ----------------------------------------------------------------------

## Build all moving averages ----
#' Generate several moving average periods from daily data
#' @param daily accepts daily data from `build_daily()`
#' @param align moving average alignment
#' @returns tibble
build_ma_from_daily <- function(daily, align = c("center", "right")) {
  align <- match.arg(align)

  # retain attribute cols
  attr <- daily |> select(grid_id, any_of(OPTS$date_attr_cols))

  # define moving average functions. Using zoo here for its handling of partial centered rolls
  roll_mean <- function(vec, width) {
    zoo::rollapply(vec, width, calc_mean, partial = TRUE, align = align)
  }

  fns <- c(
    "7day" = ~ roll_mean(.x, 7),
    "14day" = ~ roll_mean(.x, 14),
    "21day" = ~ roll_mean(.x, 21),
    "30day" = ~ roll_mean(.x, 30)
  )

  # apply moving average functions to each primary data column
  ma <- daily |>
    mutate(
      across(
        starts_with(c(
          "temperature",
          "dew_point",
          "relative_humidity",
          "evapotranspiration",
          "precip",
          "snow",
          "pressure",
          "wind",
          "soil"
        )),
        fns
      ),
      .by = grid_id,
      .keep = "none"
    ) |>
    select(-grid_id)

  # bind attributes
  bind_cols(attr, ma)
}

if (FALSE) {
  test_hourly_wx |> build_daily() |> build_ma_from_daily()
}


## Build all GDDs ----
#' Generate various growing degree day models with and without an 86F upper threshold
#' input temperatures must be Celsius and will be converted to Fahrenheit GDDs
#' @param daily accepts daily dataset from `build_daily()`
#' @returns tibble
build_gdd_from_daily <- function(daily) {
  # retain attribute cols
  attr <- daily |> select(grid_id, date)

  # convert temperatures
  tmin <- c_to_f(daily$temperature_min)
  tmax <- c_to_f(daily$temperature_max)

  # start with a base 86F model for chopping off the upper thresholds
  gdd <- tibble(base_86 = gdd_sine(tmin, tmax, 86))

  # generate each of the base temperature models with and without the upper threshold
  for (base in c(32, 39.2, 41, 45, 48, 50, 52, 55)) {
    name <- str_replace_all(paste0("base_", base), "\\.", "p")
    gdd[[name]] <- gdd_sine(tmin, tmax, base)
    gdd[[paste0(name, "_upper_86")]] <- gdd[[name]] - gdd$base_86
  }

  # remove the upper threshold model
  gdd$base_86 <- NULL

  # assemble, add cumulative cols, sort names
  bind_cols(attr, gdd) |>
    mutate(
      across(starts_with("base_"), c(cumulative = cumsum)),
      .by = grid_id
    ) %>%
    select(
      all_of(names(attr)),
      all_of(sort(names(.)))
    )
}

if (FALSE) {
  test_hourly_wx |> build_daily() |> build_gdd_from_daily()
}


# Static UI --------------------------------------------------------------------

dataUI <- function() {
  ns <- NS("data")
  div(
    div(
      style = "margin-top: 1rem; padding: 5px 10px; border: 1px solid lightsteelblue; border-radius: 5px; background: white;",

      "Explore and download hourly, daily, moving average, or growing degree day data for your sites. Values may be shown in either metric or imperial units. Forecast may be shown when end date is set to today. Forecast is available for most measures.",

      # metric/forecast switches
      uiOutput(ns("switches_ui"), style = "margin-top: 1rem;"),
    ),

    # dataset selector
    uiOutput(ns("data_type_ui"), style = "margin-top: 1rem;"),

    # dataset options like rolling mean, may be blank
    uiOutput(ns("data_options_ui")),

    # which columns to plot
    uiOutput(ns("plot_cols_ui"), style = "margin-top: 1rem;"),

    # multi-site checkboxes, plot, and download button
    # includes validation messages
    uiOutput(ns("main_ui")),
  )
}


# Module server ----------------------------------------------------------------

#' @param rv reactive values from parent server
#' @param rx list of reactive expressions from parent server
dataServer <- function(rv, rx) {
  moduleServer(
    id = "data",
    function(input, output, session) {
      ns <- session$ns

      # lightweight cache key shared by the three lazy reactives below
      wx_cache_key <- reactive({
        rlang::hash(rx$wx()$daily)
      })

      # lazy MA/GDD — computed only when selected_data() requests them
      ma_center <- reactive({
        rx$wx()$daily_full |>
          build_ma_from_daily("center")
      }) |>
        bindCache(wx_cache_key())

      ma_right <- reactive({
        rx$wx()$daily_full |>
          build_ma_from_daily("right")
      }) |>
        bindCache(wx_cache_key())

      gdd <- reactive({
        rx$wx()$daily |>
          build_gdd_from_daily()
      }) |>
        bindCache(wx_cache_key())

      ## selected_data // reactive ----
      selected_data <- reactive({
        opts <- list()
        opts$data_type <- req(input$data_type)
        if (opts$data_type == "ma") {
          opts$ma_align <- req(input$ma_align)
        }

        wx <- rx$wx()
        sites <- rx$sites() |>
          st_drop_geometry() |>
          select(
            site_id = id,
            site_name = name,
            site_lat = lat,
            site_lng = lng,
            grid_id
          )

        data <- switch(
          opts$data_type,
          "hourly" = wx$hourly,
          "daily" = wx$daily,
          "ma" = switch(
            opts$ma_align,
            "center" = ma_center(),
            "right" = ma_right()
          ),
          # "disease" = wx$disease,
          "gdd" = gdd()
        )

        req(nrow(data) > 0)

        df <- sites |>
          left_join(data, join_by(grid_id), relationship = "many-to-many") |>
          drop_na(grid_id, date) |>
          select(-grid_id) |>
          mutate(across(where(is.numeric), ~ signif(.x)))

        if (isFALSE(input$forecast)) {
          df <- if (opts$data_type == "hourly") {
            filter(df, datetime_utc < now() - hours(1))
          } else {
            filter(df, date <= today())
          }
        }
        if (input$metric) df else convert_measures(df)
      })

      # Interface ----

      ## main_ui ----
      output$main_ui <- renderUI({
        validate(need(rv$sites_ready, OPTS$validation_sites_ready))
        validate(need(rv$weather_ready, OPTS$validation_weather_ready))

        tagList(
          # shown if more than one site
          uiOutput(ns("plot_sites_ui")),
          div(
            class = "plotly-container",
            style = "margin-top: 1rem;",
            plotlyOutput(ns("data_plot"))
          ),
          # download button
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

      ## metric_switch ----
      output$switches_ui <- renderUI({
        div(
          class = "flex-across",
          materialSwitch(
            inputId = ns("metric"),
            label = "Use metric",
            value = isolate(input$metric) %||% FALSE,
            status = "primary"
          ),
          uiOutput(ns("forecast_switch"))
        )
      })

      ## forecast_switch ----
      output$forecast_switch <- renderUI({
        dates <- rx$dates()
        req(dates$end == today())
        div(
          style = "margin-left: 2rem;",
          materialSwitch(
            inputId = ns("forecast"),
            label = "Show forecast",
            value = input$forecast %||% TRUE,
            status = "primary"
          )
        )
      })

      ## data_type_ui ----
      output$data_type_ui <- renderUI({
        radioGroupButtons(
          inputId = ns("data_type"),
          label = "Dataset",
          choices = OPTS$data_type_choices,
          selected = isolate(input$data_type) %||%
            first(OPTS$data_type_choices),
          individual = TRUE,
          size = "sm"
        )
      })

      ## data_options ----
      output$data_options_ui <- renderUI({
        type <- req(input$data_type)

        # moving average type
        if (type == "ma") {
          div(
            class = "flex-across",
            style = "margin-top: 1rem;",
            div(tags$label("Moving average type:")),
            radioButtons(
              inputId = ns("ma_align"),
              label = NULL,
              choices = c("Centered" = "center", "Trailing" = "right"),
              selected = isolate(input$ma_align),
              inline = TRUE
            )
          )
        }
      })

      ## plot_cols - reactive ----
      plot_cols <- reactive({
        cols <- names(selected_data())
        cols <- cols[!(cols %in% OPTS$plot_ignore_cols)]
        set_names(cols, janitor::make_clean_names(cols, "title"))
      })

      ## plot_cols_ui ----
      output$plot_cols_ui <- renderUI({
        cols <- plot_cols()
        prev_selection <- intersect(cols, isolate(input$plot_cols))
        default_selection <- intersect(cols, OPTS$plot_default_cols)
        div(
          tags$label("Data to display"),
          div(
            class = "flex-across",
            div(
              style = "flex: 1;",
              selectizeInput(
                inputId = ns("plot_cols"),
                label = NULL,
                choices = cols,
                selected = first_truthy(
                  prev_selection,
                  default_selection,
                  cols[1]
                ),
                multiple = TRUE,
                options = list(plugins = list("remove_button"))
              )
            ),
            div(
              class = "reset-plot",
              actionLink(
                ns("reset_plot_cols"),
                icon("refresh"),
                title = "Reset to defaults"
              )
            )
          )
        )
      })

      ## Reset plot columns ----
      reset_plot_cols <- function() {
        cols <- plot_cols()
        default_col <- intersect(cols, OPTS$plot_default_cols)
        updateSelectizeInput(
          inputId = "plot_cols",
          selected = first_truthy(default_col, cols[1])
        )
      }

      # reset on button press
      observe(reset_plot_cols()) |> bindEvent(input$reset_plot_cols)

      ## plot_sites_ui ----
      output$plot_sites_ui <- renderUI({
        sites <- rx$sites()
        req(nrow(sites) > 1)

        choices <- set_names(
          sites$id,
          sprintf("%s: %s", sites$id, str_trunc(sites$name, 15))
        )
        selected <- isolate(input$plot_sites) %||% rv$selected_site

        checkboxGroupInput(
          inputId = ns("plot_sites"),
          label = "Sites to display",
          choices = choices,
          selected = selected,
          inline = TRUE
        )
      })

      ## change selection ----
      observe({
        selected <- rv$selected_site

        updateCheckboxGroupInput(
          session,
          inputId = "plot_sites",
          selected = selected
        )
      })

      ## plot_data ----
      plot_data <- reactive({
        df <- selected_data()
        sites <- rx$sites()
        dates <- rx$dates()

        req(nrow(sites) > 0)

        # build main options list from vars and inputs
        opts <- lst(
          multi_site = nrow(sites) > 1,
          dates = dates,
          date_range = c(
            ymd_hms(paste(dates$start, "00:00:00")),
            ymd_hms(paste(max(df$date), "23:00:00"))
          ),
          data_type = req(input$data_type),
          data_name = invert(OPTS$data_type_choices)[[data_type]],
          cols = req(input$plot_cols),
          unit_system = ifelse(input$metric, "metric", "imperial"),
          site_ids = unique(df$site_id),
          show_forecast = input$forecast
        )

        # if multiple sites filter by the ones selected in the UI
        if (opts$multi_site) {
          opts$selected_ids <- req(input$plot_sites)
          df <- filter(df, site_id %in% opts$selected_ids)
          sites <- filter(sites, id %in% opts$selected_ids)
        }

        req(nrow(df) > 0)
        req(all(opts$cols %in% names(df)))

        list(df = df, sites = sites, opts = opts)
      })

      ## data_plot - renderPlotly ----
      output$data_plot <- renderPlotly({
        data <- plot_data()
        data$opts$filename <- download_filename()$plot
        build_data_plot(data$df, data$sites, data$opts)
      })

      # Download button ----

      ## download_data - reactive ----
      download_data <- reactive({
        unit_system <- if_else(input$metric, "metric", "imperial")
        data <- plot_data()
        data$df |>
          rename_with_units(unit_system) |>
          mutate(across(
            where(is.POSIXct),
            ~ format(.x, "%Y-%m-%d %H:%M:%S")
          )) |>
          janitor::clean_names("big_camel")
      })

      ## download_filename - reactive ----
      # for both the csv download and plot png export
      download_filename <- reactive({
        type <- req(input$data_type)
        dates <- rx$dates()
        data <- plot_data()
        sites <- data$sites
        name_str <- invert(OPTS$data_type_choices)[[type]]
        site_str <- ifelse(
          nrow(sites) == 1,
          sprintf("(%.3f, %.3f)", sites$lat, sites$lng),
          "(multiple locations)"
        )
        date_str <- paste(dates$start, "to", dates$end)
        list(
          csv = paste(name_str, "data", site_str, "-", date_str),
          plot = paste(name_str, "plot", site_str, "-", date_str)
        )
      })

      ## download_data - downloadHandler ----
      output$download_data <- downloadHandler(
        filename = function() {
          paste0(download_filename()$csv, ".csv")
        },
        content = function(file) {
          download_data() |> write_excel_csv(file, na = "")
        }
      )
    } # end module
  )
}
