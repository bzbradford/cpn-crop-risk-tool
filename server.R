
server <- function(input, output, session) {

  # Reactive values ----

  ## rv ----
  rv <- reactiveValues(
    # IBM hourly weather, lightly modified
    weather = saved_weather,
    weather_ready = nrow(saved_weather) > 0,

    # can trigger a weather fetch
    fetch = NULL,

    # table storing site locations
    sites = sites_template,
    sites_ready = FALSE,

    # id of last-clicked site
    selected_site = 1,

    # last good date values
    start_date = OPTS$default_start_date,
    end_date = today(),
    dates_valid = TRUE,

    # sidebar site upload UI
    show_upload = FALSE, # toggle upload ui

    # which grids have been retrieved this session, for displaying on map
    grids = tibble(),

    # forecasts for sites
    # keyed by grid_id
    noaa_urls = list(),

    # keyed by forecast url
    noaa_forecasts = list(),

    # tell the map module to zoom to sites
    map_fit_sites_cmd = NULL,

    risk_last_value = tibble(),
  )

  ## rv$sites_ready ----
  # update sites_ready but only if different than existing value
  observe({
    ready <- nrow(rv$sites) > 0
    if (rv$sites_ready != ready) rv$sites_ready <- ready
  })

  ## rv$weather_ready ----
  # update weather_ready but only if different
  observe({
    ready <- nrow(rv$weather) > 0
    if (rv$weather_ready != ready) rv$weather_ready <- ready
  })


  ## rv$start_date ----
  observe({
    req(rv$dates_valid)
    rv$start_date <- req(input$start_date)
    rv$end_date <- req(input$end_date)
  })


  ## rv$dates_valid ----
  validate_dates <- function(start, end) {
    if (length(c(start, end)) < 2) return("Must provide start and end dates.")
    if (start > end) return("Start date must be before end date.")
    if ((end - start) > years(1)) return("Date range must be less than 1 year")
    NULL
  }

  observe({
    start <- input$start_date
    end <- input$end_date
    req(!is.null(start) & !is.null(end))

    msg <- validate_dates(start, end)
    rv$status_msg <- msg
    rv$dates_valid <- !isTruthy(msg)
  })



  # Reactives ----

  ## selected_dates ----
  # will block fetch button if invalid dates selected
  selected_dates <- reactive({
    dates <- list(
      start = req(rv$start_date),
      end = req(rv$end_date)
    )
  })

  ## expanded_dates ----
  expanded_dates <- reactive({
    dates <- selected_dates()
    dates$start <- dates$start - days(30)
    dates
  })



  ## wx_grids ----
  # create grids based on downloaded weather data
  wx_grids <- reactive({
    wx <- rv$weather
    req(nrow(wx) > 0)

    build_grids(wx)
  })


  ## sites_sf ----
  sites_sf <- reactive({
    sites <- rv$sites
    req(nrow(sites) > 0)

    sf <- sites %>%
      st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F)

    if (rv$weather_ready) {
      sf %>% st_join(wx_grids())
    } else {
      sf %>% mutate(grid_id = NA, grid_lat = NA, grid_lng = NA)
    }
  })


  ## wx_status ----
  # will be blocked when no weather or weather in date range
  wx_status <- reactive({
    wx <- rv$weather
    req(nrow(wx) > 0)
    dates <- selected_dates()

    weather_status(wx, dates$start, dates$end)
  })


  ## grids_with_status ----
  # will be blocked when no weather in date range
  grids_with_status <- reactive({
    status <- wx_status()
    wx_grids() %>%
      filter(grid_id %in% status$grid_id) %>%
      left_join(status, join_by(grid_id))
  })


  ## sites_with_status ----
  # will be blocked when no weather in date range
  sites_with_status <- reactive({
    req(sites_sf()) %>%
      left_join(wx_status(), join_by(grid_id)) %>%
      replace_na(list(needs_download = TRUE))
  })


  ## rv$grids handler ----
  # keep record of which grid_ids are associated with sites this session
  # used by forecasts and map
  observe({
    sites <- sites_sf()
    req(nrow(sites) > 0)

    sites <- sites %>%
      st_drop_geometry() %>%
      drop_na(grid_id) %>%
      distinct(grid_id, grid_lat, grid_lng)

    req(nrow(sites) > 0)

    rv$grids <- rv$grids %>%
      bind_rows(sites) %>%
      distinct(grid_id, grid_lat, grid_lng)
  })


  # Cookie handling ----

  set_cookie <- function(sites) {
    sites_json <- jsonlite::toJSON(sites)
    runjs(str_glue('setCookie({sites_json})'))
  }

  delete_cookie <- function() {
    runjs("deleteCookie()")
  }

  # on load read cookie data then start cookie writer
  observe({
    runjs("sendCookieToShiny()")

    cookie_writer <- observe({
      sites <- rv$sites
      if (nrow(sites) > 0) {
        set_cookie(sites)
      } else {
        delete_cookie()
      }
    })
  })

  ### Parse sites from cookie ----
  observeEvent(input$cookie, {
    cookie <- req(input$cookie)
    tryCatch({
      cookie_sites <- jsonlite::fromJSON(cookie)
      sites <- cookie_sites %>%
        select(all_of(names(sites_template))) %>%
        filter(validate_ll(lat, lng)) %>%
        distinct() %>%
        head(OPTS$max_sites) %>%
        mutate(id = row_number())

      req(nrow(sites) > 0)

      rv$sites <- sites
      rv$selected_site <- first(sites$id)
      rv$map_cmd <- "fit_sites"

      showNotification(paste("Loaded", nrow(sites), ifelse(nrow(sites) == 1, "site", "sites"), "from a previous session."))

      # trigger weather fetch after a second
      delay(1000, {
        rv$fetch <- runif(1)
      })
    }, error = function(e) {
      message("Failed to read sites from cookie: ", e)
      delete_cookie()
    })
  })


  # NOAA forecasts ----

  task_get_forecasts <- ExtendedTask$new(function(grids, cur_urls, cur_forecasts) {
    message("Getting forecasts...")
    suppressWarnings({
      future_promise({
        urls <- cur_urls %||% list()
        for (i in 1:nrow(grids)) {
          grid <- grids[i,]
          if (isTruthy(urls[[grid$grid_id]])) next # already have it
          urls[[grid$grid_id]] <- noaa_get_forecast_url(grid$grid_lat, grid$grid_lng)
        }

        forecasts <- cur_forecasts %||% list()
        for (url in unique(urls)) {
          if (!isTruthy(url) || url == "404") next # bad url
          if (isTruthy(forecasts[[url]])) next # already have it
          forecasts[[url]] <- noaa_get_forecast(url = url)
        }

        list(urls = urls, forecasts = forecasts)
      }, seed = NULL)
    })

  })

  observe({
    req(getOption("forecast", TRUE))

    grids <- rv$grids
    req(nrow(grids) > 0)
    urls <- isolate(rv$noaa_urls)
    forecasts <- isolate(rv$noaa_forecasts)
    task_get_forecasts$invoke(grids, urls, forecasts)
  })

  observe({
    res <- req(task_get_forecasts$result())
    rv$noaa_urls <- res$urls
    rv$noaa_forecasts <- res$forecasts
  })


  # Weather data ----

  ## wx_forecasts ----
  wx_forecasts <- reactive({
    sites <- sites_sf()
    req(nrow(sites) > 0)

    grids <- sites %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      distinct(grid_id, grid_lat, grid_lng, time_zone)

    urls <- rv$noaa_urls
    fcs <- rv$noaa_forecasts

    if (length(fcs) == 0) {
      # message("no forecasts")
      return(tibble())
    }

    if (!(any(names(urls) %in% grids$grid_id))) {
      # message('no forecasts for selected sites')
      return(tibble())
    }

    fc_data <- bind_rows(fcs, .id = "url")
    fc <- tibble(
      grid_id = names(urls),
      url = unlist(urls)
    ) %>%
      left_join(fc_data, join_by(url)) %>%
      select(-url)

    df <- grids %>%
      drop_na(time_zone) %>%
      left_join(fc, join_by(grid_id))

    # make sure the forecasts actually joined
    if (!("datetime_utc" %in% names(df))) return(tibble())

    df %>%
      mutate(datetime_local = with_tz(datetime_utc, first(time_zone)), .by = time_zone, .after = time_zone) %>%
      mutate(date = as_date(datetime_local), .after = datetime_local) %>%
      add_date_cols()
  })

  # observe(echo(wx_forecasts()))


  ## wx_args ----
  # all inputs to the wx_data function
  wx_args <- reactive(lst(
    weather = rv$weather,
    sites = sites_with_status(),
    selected_dates = selected_dates(),
    fetched_dates = expanded_dates(),
    dates = list(
      start = selected_dates$start,
      end = selected_dates$end,
      today = today()
    ),
    forecast = if (dates$end == today()) wx_forecasts() else tibble()
  ))

  # observe(echo(wx_args()))


  ## wx_data ----
  #' will be blocked when no weather
  #' will return only sites/dates/hourly when no weather in date range
  wx_data <- reactive({
    args <- wx_args()
    list2env(args, envir = environment())

    req(nrow(weather) > 0)
    req(nrow(sites) > 0)

    wx <- list()
    wx$sites <- sites
    wx$dates <- dates

    # allow up to 30 days before selected start date
    hourly_full <- weather %>%
      filter(grid_id %in% sites$grid_id) %>%
      filter(between(date, fetched_dates$start, fetched_dates$end)) %>%
      bind_rows(forecast) %>%
      distinct(grid_id, datetime_local, .keep_all = TRUE) %>%
      arrange(grid_id, datetime_local)

    # remove the earlier dates that were used for moving averages
    wx$hourly <- hourly_full %>%
      filter(date >= dates$start) %>%
      mutate(
        precip_cumulative = cumsum(precip),
        snow_cumulative = cumsum(snow),
        .by = grid_id
      ) %>%
      relocate(precip_cumulative, .after = precip) %>%
      relocate(snow_cumulative, .after = snow)

    if (nrow(hourly_full) == 0 || nrow(wx$hourly) == 0) return(wx)

    wx$daily_full <- build_daily(hourly_full)
    wx$daily <- wx$daily_full %>% filter(date >= dates$start)

    wx
  }) %>%
    bindCache(rlang::hash(wx_args()))

  # observe(echo(wx_data()))


  # Help modal --------------------------------------------------------------------

  observeEvent(input$about, show_modal(md = "README.md"))
  observe({
    mod <- req(input$show_modal)
    show_modal(md = mod$md, title = mod$title)
  })



# Site selection ----------------------------------------------------------

  ## site_help_ui ----
  output$site_help_ui <- renderUI({
    sites <- rv$sites
    n <- nrow(sites)
    str <- if (n == 0) {
      "You don't have any sites. Click on the map or use the search boxes at the bottom of the map to set a location."
    } else if (n == OPTS$max_sites) {
      "Edit or delete a site using the pen or trash icons."
    } else {
      "Edit or delete a site using the pen or trash icons. Click on the map or use the search boxes to add another location."
    }
    if (n > 10) {
      str <- paste(str, "<i>Note: App may be slower when many sites are added.</i>")
    }

    p(style = "font-size: small", HTML(str))
  })

  ## sites_tbl_data ----
  # sites formatted for DT
  sites_dt_data <- reactive({
    req(rv$sites) %>%
      mutate(
        id = as.character(id),
        # across(c(lat, lng), ~sprintf("%.2f", .x)),
        loc = sprintf("%.2f, %.2f", lat, lng),
        btns = paste0(
          "<div style='display:inline-flex; gap:10px; padding: 5px;'>",
          site_action_link("edit", id, name),
          site_action_link("trash", id),
          "</div>"
        ) %>% lapply(HTML)
      ) %>%
      select(id, name, loc, btns)
  })

  ## sites_dt ----
  # render initial DT
  output$sites_dt <- renderDT({
    # sites <- isolate(sites_dt_data())
    template <- tibble(
      id = numeric(),
      name = character(),
      loc = character(),
      btns = character()
    )
    # selected <- isolate(rv$selected_site)
    dt <- datatable(
      template,
      colnames = c("", "Name", "GPS", "Edit"),
      rownames = FALSE,
      selection = "none",
      class = "compact",
      options = list(
        dom = "t",
        ordering = FALSE,
        paging = FALSE,
        scrollX = TRUE,
        scrollCollapse = TRUE,
        columnDefs = list(
          list(width = "5%", targets = 0),
          list(width = "40%", targets = 1),
          list(width = "25%", targets = 2),
          list(width = "50px", targets = 3),
          list(className = "dt-right", targets = 0),
          list(className = "dt-center tbl-coords", targets = 2),
          list(className = "dt-right", targets = 3)
        )
      )
    ) %>%
      formatStyle(0:3, lineHeight = "1rem", textWrap = "nowrap")

    dt_observer$resume()

    dt
  })

  ## Handle DT update ----
  dt_observer <- observe({
    selected_id <- rv$selected_site
    df <- sites_dt_data() %>%
      mutate(id = if_else(id == selected_id, paste0(">", id), as.character(id)))

    dataTableProxy("sites_dt") %>%
      replaceData(df, rownames = FALSE, clearSelection = "none")
  }, suspended = TRUE)

  # observe(print(paste(names(input))))

  # select clicked site
  observe({
    req(rv$sites_ready)
    click <- req(input$sites_dt_cell_clicked)
    row <- req(click$row)
    req(row %in% rv$sites$id)

    rv$selected_site <- row
  })

  # highlight selected site
  # observe({
  #   selected <- req(rv$selected_site)
  #   runjs("$('#sites_dt table.dataTable tr').removeClass('selected')")
  #   runjs(sprintf("$('#sites_dt table.dataTable tr:nth-child(%s)').addClass('selected')", selected))
  # })

  ## Handle trash_site button ----
  observeEvent(input$trash_site, {
    to_delete_id <- req(input$trash_site)
    rv$sites <- rv$sites %>% filter(id != to_delete_id)
  })

  ## Handle edit_site button ----
  observeEvent(input$edit_site, {
    edits <- req(input$edit_site)
    sites <- rv$sites
    sites$name[edits$id] <- edits$name
    rv$sites <- sites
  })


  ## Site list buttons ----

  ### site_btns // renderUI ----
  output$site_btns <- renderUI({
    # btn <- function(id, label, ...) actionButton(id, label, class = "btn-sm", ...)
    sites <- isolate(rv$sites)
    div(
      style = "margin-top: 10px;",
      div(
        class = "flex-across",
        # btn("load_example", "Test sites"),
        actionButton(
          "upload_csv", "Upload csv",
          class = sprintf(
            "btn-sm btn-%s",
            ifelse(isTruthy(rv$show_upload), "primary", "default")
          )
        ),
        actionButton(
          "clear_sites", "Clear sites",
          class = "btn-sm",
          disabled = nrow(sites) == 0
        ),
        if (nrow(sites) == 0) {
          downloadButton("export_sites", "Export sites", class = "btn-sm", disabled = TRUE)
        } else {
          downloadButton("export_sites", "Export sites", class = "btn-sm")
        }
      )
    )
  })

  # disable buttons when no sites
  observe({
    sites <- rv$sites
    if (nrow(sites) == 0) {
      disable("clear_sites")
      disable("export_sites")
    } else {
      enable("clear_sites")
      enable("export_sites")
    }
  })


  ## Site csv upload ----

  observe({
    rv$show_upload <- !rv$show_upload
  }) %>% bindEvent(input$upload_csv)


  ### file_upload_ui ----

  output$file_upload_ui <- renderUI({
    req(rv$show_upload)

    div(
      style = "margin-top: 1rem;",
      tags$label("Upload csv"), br(),
      em(
        paste("Upload a csv with columns: name, lat/latitude, lng/long/longitude. Latitude and longitude must be in +/- decimal degrees. Maximum of", OPTS$max_sites, "sites.")
      ),
      div(
        style = "margin-top: 10px;",
        fileInput(
          inputId = "sites_csv",
          label = NULL,
          accept = ".csv"
        ),
      ),
      { if (!is.null(rv$upload_msg)) div(class = "shiny-error", rv$upload_msg) }
    )
  })

  observe({
    upload <- req(input$sites_csv)
    tryCatch({
      new_sites <- load_sites(upload$datapath)
      rv$sites <- new_sites
      rv$selected_site <- first(new_sites$id)
      rv$map_cmd <- "fit_sites"
      rv$show_upload <- FALSE
      rv$upload_msg <- NULL
    }, error = function(e) {
      message("File upload error: ", e)
      rv$upload_msg = "Failed to load sites from csv, please try again."
    })
  }) %>% bindEvent(input$sites_csv)

  ### Handle test site load ----
  # observe({
  #   rv$sites <- load_sites("example-sites.csv")
  #   fit_sites()
  # }) %>% bindEvent(input$load_example)

  ### Handle clear_sites button ----
  observeEvent(input$clear_sites, {
    shinyalert(
      text = "Are you sure you want to delete all your sites?",
      type = "warning",
      closeOnClickOutside = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Yes",
      confirmButtonCol = "#008bb6",
      cancelButtonText = "Cancel",
      callbackR = function(confirmed) {
        if (confirmed) {
          rv$sites <- sites_template
          rv$selected_site <- 1
          delete_cookie()
        }
      }
    )
  })

  ### Handle export_sites button ----
  output$export_sites <- downloadHandler(
    "Sites.csv", function(file) {
      sites <- rv$sites
      req(nrow(sites) > 0)
      sites %>%
        select(id, name, lat, lng) %>%
        write_csv(file, na = "")
    }
  )



# Date selection ----------------------------------------------------------

  ### date_select_ui ----
  output$date_select_ui <- renderUI({
    div(
      class = "flex-across",
      style = "row-gap: 0px; padding-top: 1rem;",
      div(
        style = "flex: 1 0; min-width: 120px",
        dateInput(
          inputId = "start_date",
          label = "Start date:",
          min = OPTS$earliest_date,
          max = today(),
          value = OPTS$default_start_date,
          width = "100%"
        )
      ),
      div(
        style = "flex: 1 0; min-width: 120px;",
        dateInput(
          inputId = "end_date",
          label = "End date:",
          min = OPTS$earliest_date,
          max = today(),
          value = today(),
          width = "100%"
        )
      )
    )
  })


  ## date_presets // reactive ----
  # creates the named set of dates for the date preset buttons
  date_presets <- reactive({
    d <- today()
    y <- year(d)
    jan1 <- make_date(y, 1, 1)
    apr1 <- make_date(y, 4, 1)
    nov1 <- make_date(y, 11, 1)
    dec31 <- make_date(y, 12, 31)
    list(
      "past_week" = c(d - 7, d),
      "past_month" = c(d - 30, d),
      "this_season" = c(min(d, apr1), min(d, nov1)),
      "this_year" = c(jan1, d),
      # "fullyear" = c(d - 365, d),
      "last_year" = c(jan1 - years(1), dec31 - years(1)),
      "last_season" = c(apr1 - years(1), nov1 - years(1))
    )
  })

  ## date_btns_ui ----
  # create a date button element
  date_btn <- function(value, label, btn_class = c("default", "primary")) {
    btn_class <- match.arg(btn_class)
    HTML(str_glue("<button class='btn btn-{btn_class} btn-xs action-button' onclick=\"this.blur(); Shiny.setInputValue('date_preset', '{value}', {{priority: 'event'}});\">{label}</button>"))
  }

  # create the date buttons component
  output$date_btns_ui <- renderUI({
    cur_dates <- as.Date(c(rv$start_date, rv$end_date))
    presets <- date_presets()

    div(
      class = "date-btns",
      lapply(names(presets), function(name) {
        value <- presets[[name]]
        label <- snakecase::to_sentence_case(name)
        selected <- setequal(cur_dates, value)
        date_btn(name, label, btn_class = ifelse(selected, "primary", "default"))
      })
    )
  })

  ## Handle date buttons ----
  observeEvent(input$date_preset, {
    val <- input$date_preset
    presets <- date_presets()
    if (val %in% names(presets)) {
      dates <- presets[[val]]
      updateDateInput(inputId = "start_date", value = dates[1])
      updateDateInput(inputId = "end_date", value = dates[2])
    } else {
      warning("Unknown date preset '", val, "'")
    }
  })



# Fetch weather button ----------------------------------------------------

  ## need_weather // reactive ----
  need_weather <- reactive({
    wx <- rv$weather
    if (is.null(wx)) return(TRUE)
    if (nrow(wx) == 0) return(TRUE)
    if (nrow(rv$sites) == 0) return(FALSE)
    sites <- sites_with_status()
    if (anyNA(sites$grid_id)) return(TRUE)
    if (any(sites$needs_download)) return(TRUE)
    FALSE
  })

  # force a weather fetch if it's needed and hasn't been triggered in 10 seconds
  observe({
    req(wx_args())
    req(need_weather())

    # message('Auto-fetching weather data in 15 seconds...')
    delay(15000, {
      # message("Auto-fetching weather data...")
      req(need_weather())
      rv$fetch <- runif(1)
    })
  })

  ## action_ui ----
  output$action_ui <- renderUI({
    btn <- function(msg, ...) {
      div(class = "submit-btn", actionButton("fetch", msg, ...))
    }

    # used to promt button to regenerate
    rv$action_nonce

    # control button appearance
    if (nrow(rv$sites) == 0) return(btn("No sites selected", disabled = TRUE))
    if (!rv$dates_valid) return(btn("Invalid date selection", disabled = TRUE))
    if (need_weather()) return(btn("Fetch weather"))
    btn("Everything up to date", class = "btn-primary", disabled = TRUE)
  })

  ## status_ui ----
  # reports to user if there's a problem with weather fetching
  output$status_ui <- renderUI({
    msg <- req(rv$status_msg)
    div(
      class = "shiny-output-error",
      style = "margin-top: 5px; padding: 10px;",
      msg
    )
  })

  ## fetch_args // reactive ----
  # record coordinates and dates queried from IBM to avoid duplicate attempts
  # uses the grid lat/lng instead of site lat/lng if available
  fetch_args <- reactive({
    sites <- req(sites_sf()) %>%
      st_drop_geometry() %>%
      mutate(
        lat = coalesce(grid_lat, lat),
        lng = coalesce(grid_lng, lng)
      ) %>%
      distinct(lat, lng)

    date_range <- expanded_dates()

    list(
      sites = sites,
      start_date = date_range$start,
      end_date = date_range$end
    )
  })

  ## already_fetched // reactive ----
  # already_fetched <- reactive({
  #   args <- fetch_args()
  #   rlang::hash(args) %in% rv$fetch_hashes
  # })

  ## Handle fetching weather ----
  do_fetch_weather <- function() {
    args <- fetch_args()
    disable("fetch")
    runjs("$('#fetch').html('Downloading weather...')")

    withProgress(
      message = "Downloading weather...",
      value = 0, min = 0, max = 2,
      {
        msg <- do.call(fetch_weather, args)
        rv$status_msg <- msg
      }
    )

    rv$action_nonce <- runif(1) # regenerates the action button
    rv$weather <- saved_weather
    rv$fetch_hashes <- c(rv$fetch_hashes, rlang::hash(args))
  }

  # fetch on button click
  observeEvent(input$fetch, {
    do_fetch_weather()
  })

  # fetch on program trigger
  observeEvent(rv$fetch, {
    do_fetch_weather()
  })


# Module servers ----------------------------------------------------------

  mapServer(
    rv = rv,
    map_data = reactive(list(
      grids = wx_grids(),
      grids_with_status = grids_with_status(),
      sites_with_status = sites_with_status()
    ))
  )


  ## Data tab ----

  dataServer(
    wx_data = reactive(wx_data()),
    selected_site = reactive(rv$selected_site),
    sites_ready = reactive(rv$sites_ready)
  )


  ## Disease models tab ----

  riskServer(
    rv = rv,
    wx_data = reactive(wx_data())
  )

}
