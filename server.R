#--- main server ---#

server <- function(input, output, session) {
  # Startup and cookie handling ------------------------------------------------

  # cookie has .userId and .sites keys
  read_cookie <- function() {
    runjs("sendCookieToShiny();")
  }

  # set the .sites key on the cookie
  set_cookie <- function(sites) {
    sites_json <- jsonlite::toJSON(sites)
    runjs(str_glue("updateCookie({{sites: {sites_json}}})"))
  }

  # clear out the .sites key on the cookie
  clear_cookie <- function() {
    set_cookie(tibble())
  }

  ## Initialize cookie ----
  # on startup read cookie data then start cookie writer
  observe({
    read_cookie()

    cookie_writer <- observe({
      sites <- rv$sites
      set_cookie(sites)
    })
  })

  ## Parse sites from cookie ----
  observeEvent(input$cookie, {
    cookie <- req(input$cookie)
    cookie_sites <- req(cookie$sites)
    sites <- parse_cookie_sites(cookie_sites)
    req(sites)

    rv$sites <- sites
    rv$selected_site <- first(sites$id)
    rv$map_cmd <- "fit_sites"

    showNotification(paste(
      "Loaded",
      nrow(sites),
      ifelse(nrow(sites) == 1, "site", "sites"),
      "from a previous session."
    ))

    # trigger weather fetch after a second
    delay(1000, {
      rv$fetch <- runif(1)
    })
  })

  # based on user ID which is set by javascript on the client
  cache_file <- reactive({
    cookie <- req(input$cookie)

    tryCatch(
      {
        user_id <- cookie[["userId"]]
        req(length(user_id) > 0)
        get_cache_file(user_id)
      },
      error = function(e) {
        message("Failed to read user ID from cookie: ", e)
        echo(cookie)
      }
    )
  })

  # observe(echo(cache_file()))

  ## Read cached weather ----
  observe({
    fname <- cache_file()
    req(file.exists(fname))

    tryCatch(
      {
        wx <- read_fst(fname) |> as_tibble()
        if (!("precipitation" %in% names(wx))) {
          message("Cache schema mismatch (pre-OM schema), discarding: ", fname)
          file.remove(fname)
        } else {
          rv$weather <- wx
        }
      },
      error = function(e) {
        message("Failed to read cache file '", fname, "'")
        file.remove(fname)
      }
    )
  }) |>
    bindEvent(cache_file())

  ## Write weather to cache ----
  observe({
    wx <- rv$weather
    fname <- cache_file()
    tryCatch(
      {
        write_fst(wx, fname, compress = 99)
      },
      error = function(e) {
        message("Could not write cache file '", fname, "': ", e)
      }
    )
  }) |>
    bindEvent(rv$weather)

  # Reactive values ------------------------------------------------------------

  ## rv ----
  rv <- reactiveValues(
    weather = tibble(),
    weather_ready = FALSE,

    # can trigger a weather fetch
    fetch = NULL,

    # last forecast fetch timestamp per site_id
    forecast_fetched_at = list(),

    # per-site forecast data, list keyed by site_id (as character)
    forecasts = list(),

    # table storing site locations
    sites = sites_template,
    sites_ready = FALSE,

    # id of last-clicked site
    selected_site = 1,

    # last good date values
    start_date = OPTS$default_start_date,
    end_date = today(),
    dates_valid = TRUE,
    start_date_setter = NULL,

    # sidebar site upload UI
    show_upload = FALSE,

    # tell the map module to zoom to sites
    map_fit_sites_cmd = NULL,

    # error message displayed under fetch button
    status_msg = NULL,

    # fetch timer
    fetch_timer = now(),
    fetch_timer_active = FALSE,
  )

  ## rv$status helper ----
  set_status <- function(msg = NULL, type = c("error", "info")) {
    rv$status <- if (is.null(msg)) {
      NULL
    } else {
      list(
        msg = msg,
        type = match.arg(type)
      )
    }
  }

  ## rv$sites_ready ----
  # update sites_ready but only if different than existing value
  observe({
    ready <- nrow(rv$sites) > 0
    if (rv$sites_ready != ready) {
      rv$sites_ready <- ready
    }
  })

  ## rv$weather_ready ----
  # update weather_ready but only if different
  observe({
    ready <- nrow(rv$weather) > 0
    if (rv$weather_ready != ready) {
      rv$weather_ready <- ready
    }
  })

  ## rv$start_date ----
  observe({
    req(rv$dates_valid)
    rv$start_date <- req(input$start_date)
    rv$end_date <- req(input$end_date)
  })

  # update start date input
  observe({
    new_start_date <- req(rv$start_date_setter)
    updateDateInput(inputId = "start_date", value = new_start_date)
  })

  ## rv$dates_valid ----
  validate_dates <- function(start, end) {
    if (length(c(start, end)) < 2) {
      return("Must provide start and end dates.")
    }
    if (start > end) {
      return("Start date must be before end date.")
    }
    if ((end - start) > years(1)) {
      return("Date range must be less than 1 year")
    }
    NULL
  }

  observe({
    start <- input$start_date
    end <- input$end_date
    req(!is.null(start) & !is.null(end))

    msg <- validate_dates(start, end)
    set_status(msg)
    rv$dates_valid <- !is_truthy(msg)
  })

  ## rv$map_risk_data ----
  # used by the map to render pin colors
  # set by the risk module
  # cleared here when sites change
  observe({
    sites <- rv$sites
    rv$map_risk_data <- NULL
  })

  # Reactives ------------------------------------------------------------------

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
  # sf of grid polygons derived from downloaded weather data
  wx_grids <- reactive({
    wx <- rv$weather
    req(nrow(wx) > 0)
    om_build_grids(wx)
  })

  ## grid_status ----
  # per-grid date range, completeness, and freshness
  grid_status <- reactive({
    req(nrow(rv$weather) > 0)
    sel_dates <- selected_dates()

    om_grid_status(
      rv$weather,
      start_date = sel_dates$start,
      end_date = sel_dates$end
    )
  })

  # observe({
  #   rv$weather |> write_csv("dev/test_wx.csv")
  #   echo(grid_status())
  # })

  ## sites_with_status ----
  # sites joined to grid status via non-spatial bbox join
  sites_with_status <- reactive({
    sites <- rv$sites
    if (nrow(sites) == 0) {
      return(tibble())
    }
    if (nrow(rv$weather) == 0) {
      return(sites |> mutate(needs_download = TRUE))
    }
    om_join_grids(sites, grid_status()) |>
      replace_na(list(needs_download = TRUE))
  })

  # observe(echo(sites_with_status()))

  ## need_weather ----
  need_weather <- function() {
    if (nrow(rv$sites) == 0) {
      return(FALSE)
    }
    dates <- expanded_dates()
    status <- if (nrow(rv$weather) > 0) grid_status() else NULL
    reqs <- om_prep_reqs(rv$sites, dates$start, dates$end, status)
    if (nrow(reqs) > 0) {
      return(TRUE)
    }
    if (dates$end >= today()) {
      fc_fetched_at <- rv$forecast_fetched_at
      for (sid in rv$sites$id) {
        fetched_at <- fc_fetched_at[[as.character(sid)]]
        if (
          is.null(fetched_at) ||
            as.numeric(now() - fetched_at, units = "hours") > 1
        ) {
          return(TRUE)
        }
      }
    }
    FALSE
  }

  # Open-Meteo Weather Extended Task -------------------------------------------

  task_get_weather <- ExtendedTask$new(function(
    sites,
    start_date,
    end_date,
    wx,
    fc_fetched_at
  ) {
    mirai(
      {
        start_date <- as.Date(start_date)
        end_date <- min(as.Date(end_date), today())

        # Fetch new historical data and merge with existing
        new_wx <- om_fetch_weather(sites, start_date, end_date, wx)
        merged_wx <- om_merge_wx(wx, new_wx)

        # Fetch forecasts for sites with stale or missing forecasts
        sites_need_fc <- if (end_date >= today()) {
          sites |>
            filter(map_lgl(id, function(sid) {
              fetched_at <- fc_fetched_at[[as.character(sid)]]
              is.null(fetched_at) ||
                as.numeric(now() - fetched_at, units = "hours") > 1
            }))
        } else {
          sites[0, ]
        }

        new_fc <- list()
        new_fc_fetched_at <- fc_fetched_at

        if (nrow(sites_need_fc) > 0) {
          fc_result <- om_fetch_forecast(sites_need_fc)
          if (nrow(fc_result) > 0) {
            for (sid in unique(fc_result$site_id)) {
              key <- as.character(sid)
              new_fc[[key]] <- fc_result |> filter(site_id == sid)
              new_fc_fetched_at[[key]] <- now()
            }
          }
        }

        if (nrow(merged_wx) == 0 && length(new_fc) == 0) {
          return(NULL)
        }

        list(wx = merged_wx, fc = new_fc, fc_fetched_at = new_fc_fetched_at)
      },
      .GlobalEnv,
      .args = lst(sites, start_date, end_date, wx, fc_fetched_at)
    )
  })

  ## Invoke weather request ----
  invoke_get_weather <- function() {
    set_status()
    req(task_get_weather$status() != "running")
    req(need_weather())

    dates <- expanded_dates()
    disable("fetch")
    runjs("$('#fetch').html('Downloading weather...')")
    task_get_weather$invoke(
      rv$sites,
      dates$start,
      dates$end,
      rv$weather,
      rv$forecast_fetched_at
    )
  }

  # fetch on button click
  observeEvent(input$fetch, {
    invoke_get_weather()
  })

  # fetch on program trigger
  observeEvent(rv$fetch, {
    invoke_get_weather()
  })

  ## Handle weather request response ----
  observe({
    req(task_get_weather$status() == "success")
    res <- task_get_weather$result()

    rv$action_nonce <- runif(1) # regenerates the action button

    if (is.null(res)) {
      contact_us <- sprintf(
        "<a href='mailto:%s?subject=CPN Tool problem'>contact us</a>",
        OPTS$contact_email
      )
      err <- HTML(
        "Failed to get weather data from Open-Meteo. Please try again. If the problem persists",
        contact_us,
        "to report the issue."
      )
      set_status(err)
    } else {
      set_status()
      if (!is.null(res$wx) && nrow(res$wx) > 0) {
        rv$weather <- res$wx
      }
      if (length(res$fc) > 0) {
        fc <- rv$forecasts
        for (sid in names(res$fc)) {
          fc[[sid]] <- res$fc[[sid]]
        }
        rv$forecasts <- fc
      }
      rv$forecast_fetched_at <- res$fc_fetched_at
    }
  })

  # observe(echo(rv$forecasts))

  # Weather data ---------------------------------------------------------------

  ## wx_data ----
  wx_data <- reactive({
    weather <- rv$weather
    forecasts <- rv$forecasts
    sites <- sites_with_status()
    sel_dates <- selected_dates()

    req(nrow(weather) > 0, nrow(sites) > 0)

    fetch_start <- sel_dates$start - days(30)

    historical <- weather |>
      filter(
        grid_id %in% sites$grid_id,
        between(date, fetch_start, sel_dates$end)
      )

    # Per-site forecasts re-tagged with the site's historical grid_id
    site_grid_map <- sites |>
      filter(!is.na(grid_id)) |>
      select(site_id = id, grid_id)

    fc_rows <- imap(forecasts, function(fc_data, sid) {
      if (is.null(fc_data) || nrow(fc_data) == 0) {
        return(NULL)
      }
      hist_grid <- site_grid_map |>
        filter(site_id == as.integer(sid)) |>
        pull(grid_id)
      if (length(hist_grid) == 0) {
        return(NULL)
      }
      fc_data |>
        # filter(between(date, fetch_start, sel_dates$end)) |>
        mutate(grid_id = hist_grid) |>
        select(-any_of("site_id"))
    }) |>
      bind_rows()

    # includes 30 days prior to selected date
    hourly_full <- bind_rows(historical, fc_rows) |>
      drop_na(datetime_utc) |>
      arrange(grid_id, datetime_utc) |>
      distinct(grid_id, datetime_utc, .keep_all = TRUE)

    daily_full <- build_daily(hourly_full)

    hourly <- hourly_full |>
      filter(date >= sel_dates$start) |>
      group_by(grid_id) |>
      mutate(
        precipitation_cumulative = cumsum(precipitation),
        .after = precipitation
      ) |>
      mutate(
        snowfall_cumulative = cumsum(snowfall),
        .after = snowfall
      ) |>
      ungroup()

    list(
      sites = sites,
      dates = list(
        start = sel_dates$start,
        end = sel_dates$end,
        today = today()
      ),
      hourly = hourly,
      daily_full = daily_full,
      daily = daily_full |> filter(date >= sel_dates$start)
    )
  })

  # observe(echo(wx_data()))

  # Help modal -----------------------------------------------------------------

  observeEvent(input$about, show_modal(md = "README.md"))
  observe({
    mod <- req(input$show_modal)
    show_modal(md = mod$md)
  })

  # Site selection -------------------------------------------------------------

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
      str <- paste(
        str,
        "<i>Note: App may be slower when many sites are added.</i>"
      )
    }

    p(style = "font-size: small", HTML(str))
  })

  ## sites_tbl_data ----
  # sites formatted for DT
  sites_dt_data <- reactive({
    req(rv$sites) |>
      mutate(
        id = as.character(id),
        # across(c(lat, lng), ~sprintf("%.2f", .x)),
        loc = sprintf("%.2f, %.2f", lat, lng),
        btns = paste0(
          "<div style='display:inline-flex; gap:10px; padding: 5px;'>",
          site_action_link("edit", id, name),
          site_action_link("trash", id),
          "</div>"
        ) |>
          lapply(HTML)
      ) |>
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
    ) |>
      formatStyle(0:3, lineHeight = "1rem", textWrap = "nowrap")

    dt_observer$resume()

    dt
  })

  ## Handle DT update ----
  dt_observer <- observe(
    {
      selected_id <- rv$selected_site
      df <- sites_dt_data() |>
        mutate(
          id = if_else(id == selected_id, paste0(">", id), as.character(id))
        )

      dataTableProxy("sites_dt") |>
        replaceData(df, rownames = FALSE, clearSelection = "none")
    },
    suspended = TRUE
  )

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
    rv$sites <- rv$sites |> filter(id != to_delete_id)
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
    sites <- isolate(rv$sites)
    n <- nrow(sites)

    div(
      style = "margin-top: 10px;",
      div(
        class = "flex-across",
        # btn("load_example", "Test sites"),
        actionButton(
          "upload_csv",
          "Upload",
          icon = icon("upload"),
          class = sprintf(
            "btn-sm btn-%s",
            ifelse(
              is_truthy(rv$show_upload),
              "primary",
              "default"
            )
          )
        ),
        actionButton(
          "sort_sites",
          "Sort",
          icon = icon("sort"),
          class = "btn-sm",
          disabled = n <= 1
        ),
        actionButton(
          "clear_sites",
          "Clear",
          icon = icon("trash"),
          class = "btn-sm",
          disabled = n == 0
        ),
        downloadButton(
          "export_sites",
          "Export",
          class = "btn-sm",
          disabled = n == 0
        )
      )
    )
  })

  # disable buttons when no sites
  observe({
    sites <- rv$sites
    n <- nrow(sites)

    if (n > 0) {
      enable("clear_sites")
    } else {
      disable("clear_sites")
    }
    if (n > 0) {
      enable("export_sites")
    } else {
      disable("export_sites")
    }
    if (n > 1) {
      enable("sort_sites")
    } else {
      disable("sort_sites")
    }
  })

  ## Site csv upload ----

  observe({
    rv$show_upload <- !rv$show_upload
  }) |>
    bindEvent(input$upload_csv)

  ### file_upload_ui ----

  output$file_upload_ui <- renderUI({
    req(rv$show_upload)

    div(
      style = "margin-top: 1rem;",
      tags$label("Upload csv"),
      br(),
      em(
        paste(
          "Upload a csv with columns: name, lat/latitude, lng/long/longitude. Latitude and longitude must be in +/- decimal degrees. Maximum of",
          OPTS$max_sites,
          "sites."
        )
      ),
      div(
        style = "margin-top: 10px;",
        fileInput(
          inputId = "sites_csv",
          label = NULL,
          accept = ".csv"
        ),
      ),
      {
        if (!is.null(rv$upload_msg)) {
          div(class = "shiny-error", rv$upload_msg)
        }
      }
    )
  })

  ## Handle uploaded sites csv ----
  observe({
    upload <- req(input$sites_csv)
    tryCatch(
      {
        new_sites <- load_sites(upload$datapath)
        rv$sites <- new_sites
        rv$selected_site <- first(new_sites$id)
        rv$map_cmd <- "fit_sites"
        rv$show_upload <- FALSE
        rv$upload_msg <- NULL
      },
      error = function(e) {
        message("File upload error: ", e)
        rv$upload_msg <- "Failed to load sites from csv, please try again."
      }
    )
  }) |>
    bindEvent(input$sites_csv)

  ### Handle test site load ----
  # observe({
  #   rv$sites <- load_sites("example-sites.csv")
  #   fit_sites()
  # }) |> bindEvent(input$load_example)

  ## Handle clear_sites button ----
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
          clear_cookie()
        }
      }
    )
  })

  ## Handle sort_sites button ----
  observeEvent(input$sort_sites, {
    sort_categories <- list(
      list(
        label = "By name:",
        options = c("a_z", "z_a")
      ),
      list(
        label = "By location:",
        options = c(
          "n_s",
          "s_n",
          "w_e",
          "e_w",
          "sw_ne",
          "nw_se",
          "ne_sw",
          "se_nw"
        )
      )
    )

    # Generate sort categories
    sort_types <- lapply(sort_categories, function(category) {
      buttons <- lapply(category$options, function(option) {
        opts <- toupper(str_split_1(option, "_"))
        label <- span(
          style = "display: inline-flex; gap: 5px; align-items: baseline;",
          opts[1],
          icon("arrow-right"),
          opts[2]
        )
        actionButton(
          paste0("sort_sites_", option),
          label,
          class = "btn-sm",
          onclick = paste0("sendShiny('sort_sites_by', '", option, "');")
        )
      })

      # attach label and buttons
      div(
        style = "margin-bottom: .5rem;",
        strong(category$label),
        div(
          style = "display: flex; flex-wrap: wrap; gap: 10px;",
          buttons
        )
      )
    })

    # create modal
    mod <- modalDialog(
      title = "Re-order sites list?",
      p("Click one of the options below to rearrange the list of sites."),
      div(
        style = "display: flex; flex-wrap: wrap; row-gap: 1rem; column-gap: 2rem;",
        sort_types
      ),
      footer = modalButton("Cancel"),
      size = "s",
      easyClose = TRUE
    )

    showModal(mod)
  })

  ## Handle site sorting ----
  observeEvent(input$sort_sites_by, {
    sort_type <- req(input$sort_sites_by)
    removeModal()
    sites <- req(rv$sites)
    sorted <- switch(
      sort_type,
      "a_z" = arrange(sites, name),
      "z_a" = arrange(sites, desc(name)),
      "n_s" = arrange(sites, desc(lat)),
      "s_n" = arrange(sites, lat),
      "w_e" = arrange(sites, lng),
      "e_w" = arrange(sites, desc(lng)),
      "ne_sw" = arrange(sites, desc(lat + lng)),
      "nw_se" = arrange(sites, desc(lat - lng)),
      "se_nw" = arrange(sites, lat - lng),
      "sw_ne" = arrange(sites, lat + lng),
      sites
    )
    rv$sites <- sorted |>
      mutate(id = row_number())
  })

  ## Handle export_sites button ----
  output$export_sites <- downloadHandler("Sites.csv", function(file) {
    sites <- rv$sites
    req(nrow(sites) > 0)
    sites |>
      select(id, name, lat, lng) |>
      write_csv(file, na = "")
  })

  # Date selection -------------------------------------------------------------

  ## date_select_ui ----
  output$date_select_ui <- renderUI({
    div(
      style = "display: flex; column-gap: 10px; margin: 10px 0;",
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
      "past_month" = c(d - months(1), d),
      "past_6_months" = c(d - months(6), d),
      "past_year" = c(d - 365, d),
      "this_season" = c(min(d, apr1), min(d, nov1)),
      "this_year" = c(jan1, d),
      "last_year" = c(jan1 - years(1), dec31 - years(1)),
      "last_season" = c(apr1 - years(1), nov1 - years(1))
    )
  })

  ## date_btns_ui ----
  output$date_btns_ui <- renderUI({
    cur_dates <- as.Date(c(rv$start_date, rv$end_date))
    presets <- date_presets()

    div(
      class = "date-btns",
      lapply(names(presets), function(name) {
        value <- presets[[name]]
        label <- snakecase::to_sentence_case(name)
        selected <- setequal(cur_dates, value)
        build_date_btn(
          name,
          label,
          btn_class = ifelse(selected, "primary", "default")
        )
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

  ## Auto-fetch timer ----

  # set a timestamp of the last inputs if weather is needed
  # observe({
  #   req(rv$sites_ready)
  #   req(need_weather())
  #   # message('Auto-fetching weather data in 15 seconds...')
  #   rv$fetch_timer <- now()
  # })

  # force a weather fetch if it's needed and hasn't been triggered in 15 seconds
  # observe({
  #   req(rv$sites_ready)
  #   req(need_weather())
  #   timestamp <- rv$fetch_timer
  #   elapsed <- now() - timestamp
  #   invalidateLater(15000)
  #   req(elapsed >= 15)
  #   if (need_weather()) {
  #     # message("Auto-fetching weather data...")
  #     rv$fetch <- runif(1)
  #   }
  # })

  # Only reset the timer when need_weather transitions FALSE → TRUE
  observe({
    req(rv$sites_ready)
    req(need_weather())
    isolate({
      if (!isTRUE(rv$fetch_timer_active)) {
        rv$fetch_timer <- now()
        rv$fetch_timer_active <- TRUE
      }
    })
  })

  # Clear the active flag when weather is no longer needed
  observe({
    if (!need_weather()) {
      rv$fetch_timer_active <- FALSE
    }
  })

  # Fire after 10s if still needed
  observe({
    req(rv$sites_ready)
    req(rv$fetch_timer_active)
    timeout <- 10 # seconds
    timestamp <- rv$fetch_timer
    invalidateLater(timeout * 1000)
    req((now() - timestamp) >= timeout)
    if (need_weather()) {
      rv$fetch <- runif(1)
      rv$fetch_timer_active <- FALSE
    }
  })

  ## action_ui ----
  output$action_ui <- renderUI({
    btn <- function(msg, ...) {
      div(class = "submit-btn", actionButton("fetch", msg, ...))
    }

    # used to promt button to regenerate
    rv$action_nonce

    # control button appearance
    if (nrow(rv$sites) == 0) {
      return(btn("No sites selected", disabled = TRUE))
    }
    if (!rv$dates_valid) {
      return(btn("Invalid date selection", disabled = TRUE))
    }
    if (need_weather()) {
      return(btn("Fetch weather"))
    }
    btn("Everything up to date", class = "btn-primary", disabled = TRUE)
  })

  ## status_ui ----

  # reports to user if there's a problem with weather fetching
  output$status_ui <- renderUI({
    status <- req(rv$status)
    msg <- req(status$msg)
    div(
      class = ifelse(
        status$type == "error",
        "shiny-output-error",
        ""
      ),
      style = "margin-top: 5px; padding: 10px;",
      msg
    )
  })

  # Module servers ----------------------------------------------------------

  ## Map server ----

  mapServer(
    rv = rv,
    map_data = reactive(
      list(
        grids = wx_grids(),
        grids_with_status = grid_status(),
        sites_with_status = sites_with_status()
      )
    )
  )

  ## Data tab ----

  dataServer(
    wx_data = reactive(wx_data()),
    selected_site = reactive(rv$selected_site),
    sites_ready = reactive(rv$sites_ready)
  )

  ## Disease models tab ----

  riskServer(rv = rv, wx_data = reactive(wx_data()))

  # Cleanup -----------------------------------------------------------------

  session$onSessionEnded(function() {
    clean_old_caches()
  })
}
