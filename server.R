#--- main server ---#

server <- function(input, output, session) {
  # Reactive values ------------------------------------------------------------

  rv <- reactiveValues(
    settings = list(
      beta = getOption("shiny.devmode") %||% FALSE,
      metric = FALSE # display units shared by the data and risk modules
    ),
    weather = tibble(),
    weather_ready = FALSE, # toggles to TRUE if > 0 rows in weather
    last_fetch_key = NULL, # track last weather params attempted to fetch
    last_fetch_time = NULL, # only try again on fail after 30 sec
    forecasts = list(), # per-grid forecast data, list keyed by grid_id
    forecast_tried = numeric(), # last forecast attempt time per grid_id
    wx_pending = FALSE, # TRUE while a weather or forecast fetch is outstanding
    fetch_retries = 0, # bumped by the retry button, part of the fetch key
    data_notice = NULL, # why data views have nothing to show, NULL if they should
    sites = sites_template, # table storing site locations
    sites_ready = FALSE, # toggle to TRUE if at least 1 site
    site_locs = NULL, # unique list of lat/lng for sites
    selected_site = 1, # id of selected site
    start_date = NULL, # last good date values
    end_date = NULL,
    start_date_setter = NULL, # will invoke an update to input$start_date if set
    show_upload = FALSE, # sidebar site upload UI
    map_fit_sites_cmd = NULL, # tell the map module to zoom to sites
    map_risk_data = NULL, # used by the map to render pin colors, set by the risk module
  )

  ## rv$weather_ready ----
  # update weather_ready but only if different
  observe({
    ready <- nrow(rv$weather) > 0
    if (rv$weather_ready != ready) {
      rv$weather_ready <- ready
    }
  })

  ## rv$sites_ready ----
  # update sites_ready but only if different
  observe({
    ready <- nrow(filter(rv$sites, !hidden)) > 0
    if (rv$sites_ready != ready) {
      rv$sites_ready <- ready
    }
  })

  ## rv$site_locs ----
  # unique lat/lng points for fetching weather
  observe({
    locs <- rv$sites |>
      distinct(lat, lng) |>
      arrange(lat, lng)
    if (!identical(rv$site_locs, locs)) {
      rv$site_locs <- locs
    }
  })

  ## rv$map_risk_data ----
  # set by risk module to show risk colors on the map
  # clear it if no sites
  observe({
    req(nrow(rv$sites) == 0)
    rv$map_risk_data <- NULL
  })

  ## sites_with_grid() ----
  # sf: each visible site's O1280 cell (canonical centroid + polygon)
  sites_with_grid <- reactive({
    sites <- rv$sites |>
      filter(!hidden)
    req(nrow(sites) > 0)
    om_build_site_grids(sites)
  })

  ## grid_status() ----
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

  ## sites_with_status() ----
  # each site's own grid cell (geometry + canonical centroid from
  # sites_with_grid) with per-grid coverage joined on from grid_status.
  # Geometry/centroid stay on the site side so a cell renders even before its
  # weather is downloaded; only the status columns come from the weather side.
  sites_with_status <- reactive({
    sites <- sites_with_grid()
    if (nrow(rv$weather) == 0) {
      sites |>
        mutate(needs_download = TRUE)
    } else {
      status <- grid_status() |>
        sf::st_drop_geometry() |>
        select(-any_of(c("grid_lat", "grid_lng")))
      sites |>
        left_join(status, join_by(grid_id)) |>
        replace_na(list(needs_download = TRUE))
    }
  })

  ## need_weather() ----
  # single source of truth with the task trigger: read needs_download off
  # sites_with_status (which itself uses needs_download from grid_status).
  need_weather <- reactive({
    sites <- sites_with_status()
    if (nrow(sites) == 0) {
      return(FALSE)
    }
    any(sites$needs_download, na.rm = TRUE)
  })

  ## has_visible_sites() ----
  # guard for the fetch requests: read rv$sites directly rather than
  # rv$sites_ready, which is set by an observer and can lag within a flush
  has_visible_sites <- reactive({
    nrow(filter(rv$sites, !hidden)) > 0
  })

  ## weather_request() ----
  # grids needing weather and the expanded date range to fetch them over, or
  # NULL if nothing is needed. Shared by the fetch trigger and wx_pending so
  # both agree on what's outstanding.
  weather_request <- reactive({
    if (
      !has_visible_sites() || is.null(rv$start_date) || is.null(rv$end_date)
    ) {
      return(NULL)
    }
    if (!need_weather()) {
      return(NULL)
    }

    # Fetch by grid, not by site: dedupe sites sharing a cell and carry the
    # canonical centroid (grid_lat/grid_lng) so the request hits the expected
    # O1280 cell and results can be stamped with our own grid identity.
    grids <- sites_with_grid() |>
      sf::st_drop_geometry() |>
      distinct(grid_id, grid_lat, grid_lng) |>
      semi_join(
        sites_with_status() |>
          sf::st_drop_geometry() |>
          filter(needs_download) |>
          distinct(grid_id),
        join_by(grid_id)
      )
    if (nrow(grids) == 0) {
      return(NULL)
    }

    dates <- expanded_dates()
    start_date <- as.Date(dates$start)
    end_date <- min(as.Date(dates$end), today())
    # includes the retry count so a manual retry reads as a new request
    key <- paste(
      paste(sort(grids$grid_id), collapse = ","),
      start_date,
      end_date,
      rv$fetch_retries,
      sep = "|"
    )
    lst(grids, start_date, end_date, key)
  })

  ## forecast_request() ----
  # site grids with no forecast yet, minus grids attempted within the retry
  # cooldown (so failed or empty responses don't loop), or NULL if none
  forecast_request <- reactive({
    if (!has_visible_sites()) {
      return(NULL)
    }
    tried <- rv$forecast_tried
    recent <- names(tried)[
      as.numeric(now()) - tried < OPTS$forecast_retry_secs
    ]
    grids <- sites_with_grid() |>
      sf::st_drop_geometry() |>
      distinct(grid_id, grid_lat, grid_lng) |>
      drop_na(grid_id) |>
      filter(
        !grid_id %in% names(rv$forecasts),
        !grid_id %in% recent
      )
    if (nrow(grids) == 0) {
      return(NULL)
    }
    grids
  })

  ## rv$wx_pending ----
  # TRUE while weather is about to be fetched or is in flight, so wx_data can
  # hold instead of computing on incomplete data. Derived from the requests
  # themselves (not just task status) so it's already TRUE in the same flush
  # that a date/site change opens a gap. A request whose key was already tried
  # (failed, or succeeded but still incomplete) doesn't count, so the hold
  # always releases. Forecasts only count when wx_data would use them.
  weather_pending <- reactive({
    if (task_weather$status() == "running") {
      return(TRUE)
    }
    request <- weather_request()
    !is.null(request) && !identical(request$key, rv$last_fetch_key)
  })

  forecast_pending <- reactive({
    if (is.null(rv$end_date) || rv$end_date != today()) {
      return(FALSE)
    }
    task_forecast$status() == "running" || !is.null(forecast_request())
  })

  # high priority so the flag updates before any output reads wx_data in the
  # same flush; only writes on change to avoid spurious invalidations
  observe(
    {
      pending <- weather_pending() || forecast_pending()
      if (!identical(rv$wx_pending, pending)) {
        rv$wx_pending <- pending
      }
    },
    priority = 100
  )

  ## weather_failed() / forecast_failed() ----
  # TRUE when the active request was already attempted and isn't running, yet
  # data is still missing. om_fetch_* swallow network/HTTP failures and return
  # partial or no data, so task status alone can't detect them. Scoped to the
  # current sites/dates by construction: a past failure that's no longer part
  # of the active request doesn't count.
  weather_failed <- reactive({
    request <- weather_request()
    !is.null(request) &&
      identical(request$key, rv$last_fetch_key) &&
      task_weather$status() != "running"
  })

  forecast_failed <- reactive({
    if (
      !has_visible_sites() || is.null(rv$end_date) || rv$end_date != today()
    ) {
      return(FALSE)
    }
    if (task_forecast$status() == "running") {
      return(FALSE)
    }
    grid_ids <- unique(sites_with_grid()$grid_id)
    missing <- setdiff(grid_ids, names(rv$forecasts))
    any(missing %in% names(rv$forecast_tried))
  })

  ## wx_available() ----
  # TRUE if any visible site has weather within the selected dates
  wx_available <- reactive({
    sites <- sites_with_status()
    "days_actual" %in% names(sites) && any(coalesce(sites$days_actual, 0L) > 0)
  })

  ## rv$data_notice ----
  # Single decision for what the data views (risk feed, data plot) should say
  # when they have no content to show: list(type, msg), or NULL when there is
  # weather to show. Modules display it in place of content, except while
  # holding content that's already on screen. Priority below the wx_pending
  # observer (reads its flag) but above outputs; only writes on change.
  observe(
    {
      notice <- if (!has_visible_sites()) {
        list(type = "info", msg = OPTS$validation_sites_ready)
      } else if (rv$wx_pending) {
        list(type = "info", msg = OPTS$validation_weather_loading)
      } else if (wx_available()) {
        NULL
      } else if (weather_failed()) {
        list(
          type = "error",
          msg = tagList(
            "Could not get any weather data for your selected sites and dates.",
            "Check your network connection and click Retry in the status box to try again.",
            contact_html()
          )
        )
      } else {
        list(type = "info", msg = OPTS$validation_weather_ready)
      }
      if (!identical(rv$data_notice, notice)) {
        rv$data_notice <- notice
      }
    },
    priority = 90
  )

  ## Retry failed fetches ----
  # a new fetch key re-requests weather; clearing the forecast attempts for
  # the active grids re-requests their forecasts
  observeEvent(input$retry_fetch, {
    rv$fetch_retries <- rv$fetch_retries + 1
    grid_ids <- unique(sites_with_grid()$grid_id)
    tried <- rv$forecast_tried
    rv$forecast_tried <- tried[!names(tried) %in% grid_ids]
  })

  # Date handlers --------------------------------------------------------------

  ## validate_dates ----
  # returns NULL if valid, else list(msg, start_invalid, end_invalid)
  validate_dates <- function(start, end) {
    start_missing <- length(start) == 0
    end_missing <- length(end) == 0
    if (start_missing || end_missing) {
      return(list(
        msg = "Please provide both start and end dates.",
        start_invalid = start_missing,
        end_invalid = end_missing
      ))
    }
    if (start > end) {
      return(list(
        msg = "Start date must be before end date.",
        start_invalid = TRUE,
        end_invalid = TRUE
      ))
    }
    if ((end - start) > years(1)) {
      return(list(
        msg = "Date range must be less than 1 year.",
        start_invalid = TRUE,
        end_invalid = TRUE
      ))
    }
    NULL
  }

  ## date_error ----
  date_error <- reactive({
    validate_dates(input$start_date, input$end_date)
  })

  ## highlight invalid date inputs ----
  observe({
    err <- date_error()
    toggleCssClass(
      id = "start_date",
      class = "input-invalid",
      condition = !is.null(err) && isTRUE(err$start_invalid)
    )
    toggleCssClass(
      id = "end_date",
      class = "input-invalid",
      condition = !is.null(err) && isTRUE(err$end_invalid)
    )
  })

  ## set rv$start_date and rv$end_date ----
  observe({
    if (is.null(date_error())) {
      rv$start_date <- input$start_date
      rv$end_date <- input$end_date
    }
  })

  # update start date input from model module to handle biofixes
  observe({
    new_start_date <- req(rv$start_date_setter)
    updateDateInput(inputId = "start_date", value = new_start_date)
  })

  ## selected_dates ----
  # will block fetch button if invalid dates selected
  selected_dates <- reactive({
    start <- req(rv$start_date)
    end <- req(rv$end_date)
    dates <- list(start = start, end = end)
  })

  ## expanded_dates ----
  expanded_dates <- reactive({
    dates <- selected_dates()
    dates$start <- dates$start - days(31)
    dates$end <- dates$end + days(30)
    dates
  })

  # Cookie handling ------------------------------------------------------------

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
  })

  # Cached data ----------------------------------------------------------------

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
        build_daily(wx) # make sure it'll work
        rv$weather <- wx
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

  # Open-Meteo Weather Extended Task -------------------------------------------

  # caller is responsible for clamping end_date to local today() before invoking;
  # don't re-clamp here because the worker's system TZ may differ from the user's.
  task_weather <- ExtendedTask$new(function(
    grids,
    start_date,
    end_date,
    wx
  ) {
    mirai(
      {
        new_wx <- om_fetch_weather(
          grids,
          as.Date(start_date),
          as.Date(end_date),
          wx
        )
        om_merge_wx(wx, new_wx)
      },
      .GlobalEnv,
      .args = lst(grids, start_date, end_date, wx),
      # a hung fetch becomes a task error, which releases rv$wx_pending
      .timeout = OPTS$weather_timeout_secs * 1000
    )
  })

  # observe({
  #   message(paste("task_weather:", task_weather$status()))
  # })

  ## Invoke weather request ----

  ## Non-mirai version of weather fetch for testing
  # observe({
  #   req(rv$sites_ready)
  #   sites <- sites_with_status()
  #   dates <- expanded_dates()
  #   start_date <- as.Date(dates$start)
  #   end_date <- min(as.Date(dates$end), today())

  #   isolate({
  #     cur_wx <- rv$weather
  #     new_wx <- om_fetch_weather(sites, start_date, end_date, cur_wx)
  #     merged_wx <- om_merge_wx(cur_wx, new_wx)
  #     if (!identical(cur_wx, merged_wx)) {
  #       rv$weather <- merged_wx
  #     }
  #   })
  # })

  # Runs from any non-running state, including after an error, so a changed
  # request (new dates/sites) always gets a fresh attempt.
  observe({
    request <- req(weather_request())
    req(task_weather$status() != "running")

    # Debounce: bail if the same (grids, dates) was fetched in the last 30s.
    # Defense against feedback loops where post-fetch state still reads as
    # needing weather.
    recent <- isolate(
      identical(rv$last_fetch_key, request$key) &&
        !is.null(rv$last_fetch_time) &&
        as.numeric(now() - rv$last_fetch_time, units = "secs") < 30
    )
    if (recent) {
      return()
    }

    isolate({
      rv$last_fetch_key <- request$key
      rv$last_fetch_time <- now()

      task_weather$invoke(
        request$grids,
        request$start_date,
        request$end_date,
        rv$weather
      )
    })
  })

  ## weather_error ----
  # user-facing message from a failed weather task, NULL otherwise
  weather_error <- reactive({
    if (task_weather$status() != "error") {
      return(NULL)
    }
    err <- task_weather$result()
    sprintf("Error <%s>: %s", class(err)[1], conditionMessage(err))
  })

  # log weather errors to the server console
  observe({
    err <- req(weather_error())
    warning(err)
  })

  ## Handle weather request response ----
  # priority above the rv$wx_pending observer so the new weather is in place
  # before pending is re-evaluated on task success
  observe(
    {
      req(task_weather$status() == "success")
      res <- task_weather$result()
      if (!identical(rv$weather, res)) {
        rv$weather <- res
      }
    },
    priority = 200
  )

  # Forecast ExtendedTask -------------------------------------------------------

  task_forecast <- ExtendedTask$new(function(grids) {
    mirai(
      {
        om_fetch_forecast(grids)
      },
      .GlobalEnv,
      .args = lst(grids),
      # a hung fetch becomes a task error, which releases rv$wx_pending
      .timeout = OPTS$forecast_timeout_secs * 1000
    )
  })

  # observe({
  #   message(paste("task_forecast:", task_forecast$status()))
  # })

  ## Invoke forecast fetch ----
  observe({
    grids <- req(forecast_request())
    req(task_forecast$status() != "running")

    # record the attempt so a failed or empty response isn't retried until
    # the cooldown passes (this also drops the grids from forecast_request)
    isolate({
      tried <- rv$forecast_tried
      tried[grids$grid_id] <- as.numeric(now())
      rv$forecast_tried <- tried
    })

    task_forecast$invoke(grids)
  })

  ## Handle forecast response ----
  # priority above the rv$wx_pending observer, same as the weather handler
  observe(
    {
      req(task_forecast$status() == "success")
      result <- task_forecast$result()
      req(!is.null(result), nrow(result) > 0)

      isolate({
        fc <- rv$forecasts
        for (gid in unique(result$grid_id)) {
          fc[[gid]] <- result |> filter(grid_id == gid)
        }
        if (!identical(rv$forecasts, fc)) {
          rv$forecasts <- fc
        }
      })
    },
    priority = 200
  )

  # Weather data ---------------------------------------------------------------

  ## wx_data ----
  # dependency on unique lat/lngs vs site ids, names, hide state, etc
  wx_data <- reactive({
    # Hold while a fetch is outstanding. "progress" leaves dependent outputs
    # showing their previous content in Shiny's recalculating (dimmed) state
    # rather than clearing them, the same way ExtendedTask$result() does.
    # Outputs with nothing on screen yet should check rv$wx_pending first and
    # show a loading message instead.
    req(!rv$wx_pending, cancelOutput = "progress")

    weather <- rv$weather
    sites <- sites_with_grid() |>
      sf::st_drop_geometry() |>
      distinct(grid_id, grid_lat, grid_lng)
    fc_list <- rv$forecasts
    sel_dates <- selected_dates()
    exp_dates <- expanded_dates()

    req(nrow(weather) > 0, nrow(sites) > 0)

    wx <- weather |>
      filter(
        grid_id %in% sites$grid_id,
        between(date, exp_dates$start, exp_dates$end)
      )

    fc_data <- if (sel_dates$end == today() & length(fc_list) > 0) {
      sel_fc <- bind_rows(fc_list) |>
        filter(grid_id %in% sites$grid_id)
      if (nrow(sel_fc) > 0) {
        sel_dates$end <- max(sel_fc$date)
      }
      sel_fc
    } else {
      tibble()
    }

    # includes 31 days prior to selected dates
    hourly_full <- bind_rows(wx, fc_data) |>
      drop_na(datetime_utc) |>
      arrange(grid_id, datetime_utc) |>
      distinct(grid_id, datetime_utc, .keep_all = TRUE)

    # drop expanded dates and build cumulative counts
    hourly <- hourly_full |>
      filter(between(date, sel_dates$start, sel_dates$end)) |>
      add_cumsum("evapotranspiration") |>
      add_cumsum("precipitation") |>
      add_cumsum("rain") |>
      add_cumsum("snowfall")

    daily_full <- build_daily(hourly_full)

    # drop expanded dates and rebuild cumulative counts
    daily <- daily_full |>
      filter(between(date, sel_dates$start, sel_dates$end)) |>
      add_cumsum("evapotranspiration_daily") |>
      add_cumsum("precipitation_daily") |>
      add_cumsum("rain_daily") |>
      add_cumsum("snowfall_daily")

    list(
      dates = sel_dates,
      hourly = hourly,
      daily = daily,
      daily_full = daily_full
    )
  })

  # Info/settings buttons ------------------------------------------------------

  output$info_btns <- renderUI({
    div(
      class = "info-btns",
      if (isTRUE(rv$settings$beta)) {
        span("[dev mode]")
      },
      div(class = "info-btn", actionLink("about", icon("circle-info"))),
      div(class = "info-btn", actionLink("settings", icon("gear")))
    )
  })

  ## 'More information' modals ----
  observe({
    mod <- req(input$show_modal)
    show_modal(md = mod$md)
  })

  ## Help modal ----
  observeEvent(
    input$about,
    show_modal(md = "README.md", title = "About this tool")
  )

  # Settings modal ----
  observeEvent(input$settings, {
    ui <- tagList(
      div(
        h4(
          "Show models currently under development?",
          style = "margin-top: 0;"
        ),
        div(
          class = "settings-item",
          "Some models have been deployed to this tool, but have not been fully completed or validated. Enable this option to show these models in the model list. Model names will include the [beta] tag.",
          materialSwitch(
            inputId = "enable_beta_models",
            # label = "Show new/beta models",
            label = NULL,
            value = rv$settings$beta,
            status = "primary"
          )
        )
      ),
      div(
        h4("Use metric units?"),
        div(
          class = "settings-item",
          "Display weather values in metric units (°C, mm, km/h) instead of imperial units (°F, in, mph).",
          materialSwitch(
            inputId = "use_metric",
            label = NULL,
            value = rv$settings$metric,
            status = "primary"
          )
        )
      )
    )

    m <- modalDialog(
      ui,
      title = "Settings",
      footer = modalButton("Close"),
      easyClose = TRUE
    )

    showModal(m)
  })

  observeEvent(input$enable_beta_models, {
    rv$settings$beta <- input$enable_beta_models
  })

  observeEvent(input$use_metric, {
    rv$settings$metric <- input$use_metric
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

  ## sites_list ----
  # flex-grid render of the user's sites with select/hide/edit/trash actions
  output$sites_list <- renderUI({
    sites <- rv$sites
    if (nrow(sites) == 0) {
      return(NULL)
    }
    selected_id <- rv$selected_site

    rows <- lapply(seq_len(nrow(sites)), function(i) {
      s <- sites[i, ]
      is_selected <- isTRUE(s$id == selected_id)
      is_hidden <- isTRUE(s$hidden)
      cls <- paste(
        c(
          "site-row",
          if (is_selected) "site-row--selected",
          if (is_hidden) "site-row--hidden"
        ),
        collapse = " "
      )
      vis_action <- if (is_hidden) "show" else "hide"
      div(
        class = cls,
        onclick = if (!is_hidden) sprintf("selectSite(%s)", s$id),
        div(class = "site-row__id", s$id),
        div(class = "site-row__name", s$name),
        div(class = "site-row__coords", sprintf("%.2f, %.2f", s$lat, s$lng)),
        div(
          class = "site-row__actions",
          onclick = "event.stopPropagation()",
          HTML(site_action_link(vis_action, s$id)),
          HTML(site_action_link("edit", s$id, s$name)),
          HTML(site_action_link("trash", s$id))
        )
      )
    })

    div(class = "sites-list", rows)
  })

  ## Handle row selection click ----
  observeEvent(input$select_site, {
    id <- req(input$select_site)
    req(id %in% rv$sites$id)
    rv$selected_site <- id
  })

  ## Handle visibility toggle ----
  observeEvent(input$toggle_site, {
    toggle_id <- req(input$toggle_site)
    rv$sites <- rv$sites |>
      mutate(hidden = if_else(id == toggle_id, !hidden, hidden))
  })

  ## Advance selection when selected site becomes hidden or deleted ----
  # picks the next visible site in list order, wrapping to the first if none after
  observe({
    sites <- rv$sites
    current <- rv$selected_site
    req(nrow(sites) > 0)

    visible_ids <- sites$id[!sites$hidden]
    if (length(visible_ids) == 0) {
      return()
    }
    if (!is.null(current) && current %in% visible_ids) {
      return()
    }

    if (!is.null(current) && current %in% sites$id) {
      current_pos <- match(current, sites$id)
      after <- sites$id[!sites$hidden & seq_len(nrow(sites)) > current_pos]
      if (length(after) > 0) {
        rv$selected_site <- after[1]
        return()
      }
    }
    rv$selected_site <- visible_ids[1]
  })

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
    cur_dates <- as.Date(c(input$start_date, input$end_date))
    presets <- date_presets()

    div(
      class = "date-btns",
      lapply(names(presets), function(name) {
        value <- presets[[name]]
        label <- snakecase::to_sentence_case(name)
        selected <- if (length(cur_dates) < 2) {
          FALSE
        } else {
          setequal(cur_dates, value)
        }
        build_date_btn(
          value = name,
          label = label,
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

  # Status ----------------------------------------------------

  ## user_status ----
  # unified, priority-ordered status (drives status_ui)
  status_msg <- reactive({
    Status <- function(type, msg, icon_name, contact = FALSE, retry = FALSE) {
      lst(type, msg, icon = icon(icon_name), contact, retry)
    }

    # check for date error
    date_err <- date_error()
    if (!is.null(date_err)) {
      return(Status("error", date_err$msg, "x"))
    }

    # if weather running
    if (task_weather$status() == "running") {
      return(Status("info", "Getting weather...", "hourglass"))
    }

    # if forecast running
    if (task_forecast$status() == "running") {
      return(Status("info", "Getting forecasts...", "hourglass"))
    }

    # failed fetches for the active sites/dates (task errors are logged to the
    # console by the weather_error observer)
    if (weather_failed()) {
      return(Status(
        "warning",
        "Could not fetch weather data.",
        "triangle-exclamation",
        contact = TRUE,
        retry = TRUE
      ))
    }
    if (forecast_failed()) {
      return(Status(
        "warning",
        "Could not fetch forecast data.",
        "triangle-exclamation",
        contact = TRUE,
        retry = TRUE
      ))
    }

    Status("info", "Everything up to date.", "check")
  })

  ## status_ui ----

  # reports app status / problems to the user
  output$status_ui <- renderUI({
    # wait for initial load to complete and default dates to be set
    req(rv$start_date, rv$end_date)

    status <- status_msg()
    contact <- if (isTRUE(status$contact)) {
      tags$div(class = "app-status__contact", contact_html())
    }
    tags$div(
      class = paste0("app-status app-status--", status$type),
      tags$div(
        class = "app-status__msg",
        HTML(status$msg),
        if (isTRUE(status$retry)) {
          actionLink(
            "retry_fetch",
            "Retry",
            icon = icon("rotate-right"),
            class = "app-status__retry"
          )
        } else {
          status$icon
        }
      ),
      contact
    )
  })

  # Module servers ----------------------------------------------------------

  ## Map server ----

  mapServer(
    rv = rv,
    rx = list(
      grid_status = grid_status,
      sites = sites_with_status
    )
  )

  ## Data tab ----

  dataServer(
    rv = rv,
    rx = list(
      sites = sites_with_status,
      dates = selected_dates,
      wx = wx_data
    )
  )

  ## Disease models tab ----

  riskServer(
    rv = rv,
    rx = list(
      sites = sites_with_status,
      dates = selected_dates,
      wx = wx_data
    )
  )

  # Cleanup -----------------------------------------------------------------

  session$onSessionEnded(function() {
    clean_old_caches()
  })
}
