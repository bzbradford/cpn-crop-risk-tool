# Plot helpers ----

axis_defaults <- local({
  x <- y <- list(
    title = "",
    showticklabels = FALSE,
    showgrid = FALSE,
    showline = FALSE,
    zeroline = FALSE,
    fixedrange = TRUE,
    tickfont = OPTS$plot_axis_font
  )
  x$showticklabels <- TRUE
  x$hoverformat <- "<b>%b %d, %Y</b>"
  x$fixedrange <- FALSE

  list(x = x, y = y)
})


# expand axis range by percentage
expand_range <- function(lo, hi, amt = .05) {
  c(lo - abs(hi - lo) * amt, hi + abs(hi - lo) * amt)
}

# expand_range(0, 1)


# add forecast annotation
plotly_get_forecast_annot <- function(xmax) {
  if ("Date" %in% class(xmax)) {
    x <- today()
    label <- "Today"
  } else {
    x <- now()
    label <- "Now"
  }

  text <- list(list(
    yref = "paper",
    x = x, y = 1,
    text = label,
    showarrow = FALSE,
    opacity = .5,
    xanchor = "right",
    align = "right"
  ))

  vline <- list(list(
    type = "line", yref = "paper",
    x0 = x, x1 = x, y0 = .05, y1 = 1,
    line = list(color = "black", dash = "dash"),
    opacity = .25
  ))

  area <- list(list(
    type = "rect",
    fillcolor = "orange",
    line = list(opacity = 0),
    opacity = 0.05,
    yref = "paper",
    x0 = x, x1 = xmax,
    y0 = .05, y1 = 1,
    layer = "below"
  ))

  list(
    shapes = c(vline, area),
    annotations = text
  )
}


plotly_get_risk_period_annot <- function(start_date, end_date, xrange) {
  text_date_min <- max(start_date, xrange[1])
  text_date_max <- min(end_date, xrange[2])
  text_date <- text_date_min + (text_date_max - text_date_min) / 2

  text <- list(list(
    yref = "paper",
    x = text_date, y = 1,
    text = "Risk Period",
    showarrow = FALSE,
    opacity = .5,
    xanchor = "center",
    align = "center"
  ))

  vlines <- lapply(c(start_date, end_date), function(dt) {
    list(
      type = "line", yref = "paper",
      x0 = dt, x1 = dt, y0 = .05, y1 = 1,
      line = list(color = "maroon", weight = .5, dash = "dot"),
      opacity = .25
    )
  })

  area <- list(list(
    type = "rect",
    fillcolor = "red",
    line = list(opacity = 0),
    opacity = 0.05,
    yref = "paper",
    x0 = start_date, x1 = end_date,
    y0 = .05, y1 = 1,
    layer = "below"
  ))

  list(
    shapes = c(vlines, area),
    annotations = text
  )
}

# plotly_get_risk_period_annot(ymd("2025-7-1"), ymd("2025-8-15"))


# field crops disease plots
plot_risk <- function(df, name, xrange = NULL, risk_period = NULL) {
  x <- axis_defaults$x
  y <- axis_defaults$y
  x$range <- xrange
  y$range <- expand_range(0, 1)

  # plot the severity
  if ("severity" %in% names(df)) {
    df$value <- df$severity
    y$range <- expand_range(0, 4)
  }

  # generate annotations
  shapes <- list()
  annot <- list()

  # forecast
  fc <- plotly_get_forecast_annot(xmax = xrange[2])
  shapes <- append(shapes, fc$shapes)
  annot <- append(annot, fc$annotations)

  # risk period
  yrs <- unique(year(df$date))
  if (!is.null(risk_period)) {
    for (yr in yrs) {
      dates <- ymd(paste(yr, risk_period))
      risk <- plotly_get_risk_period_annot(dates[1], dates[2], xrange)
      shapes <- append(shapes, risk$shapes)
      annot <- append(annot, risk$annotations)
    }
  }

  df %>%
    plot_ly(x = ~date, y = ~value, height = 100) %>%
    add_trace(
      name = name,
      text = ~value_label,
      type = "scatter",
      mode = "lines+markers",
      marker = list(color = ~risk_color),
      line = list(color = "black", width = 1),
      hovertemplate = "%{text}",
      hoverinfo = "text"
    ) %>%
    add_trace(
      name = name,
      type = "bar",
      marker = list(
        color = ~ alpha(risk_color, .2),
        line = list(width = 0)
      ),
      hoverinfo = "none"
    ) %>%
    layout(
      margin = list(l = 5, r = 5, b = 5, t = 5, pad = 5),
      xaxis = x,
      yaxis = y,
      bargap = 0,
      hovermode = "x unified",
      showlegend = FALSE
    ) %>%
    layout(shapes = shapes, annotations = annot) %>%
    config(displayModeBar = FALSE)
}

# saved_weather %>%
#   filter(grid_id == sample(grid_id, 1)) %>%
#   build_hourly() %>%
#   build_daily() %>%
#   build_disease_from_ma() %>%
#   pivot_longer(cols = -c(grid_id, date), names_to = "model") %>%
#   filter(model == sample(model, 1)) %>%
#   mutate(assign_risk(first(model), value)) %>%
#   plot_risk(name = first(.$model))
