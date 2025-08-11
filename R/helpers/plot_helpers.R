# Plot helpers ----

# expand axis range by percentage
expand_range <- function(lo, hi, amt = .05) {
  c(lo - abs(hi - lo) * amt, hi + abs(hi - lo) * amt)
}

# expand_range(0, 1)


# add forecast annotation
plotly_show_forecast <- function(plt, xmax) {
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

  plt %>% layout(shapes = c(vline, area), annotations = text)
}


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


# field crops disease plots
plot_risk <- function(df, name, xrange = NULL) {
  x <- axis_defaults$x
  y <- axis_defaults$y
  x$range <- xrange
  y$range <- expand_range(0, 1)

  # plot the severity
  if ("severity" %in% names(df)) {
    df$value <- df$severity
    y$range <- expand_range(0, 4)
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
        color = ~alpha(risk_color, .2),
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
    config(displayModeBar = FALSE) %>%
    plotly_show_forecast(xmax = xrange[2])
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
