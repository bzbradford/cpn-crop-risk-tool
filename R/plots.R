#- plots.R -#

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
    showarrow = F,
    opacity = .5
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


axis_defaults <- {
  x <- y <- list(
    title = "",
    showticklabels = FALSE,
    showgrid = FALSE,
    showline = FALSE,
    zeroline = FALSE,
    fixedrange = TRUE,
    tickfont = OPTS$plot_axis_font
  )
  x$showticklabels = TRUE
  x$hoverformat = "<b>%b %d, %Y</b>"
  x$fixedrange = FALSE

  list(x = x, y = y)
}


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


#
# # field crops disease plots
# plot_severity <- function(df, name, xrange = NULL) {
#   # value, total, avg7, severity, risk, risk_color, value_labels
#
#   x = axis_defaults$x
#   y = axis_defaults$y
#   x$range = xrange
#   y$range = expand_range(0, 4)
#
#   # severity
#   p1 <- df %>%
#     plot_ly(x = ~date, y = ~severity, height = 100) %>%
#     add_trace(
#       name = name,
#       text = ~value_label,
#       type = "scatter",
#       mode = "lines+markers",
#       marker = list(color = ~risk_color),
#       line = list(color = "black", width = 1),
#       hovertemplate = "%{text}",
#       hoverinfo = "text"
#     ) %>%
#     add_bars(
#       marker = list(
#         color = ~alpha(risk_color, .5),
#         line = list(width = 0)
#       ),
#       hoverinfo = "none"
#     )
#
#   # total
#   p2 <- df %>%
#     plot_ly(x = ~date, y = ~severity, height = 100) %>%
#     add_trace(
#       name = "Total",
#       y = ~value,
#       type = "scatter",
#       mode = "lines",
#       hovertemplate = "%{y:.1f}"
#     )
#
#   subplot(p1, p2, nrows = 2, shareX = TRUE, shareY = FALSE) %>%
#     layout(
#       margin = list(l = 5, r = 5, b = 5, t = 5, pad = 5),
#       xaxis = x,
#       yaxis = y,
#       bargap = 0,
#       hovermode = "x unified",
#       showlegend = FALSE
#     ) %>%
#     config(displayModeBar = FALSE) %>%
#     plotly_show_forecast(xmax = xrange[2])
# }

#
# saved_weather %>%
#   filter(grid_id == sample(grid_id, 1)) %>%
#   build_hourly() %>%
#   build_daily() %>%
#   build_disease_from_daily() %>%
#   pivot_longer(cols = -c(grid_id, date), names_to = "model") %>%
#   filter(model == "potato_pdays") %>%
#   mutate(assign_risk(first(model), value)) %>%
#   plot_risk(name = first(.$model))



# Old plot ----------------------------------------------------------------

# data should have cols: date, name, value, value_label, risk
# disease_plot <- function(data, xrange = NULL) {
#   # expand the range by amt %
#   yrange <- function(lo, hi, amt = .05) {
#     c(lo - abs(hi - lo) * amt, hi + abs(hi - lo) * amt)
#   }
#
#   title_font <- list(family = "Redhat Display", size = 14)
#   axis_font <- list(family = "Redhat Text", size = 12)
#
#   # axis config
#   x <- y1 <- y2 <- list(
#     title = "",
#     showticklabels = FALSE,
#     showgrid = FALSE,
#     showline = FALSE,
#     zeroline = FALSE,
#     fixedrange = TRUE,
#     tickfont = axis_font
#   )
#   x$showticklabels = TRUE
#   x$range = xrange
#   x$hoverformat = "<b>%b %d, %Y</b>"
#   x$fixedrange = FALSE
#   y1$range = yrange(0, 1)
#   y2$range = yrange(0, 4)
#   y2$overlaying = "y"
#
#   if (!("severity" %in% names(data))) data$severity <- NA
#   data <- data %>%
#     mutate(
#       yaxis = if_else(grepl("_prob", model), "y1", "y2"),
#       value = coalesce(severity, value)
#     )
#
#   models <- unique(data$model)
#   lapply(models, function(model) {
#     df <- data %>% filter(model == !!model)
#     yaxis <- first(df$yaxis)
#     plot_ly(df, x = ~date, y = ~value, height = 100) %>%
#       add_trace(
#         name = ~name,
#         text = ~value_label,
#         type = "scatter",
#         mode = "lines+markers",
#         marker = list(color = ~risk_color),
#         line = list(color = "black", width = 1),
#         hovertemplate = "%{text}",
#         hoverinfo = "text",
#         yaxis = yaxis
#       ) %>%
#       add_trace(
#         name = ~name,
#         type = "bar",
#         marker = list(
#           color = ~alpha(risk_color, .2),
#           line = list(width = 0)
#         ),
#         hoverinfo = "none",
#         yaxis = yaxis
#       ) %>%
#       layout(
#         title = list(
#           text = ~sprintf("<b>%s</b>", first(name)),
#           x = 0,
#           y = .99,
#           yanchor = "top",
#           font = title_font
#         ),
#         margin = list(l = 5, r = 5, b = 5, t = 5, pad = 5),
#         xaxis = x,
#         yaxis = y1,
#         yaxis2 = y2,
#         bargap = 0,
#         hovermode = "x unified",
#         showlegend = FALSE
#       ) %>%
#       config(displayModeBar = FALSE) %>%
#       plotly_show_forecast(xmax = xrange[2])
#   })
# }
