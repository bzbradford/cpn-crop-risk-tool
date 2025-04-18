#-- ui.R --#

ui <- fluidPage(
  title = OPTS$app_title,
  theme = shinytheme("flatly"),
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "A tool for downloading hourly weather data for any location in the continental United States"),
    tags$meta(name = "keywords", content = "uw, wisconsin, weather, tool"),
    tags$link(rel = "shortcut icon", href = OPTS$app_header_badge),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js"),
    includeHTML("www/google-analytics.html"),
    useShinyjs(),
  ),
  tags$header(
    style = paste("background-color:", OPTS$app_header_color),
    div(
      class = "uw-title",
      img(src = OPTS$app_header_badge),
      h1(OPTS$app_title)
    ),
    div(
      class = "help-btn",
      actionLink("help", icon("circle-info"))
    )
  ),
  div(class = "main-container",
    div(class = "column sidebar-container",
      h2("Site selection"),
      div(
        div(
          uiOutput("site_help_ui"),
          DTOutput("sites_tbl"),
          uiOutput("site_btns")
        ),
        div(
          style = "margin-top: 20px;",
          uiOutput("date_ui"),
        ),
        div(
          style = "margin-top: 20px;",
          uiOutput("action_ui") %>% withSpinner(type = 8, size = .5, proxy.height = 50, caption = "Please wait..."),
          uiOutput("status_ui")
        )
      )
    ),
    div(
      class = "column map-container",
      leafletOutput("map", height = "100%"),
      div(
        class = "search-overlay",
        uiOutput("searchbox_ui"),
        uiOutput("coord_search_ui")
      )
    ),
    div(
      class = "column data-container",
      if (OPTS$cpn_mode) {
        tabsetPanel(
          tabPanel("Crop risk models", riskUI()),
          tabPanel("Weather charts", dataUI()),
          type = "pills"
        )
      } else {
        tabsetPanel(
          tabPanel("View data", dataUI()),
          tabPanel("Crop risk models", riskUI()),
          type = "pills"
        )
      }
    )
  ),
  tags$footer(
    div(
      class = "badges",
      OPTS$app_footer_badge
    ),
    div(
      class = "credits",
      HTML("Developed by <a href='https://entomology.wisc.edu/directory/ben-bradford/' target='_blank'>Ben Bradford</a>, UW-Madison Entomology"),
      br(),
      HTML("Feedback welcome! <a href='https://forms.gle/LZDg9E39UdinLVq17' target='_blank'>Click here to take our survey</a>"),
      br(),
      HTML("<a href='https://github.com/bzbradford/weather-tool' target='_blank'>View source code</a>"),
    )

  )
)

