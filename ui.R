#--- main ui --#

ui <- fluidPage(

  tags$head(
    tags$title(OPTS$app_title),
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "A tool for downloading hourly weather data for any location in the continental United States"),
    tags$meta(name = "keywords", content = "uw, wisconsin, weather, tool"),
    tags$link(rel = "shortcut icon", href = OPTS$app_header_badge),
    tags$link(rel = "stylesheet", type = "text/css", href = shinytheme("flatly")),
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
      class = "info-btn",
      actionLink("about", icon("circle-info"))
    )
  ),

  div(class = "main",

    div(class = "column sidebar-col",

      h2("Site selection", style = "margin-bottom: 1rem;"),

      div(
        uiOutput("site_help_ui"),
        div(
          style = "max-height: 400px; overflow: auto;",
          DTOutput("sites_dt"),
        ),
        uiOutput("site_btns"),
        uiOutput("file_upload_ui")
      ),

      div(
        style = "margin-top: 1rem;",
        uiOutput("date_select_ui"),
        uiOutput("date_btns_ui")
      ),

      div(
        style = "margin-top: 1rem;",
        uiOutput("action_ui") %>%
          withSpinner(type = 8, size = .5, proxy.height = 70, caption = "Please wait..."),
        uiOutput("status_ui")
      )
    ),

    div(class = "column map-col", mapUI()),

    div(
      class = "column data-col",
      tabsetPanel(
        tabPanel("Crop risk models", riskUI()),
        tabPanel("Charts and data", dataUI()),
        type = "pills"
      )
    )

  ),

  tags$footer(
    div(
      class = "badges",
      div(a(img(title = "Crop Protection Network", src = "cpn-logo.png", height = "50px"), href = "https://cropprotectionnetwork.org", target = "_blank")),
      div(a(img(title = "National Predictive Modeling Tool Initiative", src = "npmti-usda-logo.png", height = "40px"), href = "https://agpmt.org", target = "_blank"))
    ),
    div(
      class = "credits",
      HTML("Developed by <a href='https://entomology.wisc.edu/directory/ben-bradford/' target='_blank'>Ben Bradford</a>, UW-Madison Entomology"),
      br(),
      HTML("<a href='https://forms.gle/LZDg9E39UdinLVq17' target='_blank'>Feedback welcome! Click here to take our survey</a>"),
      br(),
      HTML("<a href='https://github.com/bzbradford/ibm-weather-tool' target='_blank'>View source code</a>"),
    )

  )
)

