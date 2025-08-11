# UI builders ----

#' Create the missing data element based on number of sites missing
missing_weather_ui <- function(n = 1) {
  msg <- ifelse(
    n == 1,
    "This site is missing data based on your date selections.",
    "One or more sites are missing data based on your date selections."
  )

  div(
    class = "missing-weather-notice",
    div(style = "color: orange; padding: 10px; font-size: 1.5em;", icon("warning")),
    div(em(msg, "Press", strong("Fetch weather"), "on the sidebar to download any missing data."))
  )
}

# missing_weather_ui(1)
# missing_weather_ui(2)


site_action_link <- function(action = c("edit", "save", "trash"), site_id, site_name = "") {
  action <- match.arg(action)
  hovertext <- switch(action,
    edit = "Rename this site",
    save = "Pin this site to list",
    trash = "Delete this site"
  )
  onclick <- switch(action,
    edit = sprintf("editSite(%s, \"%s\")", site_id, site_name),
    save = sprintf("saveSite(%s)", site_id),
    trash = sprintf("trashSite(%s)", site_id)
  )
  content <- as.character(switch(action,
    edit = icon("pen"),
    save = icon("thumbtack"),
    trash = icon("trash")
  ))
  sprintf("<a style='cursor:pointer' title='%s' onclick='%s'>%s</a>", hovertext, onclick, content)
}

# site_action_link("edit", 1, "foo")
# site_action_link("save", 1, "foo")
# site_action_link("trash", 1, "foo")


disease_modal_link <- function(disease) {
  md <- disease$doc
  name <- disease$name
  title <- paste(name, "information")
  onclick <- sprintf("sendShiny('show_modal', {md: '%s', title: '%s'})", md, title)
  shiny::HTML(sprintf("<a style='cursor:pointer' title='%s' onclick=\"%s\">More information</a>.", title, onclick))
}

# disease_modal_link(diseases$white_mold)


show_modal <- function(md, title = NULL) {
  if (!file.exists(md)) warning(md, " does not exist")
  modalDialog(
    includeMarkdown(md),
    title = title,
    footer = modalButton("Close"),
    easyClose = TRUE
  ) %>% showModal()
}
