#-- global.R --#

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor) # name cleaning
  library(sf) # GIS
  library(fst) # file storage
  library(httr2) # requests
  library(markdown)
  library(zoo) # rollmean
  library(future) # async
  library(promises) # async

  library(shiny)
  library(shinythemes)
  library(shinyWidgets)
  library(shinyjs)
  library(shinyalert)
  library(htmltools)
  library(shinycssloaders)

  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  library(DT)
})


# Dev settings ----

# library("languageserver")
# library("lintr")
# library("styler")

## development mode
# shiny::devmode(TRUE)

## RENV
# renv::update()
# renv::snapshot()
# renv::clean()

# renv::install("httr2@1.1.0")
# renv::install("terra@1.8-42")


## turn warnings into errors
# options(warn = 2)

# disable NOAA forecasts for testing
# options(forecast = FALSE)


# Async tasks ----

# set up a second session for asynchronous tasks
plan(multisession, workers = 2)


# Load scripts ----

source_dir <- function(path) {
  files <- list.files(path, pattern = "\\.[Rr]$", full.names = TRUE)
  for (file in files) {
    source(file)
  }
}

source_dir("R/utils")

# settings and crop/disease/site defs
source_dir("R/config")
OPTS <- get_app_settings()
OPTS$crop_choices <- build_choices(crops, "name", "slug")

source_dir("R/helpers")

# IBM and NOAA api interfaces
source_dir("R/api")

# models
source_dir("R/models")

# components
source_dir("R/components")
