Sys.setenv(
  RENV_CONFIG_LOCKING_ENABLED = FALSE,
  RENV_CONFIG_SANDBOX_ENABLED = FALSE
)
# local Windows library outside OneDrive; other platforms (e.g. CI) use renv's default
if (.Platform$OS.type == "windows") {
  Sys.setenv(RENV_PATHS_LIBRARY = "C:/renv/library/cpn-weather-tool")
}
source("renv/activate.R")
