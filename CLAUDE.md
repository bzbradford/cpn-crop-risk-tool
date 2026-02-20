# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

An R Shiny web application for agricultural weather monitoring and crop disease risk forecasting. It fetches weather data from IBM Environmental Intelligence Suite (~3km resolution) and NOAA, processes it through meteorological pipelines, and displays crop disease risk model results on interactive maps.

## Common Commands

```r
# Run the app
shiny::runApp()

# Run all tests
testthat::test_dir("tests/testthat")

# Run a single test file
testthat::test_file("tests/testthat/test-models.R")

# Lint the codebase
lintr::lint_dir()

# Restore R environment from lockfile
renv::restore()

# Snapshot updated dependencies
renv::snapshot()
```

## Architecture

### Entry Points
- `global.R` — App-wide initialization: settings, utility functions, unit conversions, color helpers, location validation, site/model constructor functions, custom Shiny input/output bindings
- `ui.R` — Main Shiny UI (3-column layout: sidebar → map → data tabs)
- `server.R` — Main Shiny server: cookie handling, site management, reactive data pipeline (`rv$`), result caching via `cache/*.fst` files

### Source Modules (`src/`)
| File | Role |
|------|------|
| `api_ibm.R` | IBM Weather API authentication and hourly data fetching |
| `api_noaa_forecast.R` | NOAA 7-day forecast API |
| `api_openmeteo_forecast.R` | Open-Meteo forecast API |
| `models.R` | Data aggregation pipeline: hourly → daily → moving averages (7/14/21/30-day) → GDD → disease risk models |
| `module_map.R` | Leaflet map Shiny module (site markers, grid polygons, bounds) |
| `module_risk.R` | Crop risk display Shiny module (model selection, results, warnings) |
| `module_data.R` | Data explorer/downloader tab module |
| `plotly.R` | Plotly chart helpers |

### Client-Side (`www/`)
- `script.js` — Site edit/delete actions, modal links, Shiny JS message handlers
- `cookie-handler.js` — Browser cookie persistence for user site list
- `google-places.js` — Google Places autocomplete for location search
- `leaflet-cdl.js` — Cropland Data Layer tile integration

### Data Flow
```
User defines site (lat/lon) →
  IBM API fetch (hourly weather) →
    Daily summaries →
      Moving averages →
        GDD calculations →
          Disease model predictions →
            Plotly charts + leaflet map grid
```

### State Management
- `rv$` reactive values object in `server.R` holds all app state
- Sites persist across sessions via browser cookies (JSON-encoded)
- API auth token cached as `ibm_auth.rds`
- Weather data cached per-site as `cache/*.fst` (FST format)

### Async Execution
Expensive API calls run via `future`/`promises` with 2 worker sessions to avoid blocking the UI.

### Disease Models (12 total)
Defined in `src/models.R` and documented in `docs/`:
- **Corn**: Tar spot, Gray leaf spot, DON/Gibberella
- **Soybean**: White mold, Frogeye
- **Wheat**: Wheat scab
- **Vegetables**: Early blight, Late blight (potato/tomato), Alternaria/Cercospora (carrot), Cercospora (beet), Botrytis (onion)

## Key Conventions
- Indentation: 2 spaces (per `.Rproj` config)
- Linting configured in `.lintr.R` (line length and indentation checks disabled)
- R 4.5.2, dependencies managed with `renv` (see `renv.lock`)
- Test fixtures stored as `.rds` files in `tests/testthat/`
