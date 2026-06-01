# CLAUDE.md

## Project Overview

An R Shiny web application for agricultural weather monitoring and crop disease risk forecasting. It fetches weather data from Open-Meteo (backended by ECMWF IFS weather data), processes it through meteorological pipelines, and displays crop disease risk model results on interactive maps and charts.

## Architecture

### Entry Points
- `global.R` — App-wide initialization: settings, utility functions, unit conversions, color helpers, location validation, site/model constructor functions, custom Shiny input/output bindings
- `ui.R` — Main Shiny UI (3-column layout: sidebar → map → data tabs)
- `server.R` — Main Shiny server: cookie handling, site management, reactive data pipeline (`rv$`), result caching via `cache/*.fst` files

### Source Modules (`src/`)
| File | Role |
|------|------|
| `api_openmeteo.R` | Open-Meteo historical and forecast API |
| `models.R` | Data aggregation pipeline: hourly → daily → disease risk models |
| `module_map.R` | Leaflet map Shiny module (site markers, grid polygons, bounds) |
| `module_risk.R` | Crop risk display Shiny module (model selection, results, warnings) |
| `module_data.R` | Data explorer/downloader tab module |
| `plotly.R` | Plotly chart helpers |

### Client-Side (`www/`)
- `script.js` — Site edit/delete actions, modal links, Shiny JS message handlers
- `cookie-handler.js` — Browser cookie persistence for user site list
- `google-places.js` — Google Places autocomplete for location search
- `leaflet-cdl.js` — Cropland Data Layer tile integration
- `style.css` — Application styles

### Data Flow
1. User defines sites (lat/lon) by map click, csv load, or browser cookie
2. Each site is snapped to its O1280 grid cell via `get_o1280_cells()` — the single source of grid identity (`grid_id`/`grid_lat`/`grid_lng`/polygon). This is idempotent on its own centroids, so the same grid_id flows through sites, stored weather, and status joins.
3. Historical weather fetched per unique grid (deduped, requested at the grid centroid so Open-Meteo resolves the expected cell) and stamped with the canonical grid identity. Cached as per-user .fst files. Invoked via mirai in ExtendedTask.
4. Forecast fetch for each unique grid attached to a site, same canonical stamping. Invoked via mirai in ExtendedTask.
5. Historical and forecast data merged and sent to modules
6. Crop risk module builds each model from daily weather on demand
7. Data explorer module builds and displays hourly/daily/moving average/GDD

### State Management
- `rv$` reactive values object in `server.R` holds all app state
- Sites persist across sessions via browser cookies (JSON-encoded)

### Disease and Insect Models
Defined in `src/models.R` and documented in `docs/`:
- **Corn**: Tar spot, Gray leaf spot, DON/Gibberella
- **Soybean**: White mold, Frogeye
- **Wheat**: Wheat scab
- **Vegetables**: Early blight, Late blight (potato/tomato), Alternaria/Cercospora (carrot), Cercospora (beet), Botrytis (onion)
- **Cover crops**: Winter rye biomass
- **Insect models**: Calculated from GDD

## Key Conventions
- Indentation: 2 spaces
- R 4.5.3, dependencies managed with `renv` (see `renv.lock`)
- Test fixtures stored as `.rds` files in `tests/testthat/`
- Update CLAUDE.md after any major revisions as needed to maintain accuracy
