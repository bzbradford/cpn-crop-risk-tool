# Open-Meteo Migration Plan

Switch the app from IBM EIS hourly weather + IBM/NOAA-derived forecasts to a unified
Open-Meteo pipeline that handles both history and forecast through one async task,
adopting Open-Meteo's column names everywhere and aggressively trimming server-side
reactives.

## Confirmed decisions

1. **Column naming.** Adopt Open-Meteo names everywhere. `precip → precipitation`,
   `snow → snowfall`, `pressure_mean_sea_level → pressure_msl`, `time_zone → timezone`.
   Derived columns rename in kind: `precip_daily → precipitation_daily`, etc.
2. **Missing variables.** Enable `wind_gusts_10m` from Open-Meteo (mapped to
   `wind_gust`). Drop `pressure_change` entirely (Open-Meteo has no equivalent;
   nothing downstream uses it).
3. **Fetch design.** One unified `task_get_weather` ExtendedTask handles both
   historical and forecast Open-Meteo requests in a single parallel batch.
   Forecast is refreshed only when **(a) `end_date >= today()`** AND **(b) the
   last forecast fetch for that grid was more than 1 hour ago**. Past-only date
   ranges never touch the forecast endpoint.
4. **Forecast freshness tracking.** A new `rv$forecast_fetched_at` reactive value:
   a named list keyed by `grid_id`, value = `POSIXct` of last forecast fetch.
5. **Reactive trim.** Aggressive. The combined effect of unified fetch + non-spatial
   site→grid join lets us delete most of the IBM-era plumbing.

## Target architecture

### Storage (single source of truth)

`rv$weather` becomes the only weather state. It carries an `is_forecast` boolean
column so future-hour forecast rows and past-hour history rows can coexist for
the same grid. A small companion `rv$forecast_fetched_at` tracks when each
grid's forecast was last refreshed.

Eliminated: `rv$forecasts`, `rv$grids`, `rv$fetch_attempts`, `.daily_grid_cache`.

### Async tasks

Eliminated: `task_get_forecasts` and its two observers (`Invoke forecast requests`,
`Collect result of forecast requests`).

`task_get_weather` is rewired to call a new top-level helper
`om_fetch_unified()` (proposed name) that internally:
1. Builds historical requests via the existing `om_prep_reqs()` (skips date
   ranges already present per grid).
2. Decides which grids need a forecast refresh (end_date condition + per-grid
   staleness).
3. Builds forecast requests via `om_build_forecast_req()` for those grids.
4. Performs all requests in a single `req_perform_parallel()` call.
5. Parses each response with `om_parse_resp()` + `om_build_hourly()`, tagging
   results with `is_forecast = TRUE/FALSE`.
6. Merges into the existing `wx`:
   - For grids that got a fresh forecast, drop existing `is_forecast = TRUE` rows
     for that grid before binding new forecast rows.
   - Anti-join new history rows against existing `(grid_id, datetime_utc)` so we
     don't duplicate.
7. Returns `list(wx = updated_wx, forecast_fetched_at = updated_timestamps)`.

### Reactive shape (after)

| Reactive | Source | Purpose |
| --- | --- | --- |
| `wx_grids` | `om_build_grids(rv$weather)` | sf for map polygons |
| `grid_status` | `om_grid_status(rv$weather)` | per-grid date range + completeness |
| `sites_with_status` | `om_join_grids(rv$sites, grid_status)` | site rows joined to grid status (non-spatial bbox join) |
| `wx_data` | thin assembly: filter rv$weather to selected dates, compute cumulatives, run `build_daily()` | full payload for downstream modules |

### Reactives to delete from `server.R`

`sites_sf`, `wx_status`, `grids_with_status`, `wx_forecasts`, `wx_daily_hist`,
`wx_hist_key`, `fetch_args`, `fetch_limit_reached`, `need_weather`,
`expanded_dates` (folded into wx_data / fetch helper),
`task_get_forecasts` (and its observers), the `rv$grids` handler observer,
the daily_grid_cache env, and the fetch-limit notification observer.

`need_weather` becomes a small inline helper that calls `om_prep_reqs()` and
asks "does it return any rows?" — used only to gate the auto-fetch timer and
the action button label.

## File-by-file work

### `src/api_openmeteo.R`

1. **`openmeteo_vars`**: uncomment `wind_gust = "wind_gusts_10m"`. Confirm the
   existing `stopifnot()` against `conversion_lookup$measure` still passes
   (it will once `conversion_lookup` includes `wind_gust`).
2. **`om_parse_resp`**: no rename needed — already produces `timezone`,
   `precipitation`, `snowfall`, `pressure_msl`, etc.
3. **`om_build_hourly`**: add an `is_forecast` argument (default `FALSE`) so
   the unified fetcher can tag rows. Place column after `date`.
4. **New `om_fetch_unified()`** (replaces `om_fetch_weather()`):
   ```r
   om_fetch_unified <- function(sites, start_date, end_date, wx = tibble(),
                                 fc_fetched_at = list(),
                                 fc_stale_hours = 1) { ... }
   ```
   - History request building reuses `om_prep_reqs()` and tags responses
     `is_forecast = FALSE`.
   - Forecast eligibility: `end_date >= today()` AND for each grid,
     `is.null(fc_fetched_at[[grid_id]]) || difftime(now(), fc_fetched_at[[grid_id]], units = "hours") > fc_stale_hours`.
   - One `req_perform_parallel()` call for the combined list.
   - Returns `list(wx = merged_wx, fc_fetched_at = updated_list)`.
5. **New merge helper** (or absorb into `om_fetch_unified`): given existing wx,
   new history rows, new forecast rows, and the set of grids whose forecast was
   refreshed, produce the merged tibble per the rules above. The existing
   `om_merge_wx()` covers history merging; forecast replacement needs a new
   step that drops `is_forecast = TRUE` rows for refreshed grids first.
6. **Drop or repurpose** the existing `om_fetch_weather()` if no longer used.

### `global.R`

1. Move `add_date_cols` here from `api_ibm.R` (or into `api_openmeteo.R` — it's
   only called by `om_build_hourly` once IBM is gone). Either is fine; placing
   it in `global.R` keeps it discoverable and signals it's pipeline-agnostic.
2. **Remove** `build_grids` (use `om_build_grids` everywhere). `om_build_grids`
   does **not** include `time_zone` — add `timezone` to its output by passing
   the parsed weather and joining the `(grid_id, timezone)` distinct pairs
   onto its result. Update the function signature accordingly.
3. **Extend `om_wx_status()` to match `annotate_grids`'s contract** so the
   map label code in `global.R` and `module_map.R` doesn't need to change.
   Required additions / renames inside `om_wx_status()`:
   - `min_date → date_min`, `max_date → date_max` (rename for consistency
     with the rest of the codebase).
   - `days_ok → days_actual` (clearer name, also aligns with the IBM-era
     contract that `annotate_grids` currently expects).
   - `days_inc → days_missing`.
   - Add `days_expected` (count of dates in the queried/observed range).
   - Add `days_missing_pct = days_missing / days_expected`.
   - Add `hours_expected` (sum of `hours_expected` from per-day breakdown:
     24 for past days, `hour(now("UTC"))` for today).
   - Add `hours_missing = hours_expected - hours_actual`.
   - Add `hours_missing_pct = hours_missing / hours_expected`.
   - Add `hours_stale`: when `date_max == today()`, hours between the latest
     observed `datetime_utc` and `now("UTC")`; else 0.
   - Add `needs_download`: `TRUE` if `days_missing > 0` or
     `hours_stale > 1` (1h matches the forecast staleness threshold; tune if
     desired).
   - Keep `dates_have` list-column — `om_prep_reqs()` consumes it.
   `om_grid_status()` then inherits these columns automatically via its
   `left_join`.
4. **`OPTS$grid_attr_cols`** and **`OPTS$date_attr_cols`**: replace `"time_zone"`
   with `"timezone"`.
5. **`OPTS$ibm_*`** entries: delete (`ibm_keys`, `ibm_auth_*`,
   `ibm_weather_endpoint`, `ibm_chunk_size`, `ibm_ignore_cols`, `ibm_stale_hours`).
6. **`OPTS$validation_weather_ready`**: prose unchanged (no IBM mention).
7. **`conversion_lookup`**: add `wind_gust` row using `km_to_mi`. The
   `precipitation`/`snowfall`/`pressure_msl` rows already use the new names.

### `src/models.R` — `build_daily` and downstream

Rename column references in `build_daily` (lines 56–80):
- `c(precip, snow)` → `c(precipitation, snowfall)`
- `pressure_mean_sea_level` → `pressure_msl`
- `wind_gust` reference is fine (just renamed at the OM boundary; column already
  named `wind_gust` per the renamed `openmeteo_vars`)
- Derived: `precip_daily → precipitation_daily`, `snow_daily → snowfall_daily`,
  `precip_cumulative → precipitation_cumulative`,
  `snow_cumulative → snowfall_cumulative`
- `dry = (hours_rh_under_70 >= 6) & (precipitation_daily < 1)`

Update model builders that reference `precip_daily`:
- `build_don` (lines 565, 571)
- `build_rye_biomass` (line 1181)
- `build_cotton_planting` (line 1312)
- The Wisconet test data rename block at line 1217 — update or remove (test fixture).

### `server.R`

Sweeping rewrite of the reactive section. Concrete deletions and edits:

**Delete**:
- `rv$forecasts`, `rv$grids`, `rv$fetch_attempts` from `reactiveValues` (lines 138, 137, 153).
- `rv$grids handler` observer (lines 226–240).
- `sites_sf` reactive (281–307).
- `wx_status` reactive (311–317).
- `grids_with_status` reactive (323–328).
- `sites_with_status` reactive (332–341) — replace with new shape (below).
- `need_weather` reactive (346–365) — replace with helper.
- `fetch_args` reactive (370–388).
- `fetch_limit_reached` reactive (393–398).
- `task_get_forecasts` ExtendedTask + the two observers around it (402–448).
- `wx_forecasts` reactive (521–555).
- `wx_hist_key` reactive (561–571).
- `.daily_grid_cache` env and `wx_daily_hist` reactive (577–629).
- The fetch-limit-reached notification observer (1257–1264).

**Add**:
- `rv$forecast_fetched_at <- list()` in `reactiveValues`.
- New `wx_grids` reactive: `om_build_grids(rv$weather)` (extended to include
  `timezone`).
- New `grid_status` reactive: `om_grid_status(rv$weather)` (extended with
  `needs_download` etc.).
- New `sites_with_status` reactive: `om_join_grids(rv$sites, grid_status())`.
- Inline `need_weather()` helper inside `invoke_get_weather` and `action_ui`:
  call `om_prep_reqs(rv$sites, start_date, end_date, grid_status())` and
  return `nrow(reqs) > 0`. Single source of truth for the gate.

**Rewire**:
- `task_get_weather` ExtendedTask: pass
  `(sites, start_date, end_date, wx, fc_fetched_at)` and call
  `om_fetch_unified()`. Result is a list — handler updates both
  `rv$weather` and `rv$forecast_fetched_at`.
- `wx_data` reactive: greatly simplified. With unified storage:
  ```r
  wx_data <- reactive({
    weather <- rv$weather
    sites <- sites_with_status()
    sel_dates <- selected_dates()

    req(nrow(weather) > 0, nrow(sites) > 0)

    # Hourly slice for chart range; expand by 30d for moving-average context
    fetch_start <- sel_dates$start - days(30)
    hourly <- weather |>
      filter(grid_id %in% sites$grid_id,
             between(date, fetch_start, sel_dates$end)) |>
      arrange(grid_id, datetime_utc) |>
      distinct(grid_id, datetime_utc, .keep_all = TRUE)  # hist wins over fc

    daily_full <- build_daily(hourly)

    list(
      sites = sites,
      dates = list(start = sel_dates$start, end = sel_dates$end, today = today()),
      hourly = hourly |>
        filter(date >= sel_dates$start) |>
        mutate(precipitation_cumulative = cumsum(precipitation),
               snowfall_cumulative = cumsum(snowfall),
               .by = grid_id) |>
        relocate(precipitation_cumulative, .after = precipitation) |>
        relocate(snowfall_cumulative, .after = snowfall),
      daily_full = daily_full,
      daily = daily_full |> filter(date >= sel_dates$start)
    )
  })
  ```
  Per-grid daily caching is dropped — `build_daily` is fast enough that the
  complexity isn't justified once forecast no longer arrives on a separate
  cadence.
- The auto-fetch timer block (1224–1254) keeps its shape but the `need_weather()`
  call becomes the inline helper.

**Map module call** (1316–1325): the `map_data` reactive now passes
`grids = wx_grids(), grids_with_status = grid_status() |> left_join(...)
or similar, sites_with_status = sites_with_status()`. Verify with `module_map.R`.

### `src/module_risk.R`

- Line 331: `select(..., time_zone)` → `select(..., timezone)`.

### `src/module_data.R`

- Replace any `time_zone` reference with `timezone` (none direct in current grep,
  but verify after changes propagate via OPTS).
- `OPTS$date_attr_cols` is consumed here; should pick up the rename automatically
  once OPTS is updated.

### `src/module_map.R`

- No code changes expected: `om_wx_status` (extended) feeds the same column
  names `annotate_grids` already reads, and `needs_download` is now part of
  `sites_with_status` via `om_join_grids` against the extended status. Smoke
  test the popup label rendering after the wiring is in place.

### Files to delete

- `src/api_ibm.R` — entire file. Move `add_date_cols` first.
- `src/api_noaa_forecast.R` — entire file. Marked superseded.
- `tests/testthat/test_ibm_api.R` — replace with an Open-Meteo equivalent
  (or delete; integration tests for OM live in `dev/test_openmeteo.R`).

### Tests / fixtures

- `tests/testthat/test_hourly_wx.rds` and `tests/testthat/test_daily_wx.rds`:
  regenerate from a fresh `om_fetch_unified()` call so column names match.
- `tests/testthat/setup.R`: confirm fixture loading still works after rename;
  any explicit column references need the new names.
- `tests/testthat/test_global.R`: scan for `precip`/`snow`/`time_zone` etc.

### Cache invalidation

User-specific `.fst` files in `cache/` were written from IBM-shaped data and
will be unreadable after the schema flip (different columns, missing
`is_forecast`). Two options:

1. **Recommended**: in `Read cached weather` observer (line 77), wrap
   `read_fst()` so a `tryCatch` failure deletes the file and starts fresh.
   That observer already has a `tryCatch` block — extend it to validate the
   columns (e.g., expect `precipitation` not `precip`); if missing, delete
   and clear `rv$weather`. No user-visible disruption beyond a one-time
   re-fetch.
2. Bump a version suffix on the cache filename
   (`get_cache_file()` → `paste0(user_id, "_v2.fst")`) so old files are
   simply ignored and aged out by `clean_old_caches()`.

## Implementation order

Suggested sequence so the app stays runnable between steps where practical:

1. **api_openmeteo.R**:
   - Uncomment `wind_gust` in `openmeteo_vars`.
   - Add `is_forecast` arg to `om_build_hourly`.
   - Extend `om_build_grids` to include `timezone`.
   - Extend `om_wx_status` / `om_grid_status` to add `needs_download` +
     count/percent columns the map label expects.
   - Implement `om_fetch_unified()`.
   - Keep `om_fetch_weather()` temporarily as a thin wrapper if useful.
2. **global.R**:
   - Move `add_date_cols` in.
   - Add `wind_gust` row to `conversion_lookup`.
   - Update `OPTS` (delete IBM keys, rename `time_zone` → `timezone` in
     `grid_attr_cols`/`date_attr_cols`).
   - Replace `build_grids` body to call `om_build_grids` (or delete and
     update callers).
   - `annotate_grids` itself unchanged — `om_wx_status` now provides every
     column it reads.
3. **src/models.R**:
   - Rename column references in `build_daily` and dependent model builders.
4. **src/module_risk.R / module_data.R**:
   - Rename `time_zone` references.
5. **server.R**:
   - Wire `task_get_weather` to `om_fetch_unified()`.
   - Update result handler to set both `rv$weather` and `rv$forecast_fetched_at`.
   - Replace deleted reactives with the new trio.
   - Simplify `wx_data`.
   - Patch the cache reader to invalidate on schema mismatch.
6. **Delete `src/api_ibm.R` and `src/api_noaa_forecast.R`**.
7. **Tests**: regenerate fixtures, update assertions.
8. **Manual smoke test in browser**:
   - Fresh session, no cookie → add a site → fetch → verify forecast
     appears for today/future.
   - Set end_date to last year → fetch → verify no forecast request.
   - Refresh within 1 hour → verify forecast not re-fetched (check console
     timing).
   - Wait > 1 hour → fetch → verify forecast refreshed.
   - Add a second site → fetch → only the new grid hits the API for both
     history (new grid) and forecast (new grid only).

## Endpoint behaviour — confirmed

- **History wins on overlap.** `wx_data` arranges `is_forecast = FALSE` first
  before `distinct(grid_id, datetime_utc, .keep_all = TRUE)` so any
  history-endpoint row for a given hour beats the forecast-endpoint row.
  Worth a unit test.
- **Archive returns full-day hourly even before the day ends.** When `end_date`
  is today, the history endpoint already returns hourly values through 23:00
  local — those late-day rows are model output, not actuals. Per the rule
  above they will still beat dedicated forecast-endpoint rows on collision,
  which is fine (same upstream model in practice). The `is_forecast` flag
  therefore reflects _which endpoint produced the row_, not whether the
  value is observed vs predicted.
- **Forecast endpoint variable parity.** `wind_gusts_10m` and
  `et0_fao_evapotranspiration` are served by the forecast endpoint.
  `soil_temperature_0_to_7cm` and `soil_moisture_0_to_7cm` are **not** served
  but Open-Meteo returns them as empty arrays, so `om_parse_resp()` handles
  them without code changes — soil columns will simply be `NA` for forecast
  rows. Downstream model builders don't currently use soil columns, so no
  immediate action needed; flag if any future model does.

## Open questions / risks

1. **Forecast endpoint timezone behaviour.** Open-Meteo forecast may shift
   `datetime_local` based on `timezone=auto`. Confirm forecast and history
   from the same coordinates produce identical `timezone` strings so the
   merged table doesn't end up with mixed tz tags.
2. **`fetch_limit_reached`** is being deleted. The current behaviour
   (block after 2 failed identical fetches) was a guard against IBM
   intermittent failures. Open-Meteo is generally more reliable but if we
   start seeing flapping fetches we may want a lighter equivalent.
