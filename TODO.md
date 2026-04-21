# Performance TODOs

Performance-improvement ideas for the IBM Weather Tool Shiny app, focused on the
hourly → daily → moving-average/GDD/model pipeline and Shiny reactivity.

Legend:
- **Impact**: rough expected gain (S = small, M = medium, L = large, XL = game-changing)
- **Difficulty**: scope of change (S = trivial, M = moderate refactor, L = large rewrite)
- **Risk**: chance of regression (S/M/L)

---

## A. Reactivity graph / invalidation

### A1. Replace `bindCache(rlang::hash(wx_args()))` with a lightweight key
**Location:** `server.R` `wx_data` reactive (line ~619)
**Problem:** `wx_args()` contains the entire `rv$weather` tibble (up to ~25 sites × 365 days × 24 hrs ≈ 219k rows) plus `sites_with_status()` sf objects, forecast tibble, etc. `rlang::hash()` serialises the whole structure to compute a digest on every dependency change. That's a hidden O(N) cost on every reactive pulse.
**Fix:** Build a synthetic key from cheap scalars — e.g. `list(nrow(weather), max(weather$datetime_utc, na.rm=TRUE), sort(sites$id), sort(sites$grid_id), dates$start, dates$end, nrow(forecast))`. If any of those change, cache misses; otherwise it hits. Forecast can be keyed by `length(fcs)` plus the max `datetime_local`.
**Impact:** M (saves 50–500 ms per reactivity pulse with large weather data)
**Difficulty:** S
**Risk:** S — if the key misses a dependency, you get a stale cache

**[Complete]**

### A2. Make `module_data.R` lazy — don't build MA and GDD until the user needs them
**Location:** `src/module_data.R` lines 131–146
**Problem:** Every time `wx_data()` invalidates, the observer eagerly computes *both* centred and right-aligned moving averages **and** GDDs, even if the user is on the risk tab (or never visits the data tab). `build_ma_from_daily()` is the single most expensive non-API step — `rollapply` over 6 measure groups × 4 widths × N columns × N grids.
**Fix:** Replace the eager observer with three `reactive()` blocks (one for `ma_center`, `ma_right`, `gdd`), each computed only when `selected_data()` asks for them. Use `bindCache()` on each, keyed by `(grid_ids, date range, dataset hash)`.
**Impact:** L (often >50% of wall-clock time after a weather refresh if user is only on risk tab)
**Difficulty:** S–M
**Risk:** S

**[Complete]**

### A3. Debounce date-input observers
**Location:** `server.R` `rv$start_date` / `rv$dates_valid` observers (lines 183–218)
**Problem:** Typing into a `dateInput` fires input changes per keystroke. Each intermediate value invalidates `selected_dates()` → `expanded_dates()` → `wx_args()` → `wx_data()`. The weather fetch is already throttled to 15 s, but rebuilds of daily/MA/GDD fire immediately.
**Fix:** `debounce(500)` the date-derived reactives (or wrap `input$start_date` and `input$end_date` in debounced reactives).
**Impact:** M (smoother UX; fewer wasted rebuilds)
**Difficulty:** S
**Risk:** S

### A4. Eliminate `saved_weather <<-` global (correctness + concurrency)
**Location:** `server.R` line 96, read at line 378
**Problem:** `saved_weather <<- wx` writes a global variable from inside a per-session `server()` function. If two users connect to the same R process, their weather tables clobber each other's `fetch_args()`. Not a perf issue per se, but a silent correctness bug that will become a very confusing perf report ("why did user A suddenly get user B's fetched data back?").
**Fix:** Pass `rv$weather` (or `isolate(rv$weather)`) directly into `fetch_args()`. Remove `saved_weather` entirely.
**Impact:** correctness (critical in multi-user deployments)
**Difficulty:** S
**Risk:** S

**[Complete]**

### A5. Auto-fetch timer wakes up forever
**Location:** `server.R` lines 1107–1126
**Problem:** `invalidateLater(15000)` in an `observe` keeps that observer alive for the whole session, re-running every 15 s. Once weather is complete, it does nothing but still churns.
**Fix:** Guard the invalidateLater inside `req(need_weather())` so it stops after weather catches up. Or restructure as `observeEvent(rv$fetch_timer, ...)` driven by actual state changes.
**Impact:** S (small but measurable background CPU)
**Difficulty:** S
**Risk:** S

### A6. `dt_observer` rebuilds full DT HTML on every selected_site change
**Location:** `server.R` lines 715–727 (`sites_dt_data` + `dt_observer`)
**Problem:** Selecting a different row regenerates all `site_action_link()` HTML strings (N × 2 per click) and `replaceData`s the entire table, even though only the `id` marker (`>123`) changes.
**Fix:** Either cache the buttons column and only update the id column, or use a CSS class toggle via `runjs` for the selection indicator.
**Impact:** S (more noticeable with many sites)
**Difficulty:** S
**Risk:** S

### A7. Split `wx_data` into historical and forecast-augmented layers
**Location:** `server.R` `wx_args` / `wx_data` reactives (lines ~556–619)
**Problem:** `wx_data` depends on both `rv$weather` and `wx_forecasts()`. Post-fetch, the full hourly→daily→MA→GDD pipeline runs on 365 days of historical data. A few seconds later the forecast arrives and invalidates `wx_args()`, triggering the **entire same pipeline** again just to append 7 forecast days on the tail. With 10+ sites this doubles the perceived wait after a fetch. This is the direct cause of the double-cascade you described.
**Fix:** Split into layers:
  1. `wx_hourly_hist` — depends only on `rv$weather` + site list + dates. `bindCache` by identity of those.
  2. `wx_daily_hist` — `build_daily(wx_hourly_hist)`. Cached.
  3. `wx_daily_full` — thin reactive that appends forecast days to `wx_daily_hist`. Only this layer invalidates on forecast arrival, and only processes 7 × N_sites rows.
  4. MA/GDD layers sit on top of `wx_daily_full` (still cheap once forecast only adds tail rows).
  Combines nicely with B4 (per-grid caching): historical daily is reused grid-by-grid, forecast is always a thin append.
**Impact:** L (targets your biggest pain point — the second cascade after forecast arrival)
**Difficulty:** M (reactive refactor; some care needed to keep `selected_dates` filtering correct)
**Risk:** M

**[Complete]**

---

## B. Hourly → Daily pipeline (`build_daily` and friends)

### B1. Fuse the two summarise passes in `build_daily`
**Location:** `src/models.R` lines 42–117
**Problem:** `by_date` and `by_night` each scan the full hourly tibble. For ≥25 grids × 365 days × 24 hrs that's 2 × 219k row scans plus a `left_join`.
**Fix:** Derive `night`, `rh80`, `rh90` as integer flags once; then group once by `c(grid_id, date)` and separately by `c(grid_id, date_since_night)`. Consider doing both groupings in a single pipeline using `data.table::data.table(...)` (fast grouped aggregations) and returning a tibble.
**Impact:** M (halves the hot-path cost of `build_daily`)
**Difficulty:** M
**Risk:** M (reorder aggregations carefully)

### B2. Swap `rollapply(partial = TRUE)` for `data.table::frollmean` / `RcppRoll::roll_*`
**Location:** `src/models.R` `roll_mean`/`roll_sum` (~line 332) and `src/module_data.R` `build_ma_from_daily` (~line 10)
**Problem:** `zoo::rollapplyr(vec, width, \(x) calc_mean(x), partial = TRUE)` calls an R function per window — with 30-day windows over hundreds of days this is slow. `build_ma_from_daily` also runs 4 widths × many columns × many grids.
**Fix:** Use `data.table::frollmean(x, n = width, align = "right", hasNA = TRUE, na.rm = TRUE)` (C implementation, vectorised, handles partial/NA). For `partial = TRUE` behaviour, pass `adaptive = TRUE` with a widths vector, or accept slightly different edge behaviour. `RcppRoll::roll_meanr()` and `slider::slide_dbl()` are alternatives.
**Impact:** L for MA pipeline (5–20x on the rolling step)
**Difficulty:** M (need to match the "partial = TRUE" edge semantics and NA handling)
**Risk:** M (verify numerical parity with existing tests)

**[Complete]**

### B3. Vectorise `gdd_sine`
**Location:** `src/models.R` lines 496–560
**Problem:** `mapply()` over `tmin`, `tmax`, `base` — pure R scalar branching per day. Called inside `build_rye_biomass`, `build_insect`, `build_cotton_planting`, and especially `build_gdd_from_daily` which loops 9 bases × 2 versions (with/without 86F cap) = 18 calls over the whole daily table.
**Fix:** Rewrite as fully vectorised using `case_when()` / arithmetic on whole vectors. Swap branch → `dplyr::case_when(tmax <= base ~ 0, tmin >= base ~ avg - base, ...)`. Or `Rcpp` port for raw speed.
**Impact:** L (GDD build becomes near-instant; rye and insect models benefit too)
**Difficulty:** M
**Risk:** M (need equivalence tests vs existing behaviour — already have `test_daily_wx` fixture)

**[Complete]**

### B4. Cache daily weather per `(grid_id, date)` across fetches
**Location:** `server.R` `wx_data` reactive (line ~580)
**Problem:** When the user adds one new site, the daily build re-runs over *all* sites (including ones whose hourly data didn't change). Same for MA and GDD.
**Fix:** Maintain a module-level `daily_cache` keyed by `grid_id`. For each grid, rebuild `daily_full` only if that grid's hourly data (or its max datetime) changed since last build; otherwise reuse cached rows. Same idea for MA.
**Impact:** M–L (adding/removing sites becomes cheap)
**Difficulty:** M
**Risk:** M (cache invalidation logic)

**[Complete]**

### B5. Filter `rv$weather` to needed grids before building
**Location:** `server.R` `wx_data` (line ~594)
**Problem:** Already filters by `grid_id %in% sites$grid_id` and date range in the pipeline, but `rv$weather` persists all sites ever downloaded this session. A user who has churned through 50 sites but has 5 selected is still carrying 50-grid baggage in every reactive. Hashing, joining, and distinct() all touch the dead rows.
**Fix:** Either prune `rv$weather` to "currently live" grids on site changes, or make `wx_data` pull via `filter()` first as the earliest step (already partly done, just ensure the filter runs before joins/hashes).
**Impact:** S–M (depends on user session length)
**Difficulty:** S
**Risk:** M (don't evict data a user might still want if they restore a site)

### B6. Single-pass arrange + distinct in `fetch_weather` merge
**Location:** `src/api_ibm.R` line ~490 `bind_rows(new_wx, wx) |> distinct(...) |> arrange(...)`
**Problem:** `distinct(grid_id, datetime_utc, .keep_all = TRUE)` on the full combined tibble is O((N+M) log(N+M)) each time. For large existing weather, repeated fetches keep re-deduping everything.
**Fix:** Anti-join new_wx against existing on `(grid_id, datetime_utc)` before `bind_rows`, skipping the global dedup. Or maintain weather as keyed by `(grid_id, datetime_utc)` and merge with `data.table::setkey` + update-on-join.
**Impact:** S–M
**Difficulty:** S
**Risk:** S

### B7. Per-grid dirty tracking — only rebuild daily/MA/GDD for changed grids
**Location:** `server.R` `wx_data` reactive (line ~580), cooperating with A7 and B4
**Problem:** When `rv$weather` grows by one new site's hourly data, the daily summary, moving averages, and GDD tables are recomputed over *every* grid currently in scope, not just the new one. For the "I added one more site to an existing set of 10" case, this is ~10× more work than needed.
**Fix:** Maintain `grid_cache` (list keyed by `grid_id`) with the most recent `(max_datetime_utc, daily_tbl, ma_tbl, gdd_tbl)` for each grid. On each `wx_data` evaluation:
  - For each `grid_id` in scope, compare the grid's max datetime in `rv$weather` to the cached value.
  - Recompute daily/MA/GDD only for grids that are new or changed.
  - `bind_rows` the results.
  Combine with A7 so forecast-driven invalidation only touches the tail (7 days × N grids), not the full history.
**Impact:** L (converts incremental fetches from O(all sites) to O(new sites))
**Difficulty:** M (cache plumbing + invalidation logic)
**Risk:** M (make sure cache is cleared on date-range changes, not just grid changes)

---

## C. Async, caching, and I/O

### C1. Async cache writes + lower compression
**Location:** `server.R` lines 94–107
**Problem:** `write_fst(wx, fname, compress = 99)` runs synchronously on the main Shiny process every time `rv$weather` changes. `compress = 99` is the maximum setting and dramatically slower than compress = 50 for marginal size gains. Worse, the write blocks the UI.
**Fix:**
  1. Drop to `compress = 50` (or even 0 — fst's format is already efficient).
  2. Offload to a mirai worker (`mirai({ write_fst(wx, fname, compress = 50) })`).
  3. Throttle/debounce so you don't write twice in quick succession.
**Impact:** M (UI feels snappier after a big fetch)
**Difficulty:** S
**Risk:** S

### C2. Cache `lutz::tz_lookup_coords`
**Location:** `src/api_ibm.R` lines 202 (`ibm_create_reqs`) and 321 (`ibm_clean_resp`)
**Problem:** `lutz::tz_lookup_coords` is called once per request built and once per hourly response parsed. For the response, it's called with `grid_lat, grid_lng` on the whole hourly vector — each unique lat/lng is looked up repeatedly. The lookup uses a shapefile intersection which isn't cheap.
**Fix:** Memoise by rounded `(lat, lng)` pair, or in `ibm_clean_resp` call once per distinct `(grid_lat, grid_lng)` then join.
**Impact:** S–M (particularly visible on multi-site fetches)
**Difficulty:** S
**Risk:** S

### C3. Vectorise `validate_ll`
**Location:** `global.R` lines 695–709
**Problem:** `mapply(function(lat, lng) { st_point(...) |> st_sfc(...) |> st_transform(...); st_intersection(pt, poly) }, lat, lng)` builds one point at a time, transforms one at a time, intersects one at a time. For CSV uploads of 25 sites and for `parse_cookie_sites` this is 25× slower than necessary.
**Fix:** Build a single `sfc` of all points, transform once, use `st_intersects(pts, service_bounds_3857, sparse = FALSE)` to get a logical vector. `st_intersects` is also faster than `st_intersection` for boolean tests.
**Impact:** M for CSV upload path (25 sites: seconds → <100 ms)
**Difficulty:** S
**Risk:** S

### C4. Skip forecast re-invoke when no new grids
**Location:** `server.R` lines 427–434
**Problem:** The forecast observer fires on every `rv$grids` change and hands the grids + current forecasts to the mirai task. The task skips already-fetched grids internally, but the observer still spawns a new worker task and round-trips the whole forecast list.
**Fix:** Guard invocation: `new_ids <- setdiff(grids$grid_id, names(forecasts)); req(length(new_ids) > 0)`.
**Impact:** S
**Difficulty:** S
**Risk:** S

### C5. `clean_old_caches()` at session end — consider scheduling instead
**Location:** `global.R` lines 1036–1047 and `server.R` line 1213
**Problem:** Every session end stats every cache file. For a busy app this is O(users × sessions × files).
**Fix:** Run once per app startup (in `global.R`) rather than per session end; or use a session-startup sample so it runs probabilistically.
**Impact:** S
**Difficulty:** S
**Risk:** S

---

## D. Reactive result caching (memoisation)

### D1. `bindCache` on model `build_*` results
**Location:** `src/module_risk.R` `model_data` reactive (line ~235)
**Problem:** When the user toggles the model picker (e.g. tarspot → gls → tarspot), each visit rebuilds from scratch. Switching model groups also invalidates. Same expensive roll-means get recomputed for the same daily data.
**Fix:** Wrap `model_data` with `bindCache(model$slug, <key-for-daily_full>, <model-specific-params>)`. Key should exclude the full tibble — just hash `(grid_ids, max(date), model params)`.
**Impact:** M (interactive model-picker feel: near-instant re-selection)
**Difficulty:** S
**Risk:** S

### D2. Pre-compute shared MA columns once instead of per-model
**Location:** Multiple `build_*` functions in `src/models.R`
**Problem:** Several disease models compute the same rolling windows on the same columns (e.g. `temperature_min_21day` appears in `build_tar_spot`, `build_gray_leaf_spot`, `build_white_mold`; `temperature_max_30day` in `build_white_mold` and `build_frogeye_leaf_spot`; etc.). If the user flips between models, the same rolls get recomputed.
**Fix:** Add a memoised "rolling features" layer computed once per `daily_full` (similar to `build_ma_from_daily` but only the rolls actually referenced by models). Individual `build_*` functions read features from that table instead of computing them.
**Impact:** M (compounding with D1)
**Difficulty:** M (touches every model)
**Risk:** M (equivalence testing needed)

### D3. `bindCache` on `selected_data` in module_data
**Location:** `src/module_data.R` line ~149
**Problem:** Toggling metric/imperial or flipping dataset type rebuilds the join + `signif()` every time.
**Fix:** Cache by `(data_type, ma_align, metric, forecast_on, hash-of-wx-keys)`.
**Impact:** S–M
**Difficulty:** S
**Risk:** S

---

## E. Lower-level code micro-optimisations

### E1. Replace `calc_mean`/`calc_sum` wrappers where NA-handling isn't needed
**Location:** `global.R` lines 303–329
**Problem:** Each wrapper does `all(is.na(x))` + `mean(x, na.rm=TRUE)`. Called inside `rollapply` this is called per window. The `all(is.na)` scan is redundant (`mean(NA_vec, na.rm=TRUE)` just returns `NaN`), though it does avoid the `-Inf/Inf` on `min`/`max`.
**Fix:** For contexts where you don't need the all-NA → NA coercion, use `mean(x, na.rm=TRUE)` directly. Alternatively, combine with B2 (switch to `frollmean` which has its own NA handling).
**Impact:** S
**Difficulty:** S
**Risk:** S

### E2. `build_gdd_from_daily` mutate loop re-sorts names
**Location:** `src/module_data.R` lines 82–88
**Problem:** `select(all_of(sort(names(.))))` inside the pipe forces a full column re-permutation. Fine, but done every render.
**Fix:** Build the column order once, store, reuse. Micro-opt.
**Impact:** S
**Difficulty:** S
**Risk:** S

### E3. `sites_sf()` rebuild on every weather change
**Location:** `server.R` line ~278
**Problem:** `sites_sf` depends on both `rv$sites` and `wx_grids()` (via the join when `weather_ready`). Weather changes → join re-runs even if sites didn't move.
**Fix:** Split: `sites_geom` depends only on `rv$sites`; a separate reactive adds grid attrs via join only when needed. For display (map) you often only need the geometry.
**Impact:** S
**Difficulty:** S
**Risk:** S

---

## F. Diagnostics / measurement first

### F1. Add lightweight profiling hooks before optimising
**Problem:** Without numbers, we'll optimise the wrong things.
**Fix:** Drop `runtime()` calls (already defined in `global.R`) around:
  - `fetch_weather` total time
  - `build_daily` call inside `wx_data`
  - `build_ma_from_daily` in the data module observer
  - `build_gdd_from_daily`
  - Each `build_*` model call
Or wire in `profvis::profvis()` on a representative session and save the report.
**Impact:** N/A (enables rank-ordering the rest)
**Difficulty:** S
**Risk:** none

### F2. Verify numerical parity after each pipeline change
**Problem:** Models are validated against existing behaviour via `test_hourly_wx` fixture.
**Fix:** For every change in section B and D2, run `testthat::test_dir("tests/testthat")` and add regression tests comparing old vs new outputs on the fixture.

---

## Rough priority order

App is already fast enough for typical users (1–3 sites, 30-day window). The goal here is
fixing the "10+ sites takes many seconds, then takes them again when the forecast arrives"
pain path. Ordered for biggest wins on that case first.

### Phase 1 — kill the double-cascade

These directly attack the observed "rebuild, then rebuild again when forecast lands" issue.

1. **A2** (lazy MA/GDD in data module) — L/S. Removes the biggest chunk of post-fetch work, especially for users never on the data tab.
2. **A7** (split historical vs forecast-augmented `wx_data`) — L/M. Makes the *second* cascade (forecast arrival) nearly free.
3. **A4** (drop `saved_weather <<-`) — correctness prerequisite before raising Connect's max-connections-per-process above 1.

### Phase 2 — make the remaining rebuilds fast

Even after Phase 1, full rebuilds still happen on first fetch. Phase 2 makes each one cheap.

4. **B2** (`rollapply` → `frollmean`) — L/M. 5–20× on rolling steps.
5. **B3** (vectorise `gdd_sine`) — L/M. GDD build becomes near-instant.
6. **A1** (cheap `bindCache` key for `wx_data`) — M/S. Stops hashing the whole weather tibble on every pulse.

### Phase 3 — incremental rebuilds and polish

Compounds with the above; less necessary if Phase 1 + 2 already feel fast enough.

7. **B7** (per-grid dirty tracking) + **B4** (daily cache by grid) — L/M. Adding a site only recomputes that site.
8. **D1** (`bindCache` on `model_data`) — M/S. Snappy model-picker toggling.
9. **C1** (async/low-compress cache write) — M/S. Removes a blocking write after each fetch.
10. **A3** (debounce dates) — M/S.
11. **C2** (tz lookup cache), **C3** (vectorise `validate_ll`), **C4** (guard forecast invoke), **A5** (auto-fetch timer) — small, cheap polish items.

B1 (fuse `build_daily` passes), D2 (shared rolling features across models), and an `Rcpp` port of `gdd_sine` are larger rewrites — only worth it if Phase 1 + 2 don't bring wall-clock time into "fast enough" territory.

---

## G. Posit Connect runtime configuration

These settings live per-app under **Access → Runtime** in Connect.

### Before A4 is fixed (today)
- **Max connections per process:** **1** (mandatory). The `saved_weather <<-` global in `server.R:96` writes to `.GlobalEnv`, so two concurrent sessions in one R process will overwrite each other's fetch state. Until A4 lands, each user must have their own process.
- **Load factor:** irrelevant while max-connections = 1.
- **Min processes:** 1, or 2 if cold start is painful — `global.R` loads a large dependency stack plus `mirai::daemons(2)` + `everywhere({ library(...) })`, so cold start is probably 5–10 s.
- **Max processes:** peak expected concurrent users + small headroom.
- **Idle timeout:** default (5 s). Session-end calls `clean_old_caches()`.

### After A4 is fixed
- **Max connections per process:** **3** initially. Shiny is single-threaded per R process, so any one user's heavy reactive (current MA/GDD rebuild with 10+ sites) blocks the others in that process during the rebuild. Keep this low until Phase 2 lands, then 5 should feel fine.
- **Load factor:** **0.5**. Packs users moderately into existing processes before spinning new ones — a reasonable default for bursty Shiny workloads.
- **Min processes:** 1–2.
- **Max processes:** ceil(peak concurrent users ÷ max-connections-per-process) + headroom.
- **Initial / Connection / Read timeouts:** defaults are fine unless CSV uploads or slow IBM fetches trigger them.

### mirai daemon count
In `global.R` you start `mirai::daemons(2)` per R process. Those workers are shared across all users in that process. With max-connections-per-process = 3–5, the 2 daemons become a queue bottleneck when multiple users fetch weather or forecasts simultaneously. Options:

- Bump to `mirai::daemons(4)` once max-connections goes above 1 — costs some RAM per process, avoids serialising user requests behind each other.
- Or leave at 2 and rely on Connect spinning up more processes under load. Simpler, but individual processes feel less responsive under concurrent fetch load.

Recommended: bump to 4 when you raise max-connections to 3+.
