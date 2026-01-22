# IBM Weather API interface
# https://www.ibm.com/docs/en/geospatial-apis?topic=apis-weather-data

# Authentication ---------------------------------------------------------------

#' Get the authorization token from the authentication server
#' @param url IBM authentication endpoint
#' @param keys list with org_id, tenant_id, and api_key
ibm_refresh_auth <- function(
  url = OPTS$ibm_auth_endpoint,
  keys = OPTS$ibm_keys
) {
  require(httr2)
  auth <- tryCatch(
    {
      req <- request(url) |>
        req_url_query(orgId = keys$org_id) |>
        req_headers_redacted(
          "x-ibm-client-id" = sprintf("saascore-%s", keys$tenant_id),
          "x-api-key" = keys$api_key
        ) |>
        req_timeout(5) |>
        req_error(is_error = \(resp) FALSE)
      resp <- req_perform(req)
      status <- resp_status(resp)

      if (status >= 400) {
        echo(resp)
        stop("HTTP error ", status)
      }

      message("Authorization token refreshed at ", now("UTC"))
      list(
        timestamp = now("UTC"),
        status = status,
        token = resp_body_string(resp)
      )
    },
    error = function(e) {
      message("Failed to get authorization token: ", e$message)
      list(
        timestamp = 1,
        status = 500,
        token = NULL
      )
    }
  )
  saveRDS(auth, "ibm_auth.rds")
  assign("ibm_auth", auth, envir = .GlobalEnv)
  auth
}

# ibm_refresh_auth()

#' Get the current IBM token or refresh if needed
#' token is valid for 1 hour
#' @returns auth token
ibm_get_token <- function() {
  # look for a stored token if available
  if (!exists("ibm_auth")) {
    if (file.exists("ibm_auth.rds")) {
      auth <- read_rds("ibm_auth.rds")
      assign("ibm_auth", auth, envir = .GlobalEnv)
      auth
    } else {
      ibm_refresh_auth()
    }
  }

  # if token is stale get a new one
  if (ibm_auth$timestamp < now("UTC") - minutes(59)) {
    ibm_refresh_auth()
  }

  ibm_auth$token
}

# ibm_get_token()

# IBM API interface ------------------------------------------------------------

#' Convert vector of dates to vector of hourly datetimes
dates_to_dttm <- function(dates, tz = "UTC") {
  max_len <- OPTS$ibm_chunk_size
  offset <- minutes(20)
  dttms <- seq(
    from = as_datetime(min(dates), tz = tz) + offset,
    to = as_datetime(max(dates), tz = tz) + hours(23) + offset,
    by = as.difftime(hours(1))
  )
  if (length(dttms) > max_len) {
    warning(
      "Sequence length of ",
      length(dttms),
      " longer than maximum ",
      max_len
    )
  }
  dttms
}

# dates_to_dttm(seq.Date(today() - days(30), today(), 1))

#' Breaks up longer time periods into 1000 hour chunks
#' API will only return 1000 hours at a time
#' @param dates_need vector of dates needed
#' @param dates_have vector of dates already downloaded
#' @param tz time zone to use
#' @returns list of lists `list(start, end, length)`
ibm_chunks <- function(dates_need, dates_have = NULL, tz = "UTC") {
  if (all(dates_need %in% dates_have)) {
    return(NULL)
  }

  chunk_size <- 40 # days
  dates <- seq.Date(min(dates_need), max(dates_need), 1)
  n_chunks <- ceiling(length(dates) / chunk_size)
  i <- 1
  date_chunks <- list()

  while (length(dates) > 0) {
    # pick chunks from outside in
    if (i %% 2 != 0) {
      picked <- dates[seq_len(min(chunk_size, length(dates)))]
    } else {
      picked <- dates[max(1, length(dates) - chunk_size):length(dates)]
    }
    dates <- as_date(setdiff(dates, picked))
    date_chunks[[i]] <- picked
    i <- i + 1
  }

  # remove any chunks we already have covered
  if (length(dates_have) > 0) {
    date_chunks <- lapply(date_chunks, function(chunk) {
      if (all(chunk %in% dates_have)) NULL else chunk
    }) |>
      compact()
  }

  dttm_chunks <- lapply(date_chunks, function(chunk) dates_to_dttm(chunk, tz))
  lapply(dttm_chunks, function(chunk) {
    list(
      start = first(chunk),
      end = last(chunk),
      length = length(chunk)
    )
  })
}

# ibm_chunks(dates_need = seq.Date(as_date("2024-3-1"), as_date("2024-8-1"), by = 1))

#' Create a single request for weather data, maximum length 1000 hours
#' @param lat latitude of point
#' @param lng longitude of point
#' @param start_time dttm
#' @param end_time dttm
#' @param url endpoint, changed only for testing failures
#' @param token IBM authentication token
#' @returns httr2 request
ibm_create_request <- function(
  lat,
  lng,
  start_time,
  end_time,
  url = OPTS$ibm_weather_endpoint,
  token = ibm_get_token()
) {
  request(url) |>
    req_headers_redacted(
      "x-ibm-client-id" = sprintf("geospatial-%s", OPTS$ibm_keys$tenant_id),
      "Authorization" = sprintf("Bearer %s", token)
    ) |>
    req_url_query(
      format = "json",
      geocode = str_glue("{lat},{lng}"),
      startDateTime = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
      endDateTime = format(end_time, "%Y-%m-%dT%H:%M:%S%z"),
      units = "m"
    ) |>
    req_timeout(5) |>
    req_retry(max_tries = 2, failure_timeout = 5, retry_on_failure = TRUE) |>
    req_throttle(rate = 20)
}

# ibm_create_request(45, -89, now() - days(10), now() - days(5)) |> req_perform()

#' Create a list of necessary to send to IBM for weather data
#' Using the date vectors it selects time chunks and creates draft requests
#' API documentation: https://docs.google.com/document/d/13HTLgJDpsb39deFzk_YCQ5GoGoZCO_cRYzIxbwvgJLI/edit?tab=t.0
#' @param lat latitude of point
#' @param lng longitude of point
#' @param dates_need vector of dates needed
#' @param dates_have vector of dates already downloaded
#' @returns httr2 list of requests
ibm_create_reqs <- function(lat, lng, dates_need, dates_have = Date()) {
  t <- now()
  tz <- lutz::tz_lookup_coords(lat, lng, warn = FALSE)
  chunks <- ibm_chunks(dates_need, dates_have, tz)

  if (length(chunks) == 0) {
    message(str_glue("Already had weather for {lat}, {lng}"))
    return(tibble())
  }

  start_date <- min(dates_need)
  end_date <- max(dates_need)

  reqs <- lapply(chunks, function(chunk) {
    ibm_create_request(lat, lng, chunk$start, chunk$end)
  })

  duration <- now() - t
  message(sprintf(
    "Built requests for %.3f,%.3f from %s to %s with %s calls in %.05f",
    lat,
    lng,
    start_date,
    end_date,
    length(reqs),
    duration
  ))

  reqs
}


#' Execute the list of IBM weather requests
#' @param `reqs` list of IBM requests created by `ibm_create_reqs`
#' @returns tibble ingestable by `ibm_clean_resp` and ready for the data pipeline
get_ibm <- function(reqs) {
  stime <- Sys.time()

  resps <- tryCatch(
    {
      token <- ibm_get_token()
      if (!is.character(token)) {
        stop("Failed to get IBM token.")
      }

      message(sprintf("GET ==> %s IBM requests", length(reqs)))

      # perform parallel requests for each time chunk
      resps <- req_perform_parallel(
        reqs,
        on_error = "continue",
        progress = FALSE
      )

      # gather response data
      lapply(resps, function(resp) {
        tryCatch(
          {
            if ("httr2_failure" %in% class(resp)) {
              echo(resp)
              stop("Request failed")
            }
            if (resp_status(resp) != 200) {
              stop(paste0(
                "Received status ",
                resp_status(resp),
                " with message ",
                resp_status_desc(resp)
              ))
            }
            resp_body_json(resp, simplifyVector = TRUE) |> as_tibble()
          },
          error = function(e) {
            message(e$message)
            tibble()
          }
        )
      })
    },
    error = function(e) {
      message(e$message)
      tibble()
    }
  )

  wx <- bind_rows(resps)
  if (nrow(wx) > 0) {
    message(sprintf(
      "OK ==> Performed %s requests in %s sec",
      length(reqs),
      round(Sys.time() - stime, 3)
    ))
    return(wx)
  } else {
    message("FAIL ==> Requests did not succeed")
    return(NULL)
  }
}


# Post-processing --------------------------------------------------------------

#' Does some minimal processing on the IBM response to set local time and date
#' @param ibm_response hourly weather data received from API
#' @returns tibble
ibm_clean_resp <- function(ibm_response) {
  if (nrow(ibm_response) == 0) {
    return(tibble())
  }
  ibm_response |>
    select(-OPTS$ibm_ignore_cols) |>
    select(
      grid_id = gridpointId,
      grid_lat = latitude,
      grid_lng = longitude,
      datetime_utc = validTimeUtc,
      everything()
    ) |>
    clean_names() |>
    mutate(across(datetime_utc, ~ parse_date_time(.x, "YmdHMSz"))) |>
    mutate(
      time_zone = lutz::tz_lookup_coords(grid_lat, grid_lng, warn = FALSE),
      .after = datetime_utc
    ) |>
    mutate(
      datetime_local = with_tz(datetime_utc, first(time_zone)),
      .by = time_zone,
      .after = time_zone
    ) |>
    mutate(date = as_date(datetime_local), .after = datetime_local)
}

#' All possible and currently enabled weather columns
#' some are renamed for clarity
ibm_vars <- c(
  "temperature",
  # "temperature_change24hour",
  # "temperature_max24hour",
  # "temperature_min24hour",
  "dew_point" = "temperature_dew_point",
  # "temperature_feels_like",
  "relative_humidity",
  "precip" = "precip1hour",
  # "precip6hour",
  # "precip24hour",
  # "precip2day",
  # "precip3day",
  # "precip7day",
  # "precip_mtd",
  # "precip_ytd",
  "snow" = "snow1hour",
  # "snow6hour",
  # "snow24hour",
  # "snow2day",
  # "snow3day",
  # "snow7day",
  # "snow_mtd",
  # "snow_season",
  # "snow_ytd",
  # "uv_index",
  # "visibility",
  "wind_speed",
  "wind_gust",
  "wind_direction",
  "pressure_mean_sea_level",
  "pressure_change"
)

#' df must have cols `date` and `datetime_local`
add_date_cols <- function(df) {
  df |>
    mutate(
      yday = yday(date),
      year = year(date),
      month = month(date),
      day = day(date),
      hour = hour(datetime_local),
      night = !between(hour, 7, 19), # night is between 20:00 and 6:00
      date_since_night = as_date(datetime_local + hours(4)),
      .after = date,
      .by = grid_id
    )
}

#' Creates the working hourly weather dataset from cleaned ibm response
#' @param ibm_hourly hourly weather data from `ibm_clean_resp` function
#' @returns tibble
build_hourly <- function(ibm_hourly) {
  ibm_hourly |>
    select(
      grid_id,
      grid_lat,
      grid_lng,
      datetime_utc,
      time_zone,
      datetime_local,
      date,
      all_of(ibm_vars)
    ) |>
    add_date_cols() |>
    arrange(grid_lat, grid_lng, datetime_local) |>
    mutate(
      dew_point_depression = abs(temperature - dew_point),
      .after = dew_point
    )
}


# Weather helpers --------------------------------------------------------------

#' Update weather for sites list and date range
#' @param wx existing weather data
#' @param sites sf with site locs
#' @param start_date
#' @param end_date
fetch_weather <- function(wx, sites, start_date, end_date) {
  all_dates <- seq.Date(start_date, end_date, 1)
  sites <- sites |>
    st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE)
  reqs <- list()

  # for each site see how much weather is needed
  for (i in seq_len(nrow(sites))) {
    site <- slice(sites, i)

    # already have some weather? find dates to download
    dates_need <- all_dates
    dates_have <- Date()

    if (nrow(wx) > 0) {
      grids <- build_grids(wx)
      wx_status <- weather_status(wx, start_date, end_date)
      grid_status <- grids |>
        left_join(wx_status, join_by(grid_id))
      site <- st_join(site, grid_status)

      # if weather is up to date don't download
      if (isFALSE(site$needs_download)) {
        next
      }

      # if there is at least one day already downloaded check each date for completeness
      if (isTruthy(site$days_actual)) {
        # tz <- site$time_zone
        date_status <- wx |>
          filter(grid_id == site$grid_id) |>
          filter(between(date, start_date, end_date)) |>
          daily_status()
        dates_have <- date_status |>
          filter(hours_missing <= 2) |>
          pull(date)
        dates_need <- as_date(setdiff(all_dates, dates_have))
      }
    }

    # skip if up to date
    if (length(dates_need) == 0) {
      next
    }

    # create requests
    new_reqs <- ibm_create_reqs(
      lat = site$lat,
      lng = site$lng,
      dates_need = dates_need,
      dates_have = dates_have
    )
    reqs <- append(reqs, new_reqs)
  }

  # send requests if any
  if (length(reqs) == 0) {
    message("No weather requests in queue")
    return()
  }

  resp <- get_ibm(reqs)

  # handle response
  if (!isTruthy(resp)) {
    message("Failed to get any weather response")
    return(NULL)
  }

  # process response
  status_msg <- NULL
  new_wx <- resp |>
    ibm_clean_resp() |>
    build_hourly()

  wx <- bind_rows(new_wx, wx) |>
    distinct(grid_id, datetime_utc, .keep_all = TRUE) |>
    arrange(grid_id, datetime_utc)

  wx
}

# fetch_weather(tibble(), tibble(lat = 45, lng = -89), today() - days(7), today())

#' Similar to weather_status but returns number of hours per day
#' to check for any incomplete days
#' @param wx hourly weather data
#' @param tz time
daily_status <- function(wx) {
  wx |>
    summarize(
      tz = coalesce(first(time_zone), "UTC"),
      hours = n(),
      .by = c(grid_id, date)
    ) |>
    mutate(
      start_hour = ymd_hms(paste(date, "00:20:00"), tz = first(tz)),
      end_hour = if_else(
        date == today(tzone = first(tz)),
        now(tzone = tz),
        ymd_hms(paste(date, "23:20:00"), tz = first(tz))
      ),
      hours_expected = hours_diff(start_hour, end_hour) + 1,
      hours_missing = hours_expected - hours
    )
}


#' Summarize downloaded weather data by grid cell and creates sf object
#' used to intersect site points with existing weather data
#' @param wx hourly weather data from `ibm_clean_resp` function
#' @param start_date start of expected date range
#' @param end_date end of expected date range
#' @returns tibble
weather_status <- function(
  wx,
  start_date = min(wx$date),
  end_date = max(wx$date)
) {
  # return simple df if no weather
  if (nrow(wx) == 0) {
    return(tibble(
      grid_id = NA,
      needs_download = TRUE
    ))
  }

  dates_expected <- seq.Date(start_date, end_date, 1)

  # summarize for each grid
  lapply(unique(wx$grid_id), function(g) {
    grid_wx <- wx |>
      filter(grid_id == g, between(date, start_date, end_date))

    if (nrow(grid_wx) == 0) {
      tibble(grid_id = g, needs_download = TRUE)
    } else {
      grid_wx |>
        daily_status() |>
        summarize(
          tz = first(tz),
          date_min = min(date),
          date_max = max(date),
          time_min = min(start_hour),
          time_max = max(end_hour),
          days_expected = length(dates_expected),
          days_actual = n_distinct(date),
          days_missing = max(0, days_expected - days_actual),
          days_missing_pct = days_missing / days_expected,
          days_incomplete = sum(hours_missing > OPTS$ibm_stale_hours),
          hours_expected = sum(hours_expected),
          hours_missing = sum(hours_missing),
          hours_missing_pct = hours_missing / hours_expected,
          hours_stale = if_else(
            date_max == today(tzone = tz),
            hours_diff(time_max, now(tzone = tz)),
            0
          ),
          stale = hours_stale > OPTS$ibm_stale_hours,
          needs_download = stale | days_missing > 0 | days_incomplete > 0,
          .by = grid_id
        ) |>
        select(-tz)
    }
  }) |>
    bind_rows()
}
