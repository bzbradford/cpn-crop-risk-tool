# IBM API interface ----

#' Convert vector of dates to vector of hourly datetimes
dates_to_dttm <- function(dates, tz = "UTC") {
  max_len <- OPTS$ibm_chunk_size
  offset <- minutes(20)
  dttms <- seq(
    from = as_datetime(min(dates), tz = tz) + offset,
    to = as_datetime(max(dates), tz = tz) + hours(23) + offset,
    by = as.difftime(hours(1))
  )
  if (length(dttms) > max_len) warning("Sequence length of ", length(dttms), " longer than maximum ", max_len)
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
    }) %>% compact()
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

# # should show 4 chunks
# ibm_chunks(
#   dates_need = seq.Date(as_date("2024-3-1"), as_date("2024-8-1"), by = 1)
# )
#
# # should show 2 chunks (2 are covered)
# ibm_chunks(
#   dates_need = seq.Date(as_date("2024-3-1"), as_date("2024-8-1"), by = 1),
#   dates_have = seq.Date(as_date("2024-4-1"), as_date("2024-7-1"), by = 1)
# )
#
# # should be null (all dates covered)
# ibm_chunks(
#   dates_need = seq.Date(as_date("2024-5-1"), as_date("2024-7-1"), by = 1),
#   dates_have = seq.Date(as_date("2024-3-1"), as_date("2024-8-1"), by = 1)
# )


#' Create a single request for weather data, maximum length 1000 hours
#' @param lat latitude of point
#' @param lng longitude of point
#' @param start_time dttm
#' @param end_time dttm
#' @param url endpoint, changed only for testing failures
#' @param token IBM authentication token
#' @returns httr2 request
create_ibm_request <- function(lat, lng, start_time, end_time, url = OPTS$ibm_weather_endpoint, token = get_ibm_token()) {
  request(url) %>%
    req_headers_redacted(
      "x-ibm-client-id" = sprintf("geospatial-%s", OPTS$ibm_keys$tenant_id),
      "Authorization" = sprintf("Bearer %s", token)
    ) %>%
    req_url_query(
      format = "json",
      geocode = str_glue("{lat},{lng}"),
      startDateTime = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
      endDateTime = format(end_time, "%Y-%m-%dT%H:%M:%S%z"),
      units = "m"
    ) %>%
    req_timeout(5) %>%
    req_retry(max_tries = 2, failure_timeout = 5, retry_on_failure = TRUE) %>%
    req_throttle(rate = 20)
}

# # test
# create_ibm_request(45, -89, now() - days(10), now() - days(5)) %>%
#   req_perform()


#' Create a list of necessary to send to IBM for weather data
#' Using the date vectors it selects time chunks and creates draft requests
#' API documentation: https://docs.google.com/document/d/13HTLgJDpsb39deFzk_YCQ5GoGoZCO_cRYzIxbwvgJLI/edit?tab=t.0
#' @param lat latitude of point
#' @param lng longitude of point
#' @param dates_need vector of dates needed
#' @param dates_have vector of dates already downloaded
#' @returns httr2 list of requests
create_ibm_reqs <- function(lat, lng, dates_need, dates_have = Date()) {
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
    create_ibm_request(lat, lng, chunk$start, chunk$end)
  })

  duration <- now() - t
  message(sprintf("Built requests for %.3f,%.3f from %s to %s with %s calls in %.05f", lat, lng, start_date, end_date, length(reqs), duration))

  reqs
}

# test_ibm_reqs <- create_ibm_reqs(43.0731, -89.4012, dates_need = seq.Date(ymd("2025-1-1"), ymd("2025-4-1"), by = 1))

# str(test_ibm_reqs)



#' Execute the list of IBM weather requests
#' @param `reqs` list of IBM requests created by `create_ibm_reqs`
#' @returns tibble ingestable by `clean_ibm` and ready for the data pipeline
get_ibm <- function(reqs) {
  stime <- Sys.time()

  resps <- tryCatch(
    {
      token <- get_ibm_token()
      if (!is.character(token)) stop("Failed to get IBM token.")

      message(sprintf("GET ==> %s IBM requests", length(reqs)))

      # perform parallel requests for each time chunk
      resps <- req_perform_parallel(reqs, on_error = "continue", progress = FALSE)

      # gather response data
      lapply(resps, function(resp) {
        tryCatch(
          {
            if ("httr2_failure" %in% class(resp)) {
              echo(resp)
              stop("Request failed")
            }
            if (resp_status(resp) != 200) stop(paste0("Received status ", resp_status(resp), " with message ", resp_status_desc(resp)))
            resp_body_json(resp, simplifyVector = TRUE) %>% as_tibble()
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
  msg <- if (nrow(wx) > 0) {
    sprintf("OK ==> Performed %s requests in %s sec", length(reqs), round(Sys.time() - stime, 3))
  } else {
    "FAIL ==> Requests did not succeed"
  }
  message(msg)
  wx
}

# get_ibm(test_ibm_reqs)


#' Does some minimal processing on the IBM response to set local time and date
#' @param ibm_response hourly weather data received from API
#' @returns tibble
clean_ibm <- function(ibm_response) {
  if (nrow(ibm_response) == 0) {
    return(tibble())
  }
  ibm_response %>%
    select(-OPTS$ibm_ignore_cols) %>%
    select(
      grid_id = gridpointId,
      grid_lat = latitude,
      grid_lng = longitude,
      datetime_utc = validTimeUtc,
      everything()
    ) %>%
    clean_names() %>%
    mutate(across(datetime_utc, ~ parse_date_time(.x, "YmdHMSz"))) %>%
    mutate(time_zone = lutz::tz_lookup_coords(grid_lat, grid_lng, warn = FALSE), .after = datetime_utc) %>%
    mutate(datetime_local = with_tz(datetime_utc, first(time_zone)), .by = time_zone, .after = time_zone) %>%
    mutate(date = as_date(datetime_local), .after = datetime_local)
}

# test_ibm <- test_ibm_raw %>% clean_ibm()
# ggplot(test_ibm, aes(x = datetime_local, y = temperature)) + geom_line()


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


#' Creates the working hourly weather dataset from cleaned ibm response
#' @param ibm_hourly hourly weather data from `clean_ibm` function
#' @returns tibble
build_hourly <- function(ibm_hourly) {
  ibm_hourly %>%
    select(
      grid_id, grid_lat, grid_lng,
      datetime_utc, time_zone, datetime_local, date,
      all_of(ibm_vars)
    ) %>%
    add_date_cols() %>%
    arrange(grid_lat, grid_lng, datetime_local) %>%
    mutate(
      dew_point_depression = abs(temperature - dew_point),
      .after = dew_point
    )
}

# saved_weather %>% build_hourly()
