# Utility functions ----

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)), " <", paste(class(x), collapse = ", "), ">")
  print(x)
}

runtime <- function(label = "timestamp", ref = NULL) {
  t <- now()
  message(">> ", label, " [", t, "]")
  if (!is.null(ref)) {
    message(difftime(t, ref), " elapsed")
  }
  t
}

# swaps names and values in a list or vector
invert <- function(x) {
  y <- as(names(x), class(x))
  names(y) <- x
  y
}

# create named list from sublist elements
build_choices <- function(obj, name, value) {
  setNames(
    sapply(obj, \(x) x[[value]]),
    sapply(obj, \(x) x[[name]])
  )
}

# return the first truthy argument
first_truthy <- function(...) {
  for (arg in list(...)) {
    if (shiny::isTruthy(arg)) {
      return(arg)
    }
  }
  NULL
}

# restrict a value to between two extremes
clamp <- function(x, min, max) {
  pmax(min, pmin(max, x))
}

# calculate the difference in hours between two timestamps
hours_diff <- function(start, end) {
  as.integer(ceiling(difftime(end, start, units = "hours")))
}

# hours_diff(now(), now())
# hours_diff(now() - hours(6), now())
# hours_diff(now() - days(1), now())


#' @param ... partial dates like 'aug 1' to convert to yday using current year
get_yday <- function(...) {
  args <- list(...)
  sapply(args, function(v) {
    paste(year(Sys.Date()), v) |>
      ymd() |>
      yday()
  })
}

# get_yday("jun 1", "aug 2")

check_date_overlap <- function(dates_actual, dates_partial) {
  dates_actual <- as_date(dates_actual)
  date_seq <- seq.Date(dates_actual[1], dates_actual[2], 1)
  yrs <- unique(year(date_seq))
  sapply(set_names(yrs), function(yr) {
    dt <- ymd(paste(yr, dates_partial))
    test_seq <- seq.Date(dt[1], dt[2])
    any(date_seq %in% test_seq)
  })
}

check_date_overlap(c("2025-4-1", "2025-7-1"), c("May 1", "Aug 1"))
check_date_overlap(c("2025-4-1", "2025-7-1"), c("Jan 1", "Feb 1"))
check_date_overlap(c("2024-10-1", "2025-7-1"), c("Jun 1", "Aug 1"))


## NA-safe summary functions ----

calc_sum <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  sum(x, na.rm = TRUE)
}

calc_min <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  min(x, na.rm = TRUE)
}

calc_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  mean(x, na.rm = TRUE)
}

calc_max <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  max(x, na.rm = TRUE)
}


## Rolling functions ----

roll_mean <- function(vec, width) {
  zoo::rollapplyr(vec, width, \(x) calc_mean(x), partial = TRUE)
}

roll_sum <- function(vec, width) {
  zoo::rollapplyr(vec, width, \(x) calc_sum(x), partial = TRUE)
}


## Sliding window functions ----

# counts number consecutive runs of values above a threshold
# count_runs <- function(vec, threshold, min_run) {
#   runs <- run <- 0
#   for (val in vec) {
#     run <- if (val >= threshold) run + 1 else 0
#     if (run == min_run) runs <- runs + 1
#   }
#   runs
# }


## Cache ----

clean_old_caches <- function(max_age_days = 30) {
  cache_files <- list.files(path = "cache", pattern = ".*\\.fst", full.names = TRUE)
  old_files <- cache_files[file.mtime(cache_files) < Sys.Date() - max_age_days]
  if (length(old_files) > 0) {
    file.remove(old_files)
    message("Cleaned ", length(old_files), " old cache files")
  }
}
