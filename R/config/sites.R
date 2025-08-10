# Site management ----

sites_template <- tibble(
  id = integer(),
  name = character(),
  lat = numeric(),
  lng = numeric()
)

Site <- function(name, lat, lng, id = 999) {
  site <- as.list(environment())
}

# Site("foo", 1, 2)


#' parse lat/lng coordinates from string
#' @param str input string containing coordinates to parse in form "lat, lng"
#' @returns named list { lat: numeric, lng: numeric }
parse_coords <- function(str) {
  str <- gsub("[ ,\t°NW]", " ", str)
  parts <- str_split_1(str_squish(str), " ")
  if (length(parts) != 2) stop("Invalid coordinate format.")
  coords <- suppressWarnings(list(
    lat = as.numeric(parts[1]),
    lng = as.numeric(parts[2])
  ))
  if (any(sapply(coords, is.na))) stop("Failed to parse coordinates.")
  coords
}


#' Make sure names read from csv are valid and safe to display
#' adds a counter after any duplicate names
#' @param str character vector of names
sanitize_loc_names <- function(str) {
  str <- trimws(gsub("<[^>]+>", "", str))
  str <- str_trunc(str, 30)
  Encoding(str) <- "UTF-8"
  tibble(name = str) %>%
    mutate(count = row_number(), .by = name) %>%
    mutate(name = if_else(count > 1, paste0(name, " (", count, ")"), name)) %>%
    pull(name)
}

# # should include "foo (2)"
# sanitize_loc_names(c("foo", "foo", "bar"))
#
# # should strip html
# sanitize_loc_names(c("foo", "bar", "<a href='bad'>baz</a>"))


#' Try read sites list from csv
#' @param fpath location of csv to read
load_sites <- function(fpath) {
  df <- read_csv(fpath, col_types = "c", show_col_types = F)
  if (nrow(df) == 0) stop("File was empty")
  df <- df %>%
    clean_names() %>%
    select(any_of(OPTS$site_cols)) %>%
    drop_na()
  if (!(all(c("name", "lat", "lng") %in% names(df)))) stop("File did not contain [name] [lat] [lng] columns.")
  df <- df %>%
    mutate(name = sanitize_loc_names(name)) %>%
    distinct(name, lat, lng) %>%
    filter(validate_ll(lat, lng))
  if (nrow(df) == 0) stop("No valid locations within service area.")
  df %>%
    mutate(id = row_number(), .before = 1) %>%
    head(OPTS$max_sites)
}

# # should succeed
# load_sites("data/example-sites.csv")
# load_sites("data/wisconet stns.csv")
