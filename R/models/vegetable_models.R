# Vegetable Disease Models ----

#' Potato physiological days
#' Potato/tomato
#' Pscheidt and Stevenson 1988 https://link.springer.com/article/10.1007/BF02854357
#' More information: https://vegpath.plantpath.wisc.edu/diseases/potato-early-blight/
#' @param tmin Minimum daily temperature, Celsius
#' @param tmax Maximum daily temperature, Celsius
#' @returns numeric daily potato physiological days, approx 0-10 per day
calc_pdays <- function(tmin, tmax) {
  a <- 5 * pday(tmin)
  b <- 8 * pday((2 * tmin / 3) + (tmax / 3))
  c <- 8 * pday((2 * tmax / 3) + (tmin / 3))
  d <- 3 * pday(tmin)
  (a + b + c + d) / 24.0
}


#' P-day function for an individual temperature
#' @param temp temperature in Celsius
#' @returns numeric p-day value
pday <- function(temp) {
  case_when(
    temp < 7 ~ 0,
    between(temp, 7, 21) ~ 10 * (1 - ((temp - 21)^2 / 196)), # 196 = (21-7)^2
    between(temp, 21, 30) ~ 10 * (1 - ((temp - 21)^2 / 81)), # 81 = (30-21)^2
    TRUE ~ 0
  )
}

# expand_grid(tmin = 0:35, tmax = 0:35) %>%
#   mutate(pdays = calculate_pdays(tmin, tmax)) %>%
#   ggplot(aes(x = tmin, y = tmax, fill = pdays)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Late blight disease severity values
#' Potato/tomato
#' based on the Wallins BLITECAST model
#' - https://www.google.com/books/edition/The_Plant_Disease_Reporter/ow9BD6P2KZ4C?hl=en&gbpv=1&pg=PA95&printsec=frontcover
#' - https://ipm.ucanr.edu/DISEASE/DATABASE/potatolateblight.html
#' More information: https://vegpath.plantpath.wisc.edu/diseases/potato-late-blight/
#' @param t Mean temperature during hours where RH > 90%, Celsius
#' @param h Number of hours where RH > 90%
#' @returns numeric 0-4 disease severity values
calc_late_blight_dsv <- function(t, h) {
  case_when(
    is.na(t) | is.na(h) ~ 0,
    t < 7.2 ~ 0,
    t <= 11.6 ~ (h > 21) + (h > 18) + (h > 15),
    t <= 15.0 ~ (h > 21) + (h > 18) + (h > 15) + (h > 12),
    t <= 26.6 ~ (h > 18) + (h > 15) + (h > 12) + (h > 9),
    TRUE ~ 0
  )
}

# expand_grid(temp = 0:30, hours = 0:24) %>%
#   mutate(dsv = late_blight_dsv(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dsv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Alternaria leaf blight disease severity values
#' Carrot
#' based on the Pitblado 1992 TOMCAST model https://atrium.lib.uoguelph.ca/server/api/core/bitstreams/5c7ec712-43ef-4b73-b8b8-e85c29d3d58b/content
#' which is based on the Madden et al 1978 FAST model https://www.apsnet.org/publications/phytopathology/backissues/Documents/1978Articles/Phyto68n09_1354.PDF
#'
#' @param temp Mean temperature during hours where RH > 90%, Celsius
#' @param h Number of hours where RH > 90%
#' @returns numeric 0-4 dsv
calc_alternaria_dsv <- function(temp, h) {
  case_when(
    is.na(temp) | is.na(h) ~ 0,
    temp < 13 ~ 0,
    temp <= 18 ~ (h > 20) + (h > 15) + (h > 7),
    temp <= 21 ~ (h > 22) + (h > 15) + (h > 8) + (h > 4),
    temp <= 26 ~ (h > 20) + (h > 12) + (h > 5) + (h > 2),
    temp > 26 ~ (h > 22) + (h > 15) + (h > 8) + (h > 3)
  )
}

# expand_grid(temp = 0:30, hours = 0:24) %>%
#   mutate(dsv = carrot_foliar_dsv(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dsv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Cercospora leaf blight daily infection values
#' Beet
#' based on https://apsjournals.apsnet.org/doi/abs/10.1094/PDIS.1998.82.7.716
#' more information: https://vegpath.plantpath.wisc.edu/diseases/carrot-alternaria-and-cercospora-leaf-blights/
#' @param t Mean temperature during hours where RH > 90%, Celsius, converted to Fahrenheit internally
#' @param h Number of hours where RH > 90%
#' @returns 0-7 div
calc_cercospora_div <- function(t, h) {
  t <- c_to_f(t)
  case_when(
    is.na(t) | is.na(h) ~ 0,
    t <= 60 ~ 0,
    t <= 61 ~ (h > 21),
    t <= 62 ~ (h > 19) + (h > 22),
    t <= 63 ~ (h > 16) + (h > 19) + (h > 21),
    t <= 64 ~ (h > 13) + (h > 15) + (h > 18) + (h > 20) + (h > 23),
    t <= 65 ~ (h > 6) + (h > 8) + (h > 12) + (h > 18) + (h > 21),
    t <= 71 ~ (h > 3) + (h > 6) + (h > 10) + (h > 14) + (h > 18) + (h > 21),
    t <= 72 ~ (h > 2) + (h > 6) + (h > 9) + (h > 13) + (h > 17) + (h > 20),
    t <= 73 ~ (h > 1) + (h > 6) + (h > 9) + (h > 12) + (h > 16) + (h > 19),
    t <= 76 ~ 1 + (h > 5) + (h > 9) + (h > 11) + (h > 16) + (h > 18) + (h > 23),
    t <= 77 ~ 1 + (h > 5) + (h > 8) + (h > 12) + (h > 15) + (h > 18) + (h > 22),
    t <= 78 ~ 1 + (h > 5) + (h > 8) + (h > 11) + (h > 14) + (h > 17) + (h > 20),
    t <= 79 ~ 1 + (h > 4) + (h > 7) + (h > 9) + (h > 12) + (h > 14) + (h > 17),
    t <= 80 ~ 1 + (h > 3) + (h > 6) + (h > 8) + (h > 10) + (h > 12) + (h > 15),
    t <= 81 ~ 1 + (h > 2) + (h > 4) + (h > 6) + (h > 7) + (h > 9) + (h > 11),
    t <= 82 ~ 1 + (h > 2) + (h > 4) + (h > 5) + (h > 7) + (h > 8) + (h > 10),
    t > 82 ~ 1 + (h > 2) + (h > 4) + (h > 5) + (h > 7) + (h > 8) + (h > 9)
  )
}

# expand_grid(temp = 15:30, hours = 0:24) %>%
#   mutate(dsv = cercospora_div(temp, hours), temp = c_to_f(temp)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dsv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Onion botrytis leaf blight
#' based on Sutton et al 1986 https://doi.org/10.1016/0167-8809(86)90136-2
#' more information: https://vegpath.plantpath.wisc.edu/diseases/onion-botrytis/
#' requires 5 days of hourly weather to compute

#' Botcast daily innoculum value
#' this determines if innoculum is predicted to be present
#' A) if temp > 30 for >= 4 hrs on any of past 5 days => 0
#' B) if h < 5 => 0
#' C) if h > 12 => 1
#' D) if prev day dry (<70% rh for >= 6 hrs) and no rain => 0 else 1
#' @param hot boolean: temp > 30 for >= 4 hrs on any of past 5 days
#' @param h int: hours rh > 90 during past day
#' @param dry boolean: previous day dry
botcast_dinov <- function(hot, hours_rh90, dry) {
  case_when(
    hot ~ 0,
    hours_rh90 < 5 ~ 0,
    hours_rh90 > 12 ~ 1,
    dry ~ 0,
    TRUE ~ 1
  )
}

# expand_grid(hot = c(F, T), lw = 0:24, dry = c(F, T)) %>%
#   mutate(dinov = botcast_dinov(hot, lw, dry)) %>% summary()


#' Botcast daily infection values
#' @param t temperature of wet period
#' @param h duration of leaf wetness
botcast_dinfv <- function(t, h) {
  case_when(
    h <= 6 ~ 0,
    t < 6 | t > 28 ~ 0,
    h <= 12 & t < 9 ~ 0,
    t <= 8 ~ case_when(
      h <= 12 ~ 0,
      h <= 22 ~ 1,
      TRUE ~ 2
    ),
    h <= 15 & t >= 27 ~ 0,
    h <= 7 & t >= 24 ~ 0,
    h >= 15 & between(t, 8, 25) ~ 2,
    h >= 14 & between(t, 11, 16.5) ~ 2,
    h >= 11 & between(t, 13.5, 16.5) ~ 2,
    h - 12 <= 9 - t ~ 0,
    TRUE ~ 1
  )
}

# expand_grid(temp = (60:280) / 10, hours = 0:24) %>%
#   mutate(dinfv = botcast_dinfv(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dinfv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_equal(expand = F)


calc_botrytis_dsi <- function(hot, dry, hours_rh90, mean_temp_rh90) {
  dinov <- botcast_dinov(hot, hours_rh90, lag(dry, default = FALSE))
  dinfv <- botcast_dinfv(mean_temp_rh90, hours_rh90)
  dinov * dinfv
}

# expand_grid(
#   hot = c(T, F),
#   dry = c(T, F),
#   hours_rh90 = 0:24,
#   mean_temp_rh90 = 0:35
# ) %>%
#   mutate(value = calc_botrytis_dsi(hot, dry, hours_rh90, mean_temp_rh90)) %>%
#   count(value)
