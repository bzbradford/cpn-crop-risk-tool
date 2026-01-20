# Disease severity ----

# Functions for converting model outputs into risk scores (eg low/med/high)

assign_risk <- function(model, value) {
  switch(
    model,
    "tar_spot_prob" = risk_from_prob(value, 1, 20, 35),
    "gray_leaf_spot_prob" = risk_from_prob(value, 1, 40, 60),
    "don_prob" = risk_from_prob(value, 20, 40, 80),
    "white_mold_dry_prob" = risk_from_prob(value, .01, 20, 35),
    "white_mold_irrig_prob" = risk_from_prob(value, .01, 5, 10),
    "frogeye_leaf_spot_prob" = risk_from_prob(value, 1, 40, 50),
    "early_blight_pdays" = risk_for_early_blight(value),
    "late_blight_dsv" = risk_for_late_blight(value),
    "alternaria_dsv" = risk_for_alternaria(value),
    "cercospora_div" = risk_for_cercospora(value),
    "botrytis_dsi" = risk_for_botrytis(value),
    stop("'", model, "' is not a valid model type")
  )
}

# assign_risk("foo", 1)

## Field crops ----

#' Assign risk for field crops spore probability models
#' @param prob numeric vector of probabilities between 0 and 1
#' @param low threshold for low risk, 0-100
#' @param med threshold for medium risk, 0-100
#' @param high threhold for high risk, 0-100
risk_from_prob <- function(prob, low, med, high) {
  tibble(
    risk = cut(
      prob * 100,
      breaks = c(0, low, med, high, 100),
      labels = c("Very low risk", "Low risk", "Moderate risk", "High risk"),
      include.lowest = TRUE,
      right = FALSE
    ),
    # severity = as.numeric(risk) - 1,
    risk_color = colorFactor("Spectral", risk, reverse = TRUE)(risk),
    value_label = sprintf("%.0f%% (%s)", prob * 100, risk)
  )
}

# tibble(
#   value = runif(20),
#   risk_from_prob(value, 5, 25, 60)
# )

# reduces spore probability as temperature falls below 10C
attenuate_prob <- function(value, temp) {
  case_when(
    temp > 10 ~ value,
    temp > 0 ~ value * temp / 10,
    TRUE ~ 0
  )
}

# tibble(value = c(10:1, 2:10), temp = c(1:10, 9:1) * 3) |>
#   mutate(new_value = attenuate_prob(value, temp))

## Vegetables ----

#' Convert severity score 0-4 to a risk word and assign a color
#' @param severity numeric vector of severities
#' @returns tibble
risk_from_severity <- function(severity) {
  # pal <- c("#00cc00", "#7dff23", "#ffd700", "#ff8000", "#cc0000")
  pal <- c("#0082b7", "#00cc00", "#ffd700", "#ff8000", "#cc0000")
  tibble(
    risk = cut(
      severity,
      breaks = 0:5,
      labels = c("Very low", "Low", "Moderate", "High", "Very high"),
      include.lowest = TRUE,
      right = FALSE
    ),
    risk_color = colorFactor(pal, risk)(risk)
  )
}

# tibble(
#   value = round(runif(10, 0, 4)),
#   risk_from_severity(value)
# )

#' Assign risk score for early blight p-day accumulation
#' @param value dsv from `calc_pdays` function
risk_for_early_blight <- function(value) {
  tibble(
    total = cumsum(value),
    avg7 = rollapplyr(value, 7, mean, partial = TRUE),
    severity = case_when(
      total >= 400 ~
        (avg7 >= 1) +
        (avg7 >= 3) +
        (avg7 >= 5) +
        (avg7 >= 9),
      TRUE ~
        (total >= 200) +
        (total >= 250) +
        (total >= 300) +
        (total >= 350)
    ),
    risk_from_severity(severity),
    value_label = sprintf(
      "%.1f P-days, 7-day avg: %.1f, Total: %.0f (%s risk)",
      value,
      avg7,
      total,
      risk
    )
  )
}

# tibble(
#   value = runif(100, 5, 10),
#   risk_for_earlyblight(value),
# ) |>
#   mutate(day = row_number()) |>
#   ggplot(aes(x = day, color = risk, group = 1)) +
#   geom_line(aes(y = total)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)

#' Assign risk score for late blight dsv accumulation
#' @param value dsv from `calc_late_blight_dsv` function
risk_for_late_blight <- function(value) {
  tibble(
    total14 = rollapplyr(value, 14, sum, partial = TRUE),
    total = cumsum(value),
    severity = case_when(
      total14 >= 21 & total >= 30 ~ 4,
      total14 >= 14 & total >= 30 ~ 3,
      total14 >= 3 | total >= 30 ~ 2,
      total14 >= 1 ~ 1,
      TRUE ~ 0
    ),
    risk_from_severity(severity),
    value_label = sprintf(
      "%s DSV, 14-day: %s, Total: %s (%s risk)",
      value,
      total14,
      total,
      risk
    )
  )
}

# tibble(
#   value = runif(100, 0, 3) ,
#   risk_for_lateblight(value)
# ) |>
#   mutate(day = row_number()) |>
#   ggplot(aes(x = day, group = 1, color = risk)) +
#   geom_line(aes(y = total14)) +
#   geom_line(aes(y = total)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)

#' Assign risk score for alternaria leaf blight
#' @param value dsv from `calc_alternaria_dsv` function
risk_for_alternaria <- function(value) {
  tibble(
    total7 = rollapplyr(value, 7, sum, partial = TRUE),
    total = cumsum(value),
    severity = (total7 >= 5) +
      (total7 >= 10) +
      (total7 >= 15) +
      (total7 >= 20),
    risk_from_severity(severity),
    value_label = sprintf(
      "%s DSV, 7-day: %s, Total: %s (%s risk)",
      value,
      total7,
      total,
      risk
    )
  )
}

# tibble(
#   value = runif(100, 0, 5),
#   risk_for_alternaria(value)
# ) |>
#   mutate(day = row_number()) |>
#   ggplot(aes(x = day, color = risk, group = 1)) +
#   geom_line(aes(y = total7)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)

#' Cercospora leaf blight
#' Windels, C. E., Lamey, H. A., Hilde, D., Widner, J., & Knudsen, T. (1998). A Cerospora leaf spot model for sugar beet: in practice by an industry. Plant disease, 82(7), 716-726.
#' https://apsjournals.apsnet.org/doi/abs/10.1094/PDIS.1998.82.7.716
#' @param value dsv from `calc_cercospora_div` function
risk_for_cercospora <- function(value) {
  tibble(
    avg2 = rollapplyr(value, 2, mean, partial = TRUE),
    avg7 = rollapplyr(value, 7, mean, partial = TRUE),
    total = cumsum(value),
    severity = case_when(
      avg7 >= 5 | avg2 >= 5.5 ~ 4,
      avg7 >= 3 | avg2 >= 3.5 ~ 3,
      avg7 >= 1.5 | avg2 >= 2 ~ 2,
      avg7 >= .5 | avg2 >= 1 ~ 1,
      TRUE ~ 0
    ),
    risk_from_severity(severity),
    value_label = sprintf(
      "%s DIV, 2-day avg: %.1f, 7-day avg: %.1f, Total: %s (%s risk)",
      value,
      avg2,
      avg7,
      total,
      risk
    ),
  )
}

# tibble(
#   value = runif(100, 0, 4),
#   risk_for_cercospora(value)
# ) |>
#   mutate(day = row_number()) |>
#   ggplot(aes(x = day, color = risk, group = 1)) +
#   geom_line(aes(y = avg2)) +
#   geom_line(aes(y = avg7)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)

# Botrytis (botcast) - onion
risk_for_botrytis <- function(value) {
  tibble(
    total = cumsum(value),
    severity = (total > 10) +
      (total > 20) +
      (total > 30) +
      (total > 40),
    risk_from_severity(severity),
    value_label = sprintf(
      "%.0f daily, %.0f total DSI (%s risk)",
      value,
      total,
      risk
    )
  )
}

# tibble(
#   value = round(runif(100, 0, 4), 0),
#   risk_for_botrytis(value)
# ) |>
#   mutate(day = row_number()) |>
#   ggplot(aes(x = day, color = risk, group = 1)) +
#   geom_col(aes(y = value)) +
#   geom_line(aes(y = total)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)

# Field Crops Disease Models ----

# Logistic function to convert logit to probability
logistic <- function(logit) exp(logit) / (1 + exp(logit))


#' Tar spot model
#' Corn growth stage V10-R3
#' Risk criteria: High >=35%, Medium >=20%, Low >0%
#' No risk: Fungicide in last 14 days, temperature <32F
#' @param MeanAT_30ma Mean daily temperature, 30-day moving average, Celsius
#' @param MaxRH_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param HrsRH90Night_14ma Nighttime hours RH > 90%, 14-day moving average, 0-24 hours
#' @returns probability of spore presence
predict_tarspot <- function(MeanAT_30ma, MaxRH_30ma, HrsRH90Night_14ma) {
  mu1 <- 32.06987 - 0.89471 * MeanAT_30ma - 0.14373 * MaxRH_30ma
  mu2 <- 20.35950 - 0.91093 * MeanAT_30ma - 0.29240 * HrsRH90Night_14ma
  (logistic(mu1) + logistic(mu2)) / 2
}

# expand_grid(temp = 10:40, rh = seq(0, 100, 5), hours = 0:24) |>
#   mutate(prob = predict_tarspot(temp, rh, hours)) |>
#   ggplot(aes(x = temp, y = rh, fill = prob)) +
#   geom_tile() +
#   facet_wrap(~hours) +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)

#' Gray leaf spot model
#' Corn growth stage V10-R3
#' Risk criteria: High >=60%, Medium >=40%, Low >0%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @returns probability of spore presence
#' @param MinAT_21ma Minimum daily temperature, 21-day moving average, Celsius
#' @param MinDP_30ma Minimum dew point temperature, 30-day moving average, Celsius
#' @returns probability of spore presence
predict_gls <- function(MinAT_21ma, MinDP_30ma) {
  mu <- -2.9467 - 0.03729 * MinAT_21ma + 0.6534 * MinDP_30ma
  logistic(mu)
}

# expand_grid(temp = 0:30, dp = 0:30) |>
#   mutate(prob = predict_gls(temp, dp)) |>
#   ggplot(aes(x = temp, y = dp)) +
#   geom_tile(aes(fill = prob)) +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)

#' Gibberella ear rot / DON model
#' Risk applies during corn silking
#' Risk criteria: High >= 80%, Med >= 40%, Low >= 20%
#' "w7" weather window is 21-7 days before model date
#' "w0" window is 14-0 days before model date
#' @param w7_max_temp mean maximum temperature, w7 window
#' @param w7_min_temp mean minimum temperature, w7 window
#' @param w7_days_temp_over_25 n days temperature over 25C, w7 window
#' @param w7_days_precip n days with precip, w7 window
#' @param w0_mean_rh mean RH, w0 window
#' @param w0_days_rh_over_80 n days RH > 80%, w0 window
predict_don <- function(
  w7_max_temp,
  w7_min_temp,
  w7_days_temp_over_25,
  w7_days_precip,
  w0_mean_rh,
  w0_days_rh_over_80
) {
  mu <-
    -59.6309 +
    1.3057 * w7_max_temp +
    0.9090 * w7_min_temp +
    -1.6158 * w7_days_temp_over_25 +
    -0.9350 * w7_days_precip +
    0.2255 * w0_mean_rh +
    -0.9249 * w0_days_rh_over_80
  logistic(mu)
}

# test <- expand_grid(
#   w7_max_temp = 10:30,
#   w7_min_temp = 0:30,
#   w7_days_temp_over_25 = 0:14,
#   w7_days_precip = 0:14,
#   w0_mean_rh = 10 * (0:10),
#   w0_days_rh_over_80 = 0:14
# ) |>
#   filter(w7_max_temp > w7_min_temp) |>
#   mutate(don = predict_don(
#     w7_max_temp,
#     w7_min_temp,
#     w7_days_temp_over_25,
#     w7_days_precip,
#     w0_mean_rh,
#     w0_days_rh_over_80
#   ))
#
# hist(test$don)
#
# test |>
#   filter(don > .01) |>
#   pivot_longer(1:6) |>
#   select(name, value, don) |>
#   mutate(value = factor(value)) |>
#   ggplot(aes(x = value, y = don)) +
#   geom_boxplot(aes(fill = after_stat(middle)), outlier.size = .5) +
#   scale_fill_viridis_c() +
#   facet_wrap(~name, scales = "free")

#' White mold, dryland model
#' Soybean growth stage R1-R3
#' Risk criteria: High >=40%, Med >=20%, Low >=5%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @param MaxAT_30ma 30-day moving average of daily maximum temperature, Celsius
#' @param MaxWS_30ma 30-day moving average of daily maximum wind speed, m/s
#' @param MaxRH_30ma 30-day moving average of daily maximum relative humidity, 0-100%
#' @returns probability of spore presence
predict_white_mold_dry <- function(MaxAT_30ma, MaxWS_30ma, MaxRH_30ma) {
  m1 <- -.47 * MaxAT_30ma - 1.01 * MaxWS_30ma + 16.65
  m2 <- -.68 * MaxAT_30ma + 17.19
  m3 <- -.86 * MaxAT_30ma + 0.1 * MaxRH_30ma - 0.75 * MaxWS_30ma + 8.2
  (logistic(m1) + logistic(m2) + logistic(m3)) / 3
}

# expand_grid(temp = 0:40, wind = 0:20, rh = (0:10) * 10) |>
#   mutate(prob = predict_white_mold_dry(temp, wind, rh)) |>
#   ggplot(aes(x = temp, y = wind, fill = prob)) +
#   geom_tile() +
#   scale_fill_distiller(palette = "Spectral", limits = c(0, 1)) +
#   coord_cartesian(expand = F) +
#   facet_wrap(~rh, labeller = "label_both")

#' White mold, irrigated model
#' Soybean growth stage R1-R3
#' Risk criteria: High >=40%, Med >=20%, Low >=5%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @param MaxAT_30MA Maximum daily temperature, 30-day moving average, Celsius
#' @param MaxRH_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param spacing Row spacing, either "15" or "30", inches
#' @returns probability of spore presence
predict_white_mold_irrig <- function(
  MaxAT_30MA,
  MaxRH_30ma,
  spacing = c("15", "30")
) {
  spacing <- match.arg(spacing)
  mu <- -2.38 *
    (spacing == "30") +
    0.65 * MaxAT_30MA +
    0.38 * MaxRH_30ma -
    52.65
  logistic(mu)
}

# expand_grid(temp = 15:40, rh = seq(50, 100, 5), spacing = c("15", "30")) |>
#   rowwise() |>
#   mutate(prob = predict_white_mold_irrig(temp, rh, spacing)) |>
#   ggplot(aes(x = temp, y = rh, fill = prob)) +
#   geom_tile() +
#   facet_wrap(~spacing, ncol = 1) +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)

#' Frogeye leaf spot model
#' Soybean growth stage R1-R5
#' Risk criteria: High >=50%, Medium >=40%, Low >0%
#' No risk: Fungicide in last 14 days, temperature <32F
#' @param MaxAT_30ma Maximum daily temperature, 30-day moving average, Celsius
#' @param HrsRH80_30ma Daily hours RH > 80%, 30-day moving average, 0-24 hours
#' @returns probability of spore presence
predict_fls <- function(MaxAT_30ma, HrsRH80_30ma) {
  mu <- -5.92485 + 0.12208 * MaxAT_30ma + 0.17326 * HrsRH80_30ma
  logistic(mu)
}

# expand_grid(temp = 0:40, hours = 0:24) |>
#   mutate(prob = predict_fls(temp, hours)) |>
#   ggplot(aes(x = temp, y = hours, fill = prob)) +
#   geom_tile() +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)

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

# expand_grid(tmin = 0:35, tmax = 0:35) |>
#   mutate(pdays = calculate_pdays(tmin, tmax)) |>
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

# expand_grid(temp = 0:30, hours = 0:24) |>
#   mutate(dsv = late_blight_dsv(temp, hours)) |>
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

# expand_grid(temp = 0:30, hours = 0:24) |>
#   mutate(dsv = carrot_foliar_dsv(temp, hours)) |>
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

# expand_grid(temp = 15:30, hours = 0:24) |>
#   mutate(dsv = cercospora_div(temp, hours), temp = c_to_f(temp)) |>
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

# expand_grid(hot = c(F, T), lw = 0:24, dry = c(F, T)) |>
#   mutate(dinov = botcast_dinov(hot, lw, dry)) |> summary()

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

# expand_grid(temp = (60:280) / 10, hours = 0:24) |>
#   mutate(dinfv = botcast_dinfv(temp, hours)) |>
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
# ) |>
#   mutate(value = calc_botrytis_dsi(hot, dry, hours_rh90, mean_temp_rh90)) |>
#   count(value)
