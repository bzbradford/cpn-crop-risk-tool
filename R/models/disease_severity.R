# Disease severity ----

# Functions for converting model outputs into risk scores (eg low/med/high)

assign_risk <- function(model, value) {
  switch(model,
    "tar_spot_prob"          = risk_from_prob(value, 1, 20, 35),
    "gray_leaf_spot_prob"    = risk_from_prob(value, 1, 40, 60),
    "don_prob"               = risk_from_prob(value, 20, 40, 80),
    "white_mold_dry_prob"    = risk_from_prob(value, .01, 20, 35),
    "white_mold_irrig_prob"  = risk_from_prob(value, .01, 5, 10),
    "frogeye_leaf_spot_prob" = risk_from_prob(value, 1, 40, 50),
    "early_blight_pdays"     = risk_for_early_blight(value),
    "late_blight_dsv"        = risk_for_late_blight(value),
    "alternaria_dsv"         = risk_for_alternaria(value),
    "cercospora_div"         = risk_for_cercospora(value),
    "botrytis_dsi"           = risk_for_botrytis(value),
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

# tibble(value = c(10:1, 2:10), temp = c(1:10, 9:1) * 3) %>%
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
    value_label = sprintf("%.1f P-days, 7-day avg: %.1f, Total: %.0f (%s risk)", value, avg7, total, risk)
  )
}

# tibble(
#   value = runif(100, 5, 10),
#   risk_for_earlyblight(value),
# ) %>%
#   mutate(day = row_number()) %>%
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
    value_label = sprintf("%s DSV, 14-day: %s, Total: %s (%s risk)", value, total14, total, risk)
  )
}

# tibble(
#   value = runif(100, 0, 3) ,
#   risk_for_lateblight(value)
# ) %>%
#   mutate(day = row_number()) %>%
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
    severity =
      (total7 >= 5) +
      (total7 >= 10) +
      (total7 >= 15) +
      (total7 >= 20),
    risk_from_severity(severity),
    value_label = sprintf("%s DSV, 7-day: %s, Total: %s (%s risk)", value, total7, total, risk)
  )
}

# tibble(
#   value = runif(100, 0, 5),
#   risk_for_alternaria(value)
# ) %>%
#   mutate(day = row_number()) %>%
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
    value_label = sprintf("%s DIV, 2-day avg: %.1f, 7-day avg: %.1f, Total: %s (%s risk)", value, avg2, avg7, total, risk),
  )
}

# tibble(
#   value = runif(100, 0, 4),
#   risk_for_cercospora(value)
# ) %>%
#   mutate(day = row_number()) %>%
#   ggplot(aes(x = day, color = risk, group = 1)) +
#   geom_line(aes(y = avg2)) +
#   geom_line(aes(y = avg7)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)



# Botrytis (botcast) - onion
risk_for_botrytis <- function(value) {
  tibble(
    total = cumsum(value),
    severity =
      (total > 10) +
      (total > 20) +
      (total > 30) +
      (total > 40),
    risk_from_severity(severity),
    value_label = sprintf("%.0f daily, %.0f total DSI (%s risk)", value, total, risk)
  )
}

# tibble(
#   value = round(runif(100, 0, 4), 0),
#   risk_for_botrytis(value)
# ) %>%
#   mutate(day = row_number()) %>%
#   ggplot(aes(x = day, color = risk, group = 1)) +
#   geom_col(aes(y = value)) +
#   geom_line(aes(y = total)) +
#   scale_color_brewer(palette = "Spectral", direction = -1)
