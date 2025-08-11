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

# expand_grid(temp = 10:40, rh = seq(0, 100, 5), hours = 0:24) %>%
#   mutate(prob = predict_tarspot(temp, rh, hours)) %>%
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

# expand_grid(temp = 0:30, dp = 0:30) %>%
#   mutate(prob = predict_gls(temp, dp)) %>%
#   ggplot(aes(x = temp, y = dp)) +
#   geom_tile(aes(fill = prob)) +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)


#' Giberella ear rot / DON model
#' Corn silking
#' Risk criteria: High >= 80%, Med >= 40%, Low >= 20%
#' @param
#'
predict_don <- function(
    w7_max_temp_14ma,
    w7_min_temp_14ma,
    w7_days_temp_over_25_14ma,
    w7_days_precip_14ma,
    w0_mean_rh_14ma,
    w0_days_rh_over_80_14ma) {
  mu <-
    -59.6309 +
    1.3057 * w7_max_temp_14ma +
    0.9090 * w7_min_temp_14ma +
    -1.6158 * w7_days_temp_over_25_14ma +
    -0.9350 * w7_days_precip_14ma +
    0.2255 * w0_mean_rh_14ma +
    -0.9249 * w0_days_rh_over_80_14ma
  logistic(mu)
}

# test <- expand_grid(
#   w7_max_temp_14ma = 10:30,
#   w7_min_temp_14ma = 0:30,
#   w7_days_temp_over_25_14ma = 0:14,
#   w7_days_precip_14ma = 0:14,
#   w0_mean_rh_14ma = 10 * (0:10),
#   w0_days_rh_over_80_14ma = 0:14
# ) %>%
#   filter(w7_max_temp_14ma > w7_min_temp_14ma) %>%
#   mutate(don = predict_don(
#     w7_max_temp_14ma,
#     w7_min_temp_14ma,
#     w7_days_temp_over_25_14ma,
#     w7_days_precip_14ma,
#     w0_mean_rh_14ma,
#     w0_days_rh_over_80_14ma
#   ))
#
# hist(test$don)
#
# test %>%
#   filter(don > .01) %>%
#   pivot_longer(1:6) %>%
#   select(name, value, don) %>%
#   mutate(value = factor(value)) %>%
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

# expand_grid(temp = 0:40, wind = 0:20, rh = (0:10) * 10) %>%
#   mutate(prob = predict_white_mold_dry(temp, wind, rh)) %>%
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
predict_white_mold_irrig <- function(MaxAT_30MA, MaxRH_30ma, spacing = c("15", "30")) {
  spacing <- match.arg(spacing)
  mu <- -2.38 * (spacing == "30") + 0.65 * MaxAT_30MA + 0.38 * MaxRH_30ma - 52.65
  logistic(mu)
}

# expand_grid(temp = 15:40, rh = seq(50, 100, 5), spacing = c("15", "30")) %>%
#   rowwise() %>%
#   mutate(prob = predict_white_mold_irrig(temp, rh, spacing)) %>%
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

# expand_grid(temp = 0:40, hours = 0:24) %>%
#   mutate(prob = predict_fls(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = prob)) +
#   geom_tile() +
#   scale_fill_distiller(palette = "Spectral") +
#   coord_cartesian(expand = F)
