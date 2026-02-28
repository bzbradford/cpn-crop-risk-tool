# models and data processing

# Daily weather ----------------------------------------------------------------
# daily weather data used by most models

#' Generate daily summary data from hourly weather
#' @param hourly accepts the cleaned hourly data from `build_hourly()`
#' @returns tibble
build_daily <- function(hourly) {
  # grid attributes to be retained
  lat_lng <- hourly |>
    distinct(grid_id, grid_lat, grid_lng)

  # summarized by calendar date
  summary_fns <- c("min" = calc_min, "mean" = calc_mean, "max" = calc_max)
  by_date <- hourly |>
    summarize(
      hours = n(),
      across(
        c(temperature, dew_point, dew_point_depression, relative_humidity),
        summary_fns
      ),
      across(c(precip, snow), c("daily" = calc_sum, "max_hourly" = calc_max)),
      across(c(pressure_mean_sea_level, wind_speed), summary_fns),
      wind_gust_max = calc_max(wind_gust),
      across(c(wind_direction), summary_fns),
      hours_temp_over_20 = sum(temperature >= 20),
      hours_temp_over_30 = sum(temperature >= 30),
      hours_rh_under_70 = sum(relative_humidity < 70),
      hours_rh_over_80 = sum(relative_humidity >= 80),
      hours_rh_over_90 = sum(relative_humidity >= 90),
      .by = c(grid_id, date, yday, year, month, day)
    ) |>
    mutate(
      precip_cumulative = cumsum(precip_daily),
      .after = precip_max_hourly,
      .by = grid_id
    ) |>
    mutate(
      snow_cumulative = cumsum(snow_daily),
      .after = snow_max_hourly,
      .by = grid_id
    ) |>
    mutate(
      # for botcast
      hot_past_5_days = rollapplyr(
        hours_temp_over_30,
        width = 5,
        FUN = \(x) any(x >= 4),
        partial = TRUE
      ),
      dry = (hours_rh_under_70 >= 6) & (precip_daily < 1)
    )

  # summarized by "date since night" eg since 8 pm the day before through 7 pm
  by_night <- hourly |>
    mutate(
      rh80 = relative_humidity >= 80,
      rh90 = relative_humidity >= 90
    ) |>
    summarize(
      hours_rh_over_80_night = sum(night & rh80),
      hours_rh_over_90_night = sum(night & rh90),
      temperature_mean_rh_over_80 = if_else(
        sum(rh80) > 0,
        sum(temperature * (rh80)) / sum(rh80),
        NA
      ),
      temperature_mean_rh_over_90 = if_else(
        sum(rh90) > 0,
        sum(temperature * (rh90)) / sum(rh90),
        NA
      ),
      .by = c(grid_id, date_since_night)
    )

  # assemble the data
  by_date |>
    filter((hours >= 12) | (date == today())) |>
    left_join(by_night, join_by(grid_id, date == date_since_night)) |>
    left_join(lat_lng, join_by(grid_id)) |>
    relocate(grid_lat, grid_lng, .after = grid_id) |>
    arrange(grid_id, date)
}

# test_hourly_wx |> build_daily()

# Moving averages --------------------------------------------------------------
#' this is only used for the data explorer/downloader, models generate their own
#' moving averages if required

#' Generate several moving average periods from daily data
#' @param daily accepts daily data from `build_daily()`
#' @param align moving average alignment
#' @returns tibble
build_ma_from_daily <- function(daily, align = c("center", "right")) {
  align <- match.arg(align)

  # retain attribute cols
  attr <- daily |> select(grid_id, any_of(OPTS$date_attr_cols))

  # define moving average functions
  roll_mean <- function(vec, width) {
    rollapply(vec, width, \(x) calc_mean(x), partial = TRUE, align = align)
  }
  fns <- c(
    "7day" = ~ roll_mean(.x, 7),
    "14day" = ~ roll_mean(.x, 14),
    "21day" = ~ roll_mean(.x, 21),
    "30day" = ~ roll_mean(.x, 30)
  )

  # apply moving average functions to each primary data column
  ma <- daily |>
    select(-hours) |>
    mutate(
      across(
        starts_with(c(
          "temperature",
          "dew_point",
          "relative_humidity",
          "wind",
          "pressure",
          "hours"
        )),
        fns
      ),
      .by = grid_id,
      .keep = "none"
    ) |>
    select(-grid_id)

  # bind attributes
  bind_cols(attr, ma)
}

# test_hourly_wx |> build_daily() |> build_ma_from_daily()

# Growing degree days ----------------------------------------------------------

#' Single sine method
#' to create GDDs with an upper threshold, calculate GDDs with the upper threshold
#' as the base temperature and subtract that value from the GDDs for the base temp
#' to implement a horizontal cutoff.
#' @param tmin minimum daily temperature
#' @param tmax maximum daily temperature
#' @param base base/lower temperature threshold
#' @returns single sine growing degree days for one day
gdd_sine <- function(tmin, tmax, base) {
  mapply(
    function(tmin, tmax, base) {
      if (is.na(tmin) || is.na(tmax)) {
        return(NA)
      }

      # swap min and max if in wrong order for some reason
      if (tmin > tmax) {
        t = tmin
        tmin = tmax
        tmax = t
      }

      # min and max < lower
      if (tmax <= base) {
        return(0)
      }

      average = (tmin + tmax) / 2

      # tmin > lower = simple average gdds
      if (tmin >= base) {
        return(average - base)
      }

      # tmin < lower, tmax > lower = sine gdds
      alpha = (tmax - tmin) / 2
      base_radians = asin((base - average) / alpha)
      a = average - base
      b = pi / 2 - base_radians
      c = alpha * cos(base_radians)
      (1 / pi) * (a * b + c)
    },
    tmin,
    tmax,
    base
  )
}

# gdd_sine(10:40, 0:30, 0)
# gdd_sine(5:35, 0:30, 10)

#' Generate various growing degree day models with and without an 86F upper threshold
#' input temperatures must be Celsius and will be converted to Fahrenheit GDDs
#' @param daily accepts daily dataset from `build_daily()`
#' @returns tibble
build_gdd_from_daily <- function(daily) {
  # retain attribute cols
  attr <- daily |> select(grid_id, date)

  # convert temperatures
  tmin <- c_to_f(daily$temperature_min)
  tmax <- c_to_f(daily$temperature_max)

  # start with a base 86F model for chopping off the upper thresholds
  gdd <- tibble(base_86 = gdd_sine(tmin, tmax, 86))

  # generate each of the base temperature models with and without the upper threshold
  for (base in c(32, 39.2, 41, 45, 48, 50, 52, 55)) {
    name <- str_replace_all(paste0("base_", base), "\\.", "p")
    gdd[[name]] <- gdd_sine(tmin, tmax, base)
    gdd[[paste0(name, "_upper_86")]] <- gdd[[name]] - gdd$base_86
  }

  # remove the upper threshold model
  gdd$base_86 <- NULL

  # assemble, add cumulative cols, sort names
  bind_cols(attr, gdd) |>
    mutate(
      across(starts_with("base_"), c(cumulative = cumsum)),
      .by = grid_id
    ) %>%
    select(
      all_of(names(attr)),
      all_of(sort(names(.)))
    )
}

# test_hourly_wx |> build_daily() |> build_gdd_from_daily()

# Model helpers ----------------------------------------------------------------

# Logistic function to convert logit to probability
logistic <- function(logit) exp(logit) / (1 + exp(logit))

#' Assign risk for field crops spore probability models
#' @param prob numeric vector of probabilities between 0 and 1
#' @param high threshold for high risk, 0-1
#' @param med threshold for medium risk, 0-1
#' @param low threshold for low risk, 0-1
#' @returns tibble
risk_from_prob <- function(prob, low, med, high) {
  if (isTRUE(any(prob < 0) | any(prob > 1))) {
    warning("Risk probability out of range")
  }
  tibble(
    risk = cut(
      prob,
      breaks = c(0, low, med, high, 1),
      labels = c("Very low risk", "Low risk", "Moderate risk", "High risk"),
      include.lowest = TRUE,
      right = FALSE
    ),
    # severity = as.numeric(risk) - 1,
    risk_color = colorFactor("Spectral", risk, reverse = TRUE)(risk),
    value_label = sprintf("%.0f%% (%s)", prob * 100, risk)
  )
}
# risk_from_prob(.3, 1, 50, 90)
# risk_from_prob(2, 1, 50, 90)

# reduces spore probability as temperature falls below 10C
attenuate_prob <- function(value, temp) {
  case_when(
    temp > 10 ~ value,
    temp > 0 ~ value * temp / 10,
    TRUE ~ 0
  )
}

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

#' simple plot for viewing results
#' @param df any of the data from build_* functions
test_plot <- function(df) {
  df |>
    pivot_longer(cols = any_of(c("model_value", "severity"))) |>
    ggplot(aes(x = date, y = value)) +
    geom_col(aes(fill = risk_color), lwd = 0, width = 1) +
    geom_line(aes(group = grid_id)) +
    scale_fill_identity() +
    facet_grid(name ~ grid_id, scales = "free")
}

# Tar spot (corn) --------------------------------------------------------------

#' Vulnerable growth stages V10-R3
#' Risk criteria: High >=35%, Medium >=20%, Low >0%
#' No risk: Fungicide in last 14 days, temperature <32F
#' Credit: Damon Smith UW-Madison
#' @param mean_temp_30ma Mean daily temperature, 30-day moving average, Celsius
#' @param max_rh_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param hrs_rh90_night_14ma Nighttime hours RH > 90%, 14-day moving average, 0-24 hours
#' @returns probability of spore presence
predict_tarspot <- function(mean_temp_30ma, max_rh_30ma, hrs_rh90_night_14ma) {
  mu1 <- 32.06987 +
    -0.89471 * mean_temp_30ma +
    -0.14373 * max_rh_30ma
  mu2 <- 20.35950 +
    -0.91093 * mean_temp_30ma +
    -0.29240 * hrs_rh90_night_14ma
  (logistic(mu1) + logistic(mu2)) / 2
}

#' Build from weather
#' Note: this model overwinters at 100% risk so use an attenuation function
#' @param daily daily weather data
build_tar_spot <- function(daily) {
  daily |>
    mutate(
      date = date,
      temperature_mean_30day = roll_mean(temperature_mean, 30),
      temperature_min_21day = roll_mean(temperature_min, 21),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      hours_rh_over_90_night_14day = roll_mean(hours_rh_over_90_night, 14),
      model_value = predict_tarspot(
        temperature_mean_30day,
        relative_humidity_max_30day,
        hours_rh_over_90_night_14day
      ) |>
        attenuate_prob(temperature_min_21day),
      risk_from_prob(model_value, 0.35, 0.2, 0.01),
      .by = grid_id,
      .keep = "used"
    )
}
# test_hourly_wx |> build_daily() |> build_tar_spot() |> test_plot()

# Gray leaf spot (corn) --------------------------------------------------------

#' Vulnerable growth stages V10-R3
#' Risk criteria: High >=60%, Medium >=40%, Low >0%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @param min_temp_21ma minimum daily temperature, 21-day moving average, Celsius
#' @param min_dewpoint_30ma minimum dew point temperature, 30-day moving average, Celsius
#' @returns probability of spore presence
predict_gls <- function(min_temp_21ma, min_dewpoint_30ma) {
  mu <- -2.9467 +
    -0.03729 * min_temp_21ma +
    0.6534 * min_dewpoint_30ma
  logistic(mu)
}

# Build from weather
#' @param daily daily weather data
build_gray_leaf_spot <- function(daily) {
  daily |>
    mutate(
      date = date,
      temperature_min_21day = roll_mean(temperature_min, 21),
      dew_point_min_30day = roll_mean(dew_point_min, 30),
      model_value = predict_gls(temperature_min_21day, dew_point_min_30day),
      risk_from_prob(model_value, 0.6, 0.4, 0.01),
      .by = grid_id,
      .keep = "used"
    )
}
# test_hourly_wx |> build_daily() |> build_gray_leaf_spot() |> test_plot()

# Gibberella ear rot / DON model (corn) ----------------------------------------

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
#' @returns probability of ear rot and deoxynivalenol concentration > 1 ppm in grain
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

#' Build from weather
#' @param daily daily weather data
build_don <- function(daily) {
  daily |>
    replace_na(list(precip_daily = 0)) |>
    mutate(
      date = date,
      temp_max_14day = roll_mean(temperature_max, 14),
      temp_min_14day = roll_mean(temperature_min, 14),
      days_temp_over_25_14day = roll_sum(temperature_mean >= 25, 14),
      days_precip_14day = roll_sum(precip_daily > 0, 14),
      rh_mean_14day = roll_mean(relative_humidity_mean, 14),
      days_rh_over_80_14day = roll_sum(relative_humidity_mean > 80, 14),
      model_value = predict_don(
        lag(temp_max_14day, 7),
        lag(temp_min_14day, 7),
        lag(days_temp_over_25_14day, 7),
        lag(days_precip_14day, 7),
        rh_mean_14day,
        days_rh_over_80_14day
      ),
      risk_from_prob(model_value, 0.8, 0.4, 0.2),
      .by = grid_id,
      .keep = "used"
    ) |>
    drop_na(model_value)
}
# test_hourly_wx |> build_daily() |> build_don() |> test_plot()

# White mold (soybean) ---------------------------------------------------------

#' Soybean white mold, non-irrigated version
#' Vulnerable growth stages: R1-R3
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

# build white mold non-irrigated risk probability from daily weather
#' @param daily daily weather data
build_white_mold_dry <- function(daily) {
  daily |>
    mutate(
      date = date,
      temperature_max_30day = roll_mean(temperature_max, 30),
      temperature_min_21day = roll_mean(temperature_min, 21),
      wind_speed_max_30day = roll_mean(wind_speed_max, 30),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      model_value = predict_white_mold_dry(
        temperature_max_30day,
        kmh_to_mps(wind_speed_max_30day),
        relative_humidity_max_30day
      ) |>
        attenuate_prob(temperature_min_21day),
      risk_from_prob(model_value, 0.35, 0.2, 0.0001),
      .by = grid_id,
      .keep = "used"
    )
}
# test_hourly_wx |> build_daily() |> build_white_mold_dry() |> test_plot()

#' Soybean white mold, irrigated version
#' Soybean growth stage R1-R3
#' Risk criteria: High >=40%, Med >=20%, Low >=5%
#' No risk: Fungicide app in last 14 days, min temp <32F
#' @param max_temp_30ma Maximum daily temperature, 30-day moving average, Celsius
#' @param max_rh_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param spacing Row spacing, either "15" or "30", inches
#' @returns probability of spore presence
predict_white_mold_irrig <- function(
  max_temp_30ma,
  max_rh_30ma,
  row_spacing
) {
  mu <- -52.65 +
    -2.38 * (row_spacing == "30") +
    0.65 * max_temp_30ma +
    0.38 * max_rh_30ma
  logistic(mu)
}

# build white mold irrigated risk probability from daily weather
#' @param daily daily weather data
#' @param row_spacing row spacing, either "15" or "30"
build_white_mold_irrig <- function(daily, row_spacing) {
  daily |>
    mutate(
      date = date,
      temperature_max_30day = roll_mean(temperature_max, 30),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      model_value = predict_white_mold_irrig(
        temperature_max_30day,
        relative_humidity_max_30day,
        row_spacing
      ),
      risk_from_prob(model_value, 0.1, 0.005, 0.0001),
      .by = grid_id,
      .keep = "used"
    )
}
# test_hourly_wx |> build_daily() |> build_white_mold_irrig("30") |> test_plot()
# test_hourly_wx |> build_daily() |> build_white_mold_irrig("15") |> test_plot()

# Frogeye leaf spot (soybean) --------------------------------------------------

#' Vulnerable growth stages R1-R5
#' Risk criteria: High >=50%, Medium >=40%, Low >0%
#' No risk: Fungicide in last 14 days, temperature <32F
#' @param max_temp_30ma Maximum daily temperature, 30-day moving average, Celsius
#' @param hours_rh80_30ma Daily hours RH > 80%, 30-day moving average, 0-24 hours
#' @returns probability of spore presence
predict_fls <- function(max_temp_30ma, hours_rh80_30ma) {
  mu <- -5.92485 +
    0.12208 * max_temp_30ma +
    0.17326 * hours_rh80_30ma
  logistic(mu)
}

# build frogeye leaf spot risk probability from daily weather
#' @param daily daily weather data
build_frogeye_leaf_spot <- function(daily) {
  daily |>
    mutate(
      date = date,
      temperature_max_30day = roll_mean(temperature_max, 30),
      hours_rh_over_80_30day = roll_mean(hours_rh_over_80, 30),
      model_value = predict_fls(temperature_max_30day, hours_rh_over_80_30day),
      risk_from_prob(model_value, 0.5, 0.4, 0.01),
      .by = grid_id,
      .keep = "used"
    )
}
# test_hourly_wx |> build_daily() |> build_frogeye_leaf_spot() |> test_plot()

# Wheat scab FHB ---------------------------------------------------------------

#' Wheat scab model - fusarium head blight
#' Relevant during wheat flowering
#' Risk criteria: High >= .32, Medium >= .24, Low > 0
#' Credit: Erick DeWolf, U. Kentucky
#' @param mean_rh_14ma mean daily relative humidity 0-100, 14 day rolling mean
#' @returns matrix of disease probabilities by scab resistance rating
predict_wheat_scab <- function(mean_rh_14ma) {
  res <- c(
    "VS" = 0, # very susceptible
    "S" = -0.82795556, # susceptible
    "MS" = -1.4812696, # moderately susceptible
    "MR" = -1.8484537 # moderately resistant
  )
  sapply(res, function(r) {
    mu <- -3.6432643 + r + 0.051459669 * mean_rh_14ma
    logistic(mu)
  })
}

# Build wheat scab risk probability from daily weather
#' @param daily daily weather data
#' @param resistance wheat resistance to FHB, must match levels in `predict_wheat_scab`
build_wheat_scab <- function(daily, resistance) {
  daily |>
    mutate(
      date = date,
      rh_mean_14day = roll_mean(relative_humidity_mean, 14),
      model_value = predict_wheat_scab(rh_mean_14day)[, resistance],
      risk_from_prob(model_value, 0.32, 0.24, 0.01),
      .by = grid_id,
      .keep = "used"
    )
}
# test_hourly_wx |> build_daily() |> build_wheat_scab("VS") |> test_plot()
# test_hourly_wx |> build_daily() |> build_wheat_scab("MR") |> test_plot()

# Early blight (Solanum) -------------------------------------------------------

#' Pscheidt and Stevenson 1988: https://link.springer.com/article/10.1007/BF02854357
#' More information: https://vegpath.plantpath.wisc.edu/diseases/potato-early-blight/

#' P-day helper function for an individual temperature
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

#' Calculate potato physiological days
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

#' Assign risk from pdays
#' @param value dsv from `calc_pdays`
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

#' Build results from weather
#' @param daily daily weather data
build_early_blight <- function(daily) {
  daily |>
    mutate(
      date = date,
      model_value = calc_pdays(
        temperature_min,
        temperature_max
      ),
      risk_for_early_blight(model_value),
      .by = grid_id,
      .keep = "used"
    )
}
# test_hourly_wx |> build_daily() |> build_early_blight() |> test_plot()

# Late blight (Solanum) --------------------------------------------------------

#' Based on the Wallins BLITECAST model:
#' - https://www.google.com/books/edition/The_Plant_Disease_Reporter/ow9BD6P2KZ4C?hl=en&gbpv=1&pg=PA95&printsec=frontcover
#' - https://ipm.ucanr.edu/DISEASE/DATABASE/potatolateblight.html
#' More information:
#' - https://vegpath.plantpath.wisc.edu/diseases/potato-late-blight/

#' Calculate disease severity values
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

#' Build from weather
#' @param daily daily weather data
build_late_blight <- function(daily) {
  daily |>
    mutate(
      date = date,
      model_value = calc_late_blight_dsv(
        temperature_mean_rh_over_90,
        hours_rh_over_90
      ),
      risk_for_late_blight(model_value),
      .by = grid_id,
      .keep = "used"
    )
}
# test_hourly_wx |> build_daily() |> build_late_blight() |> test_plot()

# Alternaria leaf blight (carrot) ----------------------------------------------

#' Based on the Pitblado 1992 TOMCAST model:
#'   https://atrium.lib.uoguelph.ca/server/api/core/bitstreams/5c7ec712-43ef-4b73-b8b8-e85c29d3d58b/content
#' Which is based on the Madden et al 1978 FAST model:
#'   https://www.apsnet.org/publications/phytopathology/backissues/Documents/1978Articles/Phyto68n09_1354.PDF

#' Calculate disease severity values for a day
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

#' Assign risk for daily DSV
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

#' Build from weather
#' @param daily daily weather data
build_alternaria <- function(daily) {
  daily |>
    mutate(
      date = date,
      model_value = calc_alternaria_dsv(
        temperature_mean_rh_over_90,
        hours_rh_over_90
      ),
      risk_for_alternaria(model_value),
      .by = grid_id,
      .keep = "used"
    )
}
# test_hourly_wx |> build_daily() |> build_alternaria() |> test_plot()

# Cercospora leaf blight (beet) ------------------------------------------------

# Based on Windels 1998: https://apsjournals.apsnet.org/doi/abs/10.1094/PDIS.1998.82.7.716
# More information: https://vegpath.plantpath.wisc.edu/diseases/carrot-alternaria-and-cercospora-leaf-blights/

#' Calculate daily infection values
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

#' Assign severity from DIV
#' @param value DIV from `calc_cercospora_div`
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

#' Build from weather
#' @param daily daily weather data
build_cercospora <- function(daily) {
  daily |>
    mutate(
      date = date,
      model_value = calc_cercospora_div(
        temperature_mean_rh_over_90,
        hours_rh_over_90
      ),
      risk_for_cercospora(model_value),
      .by = grid_id,
      .keep = "used",
    )
}
# test_hourly_wx |> build_daily() |> build_cercospora() |> test_plot()

# Botrytis leaf blight (onion) -------------------------------------------------

# Onion botrytis leaf blight
# based on Sutton et al 1986 https://doi.org/10.1016/0167-8809(86)90136-2
# more information: https://vegpath.plantpath.wisc.edu/diseases/onion-botrytis/
# requires 5 days of hourly weather to compute

#' Botcast daily innoculum value
#' this determines if innoculum is predicted to be present
#' A) if temp > 30 for >= 4 hrs on any of past 5 days => 0
#' B) if h < 5 => 0
#' C) if h > 12 => 1
#' D) if prev day dry (<70% rh for >= 6 hrs) and no rain => 0 else 1
#' @param hot T/F temp > 30 for >= 4 hrs on any of past 5 days
#' @param dry T/F previous day dry
#' @param hours_rh90 hours rh > 90 during past day
botcast_dinov <- function(hot, dry, hours_rh90) {
  case_when(
    hot ~ 0,
    hours_rh90 < 5 ~ 0,
    hours_rh90 > 12 ~ 1,
    dry ~ 0,
    TRUE ~ 1
  )
}

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

# Final botrytis disease severity index calculation
#' @param hot T/F temp > 30 for >= 4 hrs on any of past 5 days
#' @param dry T/F previous day dry
#' @param hours_rh_over_90 hours with RH > 90% over past day
#' @param mean_temp_rh90 mean air temperature for hours with RH > 90%
calc_botrytis_dsi <- function(hot, dry, hours_rh_over_90, mean_temp_rh90) {
  dinov <- botcast_dinov(hot, lag(dry, default = FALSE), hours_rh_over_90)
  dinfv <- botcast_dinfv(mean_temp_rh90, hours_rh_over_90)
  dinov * dinfv
}

#' Assign risk based on total DSI
#' @param value DSI from `calc_botrytis_dsi`
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

#' Build from weather
#' @param daily daily weather data
build_botrytis <- function(daily) {
  daily |>
    mutate(
      date = date,
      model_value = calc_botrytis_dsi(
        hot_past_5_days,
        dry,
        hours_rh_over_90,
        temperature_mean_rh_over_90
      ),
      risk_for_botrytis(model_value),
      .by = grid_id,
      .keep = "used",
    )
}
# test_hourly_wx |> build_daily() |> build_botrytis() |> test_plot()
