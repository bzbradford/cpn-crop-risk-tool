# Data pipeline ----

#' Generate daily summary data from hourly weather
#' @param hourly accepts the cleaned hourly data from `build_hourly()`
#' @returns tibble
build_daily <- function(hourly) {
  # grid attributes to be retained
  lat_lng <- hourly %>%
    distinct(grid_id, grid_lat, grid_lng)

  # summarized by calendar date
  summary_fns <- c("min" = calc_min, "mean" = calc_mean, "max" = calc_max)
  by_date <- hourly %>%
    summarize(
      hours = n(),
      across(c(temperature, dew_point, dew_point_depression, relative_humidity), summary_fns),
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
    ) %>%
    mutate(precip_cumulative = cumsum(precip_daily), .after = precip_max_hourly, .by = grid_id ) %>%
    mutate(snow_cumulative = cumsum(snow_daily), .after = snow_max_hourly, .by = grid_id) %>%
    mutate(
      # for botcast
      hot_past_5_days = rollapplyr(hours_temp_over_30, width = 5, FUN = \(x) any(x >= 4), partial = TRUE),
      dry = (hours_rh_under_70 >= 6) & (precip_daily < 1)
    )

  # summarized by "date since night" eg since 8 pm the day before through 7 pm
  by_night <- hourly %>%
    mutate(
      rh80 = relative_humidity >= 80,
      rh90 = relative_humidity >= 90
    ) %>%
    summarize(
      hours_rh_over_80_night = sum(night & rh80),
      hours_rh_over_90_night = sum(night & rh90),
      temperature_mean_rh_over_80 = if_else(sum(rh80) > 0, sum(temperature * (rh80)) / sum(rh80), NA),
      temperature_mean_rh_over_90 = if_else(sum(rh90) > 0, sum(temperature * (rh90)) / sum(rh90), NA),
      .by = c(grid_id, date_since_night)
    )

  # assemble the data
  by_date %>%
    filter((hours >= 12) | (date == today())) %>%
    left_join(by_night, join_by(grid_id, date == date_since_night)) %>%
    left_join(lat_lng, join_by(grid_id)) %>%
    relocate(grid_lat, grid_lng, .after = grid_id) %>%
    arrange(grid_id, date)
}

# saved_weather %>%
#   filter(grid_id == sample(grid_id, 1)) %>%
#   build_hourly() %>%
#   build_daily() %>%
#   pivot_longer(-c(grid_id:day)) %>%
#   ggplot(aes(x = date, y = value, color = name)) +
#   geom_line(show.legend = F) +
#   facet_wrap(~name, scales = "free")



#' Generate several moving average periods from daily data
#' @param daily accepts daily data from `build_daily()`
#' @param align moving average alignment
#' @returns tibble
build_ma_from_daily <- function(daily, align = c("center", "right")) {
  align <- match.arg(align)

  # retain attribute cols
  attr <- daily %>% select(grid_id, any_of(OPTS$date_attr_cols))

  # define moving average functions
  roll_mean <- function(vec, width) rollapply(vec, width, \(x) calc_mean(x), partial = TRUE, align = align)
  fns <- c(
    "7day" = ~roll_mean(.x, 7),
    "14day" = ~roll_mean(.x, 14),
    "21day" = ~roll_mean(.x, 21),
    "30day" = ~roll_mean(.x, 30)
  )

  # apply moving average functions to each primary data column
  ma <- daily %>%
    select(-hours) %>%
    mutate(
      across(starts_with(c("temperature", "dew_point", "relative_humidity", "wind", "pressure", "hours")), fns),
      .by = grid_id,
      .keep = "none"
    ) %>%
    select(-grid_id)

  # bind attributes
  bind_cols(attr, ma)
}

# test <- saved_weather %>% build_daily() %>% filter(grid_id == sample(grid_id, 1)) %>% build_ma_from_daily()
# test <- saved_weather %>% build_daily() %>% build_ma_from_daily()
# ggplot(test, aes(x = date, color = grid_id)) +
#   geom_line(aes(y = dew_point_min_7day))


test_plot <- function(df) {
  df %>%
    ggplot(aes(x = date, y = model_value)) +
    geom_col(aes(fill = risk_color), lwd = 0, width = 1) +
    geom_line(aes(group = grid_id)) +
    scale_fill_identity() +
    facet_wrap(~grid_id)
}

test_plot_severity <- function(df) {
  df %>%
    ggplot(aes(x = date, y = severity)) +
    geom_col(aes(fill = risk_color), lwd = 0, width = 1) +
    geom_line(aes(group = grid_id)) +
    scale_fill_identity() +
    facet_wrap(~grid_id)
}


build_tar_spot <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      temperature_mean_30day = roll_mean(temperature_mean, 30),
      temperature_min_21day = roll_mean(temperature_min, 21),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      hours_rh_over_90_night_14day = roll_mean(hours_rh_over_90_night, 14),
      model_value = predict_tarspot(
        temperature_mean_30day,
        relative_humidity_max_30day,
        hours_rh_over_90_night_14day
      ) %>% attenuate_prob(temperature_min_21day),
      assign_risk("tar_spot_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>% select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_tar_spot() %>% test_plot()


build_gray_leaf_spot <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      temperature_min_21day = roll_mean(temperature_min, 21),
      dew_point_min_30day = roll_mean(dew_point_min, 30),
      model_value = predict_gls(temperature_min_21day, dew_point_min_30day),
      assign_risk("gray_leaf_spot_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>% select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_gray_leaf_spot() %>%


build_don <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    replace_na(list(precip_daily = 0)) %>%
    mutate(
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
      assign_risk("don_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>% select(-grid_id)
  bind_cols(attr, disease) %>%
    drop_na(model_value)
}

# saved_weather %>% build_daily() %>% build_don() %>% test_plot()


build_white_mold_dry <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      temperature_max_30day = roll_mean(temperature_max, 30),
      temperature_min_21day = roll_mean(temperature_min, 21),
      wind_speed_max_30day = roll_mean(wind_speed_max, 30),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      model_value = predict_white_mold_dry(
        temperature_max_30day,
        kmh_to_mps(wind_speed_max_30day),
        relative_humidity_max_30day
      ) %>% attenuate_prob(temperature_min_21day),
      assign_risk("white_mold_dry_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_white_mold_dry() %>% test_plot()


build_white_mold_irrig <- function(daily, spacing = c("30", "15")) {
  spacing <- match.arg(spacing)
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      temperature_max_30day = roll_mean(temperature_max, 30),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      model_value = predict_white_mold_irrig(
        temperature_max_30day,
        relative_humidity_max_30day,
        spacing
      ),
      assign_risk("white_mold_irrig_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_white_mold_irrig("30") %>% test_plot()
# saved_weather %>% build_daily() %>% build_white_mold_irrig("15") %>% test_plot()


build_frogeye_leaf_spot <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      temperature_max_30day = roll_mean(temperature_max, 30),
      temperature_mean_30day = roll_mean(temperature_mean, 30),
      temperature_min_21day = roll_mean(temperature_min, 21),
      wind_speed_max_30day = roll_mean(wind_speed_max, 30),
      relative_humidity_max_30day = roll_mean(relative_humidity_max, 30),
      dew_point_min_30day = roll_mean(dew_point_min, 30),
      hours_rh_over_80_30day = roll_mean(hours_rh_over_80, 30),
      hours_rh_over_90_night_14day = roll_mean(hours_rh_over_90_night, 14),
      model_value = predict_fls(temperature_max_30day, hours_rh_over_80_30day),
      assign_risk("frogeye_leaf_spot_prob", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_frogeye_leaf_spot() %>% test_plot()


build_early_blight <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      model_value = calc_pdays(
        temperature_min,
        temperature_max
      ),
      assign_risk("early_blight_pdays", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_early_blight() %>% test_plot()
# saved_weather %>% build_daily() %>% build_early_blight() %>% test_plot_severity()


build_late_blight <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      model_value = calc_late_blight_dsv(
        temperature_mean_rh_over_90,
        hours_rh_over_90
      ),
      assign_risk("late_blight_dsv", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_late_blight() %>% test_plot()
# saved_weather %>% build_daily() %>% build_late_blight() %>% test_plot_severity()


build_alternaria <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      model_value = calc_alternaria_dsv(
        temperature_mean_rh_over_90,
        hours_rh_over_90
      ),
      assign_risk("alternaria_dsv", model_value),
      .by = grid_id, .keep = "used"
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_alternaria() %>% test_plot()
# saved_weather %>% build_daily() %>% build_alternaria() %>% test_plot_severity()


build_cercospora <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      model_value = calc_cercospora_div(temperature_mean_rh_over_90, hours_rh_over_90),
      assign_risk("cercospora_div", model_value),
      .by = grid_id, .keep = "used",
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_cercospora() %>% test_plot()
# saved_weather %>% build_daily() %>% build_cercospora() %>% test_plot_severity()


build_botrytis <- function(daily) {
  attr <- daily %>% select(grid_id, date)
  disease <- daily %>%
    mutate(
      model_value = calc_botrytis_dsi(hot_past_5_days, dry, hours_rh_over_90, temperature_mean_rh_over_90),
      assign_risk("botrytis_dsi", model_value),
      .by = grid_id, .keep = "used",
    ) %>%
    select(-grid_id)
  bind_cols(attr, disease)
}

# saved_weather %>% build_daily() %>% build_botrytis() %>% test_plot()
# saved_weather %>% build_daily() %>% build_botrytis() %>% test_plot_severity()




#' Generate various growing degree day models with and without an 86F upper threshold
#' input temperatures must be Celsius and will be converted to Fahrenheit GDDs
#' @param daily accepts daily dataset from `build_daily()`
#' @returns tibble
build_gdd_from_daily <- function(daily) {
  # retain attribute cols
  attr <- daily %>% select(grid_id, date)

  # convert temperatures
  tmin <- c_to_f(daily$temperature_min)
  tmax <- c_to_f(daily$temperature_max)

  # start with a base 86F model to chopping off the upper thresholds
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
  bind_cols(attr, gdd) %>%
    mutate(
      across(starts_with("base_"), c(cumulative = cumsum)),
      .by = grid_id
    ) %>%
    select(
      all_of(names(attr)),
      all_of(sort(names(.)))
    )
}

# saved_weather %>% build_hourly() %>% build_daily() %>% build_gdd_from_daily()
