# Rye biomass ----

# generate cumulative sum up to n values then retain last value
cumsum_limit <- function(vec, n) {
  x <- cumsum(vec)
  if (n < length(vec)) {
    x[(n + 1):length(vec)] <- x[n]
  }
  x
}

# cumsum_limited(replicate(100, 1), 60)

#' @param gdd_after_plant cumulative base 32F GDD for the 60 days after planting
#' @param precip_after_plant cumulative precipitation (inches) for the 60 days after planting
#' @param gdd_before_term cumulative base 32F GDD for the 60 days prior to termination
predict_rye_biomass <- function(
  gdd_after_plant,
  precip_after_plant,
  gdd_before_term
) {
  -5725.79 +
    6.19 * gdd_after_plant +
    7.02 * gdd_before_term +
    -403.59 * precip_after_plant
}

# predict_rye_biomass(2600, 2600, 2)

if (FALSE) {
  expand_grid(
    gdd_after_plant = seq(0, 2500, by = 250),
    gdd_before_term = seq(0, 2500, by = 250),
    precip_after_plant = seq(0, 5, by = .25)
  ) |>
    mutate(
      biomass = predict_rye_biomass(
        gdd_after_plant,
        precip_after_plant,
        gdd_before_term
      ),
      biomass_clamped = clamp(biomass, 0, 10000)
    ) |>
    ggplot(aes(x = gdd_after_plant, y = gdd_before_term, fill = biomass)) +
    geom_tile() +
    facet_wrap(~precip_after_plant, labeller = "label_both") +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0
    )
}


# requires imperial units as inputs. output biomass in lbs/ac
build_rye_termination <- function(daily) {
  daily |>
    mutate(
      date = date,
      across(c(temperature_min, temperature_max), c_to_f),
      across(precip_daily, mm_to_in),
      gdd = gdd_sine(temperature_min, temperature_max, 32),
      gdd_after_plant = cumsum_limited(gdd, 60),
      gdd_before_term = roll_sum(gdd, 60),
      precip_after_plant = cumsum_limited(precip_daily, 60),
      biomass = predict_rye_biomass(
        gdd_after_plant,
        gdd_before_term,
        precip_after_plant
      ),
      model_value = clamp(biomass, 0, 10000),
      risk = case_when(
        model_value < 2000 ~ "Low",
        model_value < 4500 ~ "Moderate",
        TRUE ~ "High"
      ) |>
        factor(levels = c("Low", "Moderate", "High")),
      risk_color = recode_values(
        risk,
        "Low" ~ "red",
        "Moderate" ~ "yellow",
        "High" ~ "green"
      ),
      .by = grid_id,
      .keep = "used"
    )
}

build_rye_termination <- function(daily) {
  wx <- daily |>
    mutate(
      across(c(temperature_min, temperature_max), c_to_f),
      across(precip_daily, mm_to_in),
      gdd = gdd_sine(temperature_min, temperature_max, 32)
    )

  fall_wx <- wx |>
    filter(
      yday > 215,
      year == first(year),
      row_number() <= 60,
      .by = grid_id
    ) |>
    summarize(
      fall_gdd = sum(gdd),
      fall_precip = sum(precip_daily),
      .by = c(grid_id)
    )

  # spring weather
  wx |>
    filter(yday > 0, year == last(year)) |>
    left_join(fall_wx, join_by(grid_id)) |>
    mutate(
      date = date,
      gdd_before_term = roll_sum(gdd, 60),
      biomass = predict_rye_biomass(
        clamp(fall_gdd, 0, 1600),
        fall_precip,
        clamp(gdd_before_term, 0, 1600)
      ),
      model_value = biomass,
      # model_value = clamp(biomass, 0, 10000),
      risk = case_when(
        model_value < 2000 ~ "Low",
        model_value < 4500 ~ "Moderate",
        TRUE ~ "High"
      ) |>
        factor(levels = c("Low", "Moderate", "High")),
      risk_color = recode_values(
        risk,
        "Low" ~ "red",
        "Moderate" ~ "yellow",
        "High" ~ "green"
      ),
      .by = grid_id,
      .keep = "used"
    )
}

test_rye <- test_hourly_wx |>
  build_daily() |>
  filter(between(date, ymd("2024-10-1"), ymd("2025-6-15"))) |>
  build_rye_termination()
test_rye |> test_plot()


# Alternative model 1 ----

predict_rye_theoretical <- function(
  gdd_total,
  precip_inches,
  plant_doy,
  max_biomass = 12000
) {
  # 1. Base Growth Potential (Sigmoid)
  # Midpoint at 1200 GDD, growth rate k=0.005
  k <- 0.005
  x0 <- 1200
  base_growth <- 1 / (1 + exp(-k * (gdd_total - x0)))

  # 2. Planting Date Penalty (Exponential Decay)
  # Optimal planting is Day 250 (Sept 7). Penalty grows after that.
  optimum_doy <- 250
  doy_penalty <- if_else(
    plant_doy > optimum_doy,
    exp(-0.01 * (plant_doy - optimum_doy)),
    1.0
  )

  # 3. Precipitation Modifier (Bell Curve)
  # Assumes ~10 inches is "perfect" for the 60-day window
  precip_mod <- exp(-0.02 * (precip_inches - 5)^2)

  # Combine
  final_biomass <- max_biomass * base_growth * doy_penalty * precip_mod

  return(final_biomass)
}

# Quick Sensitivity Test
expand_grid(
  gdd = seq(0, 3000, by = 100),
  precip_inches = seq(0, 10, by = 0.1),
  plant_doy = seq(200, 300, by = 10)
) |>
  mutate(biomass = predict_rye_theoretical(gdd, precip_inches, plant_doy)) |>
  ggplot(aes(x = gdd, y = precip_inches, fill = biomass)) +
  geom_tile() +
  facet_wrap(~plant_doy, labeller = "label_both") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  )

build_rye_theoretical <- function(daily) {
  daily |>
    mutate(
      date = date,
      planting_day = first(yday),
      across(c(temperature_min, temperature_max), c_to_f),
      across(precip_daily, mm_to_in),
      gdd = gdd_sine(temperature_min, temperature_max, 32),
      gdd_cumulative = cumsum(gdd),
      precip_cumulative = cumsum_limit(precip_daily, 60),
      biomass = predict_rye_theoretical(
        gdd_cumulative,
        precip_cumulative,
        planting_day
      ),
      model_value = biomass,
      risk = case_when(
        model_value < 2000 ~ "Low",
        model_value < 4500 ~ "Moderate",
        TRUE ~ "High"
      ) |>
        factor(levels = c("Low", "Moderate", "High")),
      risk_color = recode_values(
        risk,
        "Low" ~ "red",
        "Moderate" ~ "yellow",
        "High" ~ "green"
      ),
      .by = grid_id,
      .keep = "used"
    )
}

test_rye <- test_hourly_wx |>
  build_daily() |>
  filter(between(date, ymd("2024-10-1"), ymd("2025-6-15"))) |>
  build_rye_theoretical()
test_rye |> test_plot()


# Alternative model 2 - with fall and spring precip ----

predict_rye_multi_season <- function(
  gdd_total,
  precip_fall, # Planting to Dec 31
  precip_spring, # Jan 1 to Termination
  plant_doy
) {
  # coefs
  L = 15000 # max biomass
  k = 0.005 # biomass growth per gdd
  x0 = 1200 # midpoint gdd for growth curve
  p_sat = 2.0 # precip saturation constant = optimum fall precip
  p_opt = 10 # optimum spring precip
  p_sens = 0.01 # precipitation sensitivity
  d_opt = 250 # latest planting date before penalty
  d_sens = 0.01 # planting date penalty modifier

  # 1. Base Potential (Sigmoid GDD)
  base_growth <- L / (1 + exp(-k * (gdd_total - x0)))

  # 2. Fall Precipitation Modifier (Asymptotic)
  # It needs ~4-6 inches for great establishment.
  # More than that doesn't add much, but it doesn't hurt yet.
  fall_mod <- precip_fall / (precip_fall + p_sat) # Saturation constant of 2"

  # 3. Spring Precipitation Modifier (Bell Curve)
  # Optimal spring rain is ~8-12 inches depending on your soil.
  spring_mod <- exp(-p_sens * (precip_spring - p_opt)^2)

  # 4. Late Planting Penalty (Exponential)
  doy_penalty <- if_else(
    plant_doy > d_opt,
    exp(-d_sens * (plant_doy - d_opt)),
    1.0
  )

  # Combine Multiplicatively
  final_biomass <- base_growth * fall_mod * spring_mod * doy_penalty

  return(final_biomass)
}

expand_grid(
  gdd = seq(0, 3000, by = 250),
  precip_fall = seq(0, 10, by = 0.5),
  precip_spring = seq(0, 12, by = 0.5),
  plant_doy = seq(200, 300, by = 10)
) |>
  mutate(
    biomass = predict_rye_multi_season(
      gdd,
      precip_fall,
      precip_spring,
      plant_doy
    )
  ) |>
  ggplot(aes(x = gdd, y = precip_fall + precip_spring, fill = biomass)) +
  geom_tile() +
  facet_wrap(~plant_doy, labeller = "label_both") +
  scale_fill_viridis_c()

# example model to determine the true coefficients for the model formulae
rye_model <- nls(
  biomass ~ L /
    (1 + exp(-k * (gdd - x0))) * # Sigmoid GDD
    exp(-p_sens * (precip_s - p_opt)^2) * # Spring Precip Bell
    exp(-d_sens * (plant_doy - 250)), # Planting Date Decay
  data = original_data, # sub in actual data here
  start = list(
    # Initial "Guesstimates"
    L = 12000,
    k = 0.005,
    x0 = 1200,
    p_sens = 0.01,
    p_opt = 10,
    d_sens = 0.01
  )
)
