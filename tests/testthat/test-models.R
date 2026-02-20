# tests for src/models.R

# Weather data ------------------------------------------------------------

test_that("build_daily", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      pivot_longer(-c(grid_id:day)) |>
      drop_na(value) |>
      ggplot(aes(x = date, y = value, color = name)) +
      geom_line(show.legend = F) +
      facet_wrap(~name, scales = "free")
  })
})

test_that("build_ma_from_daily", {
  expect_silent({
    test_hourly_wx |>
      filter(grid_id == sample(grid_id, 1)) |>
      build_daily() |>
      build_ma_from_daily() |>
      ggplot(aes(x = date, color = grid_id)) +
      geom_line(aes(y = temperature_mean_7day))
  })
})

test_that("gdd_sine", {
  expect_silent({
    expand_grid(tmin = 0:30, tmax = 0:30) |>
      filter(tmax >= tmin) |>
      mutate(gdd = gdd_sine(tmin, tmax, 10)) |>
      ggplot(aes(x = tmin, y = tmax, fill = gdd)) +
      geom_tile() +
      scale_fill_viridis_c() +
      coord_cartesian(expand = F)
  })
})

test_that("build_gdd_from_daily", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_gdd_from_daily()
  })
})


# Model functions --------------------------------------------------------------

## Field crops ----
test_that("predict_tarspot", {
  expect_silent({
    expand_grid(temp = 10:40, rh = seq(0, 100, 5), hours = 0:24) |>
      mutate(prob = predict_tarspot(temp, rh, hours)) |>
      ggplot(aes(x = temp, y = rh, fill = prob)) +
      geom_tile() +
      facet_wrap(~hours) +
      scale_fill_distiller(palette = "Spectral") +
      coord_cartesian(expand = F)
  })
})

test_that("predict_gls", {
  expect_silent({
    expand_grid(temp = 0:30, dp = 0:30) |>
      mutate(prob = predict_gls(temp, dp)) |>
      ggplot(aes(x = temp, y = dp)) +
      geom_tile(aes(fill = prob)) +
      scale_fill_distiller(palette = "Spectral") +
      coord_cartesian(expand = F)
  })
})

test_that("predict_don", {
  expect_silent({
    test <- expand_grid(
      w7_max_temp = 10:30,
      w7_min_temp = 0:30,
      w7_days_temp_over_25 = 0:14,
      w7_days_precip = 0:14,
      w0_mean_rh = 10 * (0:10),
      w0_days_rh_over_80 = 0:14
    ) |>
      filter(w7_max_temp > w7_min_temp) |>
      mutate(
        don = predict_don(
          w7_max_temp,
          w7_min_temp,
          w7_days_temp_over_25,
          w7_days_precip,
          w0_mean_rh,
          w0_days_rh_over_80
        )
      )

    # hist(test$don)

    test |>
      filter(don > .01) |>
      pivot_longer(1:6) |>
      select(name, value, don) |>
      mutate(value = factor(value)) |>
      ggplot(aes(x = value, y = don)) +
      geom_boxplot(aes(fill = after_stat(middle)), outlier.size = .5) +
      scale_fill_viridis_c() +
      facet_wrap(~name, scales = "free")
  })
})

test_that("predict_white_mold_dry", {
  expect_silent({
    expand_grid(temp = 0:40, wind = 0:20, rh = (0:10) * 10) |>
      mutate(prob = predict_white_mold_dry(temp, wind, rh)) |>
      ggplot(aes(x = temp, y = wind, fill = prob)) +
      geom_tile() +
      scale_fill_distiller(palette = "Spectral", limits = c(0, 1)) +
      coord_cartesian(expand = F) +
      facet_wrap(~rh, labeller = "label_both")
  })
})

test_that("predict_white_mold_irrig", {
  expect_silent({
    expand_grid(temp = 15:40, rh = seq(50, 100, 5), spacing = c("15", "30")) |>
      rowwise() |>
      mutate(prob = predict_white_mold_irrig(temp, rh, spacing)) |>
      ggplot(aes(x = temp, y = rh, fill = prob)) +
      geom_tile() +
      facet_wrap(~spacing, ncol = 1) +
      scale_fill_distiller(palette = "Spectral") +
      coord_cartesian(expand = F)
  })
})

test_that("predict_fls", {
  expect_silent({
    expand_grid(temp = 0:40, hours = 0:24) |>
      mutate(prob = predict_fls(temp, hours)) |>
      ggplot(aes(x = temp, y = hours, fill = prob)) +
      geom_tile() +
      scale_fill_distiller(palette = "Spectral") +
      coord_cartesian(expand = F)
  })
})

test_that("predict_wheat_scab", {
  expect_silent({
    expand_grid(rh = 0:100) |>
      mutate(predict_wheat_scab(rh) |> as_tibble()) |>
      pivot_longer(-rh, names_to = "resistance", values_to = "prob") |>
      ggplot(aes(x = resistance, y = rh, fill = prob)) +
      geom_tile() +
      scale_fill_distiller(palette = "Spectral") +
      coord_cartesian(expand = F)
  })
})


## Vegetable crops ----

test_that("calc_pdays", {
  expect_silent({
    expand_grid(tmin = 0:35, tmax = 0:35) |>
      mutate(pdays = calc_pdays(tmin, tmax)) |>
      ggplot(aes(x = tmin, y = tmax, fill = pdays)) +
      geom_tile() +
      scale_fill_viridis_c() +
      coord_cartesian(expand = F)
  })
})

test_that("calc_late_blight_dsv", {
  expect_silent({
    expand_grid(temp = 0:30, hours = 0:24) |>
      mutate(dsv = calc_late_blight_dsv(temp, hours)) |>
      ggplot(aes(x = temp, y = hours, fill = dsv)) +
      geom_tile() +
      scale_fill_viridis_c() +
      coord_cartesian(expand = F)
  })
})

test_that("calc_alternaria_dsv", {
  expect_silent({
    expand_grid(temp = 0:30, hours = 0:24) |>
      mutate(dsv = calc_alternaria_dsv(temp, hours)) |>
      ggplot(aes(x = temp, y = hours, fill = dsv)) +
      geom_tile() +
      scale_fill_viridis_c() +
      coord_cartesian(expand = F)
  })
})

test_that("calc_cercospora_div", {
  expect_silent({
    expand_grid(temp = 15:30, hours = 0:24) |>
      mutate(dsv = calc_cercospora_div(temp, hours), temp = c_to_f(temp)) |>
      ggplot(aes(x = temp, y = hours, fill = dsv)) +
      geom_tile() +
      scale_fill_viridis_c() +
      coord_cartesian(expand = F)
  })
})

test_that("botcast_dinov", {
  expect_silent({
    expand_grid(
      hot = c(F, T),
      lw = 0:24,
      dry = c(F, T)
    ) |>
      mutate(dinov = botcast_dinov(hot, lw, dry)) |>
      summary()
  })
})

test_that("botcast_dinfv", {
  expect_silent({
    expand_grid(temp = (60:280) / 10, hours = 0:24) |>
      mutate(dinfv = botcast_dinfv(temp, hours)) |>
      ggplot(aes(x = temp, y = hours, fill = dinfv)) +
      geom_tile() +
      scale_fill_viridis_c() +
      coord_equal(expand = F)
  })
})

test_that("calc_botrytis_dsi", {
  expect_silent({
    expand_grid(
      hot = c(T, F),
      dry = c(T, F),
      hours_rh90 = 0:24,
      mean_temp_rh90 = 0:35
    ) |>
      mutate(value = calc_botrytis_dsi(hot, dry, hours_rh90, mean_temp_rh90)) |>
      count(value)
  })
})


# Risk calculators -------------------------------------------------------------

test_that("assign_risk", {
  expect_s3_class(assign_risk("tar_spot_prob", .2), "data.frame")
  expect_warning(assign_risk("don_prob", 10))
  expect_error(assign_risk("foo", 1))
})

test_that("risk_from_prob", {
  expect_silent({
    tibble(
      value = runif(20),
      risk_from_prob(value, 5, 25, 60)
    )
  })
})

test_that("attenuate_prob", {
  expect_silent({
    tibble(value = c(10:1, 2:10), temp = c(1:10, 9:1) * 3) |>
      mutate(new_value = attenuate_prob(value, temp))
  })
})

test_that("risk_from_severity", {
  expect_silent({
    tibble(
      value = round(runif(10, 0, 4)),
      risk_from_severity(value)
    )
  })
})

test_that("risk_for_early_blight", {
  expect_silent({
    tibble(
      value = runif(100, 5, 10),
      risk_for_early_blight(value),
    ) |>
      mutate(day = row_number()) |>
      ggplot(aes(x = day, color = risk, group = 1)) +
      geom_line(aes(y = total)) +
      scale_color_brewer(palette = "Spectral", direction = -1)
  })
})

test_that("risk_for_late_blight", {
  expect_silent({
    tibble(
      value = runif(100, 0, 3),
      risk_for_late_blight(value)
    ) |>
      mutate(day = row_number()) |>
      ggplot(aes(x = day, group = 1, color = risk)) +
      geom_line(aes(y = total14)) +
      geom_line(aes(y = total)) +
      scale_color_brewer(palette = "Spectral", direction = -1)
  })
})

test_that("risk_for_alternaria", {
  expect_silent({
    tibble(
      value = runif(100, 0, 5),
      risk_for_alternaria(value)
    ) |>
      mutate(day = row_number()) |>
      ggplot(aes(x = day, color = risk, group = 1)) +
      geom_line(aes(y = total7)) +
      scale_color_brewer(palette = "Spectral", direction = -1)
  })
})

test_that("risk_for_cercospora", {
  expect_silent({
    tibble(
      value = runif(100, 0, 4),
      risk_for_cercospora(value)
    ) |>
      mutate(day = row_number()) |>
      ggplot(aes(x = day, color = risk, group = 1)) +
      geom_line(aes(y = avg2)) +
      geom_line(aes(y = avg7)) +
      scale_color_brewer(palette = "Spectral", direction = -1)
  })
})

test_that("risk_for_botrytis", {
  expect_silent({
    tibble(
      value = round(runif(100, 0, 4), 0),
      risk_for_botrytis(value)
    ) |>
      mutate(day = row_number()) |>
      ggplot(aes(x = day, color = risk, group = 1)) +
      geom_col(aes(y = value)) +
      geom_line(aes(y = total)) +
      scale_color_brewer(palette = "Spectral", direction = -1)
  })
})


# Model runners -----------------------------------------------------------

test_that("build_tar_spot", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_tar_spot() |>
      test_plot()
  })
})

test_that("build_gls", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_gray_leaf_spot() |>
      test_plot()
  })
})

test_that("build_don", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_don() |>
      test_plot()
  })
})

test_that("build_white_mold_dry", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_white_mold_dry() |>
      test_plot()
  })
})

test_that("build_white_mold_irrig", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_white_mold_irrig("30") |>
      test_plot()
  })

  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_white_mold_irrig("15") |>
      test_plot()
  })
})

test_that("build_frogeye_leaf_spot", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_frogeye_leaf_spot() |>
      test_plot()
  })
})

test_that("build_wheat_scab", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_wheat_scab(resistance = "VS") |>
      test_plot()
  })
})

test_that("build_early_blight", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_early_blight() |>
      test_plot()
  })
})

test_that("build_late_blight", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_late_blight() |>
      test_plot()
  })
})

test_that("build_alternaria", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_alternaria() |>
      test_plot()
  })
})

test_that("build_cercospora", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_cercospora() |>
      test_plot()
  })
})

test_that("build_botrytis", {
  expect_silent({
    test_hourly_wx |>
      build_daily() |>
      build_botrytis() |>
      test_plot()
  })
})
