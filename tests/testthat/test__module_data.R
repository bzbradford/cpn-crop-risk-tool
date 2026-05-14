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

test_that("build_gdd_from_daily", {
  expect_silent({
    test_daily_wx |>
      build_gdd_from_daily()
  })
})
