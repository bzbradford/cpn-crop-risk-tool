# tests for src/plotly.R

test_that("expand_range works", {
  # expands by default 5%
  expand_range(0, 1) |> expect_equal(c(-.05, 1.05))

  # expands by any amount
  expand_range(0, 1, .1) |> expect_equal(c(-.1, 1.1))
})

test_that("plotly_get_forecast_annot works", {
  plotly_get_forecast_annot(today() + 7) |>
    expect_type("list")
})

test_that("plotly_get_risk_period_annot works", {
  plotly_get_risk_period_annot(ymd("2025-7-1"), ymd("2025-8-15")) |>
    expect_type("list")
})

test_that("plot_risk works", {
  expect_silent({
    test_hourly_wx |>
      filter(grid_id == sample(grid_id, 1)) |>
      build_daily() |>
      build_frogeye_leaf_spot() |>
      plot_risk(name = "Frogeye")
  })
})
