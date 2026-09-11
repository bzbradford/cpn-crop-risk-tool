# tests for src/plotly.R

test_that("expand_range works", {
  # expands by default 5%
  expand_range(c(0, 1)) |> expect_equal(c(-.05, 1.05))

  # expands by any amount
  expand_range(c(0, 1), 0.1) |> expect_equal(c(-.1, 1.1))
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
      rename(model_value = probability) |>
      plot_risk(
        name = "Frogeye"
      )
  })
})

test_that("plot_risk works with group", {
  plt <- test_hourly_wx |>
    filter(grid_id == sample(grid_id, 1)) |>
    build_daily() |>
    build_soybean_cercospora() |>
    plot_risk(
      name = "Cercospora",
      ycol = "probability",
      group = "species"
    )

  expect_s3_class(plt, "plotly")
  # base attrs entry from plot_ly() + one add_trace() per species
  expect_length(plt$x$attrs, 4)
})

test_that("plot_risk plots additional ycols on hidden axes", {
  plt <- test_daily_wx |>
    filter(grid_id == first(grid_id)) |>
    build_frogeye_leaf_spot() |>
    plot_risk(
      name = "Frogeye",
      ycol = c(
        "probability",
        "temperature_max_30day",
        "hours_rh_over_80_30day"
      ),
      unit_system = "imperial"
    )

  expect_s3_class(plt, "plotly")
  # base attrs + scatter + bar + one trace per additional column
  expect_length(plt$x$attrs, 5)

  built <- plotly_build(plt)$x
  expect_false(built$layout$yaxis2$visible)
  expect_equal(built$layout$yaxis3$overlaying, "y")

  # additional columns are unit converted and labeled
  temp_trace <- Filter(
    \(t) identical(t$name, fmt_plot_names("temperature_max_30day")),
    built$data
  )
  expect_length(temp_trace, 1)
  expect_match(temp_trace[[1]]$text[1], "°F")
})

test_that("plot_risk breaks lines at missing values in additional ycols", {
  df <- test_daily_wx |>
    filter(grid_id == first(grid_id)) |>
    build_late_blight()
  skip_if_not(anyNA(df$temperature_mean_rh_over_90))

  plt <- plot_risk(
    df,
    name = "Late blight",
    ycol = c("severity", "temperature_mean_rh_over_90")
  )

  # base attrs + scatter + bar + line + missing value markers
  expect_length(plt$x$attrs, 5)
  expect_true(anyNA(plt$x$attrs[[4]]$y))
  expect_false(anyNA(plt$x$attrs[[5]]$y))
})

test_that("plot_risk warns and skips missing ycols", {
  df <- test_daily_wx |>
    filter(grid_id == first(grid_id)) |>
    build_frogeye_leaf_spot()

  expect_warning(
    plt <- plot_risk(df, name = "Frogeye", ycol = c("probability", "foo")),
    "foo"
  )
  # base attrs + scatter + bar, no additional traces
  expect_length(plt$x$attrs, 3)

  # nothing to plot without the primary column
  expect_warning(
    plt <- plot_risk(df, name = "Frogeye", ycol = c("foo", "probability")),
    "foo"
  )
  expect_null(plt)
})
