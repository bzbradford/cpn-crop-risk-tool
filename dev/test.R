test_wx <- read_csv("dev/test_wx.csv")
build_daily(test_wx)

hourly <- test_wx |>
  rename_with(~ paste0(., "_hourly"), temperature:last_col())

date_time_link <<- hourly |>
  summarize(
    # get the middle datetime for each date
    datetime_utc = quantile(datetime_utc, probs = 0.5, type = 1),
    .by = c(grid_id, date)
  )

wx_daily <- build_daily(test_wx)
ma_right <- build_ma_from_daily(wx_daily)
gdd <- build_gdd_from_daily(wx_daily)

daily <- date_time_link |>
  left_join(wx_daily) |>
  left_join(ma_right) |>
  left_join(gdd) |>
  rename_with(~ paste0(., "_daily"), temperature_min:last_col())
# daily <- wx_daily() |>
#   left_join(ma_right()) |>
#   left_join(gdd()) |>
#   rename_with(~ paste0(., "_daily"), temperature_min:last_col())

hourly |>
  left_join(daily) |>
  view()
