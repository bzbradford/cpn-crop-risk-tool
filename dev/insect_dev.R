test_daily_wx

#' Map a value onto a sine wave with minima at `start` and maxima at `peak`
half_sine <- function(value, start, peak) {
  # normalize the input range to [0, 1]
  x = (value - start) / (peak - start)
  (sin(x * pi - pi / 2) + 1) / 2
}


#' Map a value onto a full sine wave passing through start, peak, and end
full_sine <- function(value, start, peak, end) {
  case_when(
    !between(value, start, end) ~ 0,
    value <= peak ~ half_sine(value, start, peak),
    value > peak ~ half_sine(-1 * value, -1 * end, -1 * peak),
    .default = 0
  )
}

# gdd_to_sev <- function(gdd, start, peak, end) {
#   tibble(
#     prp = full_sine(gdd, start, peak, end),
#     severity = prp * 4
#   ) |>
#     mutate(
#       # round up when between 0 and 1 otherwise round to nearest
#       severity = if_else(
#         severity > 0 & severity < 1,
#         1,
#         round(severity)
#       )
#     )
# }

tibble(
  x = 0:100,
  y = half_sine(x, 0, 100),
  sev = round(y * 4, 0)
) |>
  ggplot() +
  geom_area(aes(x, y, fill = sev), lwd = 4) +
  scale_fill_viridis_c()

tibble(
  x = 0:300,
  y = full_sine(x, 50, 100, 250) * 4,
  sev = if_else(
    between(y, 0, 0.5),
    1,
    round(y)
  )
) |>
  ggplot(aes(x, y)) +
  geom_line(lwd = 2) +
  geom_area(aes(fill = sev), lwd = 4) +
  scale_fill_viridis_c()

# define life stages by gdd
# each generation gets a little wider to account for variation
# 720 degree days between generations
if (FALSE) {
  scm_key <- tibble(
    stage = 1:7,
    start = 295,
    peak = 360,
    end = 810,
    mult = 1
  ) |>
    mutate(
      across(c(start, peak, end), ~ .x + 720 * (stage - 1)),
      mult = mult - 0.025 * (stage - 1),
      start = start - 0.025 * start * (stage - 1),
      end = end + 0.025 * end * (stage - 1),
      across(c(start, peak, end), round_to)
    ) |>
    mutate(
      label = paste(scales::label_ordinal()(stage), "generation"),
      .after = stage
    )

  print_tribble(scm_key)
}


expand_grid(gdd = 0:100, scm_key) |>
  mutate(prp = full_sine(gdd, start, peak, end)) |>
  filter(row_number() == min(row_number() | prp > 0), .by = gdd)

predict_insect <- function(gdd, stage_key) {
  tibble(day = row_number(), gdd = gdd) |>
    left_join(stage_key, join_by(gdd >= start, gdd <= end)) |>
    mutate(value = full_sine(gdd, start, peak, end) * coalesce(mult, 1)) |>
    summarize(
      across(c(stage, label), ~ paste(na.omit(.x), collapse = ", ")),
      across(value, max),
      .by = day
    ) |>
    select(-day)
}

tibble(gdd = seq(0, 4000)) |>
  mutate(predict_insect(gdd, scm_key)) |>
  ggplot(aes(x = gdd, y = value)) +
  geom_line() +
  geom_area(aes(fill = stage)) +
  geom_line(lwd = 2)


build_insect <- function(
  daily,
  tmin,
  tmax = 30,
  stage_key,
  severity_legend
) {
  req(nrow(daily) > 0)

  daily |>
    arrange(grid_id, date) |>
    mutate(
      date = date,
      frost = roll_sum(temperature_min <= 0, 28),
      freezing = roll_sum(temperature_min <= -2, 28),
      kill = (yday(date) > 250) * (frost > 1) * (freezing > 0),
      gdd = gdd_sine(temperature_min, temperature_max, tmin, tmax),
      cum_gdd = cumsum(gdd),
      predict_insect(cum_gdd, stage_key),
      value = value * (!kill),
      severity = round(value * 4),
      .by = c(grid_id, year),
      .keep = "used"
    ) |>
    left_join(severity_legend, join_by(severity))
}

test_daily_wx |>
  mutate(grid_id = sprintf("%.2f, %.2f", grid_lat, grid_lng)) |>
  build_insect(4, 30, scm_key, scm_legend) |>
  ggplot(aes(x = date, y = value)) +
  geom_area(aes(fill = severity)) +
  geom_line() +
  geom_line(aes(y = scale(temperature_min))) +
  facet_wrap(year(date) ~ grid_id, ncol = 1, scales = "free") +
  scale_fill_viridis_c()


# alfalfa weevil - single generation insect
alfalfa_key <- tribble(
  ~start , ~sev , ~label                      ,
     300 ,    1 , "Egg hatch, begin scouting" ,
     370 ,    2 , "2nd instar larvae"         ,
     440 ,    3 , "3rd instar larvae"         ,
     505 ,    4 , "4th instar larvae"         ,
     595 ,    2 , "Pupal stage, end scouting" ,
     815 ,    1 , "Adult emergence"           ,
    1000 ,    0 , "Adult diapause"            ,
)

# Seedcorn maggot
if (FALSE) {
  scm_legend <- tribble(
    ~sev , ~label                                 ,
       0 , "Absent or very low abundance"         ,
       1 , "Low abundance and risk of damage"     ,
       2 , "Medium abundance and risk of damage"  ,
       3 , "High abundance and risk of damage"    ,
       4 , "Peak adult flight and risk of damage" ,
  )

  # construct long-form scm key
  scm_key <- tribble(
    ~stage    , ~start , ~peak , ~end ,
    "1st gen" ,    295 ,   360 ,  810 ,
    "2nd gen" ,    990 ,  1080 , 1570 ,
    "3rd gen" ,   1650 ,  1800 , 2360 ,
  ) |>
    reframe(
      gdd = start:end,
      sev = ceiling(full_sine(gdd, start, peak, end) * 4),
      .by = stage
    ) |>
    filter(sev != lag(sev)) |>
    mutate(across(gdd, round_to)) |>
    left_join(scm_legend, join_by(sev)) |>
    mutate(label = sprintf("%s: %s", stage, label)) |>
    select(-stage) |>
    rename(start = gdd)

  print_tribble(scm_key)
}

scm_key <- tribble(
  ~start , ~sev , ~label                                          ,
     295 ,    1 , "1st gen: Low abundance and risk of damage"     ,
     315 ,    2 , "1st gen: Medium abundance and risk of damage"  ,
     330 ,    3 , "1st gen: High abundance and risk of damage"    ,
     340 ,    4 , "1st gen: Peak adult flight and risk of damage" ,
     510 ,    3 , "1st gen: High abundance and risk of damage"    ,
     585 ,    2 , "1st gen: Medium abundance and risk of damage"  ,
     660 ,    1 , "1st gen: Low abundance and risk of damage"     ,
     810 ,    0 , "1st gen: Absent or very low abundance"         ,
     990 ,    1 , "2nd gen: Low abundance and risk of damage"     ,
    1020 ,    2 , "2nd gen: Medium abundance and risk of damage"  ,
    1035 ,    3 , "2nd gen: High abundance and risk of damage"    ,
    1050 ,    4 , "2nd gen: Peak adult flight and risk of damage" ,
    1245 ,    3 , "2nd gen: High abundance and risk of damage"    ,
    1325 ,    2 , "2nd gen: Medium abundance and risk of damage"  ,
    1405 ,    1 , "2nd gen: Low abundance and risk of damage"     ,
    1570 ,    0 , "2nd gen: Absent or very low abundance"         ,
    1650 ,    1 , "3rd gen: Low abundance and risk of damage"     ,
    1700 ,    2 , "3rd gen: Medium abundance and risk of damage"  ,
    1725 ,    3 , "3rd gen: High abundance and risk of damage"    ,
    1750 ,    4 , "3rd gen: Peak adult flight and risk of damage" ,
    1985 ,    3 , "3rd gen: High abundance and risk of damage"    ,
    2080 ,    2 , "3rd gen: Medium abundance and risk of damage"  ,
    2175 ,    1 , "3rd gen: Low abundance and risk of damage"     ,
    2360 ,    0 , "3rd gen: Absent or very low abundance"         ,
)


# CPB
if (FALSE) {
  # construct long-form key
  cpb_key <- tribble(
    ~stage    , ~start , ~peak , ~end ,
    "1st gen" ,    265 ,   895 , 1115 ,
    "2nd gen" ,   1115 ,  1375 , 2500 ,
  ) |>
    reframe(
      gdd = start:end,
      sev = ceiling(full_sine(gdd, start, peak, end) * 4),
      .by = stage
    ) |>
    filter(sev != lag(sev)) |>
    mutate(across(gdd, round_to)) |>
    select(gdd, sev, label = stage) |>
    rename(start = gdd)

  print_tribble(cpb_key)
}


build_insect <- function(
  daily,
  tmin,
  tmax = 30,
  stage_key
) {
  req(nrow(daily) > 0)

  key <- bind_rows(
    tibble(start = 0, sev = 0, label = "Before spring emergence"),
    stage_key
  )

  print(key)

  daily |>
    arrange(grid_id, date) |>
    mutate(
      date = date,
      freezing = roll_sum(temperature_min <= -2, 14),
      kill = (yday(date) > 250) * (freezing > 0),
      gdd = gdd_sine(temperature_min, temperature_max, tmin, tmax),
      cum_gdd = cumsum(gdd),
      .by = c(grid_id, year),
      .keep = "used"
    ) |>
    left_join(key, join_by(cum_gdd >= start), multiple = "last") |>
    mutate(
      severity = pmax(0, sev - freezing),
      label = paste0(
        label,
        if_else(
          kill == 1,
          ". Recent freezing temperatures may reduce insect populations.",
          ""
        )
      )
    )
}

test_daily_wx |>
  build_insect(tmin = 9, stage_key = alfalfa_key) |>
  ggplot(aes(x = date, y = cum_gdd)) +
  geom_area(aes(fill = sev)) +
  geom_line() +
  geom_line(aes(y = scale(temperature_min))) +
  facet_wrap(year(date) ~ grid_id, ncol = 1, scales = "free") +
  scale_fill_viridis_c()

test_daily_wx |>
  build_insect(tmin = 4, stage_key = scm_key) |>
  ggplot(aes(x = date, y = cum_gdd)) +
  geom_area(aes(fill = severity)) +
  geom_line() +
  geom_line(aes(y = scale(temperature_min))) +
  facet_wrap(year(date) ~ grid_id, ncol = 1, scales = "free") +
  scale_fill_viridis_c()
