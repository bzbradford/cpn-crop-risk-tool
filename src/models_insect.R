# Functions ----

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

build_insect <- function(
  daily,
  tmin,
  tmax,
  stage_key
) {
  req(nrow(daily) > 0)

  key <- bind_rows(
    tibble(start = 0, sev = 0, label = "Before spring emergence"),
    stage_key
  )

  daily |>
    arrange(grid_id, date) |>
    mutate(
      date = date,
      freezing = roll_sum(temperature_min <= -2, 14),
      kill = (yday(date) > 250) * (freezing > 0),
      gdd_f = gdd_sine(temperature_min, temperature_max, tmin, tmax) * 1.8,
      cum_gdd = cumsum(gdd_f),
      .by = c(grid_id, year),
      .keep = "used"
    ) |>
    left_join(key, join_by(cum_gdd >= start), multiple = "last") |>
    mutate(
      severity = pmax(0, sev - freezing),
      risk_from_severity(severity),
      value_label = paste0(
        sprintf("%.0f gdd (+%.0f)\n", cum_gdd, gdd_f),
        label,
        if_else(
          kill == 1,
          ". Recent freezing temperatures may reduce insect populations.",
          ""
        )
      )
    )
}

# make sure param$start_date == biofix
validate_biofix <- function(biofix) {
  force(biofix)

  return(function(params) {
    sd <- params$start_date
    if (yday(sd) != biofix) {
      biofix_date <- make_date(year(sd)) + biofix - 1
      paste(
        "This model requires the start date to be",
        format(biofix_date, "%b %d")
      )
    } else {
      NULL
    }
  })
}

if (FALSE) {
  validate_biofix(1)(list(start_date = today()))
}

# Insect defs ------------------------------------------------------------------

Insect <- function(
  name,
  crop = NULL,
  info,
  doc,
  biofix = 1,
  tmin,
  tmax = 30,
  key
) {
  m <- Model(
    name = name,
    crop = crop,
    group = "insect",
    info = info,
    doc = doc,
    risk_period = NULL,
    biofix = biofix,
    validate = validate_biofix(biofix),
    ycol = "cum_gdd",
    yrange = c(0, NA)
  )
  stopifnot(tmin < tmax)
  stopifnot(setequal(names(key), c("start", "sev", "label")))
  m$tmin <- tmin
  m$tmax <- tmax
  m$key <- key
  m
}

insect_models <- list(
  scm = Insect(
    name = "Seedcorn maggot",
    crop = "Corn, bean, other",
    info = "In Wisconsin there are typically 3-5 generations per year, with maximum risk coinciding with peak adult flight times: First (overwintering) generation flight peaks around 360 FDD, second generation flight peaks around 1080 FDD, third generation peaks around 1800 FDD. Wait 450 FDD after peak flight for larval pupation and minimum risk to crops.",
    doc = "docs/insects/scm.md",
    tmin = 4,
    tmax = 30,
    key = tribble(
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
  ),
  afw = Insect(
    "Alfalfa weevil",
    crop = "Alfalfa",
    info = "Alfalfa weevil egg hatch begins around 300 FDD. Light feeding damage expected during 1st and 2nd instar life stages (350-500 FDD). Heavy feeding damage expected during 3rd and 4th instar development, approx. 400-600 FDD.",
    doc = "docs/insects/alfalfa-weevil.md",
    tmin = 9,
    tmax = 30,
    key = tribble(
      ~start , ~sev , ~label                      ,
         300 ,    1 , "Egg hatch, begin scouting" ,
         370 ,    2 , "2nd instar larvae"         ,
         440 ,    3 , "3rd instar larvae"         ,
         505 ,    4 , "4th instar larvae"         ,
         595 ,    2 , "Pupal stage, end scouting" ,
         815 ,    1 , "Adult emergence"           ,
        1000 ,    0 , "Adult diapause"            ,
    )
  ),
  cpb = Insect(
    "Colorado Potato Beetle",
    crop = "Potato",
    info = "Our Colorado potato beetle risk model was derived from 10 years of scouting data in the Central Sands and uses a base 50°F degree day model. The indicated risk score represents the estimated total abundance of adults and larvae. First adult colonization of crops is typically around 265 FDD with peak first-generation populations observed around 895 FDD. Total populations decline slightly through 1115 FDD during the period between generations. Populations peak again around 1375 FDD during second generation adult emergence and decline through the end of the season.",
    doc = "docs/insects/cpb.md",
    tmin = 10,
    tmax = 30,
    key = tribble(
      ~start , ~sev , ~label                            ,
         200 ,    1 , "1st gen adult colonization"      ,
         350 ,    2 , "1st gen egg hatch"               ,
         600 ,    3 , "1st gen peak adults"             ,
         800 ,    4 , "1st gen peak larvae"             ,
         935 ,    2 , "1st gen pupation"                ,
        1100 ,    3 , "2nd gen adult emergence"         ,
        1470 ,    4 , "2nd gen peak adults"             ,
        1800 ,    3 , "2nd gen adult feeding continues" ,
        1970 ,    2 , "2nd gen adults begin dispersing" ,
        2265 ,    1 , "2nd gen adults mostly dispersed" ,
    )
  )
)
