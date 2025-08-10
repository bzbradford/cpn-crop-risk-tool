# Growing degree days ----

#' Single sine method
#' to create GDDs with an upper threshold, calculate GDDs with the upper threshold
#' as the base temperature and subtract that value from the GDDs for the base temp
#' to implement a horizontal cutoff.
#' @param tmin minimum daily temperature
#' @param tmax maximum daily temperature
#' @param base base/lower temperature threshold
#' @returns single sine growing degree days for one day
gdd_sine <- function(tmin, tmax, base) {
  mapply(function(tmin, tmax, base) {
    if (is.na(tmin) || is.na(tmax)) return(NA)

    # swap min and max if in wrong order for some reason
    if (tmin > tmax) { t = tmin; tmin = tmax; tmax = t }

    # min and max < lower
    if (tmax <= base) return(0)

    average = (tmin + tmax) / 2

    # tmin > lower = simple average gdds
    if (tmin >= base) return(average - base)

    # tmin < lower, tmax > lower = sine gdds
    alpha = (tmax - tmin) / 2
    base_radians = asin((base - average) / alpha)
    a = average - base
    b = pi / 2 - base_radians
    c = alpha * cos(base_radians)
    (1 / pi) * (a * b + c)
  }, tmin, tmax, base)
}

# expand_grid(tmin = 0:30, tmax = 0:30) %>%
#   filter(tmax >= tmin) %>%
#   mutate(gdd = gdd_sine(tmin, tmax, 10)) %>%
#   ggplot(aes(x = tmin, y = tmax, fill = gdd)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)
