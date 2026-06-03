om_test <- function(lat, lng) {
  req <- request("https://customer-archive-api.open-meteo.com/v1/archive") |>
    req_url_query(
      latitude = lat,
      longitude = lng,
      start_date = today(),
      end_date = today(),
      timezone = "auto",
      hourly = "temperature_2m",
      apikey = OPTS$open_meteo_key
    )
  resp <- req_perform(req) |>
    resp_body_json()
  tibble(grid_lat = resp$latitude, grid_lng = resp$longitude)
}

om_test(44.709444, -93.111389)

om_grid_resps <- expand_grid(
  lat = 44.709444 + seq(-0.1, 0.1, 0.025),
  lng = -93.111389 + seq(-0.1, 0.1, 0.025)
) |>
  reframe(om_test(lat, lng), .by = c(lat, lng))

om_grid_resps_2 <- expand_grid(
  lat = 44.709444 + seq(-0.1, 0.1, 0.01),
  lng = -93.111389 + seq(-0.1, 0.1, 0.01)
) |>
  reframe(om_test(lat, lng), .by = c(lat, lng))

om_grid_resps_2 |>
  mutate(grid_id = paste(grid_lat, grid_lng) |> fct_shuffle()) |>
  leaflet() |>
  addTiles() |>
  addRectangles(
    lat1 = ~ lat - 0.005,
    lat2 = ~ lat + 0.005,
    lng1 = ~ lng - 0.005,
    lng2 = ~ lng + 0.005,
    fillColor = ~ colorFactor("viridis", grid_id)(grid_id),
    fillOpacity = 1
  ) |>
  addCircleMarkers(
    lat = ~grid_lat,
    lng = ~grid_lng,
    fillColor = ~ colorFactor("viridis", grid_id)(grid_id)
  )
