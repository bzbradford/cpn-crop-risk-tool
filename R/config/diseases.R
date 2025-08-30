# Crop and Disease definitions -------------------------------------------------

#' @param name display name
#' @param info model info
#' @param doc markdown file for More Information
#' @param risk_period NULL or length two character vector eg 'Jul 1'
Disease <- function(name, info, doc, risk_period = NULL, ...) {
  stopifnot(
    "Invalid parameter provided" = length(list(...)) == 0,
    "Missing doc file" = doc %in% list.files(pattern = "*.md", recursive = TRUE)
  )

  # check that all risk period dates are valid
  if (!is.null(risk_period)) {
    withCallingHandlers({
      ymd(paste(year(Sys.Date()), risk_period))
    },
      warning = function(w) stop("Invalid date format for risk_period: ", risk_period)
    )
  }

  list(name = name, info = info, doc = doc, risk_period = risk_period)
}

diseases <- list(
  # Corn
  tar_spot = Disease(
    name = "Tar spot",
    info = HTML("<b>Corn is susceptible to tar spot when in the growth stages V10-R3 (10th leaf - milk).</b> Risk is based on probability of spore presence. Model depends on temperature and relative humidity."),
    doc = "docs/tar-spot.md",
    risk_period = c("Jul 1", "Aug 15")
  ),
  gray_leaf_spot = Disease(
    name = "Gray leaf spot",
    info = HTML("<b>Corn is susceptible to gray leaf spot when in the growth stages V10-R3 (10th leaf - milk)</b>. Risk is based on probability of spore presence. Model depends on minimum temperature and dew point."),
    doc = "docs/gray-leaf-spot.md",
    risk_period = c("Jul 1", "Aug 15")
  ),
  don = Disease(
    name = "Giberella ear rot/DON",
    info = HTML("<b>Corn is susceptible to Giberella ear rot during silking.</b> Infection by this disease may lead to deoxynivalenol (DON) accumulation in the ear to dangerous levels. Risk is based on the probability of deoxynivalenol exceeding 1ppm in harvested grain and silage. Model depends on temperature, precipitation, and relative humidity during the 3 weeks prior to silking."),
    doc = "docs/don.md",
    risk_period = c("Jul 15", "Aug 7")
  ),

  # Soybean
  white_mold = Disease(
    name = "White mold",
    info = HTML("<b>Soybean is vulnerable to white mold when in the growth stages R1-R3 (flowering - beginning pod).</b> Risk is based on probability of spore presence. Model depends on 30-day moving average maximum temperature, relative humidity, and wind speed (non-irrigated model only)."),
    doc = "docs/white-mold.md",
    risk_period = c("Jun 15", "Aug 7")
  ),
  frogeye = Disease(
    name = "Frogeye leaf spot",
    info = HTML("<b>Soybean is vulnerable to frogeye leaf spot when in the growth stages R1-R5 (flowering - beginning seed).</b> Risk is based on probability of spore presence. Model depends on 30-day moving average maximum temperature and daily hours of high humidity."),
    doc = "docs/frogeye.md",
    risk_period = c("Jun 15", "Sep 7")
  ),

  # Solanum
  early_blight = Disease(
    name = "Early blight",
    info = "Early blight may affect potato, tomato, pepper, eggplant, and other Solanaceous plants. Risk depends on the number of potato physiological days (P-days) accumulated since crop emergence, which are generated based on daily min/max temperatures.",
    doc = "docs/early-blight.md"
  ),
  late_blight = Disease(
    name = "Late blight",
    info = "Late blight may affect potato, tomato, pepper, eggplant, and other Solanaceous plants. Risk depends on the number of disease severity values generated in the last 14 days and since crop emergence. Model depends on temperature and hours of high humidity.",
    doc = "docs/late-blight.md"
  ),

  # Carrot
  alternaria = Disease(
    name = "Alternaria/Cercospora leaf blight",
    info = "Alternaria and Cercospora leaf blights are a common fungal disease of carrot leaves and petioles. Risk depends on the number of disease severity values generated in the last 7 days. Model depends on temperature and hours of high humidity.",
    doc = "docs/alternaria.md"
  ),

  # Carrot + Beet
  cercospora = Disease(
    name = "Cercospora leaf spot",
    info = "Cercospora leaf spot is a damaging fungal disease affecting beets. Risk depends on the average disease severity values in the past 2 days and 7 days. Model depends on temperature and hours of high humidity.",
    doc = "docs/cercospora.md"
  ),

  # Onion
  botrytis = Disease(
    name = "Botrytis leaf blight",
    info = "Onions are susceptible to Botrytis leaf blight. Risk depends on cumulative disease severity values since crop emergence. Model depends on temperature, hours of high humidity, and precipitation.",
    doc = "docs/botrytis.md"
  )
)

# set names as $slug
diseases <- imap(diseases, function(disease, slug) {
  disease$slug <- slug
  disease
})


Crop <- function(name, diseases) {
  list(name = name, diseases = diseases)
}

crops <- list(
  corn = Crop(
    name = "Corn",
    diseases = list(
      diseases$tar_spot,
      diseases$gray_leaf_spot,
      diseases$don
    )
  ),
  soybean = Crop(
    name = "Soybean",
    diseases = list(
      diseases$white_mold,
      diseases$frogeye
    )
  ),
  potato = Crop(
    name = "Potato/tomato",
    diseases = list(
      diseases$early_blight,
      diseases$late_blight
    )
  ),
  carrot = Crop(
    name = "Carrot",
    diseases = list(
      diseases$alternaria
    )
  ),
  beet = Crop(
    name = "Beet",
    diseases = list(
      diseases$cercospora
    )
  ),
  onion = Crop(
    name = "Onion",
    diseases = list(
      diseases$botrytis
    )
  )
)

# set names as $slug
crops <- imap(crops, function(crop, slug) {
  crop$slug <- slug
  crop
})
