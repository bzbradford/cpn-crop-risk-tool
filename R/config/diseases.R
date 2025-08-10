# Crop and Disease definitions -------------------------------------------------

Disease <- function(name, info, doc) {
  stopifnot(doc %in% list.files(pattern = "*.md", recursive = TRUE))
  list(name = name, info = info, doc = doc)
}

diseases <- list(
  # Corn
  tar_spot = Disease(
    name = "Tar spot",
    info = "Corn is susceptible to tar spot when in the growth stages V10-R3. Risk is based on proability of spore presence.",
    doc = "docs/tar-spot.md"
  ),
  gray_leaf_spot = Disease(
    name = "Gray leaf spot",
    info = "Corn is susceptible to gray leaf spot when in the growth stages V10-R3. Risk is based on probability of spore presence.",
    doc = "docs/gray-leaf-spot.md"
  ),
  don = Disease(
    name = "Giberella ear rot/DON",
    info = "Corn is susceptible to Giberella ear rot during silking. Infection by this disease may lead to deoxynivalenol (DON) accumulation in the ear to dangerous levels. Risk is based on the probability of deoxynivalenol exceeding 1ppm in harvested grain and silage.",
    doc = "docs/don.md"
  ),

  # Soybean
  white_mold = Disease(
    name = "White mold",
    info = "Soybean is vulnerable to white mold when in the growth stages R1-R3 (flowering). Risk is based on probability of spore presence.",
    doc = "docs/white-mold.md"
  ),
  frogeye = Disease(
    name = "Frogeye leaf spot",
    info = "Corn is vulnerable to tar spot when in the growth stages V10-R3. Risk is based on probability of spore presence.",
    doc = "docs/frogeye.md"
  ),

  # Solanum
  early_blight = Disease(
    name = "Early blight",
    info = "Early blight may affect potato, tomato, pepper, eggplant, and other Solanaceous plants. Risk depends on the number of potato physiological days (P-days) accumulated since crop emergence.",
    doc = "docs/early-blight.md"
  ),
  late_blight = Disease(
    name = "Late blight",
    info = "Late blight may affect potato, tomato, pepper, eggplant, and other Solanaceous plants. Risk depends on the number of disease severity values generated in the last 14 days and since crop emergence.",
    doc = "docs/late-blight.md"
  ),

  # Carrot
  alternaria = Disease(
    name = "Alternaria leaf blight",
    info = "Carrots are susceptible to Alternaria leaf blight. Risk depends on the number of disease severity values generated in the last 7 days.",
    doc = "docs/alternaria.md"
  ),

  # Carrot + Beet
  cercospora = Disease(
    name = "Cercospora leaf spot",
    info = "Carrots and beets are susceptible to Cercospora leaf blight. Risk depends on the average disease severity values in the past 2 days and 7 days.",
    doc = "docs/cercospora.md"
  ),

  # Onion
  botrytis = Disease(
    name = "Botrytis leaf blight",
    info = "Onions are susceptible to Botrytis leaf blight. Risk depends on cumulative disease severity values since crop emergence.",
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
      diseases$alternaria,
      diseases$cercospora
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
