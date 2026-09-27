
## Red Crown Rot (Soybean)

**Red crown rot** is caused by the soilborne fungus *Calonectria ilicicola* (anamorph *Cylindrocladium parasiticum*). Long known as an important disease of soybean and peanut (where it is called Cylindrocladium black rot) in Japan and the southeastern United States, red crown rot has more recently been confirmed in soybean fields in Illinois, Kentucky, Indiana, Missouri, Tennessee, and other states in the Midwest and Mid-South.

The pathogen can infect soybean roots as early as two weeks after planting, but symptoms usually do not appear until after flowering (R1). The most characteristic sign is a reddish-brown to brick-red discoloration of the lower stem and crown near the soil line, often accompanied by clusters of tiny red-orange fruiting bodies (perithecia) on the lower stem. Roots may be rotted and the taproot discolored. Leaves of infected plants often show interveinal chlorosis and necrosis that closely resemble sudden death syndrome, brown stem rot, or stem canker, so examining the lower stem for red discoloration and perithecia is important for diagnosis. Severely affected plants may defoliate and die prematurely, reducing yield.

*C. ilicicola* survives in soil and crop residue as microsclerotia, which can persist for many years. The disease tends to recur and increase in fields where it has been found before, and spreads between fields with soil movement on equipment. Host range includes soybean, peanut, alfalfa, and several other legumes. Management options are limited: no soybean variety is completely resistant, although varieties differ in susceptibility. Rotation to non-host crops such as corn or small grains, delaying planting, cleaning soil from equipment, and seed treatments with activity against *C. ilicicola* may reduce disease. Foliar fungicides are not effective against this soilborne disease.

### Model details

This model predicts the field-level incidence of red crown rot (percent of plants showing symptoms) from weather around the beginning of flowering (R1). It is based on logistic regression models developed by Ochi et al. (2026) from surveys of 242 soybean fields in northern Japan. Three weather variables are used:

- **Mean air temperature** during the 31 days beginning on R1. Higher temperatures (above about 26°C / 79°F) are associated with higher incidence.
- **Total precipitation** during the 31 days before R1 (vegetative stage). Wetter conditions (above about 200 mm / 8 in) are associated with higher incidence.
- **Total precipitation** during the 31 days beginning on R1 (reproductive stage). Drier conditions (below about 100 mm / 4 in) are associated with higher incidence.

Each variable predicts incidence separately, and the three predictions are combined into a weighted average based on how well each variable explained disease incidence (temperature 66%, vegetative precipitation 16%, reproductive precipitation 18%). Set the start date to the R1 date for your field. Until 31 days after R1, the prediction uses the mean temperature so far and projects precipitation to a full 31 days based on the average daily rainfall so far.

Provisional risk categories: Very low (less than 5% incidence), Low (5-30%), Moderate (30-60%), and High (60% or greater). Incidence above 60% has been associated with considerable yield loss. These thresholds have not been validated in the United States. Past history of red crown rot in a field was the strongest predictor of disease in the original study, and is not included in this model.

### References

- Ochi, S., Kishi, S., Kawaguchi, A., and Akamatsu, H. 2026. Meteorological and agronomic factors analysis on the field-level incidence of red crown rot of soybean. Phytopathology 116:1405-1413. <https://doi.org/10.1094/PHYTO-06-25-0207-R>
- Crop Protection Network Encyclopedia: <https://cropprotectionnetwork.org/encyclopedia/red-crown-rot-of-soybean>
