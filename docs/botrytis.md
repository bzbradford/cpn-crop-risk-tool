
## Botrytis Leaf Blight (Onion)

![](botrytis-onion.png)

**Onion Botrytis leaf blight/leaf spot** is a fungal disease of alliums caused by *Botrytis squamosa*. Symptoms first appear as small whitish spots on the leaf. These spots are oval-shaped, and sometimes surrounded by a light green or silver halo that often appears water-soaked. Leaf tips will begin to dry and wither as the disease progresses, sometimes until the whole leaf dies back. Progressed infection can stunt bulb growth and reduce yield. Heavily infected fields often appear yellowish and blighted. Severe infection can stunt bulb growth and reduce yield.

Primary infection occurs from *B. squamosa* spores that overwinter in infected in-field plant debris, cull piles, stored bulbs, volunteer bulbs in-field, and in infected soil. Secondary infection can occur when conidia spores spread from moist, infected leaves. Favorable conditions for disease development include high relative humidity and rainfall, prolonged leaf wetness, and warm temperatures.

### Model details

The Botrytis leaf blight model is based on the BOTCAST algorithm described in Sutton et al 1986. The model describes a two-step algorithm, where the first determines if inoculum is likely to be present or not, and the second estimates the risk of infection when potentially present.

The model adds up daily severity index values (DSI) from crop emergence. UW Vegetable Pathology's guidance on the season total: 21 to 30 (Moderate) is a warning, and no spray is needed unless rain is forecast or you irrigate overhead; 31 to 40 (High) means a high risk of rapid disease development, so apply the first spray as soon as possible; above 40 (Very high) the risk is extremely high. BOTCAST times the first spray only; after that, scout for leaf spots. The line between Very low (10 or less) and Low (11 to 20) is this tool's, not BOTCAST's or UW's. Hours with relative humidity at or above 90 % stand in for leaf wetness; onion fields on muck soils or under overhead irrigation may stay wet longer than the weather data show.

Updated November 2026: humid hours and their mean temperature now come from the same window (from 8 pm the evening before).

### References

-   BOTCAST model: Sutton et al 1986 [https://doi.org/10.1016/0167-8809(86)90136-2](https://doi.org/10.1016/0167-8809(86)90136-2){.uri}
-   More information about onion Botrytis leaf blight: <https://vegpath.plantpath.wisc.edu/diseases/onion-botrytis/>
