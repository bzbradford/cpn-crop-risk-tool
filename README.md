# Crop Risk and Weather Forecasting Tool

This app is designed to provide an easy interface to view and download hourly weather data for any point in the United States and Canada below the 60°N latitude. Data is derived from a subscription to [IBM's Environmental Intelligence Suite](https://www.ibm.com/products/environmental-intelligence), with a spatial resolution of 1/24th decimal degree (approximately 3 km). 7-day hourly forecasts are sourced from [NOAA](https://www.weather.gov/documentation/services-web-api).

### How to use the app

1.  **Define one or more sites of interest.** Sites can be added by uploading a CSV from the sidebar, by clicking on the map, by searching for a place name in the search bar at the lower left of the map, or by entering GPS coordinates in the search bar at the lower right of the map. Your site list can be saved to a CSV, but it is also saved in a browser cookie and restored when revisiting the app, unless the cookie has been deleted.
2.  **Fetch weather data.** Once you have one more sites defined, click the big "Fetch Weather" button to load data from the IBM service. If you have already downloaded data there may be some data available in the app, but it may be missing some recent weather data. Weather fetching will also occur automatically after a delay.
3.  **View crop risk model results.** Use the "Crop risk models" tab to select a crop and risk model to display in the charts feed and on the map.
4.  **Explore weather data.** Use the "Charts and data" tab to explore hourly, daily, and other derived weather variables on a chart. This weather data can also be downloaded from this tab.

### Weather datasets

Hourly weather parameters include:

-   Air temperature
-   Dew point
-   Relative humidity
-   Precipitation and snow accumulation
-   Wind speed and direction
-   Atmospheric pressure

In addition to hourly weather, derived datasets are generated including:

-   Daily minimum, maximum, and average values for each parameter
-   Moving averages on a 7, 14, 21, and 30-day basis
-   Daily and cumulative growing degree day calculations for common models
-   Model predictions for certain field and vegetable crop diseases

These data can be retrieved for individual locations, or multiple locations can be specified for batch downloading and comparison.

#### Hourly data

Hourly data includes the timestamp in UTC and an adjustment to local time based on the timezone associated with the GPS coordinates. Hourly weather parameters include air temperature, dew point, dew point depression (difference between air temperature and dew point), relative humidity, precipitation, snow accumulation, wind speed, wind gusts, wind direction, barometric pressure (mean sea level), and pressure change since the previous hour.

Note: A wind gust is defined as a brief increase in wind speed that is at least 10 mph (16 km/h) faster than the average wind speed and peaks above 18 mph (30 km/h). Due to these definitions not every hour or day will have recorded wind gusts.

#### Hourly weather forecast

When the End Date is set to today's date, 14-day forecasts are retrieved from [Open Meteo](https://open-meteo.com/en/docs) and incorporated into charts and model calculations. Open Meteo sources weather forecasts from multiple governmental sources to ensure broad coverage.

#### Daily data

For each hourly weather parameter, the minimum, mean, and maximum value are generated. In addition, the total daily value is generated when appropriate (precipitation and snow accumulation).

#### Moving averages

7, 14, 21, and 30-day moving averages are calculated for each daily value using the `roll_apply` function from the `zoo` package. Either centered or right-aligned (trailing) moving average types are available.

#### Growing degree days

The single sine method is used to calculate growing degree days from daily minimum and maximum air temperature values. For each base temperature, a model is provided with and without the common 86°F upper threshold temperature (horizontal cutoff). The single sine method differs from the simple average method only when the minimum temperature is below the lower threshold temperature, or the maximum temperature is above the upper threshold temperature. In such cases, the single sine method will more accurately reflect the amount of heat energy available, relative to the simple average method.

#### Units

Most values can be shown in either imperial or metric units.

-   Temperature and dew point: °C or °F
-   Relative humidity: %
-   Precipitation (rain/melted snow): mm or in
-   Snow accumulation: cm or in
-   Atmospheric pressure: mbar or inHg
-   Wind speed: km/h or mph
-   Wind direction: compass degrees (N=0°, E=90°, etc.)
-   Growing degree day base/upper thresholds and accumulations always in Fahrenheit

### Crop risk models

Selected field crops and vegetable disease model outputs are provided. These models are subject to change. The calculations used to generate each model prediction can be viewed in the source code.

-   Field crops diseases
    -   White mold (soybean): <https://cropprotectionnetwork.org/encyclopedia/white-mold-of-soybean>
    -   Frogeye leaf spot (soybean): <https://cropprotectionnetwork.org/encyclopedia/frogeye-leaf-spot-of-soybean>
    -   Gray leaf spot (corn): <https://cropprotectionnetwork.org/encyclopedia/gray-leaf-spot-of-corn>
    -   Tarspot (corn): <https://cropprotectionnetwork.org/encyclopedia/tar-spot-of-corn>
    -   Giberella ear rot (corn): <https://cropprotectionnetwork.org/encyclopedia/gibberella-ear-rot-of-corn>
-   Vegetable crop diseases
    -   Early blight (potato/tomato): <https://vegpath.plantpath.wisc.edu/diseases/potato-early-blight/>
    -   Late blight (potato/tomato): <https://vegpath.plantpath.wisc.edu/diseases/potato-late-blight/>
    -   Alternaria and Cercospora leaf blights (carrot): <https://vegpath.plantpath.wisc.edu/diseases/carrot-alternaria-and-cercospora-leaf-blights/>
    -   Cercospora leaf spot (beet): <https://www.vegetables.cornell.edu/pest-management/disease-factsheets/cercospora-leaf-spot-of-table-beet/>
    -   Botrytis leaf blight (onion): <https://vegpath.plantpath.wisc.edu/diseases/onion-botrytis/>

### Cropland data layer

Optionally, the [Cropland Data Layer](https://www.nass.usda.gov/Research_and_Science/Cropland/SARS1a.php) (CDL) can be shown as an overlay on the map by expanding the layers control in the upper right of the map and enabling a CDL overlay. This map is generated annually from satellite observations and represents the best estimate of the land cover class (eg water, forest, cultivated crop) grown at each pixel location. Dataset resolution has been 30 meters until 2024 when it increased to 10 meters. [Click here](https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php) for frequently asked questions about the CDL. [Click here](https://www.nass.usda.gov/Research_and_Science/Cropland/docs/US_2024_CDL_legend.jpg) for a legend showing the display color used for each crop. Non-crop landcover is intuitively colored (eg forest is green, grass/shrubland is lighter green, water is blue, urban is grey). Crop colors vary but corn is yellow, soybean is green, potato is maroon. The CDL may also be explored on official websites such as [CroplandCROS](https://croplandcros.scinet.usda.gov/) and [Cropscape](https://nassgeodata.gmu.edu/CropScape/).

### Credits and Contacts

-   Ben Bradford (Developer, Data Scientist, Entomologist): [bbradford\@wisc.edu](mailto:bbradford@wisc.edu)
-   Damon Smith (Extension Field Crops Pathologist): [damon.smith\@wisc.edu](mailto:damon.smith@wisc.edu)
-   Amanda Gevens (Extension Vegetable Crops Pathologist): [gevens\@wisc.edu](mailto:gevens@wisc.edu)
-   Crop Protection Network: [info\@cropprotectionnetwork.org](mailto:info@cropprotectionnetwork.org)

### Funding Sources

This project is partial supported by the National Predictive Modeling Tool Initiative, National Corn Growers Association, North Central Soybean Research Program, and United Soybean Board. This tool also relies on contributions from the University of Wisconsin-Madison Department of Entomology.
