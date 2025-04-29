# Crop Risk and Weather Forecasting Tool

This app is designed to provide an easy interface to view and download hourly weather data for any point in the continental United States. Data is derived from a subscription to [IBM's Environmental Intelligence Suite](https://www.ibm.com/products/environmental-intelligence), with a spatial resolution of 1/24th decimal degree (approximately 3 km). 7-day hourly forecasts are sourced from [NOAA](https://www.weather.gov/documentation/services-web-api). From this hourly weather data, we compute daily values, moving averages, growing degree days, and selected plant disease risk values.

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

### Hourly data

Hourly data includes the timestamp in UTC and an adjustment to local time based on the timezone associated with the GPS coordinates. Hourly weather parameters include air temperature, dew point, dew point depression (difference between air temperature and dew point), relative humidity, precipitation, snow accumulation, wind speed, wind gusts, wind direction, barometric pressure (mean sea level), and pressure change since the previous hour.

Note: A wind gust is defined as a brief increase in wind speed that is at least 10 mph (16 km/h) faster than the average wind speed and peaks above 18 mph (30 km/h). Due to these definitions not every hour or day will have recorded wind gusts.

### 7-day hourly forecast

When the End Date is set to today's date, 7-day forecasts are retrieved from [NOAA's API](https://www.weather.gov/documentation/services-web-api) (available for most locations in the United States) and incorporated into charts and calculations. Forecasted parameters include temperature, dew point, relative humidity, wind speed, and wind direction.

### Daily data

For each hourly weather parameter, the minimum, mean, and maximum value are generated. In addition, the total daily value is generated when appropriate (precipitation and snow accumulation).

### Moving averages

7, 14, 21, and 30-day moving averages are calculated for each daily value using the `roll_apply` function from the `zoo` package. Either centered or right-aligned (trailing) moving average types are available. These moving averages will use *up to* the window size, so if for example the centered 30-day moving average is desired for a particular date, weather should be downloaded for at least 15 days on either side of the desired date or date range.

### Growing degree days

The single sine method is used to calculate growing degree days from daily minimum and maximum air temperature values. For each base temperature, a model is provided with and without the common 86°F upper threshold temperature (horizontal cutoff). The single sine method differs from the simple average method only when the minimum temperature is below the lower threshold temperature, or the maximum temperature is above the upper threshold temperature. In such cases, the single sine method will more accurately reflect the amount of heat energy available, relative to the simple average method.

### Units

Most values can be shown in either imperial or metric units.

-   Temperature and dew point: °C or °F
-   Relative humidity: %
-   Precipitation (rain/melted snow): mm or in
-   Snow accumulation: cm or in
-   Atmospheric pressure: mbar or inHg
-   Wind speed: km/h or mph
-   Wind direction: compass degrees (N=0°, E=90°, etc.)
-   Growing degree day base/upper thresholds and accumulations always in Fahrenheit

### Plant disease models

Selected field crops and vegetable disease model outputs are provided. These models are subject to change. The calculations used to generate each model prediction can be viewed in the source code.

-   Field crops diseases
    -   White mold: <https://cropprotectionnetwork.org/news/smartphone-application-to-forecast-white-mold-in-soybean-now-available-to-growers>
    -   Frogeye leaf spot: <https://cropprotectionnetwork.org/encyclopedia/frogeye-leaf-spot-of-soybean>
    -   Gray leaf spot: <https://cropprotectionnetwork.org/encyclopedia/gray-leaf-spot-of-corn>
    -   Tarspot: <https://cropprotectionnetwork.org/encyclopedia/tar-spot-of-corn>
-   Vegetable crop diseases
    -   Early blight: <https://vegpath.plantpath.wisc.edu/diseases/potato-early-blight/>
    -   Late blight: <https://vegpath.plantpath.wisc.edu/diseases/potato-late-blight/>
    -   Alternaria leaf blight: <https://vegpath.plantpath.wisc.edu/diseases/carrot-alternaria-and-cercospora-leaf-blights/>
    -   Cercospora leaf spot daily infection values - risk of disease increases with accumulated values. Based on the model outlined in [A Cerospora Leaf Spot Model for Sugar Beet: In Practice by an Industry](https://apsjournals.apsnet.org/doi/abs/10.1094/PDIS.1998.82.7.716). More information about Cercospora leaf spot: <https://vegpath.plantpath.wisc.edu/diseases/carrot-alternaria-and-cercospora-leaf-blights/>
    -   Botrytis leaf blight (onion): <https://vegpath.plantpath.wisc.edu/diseases/onion-botrytis/>

### Credits and Contacts

-   Ben Bradford (Developer): [bbradford\@wisc.edu](mailto:bbradford@wisc.edu)
-   Damon Smith (Extension Plant Pathologist): [damon.smith\@wisc.edu](mailto:damon.smith@wisc.edu)
-   Crop Protection Network: [info\@cropprotectionnetwork.org](mailto:info@cropprotectionnetwork.org)

### Funding Sources

This project is partial supported by the National Predictive Modeling Tool Initiative, National Corn Growers Association, North Central Soybean Research Program, and United Soybean Board. This tool also relies on contributions from the University of Wisconsin-Madison Department of Entomology.
