# Methodology

This study aims to identify the underlying trends in air quality independent of weather influences. By removing the effects of meteorological variables, we can better understand how air quality changes over time due to factors other than weather.

## Data Sources

Air quality data were obtained from the Central Pollution Control Board (CPCB). The dataset comprises hourly measurements recorded at various monitoring stations within cities. We first performed temporal aggregation at the station level by averaging hourly measurements into daily averages. We then conducted spatial aggregation at the city level by combining daily averages from all stations within the same city.

Weather variables known to influence air quality were extracted from the ERA5 dataset provided by the European Centre for Medium-Range Weather Forecasts (ECMWF). The variables included are:
- Minimum air temperature
- Atmospheric pressure
- Wind direction
- Wind speed
- Precipitation
- Dew point temperature
- Minimum planetary boundary layer height
- Maximum planetary boundary layer height

To account for potential delayed effects of weather on air quality, we also included the same meteorological variables with a one-day lag. This allows the model to capture any lagged relationships between weather conditions and air quality changes.

## Data Selection Criteria

To ensure sufficient data coverage for capturing monthly trends, we included only cities with data available for at least 50% of the days in each month. Cities that did not meet this data availability threshold were excluded from the study. A list of the removed cities is provided in [this file](results/incomplete_cities.csv).

## Modeling Approach

We employed a Gradient Boosting Machine (GBM) to model the relationship between air quality and the predictor variables. GBM is effective at capturing complex nonlinear relationships and interactions among variables.

### Model Parameters

The model parameters were selected based on validation results to optimize the balance between model complexity and predictive accuracy. The parameters used are:

- Interaction depth: 7
- Learning rate: 0.01
- Trees: 20,000
- Cross-validation folds: 3

Cross-validation folds as well as a separate validation subset were used to evaluate model performance and ensure the model's generalizability.

### Temporal Variables

To account for temporal patterns and remove the effects of seasonality and weekly cycles, we included the following time variables:

- Decimal date: A continuous numerical representation of the date, serving as a trend term to capture long-term changes in air quality over time.
- Day of the year: Represents the specific day within the year (ranging from 1 to 365 or 366), capturing seasonal effects that repeat annually.
- Day of the week: Indicates the day of the week (e.g., Monday, Tuesday), capturing weekly patterns that may influence air quality.

## Analysis and Interpretation

The GBM model was trained to predict daily city-level air quality based on the weather variables and temporal variables described above. By including the decimal date as a predictor, the model captures underlying trends in air quality over time, independent of weather, seasonality, and weekly cycles.

The trend in air quality, corrected for weather conditions, was extracted by analyzing the model's dependence on the decimal date variable. The resulting trend represents the underlying changes in air quality over time that are not attributable to weather variations or recurring temporal patterns.


## References
This work is building upon [CREA deweathering R package](https://github.com/energyandcleanair/creadeweather). Below are useful references to learn more about the approach and its limitations.


- Carslaw, D. [deweather package](https://github.com/davidcarslaw/deweather)

- Grange, Stuart K., and David C. Carslaw. “Using Meteorological Normalisation to Detect Interventions in Air Quality Time Series.” Science of The Total Environment 653 (February 25, 2019): 578–88. https://doi.org/10.1016/j.scitotenv.2018.10.344.

- Qiu, Minghao, Corwin Zigler, and Noelle E. Selin. “Statistical and Machine Learning Methods for Evaluating Trends in Air Quality under Changing Meteorological Conditions.” Atmospheric Chemistry and Physics 22, no. 16 (August 19, 2022): 10551–66. https://doi.org/10.5194/acp-22-10551-2022.

- Copernicus Climate Change Service (C3S) (2017): ERA5: Fifth generation of ECMWF atmospheric reanalyses of the global climate . Copernicus Climate Change Service Climate Data Store (CDS). https://cds.climate.copernicus.eu/cdsapp#!/home