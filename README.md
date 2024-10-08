# 202409_india_deweather

Analysing PM10 trends in NCAP cities, with weather-normalisation and comparison with satellite-derived AOD.

Author: [Hubert Thieriot](mailto:hubert@energyancleanair.org)

## Table of Contents
- [Results](#results)
- [Diagnostics](#diagnostics)
- [Methodology](#methodology)

## Results

### Wheather-normalisation
We first normalise for weather conditions (see [methodology](./methodology.md)):
![PM10 timeseries](results/ts_pm10_365days.png)


We then extract overall trends for each city, since 2017 or the earliest available data,
using Theil-Sen estimator (data available [here](results/trends.csv)):

![PM10 trends](results/trends_pm10_city.png)

Regrouping by state:
![PM10 trends by state](results/trends_pm10_state.png)


If we consider more recent data only, i.e. from 2021 onwards, both to include more cities and to avoid COVID year:
![PM10 trends by state](results/trends_pm10_state_since_2021.png)


![PM10 map](results/trends_pm10_map_since_2021.png)

All covered cities in Uttar Pradesh show significant decrease in PM10 levels since 2017.

We do similar analysis for PM2.5, which has slightly better time coverage:

![PM2.5 trends](results/trends_pm25_city.png)

Regrouping by state:
![PM2.5 trends by state](results/trends_pm25_state.png)

If we consider more recent data only, i.e. from 2021 onwards:
![PM2.5 trends by state](results/trends_pm25_state_since_2021.png)

![PM2.5 map](results/trends_pm25_map_since_2021.png)


### Comparison with satellite-derived AOD
There has been some suspicion over Uttar Pradesh data. I compare ground-truth measurements of PM10 and PM2.5 with MODIS AOD.

![PM10 vs AOD](results/ts_pm10_aod_365days.png)


![PM2.5 vs AOD](results/ts_pm25_aod_365days.png)


## Diagnostics

### PM10
![Data availability](./diagnostics/data_availability_pm10.png)
![Model performance](./diagnostics/rsquared_testing_pm10.png)
![Variable importance](./diagnostics/importance_pm10.png)


## Methodology
See [methodology](./methodology.md) for details on how the data was collected and processed.

