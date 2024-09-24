# 202409_india_deweather

Build and analyse deweathered data for NCAP cities.

Author: [Hubert Thieriot](mailto:hubert@energyancleanair.org)

## Table of Contents
- [Results](#results)
- [Diagnostics](#diagnostics)
- [Methodology](#methodology)

## Results

### PM10
Data is available here:
- [narrow csv file](results/yoy_pm10.csv)
- [wide csv file](results/yoy_pm10_wide.csv)

![PM10](results/yoy_pm10.png)


### PM2.5
Data is available here:
- [narrow csv file](results/yoy_pm25.csv)
- [wide csv file](results/yoy_pm25_wide.csv)

![PM25](results/yoy_pm25.png)


### NO2
Data is available here:
- [narrow csv file](results/yoy_no2.csv)
- [wide csv file](results/yoy_no2_wide.csv)


![NO2](results/yoy_no2.png)

## Diagnostics

### PM10
![Data availability](./diagnostics/data_availability_pm10.png)
![Model performance](./diagnostics/rsquared_testing_pm10.png)


### PM2.5
![Data availability](./diagnostics/data_availability_pm25.png)
![Model performance](./diagnostics/rsquared_testing_pm25.png)

### NO2
![Data availability](./diagnostics/data_availability_no2.png)
![Model performance](./diagnostics/rsquared_testing_no2.png)


## Methodology
See [methodology](./methodology.md) for details on how the data was collected and processed.





