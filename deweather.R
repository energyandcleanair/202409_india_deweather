library(tidyverse)
library(creadeweather)
library(rcrea)
readRenviron("~/development/crea/deweather/.Renviron")

# Use current url to collect NCAP locations
anomalies <- read_csv("https://api.energyandcleanair.org/ncap/anomaly?pollutant=pm25&ncap_only=true&year=2024&month=8&deweather_method=default_anomaly_2018_2099,&format=csv")
locations_ncap <- unique(anomalies$location_id)

weather_file <- "tmp/weather_ncap.RDS"
dir.create("tmp", showWarnings = F)


# deweathered_trend_pm10 <- creadeweather::deweather(
#   location_id = locations_ncap,
#   poll = c("pm10"),
#   source = c("cpcb"),
#   deweather_process_id = "default_trend",
#   upload_results = T,
#   save_weather_filename = weather_file,
#   read_weather_filename = weather_file,
#   weather_update_era5 = F
# )
# 
# 
# dir.create("outputs")
# saveRDS(deweathered_trend_pm10, "outputs/deweathered_trend_pm10.RDS")
# 


deweathered_trend_pm25 <- creadeweather::deweather(
  location_id = locations_ncap,
  poll = c("pm25"),
  source = c("cpcb"),
  deweather_process_id = "default_trend",
  upload_results = T,
  save_weather_filename = weather_file,
  read_weather_filename = weather_file,
  weather_update_era5 = F
)


dir.create("outputs")
saveRDS(deweathered_trend_pm25, "outputs/deweathered_trend_pm25.RDS")

deweathered_trend_no2 <- creadeweather::deweather(
  location_id = locations_ncap,
  poll = c("no2"),
  source = c("cpcb"),
  deweather_process_id = "default_trend",
  upload_results = T,
  save_weather_filename = weather_file,
  read_weather_filename = weather_file,
  weather_update_era5 = F
)


dir.create("outputs")
saveRDS(deweathered_trend_no2, "outputs/deweathered_trend_no2.RDS")