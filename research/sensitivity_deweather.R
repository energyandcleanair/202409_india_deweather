locations <- rcrea::cities(name=c("Delhi", "Agra", "Varanasi", "Navi Mumbai"), country="IN")$id


weather_file_sensitivity <- glue("cache/weather_sensitivity.RDS")

deweathered_trend <- creadeweather::deweather(
  location_id = locations_ncap$location_id,
  poll = c(poll),
  source = c("cpcb"),
  deweather_process_id = "default_trend",
  upload_results = T,
  save_weather_filename = weather_file_sensitivity,
  read_weather_filename = weather_file_sensitivity,
  use_weather_cache = F,
  date_to="2024-08-31",
  ntrainings=1
)

deweathered_anomaly <- creadeweather::deweather(
  location_id = locations_ncap$location_id,
  poll = c(poll),
  source = c("cpcb"),
  deweather_process_id = "default_anomaly_2017_2099",
  upload_results = T,
  save_weather_filename = weather_file_sensitivity,
  read_weather_filename = weather_file_sensitivity,
  use_weather_cache = F,
  date_to="2024-08-31",
  ntrainings=1
)
