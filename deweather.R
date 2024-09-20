deweather_ncap <- function(polls, use_cache){

  readRenviron("~/development/crea/deweather/.Renviron")

  # Use current url to collect NCAP locations
  anomalies <- read_csv("https://api.energyandcleanair.org/ncap/anomaly?pollutant=pm25&ncap_only=true&year=2024&month=8&deweather_method=default_anomaly_2018_2099&format=csv")
  locations_ncap <- unique(anomalies$location_id)

  weather_file <- "cache/weather_ncap.RDS"
  dir.create(dirname(weather_file), showWarnings = FALSE)

  for(poll in polls){
    
    filepath <- glue("outputs/deweathered_trend_{poll}.RDS")
    if(file.exists(filepath) && use_cache){
      next
    }
    
    deweathered_trend <- creadeweather::deweather(
      location_id = locations_ncap,
      poll = c(poll),
      source = c("cpcb"),
      deweather_process_id = "default_trend",
      upload_results = T,
      save_weather_filename = weather_file,
      read_weather_filename = weather_file,
      weather_update_era5 = T,
      use_weather_cache = F,
      date_to="2024-08-31"
    )
    
    dir.create(dirname(filepath), showWarnings = FALSE)
    saveRDS(deweathered_trend, filepath)
  }
}