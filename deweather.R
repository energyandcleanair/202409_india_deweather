deweather <- function(polls, use_cache){

  readRenviron("~/development/crea/deweather/.Renviron")

  # Use current url to collect NCAP locations
  locations_ncap <- read_csv("data/ncap_cities.csv") %>% distinct(location_id)
  weather_file <- glue("cache/weather_ncap.RDS")
  dir.create(dirname(weather_file), showWarnings = FALSE)

  for(poll in polls){

    filepath <- glue("outputs/deweathered_trend_{poll}.RDS")
    if(file.exists(filepath) && use_cache){
      next
    }


    deweathered_trend <- creadeweather::deweather(
      location_id = locations_ncap$location_id,
      poll = c(poll),
      source = c("cpcb"),
      deweather_process_id = "default_trend",
      upload_results = T,
      save_weather_filename = weather_file,
      read_weather_filename = weather_file,
      use_weather_cache = F,
      date_to="2024-08-31",
      ntrainings=1,
      with_normalisation=T
    )

    dir.create(dirname(filepath), showWarnings = FALSE)
    saveRDS(deweathered_trend, filepath)
  }
}
