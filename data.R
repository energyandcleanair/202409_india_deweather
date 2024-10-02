get_deweathered <- function(use_local=T, polls=c("pm25", "pm10", "no2")){

  if(use_local){
    deweathered <- list.files("outputs", pattern="deweathered.*\\.RDS", full.names = T) %>%
      lapply(readRDS) %>%
      bind_rows() %>%
      filter(poll %in% polls)

    locations <- rcrea::locations(id=unique(deweathered$location_id)) %>%
      distinct(location_id=id, location_name=name)

    deweathered %>%
      left_join(locations, by="location_id")

  } else {
    # Use API
    pollutant_filter <- paste0(polls, collapse=",")
    anomalies <- read_csv(glue("https://api.energyandcleanair.org/ncap/anomaly?pollutant={pollutant_filter}&ncap_only=true&year=2024&month=8&deweather_method=default_anomaly_2018_2099,&format=csv"))
    locations_ncap <- unique(anomalies$location_id)
    locations_ncap_chunks <- split(locations_ncap, ceiling(seq_along(locations_ncap) / 10))
    download <- function(ls){
      ls_str <- paste0(ls, collapse = ",")
      url <- glue("https://api.energyandcleanair.org/v1/measurements?location_id={ls_str}&process_id=default_trend&source=cpcb&pollutant={pollutant_filter&variable=trend,observed&date_from=2015-01-01&format=csv&gzip=true")
      # read quietly
      read_csv(gzcon(url(url)), col_types = cols())
    }

    meas <- lapply(locations_ncap_chunks, download) %>% bind_rows()
    meas %>%
      group_by(location_id, location_name=city_name, poll=pollutant, unit, process_id) %>%
      nest() %>%
      rename(result=data) %>%
     # Create empty performances
       mutate(rsquared_testing = list(0)) %>%
      group_by(location_id, location_name, poll=pollutant, unit, process_id, result) %>%
      nest() %>%
      rename(performances=data)  %>%
      ungroup()
  }

}

get_measurements <- function( polls=c("pm25", "pm10", "no2")){

    # Use API
    pollutant_filter <- paste0(polls, collapse=",")
    locations_ncap <- read_csv("data/ncap_cities.csv") %>% distinct(location_id) %>% pull(location_id)
    locations_ncap_chunks <- split(locations_ncap, ceiling(seq_along(locations_ncap) / 20))
    download <- function(ls){
      ls_str <- paste0(ls, collapse = ",")
      url <- glue("https://api.energyandcleanair.org/v1/measurements?location_id={ls_str}&process_id=city_day_mad&source=cpcb&pollutant={pollutant_filter}&variable=trend,observed&date_from=2015-01-01&format=csv&gzip=true")
      # encode url
      url <- URLencode(url)

      # read quietly
      read_csv(gzcon(url(url)), col_types = cols())
    }

    lapply(locations_ncap_chunks, download) %>%
      bind_rows() %>%
      rename(poll=pollutant, location_name=city_name)
}


rename_cities <- function(){

}


remove_incomplete <- function(deweathered,
                              min_availability_each_month=0.5){

  yoys <- deweathered %>%
    tidyr::unnest(result) %>%
    add_period %>%
    filter(!is.na(period),
           !is.na(value)) %>%
    group_by(location_id,
             location_name,
             source,
             poll,
             variable,
             period,
             unit) %>%
    summarise(value = mean(value),
              n = n()
    ) %>%
    filter(n >= 365 * 0.7) %>%
    select(-c(n)) %>%
    ungroup() %>%
    tidyr::spread(period, value) %>%
    mutate(yoy = after - before,
           yoy_rel = yoy / before) %>%
    select(location_id, location_name, source, poll, variable, yoy, yoy_rel)


}

add_state <- function(yoys){

  locations <- rcrea::locations(id=unique(yoys$location_id), with_metadata = T) %>%
    select(location_id=id, state=gadm1_name) %>%
    distinct()

  yoys %>%
    left_join(locations, by="location_id")
}


