extract_yoys_from_trend <- function(meas) {
  meas %>%
    group_by(location_id,
             source,
             pollutant,
             year = year(date),
             month = month(date),
             variable) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    arrange(year) %>%
    group_by(across(-c(year, value))) %>%
    mutate(delta = value - lag(value)) %>%
    # Add observed value of first year
    ungroup() %>%
    group_by(location_id, source, pollutant, month, year) %>%
    mutate(observed_prev = value[variable == "observed"] - delta[variable == "observed"]) %>%
    ungroup() %>%
    filter(!is.na(delta)) %>%
    select(-c(value)) %>%
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "delta",
      names_prefix = "yoy_"
    ) %>%
    rename(
      yoy_total = yoy_observed,
      yoy_emission = yoy_trend,
    ) %>%
    mutate(
      yoy_weather = yoy_total - yoy_emission,
      yoy_total_rel = yoy_total / observed_prev,
      yoy_weather_rel = yoy_weather / observed_prev,
      yoy_emission_rel = yoy_emission / observed_prev
    ) %>%
    pivot_longer(
      cols = -c(year, observed_prev, location_id, source, pollutant, year, month),
      names_to = "variable",
      values_to = "value",
    ) %>%
    filter(grepl("yoy.*_rel", variable)) %>%
    select(location_id, source, pollutant, variable, year, month, value)
}