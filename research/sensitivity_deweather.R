library(tidyverse)
library(glue)
library(ggrepel)
library(creadeweather)
library(rcrea)

locations <- rcrea::cities(name=c("Delhi", "Agra", "Varanasi", "Navi Mumbai"), country="IN")$id


readRenviron("~/development/crea/deweather/.Renviron")
weather_file_sensitivity <- glue("cache/weather_sensitivity.RDS")
weather_file_sensitivity_noaa <- glue("cache/weather_sensitivity_noaa.RDS")


deweathered_trend <- creadeweather::deweather(
  location_id = locations,
  poll = c(poll),
  source = c("cpcb"),
  deweather_process_id = "default_trend",
  upload_results = F,
  save_weather_filename = weather_file_sensitivity,
  read_weather_filename = weather_file_sensitivity,
  use_weather_cache = F,
  weather_update_era5 = F,
  date_to="2024-08-31",
  ntrainings=1
)



deweathered_trend2 <- creadeweather::deweather(
  location_id = locations,
  poll = c(poll),
  source = c("cpcb"),
  deweather_process_id = "default_trend",
  upload_results = F,
  save_weather_filename = weather_file_sensitivity_noaa,
  read_weather_filename = weather_file_sensitivity_noaa,
  use_weather_cache = F,
  date_to="2024-08-31",
  ntrainings=1,
  override_params = list(interaction.depth=5, weather_sources=c("noaa","era5"))
)



# compare weather
weather_sensitivity <- readRDS(weather_file_sensitivity)
weather_sensitivity_noaa <- readRDS(weather_file_sensitivity_noaa)

combined_weather <- bind_rows(
  weather_sensitivity %>% mutate(source = "ERA5"),
  weather_sensitivity_noaa %>% mutate(source = "NOAA + ERA5")
)

# Plotting the combined weather data
ggplot(combined_weather, aes(x = date, y = value, color = source)) +
  geom_line() +
  labs(title = "Weather Sensitivity Comparison",
       x = "Date",
       y = "Value",
       color = "Source") +
  theme_minimal()
