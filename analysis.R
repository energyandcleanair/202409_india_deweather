library(tidyverse)
library(glue)
library(ggrepel)
library(creadeweather)
library(rcrea)


source('deweather.R')
source('plot.R')
source('data.R')
source('compute_yoy.R')
source('diagnostics.R')
source('trends.R')


default_width <- 9

# Build deweathered data ---------------------------------------------------
deweather(polls=c("pm10"), use_cache=T)
deweather(polls=c("pm25"), use_cache=T)


# Read deweathered data ----------------------------------------------------
deweathered <- get_deweathered(use_local=T, polls=c("pm10", "pm25"))
meas <- get_measurements()


# Compute yoy -----------------------------------------------------------------
fys_from <- seq(2022, 2022)
fys_to <- 2023

fys <- crossing(fys_from, fys_to) %>%
  mutate(period = glue("FY {fys_from}-{fys_from + 1} to FY {fys_to}-{fys_to + 1}"))

yoys <- fys %>%
  pmap_dfr(function(fys_from, fys_to, period) {
    compute_yoy(deweathered,
                before = list(date_from = glue("{fys_from}-04-01"),
                              date_to = glue("{fys_from + 1}-03-31")),
                after = list(date_from = glue("{fys_to}-04-01"),
                             date_to = glue("{fys_to + 1}-03-31")),
                min_availability_each_month = 0.5,
                min_rsquared_testing = 0.6) %>%
      mutate(period = period)
  })



# Extract national average ------------------------------------------------
plot_national_change(yoys, "pm10", filepath="results/national_change.png", width=default_width, height=default_width*0.5)

# Diagnose deweathered data ------------------------------------------------
diagnose_deweathering_performance(deweathered, yoys=yoys, poll="pm10")
diagnose_deweathered_availability(deweathered, meas, poll="pm10")
diagnose_models_importance(deweathered, yoys=yoys, poll="pm10", width=12, height=14)


# Plot trends timeseries -------------------------------------------------------------
plot_timeseries(deweathered = deweathered, poll="pm10", yoys=yoys, running_days=365, filepath="results/ts_pm10_365days.png", width=default_width, height=default_width*1.2)
plot_timeseries_yearly(deweathered = deweathered, poll="pm10", yoys=yoys, ncol=7, filepath="results/ts_yearly_pm10.png", width=default_width, height=default_width*1.2)

plot_timeseries(deweathered = deweathered, poll="pm25", yoys=yoys, running_days=365, filepath="results/ts_pm25_365days.png", width=default_width, height=default_width*0.8)
plot_timeseries_yearly(deweathered = deweathered, poll="pm25", yoys=yoys, ncol=7, filepath="results/ts_yearly_pm25.png", width=default_width, height=default_width*1.2)

# Get and plot trends ----------------------------------------------------------------
trends <- get_trends(deweathered, c("pm10","pm25"), min_rsquared_testing=0.6, min_years=3, date_from="2017-01-01")
write_csv(trends, "results/trends.csv")
plot_trends(trends=trends, "pm10", by="city", filepath="results/trends_pm10_city.png", width=default_width, height=default_width*0.8)
plot_trends(trends=trends, "pm10", by="state", filepath="results/trends_pm10_state.png", width=default_width, height=default_width*0.8)
plot_trends_map(trends=trends, "pm10", filepath="results/trends_pm10_map.png", width=default_width, height=default_width)

plot_trends(trends=trends, "pm25", by="city", filepath="results/trends_pm25_city.png", width=default_width, height=default_width*0.8)
plot_trends(trends=trends, "pm25", by="state", filepath="results/trends_pm25_state.png", width=default_width, height=default_width*0.8)
plot_trends_map(trends=trends, "pm25", filepath="results/trends_pm25_map.png", width=default_width, height=default_width)


trends_since_2021 <- get_trends(deweathered, c("pm10","pm25"), min_rsquared_testing=0.6, date_from="2021-01-01", min_years=2)
write_csv(trends_since_2021, "results/trends_since_2021.csv")
plot_trends(trends=trends_since_2021, "pm10", by="city", filepath="results/trends_pm10_city_since_2021.png", width=default_width, height=default_width*0.8)
plot_trends(trends=trends_since_2021, "pm10", by="state", filepath="results/trends_pm10_state_since_2021.png", width=default_width, height=default_width*0.8)
plot_trends_map(trends=trends_since_2021, "pm10", filepath="results/trends_pm10_map_since_2021.png", width=default_width, height=default_width)

plot_trends(trends=trends_since_2021, "pm25", by="city", filepath="results/trends_pm25_city_since_2021.png", width=default_width, height=default_width*0.8)
plot_trends(trends=trends_since_2021, "pm25", by="state", filepath="results/trends_pm25_state_since_2021.png", width=default_width, height=default_width*0.8)
plot_trends_map(trends=trends_since_2021, "pm25", filepath="results/trends_pm25_map_since_2021.png", width=default_width, height=default_width)

# Compare with AOD --------------------------------------------------------
plot_timeseries_w_aod(meas = meas %>% filter(grepl("modis_aod_550|cams|pm10", poll)), poll="pm10", min_years=3, running_days=365, filepath="results/ts_pm10_aod_365days.png", width=default_width, height=default_width*1.2, ncol=6)
plot_timeseries_w_aod(meas = meas %>% filter(grepl("modis_aod_470|pm25", poll)), poll="pm25", min_years=3, running_days=365, filepath="results/ts_pm25_aod_365days.png", width=default_width, height=default_width*1.2, ncol=6)

trends_aod <- get_scaled_trends_w_aod(meas, c("pm10"), min_years=3, period="season")
write_csv(trends_aod, "results/trends_aod.csv")
plot_trends_w_aod(trends_aod=trends_aod, "pm10", max_p=0.2, filepath="results/trends_pm10_aod_city.png", width=default_width, height=default_width*0.8)


# plot yoys by FY -------------------------------------------------------------------
yoys_split <- split(yoys, yoys$period)
lapply(names(yoys_split), function(period) {

  yoys <- yoys_split[[period]]

  write_csv(yoys %>% filter(poll=="pm10", variable %in% c("observed","trend")), glue("results/yoy_pm10_{period}.csv"))

  yoys %>% filter(poll=="pm10", variable %in% c("observed","trend")) %>% select(location_name, poll, variable, yoy, rsquared_testing, period) %>%
    tidyr::pivot_wider(names_from = variable, values_from = yoy, names_prefix = "delta_") %>%
    mutate(delta_weather = delta_observed - delta_trend) %>%
    write_csv(glue("results/yoy_pm10_wide_{period}.csv"))

  plot_yoy(yoys, "pm10", period, glue("results/yoy_pm10_bars_{period}.png"), names_at_0 = F, type="hbars", width=default_width, height=default_width*0.7)
  plot_yoy(yoys, "pm10", period, glue("results/yoy_pm10_dots_{period}.png"), names_at_0 = F, type="dots", width=default_width, height=default_width*0.7)
  plot_yoy_states(yoys, "pm10", period, glue("results/yoy_states_pm10_dots_{period}.png"), width=default_width, height=default_width*0.7)
  plot_yoy_national(yoys, "pm10", period, glue("results/yoy_national_pm10_{period}.png"), width=default_width, height=default_width*0.7)
})

# Export appendix tables --------------------------------------------------
yoys %>%
  filter(poll=="pm10", variable %in% c("trend", "observed")) %>%
  select(location_name, variable, yoy, period) %>%
  # round and replace NA with -
  mutate(yoy = ifelse(is.na(yoy), "-", glue("{ifelse(yoy>0,'+','')}{round(yoy, 1)}"))) %>%
  spread(variable, yoy) %>%
  mutate(label = glue("'{observed} ({trend})")) %>%
  select(-observed, -trend) %>%
  tidyr::pivot_wider(names_from = period, values_from = label) %>%
  mutate(unit = "µg/m³") %>%
  dplyr::arrange(location_name) %>%
  write_csv("results/yoy_deweatherd_pm10_wide.csv") %>%
  #copy to clipboard
  clipr::write_clip()


# Misc --------------------------------------------------------------------
# Create a shorter lit of ncap with only cities that have good enough model
read_csv("data/ncap_cities.csv") %>%
  inner_join(yoys %>% filter(variable %in% c("observed","trend")) %>% distinct(location_id)) %>%
  write_csv("data/ncap_cities_filtered.csv")
