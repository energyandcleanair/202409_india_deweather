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


# Build deweathered data ---------------------------------------------------
deweather_ncap(polls=c("pm10", "pm25", "no2"), use_cache=T)

# Read deweathered data ----------------------------------------------------
deweathered <- get_deweathered(use_local=T)
# deweathered_api <- get_deweathered(use_local=F)
meas <- get_measurements()


# Diagnose deweathered data ------------------------------------------------
diagnose_deweathering_performance(deweathered, poll="pm10")
diagnose_deweathering_performance(deweathered, poll="pm25")
diagnose_deweathering_performance(deweathered, poll="no2")

diagnose_deweathered_availability(deweathered, meas, poll="pm10")
diagnose_deweathered_availability(deweathered, meas, poll="pm25")
diagnose_deweathered_availability(deweathered, meas, poll="no2")


# Plot yoy -----------------------------------------------------------------
yoys <- compute_yoy(deweathered,
                    date_break = "2023-04-01",
                    min_availability_each_month = 0.5,
                    min_rsquared_testing = 0.6)

write_csv(yoys %>% filter(poll=="pm10", variable %in% c("observed","trend")), "results/yoy_pm10.csv")
write_csv(yoys %>% filter(poll=="pm25", variable %in% c("observed","trend")), "results/yoy_pm25.csv")
write_csv(yoys %>% filter(poll=="no2", variable %in% c("observed","trend")), "results/yoy_no2.csv")


yoys %>% filter(poll=="pm10", variable %in% c("observed","trend")) %>% select(location_name, poll, variable, yoy, rsquared_testing) %>%
  tidyr::pivot_wider(names_from = variable, values_from = yoy, names_prefix = "delta_") %>%
  mutate(delta_weather = delta_observed - delta_trend) %>%
  write_csv("results/yoy_pm10_wide.csv")


yoys %>% filter(poll=="pm25", variable %in% c("observed","trend")) %>% select(location_name, poll, variable, yoy, rsquared_testing) %>%
  tidyr::pivot_wider(names_from = variable, values_from = yoy, names_prefix = "delta_") %>%
  mutate(delta_weather = delta_observed - delta_trend) %>%
  write_csv("results/yoy_pm25_wide.csv")

yoys %>% filter(poll=="no2", variable %in% c("observed","trend")) %>% select(location_name, poll, variable, yoy, rsquared_testing) %>%
  tidyr::pivot_wider(names_from = variable, values_from = yoy, names_prefix = "delta_") %>%
  mutate(delta_weather = delta_observed - delta_trend) %>%
  write_csv("results/yoy_no2_wide.csv")


plot_yoy(yoys, "pm10", "results/yoy_pm10.png", names_at_0 = T, width=8, height=11, logo=T)
plot_yoy(yoys, "pm25", "results/yoy_pm25.png", names_at_0 = T, width=8, height=11, logo=T)
plot_yoy(yoys, "no2", "results/yoy_no2.png", names_at_0 = T, width=8, height=11, logo=T)


# Plot trends -------------------------------------------------------------
plot_trends(deweathered = deweathered, poll="pm10", filepath="results/trend_pm10.png")
plot_trends(deweathered = deweathered, poll="pm25", filepath="results/trend_pm25.png")
plot_trends(deweathered = deweathered, poll="no2", filepath="results/trend_no2.png")

