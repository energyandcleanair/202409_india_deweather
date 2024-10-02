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
deweather(polls=c("pm10"), use_cache=T)


# Read deweathered data ----------------------------------------------------
deweathered <- get_deweathered(use_local=T, polls=c("pm10"))
meas <- get_measurements()


# Diagnose deweathered data ------------------------------------------------
diagnose_deweathering_performance(deweathered, poll="pm10")
diagnose_deweathered_availability(deweathered, meas, poll="pm10")


# Plot yoy -----------------------------------------------------------------
fys_from <- seq(2017, 2022)
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

yoys_split <- split(yoys, yoys$period)

lapply(names(yoys_split), function(period) {

  yoys <- yoys_split[[period]]

  # write_csv(yoys %>% filter(poll=="pm10", variable %in% c("observed","trend")), glue("results/yoy_pm10_{period}.csv"))
  #
  # yoys %>% filter(poll=="pm10", variable %in% c("observed","trend")) %>% select(location_name, poll, variable, yoy, rsquared_testing, period) %>%
  #   tidyr::pivot_wider(names_from = variable, values_from = yoy, names_prefix = "delta_") %>%
  #   mutate(delta_weather = delta_observed - delta_trend) %>%
  #   write_csv(glue("results/yoy_pm10_wide_{period}.csv"))
  #
  # plot_yoy(yoys, "pm10", period, glue("results/yoy_pm10_bars_{period}.png"), names_at_0 = F, width=11, height=8, logo=T, type="hbars")
  # plot_yoy(yoys, "pm10", period, glue("results/yoy_pm10_dots_{period}.png"), names_at_0 = F, width=11, height=7, logo=T, type="dots")
  plot_yoy_states(yoys, "pm10", period, glue("results/yoy_states_pm10_dots_{period}.png"), width=11, height=7, logo=T)
})


# Plot trends -------------------------------------------------------------
plot_trends(deweathered = deweathered, poll="pm10", filepath="results/trend_pm10.png")

