library(tidyverse)
library(glue)
library(ggrepel)
library(rcrea)

source('plot.R')
source('data.R')
source('compute_yoy.R')
source('diagnostics.R')



# Read deweathered data ---------------------------------------------------
deweathered <- get_deweathered(use_local=T)
deweathered_api <- get_deweathered(use_local=F)
meas <- get_measurements()


# Check model quality -----------------------------------------------------


# Check data availability -------------------------------------------------
diagnose_deweathering_performance(deweathered)
diagnose_deweathered_availability(deweathered)



# Plot yoy ----------------------------------------------------------------
yoys <- compute_yoy(deweathered,
                    date_break = "2023-04-01",
                    min_availability_each_month=0.5)

write_csv(yoys %>% filter(poll=="pm10", variable %in% c("observed","trend")), "results/yoy_pm10.csv")
write_csv(yoys %>% filter(poll=="pm25", variable %in% c("observed","trend")), "results/yoy_pm25.csv")

plot_yoy(yoys, "pm10", "results/yoy_pm10_w_logo.png", names_at_0 = T, width=8, height=11, logo=T)
plot_yoy(yoys, "pm25", "results/yoy_pm25_w_logo.png", names_at_0 = T, width=8, height=11, logo=T)

