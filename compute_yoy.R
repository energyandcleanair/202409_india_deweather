compute_yoy <- function(deweathered,
                        before = list(date_from="2022-04-01", date_to="2023-03-31"),
                        after = list(date_from="2023-04-01", date_to="2024-03-31"),
                        min_availability_each_month=0.5,
                        min_rsquared_testing=NULL
                        ){


  add_period <- function(x){ x %>% mutate(period=case_when(
    date >= ymd(before$date_from) & date < ymd(before$date_to) ~ "before",
    date >= ymd(after$date_from) & date < ymd(after$date_to) ~ "after",
    T ~ NA))}

  yoys_completeness <- deweathered %>%
    tidyr::unnest(result) %>%
    add_period %>%
    filter(variable %in% c("observed", "trend")) %>%
    filter(!is.na(period),
           !is.na(value)) %>%
    group_by(location_id,
             location_name,
             source,
             poll,
             variable,
             period,
             unit,
             month=lubridate::month(date)) %>%
    summarise(availabiliy = n() / lubridate::days_in_month(unique(month))) %>%
    summarise(is_complete = sum(availabiliy > 0.5) == 12) %>%
    # group by - period
    group_by(location_id,
             location_name,
             source,
             poll,
             variable) %>%
    summarise(is_complete = sum(is_complete) == 2) %>%
    ungroup()


  # Warn that we're removing incomplete data
  removed <- yoys_completeness %>%
    filter(!is_complete) %>%
    ungroup() %>%
    distinct(location_id, location_name, poll)

  write_csv(removed, "results/incomplete_cities.csv")

  if(nrow(removed) > 0){
    message(
      paste0(
        c(glue("Removing incomplete data for {nrow(removed)} locations and pollutants:"),
        paste0(paste0(removed$location_name, " - ", removed$poll), collapse = "\n"))
        )
    )
  }


  # performances
  performance <- deweathered %>%
    unnest(performances) %>%
    unnest_wider(performances) %>%
    select(location_id, poll, rsquared_testing, rmse_testing)


  deweathered %>%
    anti_join(removed, by=c("location_id", "poll")) %>%
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
    summarise(value=mean(value, na.rm=T)) %>%
    ungroup() %>%
    tidyr::spread(period, value) %>%
    mutate(yoy = after - before,
           yoy_rel = yoy / before) %>%
    select(location_id, location_name, source, poll, variable, yoy, yoy_rel) %>%
    left_join(performance, by=c("location_id", "poll")) %>%
    {
      if(is.null(min_rsquared_testing)){
        .
      } else {
        filter(rsquared_testing > min_rsquared_testing)
      }
    }


  deweathered %>%
    anti_join(removed, by = c("location_id", "poll")) %>%
    tidyr::unnest(result) %>%
    left_join(performance, by = c("location_id", "poll")) %>%
    add_period %>%
    filter(!is.na(period), !is.na(value)) %>%
    group_by(location_id, location_name, source, poll, variable, period, unit, rmse_testing) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    tidyr::spread(period, value) %>%
    mutate(
      # Year-on-year difference and relative change
      yoy = after - before,
      yoy_rel = yoy / before,

      # Uncertainty propagation for the absolute YoY difference (yoy)
      sigma_yoy_abs = sqrt(rmse_testing^2 + rmse_testing^2),  # Propagating RMSE for both periods

      # Confidence interval calculation (95% CI) for absolute YoY difference
      lower_ci_yoy = yoy - 1.96 * sigma_yoy_abs,
      upper_ci_yoy = yoy + 1.96 * sigma_yoy_abs,

      # Uncertainty propagation for the relative YoY ratio (yoy_rel)
      sigma_yoy_rel = yoy_rel * sqrt((rmse_testing / before)^2 + (rmse_testing / after)^2),

      # Confidence interval calculation (95% CI) for relative YoY ratio
      lower_ci_yoy_rel = yoy_rel - 1.96 * sigma_yoy_rel,
      upper_ci_yoy_rel = yoy_rel + 1.96 * sigma_yoy_rel
    ) %>%
    select(location_id, location_name, source, poll, variable, yoy, yoy_rel,
           lower_ci_yoy, upper_ci_yoy, lower_ci_yoy_rel, upper_ci_yoy_rel) %>%
    {
      if(is.null(min_rsquared_testing)){
        .
      } else {
        filter(rsquared_testing > min_rsquared_testing)
      }
    }
}
