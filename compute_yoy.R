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
    filter(is.null(min_rsquared_testing) | rsquared_testing > min_rsquared_testing)
}
