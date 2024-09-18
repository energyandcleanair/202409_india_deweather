compute_yoy <- function(deweathered,
                        date_break,
                        min_availability_each_month=0.5){
  
  
  date_break <- as.Date(date_break)
  add_period <- function(x){ x %>% mutate(period=case_when(
    date >= date_break & date < date_break + years(1) ~ "after",
    date >= date_break - years(1) & date < date_break ~ "before",
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
             month=lubridate::month(date),) %>%
    summarise(availabiliy = n() / lubridate::days_in_month(unique(month))) %>%
    summarise(is_complete = sum(availabiliy > 0.5) == 12) %>%
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
    left_join(performance, by=c("location_id", "poll"))
}
