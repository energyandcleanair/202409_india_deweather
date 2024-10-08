get_trends <- function(deweathered, poll, period="month", min_rsquared_testing=0.6, min_years=3, date_from=NULL, plot=F){

  deweathered %>%
    filter(poll %in% !!poll) %>%
    tidyr::unnest(performances) %>%
    tidyr::unnest_wider(performances) %>%
    filter(rsquared_testing >= min_rsquared_testing) %>%
    select(location_id, location_name, poll, result) %>%
    unnest(result) %>%
    mutate(date_num = as.numeric(date)) %>%
    {
      if(!is.null(date_from)){
        filter(., date >= date_from)
      }else{
        .
      }
    } %>%
    filter(variable %in% c("trend", "observed"),
           !is.na(value),
           !is.na(date)
           ) %>%
    group_by(location_id, location_name, poll, variable) %>%
    # at least two y
    filter((max(date_num) - min(date_num)) / 365 >= min_years) %>%
    group_modify(function(data, ...){
      d <- openair::TheilSen(
        data,
        pollutant="value",
        data.thresh=50,
        avg.time=period,
        plot=plot
      )

      res <- d$data$res2 %>% filter(!is.na(slope))

      tibble(
        slope = res$slope,
        slope_lower = res$lower,
        slope_upper = res$upper,
        p.stars = res$p.stars,
        p = res$p,
        date_from = min(data$date),
        date_to = max(data$date)
      )
    })
}

get_scaled_trends_w_aod <- function(meas, poll, period="month", min_years=3){

   meas %>%
    filter(grepl("aod", poll) | poll==!!poll) %>%
    filter(date >= "2017-01-01") %>%
    filter(!is.na(value)) %>%
    select(location_id, location_name, poll, source, date, variable, value) %>%
    mutate(variable=case_when(grepl("aod", poll) ~ toupper(gsub("_|550"," ",poll)),
                              T ~ rcrea::poll_str(poll))) %>%
    filter(!is.na(value)) %>%
    # Keep only since observed data is available
    group_by(location_name) %>%
    filter(date >= min(date[!grepl("AOD", variable)])) %>%
    # standardise by location/poll
    group_by(location_name, poll, variable) %>%
    mutate(value = scale(value)) %>%
     mutate(date_num = as.numeric(date)) %>%

     group_by(location_id, location_name, poll, variable) %>%
     # at least two y
     filter((max(date_num) - min(date_num)) / 365 >= min_years) %>%
     group_modify(function(data, ...){
       d <- openair::TheilSen(
         data,
         pollutant="value",
         data.thresh=50,
         avg.time=period
       )

       if(is.null(d$data$res2)){
         return(tibble())
       }

       res <- d$data$res2 %>% filter(!is.na(slope))
       tibble(
         slope = res$slope,
         slope_lower = res$lower,
         slope_upper = res$upper,
         p.stars = as.character(res$p.stars),
         p = res$p,
         date_from = min(data$date),
         date_to = max(data$date)
       )
     })
}

