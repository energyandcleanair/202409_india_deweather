diagnose_deweathering_performance <- function(deweathered, poll){

  performance <- deweathered %>%
    filter(poll==!!poll) %>%
    unnest(performances) %>%
    unnest_wider(performances)
  
  ggplot(performance,
         # reorder location_id based on rsauqred_testing
         aes(y=reorder(location_name, rsquared_testing),
             x=rsquared_testing)
         ) +
    geom_bar(stat="identity", fill=rcrea::pal_crea[["Dark.blue"]]) +
    facet_wrap(~rcrea::poll_str(poll)) +
    rcrea::theme_crea_new()
  
  quicksave(glue("diagnostics/rsquared_testing_{poll}.png"), width=8, height=11)
  
}

diagnose_deweathered_availability <- function(deweathered, meas=NULL, poll, date_from="2018-01-01"){
  
  count_deweathered <- deweathered %>%
    filter(poll==!!poll) %>%
    tidyr::unnest(result) %>%
    filter(date >= date_from) %>%
    filter(variable=="trend") %>%
    filter(!is.na(value)) %>%
    filter(date < floor_date(Sys.Date(), "month")) %>%
    group_by(location_id, location_name, poll, month=floor_date(date, "month")) %>%
    summarise(availability=n_distinct(date) / lubridate::days_in_month(unique(month))) %>%
    ungroup() %>%
    tidyr::complete(nesting(location_id, location_name), poll, month, fill=list(availability=0))  %>%
    mutate(variable="deweathered")
  
  if(!is.null(meas)){
    count_meas <- meas %>%
      filter(poll==!!poll) %>%
      filter(date >= date_from) %>%
      filter(!is.na(value)) %>%
      filter(date < floor_date(Sys.Date(), "month")) %>%
      group_by(location_id, location_name, poll, month=floor_date(date, "month")) %>%
      summarise(availability=n_distinct(date) / lubridate::days_in_month(unique(month))) %>%
      ungroup() %>%
      tidyr::complete(nesting(location_id, location_name), poll, month, fill=list(availability=0)) %>%
      mutate(variable="meas")
  }else{
    count_meas <- NULL
  }
  
  
  count <- bind_rows(count_deweathered, count_meas)
  
  
  # Plot a heatmap
  ggplot(count,
         aes(x=as.character(month), y=location_name, fill=availability)) +
    geom_tile() +
    facet_wrap(~rcrea::poll_str(poll)) +
    rcrea::theme_crea_new() +
    scale_fill_distiller(palette="Blues", direction=1,
                         # Make it in percentage
                         labels=scales::percent_format(accuracy=1)
    ) +
    scale_x_discrete(breaks=as.character(seq(min(count$month), max(count$month), by="12 months")),
                     # Label as year
                     labels=as.character(year(seq(min(count$month), max(count$month), by="12 months")))
    ) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size=7),
          ) +
    labs(title=glue("Data availability for {rcrea::poll_str(poll)}"),
         subtitle="Availability of deweathered measurements",
         x=NULL,
         y=NULL,
         fill=NULL) +
    # widen legend
    theme(legend.key.width = unit(3, "cm")) +
    facet_wrap(~recode(variable, "deweathered"="Deweathered", "meas"="Measured"))
  
  quicksave(glue("diagnostics/data_availability_{poll}.png"), width=10, height=10, logo_scale = 0.025)
  
}
