diagnose_deweathering_performance <- function(deweathered, poll,
                                              yoys=NULL,
                                              min_r2=NULL,
                                              filepath=glue("diagnostics/rsquared_testing_{poll}.png"),
                                              height=9, width=10
                                              ){

  performance <- deweathered %>%
    filter(poll==!!poll) %>%
    unnest(performances) %>%
    unnest_wider(performances) %>%
    select(location_id, location_name, rmse_testing, rsquared_testing, poll) %>%
    {
      if(!is.null(min_r2)){
        filter(., rsquared_testing >= min_r2)
      }else{
        .
      }
    } %>%
    mutate(location_name=reorder(location_name, rsquared_testing)) %>%
    tidyr::pivot_longer(cols=c(rmse_testing, rsquared_testing),
                        names_to="metric",
                        values_to="value") %>%
    mutate(metric=recode(metric, "rmse_testing"="RMSE (µg/m³)", "rsquared_testing"="R²"))

  if(!is.null(yoys)){
    performance <- performance %>%
     inner_join(yoys %>% filter(!is.na(yoy)) %>% distinct(location_id, poll))
  }


  ggplot(performance,
         # reorder location_id based on rsauqred_testing
         aes(y=location_name,
             x=value,
             fill=metric)
         ) +
    geom_bar(stat="identity", show.legend = F) +
    geom_text(aes(label=sprintf(ifelse(metric=="R²", "%.2f", "%.0f"), value)),
             hjust=-0.1,
              size=3,
             color="grey40"
             ) +
    facet_wrap(~metric, scales="free") +
    rcrea::theme_crea_new() +
    rcrea::scale_fill_crea_d() +
    scale_x_continuous(expand=expansion(mult=c(0, 0.1))) +
    labs(
      title=glue("Performance of deweathering models on validation data for {rcrea::poll_str(poll)}"),
      x=NULL,
      y=NULL,
      fill=NULL,
      caption="Source: CREA analysis"
    )

  quicksave(filepath,
            width=width,
            height=height,
            logo_scale = 0.025)

}

diagnose_deweathered_availability <- function(deweathered, meas=NULL, poll, date_from="2015-01-01"){

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


diagnose_models_importance <- function(deweathered,
                                       poll,
                                       yoys=NULL,
                                       min_r2=NULL,
                                       filepath=glue("diagnostics/importance_{poll}.png"),
                                       height=9,
                                       width=10
){


 deweathered %>%
    filter(poll==!!poll) %>%
    add_state() %>%
    unnest(performances) %>%
    unnest_wider(performances) %>%
    {
      if(!is.null(min_r2)){
        filter(., rsquared_testing >= min_r2)
      }else{
        .
      }
    } %>%
    # unnest(models) %>%
    rowwise() %>%
    mutate(importance=models$importance) %>%
    ungroup() %>%
   select(location_id, importance, state) %>%
   unnest(importance) %>%
    ggplot(aes(y=rel_inf,
               x=reorder(var, -rel_inf, FUN=median),
               col=var),
           show.legend=F) +
    geom_boxplot(show.legend = F) +
    geom_jitter(alpha=0.5, show.legend = F) +
    facet_wrap(~state) +
    rcrea::theme_crea_new() +
    labs(
      title=glue("Importance of variables in deweathering models for {rcrea::poll_str(poll)}"),
      x=NULL,
      y=NULL,
      caption="Source: CREA analysis"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  quicksave(filepath, width=width, height=height, logo_scale = 0.025)

}
