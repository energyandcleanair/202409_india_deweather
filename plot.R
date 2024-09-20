plot_yoy <- function(yoys, poll, filepath, names_at_0 = T, width=8, height=11, logo=T){
  
  
  plt <- yoys %>%
    filter(variable == "trend") %>%
    filter(!is.na(yoy_rel)) %>%
    filter(poll %in% !!poll) %>%
    ggplot(aes(y = reorder(location_name, yoy_rel), x = yoy_rel, group=location_id)) +
    geom_bar(stat="identity", width=0.5, aes(fill=yoy_rel>0), show.legend = F) +
    labs(title = glue("Year-on-year change in {rcrea::poll_str(poll)} concentration in NCAP cities"),
         subtitle = "After removing weather effects | Apr-Mar 2024 vs Apr-Mar 2023",
         x=NULL,
         y=NULL,
         caption="Source: CREA analysis based on CPCB and ERA5."
    ) +
    scale_x_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0.1, 0.1))) +
    scale_fill_manual(values=rev(c(rcrea::pal_crea[["Dark.red"]], rcrea::pal_crea[["Dark.blue"]]))) +
    
    # Add label at extremity
    geom_text(aes(label=paste0(ifelse(yoy_rel > 0, "+", ""), scales::percent(yoy_rel, accuracy=1.0)),
                  x=yoy_rel,
                  hjust=ifelse(yoy_rel > 0, -0.1, 1.1)),
              size=2.8,
              col="grey70") +
    rcrea::theme_crea_new() 

  if(names_at_0){
    plt <- plt +
      # Add city name
      geom_text(aes(label=trimws(location_name),
                    x=ifelse(yoy_rel > 0, -0.01, 0.01),
                    y=location_name,
                    hjust = ifelse(yoy_rel > 0, 1, 0)
      ),
      size=2.8,
      col="grey20") + 
      # hide y axis
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()
      )
  }

  
  if(!is.null(filepath)){
    quicksave(plot = plt,
              file = filepath,
              width=width,
              height=height,
              logo=logo,
              preview=T,
              logo_scale = 0.025
              )  
  }else{
    print(plt)
  }
  
  return(plt)
}


plot_trends <- function(deweathered, poll, filepath, width=11, height=16, logo=T){
  
  data <- deweathered %>%
    filter(poll %in% !!poll) %>%
    # filter(location_id %in% sample(deweathered$location_id, 60)) %>%
    unnest(result) %>%
    filter(variable %in% c("observed", "trend")) %>%
    filter(!is.na(value)) %>%
    select(location_id, location_name, poll, source, date, variable, value) %>%
    rcrea::utils.running_average(30, min_values = 15)
  
  data %>%
    mutate(variable_str=tools::toTitleCase(variable)) %>%
    ggplot(aes(x = date, y = value, color = variable_str)) +
      geom_line(aes(linewidth=variable_str)) +
      labs(title = glue("{rcrea::poll_str(poll)} trend after correcting for weather conditions in NCAP cities"),
           subtitle = "30-day rolling average in µg/m³",
           x = NULL,
           y = NULL,
           color=NULL,
           linewidth=NULL,
           caption="Source: CREA analysis based on CPCB and ERA5."
      ) +
      facet_wrap(~location_name, scales = "free_y", ncol = 9) +
      rcrea::scale_y_crea_zero() +
      scale_color_manual(values=c("Observed"=rcrea::pal_crea[["Light.gray"]],
                                  "Trend"=rcrea::pal_crea[["Dark.red"]])) +
      scale_linewidth_manual(values=c("Observed"=0.2,
                                        "Trend"=0.5)) +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y", date_minor_breaks = "1 year") +
      scale_y_continuous(breaks = scales::pretty_breaks(2), limits = c(0, NA)) +
    # hide y axis
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    rcrea::theme_crea_new() +
    theme(
      # make axis text smaller
      axis.text.x = element_text(size=6, color="grey80"),
      axis.text.y = element_text(size=6, color="grey80"),
      
      # remove grid
      panel.grid.major = element_blank(),
      
      # reduce spacing between panels
      panel.spacing = unit(0.4, "cm"),
      
      # Strip text much smaller
      strip.text = element_text(size=8)
    ) -> plt
  
  

  quicksave(plot = plt,
            file = filepath,
            width=width,
            height=height,
            logo=logo,
            logo_scale=0.025,
            preview=F)    
  
  
}