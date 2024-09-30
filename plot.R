plot_yoy <- function(yoys, poll, filepath, names_at_0 = T, width=8, height=11, logo=T, type="hbars", relative=F){
  
  
  
  if(type=="hbar"){
    
    # Bar version
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
  }
  
  if(type=="dots"){
    
    # Bar version
    yoys %>%
      select(location_name, poll, variable, value=yoy) %>%
      filter(poll %in% !!poll) %>%
      filter(variable %in% c("observed", "trend") )%>%
      filter(!is.na(value)) %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      ggplot(aes(x = reorder(location_name, -trend), y = trend, group=location_name)) +
      geom_hline(yintercept = 0, linetype="solid", color="grey80") +
      # hollow point
      # geom_col(aes(y = trend), width=0.1, fill="grey90") +
      geom_point(aes(color="Observed", y=observed), size=2, shape=1, fill="white", stroke=1.5) +
      geom_point(aes(color="Weather-corrected"), size=2, shape=1, fill="white", stroke=1.5) +
      
      
      
      
      labs(title = glue("Year-on-year change in {rcrea::poll_str(poll)} concentration in NCAP cities"),
           subtitle = "Apr-Mar 2024 vs Apr-Mar 2023",
           y="µg/m³",
           x=NULL,
           color=NULL,
           caption="Source: CREA analysis based on CPCB and ERA5."
      ) +
      scale_y_continuous(
        #labels = scales::percent,
                         expand = expansion(mult = c(0.1, 0.1))) +
      scale_color_manual(values=rev(c(rcrea::pal_crea[["Dark.red"]], "grey80"))) +
      
      
      # Add label at extremity
      geom_text(aes(label=paste0(ifelse(trend > 0, "+", ""), 
                                 round(trend, 0),
                                 #if max value add unit
                                 ifelse(trend==max(trend), " µg/m³", "")
                                 # scales::percent(trend, accuracy=1.0)
                                 ),
                    y=trend,
                    vjust=ifelse(trend > observed, -1, -1)
                    
                    ),
                    # direction="y",
                size=3,
                col="grey50") +
      
      
      
      # add space on left and right
      scale_x_discrete(expand = expansion(add = c(1.5,1.5))) +
      
      rcrea::theme_crea_new()  +
      # remoe y grid
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # axis.text.y = element_text(angle = 0, hjust = 0.5)
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> plt 
    
  }
    
  
  if(type=="hbars"){
    
    plt <- yoys %>%
      filter(variable %in% c("trend", "observed")) %>%
      
      mutate(value=case_when(relative ~ yoy_rel,
                             T ~ yoy)) %>%
      filter(!is.na(value)) %>%
      filter(poll %in% !!poll) %>%
      
      # Reorder location_name by "trend" value first
      group_by(location_name) %>%
      mutate(trend_value = value[variable == "trend"]) %>%
      ungroup() %>%
      mutate(location_name = reorder(location_name, trend_value)) %>%
      
      mutate(variable=factor(variable,
                             levels=c("trend", "observed"),
                             labels=c("Weather-corrected", "Observed"))
                             ) %>%


      ggplot(aes(y = location_name, x = value, group=location_id)) +
      geom_bar(stat="identity", width=0.5, aes(fill=value>0), show.legend = F) +
      labs(title = glue("Year-on-year change in {rcrea::poll_str(poll)} concentration in NCAP cities"),
           subtitle = "Apr-Mar 2024 vs Apr-Mar 2023",
           x=ifelse(relative, "", "µg/m³"),
           y=NULL,
           caption="Source: CREA analysis based on CPCB and ERA5."
      ) +
      scale_x_continuous(
        labels = ifelse(relative, scales::percent, scales::number),
                         expand = expansion(mult = c(0.1, 0.1))
        ) +
      scale_fill_manual(values=rev(c(rcrea::pal_crea[["Dark.red"]], rcrea::pal_crea[["Dark.blue"]]))) +
      
      # Add label at extremity
      geom_text(aes(label=paste0(ifelse(value > 0, "+", ""),
                                 case_when(relative ~ scales::percent(value, accuracy=1.0), T ~ scales::number(value, accuracy=1.0))),
                    x=value,
                    hjust=ifelse(value > 0, -0.1, 1.1)),
                size=2.8,
                col="grey70") +
      rcrea::theme_crea_new() +
      
      # Use facet_grid to control facet positioning
      facet_wrap(. ~ variable, scales="free_y") 
    
    if(names_at_0){
      plt <- plt +
        # Add city name
        geom_text(aes(label=trimws(location_name),
                      x=ifelse(value > 0, -1, 1) * ifelse(relative, 0.01, 2),
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
