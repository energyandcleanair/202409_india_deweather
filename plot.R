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
              preview=T)  
  }else{
    print(plt)
  }
  
  return(plt)
}