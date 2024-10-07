plot_yoy <- function(yoys, poll, period, filepath, names_at_0 = T, width=8, height=11, logo=T, type="hbars", relative=F){


  if(type=="hbar"){

    # Bar version
    plt <- yoys %>%
      filter(variable == "trend") %>%
      filter(!is.na(yoy_rel)) %>%
      filter(poll %in% !!poll) %>%
      ggplot(aes(y = reorder(location_name, yoy_rel), x = yoy_rel, group=location_id)) +
      geom_bar(stat="identity", width=0.5, aes(fill=yoy_rel>0), show.legend = F) +
      labs(title = glue("Year-on-year change in {rcrea::poll_str(poll)} concentration in NCAP cities"),
           subtitle = glue("After removing weather effects | {period}"),
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
      mutate(location_name = fct_reorder(location_name, -trend)) %>%
      gather(variable, value, observed, trend) %>%
      ggplot(aes(x = location_name, y = value, group=location_name, color=variable, fill=variable)) +
      geom_hline(yintercept = 0, linetype="solid", color="grey80") +
      # hollow point
      # geom_col(aes(y = trend), width=0.1, fill="grey90") +
      # add line between dots for each x
      geom_line(data=function(x) x %>% mutate(variable="Weather contribution"), size=2) +
      geom_point(size=3, shape=21, stroke=0.5, alpha=0.7) +


      # geom_point(aes(color="Weather-corrected"), size=2, shape=1, fill="white", stroke=1.5) +


      labs(title = glue("Year-on-year change in {rcrea::poll_str(poll)} concentration in NCAP cities"),
           subtitle = glue("Before and after removing weather effects | {period}"),
           y="µg/m³",
           x=NULL,
           color=NULL,
           fill=NULL,
           caption="Source: CREA analysis based on CPCB and ERA5."
      ) +
      scale_y_continuous(
        #labels = scales::percent,
                         expand = expansion(mult = c(0.1, 0.1))) +
      scale_color_manual(values=rev(unname(rcrea::pal_crea[c("Light.blue", "Dark.red", "Orange")]))) +
      scale_fill_manual(values=rev(unname(rcrea::pal_crea[c("Light.blue", "Dark.red", "Orange")]))) +



      # Add label at extremity
      geom_text(
        data=function(x) x %>% filter(variable=="trend") %>% filter(value==max(value) | value==min(value)),
        aes(label=ifelse(variable=="observed", "",
                            paste0(ifelse(value > 0, "+", ""),
                                 round(value, 0),
                                 #if max value add unit
                                 ifelse(value==max(value) |value==min(value), " µg/m³", "")
                                 # scales::percent(trend, accuracy=1.0)
                                 )),
                    # y=trend,
                    vjust=-1

                    ),
                    direction="y",
                size=3,
        show.legend = F
                # min.segment.length=0,
                # no segment
                # segment.size=0,
                # min distance
                # box.padding=0.3,

                # col="grey80"#rcrea::pal_crea[["Dark.red"]]
                ) +



      # add space on left and right
      scale_x_discrete(expand = expansion(add = c(1.5,1.5))) +


      rcrea::theme_crea_new()  +
      # remoe y grid
      theme(
        # panel.grid.major.y = element_blank()
        panel.grid.minor.y = element_line(color="grey90", size=0.1)
        # axis.text.y = element_text(angle = 0, hjust = 0.5)
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"
            ) -> plt

    plt
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
           subtitle = glue("Before and after removing weather effects | {period}"),
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
              logo_scale = 0.035
              )
  }else{
    print(plt)
  }

  return(plt)
}


plot_yoy_states <- function(yoys, poll, period, filepath, width=8, height=6, logo=T, relative=F){

    data <- yoys %>%
      add_state() %>%
      filter(variable == "trend",
             poll %in% !!poll)

      # gather(key="variable", value="value", yoy, yoy_rel) %>%
      # mutate(variable=recode(variable, yoy="Absolute change in µg/m3",
      #                        yoy_rel="Relative change"))
      #

    n_states <- data %>% pull(state) %>% unique() %>% length()
    pal <- rcrea::pal_crea
    # remove Yellow and Light.gray
    idx <- match(c("Yellow", "Light.gray", "Light.blue"), names(pal))
    pal <- pal[-idx]

    pal_full <- colorRampPalette(pal)(n_states)


    # plot boxplot of yoy color per state
    ggplot(data, aes(x=reorder(state, -yoy, FUN=median), y=yoy, fill=state)) +

      geom_hline(yintercept = 0, linetype="solid", color="grey80") +
      # horizonral line at median for each state
      # geom_errorbar(aes(ymin=median(yoy), ymax=median(yoy), col=state, group=state), width=0.2, size=0.5) +
      stat_summary(aes(y = yoy, ymax = after_stat(y), ymin = after_stat(y), col=state),
                   fun = median, geom = "col",
                   linewidth = 0.1,
                   show.legend = F, alpha=0.5) +

      # show jittered dots
      geom_jitter(width=0, height=0, alpha=0.9, size=3, aes(col=state), show.legend = F) +


      scale_color_manual(values=pal_full) +
      scale_fill_manual(values=pal_full) +
      rcrea::theme_crea_new() +
      labs(
        title = glue("Year-on-year change in {rcrea::poll_str(poll)} concentration in NCAP cities"),
        subtitle = glue("After removing weather effects | {period}"),
        x=NULL,
        y="µg/m³",
        # add explanation of box plot
        caption=paste0(
          c("The bar represents the median value for each state. The dots represent individual cities.",
            "Source: CREA analysis based on CPCB and ERA5."),
          collapse="\n")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))-> plt


    plt
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

plot_trends <- function(deweathered, poll, filepath, yoys=NULL, running_days=30, width=11, height=14, ncol=8, logo=T){

  data <- deweathered %>%
    filter(poll %in% !!poll) %>%
    # filter(location_id %in% sample(deweathered$location_id, 60)) %>%
    unnest(result) %>%
    filter(variable %in% c("observed", "trend")) %>%
    filter(!is.na(value)) %>%
    select(location_id, location_name, poll, source, date, variable, value) %>%
    rcrea::utils.running_average(running_days, min_values = running_days/2)

  if(!is.null(yoys)){
    data <- data %>%
      inner_join(yoys %>% filter(!is.na(yoy), variable=="trend") %>% distinct(location_id, poll), by="location_id")
  }

  data %>%
    mutate(variable_str=tools::toTitleCase(variable)) %>%
    ggplot(aes(x = date, y = value, color = variable_str)) +
      geom_line(aes(linewidth=variable_str)) +
      labs(title = glue("{rcrea::poll_str(poll)} trend after correcting for weather conditions in NCAP cities"),
           subtitle = glue("{running_days}-day rolling average in µg/m³"),
           x = NULL,
           y = NULL,
           color=NULL,
           linewidth=NULL,
           caption="Source: CREA analysis based on CPCB and ERA5."
      ) +
      facet_wrap(~location_name, scales = "free_y", ncol = ncol) +
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

plot_trends_yearly <- function(deweathered, poll, filepath, yoys=NULL, width=11, height=14, ncol=5, logo=T){

  data <- deweathered %>%
    filter(poll %in% !!poll) %>%
    # filter(location_id %in% sample(deweathered$location_id, 60)) %>%
    unnest(result) %>%
    filter(variable %in% c("observed", "trend")) %>%
    filter(!is.na(value)) %>%
    group_by(location_id, location_name, poll, year=year(date), variable) %>%
    summarise(value = mean(value),
              n = n()
              ) %>%
    filter(n >= 365 * 0.7) %>%
    ungroup() %>%
    group_by(location_id) %>%
    complete(year = seq(min(year), max(year)),
             location_name,
             poll,
             variable,
             fill=list(value=NA))

  if(!is.null(yoys)){
    data <- data %>%
      inner_join(yoys %>% filter(!is.na(yoy), variable=="trend") %>% distinct(location_id, poll), by="location_id")
  }

  data %>%

    mutate(variable=factor(variable,
                           levels=c("observed", "trend"),
                           labels=c("Observed", "After weather correction"))) %>%
    ggplot(aes(x = year, y = value, color = variable)) +
    geom_line(aes(color=variable), alpha=0.7, linewidth=1) +
    labs(title = glue("Yearly average of {rcrea::poll_str(poll)} ambient concentration"),
         subtitle = "Before and after correcting for weather conditions in NCAP cities",
         x = NULL,
         y = "µg/m³",
         color=NULL,
         linewidth=NULL,
         caption="Only NCAP cities and years with at least 70% data availability are shown.\nSource: CREA analysis based on CPCB and ERA5. "
    ) +
    facet_wrap(~location_name, scales = "free_y", ncol = ncol) +
    scale_color_manual(values=c(rcrea::pal_crea[["Orange"]],rcrea::pal_crea[["Dark.red"]])) +
    # scale_linewidth_manual(values=c("observed"=0.4,
    #                                 "trend"=0.5)) +
    # scale_x_date(date_breaks = "2 year", date_labels = "%Y", date_minor_breaks = "1 year") +
    scale_y_continuous(breaks = scales::pretty_breaks(2), limits = c(0, NA)) +
    scale_x_continuous(breaks = scales::pretty_breaks(3), limits = c(2015, 2024)) +
    # hide y axis
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    rcrea::theme_crea_new() +
    theme(
      # make axis text smaller
      axis.text.x = element_text(size=6, color="grey40"),
      axis.text.y = element_text(size=6, color="grey40"),

      # remove grid
      panel.grid.major = element_blank(),

      # reduce spacing between panels
      panel.spacing = unit(0.4, "cm"),

      # Strip text much smaller
      strip.text = element_text(size=8),

      # Legend at the top
      legend.position = "top"
    )



  quicksave(plot = last_plot(),
            file = filepath,
            width=width,
            height=height,
            logo=logo,
            logo_scale=0.03,
            preview=F)


}



