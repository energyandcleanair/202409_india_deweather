factorise_variable <- function(df){
  df %>%
    mutate(variable = factor(variable,
                            levels=c("observed", "trend"),
                            labels=c("Before weather normalisation", "After weather normalisation"))
    )
}


plot_yoy <- function(yoys, poll, period, filepath, names_at_0 = T, width=8, height=11, logo=T, type="hbars", relative=F){


  subtitle <- glue("{ifelse(relative, 'Relative change', 'Change in µg/m³')} from {period}")
  if(type=="hbar"){

    # Bar version
    plt <- yoys %>%
      filter(variable == "trend") %>%
      filter(!is.na(yoy_rel)) %>%
      filter(poll %in% !!poll) %>%
      factorise_variable() %>%
      ggplot(aes(y = reorder(location_name, yoy_rel), x = yoy_rel, group=location_id)) +
      geom_bar(stat="identity", width=0.5, aes(fill=yoy_rel>0), show.legend = F) +
      labs(title = glue("Year-on-year change in {rcrea::poll_str(poll)} concentration in NCAP cities"),
           subtitle = subtitle,
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
      factorise_variable() %>%
      ggplot(aes(x = location_name, y = value, group=location_name, color=variable, fill=variable)) +
      geom_hline(yintercept = 0, linetype="solid", color="grey80") +
      # hollow point
      # geom_col(aes(y = trend), width=0.1, fill="grey90") +
      # add line between dots for each x
      geom_line(data=function(x) x %>% mutate(variable="Weather contribution"), size=2) +
      geom_point(size=3, shape=21, stroke=0.5, alpha=0.7) +


      # geom_point(aes(color="Weather-corrected"), size=2, shape=1, fill="white", stroke=1.5) +


      labs(title = glue("Year-on-year change in {rcrea::poll_str(poll)} concentration in NCAP cities"),
           subtitle = subtitle,
           y=NULL,
           x=NULL,
           color=NULL,
           fill=NULL,
           caption="Source: CREA analysis based on CPCB and ERA5."
      ) +
      scale_y_continuous(
        #labels = scales::percent,
                         expand = expansion(mult = c(0.1, 0.1))) +
      scale_color_manual(values=rev(unname(rcrea::pal_crea[c("Light.blue", "Orange", "Dark.red")]))) +
      scale_fill_manual(values=rev(unname(rcrea::pal_crea[c("Light.blue", "Orange", "Dark.red")]))) +
      # Add label at extremity
      geom_text(
        data=function(x) x %>% filter(grepl("after|trend", variable,  ignore.case = T)) %>% filter(value==max(value) | value==min(value)),
        aes(label=ifelse(variable==grepl("before|observed", variable,  ignore.case = T), "",
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

      # reverse legend order
      guides(color = guide_legend(reverse = T),
             fill = guide_legend(reverse = T)
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
      factorise_variable() %>%
      mutate(location_name = reorder(location_name, trend_value)) %>%
      ggplot(aes(y = location_name, x = value, group=location_id)) +
      geom_bar(stat="identity", width=0.5, aes(fill=value>0), show.legend = F) +
      labs(title = glue("Year-on-year change in {rcrea::poll_str(poll)} concentration in NCAP cities"),
           subtitle = subtitle,
           x=NULL,
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


plot_yoy_states <- function(yoys, poll, period, filepath, width=8, height=6, logo=T, relative=F){

    subtitle <- glue("{ifelse(relative, 'Relative change', 'Change in µg/m³')} from {period} after correcting for weather conditions")

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
      geom_text_repel(aes(label=location_name), size=1.8, color="grey20") +


      scale_color_manual(values=pal_full) +
      scale_fill_manual(values=pal_full) +
      rcrea::theme_crea_new() +
      labs(
        title = glue("Year-on-year change in {rcrea::poll_str(poll)} concentration in NCAP cities"),
        subtitle = subtitle,
        x=NULL,
        y=NULL,
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
                logo_scale = 0.03
                )
    }else{
      print(plt)
    }

    return(plt)
}

plot_yoy_national <- function(yoys, poll, period, filepath, width=8, height=6, logo=T, relative=F){


  data <- yoys %>%
    filter(poll %in% !!poll) %>%
    filter(variable %in% c("observed","trend")) %>%
    group_by(variable, poll, period) %>%
    summarise(delta = mean(yoy, na.rm=T),
              n=n()) %>%
    group_by(poll, period) %>%
    spread(variable, delta) %>%
    mutate(weather = observed - trend) %>%
    gather(variable, value, observed, trend, weather) %>%
    mutate(share= value / value[variable=="observed"]) %>%
    filter(variable != "observed") %>%
    mutate(variable = factor(variable,
                            levels=rev(c("trend", "weather")),
                            labels=rev(c("Non-weather factors", "Weather contribution")))
    ) %>%
    write_csv(glue("results/yoy_national_{poll}_{period}.csv"))

  ggplot(data, aes(x=glue("{n} cities"), y=value, fill=variable)) +
    geom_hline(yintercept = 0, linetype="solid", color="grey80") +
    geom_col(width=0.2, show.legend = F) +
    # geom_text in the middle of the bar
    geom_text(aes(

      label=glue("{variable}\n{round(value, 1)} µg/m³\n({scales::percent(share, accuracy=1.0)})"),
              y=value, colour=variable),
              hjust=0.5,
              size=3,
              position = position_stack(vjust = 0.5),
              show.legend = F
              ) +
    rcrea::theme_crea_new() +
    labs(title = glue("Weather contribution to the measured reduction in {rcrea::poll_str(poll)}"),
         subtitle=glue("Average over {unique(data$n)} cities for the period {unique(data$period)}"),
         x=NULL,
         y=NULL,
         fill=NULL,
         caption="Source: CREA analysis based on CPCB and ERA5."
    ) +
    # remove grid and axis
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_blank(),
      # remove panel border
      panel.border = element_blank()
    ) +
    scale_fill_manual(values=unname(rcrea::pal_crea[c("Light.blue", "Dark.red")])) +
    scale_color_manual(values=c("black", "white"))


  quicksave(
    plot = last_plot(),
    file = filepath,
    width=width,
    height=height,
    logo=logo,
    preview=T,
    logo_scale = 0.025
  )
}



plot_timeseries <- function(deweathered, poll, filepath, yoys=NULL, running_days=30, width=11, height=14, ncol=8, logo=T){

  data <- deweathered %>%
    filter(poll %in% !!poll) %>%
    {
      if(!is.null(yoys)){
        inner_join(., yoys %>% filter(!is.na(yoy), variable=="trend") %>% distinct(location_id, poll))
      }else{
        .
      }
    } %>%
    # filter(location_id %in% sample(deweathered$location_id, 60)) %>%
    unnest(result) %>%
    filter(variable %in% c("observed", "trend")) %>%
    factorise_variable() %>%
    select(location_id, location_name, poll, source, date, variable, value) %>%
    tidyr::complete(
      nesting(location_id, location_name, poll, source, variable),
      date=seq(min(date), max(date), by="1 day"),
      fill=list(value=NA)) %>%
    rcrea::utils.running_average(running_days, min_values = running_days/2)

  data %>%
    ggplot(aes(x = date, y = value, color = variable)) +
      geom_line() +
      labs(title = glue("{rcrea::poll_str(poll)} trend in NCAP cities"),
           subtitle = glue("Before and after normalising for weather conditions  | {running_days}-day rolling average in µg/m³"),
           x = NULL,
           y = NULL,
           color=NULL,
           linewidth=NULL,
           caption="Source: CREA analysis based on CPCB and ERA5."
      ) +
      facet_wrap(~location_name, scales = "free_y", ncol = ncol) +
      rcrea::scale_y_crea_zero() +
    scale_color_manual(values=c(rcrea::pal_crea[["Orange"]],rcrea::pal_crea[["Dark.red"]])) +
    scale_x_date(breaks = scales::pretty_breaks(3), date_labels = "%Y") +
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

      legend.position = "top",

      # Strip text smaller
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

plot_timeseries_yearly <- function(deweathered, poll, filepath, yoys=NULL, width=11, height=14, ncol=5, logo=T){

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
      inner_join(yoys %>% filter(!is.na(yoy), variable=="trend") %>% distinct(location_id, poll))
  }

  data %>%
    factorise_variable() %>%
    ggplot(aes(x = year, y = value, color = variable)) +
    geom_line(aes(color=variable), alpha=0.7, linewidth=0.7) +
    labs(title = glue("Yearly average of {rcrea::poll_str(poll)} ambient concentration"),
         subtitle = "Before and after normalising for weather conditions in µg/m³",
         x = NULL,
         y = NULL,
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
            logo_scale=0.025,
            preview=F)


}


plot_trends <- function(trends, poll, variable="trend", p_min=0.1, filepath, by="city", width=8, height=6, logo=T){


  min_year <- year(min(trends$date_from))

  trends %>%
    filter(poll==!!poll) %>%
    filter(variable %in% !!variable) %>%
    add_state() %>%
    filter(p < p_min) -> data

  n_states <- data %>% pull(state) %>% unique() %>% length()
  pal <- rcrea::pal_crea
  idx <- match(c("Yellow", "Light.gray", "Light.blue"), names(pal))
  pal <- pal[-idx]
  pal_full <- colorRampPalette(pal)(n_states)


  if(by=="city"){
    data %>%
      ggplot(aes(x=reorder(location_name, slope), color=state)) +
      geom_errorbar(aes(ymin=slope_lower, ymax=slope_upper), width=0.2, show.legend = F) +
      geom_point(aes(y=slope), show.legend = F) +
      geom_hline(yintercept = 0, linetype="solid", color="grey80") +
      {
        if(length(variable) > 1){
          facet_wrap(~variable)
        }
      } +
      scale_color_manual(values=pal_full) +
      rcrea::theme_crea_new()  +
      theme(
        panel.grid.minor.y = element_line(color="grey90", size=0.1)
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top"
      ) +
      labs(
        title = glue("Trend of {rcrea::poll_str(poll)} concentration in NCAP cities"),
        subtitle = glue("Yearly average change expressed in µg/m³ per year after weather-correction"),
        x=NULL,
        y=NULL,
        caption=paste0(
          c(glue("The trend is calculated using the Theil-Sen estimator, since {min_year} or earliest available measurement."),
            glue("Only trends with p < {p_min} are shown."),
            "The error bars represent the 95% confidence interval of the trend.",
            "Source: CREA analysis based on CPCB and ERA5."),
          collapse="\n")
      )
  }

  if(by=="state"){
    # plot boxplot of yoy color per state
    ggplot(data, aes(x=reorder(state, -slope, FUN=median), y=slope, fill=state)) +

      geom_hline(yintercept = 0, linetype="solid", color="grey80") +
      # horizonral line at median for each state
      # geom_errorbar(aes(ymin=median(yoy), ymax=median(yoy), col=state, group=state), width=0.2, size=0.5) +
      stat_summary(aes(y = slope, ymax = after_stat(y), ymin = after_stat(y), col=state),
                   fun = median, geom = "col",
                   linewidth = 0.1,
                   show.legend = F, alpha=0.5) +

      # show jittered dots
      geom_jitter(width=0, height=0, alpha=0.9, size=3, aes(col=state), show.legend = F) +
      geom_text_repel(aes(label=location_name), size=2.5, color="grey20") +
      scale_color_manual(values=pal_full) +
      scale_fill_manual(values=pal_full) +
      rcrea::theme_crea_new() +
      labs(
        title = glue("Trend of {rcrea::poll_str(poll)} concentration in NCAP cities"),
        subtitle = glue("Yearly average change expressed in µg/m³ per year after weather-correction"),
        x=NULL,
        y=NULL,
        # add explanation of box plot
        caption=paste0(
          c(glue("The trend is calculated using the Theil-Sen estimator, since {min_year} or earliest available measurement."),
            glue("Only trends with p < {p_min} are shown."),
            "The bar represents the median trend for each state. The dots represent the trend for each city.",
            "Source: CREA analysis based on CPCB and ERA5."),
          collapse="\n")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }


  quicksave(plot = last_plot(),
            file = filepath,
            width=width,
            height=height,
            logo=logo,
            logo_scale=0.025,
            preview=F)
}

plot_trends_map <- function(trends, poll, p_min=0.1, variable="trend", filepath, width=8, height=6, logo=T){

  min_year <- year(min(trends$date_from))


  data_sf <- trends %>%
    filter(poll==!!poll) %>%
    filter(variable %in% !!variable) %>%
    add_geometry() %>%
    mutate(slope=case_when(p < p_min ~ slope,
                          T ~ NA_real_)) %>%
    select(location_name, p, slope, geometry) %>%
    sf::st_as_sf()

  # Get provincial borders of India
  india <- rnaturalearth::ne_countries(scale = "medium", country = "India", returnclass = "sf")
  provinces <- rnaturalearth::ne_states(country = "India", returnclass = "sf")
  provinces_labels <- rnaturalearth::ne_states(country = "India", returnclass = "sf") %>%
    dplyr::select(name) %>%
    mutate(name=recode(name,
      `Dadra and Nagar Haveli and Daman and Diu`= "Dadra and Nagar Haveli"
    )) %>%
    sf::st_centroid()



  # Create divergent scale centered on 0
  bound <- max(abs(range(data_sf$slope, na.rm=T)))


  # plot on background with provincial borders
  ggplot() +
    # fill with very pale yellow
    geom_sf(data=provinces, fill="#f5f5f5", color="grey60",
            aes(label=name)
            ) +

    # add labels
    geom_sf_text(data=provinces_labels, aes(label=name),
                 size=1.5, color="grey60") +
    # geom_sf(data=india, fill="transparent", color="grey60") +
    theme_minimal() +
    rcrea::theme_crea_new() +

    # filled points
    # Draw the NA values first (for missing slopes)
    # geom_sf(data=data_sf[is.na(data_sf$slope),], fill="gray90", shape=21, size=2, stroke=0) +

    # Draw valid slope points
    geom_sf(data=data_sf[!is.na(data_sf$slope),], aes(fill=slope), shape=21, size=2, stroke=0) +

    # scale_fill_distiller(palette="RdBu", limits=c(-bound, bound), na.value="grey60") +
    # scale_fill_gradient2(
    #   low = "#313695",   # Dark blue for extreme negative values
    #   mid = "#f7f7f7",   # Light gray for near-zero (neutral) values
    #   high = "#a50026",  # Light red for small positive values
    #   midpoint = 0,      # Centered at 0
    #   limits = c(-bound, bound),  # Ensure scale covers all trends symmetrically
    #   # space = "Lab",     # Color space for smooth transitions
    #   na.value = "gray90",  # Handling missing data (optional)
    #   guide = "colorbar",
    #   aesthetics = "fill"
    # ) +
  # scale_fill_distiller(palette="RdBu", limits=c(-bound, bound), na.value="grey60") +
  # Turbo colors
  scale_fill_viridis_c(
    option = "turbo",  # Use the turbo color palette
    limits = c(-bound, bound),  # Ensure scale covers all trends symmetrically
    direction = 1,     # Normal direction for color scaling
    na.value = "gray90",  # Handling missing data
    guide = "colorbar"
  ) +    # remove axis grid
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.5, "cm"),
          # increase legend width
          legend.key.width = unit(2, "cm"),

          ) +
    labs(
      title = glue("Trend of {rcrea::poll_str(poll)} concentration in NCAP cities"),
      subtitle = glue("Yearly average change expressed in µg/m³ per year after weather-correction"),
      caption=paste0(
        c(glue("The trend is calculated using the Theil-Sen estimator, since {min_year} or earliest available measurement."),
          glue("Only trends with p < {p_min} are shown."),
          "Source: CREA analysis based on CPCB and ERA5."),
        collapse="\n"),
      x=NULL,
      y=NULL
    )

  quicksave(plot = last_plot(),
            file = filepath,
            width=width,
            height=height,
            logo=logo,
            logo_scale=0.025,
            preview=F)
}

plot_timeseries_w_aod <- function(meas, poll, filepath, min_years=NULL, yoys=NULL, running_days=30, width=11, height=14, ncol=8, logo=T){

  data <- meas %>%
    filter(grepl("aod", poll) | poll==!!poll) %>%
    filter(date >= "2017-01-01") %>%
    ungroup() %>%
    select(location_id, location_name, poll, source, date, variable, value) %>%
    tidyr::complete(
      nesting(location_id, location_name, poll, source, variable),
      date=seq(min(date), max(date), by="1 day"),
      fill=list(value=NA)) %>%
    rcrea::utils.running_average(running_days, min_values=running_days/2) %>%
    mutate(variable=case_when(grepl("aod", poll) ~ toupper(gsub("_"," ",poll)),
                              T ~ rcrea::poll_str(poll))) %>%
    {
      if(!is.null(yoys)){
        inner_join(., yoys %>% filter(!is.na(yoy), variable=="trend") %>% distinct(location_id))
      }else{
        .
      }
    } %>%
    {
      if(!is.null(min_years)){
        locs_filtered <- filter(., !grepl("AOD", variable)) %>%
          filter(!is.na(value)) %>%
          group_by(location_id) %>%
          summarise(days = (max(date) - min(date)) %>% as.numeric()) %>%
          filter(days >= min_years * 365) %>%
          pull(location_id)

        filter(., location_id %in% locs_filtered)
      }else{
        .
      }
    } %>%
    # Keep only since observed data is available
    group_by(location_name) %>%
    filter(date >= min(date[!grepl("AOD", variable) & !is.na(value)])) %>%
    # standardise by location/poll
    group_by(location_name, poll, variable) %>%
    mutate(value = scale(value)) %>%
    mutate(variable_str=tools::toTitleCase(variable))


  ggplot(data, aes(x = date, y = value, color = variable_str)) +
    geom_line(linewidth=0.4) +
    labs(title = glue("Trends in {rcrea::poll_str(poll)} and Aerosol Optical Depth in NCAP cities"),
         subtitle = glue("Standardised values | {running_days}-day rolling average"),
         x = NULL,
         y = NULL,
         color=NULL,
         linewidth=NULL,
         caption="Source: CREA analysis based on CPCB, CAMS and MODIS."
    ) +
    facet_wrap(~location_name, scales = "free_y", ncol = ncol) +
    scale_color_manual(values=unname(rcrea::pal_crea[c( "Dark.blue", "Orange", "Green", "Turquoise")])) +
    # prettu breaks for years
    scale_x_date(breaks = scales::pretty_breaks(3), date_labels = "%Y") +
    # scale_y_continuous(breaks = scales::pretty_breaks(2), limits = c(0, NA)) +
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

      # Strip text smaller
      strip.text = element_text(size=8),

      # Legend at top
      legend.position = "top"
    )


  quicksave(plot = last_plot(),
            file = filepath,
            width=width,
            height=height,
            logo=logo,
            logo_scale=0.025,
            preview=F
            )
}



plot_trends_w_aod <- function(trends_aod, poll, filepath, max_p=0.1, width=8, height=6, logo=T){


  trends_aod %>%
    ungroup() %>%
    filter(grepl("aod", poll, ignore.case=T) | poll==!!poll) %>%
    filter(p < max_p) %>%
    select(location_id, location_name, poll, slope) %>%
    spread(poll, slope) %>%
    add_state() -> data

  n_states <- data %>% pull(state) %>% unique() %>% length()
  pal <- rcrea::pal_crea
  idx <- match(c("Yellow", "Light.gray", "Light.blue"), names(pal))
  pal <- pal[-idx]
  pal_full <- colorRampPalette(pal)(n_states)


  data %>%
    ggplot(aes(x=pm10, y=modis_aod_550, color=state)) +
    # geom_errorbar(aes(ymin=slope_lower, ymax=slope_upper), width=0.2, show.legend = F) +
    geom_point(show.legend = F) +
    geom_abline(intercept = 0, slope = 1, linetype="dashed", color="grey80") +
    scale_color_manual(values=pal_full) +
    rcrea::theme_crea_new()  +
    theme(
      panel.grid.minor.y = element_line(color="grey90", size=0.1)
    ) +
    geom_text_repel(aes(label=location_name), size=2.5, color="grey20") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top"
    ) +
    labs(
      title = glue("Trend of {rcrea::poll_str(poll)} concentration in NCAP cities"),
      subtitle = glue("Yearly average change expressed normalized values per year"),
      x="Normalised trend in PM10",
      y="Normalised trend in AOD",
      caption=paste0(
        c(glue("The trend is calculated using the Theil-Sen estimator. Only trends with p < {max_p} are shown."),
          "Source: CREA analysis based on CPCB and MODIS."),
        collapse="\n")
    )

  quicksave(plot = last_plot(),
            file = filepath,
            width=width,
            height=height,
            logo=logo,
            logo_scale=0.025,
            preview=F)
}


