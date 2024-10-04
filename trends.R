get_theil_sen <- function(deweathered, poll, min_rsquared_testing=0.6, min_years=3){

  theil_trends <- deweathered %>%
    filter(poll==!!poll) %>%
    tidyr::unnest(performances) %>%
    tidyr::unnest_wider(performances) %>%
    filter(rsquared_testing >= min_rsquared_testing) %>%
    select(location_id, location_name, poll, result) %>%
    unnest(result) %>%
    mutate(date_num = as.numeric(date)) %>%
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
        data.thresh=50
      )
      tibble(
        slope = d$data$res2$slope,
        slope_lower = d$data$res2$lower,
        slope_upper = d$data$res2$upper,
        p.stars = d$data$res2$p.stars,
        p = d$data$res2$p
      )
    })


  theil_trends %>%
    add_state() %>%
    filter(p < 0.1) %>%
    ggplot(aes(x=reorder(location_name, slope), color=state)) +
    geom_errorbar(aes(ymin=slope_lower, ymax=slope_upper), width=0.2) +
    geom_point(aes(y=slope)) +
    facet_wrap(~variable) +
    # geom_col(aes(y=slope, fill=variable), position="dodge") +

    rcrea::theme_crea_new()  +
    # remoe y grid
    theme(
      # panel.grid.major.y = element_blank()
      panel.grid.minor.y = element_line(color="grey90", size=0.1)
      # axis.text.y = element_text(angle = 0, hjust = 0.5)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top"
    )


  theil_trends %>%
    filter(variable=="trend") %>%
    add_state() %>%
    filter(p < 0.1) -> data

  n_states <- data %>% pull(state) %>% unique() %>% length()
  pal <- rcrea::pal_crea
  # remove Yellow and Light.gray
  idx <- match(c("Yellow", "Light.gray", "Light.blue"), names(pal))
  pal <- pal[-idx]

  pal_full <- colorRampPalette(pal)(n_states)


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
    geom_text_repel(aes(label=location_name), size=2.5, color="grey35") +
    scale_color_manual(values=pal_full) +
    scale_fill_manual(values=pal_full) +
    rcrea::theme_crea_new() +
    labs(
      title = glue("Trend of {rcrea::poll_str(poll)} concentration in NCAP cities"),
      subtitle = glue("Yearly change expressed in µg/m³ per year after weather-correction"),
      x=NULL,
      y="µg/m³",
      # add explanation of box plot
      caption=paste0(
        c("The trend is calculated using the Theil-Sen estimator. Only trends with p < 0.1 are shown.",
          "The bar represents the median trend for each state. The dots represent the trend for each city.",
          "Source: CREA analysis based on CPCB and ERA5."),
        collapse="\n")
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

}
