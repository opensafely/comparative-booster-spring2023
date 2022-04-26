# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Purpose: To assess geograhpcial distribution of moderna / pfizer booster vaccines
## by region, STP, MSOA
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Import libraries ----
library('tidyverse')
library('lubridate')
library('arrow')
library('here')

# Import custom user functions from lib
source(here("lib", "functions", "utility.R"))

# Import design elements
source(here("lib", "design", "design.R"))

# output processed data to rds ----
output_dir <- here("output", "geography")
fs::dir_create(output_dir)



## Import processed data ----

data_processed <- read_rds(here("output", "data", "data_processed.rds"))


data_regional <-
  data_processed %>%
  filter(vax3_type %in% c("pfizer", "moderna")) %>%
  mutate(
    treatment=vax3_type=="moderna",
    vax3_week = floor_date(vax3_date, "weeks", week_start = 5) # start on friday as this was first day moderna was used
  )



summary <-
  data_regional %>%
  group_by(
    treatment, region, vax3_week
  ) %>%
  summarise(
    n=n()
  ) %>%
  ungroup() %>%
  complete(treatment, region, vax3_week, fill = list(n = 0)) %>%
  group_by(region, vax3_week) %>%
  mutate(
    N = sum(n),
    pct=n/N,
  ) %>%
  filter(treatment)


regional <- function(data, region, period, region_name = "geography", period_name="Period", suppress_region_label=FALSE){

  region_var <- deparse(substitute(region))

  print(region_var)
  summary <-
    data %>%
    group_by(
      treatment, {{region}}, {{period}}
    ) %>%
    summarise(
      n=n()
    ) %>%
    ungroup() %>%
    complete(treatment, {{region}}, {{period}}, fill = list(n = 0)) %>%
    group_by({{region}}, {{period}}) %>%
    mutate(
      N = sum(n),
      pct = n/N,
    )

  write_csv(summary, fs::path(output_dir, glue("{region_var}_proportions.csv")))

  summary <- summary %>% filter(treatment)

  summary_noregion <-
    data %>%
    group_by(
      treatment, {{period}}
    ) %>%
    summarise(
      n=n()
    ) %>%
    ungroup() %>%
    complete(treatment, {{period}}, fill = list(n = 0)) %>%
    group_by({{period}}) %>%
    mutate(
      N = sum(n),
      pct = n/N,
    ) %>%
    filter(treatment)

  #background_summary <- summary %>% ungroup() %>% select(-{{period}})

  plot_line <-
    ggplot(summary)+
    geom_segment(
      data=summary %>% ungroup() %>% select(-{{region}}),
      aes(
        x=pct,
        xend=pct,
        y={{period}}-3,
        yend={{period}}+3,
        colour=factor({{period}}),
      ),
      alpha=0.2,
      size=1
    )+
    geom_segment(
      aes(
        x=pct,
        xend=pct,
        y={{period}}-4,
        yend={{period}}+4,
        colour=factor({{period}}),
      ),
      #shape="|",
      size=1
    ) +
    geom_hline(yintercept=0, colour="darkgrey")+
    facet_grid(rows=vars({{region}}), switch="y")+
    scale_colour_viridis_d()+
    scale_y_date(date_breaks="4 weeks")+
    labs(
      x="Moderna, proportion",
      y=period_name,
      colour=period_name
    )+
    theme_minimal() +
    theme(
      axis.text.x.top=element_text(hjust=0),
      strip.text.y.left = if(suppress_region_label){element_blank()} else{element_text(angle = 0, hjust=1)},
      strip.placement = "outside",
      axis.text.y = element_blank()

    )



  plot_bar <-
    ggplot(summary)+
    geom_col(
      aes(
        x=pct,
        y={{region}},
        fill=factor({{period}}),
      ),
      width=0.95,
      colour="transparent"
    )+
    geom_vline(xintercept=0, colour="darkgrey")+
    geom_vline(xintercept=1, colour="darkgrey")+
    facet_grid(cols=vars({{period}}), switch="y")+
    scale_fill_viridis_d()+
    labs(
      x="Moderna, proportion",
      y=NULL,
      fill=period_name
    )+
    theme_minimal() +
    theme(
      axis.text.x.top=element_text(hjust=0),
      strip.placement = "outside",
      strip.text.x.top = element_blank(),
      axis.text.y = if(suppress_region_label){element_blank()} else{element_text(angle = 0, hjust=1)},

    )


  plot_overlay <-
    ggplot(summary)+
    geom_line(
      aes(
        x={{period}},
        y=pct,
        group={{region}},
        #colour={{region}}
      ),
      colour='grey'
    )+
    geom_line(
      data=summary_noregion,
      aes(
        x={{period}},
        y=pct,
        #colour={{region}}
      ),
      colour='black'
    )+
    geom_hline(yintercept=0, colour="darkgrey")+
    geom_hline(yintercept=1, colour="darkgrey")+
    labs(
      x=period_name,
      y="Moderna, proportion",
    )+
    theme_minimal()




  plot_funnel <-
    ggplot(summary)+
    geom_point(
      data=summary %>% ungroup() %>% select(-{{period}}),
      aes(x=N, y=pct),
      colour='grey', alpha=0.25
    )+
    geom_point(
      aes(
        x=N,
        y=pct,
        colour=factor({{period}}),
      ),
    ) +

    geom_hline(yintercept=0, colour="darkgrey")+
    geom_hline(yintercept=1, colour="darkgrey")+
    facet_wrap(vars({{period}}))+
    scale_colour_viridis_d()+
    labs(
      x="Doses, N",
      y="Moderna, proportion",
      colour=period_name
    )+
    theme_minimal() +
    theme(
      axis.text.x.top=element_text(hjust=0),
      strip.text.y = element_text(angle = 0, hjust=0),
      legend.position="none"
    )


  #plot_line
  #plot_bar
  #plot_overlay
  #plot_funnel

  ggsave(
    filename = fs::path(output_dir, glue("{region_var}_line.png")),
    plot=plot_line
  )

  ggsave(
    filename = fs::path(output_dir, glue("{region_var}_bar.png")),
    plot=plot_bar
  )

  ggsave(
    filename = fs::path(output_dir, glue("{region_var}_overlay.png")),
    plot=plot_overlay
  )

  ggsave(
    filename = fs::path(output_dir, glue("{region_var}_funnel.png")),
    plot=plot_funnel
  )

}

regional(data_regional, region, vax3_week, "NHS region", "Calendar week", suppress_region_label=FALSE)
regional(data_regional, stp, vax3_week, "STP", "Calendar week", suppress_region_label=TRUE)
regional(data_regional, msoa, vax3_week, "MSOA", "Calendar week", suppress_region_label=TRUE)

