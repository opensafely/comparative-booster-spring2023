######################################

# This script:
# imports ICD10 check data extracted by the cohort extractor
# compares time to event rates for different definitions of "covid-related"
######################################




# Import libraries ----
library('tidyverse')
library('lubridate')
library('arrow')
library('here')
library('glue')
library('survival')


# Import custom user functions from lib
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "survival.R"))

# output processed data to rds ----

fs::dir_create(here("output", "ICD10check"))



# create output directories ----

output_dir <- here("output", "ICD10check")
fs::dir_create(output_dir)

# process ----

data_extract <- read_feather(here("output", "input_ICD10check.feather")) %>%
  #because date types are not returned consistently by cohort extractor
  mutate(across(ends_with("_date"),  as.Date))


maxfup <- 100

data_processed <- data_extract %>%
  mutate(

    ageband = cut(
      age,
      breaks=c(-Inf, 18, 40, 50, 60, 70, 80, 90, Inf),
      labels=c("under 18", "18-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
      right=FALSE
    ),

    sex = fct_case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      #sex == "I" ~ "Inter-sex",
      #sex == "U" ~ "Unknown",
      TRUE ~ NA_character_
    ),

    censor_date = pmin(dereg_date, anycovidvax_3_date+maxfup, na.rm=TRUE),

    admitted_primary_U07_date = pmin(admitted_primary_U071_date, admitted_primary_U072_date, na.rm=TRUE),
    admitted_any_U07_date = pmin(admitted_any_U071_date, admitted_any_U072_date, na.rm=TRUE),

    admitted_primary_all_date = pmin(admitted_primary_U071_date, admitted_primary_U072_date, admitted_primary_U099_date, admitted_primary_U109_date, na.rm=TRUE),
    admitted_any_all_date = pmin(admitted_any_U071_date, admitted_any_U072_date, admitted_any_U099_date, admitted_any_U109_date, na.rm=TRUE),

    death_primary_U07_date = pmin(death_primary_U071_date, death_primary_U072_date, na.rm=TRUE),
    death_any_U07_date = pmin(death_any_U071_date, death_any_U072_date, na.rm=TRUE),

    death_primary_all_date = pmin(death_primary_U071_date, death_primary_U072_date, death_primary_U099_date, death_primary_U109_date, na.rm=TRUE),
    death_any_all_date = pmin(death_any_U071_date, death_any_U072_date, death_any_U099_date, death_any_U109_date, na.rm=TRUE),


    ## time to event

    admitted_primary_U071_tte = tte(anycovidvax_3_date-1, admitted_primary_U071_date, censor_date),
    admitted_primary_U072_tte = tte(anycovidvax_3_date-1, admitted_primary_U072_date, censor_date),
    admitted_primary_U099_tte = tte(anycovidvax_3_date-1, admitted_primary_U099_date, censor_date),
    admitted_primary_U109_tte = tte(anycovidvax_3_date-1, admitted_primary_U109_date, censor_date),
    admitted_primary_U07_tte = tte(anycovidvax_3_date-1, admitted_primary_U07_date, censor_date),
    admitted_primary_all_tte = tte(anycovidvax_3_date-1, admitted_primary_all_date, censor_date),

    admitted_any_U071_tte = tte(anycovidvax_3_date-1, admitted_any_U071_date, censor_date),
    admitted_any_U072_tte = tte(anycovidvax_3_date-1, admitted_any_U072_date, censor_date),
    admitted_any_U099_tte = tte(anycovidvax_3_date-1, admitted_any_U099_date, censor_date),
    admitted_any_U109_tte = tte(anycovidvax_3_date-1, admitted_any_U109_date, censor_date),
    admitted_any_U07_tte = tte(anycovidvax_3_date-1, admitted_any_U07_date, censor_date),
    admitted_any_all_tte = tte(anycovidvax_3_date-1, admitted_any_all_date, censor_date),

    death_primary_U071_tte = tte(anycovidvax_3_date-1, death_primary_U071_date, censor_date),
    death_primary_U072_tte = tte(anycovidvax_3_date-1, death_primary_U072_date, censor_date),
    death_primary_U099_tte = tte(anycovidvax_3_date-1, death_primary_U099_date, censor_date),
    death_primary_U109_tte = tte(anycovidvax_3_date-1, death_primary_U109_date, censor_date),
    death_primary_U07_tte = tte(anycovidvax_3_date-1, death_primary_U07_date, censor_date),
    death_primary_all_tte = tte(anycovidvax_3_date-1, death_primary_all_date, censor_date),

    death_any_U071_tte = tte(anycovidvax_3_date-1, death_any_U071_date, censor_date),
    death_any_U072_tte = tte(anycovidvax_3_date-1, death_any_U072_date, censor_date),
    death_any_U099_tte = tte(anycovidvax_3_date-1, death_any_U099_date, censor_date),
    death_any_U109_tte = tte(anycovidvax_3_date-1, death_any_U109_date, censor_date),
    death_any_U07_tte = tte(anycovidvax_3_date-1, death_any_U07_date, censor_date),
    death_any_all_tte = tte(anycovidvax_3_date-1, death_any_all_date, censor_date),

    # censoring indicator

    admitted_primary_U071_ind = censor_indicator(admitted_primary_U071_date, censor_date),
    admitted_primary_U072_ind = censor_indicator(admitted_primary_U072_date, censor_date),
    admitted_primary_U099_ind = censor_indicator(admitted_primary_U099_date, censor_date),
    admitted_primary_U109_ind = censor_indicator(admitted_primary_U109_date, censor_date),
    admitted_primary_U07_ind = censor_indicator(admitted_primary_U07_date, censor_date),
    admitted_primary_all_ind = censor_indicator(admitted_primary_all_date, censor_date),

    admitted_any_U071_ind = censor_indicator(admitted_any_U071_date, censor_date),
    admitted_any_U072_ind = censor_indicator(admitted_any_U072_date, censor_date),
    admitted_any_U099_ind = censor_indicator(admitted_any_U099_date, censor_date),
    admitted_any_U109_ind = censor_indicator(admitted_any_U109_date, censor_date),
    admitted_any_U07_ind = censor_indicator(admitted_any_U07_date, censor_date),
    admitted_any_all_ind = censor_indicator(admitted_any_all_date, censor_date),

    death_primary_U071_ind = censor_indicator(death_primary_U071_date, censor_date),
    death_primary_U072_ind = censor_indicator(death_primary_U072_date, censor_date),
    death_primary_U099_ind = censor_indicator(death_primary_U099_date, censor_date),
    death_primary_U109_ind = censor_indicator(death_primary_U109_date, censor_date),
    death_primary_U07_ind = censor_indicator(death_primary_U07_date, censor_date),
    death_primary_all_ind = censor_indicator(death_primary_all_date, censor_date),

    death_any_U071_ind = censor_indicator(death_any_U071_date, censor_date),
    death_any_U072_ind = censor_indicator(death_any_U072_date, censor_date),
    death_any_U099_ind = censor_indicator(death_any_U099_date, censor_date),
    death_any_U109_ind = censor_indicator(death_any_U109_date, censor_date),
    death_any_U07_ind = censor_indicator(death_any_U07_date, censor_date),
    death_any_all_ind = censor_indicator(death_any_all_date, censor_date),

  )

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){
  data_processed <-data_processed %>% filter(!is.na(anycovidvax_3_date))
}

metavars <- expand_grid(
  event = factor(c("admitted", "death")),
  scope = factor(c("primary", "any")),
  code = factor(c("U071", "U072", "U099", "U109", "U07", "all"))
)


data_surv <- metavars %>%
  mutate(
    surv_obj = pmap(
      list(event, scope, code),
      function(event, scope, code){
        survfit(Surv(data_processed[[glue("{event}_{scope}_{code}_tte")]], data_processed[[glue("{event}_{scope}_{code}_ind")]]) ~ 1, conf.type="log-log")
      }
    ),
    surv_obj_tidy = map(
      surv_obj, ~ {
        broom::tidy(.x) %>%
        complete(
          time = seq_len(maxfup), # fill in 1 row for each day of follow up
          fill = list(n.event = 0, n.censor = 0) # fill in zero events on those days
        ) %>%
        fill(n.risk, .direction = c("up")) # fill in n.risk on each zero-event day
      }
    ),
  ) %>%
  select(-surv_obj) %>%
  unnest(surv_obj_tidy)


km_process <- function(.data, round_by) {
  .data %>%
    group_by(event, scope, code) %>%
    mutate(
      lagtime = lag(time, 1, 0),
      leadtime = lead(time, 1, max(time) + 1),
      interval = time - lagtime,
      N = max(n.risk, na.rm = TRUE),

      # rounded to `round_by - (round_by/2)`
      cml.eventcensor = roundmid_any(cumsum(n.event + n.censor), round_by),
      cml.event = roundmid_any(cumsum(n.event), round_by),
      cml.censor = cml.eventcensor - cml.event,
      n.event = diff(c(0, cml.event)),
      n.censor = diff(c(0, cml.censor)),
      n.risk = roundmid_any(N, round_by) - lag(cml.eventcensor, 1, 0),

      # KM estimate for event of interest, combining censored and competing events as censored
      summand = (1 / (n.risk - n.event)) - (1 / n.risk), # = n.event / ((n.risk - n.event) * n.risk) but re-written to prevent integer overflow
      surv = cumprod(1 - n.event / n.risk),
      surv.se = surv * sqrt(cumsum(summand)), # greenwood's formula
      surv.ln.se = surv.se / surv,

      ## standard errors on log scale
      # surv.ll = exp(log(surv) + qnorm(0.025)*surv.ln.se),
      # surv.ul = exp(log(surv) + qnorm(0.975)*surv.ln.se),

      llsurv = log(-log(surv)),
      llsurv.se = sqrt((1 / log(surv)^2) * cumsum(summand)),

      ## standard errors on complementary log-log scale
      surv.ll = exp(-exp(llsurv + qnorm(0.975) * llsurv.se)),
      surv.ul = exp(-exp(llsurv + qnorm(0.025) * llsurv.se)),
      risk = 1 - surv,
      risk.se = surv.se,
      risk.ln.se = surv.ln.se,
      risk.ll = 1 - surv.ul,
      risk.ul = 1 - surv.ll
    ) %>%
    select(
      event, scope, code, time, lagtime, leadtime, interval,
      cml.event, cml.censor,
      n.risk, n.event, n.censor,
      surv, surv.se, surv.ll, surv.ul,
      risk, risk.se, risk.ll, risk.ul
    )
}

data_surv_unrounded <- km_process(data_surv, 1)
data_surv_rounded <- km_process(data_surv, 7)

km_plot <- function(.data) {
  .data %>%
    group_modify(
      ~ add_row(
        .x,
        time = 0,
        lagtime = 0,
        leadtime = 1,
        # interval=1,
        surv = 1,
        surv.ll = 1,
        surv.ul = 1,
        risk = 0,
        risk.ll = 0,
        risk.ul = 0,
        .before = 0
      )
    ) %>%
    ggplot(aes(group = code, colour = code, fill = code)) +
    geom_step(aes(x = time, y = risk), direction = "vh") +
    geom_step(aes(x = time, y = risk), direction = "vh", linetype = "dashed", alpha = 0.5) +
    geom_rect(aes(xmin = lagtime, xmax = time, ymin = risk.ll, ymax = risk.ul), alpha = 0.1, colour = "transparent") +
    facet_grid(rows = vars(event), cols=vars(scope), scales="free_y") +
    scale_color_brewer(type = "qual", palette = "Set1", na.value = "grey") +
    scale_fill_brewer(type = "qual", palette = "Set1", guide = "none", na.value = "grey") +
    scale_x_continuous(breaks = seq(0, 600, 14)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(xlim = c(0, NA)) +
    labs(
      x = "Days",
      y = "Cumulative incidence",
      colour = NULL,
      title = NULL
    ) +
    #theme_minimal() +
    theme(
      axis.line.x = element_line(colour = "black"),
      panel.grid.minor.x = element_blank(),
      legend.position = c(.05, .95),
      legend.justification = c(0, 1),
    )
}

km_plot_unrounded <- km_plot(data_surv_unrounded)
km_plot_rounded <- km_plot(data_surv_rounded)

ggsave(filename = fs::path(output_dir, "km_plot_unrounded.png"), km_plot_unrounded, width = 20, height = 15, units = "cm")
ggsave(filename = fs::path(output_dir, "km_plot_rounded.png"), km_plot_rounded, width = 20, height = 15, units = "cm")


# the following function summarises the code combinations (any position) for a given event
count_codes <- function(event) {

  cat(glue("Number and percent of \"{event}\" events with given code combinations:"), "\n")
  data_processed %>%
    mutate(across(matches(glue("{event}_any_\\w+_date")), ~if_else(.x != !!sym(glue("{event}_any_all_date")), as.Date(NA_character_), .x))) %>%
    select(patient_id, matches(glue("{event}_any_\\w+_date")), -sym(glue("{event}_any_all_date"))) %>%
    pivot_longer(
      cols = -patient_id,
      values_drop_na = TRUE,
      names_pattern = glue("{event}_any_(.*)_date"),
    ) %>%
    group_by(patient_id) %>%
    arrange(name) %>%
    mutate(codes = str_c(name, collapse = ", ")) %>%
    ungroup() %>%
    group_by(codes) %>%
    count() %>%
    ungroup() %>%
    mutate(percent = 100*n/sum(n))

}

count_codes("admitted")
count_codes("death")

