# # # # # # # # # # # # # # # # # # # # #
# Purpose: describe matching results
# imports matching data
# reports on matching coverage, matching flowcharts, creates a "table 1", etc
# # # # # # # # # # # # # # # # # # # # #

## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')
library('gt')
library('gtsummary')

## Import custom user functions from lib
source(here("analysis", "functions", "utility.R"))

## Import design elements
source(here("analysis", "design", "design.R"))


# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  removeobjects <- FALSE
  matchset <- "A"
} else {
  removeobjects <- TRUE
  matchset <- args[[1]]
}



# create output directories ----

output_dir <- here("output", "match", matchset, "report")
fs::dir_create(output_dir)


## import matching info ----
data_matchstatus <- read_rds(fs::path(here("output", "match", matchset), "data_matchstatus.rds"))

# matching coverage on each day of recruitment period ----


# matching coverage for boosted people
data_coverage <-
  data_matchstatus %>%
  mutate(eligible=1) %>%
  group_by(treatment, boost_date) %>%
  summarise(
    n_eligible = sum(eligible, na.rm=TRUE),
    n_matched = sum(matched, na.rm=TRUE),
  ) %>%
  mutate(
    n_unmatched = n_eligible - n_matched,
  ) %>%
  pivot_longer(
    cols = c(n_unmatched, n_matched),
    names_to = "status",
    names_prefix = "n_",
    values_to = "n"
  ) %>%
  arrange(treatment, boost_date, status) %>%
  group_by(treatment, boost_date, status) %>%
  summarise(
    n = sum(n),
  ) %>%
  group_by(treatment, status) %>%
  complete(
    boost_date = full_seq(.$boost_date, 1), # go X days before to
    fill = list(n=0)
  ) %>%
  mutate(
    cumuln = cumsum(n)
  ) %>%
  ungroup() %>%
  mutate(
    status = factor(status, levels=c("unmatched", "matched")),
    status_descr = fct_recoderelevel(status, recoder$status)
  ) %>%
  arrange(treatment, status_descr, boost_date)




data_coverage_rounded <-
  data_coverage %>%
  group_by(treatment, status) %>%
  mutate(
    cumuln = roundmid_any(cumuln, to = threshold),
    n = diff(c(0,cumuln)),
  )

write_csv(data_coverage_rounded, fs::path(output_dir, "data_coverage.csv"))



## plot matching coverage ----

xmin <- min(data_coverage$boost_date )
xmax <- max(data_coverage$boost_date )+1

plot_coverage_n <-
  data_coverage %>%
  mutate(
    treatment_descr = fct_recoderelevel(as.character(treatment), recoder$treatment),
    n=n*((treatment*2) - 1)
  ) %>%
  ggplot()+
  geom_col(
    aes(
      x=boost_date+0.5,
      y=n,
      group=paste0(treatment,status),
      fill=treatment_descr,
      alpha=fct_rev(status),
      colour=NULL
    ),
    position=position_stack(reverse=TRUE),
    #alpha=0.8,
    width=1
  )+
  #geom_rect(xmin=xmin, xmax= xmax+1, ymin=-6, ymax=6, fill="grey", colour="transparent")+
  geom_hline(yintercept = 0, colour="black")+
  scale_x_date(
    breaks = unique(lubridate::ceiling_date(data_coverage$boost_date, "1 month")),
    limits = c(xmin-1, NA),
    labels = scales::label_date("%d/%m"),
    expand = expansion(add=1),
  )+
  scale_y_continuous(
    #labels = ~scales::label_number(accuracy = 1, big.mark=",")(abs(.x)),
    expand = expansion(c(0, NA))
  )+
  scale_fill_brewer(type="qual", palette="Set2")+
  scale_colour_brewer(type="qual", palette="Set2")+
  scale_alpha_discrete(range= c(0.8,0.4))+
  labs(
    x="Date",
    y="Booster vaccines per day",
    colour=NULL,
    fill=NULL,
    alpha=NULL
  ) +
  theme_minimal()+
  theme(
    axis.line.x.bottom = element_line(),
    axis.text.x.top=element_text(hjust=0),
    strip.text.y.right = element_text(angle = 0),
    axis.ticks.x=element_line(),
    legend.position = "bottom"
  )+
  NULL

plot_coverage_n

ggsave(plot_coverage_n, filename="coverage_count.png", path=output_dir)

plot_coverage_cumuln <-
  data_coverage %>%
  mutate(
    treatment_descr = fct_recoderelevel(as.character(treatment), recoder$treatment),
    cumuln=cumuln*((treatment*2) - 1)
  ) %>%
  ggplot()+
  geom_col(
    aes(
      x=boost_date+0.5,
      y=cumuln,
      group=paste0(treatment,status),
      fill=treatment_descr,
      alpha=fct_rev(status),
      colour=NULL
    ),
    position=position_stack(reverse=TRUE),
    width=1
  )+
  geom_rect(xmin=xmin, xmax= xmax+1, ymin=-6, ymax=6, fill="grey", colour="transparent")+
  scale_x_date(
    breaks = unique(lubridate::ceiling_date(data_coverage$boost_date, "1 month")),
    limits = c(xmin-1, NA),
    labels = scales::label_date("%d/%m"),
    expand = expansion(add=1),
  )+
  scale_y_continuous(
    #labels = ~scales::label_number(accuracy = 1, big.mark=",")(abs(.)),
    expand = expansion(c(0, NA))
  )+
  scale_fill_brewer(type="qual", palette="Set2")+
  scale_colour_brewer(type="qual", palette="Set2")+
  scale_alpha_discrete(range= c(0.8,0.4))+
  labs(
    x="Date",
    y="Cumulative booster vaccines",
    colour=NULL,
    fill=NULL,
    alpha=NULL
  ) +
  theme_minimal()+
  theme(
    axis.line.x.bottom = element_line(),
    axis.text.x.top=element_text(hjust=0),
    strip.text.y.right = element_text(angle = 0),
    axis.ticks.x=element_line(),
    legend.position = "bottom"
  )+
  NULL

plot_coverage_cumuln

ggsave(plot_coverage_cumuln, filename="coverage_stack.png", path=output_dir)



# table 1 style baseline characteristics ----



var_labels <- list(
  N  ~ "Total N",
  treatment_descr ~ "Vaccine type",
  vax_interval ~ "Days since previous vaccine",
  vax_previous_count ~ "Previous vaccine count",
  age_july2023 ~ "Age",
  ageband ~ "Age band",
  sex ~ "Sex",
  ethnicity ~ "Ethnicity",
  imd_Q5 ~ "Deprivation",
  region ~ "Region",
  cv ~ "Clinically at-risk",

  sev_obesity ~ "Body Mass Index > 40 kg/m^2",

  chronic_heart_disease ~ "Chronic heart disease",
  chronic_kidney_disease ~ "Chronic kidney disease",
  diabetes ~ "Diabetes",
  chronic_liver_disease ~ "Chronic liver disease",
  chronic_resp_disease ~ "Chronic respiratory disease",
  asthma ~ "Asthma",
  chronic_neuro_disease ~ "Chronic neurological disease",

  multimorb ~ "Morbidity count",

  immunosuppressed ~ "Immunosuppressed",
  immuno_any ~ "Immunosuppressed (all)",

  asplenia ~ "Asplenia or poor spleen function",
  cancer ~ "Cancer, within previous 3 years",
  solid_organ_transplant ~ "Solid organ transplant",
  immrx ~ "Immunosuppressive medications, within 6 months",
  hiv_aids ~ "HIV/AIDS",

  learndis ~ "Learning disabilities",
  sev_mental ~ "Serious mental illness",

  prior_tests_cat ~ "Number of SARS-CoV-2 tests",

  prior_covid_infection ~ "Prior documented SARS-CoV-2 infection"
) %>%
  set_names(., map_chr(., all.vars))

map_chr(var_labels[-c(1,2)], ~last(as.character(.)))


# append all reported characteristics to matchstatus data
data_matched_baseline <- read_rds(here("output", "data", "data_cohort.rds")) %>%
  filter(patient_id %in% data_matchstatus$patient_id) %>%
  select(patient_id, all_of(names(var_labels[-c(1,2)]))) %>%
  left_join(
    data_matchstatus %>% filter(matched),
    .,
    by="patient_id"
  )

tab_summary_baseline <-
  data_matched_baseline %>%
  mutate(
    N = 1L,
    treatment_descr = fct_recoderelevel(as.character(treatment), recoder$treatment),
  ) %>%
  select(
    treatment_descr,
    all_of(names(var_labels)),
  ) %>%
  tbl_summary(
    by = treatment_descr,
    label = unname(var_labels[names(.)]),
    statistic = list(
      N = "{N}",
      age_july2023="{mean} ({sd})",
      vax_interval="{mean} ({sd})"
    ),
  )


raw_stats <- tab_summary_baseline$meta_data %>%
  select(var_label, df_stats) %>%
  unnest(df_stats)

raw_stats_redacted <- raw_stats %>%
  mutate(
    n = roundmid_any(n, threshold),
    N = roundmid_any(N, threshold),
    p = n / N,
    N_miss = roundmid_any(N_miss, threshold),
    N_obs = roundmid_any(N_obs, threshold),
    p_miss = N_miss / N_obs,
    N_nonmiss = roundmid_any(N_nonmiss, threshold),
    p_nonmiss = N_nonmiss / N_obs,
    var_label = factor(var_label, levels = map_chr(var_labels[-c(1, 2)], ~ last(as.character(.)))),
    variable_levels = replace_na(as.character(variable_levels), "")
  )

write_csv(raw_stats_redacted, fs::path(output_dir, "table1.csv"))



# # love / smd plot ----

data_smd <- tab_summary_baseline$meta_data %>%
  select(var_label, df_stats) %>%
  unnest(df_stats) %>%
  filter(
    variable != "N"
  ) %>%
  group_by(var_label, variable_levels) %>%
  mutate(
    mean = coalesce(mean,p),
    sd = coalesce(sd,sqrt(p*(1-p)))
  ) %>%
  summarise(
    diff = diff(mean),
    sd = sqrt(mean(sd^2)),
    smd = diff/sd,
  ) %>%
  ungroup() %>%
  mutate(
    variable = factor(var_label, levels=map_chr(var_labels[-c(1,2)], ~last(as.character(.)))),
    variable_card = as.numeric(variable)%%2,
    variable_levels = replace_na(as.character(variable_levels), ""),
  ) %>%
  arrange(variable) %>%
  mutate(
    level = fct_rev(fct_inorder(str_replace(paste(variable, variable_levels, sep=": "),  "\\:\\s$", ""))),
    cardn = row_number()
  )

plot_smd <-
  ggplot(data_smd)+
  geom_point(aes(x=smd, y=level))+
  geom_rect(aes(alpha = variable_card, ymin = rev(cardn)-0.5, ymax =rev(cardn+0.5)), xmin = -Inf, xmax = Inf, fill='grey', colour="transparent") +
  scale_alpha_continuous(range=c(0,0.3), guide=FALSE)+
  labs(
    x="Standardised mean difference",
    y=NULL,
    alpha=NULL
  )+
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.background = element_rect(fill="transparent", colour="transparent"),
    strip.text.y.left = element_text(angle = 0, hjust=1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(0, "lines")
  )

write_csv(data_smd, fs::path(output_dir, "data_smd.csv"))
ggsave(plot_smd, filename="plot_smd.png", path=output_dir)




# flowchart ----

data_flowchart_match <-
  read_rds(here("output", "data", "data_inclusioncriteria.rds")) %>%
  left_join(
    data_matchstatus %>% select(patient_id, matched),
    by="patient_id"
  ) %>%
  mutate(
    c7 = c6 & matched,
  ) %>%
  select(-patient_id, -matched) %>%
  group_by(boost_type) %>%
  summarise(
    across(.cols=everything(), .fns=sum)
  ) %>%
  pivot_longer(
    cols=-boost_type,
    names_to="criteria",
    values_to="n"
  ) %>%
  group_by(boost_type) %>%
  mutate(
    n_exclude = lag(n) - n,
    pct_exclude = n_exclude/lag(n),
    pct_all = n / first(n),
    pct_step = n / lag(n),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "Recieved booster dose of Pfizer or Sanofi between 1 April and 30 June 2023",
      crit == "c1" ~ "  and clinically at-risk or aged 75+",
      crit == "c2" ~ "  with no missing demographic information",
      crit == "c3" ~ "  and not a health and social care worker",
      crit == "c4" ~ "  and not a care/nursing home resident, end-of-life or housebound",
      crit == "c5" ~ "  and no COVID-19-related events within 28 days",
      crit == "c6" ~ "  and not admitted in hospital at time of booster",
      crit == "c7" ~ "  and successfully matched",
      TRUE ~ NA_character_
    )
  )


# flowchart -- rounded so disclosure-safe ----

data_flowchart_match_rounded <-
  read_rds(here("output", "data", "data_inclusioncriteria.rds")) %>%
  left_join(
    data_matchstatus %>% select(patient_id, matched),
    by="patient_id"
  ) %>%
  mutate(
    c7 = c6 & matched,
  ) %>%
  select(-patient_id, -matched) %>%
  group_by(boost_type) %>%
  summarise(
    across(.cols = everything(), .fns=~roundmid_any(sum(.), threshold))
  ) %>%
  pivot_longer(
    cols=-boost_type,
    names_to="criteria",
    values_to="n"
  ) %>%
  group_by(boost_type) %>%
  mutate(
    n_exclude = lag(n) - n,
    pct_exclude = n_exclude/lag(n),
    pct_all = n / first(n),
    pct_step = n / lag(n),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "Recieved booster dose of Pfizer or Sanofi between 1 April and 30 June 2023",
      crit == "c1" ~ "  and clinically at-risk or aged 75+",
      crit == "c2" ~ "  with no missing demographic information",
      crit == "c3" ~ "  and not a health and social care worker",
      crit == "c4" ~ "  and not a care/nursing home resident, end-of-life or housebound",
      crit == "c5" ~ "  and no COVID-19-related events within 28 days",
      crit == "c6" ~ "  and not admitted in hospital at time of booster",
      crit == "c7" ~ "  and successfully matched",
      TRUE ~ NA_character_
    )
  ) #

write_csv(data_flowchart_match_rounded, fs::path(output_dir, "flowchart.csv"))

