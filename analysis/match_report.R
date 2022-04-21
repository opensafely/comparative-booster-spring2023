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

## Import custom user functions from lib
source(here("lib", "functions", "utility.R"))
#source(here("lib", "functions", "survival.R"))
source(here("lib", "functions", "redaction.R"))

## Import design elements
source(here("lib", "design", "design.R"))


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

## create special log file ----
cat(glue("## script info for natch report ##"), "  \n", file = fs::path(output_dir, glue("log.txt")), append = FALSE)

## functions to pass additional log info to seperate file
logoutput <- function(...){
  cat(..., file = fs::path(output_dir, glue("log.txt")), sep = "\n  ", append = TRUE)
  cat("\n", file = fs::path(output_dir, glue("log.txt")), sep = "\n  ", append = TRUE)
}

logoutput_datasize <- function(x){
  nm <- deparse(substitute(x))
  logoutput(
    glue(nm, " data size = ", nrow(x)),
    glue(nm, " memory usage = ", format(object.size(x), units="GB", standard="SI", digits=3L))
  )
}

## import matching info ----
data_matchstatus <- read_rds(fs::path(here("output", "match", matchset), "data_matchstatus.rds"))


# matching coverage per trial / day of follow up




# matching coverage for boosted people
data_coverage <-
  data_matchstatus %>%
  mutate(eligible=1) %>%
  group_by(treatment, vax3_date) %>%
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
  arrange(treatment, vax3_date, status) %>%
  group_by(treatment, vax3_date, status) %>%
  summarise(
    n = sum(n),
  ) %>%
  group_by(treatment, status) %>%
  complete(
    vax3_date = full_seq(.$vax3_date, 1), # go X days before to
    fill = list(n=0)
  ) %>%
  mutate(
    cumuln = cumsum(n)
  ) %>%
  ungroup() %>%
  mutate(
    status = factor(status, levels=c("unmatched", "matched")),
    status_descr = fct_recode(status, !!!recoder$status)
  ) %>%
  arrange(treatment, status_descr, vax3_date)

write_csv(data_coverage, fs::path(output_dir, "data_coverage.csv"))

# report matching info ----

day1_date <- study_dates$index_date

## matching coverage ----

xmin <- min(data_coverage$vax3_date )
xmax <- max(data_coverage$vax3_date )+1

plot_coverage_n <-
  data_coverage %>%
  mutate(
    treatment_descr = fct_recode(as.character(treatment), !!!recoder$treatment),
    n=n*((treatment*2) - 1)
  ) %>%
  ggplot()+
  geom_col(
    aes(
      x=vax3_date+0.5,
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
    breaks = unique(lubridate::ceiling_date(data_coverage$vax3_date, "1 month")),
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
    treatment_descr = fct_recode(as.character(treatment), !!!recoder$treatment),
    cumuln=cumuln*((treatment*2) - 1)
  ) %>%
  ggplot()+
  geom_col(
    aes(
      x=vax3_date+0.5,
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
    breaks = unique(lubridate::ceiling_date(data_coverage$vax3_date, "1 month")),
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

library('gt')
library('gtsummary')

var_labels <- list(
  N  ~ "Total N",
  treatment_descr ~ "Vaccine type",
  vax12_type_descr ~ "Primary vaccine course",
  #age ~ "Age",
  ageband ~ "Age",
  sex ~ "Sex",
  ethnicity_combined ~ "Ethnicity",
  imd_Q5 ~ "IMD",
  region ~ "Region",
  cev_cv ~ "JCVI clinical risk group",

  sev_obesity ~ "Body Mass Index > 40 kg/m^2",

  chronic_heart_disease ~ "Chronic heart disease",
  chronic_kidney_disease ~ "Chronic kidney disease",
  diabetes ~ "Diabetes",
  chronic_liver_disease ~ "Chronic liver disease",
  chronic_resp_disease ~ "Chronic respiratory disease",
  asthma ~ "Asthma",
  chronic_neuro_disease ~ "Chronic neurological disease",

  #multimorb ~ "Morbidity count",
  immunosuppressed ~ "Immunosuppressed",
  asplenia ~ "Asplenia or poor spleen function",
  learndis ~ "Learning disabilities",
  sev_mental ~ "Serious mental illness",

  prior_tests_cat ~ "Number of SARS-CoV-2 tests",

  prior_covid_infection ~ "Prior documented SARS-CoV-2 infection",
  inhospital_planned ~ "In hospital (planned admission)"
) %>%
  set_names(., map_chr(., all.vars))

map_chr(var_labels[-c(1,2)], ~last(as.character(.)))


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
    treatment_descr = fct_recode(as.character(treatment), !!!recoder$treatment),
  ) %>%
  select(
    treatment_descr,
    all_of(names(var_labels)),
  ) %>%
  tbl_summary(
    by = treatment_descr,
    label = unname(var_labels[names(.)]),
    statistic = list(N = "{N}")
  ) %>%
  modify_footnote(starts_with("stat_") ~ NA) %>%
  modify_header(stat_by = "**{level}**") %>%
  bold_labels()

tab_summary_baseline_redacted <- redact_tblsummary(tab_summary_baseline, 5, "[REDACTED]")

raw_stats <- tab_summary_baseline_redacted$meta_data %>%
  select(var_label, df_stats) %>%
  unnest(df_stats)

write_csv(tab_summary_baseline_redacted$table_body, fs::path(output_dir, "table1.csv"))
write_csv(tab_summary_baseline_redacted$df_by, fs::path(output_dir, "table1by.csv"))
gtsave(as_gt(tab_summary_baseline_redacted), fs::path(output_dir, "table1.html"))




# love / smd plot ----

data_smd <- tab_summary_baseline$meta_data %>%
  select(var_label, df_stats) %>%
  unnest(df_stats) %>%
  filter(
    variable != "N"
  ) %>%
  group_by(var_label, variable_levels) %>%
  summarise(
    diff = diff(p),
    sd = sqrt(sum(p*(1-p))),
    smd = diff/sd
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
  summarise(
    across(.fns=sum)
  ) %>%
  pivot_longer(
    cols=everything(),
    names_to="criteria",
    values_to="n"
  ) %>%
  mutate(
    n_exclude = lag(n) - n,
    pct_exclude = n_exclude/lag(n),
    pct_all = n / first(n),
    pct_step = n / lag(n),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "Aged 18+ and recieved booster dose of BNT162b2 or Moderna between 29 October 2021 and 31 January 2022", # paste0("Aged 18+\n with 2 doses on or before ", format(study_dates$lastvax2_date, "%d %b %Y")),
      crit == "c1" ~ "  with no missing demographic information",
      crit == "c2" ~ "  with homologous primary vaccination course of pfizer or AZ",
      crit == "c3" ~ "  and not a HSC worker",
      crit == "c4" ~ "  and not a care/nursing home resident, end-of-life or housebound",
      crit == "c5" ~ "  and no COVID-19-related events within 90 days",
      crit == "c6" ~ "  and not in hospital at time of booster",
      crit == "c7" ~ "  and successfully matched",
      TRUE ~ NA_character_
    )
  )

write_csv(data_flowchart_match, fs::path(output_dir, "flowchart.csv"))










## matching summary ----
# FIXME -- need to import baseline variabels here

# summary of trial participants, by treatment group
# match_summary_treatment <-
#   data_matchstatus %>%
#   group_by(treatment) %>%
#   summarise(
#     n=n(),
#     firstrecruitdate = min(vax3_date),
#     lastrecruitdate = max(vax3_date),
#     # fup_sum = sum(fup),
#     # fup_years = sum(fup)/365.25,
#     # fup_mean = mean(fup),
#     # fup_median = median(fup),
#
#     n_12pfizer = sum(vax12_type=="pfizer-pfizer"),
#     prop_12pfizer = n_12pfizer/n,
#     n_12az = sum(vax12_type=="az-az"),
#     prop_12az = n_12az/n,
#     # n_12moderna = sum(vax12_type=="moderna-moderna"),
#     # prop_12moderna = n_12moderna/n,
#
#     age_median = median(age),
#     age_Q1 = quantile(age, 0.25),
#     age_Q3 = quantile(age, 0.75),
#     female = mean(sex=="Female"),
#   )
#
# write_csv(match_summary_treated, fs::path(output_dir, "report_summary_treated.csv"))
#
