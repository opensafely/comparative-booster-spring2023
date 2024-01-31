# # # # # # # # # # # # # # # # # # # # #
# Purpose: import processed data and filter out people who are excluded from the main analysis
# outputs:
#  - inclusion/exclusions flowchart data (up to matching step)
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('gt')
library('gtsummary')


## Import custom user functions from lib
source(here("analysis", "functions", "utility.R"))

## Import design elements
source(here("analysis", "design", "design.R"))

## Import redaction functions
source(here("analysis", "functions", "redaction.R"))


## import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  removeobjects <- FALSE
  cohort <- "age75plus" #currently `age75plus` or `cv`
} else {
  removeobjects <- TRUE
  cohort <- args[[1]]
}

# derive subgroup info
cohort_sym <- sym(cohort)


## create output directories for data ----
output_dir <- here("output", cohort)
fs::dir_create(output_dir)


# Main ----

## Import processed data ----

data_processed <- read_rds(here("output", "data", "data_processed.rds"))


# Report total number of people vaccinated in time period, by vaccine type ----
# This output ignores cohort, so is the same across different cohorts
# but it is more lightweight than outputting in the `data_process` script, because then
# the release action will need to copy in the entire processed data set

## unrounded totals
total_n_unrounded <-
  bind_rows(
    tibble(boost_type="any", n=nrow(data_processed)),
    count(data_processed %>% mutate(boost_type=fct_other(boost_type, keep=treatment_lookup$treatment, other_level="other")), boost_type, .drop=FALSE)
  ) %>%
  mutate(
    pct = n/first(n)
  )
write_csv(total_n_unrounded, fs::path(output_dir, "total_allcohorts_unrounded.csv"))

## rounded totals
total_n_rounded <-
  total_n_unrounded %>%
  mutate(
    n= ceiling_any(n, threshold),
    pct = n/first(n)
  )
write_csv(total_n_rounded, fs::path(output_dir, "total_allcohorts_rounded.csv"))





## Define selection criteria ----
data_criteria <- data_processed %>%
  filter(!!cohort_sym) %>%
  transmute(
    patient_id,
    boost_date,
    boost_type,
    has_age = !is.na(age_july2023),

    has_sex = !is.na(sex) & !(sex %in% c("intersex", "unknown")),
    has_imd = !is.na(imd_Q5),
    has_ethnicity = !is.na(ethnicity),
    has_region = !is.na(region),
    #has_msoa = !is.na(msoa),
    isnot_hscworker = !hscworker,
    #isnot_carehomeresident = !care_home_combined,
    isnot_endoflife = !endoflife,
    #isnot_housebound = !housebound,
    no_prior_pfizerBA45 = !vaxhist_pfizerBA45,
    no_prior_sanofi = !vaxhist_sanofi,
    vax_dates_possible,
    vax_intervals_atleast14days,
    vax_type_pfixer_or_sanofi = boost_type %in% c("pfizerBA45", "sanofi"),
    vax_previous_2plus = (vax_previous_count >= 2),
    has_norecentcovid = ((boost_date - anycovid_0_date) >= 28) | is.na(anycovid_0_date),
    isnot_inhospital = !inhospital,

    include = (
      vax_dates_possible &
      vax_intervals_atleast14days & vax_type_pfixer_or_sanofi & no_prior_pfizerBA45 & no_prior_sanofi &
      vax_previous_2plus &
      has_age & has_sex & has_imd & has_region & #has_ethnicity &
      isnot_hscworker &
      isnot_endoflife &
      has_norecentcovid &
      isnot_inhospital
    ),
  )

data_cohort <- data_criteria %>%
  filter(include) %>%
  select(patient_id) %>%
  left_join(data_processed, by="patient_id") %>%
  droplevels()

write_rds(data_cohort, fs::path(output_dir, "data_cohort.rds"), compress="gz")
#arrow::write_feather(data_cohort, fs::path(output_dir, "data_cohort.arrow"))

data_inclusioncriteria <- data_criteria %>%
  transmute(
    patient_id,
    boost_type,
    c0 = TRUE,
    c1 = c0 & vax_type_pfixer_or_sanofi,
    c2 = c1 & vax_dates_possible & vax_intervals_atleast14days & no_prior_pfizerBA45 & no_prior_sanofi,
    c3 = c2 & vax_previous_2plus,
    c4_1 = c3 & (has_age & has_sex & has_imd & has_region),
    c4_2 = c3 & (isnot_hscworker),
    c4_3 = c3 & (isnot_endoflife),
    c4_4 = c3 & (has_norecentcovid),
    c4_5 = c3 & (isnot_inhospital),
    c4 = c4_1 & c4_2 & c4_3 & c4_4 & c4_5
  ) %>%
  filter(c0)

# remove large in-memory objects
remove(data_criteria)

write_rds(data_inclusioncriteria, fs::path(output_dir, "data_inclusioncriteria.rds"), compress="gz")


## Create flowchart ----

create_flowchart <- function(round_level = 1){

  flowchart_output <-
    data_inclusioncriteria %>%
    select(-patient_id, -c0) %>%
    filter(c1) %>%
    group_by(boost_type) %>%
    summarise(
      across(.cols=everything(), .fns=~ceiling_any(sum(.), round_level))
    ) %>%
    pivot_longer(
      cols=-c(boost_type),
      names_to="criteria",
      values_to="n"
    ) %>%
    mutate(
      level = if_else(str_detect(criteria, "c\\d+$"), 1, 2),
      n_level1 = if_else(level==1, n, NA_real_),
      n_level1_fill = n_level1
    ) %>%
    fill(n_level1_fill) %>%
    group_by(boost_type) %>%
    mutate(
      n_exclude = lag(n_level1_fill) - n,
      pct_exclude = n_exclude/lag(n_level1_fill),
      pct_all = n_level1 / first(n),
      pct_step = n_level1 / lag(n_level1_fill),
      #crit = str_extract(criteria, "^c\\d+"),
      crit = criteria,
      criteria = fct_case_when(
        crit == "c1" ~ "Received COVID-19 vaccine between 1 April and 30 June 2023",
        crit == "c2" ~ "  with no prior vaccine within 14 days and no prior PfizerBA45 or Sanofi",
        crit == "c3" ~ "  with at least 2 previous COVID-19 vaccine doses",
        crit == "c4_1" ~ "    no missing demographic information",
        crit == "c4_2" ~ "    not a health and social care worker",
        crit == "c4_3" ~ "    not end-of-life",
        crit == "c4_4" ~ "    no documented COVID-19 infection/disease within prior 28 days",
        crit == "c4_5" ~ "    not admitted in hospital at time of booster",
        crit == "c4" ~ "  included in matching run",
        TRUE ~ "NA_character_boop" # should not appear
      )
    )

  return(flowchart_output)
}

## unrounded flowchart
data_flowchart <- create_flowchart(1)
write_rds(data_flowchart, fs::path(output_dir, "flowchart.rds"))
#write_csv(data_flowchart, here("output", "data", "flowchart.csv"))

## rounded flowchart
data_flowchart_rounded <- create_flowchart(threshold)
write_rds(data_flowchart_rounded, fs::path(output_dir, "flowchart_rounded.rds"))
write_csv(data_flowchart_rounded, fs::path(output_dir, "flowchart_rounded.csv"))

## unrounded totals
total_n_unrounded <-
  bind_rows(
    tibble(boost_type="any", n=nrow(data_inclusioncriteria)),
    count(data_inclusioncriteria %>% mutate(boost_type=fct_other(boost_type, keep=treatment_lookup$treatment, other_level="other")), boost_type, .drop=FALSE)
  ) %>%
  mutate(
    pct = n/first(n)
  )
write_csv(total_n_unrounded, fs::path(output_dir, "total_unrounded.csv"))

## rounded totals
total_n_rounded <-
  total_n_unrounded %>%
  mutate(
    n= ceiling_any(n, threshold),
    pct = n/first(n)
  )
write_csv(total_n_rounded, fs::path(output_dir, "total_rounded.csv"))

## remove large in-memory objects
remove(data_inclusioncriteria)


# table 1 style baseline characteristics amongst those eligible for matching ----

var_labels <- list(
  N  ~ "Total N",
  treatment_descr ~ "Vaccine type",
  vax_interval ~ "Days since previous vaccine",
  vax_previous_group ~ "Previous vaccine count",
  age_july2023 ~ "Age",
  ageband ~ "Age band",
  sex ~ "Sex",
  ethnicity ~ "Ethnicity",
  imd_Q5 ~ "Deprivation",
  region ~ "Region",
  cv ~ "Clinically at-risk",

  housebound ~ "Clinically housebound",
  care_home_combined ~ "Care/nursing home resident",

  sev_obesity ~ "Body Mass Index > 40 kg/m^2",

  chronic_heart_disease ~ "Chronic heart disease",
  chronic_kidney_disease ~ "Chronic kidney disease",
  diabetes ~ "Diabetes",
  chronic_liver_disease ~ "Chronic liver disease",
  chronic_resp_disease ~ "Chronic respiratory disease",
  asthma ~ "Asthma",
  chronic_neuro_disease ~ "Chronic neurological disease",

  immunosuppressed ~ "Immunosuppressed",
  immuno_any ~ "Immunosuppressed (all)",

  immdx ~ "Immunocompromising diagnosis",
  immrx ~ "Immunosuppressive medications, previous 3 years",
  dxt_chemo ~ "Chemotherapy, previous 3 years",
  cancer ~ "Cancer, previous 3 years",
  asplenia ~ "Asplenia or poor spleen function",
  solid_organ_transplant ~ "Solid organ transplant",
  hiv_aids ~ "HIV/AIDS",

  multimorb ~ "Morbidity count",

  learndis ~ "Learning disabilities",
  sev_mental ~ "Serious mental illness",

  prior_tests_cat ~ "Number of SARS-CoV-2 tests",

  prior_covid_infection ~ "Prior documented SARS-CoV-2 infection",

  vaxhist_pfizer  ~ "Previously received Pfizer (original)",
  vaxhist_az  ~ "Previously received AZ",
  vaxhist_moderna  ~ "Previously received Moderna",
  vaxhist_pfizerBA1  ~ "Previously received Pfizer/BA.1",
  vaxhist_pfizerXBB15  ~ "Previously received Pfizer/XBB.1.5",
  vaxhist_modernaomicron  ~ "Previously received Moderna/Omicron",
  vaxhist_modernaXBB15  ~ "Previously received Moderna/XBB.1.5"
) %>%
  set_names(., map_chr(., all.vars))



tab_summary_prematch <-
  data_cohort %>%
  mutate(
    N=1L,
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
      N ~ "{N}",
      all_continuous() ~ "{median} ({p25}, {p75});  {mean} ({sd})"
    ),
  )


raw_stats <- tab_summary_prematch$meta_data %>%
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


#
# # love / smd plot ----
#
# data_smd <- tab_summary_baseline$meta_data %>%
#   select(var_label, df_stats) %>%
#   unnest(df_stats) %>%
#   filter(
#     variable != "N"
#   ) %>%
#   group_by(var_label, variable_levels) %>%
#   summarise(
#     diff = diff(p),
#     sd = sqrt(sum(p*(1-p))),
#     smd = diff/sd
#   ) %>%
#   ungroup() %>%
#   mutate(
#     variable = factor(var_label, levels=map_chr(var_labels[-c(1,2)], ~last(as.character(.)))),
#     variable_card = as.numeric(variable)%%2,
#     variable_levels = replace_na(as.character(variable_levels), ""),
#   ) %>%
#   arrange(variable) %>%
#   mutate(
#     level = fct_rev(fct_inorder(str_replace(paste(variable, variable_levels, sep=": "),  "\\:\\s$", ""))),
#     cardn = row_number()
#   )
#
# write_csv(data_smd, fs::path(output_dir, "smd.csv"))
#
#

