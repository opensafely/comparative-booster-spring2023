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
    isnot_carehomeresident = !care_home_combined,
    isnot_endoflife = !endoflife,
    isnot_housebound = !housebound,
    no_prior_pfizerBA45 = !vaxhist_pfizerBA45,
    no_prior_sanofi = !vaxhist_sanofi,
    vax_dates_possible,
    vax_intervals_atleast14days,
    vax_type_pfixer_or_sanofi = boost_type %in% c("pfizerBA45", "sanofi"),
    has_norecentcovid = ((boost_date - anycovid_0_date) >= 28) | is.na(anycovid_0_date),
    isnot_inhospital = !inhospital,

    include = (
      vax_dates_possible &
      vax_intervals_atleast14days & vax_type_pfixer_or_sanofi & no_prior_pfizerBA45 & no_prior_sanofi &
      has_age & has_sex & has_imd & has_region & #has_ethnicity &
      isnot_hscworker &
      isnot_carehomeresident & isnot_endoflife & isnot_housebound &
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
    c3_1 = c2 & (has_age & has_sex & has_imd & has_region),
    c3_2 = c2 & (isnot_hscworker),
    c3_3 = c2 & (isnot_carehomeresident & isnot_endoflife & isnot_housebound),
    c3_4 = c2 & (has_norecentcovid),
    c3_5 = c2 & (isnot_inhospital),
    c3 = c3_1 & c3_2 & c3_3 & c3_4 & c3_5
  ) %>%
  filter(c0)

# remove large in-memory objects
remove(data_criteria)

write_rds(data_inclusioncriteria, fs::path(output_dir, "data_inclusioncriteria.rds"), compress="gz")


## Create flowchart ----

create_flowchart <- function(round_level = 1){

  flowchart_output <-
    data_inclusioncriteria %>%
    select(-patient_id) %>%
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
        crit == "c0" ~ "Received COVID-19 vaccination between 1 April and 30 June 2023", # paste0("Aged 18+\n with 2 doses on or before ", format(study_dates$lastvax2_date, "%d %b %Y")),
        crit == "c1" ~ "  vaccine product was PfizerBA45 or Sanofi",
        crit == "c2" ~ "  with no prior vaccination within 14 days, or with PfizerBA45 or Sanofi",
        crit == "c3_1" ~ "    no missing demographic information",
        crit == "c3_2" ~ "    not a health and social care worker",
        crit == "c3_3" ~ "    not a care/nursing home resident, end-of-life or housebound",
        crit == "c3_4" ~ "    no COVID-19-related events within 28 days",
        crit == "c3_5" ~ "    not admitted in hospital at time of booster",
        crit == "c3" ~ "  included in matching run",
        TRUE ~ "NA_character_boop"
      )
    )

  return(flowchart_output)
}


## unrounded flowchart
data_flowchart <- create_flowchart(1)
write_rds(data_flowchart, fs::path(output_dir, "flowchart.rds"))
#write_csv(data_flowchart, here("output", "data", "flowchart.csv"))

## rounded flowchart
data_flowchart_rounded <- create_flowchart(7)
write_rds(data_flowchart_rounded, fs::path(output_dir, "flowchart_rounded.rds"))
write_csv(data_flowchart_rounded, fs::path(output_dir, "flowchart_rounded.csv"))


## remove large in-memory objects
remove(data_inclusioncriteria)


# table 1 style baseline characteristics amongst those eligible for matching ----

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

  immunosuppressed ~ "Immunosuppressed",
  immuno_any ~ "Immunosuppressed (all)",

  asplenia ~ "Asplenia or poor spleen function",
  cancer ~ "Cancer, within previous 3 years",
  solid_organ_transplant ~ "Solid organ transplant",
  immrx ~ "Immunosuppressive medications, within 6 months",
  hiv_aids ~ "HIV/AIDS",

  multimorb ~ "Morbidity count",

  learndis ~ "Learning disabilities",
  sev_mental ~ "Serious mental illness",

  prior_tests_cat ~ "Number of SARS-CoV-2 tests",

  prior_covid_infection ~ "Prior documented SARS-CoV-2 infection"
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
      N = "{N}",
      age_july2023="{mean} ({sd})",
      vax_interval="{mean} ({sd})"
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

