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

## create output directories for data ----
fs::dir_create(here("output", "data"))

## create output directories for tables/summaries ----

output_dir <- here("output", "prematch")
fs::dir_create(output_dir)


# Main ----

## Import processed data ----

data_processed <- read_rds(here("output", "data", "data_processed.rds"))


## Define selection criteria ----
data_criteria <- data_processed %>%
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
    is_75plus = age_july2023 >= 75,
    is_cv = cv,
    is_eligible = is_cv | is_75plus,
    isnot_hscworker = !hscworker,
    isnot_carehomeresident = !care_home_combined,
    isnot_endoflife = !endoflife,
    isnot_housebound = !housebound,
    vax_dates_possible,
    vax_intervals_atleast14days,
    vax_type_pfixer_or_sanofi = boost_type %in% c("pfizerBA45", "sanofi"),
    has_norecentcovid = ((boost_date - anycovid_0_date) >= 28) | is.na(anycovid_0_date),
    isnot_inhospital = !inhospital,

    include = (
      vax_dates_possible &
      vax_intervals_atleast14days & vax_type_pfixer_or_sanofi &
      has_age & has_sex & has_imd & has_region & #has_ethnicity &
      is_eligible &
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

write_rds(data_cohort, here("output", "data", "data_cohort.rds"), compress="gz")
arrow::write_feather(data_cohort, here("output", "data", "data_cohort.arrow"))

data_inclusioncriteria <- data_criteria %>%
  transmute(
    patient_id,
    boost_type,
    c0 = vax_dates_possible & vax_intervals_atleast14days & vax_type_pfixer_or_sanofi,
    c1 = c0 & (is_eligible),
    c2 = c1 & (has_age & has_sex & has_imd & has_region),
    c3 = c2 & (isnot_hscworker),
    c4 = c3 & (isnot_carehomeresident & isnot_endoflife & isnot_housebound),
    c5 = c4 & (has_norecentcovid),
    c6 = c5 & (isnot_inhospital),
  ) %>%
  filter(c0)

write_rds(data_inclusioncriteria, here("output", "data", "data_inclusioncriteria.rds"), compress="gz")


## flowchart -- rounded so disclosure-safe ----

data_flowchart <-
  data_inclusioncriteria %>%
  select(-patient_id) %>%
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
      crit == "c0" ~ "Recieved booster dose of Pfizer or Sanofi between 1 April and 30 June 2023", # paste0("Aged 18+\n with 2 doses on or before ", format(study_dates$lastvax2_date, "%d %b %Y")),
      crit == "c1" ~ "  and clinically at-risk or aged 75+",
      crit == "c2" ~ "  with no missing demographic information",
      crit == "c3" ~ "  and not a health and social care worker",
      crit == "c4" ~ "  and not a care/nursing home resident, end-of-life or housebound",
      crit == "c5" ~ "  and no COVID-19-related events within 28 days",
      crit == "c6" ~ "  and not admitted in hospital at time of booster",
      TRUE ~ NA_character_
    )
  ) #
#write_csv(data_flowchart, here("output", "data", "flowchart.csv"))



## flowchart -- rounded so disclosure-safe ----
data_flowchart_rounded <-
  data_inclusioncriteria %>%
  select(-patient_id) %>%
  group_by(boost_type) %>%
  summarise(
    across(.cols=everything(), .fns=~ceiling_any(sum(.), 7))
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
      crit == "c0" ~ "Recieved booster dose of Pfizer or Sanofi between 1 April and 30 June 2023", # paste0("Aged 18+\n with 2 doses on or before ", format(study_dates$lastvax2_date, "%d %b %Y")),
      crit == "c1" ~ "  and clinically at-risk or aged 75+",
      crit == "c2" ~ "  with no missing demographic information",
      crit == "c3" ~ "  and not a health and social care worker",
      crit == "c4" ~ "  and not a care/nursing home resident, end-of-life or housebound",
      crit == "c5" ~ "  and no COVID-19-related events within 28 days",
      crit == "c6" ~ "  and not admitted in hospital at time of booster",
      TRUE ~ NA_character_
    )
  ) #
write_csv(data_flowchart_rounded, fs::path(output_dir, "flowchart.csv"))




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

