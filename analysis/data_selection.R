
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


# Import custom user functions from lib
source(here("lib", "functions", "utility.R"))

# Import design elements
source(here("lib", "design", "design.R"))

# Import redaction functions
source(here("lib", "functions", "redaction.R"))


## create output directories for data ----
fs::dir_create(here("output", "data"))

## create output directories for tables/summaries ----

output_dir <- here("output", "prematch")
fs::dir_create(output_dir)

## Import processed data ----

data_processed <- read_rds(here("output", "data", "data_processed.rds"))


# Define selection criteria ----
data_criteria <- data_processed %>%
  transmute(
    patient_id,
    vax3_type,
    has_age = !is.na(age),
    has_sex = !is.na(sex) & !(sex %in% c("I", "U")),
    has_imd = imd_Q5 != "Unknown",
    #has_ethnicity = !is.na(ethnicity_combined),
    has_region = !is.na(region),
    #has_msoa = !is.na(msoa),
    isnot_hscworker = !hscworker,
    isnot_carehomeresident = !care_home_combined,
    isnot_endoflife = !endoflife,
    isnot_housebound = !housebound,
    vax1_afterfirstvaxdate = case_when(
      (vax1_type=="pfizer") & (vax1_date >= study_dates$firstpfizer_date) ~ TRUE,
      (vax1_type=="az") & (vax1_date >= study_dates$firstaz_date) ~ TRUE,
      (vax1_type=="moderna") & (vax1_date >= study_dates$firstmoderna_date) ~ TRUE,
      TRUE ~ FALSE
    ),
    consistentvax3date = vax3_date == anycovidvax_3_date,
    vax3_afterstartdate = vax3_date >= study_dates$studystart_date,
    vax3_beforeenddate = vax3_date <= study_dates$studyend_date,
    vax12_homologous = vax1_type==vax2_type,
    has_vaxgap12 = vax2_date >= (vax1_date+17), # at least 17 days between first two vaccinations
    has_vaxgap23 = vax3_date >= (vax2_date+17) | is.na(vax3_date), # at least 17 days between second and third vaccinations
    has_knownvax1 = vax1_type %in% c("pfizer", "az"),
    has_knownvax2 = vax2_type %in% c("pfizer", "az"),
    has_expectedvax3type = vax3_type %in% c("pfizer", "moderna"),
    has_norecentcovid = vax3_date - anycovid_0_date >= 90 | is.na(anycovid_0_date),
    isnot_inhospital = !inhospital,

    jcvi_group_6orhigher = jcvi_group %in% as.character(1:6),

    include = (
      consistentvax3date &
      vax1_afterfirstvaxdate &
      vax3_afterstartdate & vax3_beforeenddate & has_expectedvax3type &
      has_age & has_sex & has_imd & has_region & #has_ethnicity &
      has_vaxgap12 & has_vaxgap23 & has_knownvax1 & has_knownvax2 & vax12_homologous &
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
arrow::write_feather(data_cohort, here("output", "data", "data_cohort.feather"))

data_inclusioncriteria <- data_criteria %>%
  transmute(
    patient_id,
    vax3_type,
    c0 = consistentvax3date & vax1_afterfirstvaxdate & vax3_afterstartdate & vax3_beforeenddate & has_expectedvax3type,
    c1 = c0 & (has_age & has_sex & has_imd & has_region),
    c2 = c1 & (has_vaxgap12 & has_vaxgap23 & has_knownvax1 & has_knownvax2 & vax12_homologous),
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
  group_by(vax3_type) %>%
  summarise(
    across(.fns=sum)
  ) %>%
  pivot_longer(
    cols=-vax3_type,
    names_to="criteria",
    values_to="n"
  ) %>%
  group_by(vax3_type) %>%
  mutate(
    n_exclude = lag(n) - n,
    pct_exclude = n_exclude/lag(n),
    pct_all = n / first(n),
    pct_step = n / lag(n),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "Aged 18+ and recieved booster dose of BNT162b2 or mRNA-1273 between 29 October 2021 and 31 January 2022", # paste0("Aged 18+\n with 2 doses on or before ", format(study_dates$lastvax2_date, "%d %b %Y")),
      crit == "c1" ~ "  with no missing demographic information",
      crit == "c2" ~ "  with homologous primary vaccination course of BNT162b2 or ChAdOx1",
      crit == "c3" ~ "  and not a health and social care worker",
      crit == "c4" ~ "  and not a care/nursing home resident, end-of-life or housebound",
      crit == "c5" ~ "  and no COVID-19-related events within 90 days",
      crit == "c6" ~ "  and not admitted in hospital at time of booster",
      TRUE ~ NA_character_
    )
  ) #
#write_csv(data_flowchart, here("output", "data", "flowchart.csv"))



## flowchart -- rounded so disclosure-safe ----
data_flowchart_rounded <-
  data_inclusioncriteria %>%
  select(-patient_id) %>%
  group_by(vax3_type) %>%
  summarise(
    across(.fns=~ceiling_any(sum(.), 7))
  ) %>%
  pivot_longer(
    cols=-vax3_type,
    names_to="criteria",
    values_to="n"
  ) %>%
  group_by(vax3_type) %>%
  mutate(
    n_exclude = lag(n) - n,
    pct_exclude = n_exclude/lag(n),
    pct_all = n / first(n),
    pct_step = n / lag(n),
    crit = str_extract(criteria, "^c\\d+"),
    criteria = fct_case_when(
      crit == "c0" ~ "Aged 18+ and received booster dose of BNT162b2 or mRNA-1273 between 29 October 2021 and 31 January 2022", # paste0("Aged 18+\n with 2 doses on or before ", format(study_dates$lastvax2_date, "%d %b %Y")),
      crit == "c1" ~ "  with no missing demographic information",
      crit == "c2" ~ "  with homologous primary vaccination course of BNT162b2 or ChAdOx1",
      crit == "c3" ~ "  and not a health and social care worker",
      crit == "c4" ~ "  and not a care/nursing home resident, end-of-life or housebound",
      crit == "c5" ~ "  and no COVID-19-related events within 90 days",
      crit == "c6" ~ "  and not admitted in hospital at time of booster",
      TRUE ~ NA_character_
    )
  ) #
write_csv(data_flowchart_rounded, fs::path(output_dir, "flowchart.csv"))




# table 1 style baseline characteristics amongst those eligible for matching ----



var_labels <- list(
  N  ~ "Total N",
  treatment_descr ~ "Vaccine type",
  vax12_type_descr ~ "Primary vaccine course",
  vax23_interval ~ "Dose 2/3 interval",
  age ~ "Age",
  ageband ~ "Age",
  sex ~ "Sex",
  ethnicity_combined ~ "Ethnicity",
  imd_Q5 ~ "Deprivation",
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
  cancer ~ "Cancer, within previous 3 years",

  #multimorb ~ "Morbidity count",
  immunosuppressed ~ "Immunosuppressed",
  asplenia ~ "Asplenia or poor spleen function",
  learndis ~ "Learning disabilities",
  sev_mental ~ "Serious mental illness",

  prior_tests_cat ~ "Number of SARS-CoV-2 tests",

  prior_covid_infection ~ "Prior documented SARS-CoV-2 infection"
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

tab_summary_prematch <-
  data_cohort %>%
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
    statistic = list(N = "{N}")
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

write_csv(data_smd, fs::path(output_dir, "smd.csv"))


