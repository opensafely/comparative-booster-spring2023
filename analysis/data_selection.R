
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

# Import custom user functions from lib
source(here("lib", "functions", "utility.R"))

# Import design elements
source(here("lib", "design", "design.R"))


## create output directories ----
fs::dir_create(here("output", "data"))


## Import processed data ----

data_processed <- read_rds(here("output", "data", "data_processed.rds"))


# Define selection criteria ----
data_criteria <- data_processed %>%
  transmute(
    patient_id,
    vax3_type,
    has_age = !is.na(age),
    has_sex = !is.na(sex) & !(sex %in% c("I", "U")),
    has_imd = !is.na(imd),
    has_ethnicity = !is.na(ethnicity_combined),
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
    vax3_afterstartdate = vax3_date >= study_dates$studystart_date,
    vax3_beforeenddate = vax3_date <= study_dates$studyend_date,
    vax12_homologous = vax1_type==vax2_type,
    has_vaxgap12 = vax2_date >= (vax1_date+17), # at least 17 days between first two vaccinations
    has_vaxgap23 = vax3_date >= (vax2_date+17) | is.na(vax3_date), # at least 17 days between second and third vaccinations
    has_knownvax1 = vax1_type %in% c("pfizer", "az"),
    has_knownvax2 = vax2_type %in% c("pfizer", "az"),
    has_expectedvax3type = vax3_type %in% c("pfizer", "moderna"),
    has_norecentcovid = vax3_date - anycovid_0_date >= 90 | is.na(anycovid_0_date),
    isnot_inhospitalunplanned = !inhospital_unplanned,

    jcvi_group_6orhigher = jcvi_group %in% as.character(1:6),

    include = (
      #jcvi_group_6orhigher & # temporary until more data available
      vax1_afterfirstvaxdate &
      vax3_afterstartdate & vax3_beforeenddate & has_expectedvax3type &
      has_age & has_sex & has_imd & has_ethnicity & has_region &
      has_vaxgap12 & has_vaxgap23 & has_knownvax1 & has_knownvax2 & vax12_homologous &
      isnot_hscworker &
      isnot_carehomeresident & isnot_endoflife & isnot_housebound &
      has_norecentcovid &
      isnot_inhospitalunplanned

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
    c0 = vax1_afterfirstvaxdate & vax3_afterstartdate & vax3_beforeenddate & has_expectedvax3type,
    c1 = c0 & (has_age & has_sex & has_imd & has_ethnicity & has_region),
    c2 = c1 & (has_vaxgap12 & has_vaxgap23 & has_knownvax1 & has_knownvax2 & vax12_homologous),
    c3 = c2 & (isnot_hscworker ),
    c4 = c3 & (isnot_carehomeresident & isnot_endoflife & isnot_housebound),
    c5 = c4 & (has_norecentcovid),
    c6 = c5 & (isnot_inhospitalunplanned),
  ) %>%
  filter(c0)

write_rds(data_inclusioncriteria, here("output", "data", "data_inclusioncriteria.rds"), compress="gz")

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
      crit == "c0" ~ "Aged 18+ and recieved booster dose of BNT162b2 or Moderna between 29 October 2021 and 31 January 2022", # paste0("Aged 18+\n with 2 doses on or before ", format(study_dates$lastvax2_date, "%d %b %Y")),
      crit == "c1" ~ "  with no missing demographic information",
      crit == "c2" ~ "  with homologous primary vaccination course of pfizer or AZ",
      crit == "c3" ~ "  and not a HSC worker",
      crit == "c4" ~ "  and not a care/nursing home resident, end-of-life or housebound",
      crit == "c5" ~ "  and no COVID-19-related events within 90 days",
      crit == "c6" ~ "  and not in hospital at time of booster",
      TRUE ~ NA_character_
    )
  ) #
#write_csv(data_flowchart, here("output", "data", "flowchart.csv"))



## flowchart -- rounded -- disclosure-safe
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
      crit == "c0" ~ "Aged 18+ and recieved booster dose of BNT162b2 or Moderna between 29 October 2021 and 31 January 2022", # paste0("Aged 18+\n with 2 doses on or before ", format(study_dates$lastvax2_date, "%d %b %Y")),
      crit == "c1" ~ "  with no missing demographic information",
      crit == "c2" ~ "  with homologous primary vaccination course of pfizer or AZ",
      crit == "c3" ~ "  and not a HSC worker",
      crit == "c4" ~ "  and not a care/nursing home resident, end-of-life or housebound",
      crit == "c5" ~ "  and no COVID-19-related events within 90 days",
      crit == "c6" ~ "  and not in hospital at time of booster",
      TRUE ~ NA_character_
    )
  ) #
write_csv(data_flowchart_rounded, here("output", "data", "flowchart.csv"))


