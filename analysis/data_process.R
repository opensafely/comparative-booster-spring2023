######################################
# Purpose: process (tidy, clean, reshape, derive) data extracted using ehrQL (or dummy data)
#
# standardises some variables (eg convert to factor) and derives some new ones
# organises vaccination date data to "vax X type", "vax X date" in long format
######################################

# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('lubridate')
library('arrow')
library('here')

## Import custom user functions from lib
source(here("analysis", "functions", "utility.R"))

## Import design elements
source(here("analysis", "design", "design.R"))

## create output directories for data
fs::dir_create(here("output", "data"))



# Import and process data ----

## Import extracted dataset ----
data_extract <- read_feather(here("output", "extracts", "extract.arrow"))

## Process extracted dataset ----
data_processed <- data_extract %>%
  mutate(

    # use short product names
    boost_type = factor(boost_type, vax_product_lookup, names(vax_product_lookup)) %>% fct_explicit_na("other"),

    # binary variable for the exposure
    # helpful for various downstream matching / plotting / table functions
    treatment = case_when(
      boost_type=="pfizerBA45" ~ 0L,
      boost_type=="sanofi" ~ 1L,
      TRUE ~ NA_integer_
    ),

    # boost date represented as an integer, using for matching instead of date-formatted variable to avoid issues
    boost_day = as.integer(boost_date - study_dates$studystart_date),

    ageband = cut(
      age_july2023, # use fixed date to ascertain age so that age bands align with eligibility. because age and vax date are closely matched, this doesn't cause any problems
      breaks=c(-Inf, 16, 50, 65, 75, 80, 85, Inf),
      labels=c("under 16", "16-49", "50-64", "65-74", "75-79", "80-84", "85+"),
      right=FALSE
    ),

    ethnicity = fct_case_when(
      ethnicity == "1" ~ "White",
      ethnicity == "4" ~ "Black",
      ethnicity == "3" ~ "South Asian",
      ethnicity == "2" ~ "Mixed",
      ethnicity == "5" ~ "Other",
      TRUE ~ "Unknown"
    ),

    region = fct_collapse(
      region,
      `East of England` = "East",
      `London` = "London",
      `Midlands` = c("West Midlands", "East Midlands"),
      `North East and Yorkshire` = c("Yorkshire and The Humber", "North East"),
      `North West` = "North West",
      `South East` = "South East",
      `South West` = "South West"
    ),

    imd_Q5 = cut(
      imd,
      breaks = (32844/5)*c(-0.1,1,2,3,4,5),
      labels = c("1 most deprived", "2", "3", "4", "5 least deprived"),
      include.lowest = TRUE,
      right = FALSE
    ),

    rural_urban_group = fct_case_when(
      rural_urban %in% c(1,2) ~ "Urban conurbation",
      rural_urban %in% c(3,4) ~ "Urban city or town",
      rural_urban %in% c(5,6,7,8) ~ "Rural town or village",
      TRUE ~ NA_character_
    ),

    care_home_combined = care_home_tpp | care_home_code, # any carehome flag

    immuno_any = immunosuppressed | asplenia | cancer | solid_organ_transplant |  hiv_aids,

    # clinically at-risk group
    cv = immuno_any | chronic_kidney_disease | chronic_resp_disease | diabetes | chronic_liver_disease |
      chronic_neuro_disease | chronic_heart_disease | learndis | sev_mental,

    multimorb =
      (sev_obesity) +
      (chronic_heart_disease) +
      (chronic_kidney_disease)+
      (diabetes) +
      (chronic_liver_disease)+
      (chronic_resp_disease)+
      (chronic_neuro_disease)+
      (cancer)+
      #(learndis)+
      #(sev_mental),
      0,
    multimorb = cut(multimorb, breaks = c(0, 1, 2, Inf), labels=c("0", "1", "2+"), right=FALSE),

    ## define additional subgroups used for subgroup analyses

    all=factor("all"),
    age75plus = age_july2023>=75,

    prior_tests_cat = cut(prior_covid_test_frequency, breaks=c(0, 1, 2, 3, Inf), labels=c("0", "1", "2", "3+"), right=FALSE),

    prior_covid_infection = (!is.na(postest_0_date))  | (!is.na(covidemergency_0_date)) | (!is.na(covidadmitted_0_date)) | (!is.na(primary_care_covid_case_0_date)),


    ## process outcomes data

    # latest covid event before study start
    anycovid_0_date = pmax(postest_0_date, covidemergency_0_date, covidadmitted_0_date, na.rm=TRUE),

    coviddeath_date = if_else(death_cause_covid, death_date, NA_Date_),
    noncoviddeath_date = if_else(!is.na(death_date) & is.na(coviddeath_date), death_date, as.Date(NA_character_)),

    # replace events dates with more severe dates if the precede less severe dates
    #covidemergency_date = pmin(covidemergency_date, covidadmitted_date, covidcritcare_date, coviddeath_date, na.rm=TRUE),
    #covidadmitted_date = pmin(covidadmitted_date, covidcritcare_date, coviddeath_date, na.rm=TRUE),
    #covidcritcare_date = pmin(covidcritcare_date, coviddeath_date, na.rm=TRUE),

    # earliest covid event after study start
    anycovid_date = pmin(postest_date, covidemergency_date, covidadmitted_date, coviddeath_date, na.rm=TRUE),

    # cause_of_death = fct_case_when(
    #   !is.na(coviddeath_date) ~ "covid-related",
    #   !is.na(death_date) ~ "not covid-related",
    #   TRUE ~ NA_character_
    # ),

    cause_of_death = fct_case_when(
      death_cause_covid ~ "covid-related",
      !death_cause_covid ~ "not covid-related",
      TRUE ~ NA_character_
    ),

    fracturedeath_date = if_else(death_cause_fracture, death_date, NA_Date_),
    fracture_date = pmin(fractureemergency_date, fractureadmitted_date, fracturedeath_date, na.rm=TRUE),

  )

# Process vaccination dates ----

## Reshape vaccination data from wide to long ----
data_vax <-
  data_processed %>%
  select(
    patient_id,
    matches("covid_vax\\_\\d+\\_date"),
    matches("covid_vax_type_\\d+"),
  ) %>%
  pivot_longer(
    cols = -patient_id,
    names_to = c(".value", "vax_index"),
    names_pattern = "^(.*)_(\\d+)",
    values_drop_na = TRUE,
    names_transform = list(vax_index = as.integer)
  ) %>%
  rename(
    vax_date = covid_vax,
    vax_type = covid_vax_type,
  ) %>%
  # relabel vaccination codes from long ttp product names to short readable names
  mutate(
    vax_type = fct_recode(factor(vax_type, vax_product_lookup), !!!vax_product_lookup) %>% fct_explicit_na("other")
  ) %>%

  # calculate time between vaccine intervals
  arrange(patient_id, vax_date) %>%
  group_by(patient_id) %>%
  mutate(
    vax_interval = as.integer(vax_date - lag(vax_date,1)),
    duplicate = vax_interval==0
  ) %>%
  ungroup() %>%

  # remove vaccinations that occur after booster date
  # boost_date, as per dataset_definition, is the first vaccination to occur between 1 April and 30 June 2023
  left_join(
    data_processed %>% select(patient_id, boost_date), by = "patient_id"
  ) %>%
  filter(
    vax_date<=boost_date
  )

write_rds(data_vax, here("output", "data", "data_vaxlong.rds"), compress="gz")

## Summarise vaccination history to one-row-per-patient info ----
data_vax_history <- data_vax %>%
  group_by(patient_id) %>%
  summarise(
    vax_count = n(),
    vax_previous_count = last(vax_index)-1,
    vax_interval = last(vax_interval),
    vax_intervals_atleast14days = all(vax_interval[-1]>=14),
    vax_dates_possible = first(vax_date)>=as.Date("2020-06-01"), # earlier than 2020-12-08 to include trial participants, but excludes "unknowns" coded as eg 1900-01-01
    vaxhist_pfizer = "pfizer" %in% (vax_type[-vax_count]),
    vaxhist_az = "az" %in% (vax_type[-vax_count]),
    vaxhist_moderna = "moderna" %in% (vax_type[-vax_count]),
    vaxhist_pfizerBA1 = "pfizerBA1" %in% (vax_type[-vax_count]),
    vaxhist_pfizerBA45 = "pfizerBA45" %in% (vax_type[-vax_count]),
    vaxhist_pfizerXBB15 = "pfizerXBB15" %in% (vax_type[-vax_count]),
    vaxhist_sanofi = "sanofi" %in% (vax_type[-vax_count]),
    vaxhist_modernaomicron = "modernaomicron" %in% (vax_type[-vax_count]),
    vaxhist_modernaXBB15 = "modernaXBB15" %in% (vax_type[-vax_count]),
  ) %>%
  ungroup() %>%
  mutate(
    vax_interval_bigM = if_else(vax_previous_count==0, 365L*5L, vax_interval) #if spring "booster" is first recorded vaccine, then set vax_interval to be very large
  )

stopifnot("vax_count should be equal to vax_previous_count+1" = all(data_vax_history$vax_count == data_vax_history$vax_previous_count+1))
table(data_vax_history$vaxhist_sanofi)
table(data_vax_history$vaxhist_pfizerBA45)

# Combine and output ----

data_processed <- data_processed %>%
  left_join(data_vax_history, by ="patient_id") %>%
  select(-starts_with("covid_vax_"))

write_rds(data_processed, here("output", "data", "data_processed.rds"), compress="gz")


