# # # # # # # # # # # # # # # # # # # # #
# Purpose: creates metadata objects for aspects of the study design
# This script should be sourced (ie `source(".../design.R")`) in the analysis scripts
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('here')
## create output directories ----
fs::dir_create(here("lib", "design"))



# import globally defined repo variables
study_dates <-
  jsonlite::read_json(path=here("lib", "design", "study-dates.json")) %>%
  map(as.Date)

# define outcomes ----

events_lookup <- tribble(
  ~event, ~event_var, ~event_descr,

  # other
  "test", "covid_test_date", "SARS-CoV-2 test",

  # effectiveness
  "postest", "positive_test_date", "Positive SARS-CoV-2 test",
  "covidemergency", "covidemergency_date", "COVID-19 A&E attendance",
  "covidadmitted", "covidadmitted_date", "COVID-19 hospitalisation",
  "noncovidadmitted", "noncovidadmitted_date", "Non-COVID-19 hospitalisation",
  "covidadmittedproxy1", "covidadmittedproxy1_date", "COVID-19 hospitalisation (A&E proxy)",
  "covidadmittedproxy2", "covidadmittedproxy2_date", "COVID-19 hospitalisation (A&E proxy v2)",
  "covidcritcare", "covidcritcare_date", "COVID-19 critical care",
  "coviddeath", "coviddeath_date", "COVID-19 death",
  "noncoviddeath", "noncoviddeath_date", "Non-COVID-19 death",
  "death", "death_date", "Any death",

  # safety
  "admitted", "admitted_unplanned_1_date", "Unplanned hospitalisation",
  "emergency", "emergency_date", "A&E attendance",
)

treatement_lookup <-
  tribble(
    ~treatment, ~treatment_descr,
    "pfizer", "BNT162b2",
    "az", "ChAdOx1-S",
    "moderna", "mRNA-1273",
    "pfizer-pfizer", "BNT162b2",
    "az-az", "ChAdOx1-S",
    "moderna-moderna", "mRNA-1273"
  )

# where to split follow-up time after recruitment
postbaselinecuts <- c(0,7,14,21,28,42,56,70,84)

# maximum follow-up duration

maxfup <- max(postbaselinecuts)


## lookups to convert coded variables to full, descriptive variables ----

recoder <-
  list(
    subgroups = c(
      `Main` = "all",
      `Primary vaccine course` = "vax12_type",
      `Age` = "age65plus",
      `Prior SARS-CoV-2 infection status` = "prior_covid_infection",
      `Clinical vulnerability` = "cev_cv",
      `Variant era` = "variantera"
    ),
    status = c(
      `Unmatched`= "unmatched",
      `Matched` = "matched"
    ),
    treatment = c(
      `BNT162b2` = "0",
      `Moderna mRNA-1273` = "1"
    ),
    outcome = c(
      "Positive SARS-CoV-2 test"= "postest",
      "COVID-19 A&E attendance" = "covidemergency",
      "COVID-19 hospitalisation" = "covidadmitted",
      "COVID-19 hospitalisation (A&E proxy)" = "covidadmittedproxy1",
      "COVID-19 hospitalisation (A&E proxy v2)" = "covidadmittedproxy2",
      "COVID-19 critical care" = "covidcritcare",
      "COVID-19 death" = "coviddeath",
      "Non-COVID-19 death" = "noncoviddeath",
      "All-cause death" = "death"
    ),
    all = c(` ` = "all"),
    vax12_type = c(
      `BNT162b2` = "pfizer-pfizer",
      `ChAdOx1-S` = "az-az"
    ),
    age65plus = c(
      `18-64` = "FALSE",
      `65 and over` = "TRUE"
    ),
    cev_cv = c(
      "Clinically extremely vulnerable" = "Clinically extremely vulnerable",
      "Clinically at-risk" = "Clinically at-risk",
      "Not clinically at-risk" = "Not clinically at-risk"
    ),
    prior_covid_infection = c(
      `No prior SARS-CoV-2 infection` = "FALSE",
      `Prior SARS-CoV-2 infection` = "TRUE"
    ),
    variantera = c(
      `Delta (29 Nov - 31 Dec)` = "Delta (29 Nov - 31 Dec)",
      `Omicron (1 Jan onwards)` = "Omicron (1 Jan onwards)"
    )
  )

## model formulae ----

treated_period_variables <- paste0("treatment_period_id", "_", seq_len(length(postbaselinecuts)-1))


if(exists("matchset")){

  local({

    matching_variables=list()

    # matching set A
    exact <- c(

      "vax3_date",
      "vax12_type",
      "jcvi_ageband",
      "region",
      "cev_cv",
      "imd_Q5",
      NULL
    )

    caliper <- c(
      age = 3,
      vax2_day = 7,
      #imd = 1000,
      NULL
    )

    all <- c(exact, names(caliper))


    matching_variables$A = lst(exact, caliper, all)

    # matching set B
    exact <- c(

      "vax3_date",
      "jcvi_ageband",
      "cev_cv",
      "sex",
      "vax12_type",
      #"vax2_week",
      "stp",
      "imd_Q5",


      "multimorb",
      "prior_covid_infection",
      #"immunosuppressed",
      #"status_hospplanned"
      NULL
    )

    caliper <- c(
      age = 3,
      vax2_day = 7,
      NULL
    )

    all <- c(exact, names(caliper))

    matching_variables$B = lst(exact, caliper, all)


    matching_variables <<- matching_variables

  })

}
