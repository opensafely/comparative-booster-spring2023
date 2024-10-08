# # # # # # # # # # # # # # # # # # # # #
# Purpose: creates metadata objects for aspects of the study design
# This script should be sourced (ie `source(".../design.R")`) in the analysis scripts
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('here')

## create output directories ----
fs::dir_create(here("analysis", "design"))



# import globally defined repo variables
study_dates <-
  jsonlite::read_json(path=here("analysis", "design", "study-dates.json")) %>%
  map(as.Date)

# define outcomes ----

# maximum follow-up duration

maxfup_effectiveness <- 112L
maxfup_safety <- 28L

events_lookup <- tribble(
  ~event, ~event_var, ~event_descr, ~maxfup,

  # other
  "test", "covid_test_date", "SARS-CoV-2 test", maxfup_effectiveness,

  # effectiveness
  "postest", "positive_test_date", "Positive SARS-CoV-2 test", maxfup_effectiveness,
  "covidemergency", "covidemergency_date", "COVID-19 A&E attendance", maxfup_effectiveness,
  "covidadmitted", "covidadmitted_date", "COVID-19 hospitalisation", maxfup_effectiveness,
  "noncovidadmitted", "noncovidadmitted_date", "Non-COVID-19 hospitalisation", maxfup_effectiveness,
  "covidadmittedproxy1", "covidadmittedproxy1_date", "COVID-19 hospitalisation (A&E proxy)", maxfup_effectiveness,
  "covidadmittedproxy2", "covidadmittedproxy2_date", "COVID-19 hospitalisation (A&E proxy v2)", maxfup_effectiveness,
  "covidcritcare", "covidcritcare_date", "COVID-19 critical care", maxfup_effectiveness,
  "coviddeath", "coviddeath_date", "COVID-19 death", maxfup_effectiveness,
  "death", "death_date", "Any death", maxfup_effectiveness,

  # safety
  "admitted", "admitted_unplanned_1_date", "Unplanned hospitalisation", maxfup_safety,
  "emergency", "emergency_date", "A&E attendance", maxfup_safety,
  "pericarditis", "pericarditis_date", "Pericarditis", maxfup_safety,
  "myocarditis", "myocarditis_date", "Myocarditis", maxfup_safety,

  # negative control
  "noncoviddeath", "noncoviddeath_date", "Non-COVID-19 death", maxfup_effectiveness,
  "fracture", "fracture_date", "Fracture", maxfup_effectiveness,
)



vax_product_lookup = c(
  "pfizer"="COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)",
  "az"="COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)",
  "moderna"="COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)",
  "pfizerBA1"="Comirnaty Original/Omicron BA.1 COVID-19 Vacc md vials",
  "pfizerBA45"="Comirnaty Original/Omicron BA.4-5 COVID-19 Vacc md vials",
  "pfizerXBB15"="Comirnaty Omicron XBB.1.5 COVID-19 Vacc md vials",
  "sanofi"="COVID-19 Vacc VidPrevtyn (B.1.351) 0.5ml inj multidose vials",
  "modernaomicron"="COVID-19 Vac Spikevax (Zero)/(Omicron) inj md vials",
  "pfizerchildren"="COVID-19 mRNA Vaccine Comirnaty Children 5-11yrs 10mcg/0.2ml dose conc for disp for inj MDV (Pfizer)",
  "azhalf"="COVID-19 Vac AZD2816 (ChAdOx1 nCOV-19) 3.5x10*9 viral part/0.5ml dose sol for inj MDV (AstraZeneca)",
  "modernaXBB15"="COVID-19 Vacc Spikevax (XBB.1.5) 0.1mg/1ml inj md vials"
)

# vax_type_lookup = c(
#   "BNT162b2"="pfizer",
#   "ChAdOx1"="az",
#   "mRNA-1273"="moderna",
#   "BNT162b2/BA.1"="pfizerBA1",
#   "BNT162b2/BA.4-5"="pfizerBA45",
#   "BNT162b2/XBB.1.5"="pfizerXBB15",
#   "VidPrevtyn" = "sanofi",
#   "mRNA-1273/omicron"="modernaomicron",
#   "BNT162b2/children"="pfizerchildren",
#   "ChAdOx1/2"="azhalf",
#   "mRNA-1273/XBB.1.5"="modernaXBB15",
#   "Other"="other"
# )


treatment_lookup <-
  tribble(
    ~treatment, ~treatment_descr,
    "pfizerBA45", "pfizer/BA.4-5",
    "sanofi", "Sanofi",
  )

# where to split follow-up time after recruitment
postbaselinecuts <- c(0,7,14,28,56,84,112)

# redaction threshold
threshold <- 6

## lookups to convert coded variables to full, descriptive variables ----

recoder <-
  lst(
    cohort = c(
      `Clinically vulnerable` = "cv",
      `Aged 75 years or over` = "age75plus"
    ),
    subgroups = c(
      `Main` = "all",
      `Age` = "ageband",
      `Clinically at-risk` = "cv",
      `Prior COVID-19 vaccine count` = "vax_previous_group",
      #`Prior SARS-CoV-2 infection status` = "prior_covid_infection"
      NULL
    ),
    status = c(
      `Unmatched`= "unmatched",
      `Matched` = "matched"
    ),
    treatment = c(
      `pfizer/BA.4-5` = "0",
      `Sanofi` = "1"
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
      "All-cause death" = "death",
      "Fracture" = "fracture",
      "Pericarditis" = "pericarditis",
      "Myocarditis" = "myocarditis",
      NULL
    ),
    all = c(` ` = "all"),
    ageband = c(
      "50-64", "65-74", "75-79", "80-84", "85+"
    ) %>% {set_names(.,.)},
    cv = c(
      `Clinically at-risk` = "TRUE",
      `Not clinically at-risk` = "FALSE"
    ),
    prior_covid_infection = c(
      `No prior SARS-CoV-2 infection` = "FALSE",
      `Prior SARS-CoV-2 infection` = "TRUE"
    ),
    vax_previous_group = c(
      "0-1", "2-4", "5", "6+"
    ) %>% {set_names(.,.)},
  )

## model formulae ----

treated_period_variables <- paste0("treatment_period_id", "_", seq_len(length(postbaselinecuts)-1))

local({
  matching_variables=list()

  # matching set A
  exact <- c(
    "ageband",
    "cv",
    #"sex",
    #"region",
    #"imd_Q5",
      #"multimorb",
    "prior_covid_infection",
    NULL
  )
  caliper <- c(
    boost_day = 3,
    age = 3,
    vax_interval_bigM = 14,
    NULL
  )
  all <- c(exact, names(caliper))
    matching_variables$A = lst(exact, caliper, all)

  # matching set B
  exact <- c(
    "ageband",
    "cv",
    "sex",
    "region",
    #"imd_Q5",
    "vax_previous_group",
    "multimorb",
    "prior_covid_infection",
    "immunosuppressed",
    NULL
  )
  caliper <- c(
    boost_day = 3,
    age = 3,
    vax_interval_bigM = 28,
    imd = 5000,
    NULL
  )
  all <- c(exact, names(caliper))
  matching_variables$B = lst(exact, caliper, all)

  matching_variables <<- matching_variables

})
