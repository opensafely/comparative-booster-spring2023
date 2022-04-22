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
  "covidcc", "covidcc_date", "COVID-19 critical care",
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
postbaselinecuts <- c(0,7,14,28,42,70)

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
      `Clinical vulnerability` = "cev_cv"
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
      "COVID-19 critical care" = "covidcc",
      "COVID-19 death" = "coviddeath",
      "Non-COVID-19 death" = "noncoviddeath",
      "All-cause death" = "death"
    ),

    vax12_type = c(
      `BNT162b2` = "pfizer-pfizer",
      `ChAdOx1-S` = "az-az"
    ),
    age65plus = c(
      `Aged 18-64` = "FALSE",
      `Aged 65 and over` = "TRUE"
    ),
    prior_covid_infection = c(
      `No prior SARS-CoV-2 infection` = "FALSE",
      `Prior SARS-CoV-2 infection` = "TRUE"
    )
  )



## lookups for subgroup analyses ----
## NOTE: subgroups must belong to the exact matching set (or be exactly matched by other variables)
subgroups <-
  list(
    main = c("all"),
    vax12_type = c(
      `BNT162b2` = "pfizer-pfizer",
      `ChAdOx1` = "az-az"
    ),
    age65plus = c(TRUE, FALSE),
    prior_covid_infection = c(TRUE, FALSE)
  )



## model formulae ----

treated_period_variables <- paste0("treatment_period_id", "_", seq_len(length(postbaselinecuts)-1))


# unadjusted
formula_vaxonly <- as.formula(
  str_c(
    "Surv(tstart, tstop, ind_outcome) ~ ",
    str_c(treated_period_variables, collapse = " + ")
  )
)

#formula_vaxonly <- Surv(tstart, tstop, ind_outcome) ~ treated:strata(fup_period)

# cox stratification
formula_strata <- . ~ . +
  strata(vax3_date) +
  strata(region) +
  #strata(jcvi_group) +
  strata(vax12_type)

formula_demog <- . ~ . +
  poly(age, degree=2, raw=TRUE) +
  sex +
  imd_Q5 +
  ethnicity_combined

formula_clinical <- . ~ . +
  prior_tests_cat +
  multimorb +
  learndis +
  sev_mental +
  immunosuppressed +
  asplenia

formula_timedependent <- . ~ . +
  prior_covid_infection +
  inhospital_planned


if(exists("matchset")){

  local({

    matching_variables=list()

    # matching set A
    exact <- c(

      "vax3_date",
      "jcvi_ageband",
      "cev_cv",
      "vax12_type",
      #"vax2_week",
      "region",
      #"sex",
      #"cev_cv",

      #"multimorb",
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


    matching_variables$A = lst(exact, caliper, all)

    # matching set B
    exact <- c(

      "vax3_date",
      "jcvi_ageband",
      "cev_cv",
      "vax12_type",
      #"vax2_week",
      "msoa",
      "sex",

      #"multimorb",
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

  # remove matching variables from formulae, as treatment groups are already balanced
  formula_remove_matching <- as.formula(paste(". ~ . - ", paste(matching_variables[[matchset]]$all, collapse=" -"), "- poly(age, degree=2, raw=TRUE)"))

  formula0_pw <- formula_vaxonly %>% update(formula_remove_matching)
  formula1_pw <- formula_vaxonly %>% update(formula_strata) %>% update(formula_remove_matching)
  formula2_pw <- formula_vaxonly %>% update(formula_strata) %>% update(formula_demog) %>% update(formula_remove_matching)
  formula3_pw <- formula_vaxonly %>% update(formula_strata) %>% update(formula_demog) %>% update(formula_clinical) %>% update(formula_timedependent) %>% update(formula_remove_matching)

  formula_allcovariates <- as.formula("1 ~ 1") %>% update(formula_strata) %>% update(formula_demog) %>% update(formula_clinical) %>% update(formula_timedependent) %>% update(formula_remove_matching)

}

model_descr = c(
  "Unadjusted" = "0",
  #"region- and trial-stratified" = "1",
  #"Demographic adjustment" = "2",
  "Full adjustment" = "3"
)
