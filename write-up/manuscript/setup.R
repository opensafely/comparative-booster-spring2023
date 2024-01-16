
library('tidyverse')
library('here')
library('glue')
library('lubridate')
library('gt')
library('patchwork')
library('scales')

# where are the outputs (ie the inputs for this manuscript!) saved?
output_dir_os <- here("output", "release-objects")
#output_dir_os <- here("released-output", "release-objects-v2")


## import functions and design elements
source(here("analysis", "functions", "utility.R"))
source(here("analysis", "design", "design.R"))

study_dates_format <- map(study_dates, ~format(as.Date(.), "%e %B %Y"))

# where should we put the objects created within this rmd script?
output_dir_qmd <- here("write-up", "manuscript", "figures")
fs::dir_create(output_dir_qmd)

# only applicable if `self-contained: yes` option is enables in yaml header
# knitr::opts_chunk$set(
#   echo = TRUE,
#   fig.path = paste0(output_dir_rmd, "/"),
#   fig.pos = 'H' #to stop figures floating around when rendering to pdf
# )


## define outcome sets

outcomes_primary <- c(
  "covidemergency",
  "covidadmitted",
  #"covidadmittedproxy1",
  #"covidcritcare",
  "coviddeath",
  "noncoviddeath",
  NULL
) %>%
  set_names(., .)

# outcomes_negative <- c(
#   #"noncoviddeath",
#   #"fracture",
#   NULL
# ) %>%
#   set_names(., .)

outcomes <- outcomes_primary

subgroups <-
  c(
    "all",
    "vax_prior_history",
    "ageband",
    "prior_covid_infection",
    "cv",
    NULL
  ) %>%
  set_names(., .)


