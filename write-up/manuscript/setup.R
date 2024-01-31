
library('tidyverse')
library('here')
library('glue')
library('lubridate')
library('gt')
library('patchwork')
library('scales')
library("fs")

# where are the outputs (ie the inputs for this manuscript!) saved?
#output_dir_os <- here("output", "release-objects")
output_dir_os <- here("released-output", "release1")


## import functions and design elements
source(here("analysis", "functions", "utility.R"))
source(here("analysis", "design", "design.R"))

study_dates_format <- map(study_dates, ~format(as.Date(.), "%e %B %Y"))

# where should we put the objects created within this rmd script?
output_dir_qmd <- here("write-up", "manuscript", "figures_manual")
dir_create(output_dir_qmd)

# only applicable if `self-contained: yes` option is enables in yaml header
# knitr::opts_chunk$set(
#   echo = TRUE,
#   fig.path = paste0(output_dir_rmd, "/"),
#   fig.pos = 'H' #to stop figures floating around when rendering to pdf
# )


## define outcome sets

outcomes_effectiveness <- c(
  #"covidemergency",
  "covidcritcare",
  "covidadmitted",
  "coviddeath",
  "noncoviddeath",
  NULL
) %>%
  set_names(., .)

 outcomes_safety <- c(
  "pericarditis",
  "myocarditis",
  NULL
) %>%
  set_names(., .)


outcomes <- c(outcomes_effectiveness, outcomes_safety)

subgroups <-
  c(
    "all",
    "vax_previous_group",
    "ageband",
    "cv",
    NULL
  ) %>%
  set_names(., .)


