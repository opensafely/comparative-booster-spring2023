
# # # # # # # # # # # # # # # # # # # # #
# Purpose: Combine km estimates for different outcomes
#  - import matched data
#  - adds outcome variable and restricts follow-up
#  - gets KM estimates
#  - The script must be accompanied by two arguments:
#    `matchset` - the matching set used for matching
#    `subgroup` - the subgroup variable, which is used to stratify KM estimates
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  removeobjects <- FALSE
  matchset <- "A"

} else {
  removeobjects <- TRUE
  matchset <- args[[1]]

}

## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')

## Import custom user functions from lib
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "survival.R"))
source(here("lib", "functions", "redaction.R"))

## Import design elements
source(here("lib", "design", "design.R"))


output_dir <- here("output", "match", matchset, "km", "combined")
fs::dir_create(output_dir)

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){
  metaparams <-
    expand_grid(
      outcome = factor(c("postest", "coviddeath")),
      subgroup = factor(recoder$subgroup),
      #treatment = recoder$treatment
    ) %>%
    mutate(
      #treatment_descr = fct_recode(as.character(treatment),  !!!recoder$treatment),
      outcome_descr = fct_recode(as.character(outcome),  !!!recoder$outcome),
      subgroup_descr = fct_recode(subgroup,  !!!recoder$subgroup),
    )
} else {
  metaparams <-
    expand_grid(
      outcome = factor(c("postest", "covidemergency", "covidadmittedproxy1", "covidadmitted", "coviddeath", "noncoviddeath")),
      subgroup = factor(recoder$subgroup),
      #treatment = recoder$treatment
    ) %>%
    mutate(
      #treatment_descr = fct_recode(as.character(treatment),  !!!recoder$treatment),
      outcome_descr = fct_recode(as.character(outcome),  !!!recoder$outcome),
      subgroup_descr = fct_recode(subgroup,  !!!recoder$subgroup),
    )
}


km_estimates <- metaparams %>%
  mutate(
    data = pmap(list(matchset, subgroup, outcome), function(matchset, subgroup, outcome) {
      dat <- read_csv(here("output", "match", matchset, "km", subgroup, outcome, glue("km_estimates.csv")))
      dat %>% select(-all_of(c(subgroup)))
    }
    )
  ) %>%
  unnest(data)

write_csv(km_estimates, fs::path(output_dir, "km_estimates.csv"))



km_contrasts_daily <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup){
        dat <- read_csv(here("output", "match", matchset, "km", subgroup, outcome, glue("km_contrasts_daily.csv")))
        dat %>% select(-all_of(subgroup))
      }
    )
  ) %>%
  unnest(data)

write_csv(km_contrasts_daily, fs::path(output_dir, "km_contrasts_daily.csv"))



km_contrasts_overall <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup) {
        dat <- read_csv(here("output", "match", matchset, "km", subgroup, outcome, glue("km_contrasts_overall.csv")))
        dat %>% select(-all_of(subgroup))
      }
    )
  ) %>%
  unnest(data)

write_csv(km_contrasts_overall, fs::path(output_dir, "km_contrasts_overall.csv"))
