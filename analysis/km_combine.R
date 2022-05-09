
# # # # # # # # # # # # # # # # # # # # #
# Purpose: Combine km estimates from different outcomes
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
  matchset <- "A"

} else {
  matchset <- args[[1]]

}

## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')

## Import custom user functions from lib
source(here("lib", "functions", "utility.R"))

## Import design elements
source(here("lib", "design", "design.R"))


output_dir <- here("output", "match", matchset, "km", "combined")
fs::dir_create(output_dir)

metaparams <-
  expand_grid(
    outcome = factor(c("postest", "covidemergency", "covidadmittedproxy1", "covidadmitted", "coviddeath", "noncoviddeath")),
    subgroup = factor(recoder$subgroup),
  ) %>%
  mutate(
    outcome_descr = fct_recoderelevel(outcome,  recoder$outcome),
    subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroup),
  )


km_estimates <- metaparams %>%
  mutate(
    data = pmap(list(matchset, subgroup, outcome), function(matchset, subgroup, outcome) {
      subgroup <- as.character(subgroup)
      dat <- read_csv(here("output", "match", matchset, "km", subgroup, outcome, glue("km_estimates.csv")), na="NA", col_types = cols())
      dat %>%
      add_column(
        subgroup_level = as.character(.[[subgroup]]),
        subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
        .before=1
      ) %>%
      select(-all_of(subgroup))
    })
  ) %>%
  unnest(data)

write_csv(km_estimates, fs::path(output_dir, "km_estimates.csv"))


contrasts_daily <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup){
      subgroup <- as.character(subgroup)
      dat <- read_csv(here("output", "match", matchset, "km", subgroup, outcome, glue("contrasts_daily.csv")), na="NA", col_types = cols())
      dat %>%
        add_column(
          subgroup_level = as.character(.[[subgroup]]),
          subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
          .before=1
        ) %>%
        select(-all_of(subgroup))
      }
    )
  ) %>%
  unnest(data)

write_csv(contrasts_daily, fs::path(output_dir, "contrasts_daily.csv"))



contrasts_overall <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup) {
      subgroup <- as.character(subgroup)
      dat <- read_csv(here("output", "match", matchset, "km", subgroup, outcome, glue("contrasts_overall.csv")), na="NA", col_types = cols())
      dat %>%
        add_column(
          subgroup_level = as.character(.[[subgroup]]),
          subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
          .before=1
        ) %>%
        select(-all_of(subgroup))
      }
    )
  ) %>%
  unnest(data)

write_csv(contrasts_overall, fs::path(output_dir, "contrasts_overall.csv"))
