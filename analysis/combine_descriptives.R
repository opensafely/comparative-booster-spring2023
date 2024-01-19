# # # # # # # # # # # # # # # # # # # # #
# Purpose: Combine descriptive statistics from pre-modelling / pre-contrasting scripts (non-outcome specific data)
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----


## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')
library("fs")

## Import custom user functions from lib
source(here("analysis", "functions", "utility.R"))

## Import design elements
source(here("analysis", "design", "design.R"))

## import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort <- "age75plus"
  #matchset <- "A"

} else {
  cohort <- args[[1]]
  #matchset <- args[[2]]
}


metaparams <-
  expand_grid(
    matchset = c("A", "B"),
  )


output_dir <- here("output", "combine", cohort, "descriptives")
dir_create(output_dir)

## flowcharts ----

## total vaccinated in time period, by vaccine type
# choosing cv cohort arbitrarily, could use any cohort as output is the same
if(cohort=="cv"){
  file_copy(here("output", cohort, "total_allcohorts_rounded.csv"), here("output", "combine", "flowchart_totals_allcohorts_rounded.csv"), overwrite = TRUE)
}

file_copy(here("output", cohort, "total_rounded.csv"), path(output_dir, "flowchart_totals_rounded.csv"), overwrite = TRUE)

flowchart <-
  metaparams %>%
  mutate(
    data = pmap(list(matchset), function(matchset) {
      read_csv(here("output", cohort, matchset, "report", "flowchart_rounded.csv"))
    })
  ) %>%
  unnest(data)

write_csv(flowchart, path(output_dir, "flowchart_rounded.csv"))

## coverage ----

coverage <-
  metaparams %>%
  mutate(
    data = pmap(list(matchset), function(matchset) {
      read_csv(here("output", cohort, matchset, "report", "data_coverage.csv"))
    })
  ) %>%
  unnest(data)

write_csv(coverage, path(output_dir, "coverage_rounded.csv"))



## table 1 ----

table1_pre <-
  read_csv(here("output", cohort, "table1.csv")) %>%
  mutate(matchset = "prematch", .before=1)

table1_post <-
  metaparams %>%
  mutate(
    data = pmap(list(matchset), function(matchset) {
      read_csv(here("output", cohort, matchset, "report", "table1.csv"))
    })
  ) %>%
  unnest(data)

table1 <- bind_rows(
  table1_pre,
  table1_post
)

write_csv(table1, path(output_dir, "table1_rounded.csv"))

## move event counts data ----


### by treatment ----
eventcounts <-
  metaparams %>%
  mutate(
    data = pmap(list(matchset), function(matchset) {
      read_rds(here("output", cohort, matchset, "eventcounts", "eventcounts.rds"))
    })
  ) %>%
  unnest(data)

write_csv(eventcounts, path(output_dir, "eventcounts.csv"))



