
# # # # # # # # # # # # # # # # # # # # #
# Purpose: report count of
#  - import matched data
#  - reports rate of tests and positive tests
#  - The script must be accompanied by 2 arguments:
#    `matchset` - the matching set used for matching
#    `subgroup` - the subgroup variable, which is used to stratify KM estimates

# # # # # # # # # # # # # # # # # # # # #


# Preliminaries ----



## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')

## Import custom user functions from lib
source(here("analysis", "functions", "utility.R"))
source(here("analysis", "functions", "survival.R"))

## Import design elements
source(here("analysis", "design", "design.R"))





## import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  removeobjects <- FALSE
  cohort <- "age75plus"
  matchset <- "A"
} else {
  removeobjects <- TRUE
  cohort <- args[[1]]
  matchset <- args[[2]]
}

## create output directories ----


output_dir <- here("output", cohort, matchset, "eventcounts")
fs::dir_create(output_dir)



## derive subgroup info ----

metaparams <-
  expand_grid(
    subgroup = as.character(recoder$subgroups),
  ) %>%
  mutate(
    subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroups),
  )

# Import match status data ----

data_matchstatus <- read_rds(here("output", cohort, matchset, "data_matchstatus.rds"))

## import baseline data, restrict to matched individuals and derive time-to-event variables
data_matched <-
  read_rds(here("output", cohort, "data_cohort.rds")) %>%
  mutate(all = "all") %>%
  select(
    # select only variables needed for models to save space
    patient_id, boost_date,
    all_of(metaparams$subgroup),
    death_date,
    covid_test_frequency
  ) %>%
  #filter(patient_id %in% data_matchstatus$patient_id[data_matchstatus$matched]) %>%
  left_join(
    data_matchstatus %>% filter(matched) %>% select(-boost_date),
    .,
    by= c("patient_id")
  ) %>%
  mutate(
    censor_date = pmin(
      death_date,
      study_dates$followupend_date,
      na.rm = TRUE
    ), # only use death and end of follow-up
  )

# report number of tests ----

data_counts <-
  metaparams %>%
  mutate(
    data = map(
      subgroup,
      function(subgroup)
      {
        data_matched %>%
          group_by_(subgroup, "treatment") %>%
          summarise(
            n = roundmid_any(n(), threshold),
            persontime = sum(as.numeric(censor_date - (boost_date - 1))),
            test_count = sum(covid_test_frequency),
            test_rate = sum(covid_test_frequency) / persontime,
          ) %>%
          ungroup() %>%
          add_column(
            subgroup_level = as.character(.[[subgroup]]),
            subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
            .before=1
          ) %>%
          select(-all_of(c(subgroup)))
      }
    )
  ) %>%
  unnest(data)


write_rds(data_counts, fs::path(output_dir, "eventcounts.rds"))

