
# # # # # # # # # # # # # # # # # # # # #
# Purpose: report count of
#  - import matched data
#  - reports rate of tests and positive tests
#  - The script must be accompanied by 2 arguments:
#    `matchset` - the matching set used for matching
#    `subgroup` - the subgroup variable, which is used to stratify KM estimates

# # # # # # # # # # # # # # # # # # # # #


# Preliminaries ----

# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  removeobjects <- FALSE
  matchset <- "B"
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

## Import design elements
source(here("lib", "design", "design.R"))



# create output directories ----


output_dir <- here("output", "match", matchset, "eventcounts")
fs::dir_create(output_dir)



# derive subgroup info

metaparams <-
  expand_grid(
    subgroup = as.character(recoder$subgroups),
  ) %>%
  mutate(
    subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroups),
  )



data_matchstatus <- read_rds(here("output", "match", matchset, "data_matchstatus.rds"))

## import baseline data, restrict to matched individuals and derive time-to-event variables
data_matched <-
  read_rds(here("output", "data", "data_cohort.rds")) %>%
  mutate(all = "all") %>%
  select(
    # select only variables needed for models to save space
    patient_id, vax3_date,
    all_of(metaparams$subgroup),
    death_date,
    ends_with("_count"),
  ) %>%
  #filter(patient_id %in% data_matchstatus$patient_id[data_matchstatus$matched]) %>%
  left_join(
    data_matchstatus %>% filter(matched) %>% select(-vax3_date),
    .,
    by= c("patient_id")
  ) %>%
  mutate(
    censor_date = pmin(
      death_date,
      study_dates$postestfollowupend_date,
      na.rm = TRUE
    ), # only use death and end of follow-up as these are the only things that can be used in a study definition currently
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
          group_by_(c(subgroup, "treatment")) %>%
          summarise(
            n = roundmid_any(n(), threshold),
            persontime = sum(as.numeric(censor_date - (vax3_date - 1))),
            #test_count = sum(test_count),
            test_rate = sum(test_count) / persontime,
            #postest_count = sum(postest_count),
            postest_rate = sum(postest_count) / persontime,
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

