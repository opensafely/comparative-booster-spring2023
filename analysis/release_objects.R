
# # # # # # # # # # # # # # # # # # # # #
# Purpose: To gather level 4 files ("moderately sensitive") place in a single directory for easy review and release
# # # # # # # # # # # # # # # # # # # # #

## Import libraries ----
library('tidyverse')
library('here')
library('fs')
library('glue')

dir_create(here("output", "release-objects"))

## total vaccinated in time period, by vaccine type
# choosing cv cohort arbitrarily, could use any cohort as output is the same
file_copy(
  here("output", "combine", "cv", "descriptives", "flowchart_totals_allcohorts_rounded.csv"),
  here("output", "release-objects", "flowchart_totals_allcohorts_rounded.csv"),
  overwrite = TRUE
)

source(here("analysis", "design", "design.R"))

for(cohort in recoder$cohort){

  output_dir <- here("output", "release-objects", cohort)
  dir_create(output_dir)
  # pre-contrasts ----
  # non-outcome specific files

  file_copy(here("output", "combine", cohort, "descriptives", "table1_rounded.csv"), path(output_dir, glue("table1_rounded.csv")), overwrite = TRUE)
  file_copy(here("output", "combine", cohort, "descriptives", "flowchart_totals_rounded.csv"), path(output_dir, glue("flowchart_totals_rounded.csv")), overwrite = TRUE)
  file_copy(here("output", "combine", cohort, "descriptives", "flowchart_rounded.csv"), path(output_dir, glue("flowchart_rounded.csv")), overwrite = TRUE)
  file_copy(here("output", "combine", cohort, "descriptives", "coverage_rounded.csv"), path(output_dir, glue("coverage_rounded.csv")), overwrite = TRUE)
  file_copy(here("output", "combine", cohort, "descriptives", "eventcounts.csv"), path(output_dir, "eventcounts_rounded.csv"), overwrite = TRUE)

  ## Contrasts ----
  # outcome specific files

  file_copy(here("output", "combine", cohort, "contrasts", "km_estimates_rounded.csv"), path(output_dir, "km_estimates_rounded.csv"), overwrite = TRUE)
  file_copy(here("output", "combine", cohort, "contrasts", "contrasts_daily_rounded.csv"), path(output_dir, "contrasts_daily_rounded.csv"), overwrite = TRUE)
  file_copy(here("output", "combine", cohort, "contrasts", "contrasts_cuts_rounded.csv"), path(output_dir, "contrasts_cuts_rounded.csv"), overwrite = TRUE)
  file_copy(here("output", "combine", cohort, "contrasts", "contrasts_overall_rounded.csv"), path(output_dir, "contrasts_overall_rounded.csv"), overwrite = TRUE)
  file_copy(here("output", "combine", cohort, "contrasts", "followup_rounded.csv"), path(output_dir, "followup_rounded.csv"), overwrite = TRUE)
  file_copy(here("output", "combine", cohort, "contrasts", "followup_treatment_rounded.csv"), path(output_dir, "followup_treatment_rounded.csv"), overwrite = TRUE)

}

## create text for output review issue ----
dir_ls(here("output", "release-objects"), type="file", recurse=TRUE) %>%
  map_chr(~str_remove(., fixed(here()))) %>%
  map_chr(~paste0("- [ ] ", str_remove(.,fixed("/")))) %>%
  paste(collapse="\n") %>%
  writeLines(here("output", "files-for-release.txt"))


## create command for releasing using osrelease ----
dir_ls(here("output", "release-objects"), type="file", recurse=TRUE) %>%
  map_chr(~str_remove(., fixed(paste0(here(), "/")))) %>%
  #map_chr(~paste0("'",. ,"'")) %>%
  paste(., collapse=" ") %>%
  paste("osrelease", .) %>%
  writeLines(here("output", "osrelease-command.txt"))


