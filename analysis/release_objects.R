
# # # # # # # # # # # # # # # # # # # # #
# Purpose: To gather level 4 files ("moderately sensitive") place in a single directory for easy review and release
# # # # # # # # # # # # # # # # # # # # #

## Import libraries ----
library('tidyverse')
library('here')
library('fs')
library('glue')


## pre-matching ----


## post-matching ----

for(cohort in c("age75plus", "cv")){
  dir_create(here("output", "release-objects", cohort, "prematch"))

  file_copy(here("output", cohort, "table1.csv"), here("output", "release-objects", cohort, "prematch_table1.csv"), overwrite = TRUE)
  file_copy(here("output", cohort, "total_rounded.csv"), here("output", "release-objects", cohort, "total_flowchart.csv"), overwrite = TRUE)

  for(matchset in c("A", "B")){

    output_dir <- here("output", "release-objects", cohort, matchset)
    dir_create(output_dir)

    ## matching ----

    file_copy(here("output", cohort, matchset, "report", "table1.csv"), path(output_dir, "match_table1.csv"), overwrite = TRUE)
    file_copy(here("output", cohort, matchset, "report", "data_coverage.csv"), path(output_dir, "match_coverage.csv"), overwrite = TRUE)
    file_copy(here("output", cohort, matchset, "report", "flowchart_rounded.csv"), path(output_dir, "match_flowchart.csv"), overwrite = TRUE)

    ## Contrasts ----

    file_copy(here("output", cohort, matchset, "combined", "km_estimates_rounded.csv"), path(output_dir, "km_estimates_rounded.csv"), overwrite = TRUE)
    file_copy(here("output", cohort, matchset, "combined", "contrasts_daily_rounded.csv"), path(output_dir, "contrasts_daily_rounded.csv"), overwrite = TRUE)
    file_copy(here("output", cohort, matchset, "combined", "contrasts_cuts_rounded.csv"), path(output_dir, "contrasts_cuts_rounded.csv"), overwrite = TRUE)
    file_copy(here("output", cohort, matchset, "combined", "contrasts_overall_rounded.csv"), path(output_dir, "contrasts_overall_rounded.csv"), overwrite = TRUE)
    file_copy(here("output", cohort, matchset, "combined", "eventcounts.csv"), path(output_dir, "eventcounts.csv"), overwrite = TRUE)
    file_copy(here("output", cohort, matchset, "combined", "followup_rounded.csv"), path(output_dir, "followup_rounded.csv"), overwrite = TRUE)
    file_copy(here("output", cohort, matchset, "combined", "followup_treatment_rounded.csv"), path(output_dir, "followup_treatment_rounded.csv"), overwrite = TRUE)

  }
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


