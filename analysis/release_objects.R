
# # # # # # # # # # # # # # # # # # # # #
# Purpose: To gather level 4 files ("moderately sensitive") place in a single directory for easy review and release
# # # # # # # # # # # # # # # # # # # # #

## Import libraries ----
library('tidyverse')
library('here')
library('glue')


## pre-matching ----


## post-matching ----

for(cohort in c("age75plus", "cv")){
  fs::dir_create(here("output", "release-objects", cohort, "prematch"))

  fs::file_copy(here("output", cohort, "table1.csv"), fs::path(here("output", "release-objects", cohort, "prematch_table1.csv")), overwrite = TRUE)


  for(matchset in c("A", "B")){

    output_dir <- here("output", "release-objects", cohort, matchset)
    fs::dir_create(output_dir)

    ## matching ----

    fs::file_copy(here("output", cohort, matchset, "report", "table1.csv"), fs::path(output_dir, "match_table1.csv"), overwrite = TRUE)
    fs::file_copy(here("output", cohort, matchset, "report", "data_coverage.csv"), fs::path(output_dir, "match_coverage.csv"), overwrite = TRUE)
    fs::file_copy(here("output", cohort, matchset, "report", "flowchart.csv"), fs::path(output_dir, "match_flowchart.csv"), overwrite = TRUE)

    ## Contrasts ----

    fs::file_copy(here("output", cohort, matchset, "combined", "km_estimates_rounded.csv"), fs::path(output_dir, "km_estimates_rounded.csv"), overwrite = TRUE)
    fs::file_copy(here("output", cohort, matchset, "combined", "contrasts_daily_rounded.csv"), fs::path(output_dir, "contrasts_daily_rounded.csv"), overwrite = TRUE)
    fs::file_copy(here("output", cohort, matchset, "combined", "contrasts_cuts_rounded.csv"), fs::path(output_dir, "contrasts_cuts_rounded.csv"), overwrite = TRUE)
    fs::file_copy(here("output", cohort, matchset, "combined", "contrasts_overall_rounded.csv"), fs::path(output_dir, "contrasts_overall_rounded.csv"), overwrite = TRUE)
    fs::file_copy(here("output", cohort, matchset, "combined", "eventcounts.csv"), fs::path(output_dir, "eventcounts.csv"), overwrite = TRUE)
    fs::file_copy(here("output", cohort, matchset, "combined", "followup_rounded.csv"), fs::path(output_dir, "followup_rounded.csv"), overwrite = TRUE)
    fs::file_copy(here("output", cohort, matchset, "combined", "followup_treatment_rounded.csv"), fs::path(output_dir, "followup_treatment_rounded.csv"), overwrite = TRUE)

  }
}

## create text for output review issue ----
fs::dir_ls(here("output", "release-objects"), type="file", recurse=TRUE) %>%
  map_chr(~str_remove(., fixed(here()))) %>%
  map_chr(~paste0("- [ ] ", str_remove(.,fixed("/")))) %>%
  paste(collapse="\n") %>%
  writeLines(here("output", "files-for-release.txt"))


## create command for releasing using osrelease ----
fs::dir_ls(here("output", "release-objects"), type="file", recurse=TRUE) %>%
  map_chr(~str_remove(., fixed(paste0(here(), "/")))) %>%
  #map_chr(~paste0("'",. ,"'")) %>%
  paste(., collapse=" ") %>%
  paste("osrelease", .) %>%
  writeLines(here("output", "osrelease-command.txt"))

