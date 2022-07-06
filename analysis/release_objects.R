
# # # # # # # # # # # # # # # # # # # # #
# Purpose: To gather level 4 files ("moderately sensitive") place in a single directory for easy review and release
# # # # # # # # # # # # # # # # # # # # #

## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')


for(matchset in c("A", "B")){

  output_dir <- here("output", "release-objects", matchset)
  fs::dir_create(output_dir)


  ## pre-matching ----

  fs::file_copy(here("output", "prematch", "table1.csv"), fs::path(output_dir, "prematch_table1.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "prematch", "table1by.csv"), fs::path(output_dir, "prematch_table1by.csv"), overwrite = TRUE)

  ## matching ----

  fs::file_copy(here("output", "match", matchset, "report", "table1.csv"), fs::path(output_dir, "match_table1.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "report", "table1by.csv"), fs::path(output_dir, "match_table1by.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "report", "data_coverage.csv"), fs::path(output_dir, "match_coverage.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "report", "data_smd.csv"), fs::path(output_dir, "match_smd.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "report", "flowchart.csv"), fs::path(output_dir, "match_flowchart.csv"), overwrite = TRUE)

  ## KM ----

  #fs::file_copy(here("output", "match", matchset, "km", "combined", "km_estimates.csv"), fs::path(output_dir, "km_estimates.csv"), overwrite = TRUE)
  #fs::file_copy(here("output", "match", matchset, "km", "combined", "contrasts_daily.csv"), fs::path(output_dir, "km_contrasts_daily.csv"), overwrite = TRUE)
  #fs::file_copy(here("output", "match", matchset, "km", "combined", "contrasts_overall.csv"), fs::path(output_dir, "km_contrasts_overall.csv"), overwrite = TRUE)

  ## CI ----

  fs::file_copy(here("output", "match", matchset, "ci", "combined", "ci_estimates.csv"), fs::path(output_dir, "ci_estimates.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "ci", "combined", "contrasts_daily.csv"), fs::path(output_dir, "ci_contrasts_daily.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "ci", "combined", "contrasts_cuts.csv"), fs::path(output_dir, "ci_contrasts_cuts.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "ci", "combined", "contrasts_overall.csv"), fs::path(output_dir, "ci_contrasts_overall.csv"), overwrite = TRUE)

}

## create text for output review issue ----
fs::dir_ls(here("output", "release-objects"), type="file", recurse=TRUE) %>%
  map_chr(~str_remove(., fixed(here()))) %>%
  map_chr(~paste0("- [ ] ", str_remove(.,fixed("/")))) %>%
  paste(collapse="\n") %>%
  writeLines(here("output", "files-for-release.txt"))
