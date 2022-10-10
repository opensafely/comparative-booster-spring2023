
# # # # # # # # # # # # # # # # # # # # #
# Purpose: To gather level 4 files ("moderately sensitive") place in a single directory for easy review and release
# # # # # # # # # # # # # # # # # # # # #

## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')


## pre-matching ----

fs::dir_create(here("output", "release-objects", "prematch"))

fs::file_copy(here("output", "prematch", "table1.csv"), fs::path(here("output", "release-objects", "prematch", "prematch_table1.csv")), overwrite = TRUE)
fs::file_copy(here("output", "prematch", "table1by.csv"), fs::path(here("output", "release-objects", "prematch", "prematch_table1by.csv")), overwrite = TRUE)
fs::file_copy(here("output", "prematch", "smd.csv"), fs::path(here("output", "release-objects", "prematch", "prematch_smd.csv")), overwrite = TRUE)


## post-matching ----

for(matchset in c("A", "B")){

  output_dir <- here("output", "release-objects", matchset)
  fs::dir_create(output_dir)


  ## matching ----

  fs::file_copy(here("output", "match", matchset, "report", "table1.csv"), fs::path(output_dir, "match_table1.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "report", "table1by.csv"), fs::path(output_dir, "match_table1by.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "report", "data_coverage.csv"), fs::path(output_dir, "match_coverage.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "report", "data_smd.csv"), fs::path(output_dir, "match_smd.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "report", "flowchart.csv"), fs::path(output_dir, "match_flowchart.csv"), overwrite = TRUE)

  ## Contrasts ----

  fs::file_copy(here("output", "match", matchset, "combined", "km_estimates_rounded.csv"), fs::path(output_dir, "km_estimates_rounded.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "combined", "contrasts_daily_rounded.csv"), fs::path(output_dir, "contrasts_daily_rounded.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "combined", "contrasts_cuts_rounded.csv"), fs::path(output_dir, "contrasts_cuts_rounded.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", matchset, "combined", "contrasts_overall_rounded.csv"), fs::path(output_dir, "contrasts_overall_rounded.csv"), overwrite = TRUE)

}

## create text for output review issue ----
fs::dir_ls(here("output", "release-objects"), type="file", recurse=TRUE) %>%
  map_chr(~str_remove(., fixed(here()))) %>%
  map_chr(~paste0("- [ ] ", str_remove(.,fixed("/")))) %>%
  paste(collapse="\n") %>%
  writeLines(here("output", "files-for-release.txt"))


## create command for releasing using osrelease ----
fs::dir_ls(here("output", "release-objects"), type="file", recurse=TRUE) %>%
  map_chr(~str_remove(., fixed(here()))) %>%
  #map_chr(~paste0("'",. ,"'")) %>%
  paste(., collapse=" ") %>%
  paste("osrelease", .) %>%
  writeLines(here("output", "osrelease-command.txt"))

