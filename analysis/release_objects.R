
# # # # # # # # # # # # # # # # # # # # #
# Purpose: To gather level 4 files ("moderately sensitive") place in a single directory for easy review and release
# # # # # # # # # # # # # # # # # # # # #

## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')


for(matchset in c("A")){

  output_dir <- here("output", "release-objects", matchset)
  fs::dir_create(output_dir)


  ## matching ----

  fs::file_copy(here("output", "match", "A", "report", "table1.csv"), here("output", "release-objects", matchset, "match_table1.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", "A", "report", "table1by.csv"), here("output", "release-objects", matchset, "match_table1by.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", "A", "report", "data_coverage.csv"), here("output", "release-objects", matchset, "match_coverage.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", "A", "report", "data_smd.csv"), here("output", "release-objects", matchset, "match_smd.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", "A", "report", "flowchart.csv"), here("output", "release-objects", matchset, "match_flowchart.csv"), overwrite = TRUE)

    ## KM ----

  fs::file_copy(here("output", "match", "A", "km", "combined", "km_estimates.csv"), here("output", "release-objects", matchset, "km_estimates.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", "A", "km", "combined", "km_contrasts_daily.csv"), here("output", "release-objects", matchset, "km_contrasts_daily.csv"), overwrite = TRUE)
  fs::file_copy(here("output", "match", "A", "km", "combined", "km_contrasts_overall.csv"), here("output", "release-objects", matchset, "km_contrasts_overall.csv"), overwrite = TRUE)

}

## create text for output review issue ----
fs::dir_ls(here("output", "release-objects"), type="file", recurse=TRUE) %>%
  map_chr(~str_remove(., fixed(here()))) %>%
  map_chr(~paste0("- [ ] ", str_remove(.,fixed("/")))) %>%
  paste(collapse="\n") %>%
  writeLines(here("output", "files-for-release.txt"))
