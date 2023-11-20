# # # # # # # # # # # # # # # # # # # # #
# Purpose: Define the study dates that are used throughout the rest of the project
# Notes:
# This script is separate from the design.R script as the dates are used by the study definition as well as analysis R scripts.
# # # # # # # # # # # # # # # # # # # # #

## create output directories ----
fs::dir_create(here::here("lib", "design"))

# define key dates ----

study_dates <- tibble::lst(
  studystart_date = "2023-04-01", # first possible study entry date (when both moderna and pfizer and sanofi being administered for boosters), and index date for dates as "time since index date" format
  studyend_date = "2023-06-30", # last study entry dates
  followupend_date = "2023-08-31", # end of follow-up
  firstpossiblevax_date = "2020-06-01", # used to catch "real" vaccination dates (eg not 1900-01-01)
)


jsonlite::write_json(study_dates, path = here("lib", "design", "study-dates.json"), auto_unbox=TRUE, pretty =TRUE)
