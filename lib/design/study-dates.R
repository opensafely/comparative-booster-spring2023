# # # # # # # # # # # # # # # # # # # # #
# Purpose: Define the study dates that are used throughout the rest of the project
# Notes:
# This script is separate from the design.R script as the dates are used by the study definition as well as analysis R scripts.
# # # # # # # # # # # # # # # # # # # # #

## create output directories ----
fs::dir_create(here::here("lib", "design"))

# define key dates ----

study_dates <- tibble::lst(
  studystart_date = "2021-10-29", # first possible study entry date (when both moderna and pfizer being adnimistered for boosters), and index date for dates as "time since index date" format
  studyend_date = "2022-01-31", # last study entry dates
  followupend_date = "2021-03-28", # end of follow-up

  firstpfizer_date = "2020-12-08", # first pfizer vaccination in national roll-out
  firstaz_date = "2021-01-04", # first az vaccination in national roll-out
  firstmoderna_date = "2021-04-13", # first moderna vaccination in national roll-out

  firstpossiblevax_date = "2020-06-01", # used to catch "real" vaccination dates (eg not 1900-01-01)
)


jsonlite::write_json(study_dates, path = here("lib", "design", "study-dates.json"), auto_unbox=TRUE, pretty =TRUE)
