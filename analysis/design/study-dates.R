# # # # # # # # # # # # # # # # # # # # #
# Purpose: Define the study dates that are used throughout the rest of the project
# Notes:
# This script is separate from the design.R script as
#  the dates are also used by the dataset definition (as well as analysis R scripts),
#  where they cannot be "sourced" as usual
# # # # # # # # # # # # # # # # # # # # #

## create output directories ----
fs::dir_create(here::here("lib", "design"))

# define key dates ----

study_dates <- tibble::lst(
  studystart_date = "2023-04-01", # first possible study entry date (when both moderna and pfizer and sanofi being administered for boosters), and index date for dates as "time since index date" format
  studyend_date = "2023-06-30", # last study entry dates
  followupend_date = "2023-10-19", # end of follow-up (+ 16 weeks after last recruitment date)
  firstpossiblevax_date = "2020-06-01", # used to catch "real" vaccination dates (eg not 1900-01-01)
)


jsonlite::write_json(study_dates, path = here("lib", "design", "study-dates.json"), auto_unbox=TRUE, pretty =TRUE)
