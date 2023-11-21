
# # # # # # # # # # # # # # # # # # # # #
# Purpose: match pfizer recipients to sanofi recipients
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')
library('MatchIt')
library("doParallel")

## import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  removeobjects <- FALSE
  matchset <- "A"
} else {
  removeobjects <- TRUE
  matchset <- args[[1]]
}


## Import custom user functions from lib
source(here("analysis", "functions", "utility.R"))

## Import design elements
source(here("analysis", "design", "design.R"))

## create output directories ----

output_dir <- here("output", "match", matchset)
fs::dir_create(output_dir)

# Import and prepare data ----

## one pow per patient ----
data_cohort <- read_rds(here("output", "data", "data_cohort.rds"))

print(
  cat(
    glue("data_cohort", " data size = ", nrow(data_cohort)),
    glue("data_cohort", " memory usage = ", format(object.size(data_cohort), units="GB", standard="SI", digits=3L)),
    sep = "\n  "
  )
)

## select all matching candidates and variables necessary for matching
data_matchingcandidates <-
  data_cohort %>%
  mutate(
    # create variable to parallelise on
    thread_variable = ageband,
    thread_id = dense_rank(thread_variable)
  ) %>%
  select(
    thread_id,
    thread_variable,
    patient_id,
    boost_type,
    treatment,
    boost_date,
    all_of(matching_variables[[matchset]]$all),
  ) %>%
  arrange(patient_id)


# Set up parallelisation and run matching ----

# create function that catches errors in case no matches are found within a thread
safely_matchit <- purrr::safely(matchit)

## parallelisation preliminaries ----

parallel::detectCores() # how many cores available?
n_threads <- 2

cluster <- parallel::makeCluster(
  n_threads,
  type = "PSOCK" # this should work across multi-core windows or linux machines
)
print(cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = cluster)

# create parallel matching streams
matchthreads <- as.character(unique(sort(data_matchingcandidates$thread_variable)))

table(data_matchingcandidates$thread_variable, useNA="ifany")

## match in parallel ----
data_matchstatus <-
  foreach(
    matchthread = matchthreads,
    .combine = 'bind_rows',
    .packages = c("dplyr", "MatchIt", "tibble", "lubridate")
  ) %dopar% {
  #for(matchthread in matchthreads){
  #matchthread<-"85+"

    data_thread <- data_matchingcandidates %>% filter(thread_variable==matchthread)

    # run matching algorithm
    obj_matchit <-
      safely_matchit(
      #matchit(
        formula = treatment ~ 1,
        data = data_thread,
        method = "nearest", distance = "glm", # these two options don't really do anything because we only want exact + caliper matching
        replace = FALSE,
        estimand = "ATT",
        exact = matching_variables[[matchset]]$exact,
        caliper = matching_variables[[matchset]]$caliper, std.caliper=FALSE,
        m.order = "data", # data is sorted on (effectively random) patient ID
        #verbose = TRUE,
        ratio = 1L # could also consider exact matching only, with n:m ratio, determined by availability
      )[[1]]

    ## process matchit object to give one row per candidate, matched status (0/1) and match id

    data_matchstatus <-
      if(is.null(obj_matchit)){
        tibble(
          patient_id = data_thread$patient_id,
          matched = FALSE,
          thread_id = data_thread$thread_id,
          threadmatch_id = NA_integer_,
          treatment = data_thread$treatment,
          weight = 0,
        )
      } else {
        tibble(
          patient_id = data_thread$patient_id,
          matched = !is.na(obj_matchit$subclass),
          thread_id = data_thread$thread_id,
          threadmatch_id = as.integer(as.character(obj_matchit$subclass)),
          treatment = obj_matchit$treat,
          weight = obj_matchit$weights,
          .before = 1
        ) %>% as_tibble()
      }

  data_matchstatus
}

parallel::stopCluster(cl = cluster)

# Summarise matched / unmatched patients and export ----

data_matchstatus <-
  data_matchstatus %>%
  left_join(data_cohort %>% select(patient_id, boost_date), by="patient_id") %>%
  arrange(thread_id, threadmatch_id) %>%
  mutate(
    match_id = dense_rank(threadmatch_id * max(thread_id) + thread_id) # create unique match id across all threads
  )

write_rds(data_matchstatus, fs::path(output_dir, "data_matchstatus.rds"), compress="gz")

data_matchstatus %>%
  group_by(treatment, matched) %>%
  summarise(
    n=n()
  )


# bootstrap sampling ----

## bootstrap sample matched pairs and use this sampling throughout the analysis
## doing it here avoids repeating the sampling process in each individual outcome script
## and provides consistency across different analyses
## but the leg work is still done by the analysis scripts

boot_n <- 500 # more than necessary, can select fewer in the analysis scripts

boot_id <- seq_len(boot_n)

match_ids <- unique(data_matchstatus$match_id[!is.na(data_matchstatus$match_id)])

set.seed(20230401)

boot_samples <-
  tibble(boot_id) %>%
  mutate(
    match_id = map(boot_id, ~sample(match_ids, size=length(match_ids), replace=TRUE))
  ) %>%
  unnest(match_id)

write_rds(boot_samples, fs::path(output_dir, "boot_samples.rds"), compress="gz")


