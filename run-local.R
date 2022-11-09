library('here')

action_ls <- function(...){
  # provide files to source, separated by a comma
  # for any files with no arguments needed
  pre_ls <- ls(envir = globalenv())
  filelist <- list(...)
  lapply(filelist, source)
  post_ls <- ls(envir = globalenv())
  rm(list=post_ls[!(post_ls %in% pre_ls)], envir = globalenv())
}


compose_action <- function(...){
  # returns a function that sources one or more scripts on a set of arguments
  .files <- list(...)
  fun <- function(...){
    pre_ls <- ls(envir = globalenv())
    .args <- c(...)
    .GlobalEnv$commandArgs <- function(...) .args
    lapply(.files, source)
    post_ls <- ls(envir = globalenv())
    rm(list=post_ls[!(post_ls %in% pre_ls)], envir = globalenv())
  }

  fun
}


action_match <- compose_action(
  here("analysis", "match.R"),
  here("analysis", "match_report.R")
)

action_contrasts <- compose_action(
    here("analysis", "km.R"),
    #here("analysis", "ci.R"),
    here("analysis", "eventcounts.R")
)

action_combine <- compose_action(
  #here("analysis", "km_combine.R"),
  here("analysis", "ci_combine.R")
)


## run actions ----


action_ls(
  here("analysis", "data_process.R"),
  here("analysis", "data_selection.R")
)

action_match("A")
action_contrasts("A", "all", "postest")
action_contrasts("A", "all", "covidemergency")
action_contrasts("A", "all", "covidadmittedproxy1")
action_contrasts("A", "all", "covidadmitted")
#action_contrasts("A", "all", "noncovidadmitted")
action_contrasts("A", "all", "covidcritcare")
action_contrasts("A", "all", "coviddeath")
action_contrasts("A", "all", "noncoviddeath")


action_contrasts("A", "vax12_type", "postest")
action_contrasts("A", "vax12_type", "covidemergency")
action_contrasts("A", "vax12_type", "covidadmittedproxy1")
action_contrasts("A", "vax12_type", "covidadmitted")
#action_contrasts("A", "vax12_type", "noncovidadmitted")
action_contrasts("A", "vax12_type", "covidcritcare")
action_contrasts("A", "vax12_type", "coviddeath")
action_contrasts("A", "vax12_type", "noncoviddeath")



action_contrasts("A", "cev_cv", "postest")
action_contrasts("A", "cev_cv", "covidemergency")
action_contrasts("A", "cev_cv", "covidadmittedproxy1")
action_contrasts("A", "cev_cv", "covidadmitted")
#action_contrasts("A", "cev_cv", "noncovidadmitted")
action_contrasts("A", "cev_cv", "covidcritcare")
action_contrasts("A", "cev_cv", "coviddeath")
action_contrasts("A", "cev_cv", "noncoviddeath")


action_contrasts("A", "prior_covid_infection", "postest")
action_contrasts("A", "prior_covid_infection", "covidemergency")
action_contrasts("A", "prior_covid_infection", "covidadmittedproxy1")
action_contrasts("A", "prior_covid_infection", "covidadmitted")
#action_contrasts("A", "prior_covid_infection", "noncovidadmitted")
action_contrasts("A", "prior_covid_infection", "covidcritcare")
action_contrasts("A", "prior_covid_infection", "coviddeath")
action_contrasts("A", "prior_covid_infection", "noncoviddeath")


action_contrasts("A", "age65plus", "postest")
action_contrasts("A", "age65plus", "covidemergency")
action_contrasts("A", "age65plus", "covidadmittedproxy1")
action_contrasts("A", "age65plus", "covidadmitted")
#action_contrasts("A", "age65plus", "noncovidadmitted")
action_contrasts("A", "age65plus", "covidcritcare")
action_contrasts("A", "age65plus", "coviddeath")
action_contrasts("A", "age65plus", "noncoviddeath")

action_contrasts("A", "variantera", "postest")
action_contrasts("A", "variantera", "covidemergency")
action_contrasts("A", "variantera", "covidadmittedproxy1")
action_contrasts("A", "variantera", "covidadmitted")
#action_contrasts("A", "variantera", "noncovidadmitted")
action_contrasts("A", "variantera", "covidcritcare")
action_contrasts("A", "variantera", "coviddeath")
action_contrasts("A", "variantera", "noncoviddeath")

action_combine("A")




action_match("B")
action_contrasts("B", "all", "postest")
action_contrasts("B", "all", "covidemergency")
action_contrasts("B", "all", "covidadmittedproxy1")
action_contrasts("B", "all", "covidadmitted")
#action_contrasts("B", "all", "noncovidadmitted")
action_contrasts("B", "all", "covidcritcare")
action_contrasts("B", "all", "coviddeath")
action_contrasts("B", "all", "noncoviddeath")


action_contrasts("B", "vax12_type", "postest")
action_contrasts("B", "vax12_type", "covidemergency")
action_contrasts("B", "vax12_type", "covidadmittedproxy1")
action_contrasts("B", "vax12_type", "covidadmitted")
#action_contrasts("B", "vax12_type", "noncovidadmitted")
action_contrasts("B", "vax12_type", "covidcritcare")
action_contrasts("B", "vax12_type", "coviddeath")
action_contrasts("B", "vax12_type", "noncoviddeath")



action_contrasts("B", "cev_cv", "postest")
action_contrasts("B", "cev_cv", "covidemergency")
action_contrasts("B", "cev_cv", "covidadmittedproxy1")
action_contrasts("B", "cev_cv", "covidadmitted")
#action_contrasts("B", "cev_cv", "noncovidadmitted")
action_contrasts("B", "cev_cv", "covidcritcare")
action_contrasts("B", "cev_cv", "coviddeath")
action_contrasts("B", "cev_cv", "noncoviddeath")


action_contrasts("B", "prior_covid_infection", "postest")
action_contrasts("B", "prior_covid_infection", "covidemergency")
action_contrasts("B", "prior_covid_infection", "covidadmittedproxy1")
action_contrasts("B", "prior_covid_infection", "covidadmitted")
#action_contrasts("B", "prior_covid_infection", "noncovidadmitted")
action_contrasts("B", "prior_covid_infection", "covidcritcare")
action_contrasts("B", "prior_covid_infection", "coviddeath")
action_contrasts("B", "prior_covid_infection", "noncoviddeath")


action_contrasts("B", "age65plus", "postest")
action_contrasts("B", "age65plus", "covidemergency")
action_contrasts("B", "age65plus", "covidadmittedproxy1")
action_contrasts("B", "age65plus", "covidadmitted")
#action_contrasts("B", "age65plus", "noncovidadmitted")
action_contrasts("B", "age65plus", "covidcritcare")
action_contrasts("B", "age65plus", "coviddeath")
action_contrasts("B", "age65plus", "noncoviddeath")

action_contrasts("B", "variantera", "postest")
action_contrasts("B", "variantera", "covidemergency")
action_contrasts("B", "variantera", "covidadmittedproxy1")
action_contrasts("B", "variantera", "covidadmitted")
#action_contrasts("B", "variantera", "noncovidadmitted")
action_contrasts("B", "variantera", "covidcritcare")
action_contrasts("B", "variantera", "coviddeath")
action_contrasts("B", "variantera", "noncoviddeath")

action_combine("B")

action_ls(
  here("analysis", "release_objects.R")
)

