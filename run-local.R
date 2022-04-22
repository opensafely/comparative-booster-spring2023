library('here')

action_km <- function(matchset, subgroup, outcome){

    commandArgs <- function(...) c(matchset, subgroup, outcome)
    source(here("analysis", "km.R"))
}

  action_km("A", "all", "postest")
  action_km("A", "all", "covidemergency")
  action_km("A", "all", "covidadmittedproxy1")
  action_km("A", "all", "covidadmitted")
  #action_km("A", "all", "noncovidadmitted")
  #action_km("A", "all", "covidcc")
  action_km("A", "all", "coviddeath")
  action_km("A", "all", "noncoviddeath")


  action_km("A", "vax12_type", "postest")
  action_km("A", "vax12_type", "covidemergency")
  action_km("A", "vax12_type", "covidadmittedproxy1")
  action_km("A", "vax12_type", "covidadmitted")
  #action_km("A", "vax12_type", "noncovidadmitted")
  #action_km("A", "vax12_type", "covidcc")
  action_km("A", "vax12_type", "coviddeath")
  action_km("A", "vax12_type", "noncoviddeath")



  action_km("A", "cev_cv", "postest")
  action_km("A", "cev_cv", "covidemergency")
  action_km("A", "cev_cv", "covidadmittedproxy1")
  action_km("A", "cev_cv", "covidadmitted")
  #action_km("A", "cev_cv", "noncovidadmitted")
  #action_km("A", "cev_cv", "covidcc")
  action_km("A", "cev_cv", "coviddeath")
  action_km("A", "cev_cv", "noncoviddeath")


  action_km("A", "prior_covid_infection", "postest")
  action_km("A", "prior_covid_infection", "covidemergency")
  action_km("A", "prior_covid_infection", "covidadmittedproxy1")
  action_km("A", "prior_covid_infection", "covidadmitted")
  #action_km("A", "prior_covid_infection", "noncovidadmitted")
  #action_km("A", "prior_covid_infection", "covidcc")
  action_km("A", "prior_covid_infection", "coviddeath")
  action_km("A", "prior_covid_infection", "noncoviddeath")


  action_km("A", "age65plus", "postest")
  action_km("A", "age65plus", "covidemergency")
  action_km("A", "age65plus", "covidadmittedproxy1")
  action_km("A", "age65plus", "covidadmitted")
  #action_km("A", "age65plus", "noncovidadmitted")
  #action_km("A", "age65plus", "covidcc")
  action_km("A", "age65plus", "coviddeath")
  action_km("A", "age65plus", "noncoviddeath")



