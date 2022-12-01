library('tidyverse')
library('yaml')
library('here')
library('glue')

# create action functions ----

## create comment function ----
comment <- function(...){
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}


## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt){
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}


## generic action function ----
action <- function(
  name,
  run,
  arguments=NULL,
  needs=NULL,
  highly_sensitive=NULL,
  moderately_sensitive=NULL,
  ... # other arguments / options for special action types
){

  outputs <- list(
    highly_sensitive = highly_sensitive,
    moderately_sensitive = moderately_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL

  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    needs = needs,
    outputs = outputs,
    ... = ...
  )
  action[sapply(action, is.null)] <- NULL

  action_list <- list(name = action)
  names(action_list) <- name

  action_list
}


## match action function ----

action_match <- function(matchset){

  splice(

    action(
      name = glue("match_{matchset}"),
      run = "r:latest analysis/match.R",
      arguments = c(matchset),
      needs = list("data_selection"),
      highly_sensitive = lst(
        rds = glue("output/match/{matchset}/*.rds")
      )
    ),

    action(
      name = glue("match_report_{matchset}"),
      run = "r:latest analysis/match_report.R",
      arguments = c(matchset),
      needs = list("data_selection",  glue("match_{matchset}")),
      moderately_sensitive = lst(
        #txt = glue("output/match/{matchset}/report/*.txt"),
        csv = glue("output/match/{matchset}/report/*.csv"),
        png = glue("output/match/{matchset}/report/*.png")
      )
    )
  )


}

## get km or ci actions function ----
action_contrasts <- function(
  matchset, subgroup, outcome
){

  splice(

    ## kaplan-meier action
    action(
      name = glue("km_{matchset}_{subgroup}_{outcome}"),
      run = glue("r:latest analysis/km.R"),
      arguments = c(matchset, subgroup, outcome),
      needs = list(
        glue("match_{matchset}"),
        "data_selection"
      ),
      moderately_sensitive = lst(
        #txt = glue("output/match/{matchset}/km/{subgroup}/{outcome}/*.txt"),
        rds = glue("output/match/{matchset}/km/{subgroup}/{outcome}/*.rds"),
        png = glue("output/match/{matchset}/km/{subgroup}/{outcome}/*.png"),
      )
    )

    ## competing risks action, including kaplan-meier estimates
    # action(
    #   name = glue("ci_{matchset}_{subgroup}_{outcome}"),
    #   run = glue("r:latest analysis/ci.R"),
    #   arguments = c(matchset, subgroup, outcome),
    #   needs = list(
    #     glue("match_{matchset}"),
    #     "data_selection"
    #   ),
    #   moderately_sensitive = lst(
    #     txt = glue("output/match/{matchset}/ci/{subgroup}/{outcome}/*.txt"),
    #     csv = glue("output/match/{matchset}/ci/{subgroup}/{outcome}/*.csv"),
    #     png = glue("output/match/{matchset}/ci/{subgroup}/{outcome}/*.png"),
    #   )
    # )
  )
}



## get delayedentry km actions function ----
action_delayedentry_contrasts <- function(
    matchset, subgroup, outcome
){

  splice(

    ## kaplan-meier action
    action(
      name = glue("delayedentry_{matchset}_{subgroup}_{outcome}"),
      run = glue("r:latest analysis/delayedentry.R"),
      arguments = c(matchset, subgroup, outcome),
      needs = list(
        glue("match_{matchset}"),
        "data_selection"
      ),
      moderately_sensitive = lst(
        #txt = glue("output/match/{matchset}/delayedentry/{subgroup}/{outcome}/*.txt"),
        rds = glue("output/match/{matchset}/delayedentry/{subgroup}/{outcome}/*.rds"),
        png = glue("output/match/{matchset}/delayedentry/{subgroup}/{outcome}/*.png"),
      )
    )
  )
}


action_eventcounts <- function(matchset) {
  action(
    name = glue("eventcounts_{matchset}"),
    run = glue("r:latest analysis/eventcounts.R"),
    arguments = c(matchset),
    needs = list(
      glue("match_{matchset}"),
      "data_selection"
    ),
    moderately_sensitive = lst(
      rds = glue("output/match/{matchset}/eventcounts/*.rds"),
    )
  )
}


## model action function ----
action_contrasts_combine <- function(
    matchset, subgroups, outcomes
){

  splice(
    # action(
    #   name = glue("km_combine_{matchset}"),
    #   run = glue("r:latest analysis/km_combine.R"),
    #   arguments = c(matchset),
    #   needs = splice(
    #     as.list(
    #       glue_data(
    #         .x=expand_grid(
    #           subgroup=subgroups,
    #           outcome=outcomes
    #         ),
    #         "km_{matchset}_{subgroup}_{outcome}"
    #       )
    #     )
    #   ),
    #   moderately_sensitive = lst(
    #     csv = glue("output/match/{matchset}/km/combined/*.csv"),
    #     png = glue("output/match/{matchset}/km/combined/plots/*.png"),
    #   )
    # ),

    action(
      name = glue("contrasts_combine_{matchset}"),
      run = glue("r:latest analysis/contrasts_combine.R"),
      arguments = c(matchset),
      needs = splice(
        glue_data(
          .x=expand_grid(
            subgroup=subgroups,
            outcome=outcomes
          ),
          "km_{matchset}_{subgroup}_{outcome}"
        ) %>% as.list,
        glue_data(
          .x=expand_grid(
            subgroup="all",
            outcome=outcomes
          ),
          "delayedentry_{matchset}_{subgroup}_{outcome}"
        ) %>% as.list,
        list(glue("eventcounts_{matchset}"))
      ),
      moderately_sensitive = lst(
        csv = glue("output/match/{matchset}/combined/*.csv"),
        png = glue("output/match/{matchset}/combined/plots/*.png"),
      )
    )

  )
}



## model action function ----
# action_model <- function(
#     matchset, outcome, subgroup
# ){
#
#   splice(
#
#     action(
#       name = glue("km_{matchset}_{subgroup}_{outcome}"),
#       run = glue("r:latest analysis/model.R"),
#       arguments = c(matchset, subgroup, outcome),
#       needs = list(
#         glue("match_{matchset}"),
#         "data_selection",
#         "data_process"
#       ),
#       highly_sensitive = lst(
#         rds = glue("output/match/{matchset}/model/{subgroup}/{outcome}/*.rds")
#       ),
#       moderately_sensitive = lst(
#         txt = glue("output/match/{matchset}/model/{subgroup}/{outcome}/*.txt"),
#         csv = glue("output/match/{matchset}/model/{subgroup}/{outcome}/*.csv"),
#         png = glue("output/match/{matchset}/model/{subgroup}/{outcome}/*.png"),
#       )
#     ),
#
#     action(
#       name = glue("model_report_{matchset}_{subgroup}_{outcome}"),
#       run = glue("r:latest analysis/model_report.R"),
#       arguments = c(treatment, outcome, subgroup),
#       needs = list(
#         "data_selection",
#         glue("model_{matchset}_{subgroup}_{outcome}"),
#         glue("match_{matchset}")
#
#       ),
#       moderately_sensitive = lst(
#         csv = glue("output/match/{matchset}/models/{subgroup}/{outcome}/report/*.csv"),
#         #svg = glue("output/match/{matchset}/models/{subgroup}/{outcome}/report/*.svg"),
#         png = glue("output/match/{matchset}/models/{subgroup}/{outcome}/report/*.png")
#       )
#     )
#   )
# }


# specify project ----

## defaults ----
defaults_list <- lst(
  version = "3.0",
  expectations= lst(population_size=1000L)
)

## actions ----
actions_list <- splice(

  comment("# # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create-project.R",
          "Edit and run create-project.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # #"
          ),


  comment("# # # # # # # # # # # # # # # # # # #", "Pre-server scripts", "# # # # # # # # # # # # # # # # # # #"),

  # do not incorporate into project for now -- just run locally

  # action(
  #   name = "checkyaml",
  #   run = "r:latest create-project.R",
  #   moderately_sensitive = lst(
  #     project = "project.yaml"
  #   )
  # ),

  # action(
  #   name = "dummydata",
  #   run = "r:latest analysis/dummydata.R",
  #   moderately_sensitive = lst(
  #     metadata = "output/design/*"
  #   )
  # ),


  comment("# # # # # # # # # # # # # # # # # # #", "Extract and tidy", "# # # # # # # # # # # # # # # # # # #"),

  action(
    name = "extract",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition --output-format feather",
    needs = list(),
    highly_sensitive = lst(
      cohort = "output/input.feather"
    )
  ),

  # action(
  #   name = "extract_report",
  #   run = "cohort-report:v3.0.0 output/input.feather",
  #   needs = list("extract"),
  #   config = list(output_path = "output/data/reports/extract/"),
  #   moderately_sensitive = lst(
  #     html = "output/data/reports/extract/*.html",
  #     png = "output/data/reports/extract/*.png",
  #   )
  # ),


  action(
    name = "data_process",
    run = "r:latest analysis/data_process.R",
    needs = list("extract"),
    highly_sensitive = lst(
      rds = "output/data/data_processed.rds",
      vaxlong = "output/data/data_vaxlong.rds"
    )
  ),

  action(
    name = "skim_process",
    run = "r:latest analysis/data_skim.R",
    arguments = c("output/data/data_processed.rds", "output/data_properties"),
    needs = list("data_process"),
    moderately_sensitive = lst(
      cohort = "output/data_properties/data_processed*.txt"
    )
  ),

  action(
    name = "data_selection",
    run = "r:latest analysis/data_selection.R",
    needs = list("data_process"),
    highly_sensitive = lst(
      feather = "output/data/*.feather",
      rds = "output/data/*.rds",
    ),
    moderately_sensitive = lst(
      csv = "output/prematch/*.csv"
    )
  ),

  action(
    name = "skim_selection",
    run = "r:latest analysis/data_skim.R",
    arguments = c("output/data/data_cohort.rds", "output/data_properties"),
    needs = list("data_selection"),
    moderately_sensitive = lst(
      cohort = "output/data_properties/data_cohort*.txt"
    )
  ),


  action(
    name = "geography",
    run = "r:latest analysis/geography.R",
    needs = list("data_process"),
    moderately_sensitive = lst(
      png = "output/geography/*.png",
      csv = "output/geography/*.csv",
    )
  ),

  # action(
  #   name = "cohort_report",
  #   run = "cohort-report:v3.0.0 output/data/data_cohort.feather",
  #   needs = list("data_selection"),
  #   config = list(output_path = "output/data/reports/cohort/"),
  #   moderately_sensitive = lst(
  #     html = "output/data/reports/cohort/*.html",
  #     png = "output/data/reports/cohort/*.png",
  #   )
  # ),



  comment("# # # # # # # # # # # # # # # # # # #", "matching set A", "# # # # # # # # # # # # # # # # # # #"),

  action_match("A"),

  comment("### Overall models ('all')"),

  action_contrasts("A", "all", "postest"),
  action_contrasts("A", "all", "covidemergency"),
  #action_contrasts("A", "all", "covidadmittedproxy1"),
  action_contrasts("A", "all", "covidadmitted"),
  #action_contrasts("A", "all", "noncovidadmitted"),
  action_contrasts("A", "all", "covidcritcare"),
  action_contrasts("A", "all", "coviddeath"),
  action_contrasts("A", "all", "noncoviddeath"),
  action_contrasts("A", "all", "fracture"),

  action_delayedentry_contrasts("A", "all", "postest"),
  action_delayedentry_contrasts("A", "all", "covidemergency"),
  #action_delayedentry_contrasts("A", "all", "covidadmittedproxy1"),
  action_delayedentry_contrasts("A", "all", "covidadmitted"),
  #action_delayedentry_contrasts("A", "all", "noncovidadmitted"),
  action_delayedentry_contrasts("A", "all", "covidcritcare"),
  action_delayedentry_contrasts("A", "all", "coviddeath"),
  action_delayedentry_contrasts("A", "all", "noncoviddeath"),
  action_delayedentry_contrasts("A", "all", "fracture"),


  comment("### Models by primary course ('vax12_type')"),

  action_contrasts("A", "vax12_type", "postest"),
  action_contrasts("A", "vax12_type", "covidemergency"),
  #action_contrasts("A", "vax12_type", "covidadmittedproxy1"),
  action_contrasts("A", "vax12_type", "covidadmitted"),
  #action_contrasts("A", "vax12_type", "noncovidadmitted"),
  action_contrasts("A", "vax12_type", "covidcritcare"),
  action_contrasts("A", "vax12_type", "coviddeath"),
  action_contrasts("A", "vax12_type", "noncoviddeath"),
  action_contrasts("A", "vax12_type", "fracture"),


  comment("### Models by clinically vulnerable group ('cev_cv' or 'cv')"),

  action_contrasts("A", "cev_cv", "postest"),
  action_contrasts("A", "cev_cv", "covidemergency"),
  #action_contrasts("A", "cev_cv", "covidadmittedproxy1"),
  action_contrasts("A", "cev_cv", "covidadmitted"),
  #action_contrasts("A", "cev_cv", "noncovidadmitted"),
  action_contrasts("A", "cev_cv", "covidcritcare"),
  action_contrasts("A", "cev_cv", "coviddeath"),
  action_contrasts("A", "cev_cv", "noncoviddeath"),
  action_contrasts("A", "cev_cv", "fracture"),

  action_contrasts("A", "cv", "postest"),
  action_contrasts("A", "cv", "covidemergency"),
  #action_contrasts("A", "cv", "covidadmittedproxy1"),
  action_contrasts("A", "cv", "covidadmitted"),
  #action_contrasts("A", "cv", "noncovidadmitted"),
  action_contrasts("A", "cv", "covidcritcare"),
  action_contrasts("A", "cv", "coviddeath"),
  action_contrasts("A", "cv", "noncoviddeath"),
  action_contrasts("A", "cv", "fracture"),


  comment("### Models by prior infection ('prior_covid_infection')"),

  action_contrasts("A", "prior_covid_infection", "postest"),
  action_contrasts("A", "prior_covid_infection", "covidemergency"),
  #action_contrasts("A", "prior_covid_infection", "covidadmittedproxy1"),
  action_contrasts("A", "prior_covid_infection", "covidadmitted"),
  #action_contrasts("A", "prior_covid_infection", "noncovidadmitted"),
  action_contrasts("A", "prior_covid_infection", "covidcritcare"),
  action_contrasts("A", "prior_covid_infection", "coviddeath"),
  action_contrasts("A", "prior_covid_infection", "noncoviddeath"),
  action_contrasts("A", "prior_covid_infection", "fracture"),


  comment("### Models by age ('age65plus')"),

  action_contrasts("A", "age65plus", "postest"),
  action_contrasts("A", "age65plus", "covidemergency"),
  #action_contrasts("A", "age65plus", "covidadmittedproxy1"),
  action_contrasts("A", "age65plus", "covidadmitted"),
  #action_contrasts("A", "age65plus", "noncovidadmitted"),
  action_contrasts("A", "age65plus", "covidcritcare"),
  action_contrasts("A", "age65plus", "coviddeath"),
  action_contrasts("A", "age65plus", "noncoviddeath"),
  action_contrasts("A", "age65plus", "fracture"),

  # comment("### Models by JCVI age-band ('jcvi_ageband')"),
  #
  # action_contrasts("A", "jcvi_ageband", "postest"),
  # action_contrasts("A", "jcvi_ageband", "covidemergency"),
  # #action_contrasts("A", "jcvi_ageband", "covidadmittedproxy1"),
  # action_contrasts("A", "jcvi_ageband", "covidadmitted"),
  # #action_contrasts("A", "jcvi_ageband", "noncovidadmitted"),
  # action_contrasts("A", "jcvi_ageband", "covidcritcare"),
  # action_contrasts("A", "jcvi_ageband", "coviddeath"),
  # action_contrasts("A", "jcvi_ageband", "noncoviddeath"),
  # action_contrasts("A", "jcvi_ageband", "fracture"),


  comment("### Models by age-band ('ageband')"),

  action_contrasts("A", "ageband", "postest"),
  action_contrasts("A", "ageband", "covidemergency"),
  #action_contrasts("A", "ageband", "covidadmittedproxy1"),
  action_contrasts("A", "ageband", "covidadmitted"),
  #action_contrasts("A", "ageband", "noncovidadmitted"),
  action_contrasts("A", "ageband", "covidcritcare"),
  action_contrasts("A", "ageband", "coviddeath"),
  action_contrasts("A", "ageband", "noncoviddeath"),
  action_contrasts("A", "ageband", "fracture"),

  comment("### Models by variant era ('variantera')"),

  action_contrasts("A", "variantera", "postest"),
  action_contrasts("A", "variantera", "covidemergency"),
  #action_contrasts("A", "variantera", "covidadmittedproxy1"),
  action_contrasts("A", "variantera", "covidadmitted"),
  #action_contrasts("A", "variantera", "noncovidadmitted"),
  action_contrasts("A", "variantera", "covidcritcare"),
  action_contrasts("A", "variantera", "coviddeath"),
  action_contrasts("A", "variantera", "noncoviddeath"),
  action_contrasts("A", "variantera", "fracture"),


  comment("# # # # # # # # # # # # # # # # # # #", "matching set B", "# # # # # # # # # # # # # # # # # # #"),

  action_match("B"),

  comment("### Overall models ('all')"),

  action_contrasts("B", "all", "postest"),
  action_contrasts("B", "all", "covidemergency"),
  #action_contrasts("B", "all", "covidadmittedproxy1"),
  action_contrasts("B", "all", "covidadmitted"),
  #action_contrasts("B", "all", "noncovidadmitted"),
  action_contrasts("B", "all", "covidcritcare"),
  action_contrasts("B", "all", "coviddeath"),
  action_contrasts("B", "all", "noncoviddeath"),
  action_contrasts("B", "all", "fracture"),

  action_delayedentry_contrasts("B", "all", "postest"),
  action_delayedentry_contrasts("B", "all", "covidemergency"),
  #action_delayedentry_contrasts("B", "all", "covidadmittedproxy1"),
  action_delayedentry_contrasts("B", "all", "covidadmitted"),
  #action_delayedentry_contrasts("B", "all", "noncovidadmitted"),
  action_delayedentry_contrasts("B", "all", "covidcritcare"),
  action_delayedentry_contrasts("B", "all", "coviddeath"),
  action_delayedentry_contrasts("B", "all", "noncoviddeath"),
  action_delayedentry_contrasts("B", "all", "fracture"),


  comment("### Models by primary course ('vax12_type')"),

  action_contrasts("B", "vax12_type", "postest"),
  action_contrasts("B", "vax12_type", "covidemergency"),
  #action_contrasts("B", "vax12_type", "covidadmittedproxy1"),
  action_contrasts("B", "vax12_type", "covidadmitted"),
  #action_contrasts("B", "vax12_type", "noncovidadmitted"),
  action_contrasts("B", "vax12_type", "covidcritcare"),
  action_contrasts("B", "vax12_type", "coviddeath"),
  action_contrasts("B", "vax12_type", "noncoviddeath"),
  action_contrasts("B", "vax12_type", "fracture"),


  comment("### Models by clinically vulnerable group ('cev_cv' or 'cv')"),

  action_contrasts("B", "cev_cv", "postest"),
  action_contrasts("B", "cev_cv", "covidemergency"),
  #action_contrasts("B", "cev_cv", "covidadmittedproxy1"),
  action_contrasts("B", "cev_cv", "covidadmitted"),
  #action_contrasts("B", "cev_cv", "noncovidadmitted"),
  action_contrasts("B", "cev_cv", "covidcritcare"),
  action_contrasts("B", "cev_cv", "coviddeath"),
  action_contrasts("B", "cev_cv", "noncoviddeath"),
  action_contrasts("B", "cev_cv", "fracture"),

  action_contrasts("B", "cv", "postest"),
  action_contrasts("B", "cv", "covidemergency"),
  #action_contrasts("B", "cv", "covidadmittedproxy1"),
  action_contrasts("B", "cv", "covidadmitted"),
  #action_contrasts("B", "cv", "noncovidadmitted"),
  action_contrasts("B", "cv", "covidcritcare"),
  action_contrasts("B", "cv", "coviddeath"),
  action_contrasts("B", "cv", "noncoviddeath"),
  action_contrasts("B", "cv", "fracture"),


  comment("### Models by prior infection ('prior_covid_infection')"),

  action_contrasts("B", "prior_covid_infection", "postest"),
  action_contrasts("B", "prior_covid_infection", "covidemergency"),
  #action_contrasts("B", "prior_covid_infection", "covidadmittedproxy1"),
  action_contrasts("B", "prior_covid_infection", "covidadmitted"),
  #action_contrasts("B", "prior_covid_infection", "noncovidadmitted"),
  action_contrasts("B", "prior_covid_infection", "covidcritcare"),
  action_contrasts("B", "prior_covid_infection", "coviddeath"),
  action_contrasts("B", "prior_covid_infection", "noncoviddeath"),
  action_contrasts("B", "prior_covid_infection", "fracture"),


  comment("### Models by age ('age65plus')"),

  action_contrasts("B", "age65plus", "postest"),
  action_contrasts("B", "age65plus", "covidemergency"),
  #action_contrasts("B", "age65plus", "covidadmittedproxy1"),
  action_contrasts("B", "age65plus", "covidadmitted"),
  #action_contrasts("B", "age65plus", "noncovidadmitted"),
  action_contrasts("B", "age65plus", "covidcritcare"),
  action_contrasts("B", "age65plus", "coviddeath"),
  action_contrasts("B", "age65plus", "noncoviddeath"),
  action_contrasts("B", "age65plus", "fracture"),


  # comment("### Models by JCVI age-band ('jcvi_ageband')"),
  #
  # action_contrasts("B", "jcvi_ageband", "postest"),
  # action_contrasts("B", "jcvi_ageband", "covidemergency"),
  # #action_contrasts("B", "jcvi_ageband", "covidadmittedproxy1"),
  # action_contrasts("B", "jcvi_ageband", "covidadmitted"),
  # #action_contrasts("B", "jcvi_ageband", "noncovidadmitted"),
  # action_contrasts("B", "jcvi_ageband", "covidcritcare"),
  # action_contrasts("B", "jcvi_ageband", "coviddeath"),
  # action_contrasts("B", "jcvi_ageband", "noncoviddeath"),
  # action_contrasts("B", "jcvi_ageband", "fracture"),

  comment("### Models by age-band ('ageband')"),

  action_contrasts("B", "ageband", "postest"),
  action_contrasts("B", "ageband", "covidemergency"),
  #action_contrasts("B", "ageband", "covidadmittedproxy1"),
  action_contrasts("B", "ageband", "covidadmitted"),
  #action_contrasts("B", "ageband", "noncovidadmitted"),
  action_contrasts("B", "ageband", "covidcritcare"),
  action_contrasts("B", "ageband", "coviddeath"),
  action_contrasts("B", "ageband", "noncoviddeath"),
  action_contrasts("B", "ageband", "fracture"),


  comment("### Models by variant era ('variantera')"),

  action_contrasts("B", "variantera", "postest"),
  action_contrasts("B", "variantera", "covidemergency"),
  #action_contrasts("B", "variantera", "covidadmittedproxy1"),
  action_contrasts("B", "variantera", "covidadmitted"),
  #action_contrasts("B", "variantera", "noncovidadmitted"),
  action_contrasts("B", "variantera", "covidcritcare"),
  action_contrasts("B", "variantera", "coviddeath"),
  action_contrasts("B", "variantera", "noncoviddeath"),
  action_contrasts("B", "variantera", "fracture"),


  action_eventcounts("A"),
  action_eventcounts("B"),

  comment("# # # # # # # # # # # # # # # # # # #", "Combine KM / CI estimates across outcomes and subgroups", "# # # # # # # # # # # # # # # # # # #"),

  action_contrasts_combine(
    "A",
    subgroups = c("all", "vax12_type", "prior_covid_infection", "age65plus", "ageband", "cev_cv", "cv", "variantera"),
    outcomes = c("postest", "covidemergency", "covidadmitted", "covidcritcare", "coviddeath", "noncoviddeath", "fracture")
  ),

  action_contrasts_combine(
    "B",
    subgroups = c("all", "vax12_type", "prior_covid_infection", "age65plus", "ageband", "cev_cv", "cv", "variantera"),
    outcomes = c("postest", "covidemergency", "covidadmitted", "covidcritcare", "coviddeath", "noncoviddeath", "fracture")
  ),


  comment("# # # # # # # # # # # # # # # # # # #", "Files for release", "# # # # # # # # # # # # # # # # # # #"),

  action(
    name = "release_objects",
    run = "r:latest analysis/release_objects.R",
    needs = list(
      "data_selection",
      "match_report_A",
      "match_report_B",
      "contrasts_combine_A",
      "contrasts_combine_B"
    ),
    moderately_sensitive = lst(
      releaselist = "output/files-for-release.txt",
      command = "output/osrelease-command.txt",
      csv = "output/release-objects/*/*.csv",
    )
  ),

  comment("# # # # # # # # # # # # # # # # # # #", "ICD10 check", "# # # # # # # # # # # # # # # # # # #"),


  action(
    name = "extract_ICD10check",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_ICD10check --output-format feather",
    needs = list(),
    highly_sensitive = lst(
      cohort = "output/input_ICD10check.feather"
    )
  ),

  action(
    name = "report_ICD10check",
    run = "r:latest analysis/report_ICD10check.R",
    needs = list("extract_ICD10check"),
    moderately_sensitive = lst(
      png = "output/ICD10check/*.png"
    )
  ),


comment("# # # # # # # # # # # # # # # # # # #", "End", "# # # # # # # # # # # # # # # # # # #")

)


project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)

## convert list to yaml, reformat comments and whitespace ----
thisproject <- as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1")


# if running via opensafely, check that the project on disk is the same as the project created here:
if (Sys.getenv("OPENSAFELY_BACKEND") %in% c("expectations", "tpp")){

  thisprojectsplit <- str_split(thisproject, "\n")
  currentproject <- readLines(here("project.yaml"))

  stopifnot("project.yaml is not up-to-date with create-project.R.  Run create-project.R before running further actions." = identical(thisprojectsplit, currentproject))

# if running manually, output new project as normal
} else if (Sys.getenv("OPENSAFELY_BACKEND") %in% c("")){

## output to file ----
  writeLines(thisproject, here("project.yaml"))
#yaml::write_yaml(project_list, file =here("project.yaml"))

## grab all action names and send to a txt file

names(actions_list) %>% tibble(action=.) %>%
  mutate(
    model = action==""  & lag(action!="", 1, TRUE),
    model_number = cumsum(model),
  ) %>%
  group_by(model_number) %>%
  summarise(
    sets = str_trim(paste(action, collapse=" "))
  ) %>% pull(sets) %>%
  paste(collapse="\n") %>%
  writeLines(here("actions.txt"))

# fail if backend not recognised
} else {
  stop("Backend not recognised")
}

