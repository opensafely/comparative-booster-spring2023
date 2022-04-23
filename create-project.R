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
      ),
      moderately_sensitive = lst(
        txt = glue("output/match/{matchset}/*.txt"),
        #csv = glue("output/match/{matchset}/*.csv"),
      )
    ),

    action(
      name = glue("match_report_{matchset}"),
      run = "r:latest analysis/match_report.R",
      arguments = c(matchset),
      needs = list("data_selection",  glue("match_{matchset}")),
      moderately_sensitive = lst(
        txt = glue("output/match/{matchset}/report/*.txt"),
        csv = glue("output/match/{matchset}/report/*.csv"),
        png = glue("output/match/{matchset}/report/*.png"),
        html = glue("output/match/{matchset}/report/*.html")
      )
    )
  )


}

## kaplan-meier action function ----
action_km <- function(
  matchset, subgroup, outcome
){

  action(
    name = glue("km_{matchset}_{subgroup}_{outcome}"),
    run = glue("r:latest analysis/km.R"),
    arguments = c(matchset, subgroup, outcome),
    needs = list(
      glue("match_{matchset}"),
      "data_selection"
    ),
    moderately_sensitive = lst(
      txt = glue("output/match/{matchset}/km/{subgroup}/{outcome}/*.txt"),
      csv = glue("output/match/{matchset}/km/{subgroup}/{outcome}/*.csv"),
      png = glue("output/match/{matchset}/km/{subgroup}/{outcome}/*.png"),
    )
  )
}



## model action function ----
action_km_combine <- function(
    matchset, subgroups, outcomes
){
  action(
    name = glue("km_combine_{matchset}"),
    run = glue("r:latest analysis/km_combine.R"),
    arguments = c(matchset),
    needs = splice(
      as.list(
        glue_data(
          .x=expand_grid(
            subgroup=subgroups,
            outcome=outcomes
          ),
          "km_{matchset}_{subgroup}_{outcome}"
        )
      )
    ),
    moderately_sensitive = lst(
      csv = glue("output/match/{matchset}/km/combined/*.csv"),
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
      feather = "output/data/data_cohort.feather",
      cohortrds = "output/data/data_cohort.rds",
      criteriards = "output/data/data_inclusioncriteria.rds"
    ),
    moderately_sensitive = lst(
      flow = "output/data/flowchart.csv"
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


  comment("# # # # # # # # # # # # # # # # # # #", "Matching", "# # # # # # # # # # # # # # # # # # #"),

  action_match("A"),

  comment("# # # # # # # # # # # # # # # # # # #", "matching set A", "# # # # # # # # # # # # # # # # # # #"),


  comment("### Overall models ('all')"),

  action_km("A", "all", "postest"),
  action_km("A", "all", "covidemergency"),
  action_km("A", "all", "covidadmittedproxy1"),
  action_km("A", "all", "covidadmitted"),
  #action_km("A", "all", "noncovidadmitted"),
  #action_km("A", "all", "covidcc"),
  action_km("A", "all", "coviddeath"),
  action_km("A", "all", "noncoviddeath"),


  comment("### Models by primary course ('vax12_type')"),

  action_km("A", "vax12_type", "postest"),
  action_km("A", "vax12_type", "covidemergency"),
  action_km("A", "vax12_type", "covidadmittedproxy1"),
  action_km("A", "vax12_type", "covidadmitted"),
  #action_km("A", "vax12_type", "noncovidadmitted"),
  #action_km("A", "vax12_type", "covidcc"),
  action_km("A", "vax12_type", "coviddeath"),
  action_km("A", "vax12_type", "noncoviddeath"),


  comment("### Models by clinically vulnerable group ('cev_cv')"),

  action_km("A", "cev_cv", "postest"),
  action_km("A", "cev_cv", "covidemergency"),
  action_km("A", "cev_cv", "covidadmittedproxy1"),
  action_km("A", "cev_cv", "covidadmitted"),
  #action_km("A", "cev_cv", "noncovidadmitted"),
  #action_km("A", "cev_cv", "covidcc"),
  action_km("A", "cev_cv", "coviddeath"),
  action_km("A", "cev_cv", "noncoviddeath"),


  comment("### Models by prior infection ('prior_covid_infection')"),

  action_km("A", "prior_covid_infection", "postest"),
  action_km("A", "prior_covid_infection", "covidemergency"),
  action_km("A", "prior_covid_infection", "covidadmittedproxy1"),
  action_km("A", "prior_covid_infection", "covidadmitted"),
  #action_km("A", "prior_covid_infection", "noncovidadmitted"),
  #action_km("A", "prior_covid_infection", "covidcc"),
  action_km("A", "prior_covid_infection", "coviddeath"),
  action_km("A", "prior_covid_infection", "noncoviddeath"),


  comment("### Models by age ('age65plus')"),

  action_km("A", "age65plus", "postest"),
  action_km("A", "age65plus", "covidemergency"),
  action_km("A", "age65plus", "covidadmittedproxy1"),
  action_km("A", "age65plus", "covidadmitted"),
  #action_km("A", "age65plus", "noncovidadmitted"),
  #action_km("A", "age65plus", "covidcc"),
  action_km("A", "age65plus", "coviddeath"),
  action_km("A", "age65plus", "noncoviddeath"),

  comment("# # # # # # # # # # # # # # # # # # #", "Combine KM estimates across outcomes and subgroups", "# # # # # # # # # # # # # # # # # # #"),

  action_km_combine(
    "A",
    subgroups = c("all", "vax12_type", "prior_covid_infection", "age65plus", "cev_cv"),
    outcomes=c("postest", "covidemergency", "covidadmittedproxy1", "covidadmitted", "coviddeath", "noncoviddeath")
  ),

  comment("# # # # # # # # # # # # # # # # # # #", "Files for release", "# # # # # # # # # # # # # # # # # # #"),

  action(
    name = "release_objects",
    run = "r:latest analysis/release_objects.R",
    needs = list(
      "data_selection",
      "match_report_A",
      "km_combine_A"
    ),
    moderately_sensitive = lst(
      txt = "output/files-for-release.txt",
      csv = "output/release-objects/*/*.csv",
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

