
# # # # # # # # # # # # # # # # # # # # #
# Purpose: fit models for comparative effectiveness
#  - import matched data
#  - adds outcome variable and restricts follow-up
#  - fits the Cox models with time-varying effects
#  - The script must be accompanied by two arguments:
#    `outcome` - the dependent variable in the regression model
#    `subgroup` - the subgroup variable for the regression model followed by a hyphen and the level of the subgroup

# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----


# import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  removeobjects <- FALSE
  matchset <- "A"
  subgroup <- "all"
  #subgroup <- "vax12_type"
  outcome <- "postest"

} else {
  removeobjects <- TRUE
  matchset <- args[[1]]
  subgroup <- args[[2]]
  outcome <- args[[3]]

}


## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')

## Import custom user functions from lib
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "survival.R"))
source(here("lib", "functions", "redaction.R"))

## Import design elements
source(here("lib", "design", "design.R"))


# derive subgroup info
#subgroup_variable <-  str_split_fixed(subgroup,"-",2)[,1]
#subgroup_level <- str_split_fixed(subgroup,"-",2)[,2]
#subgroup_dummy <- paste(c(subgroup_variable,subgroup_level), collapse="_")

# create output directories ----

output_dir <- here("output", "match", matchset, "model", subgroup, outcome)
fs::dir_create(output_dir)

## create special log file ----
cat(glue("## script info for {outcome} ##"), "  \n", file = fs::path(output_dir, glue("model_log.txt")), append = FALSE)

## functions to pass additional log info to seperate file
logoutput <- function(...){
  cat(..., file = fs::path(output_dir, glue("model_log.txt")), sep = "\n  ", append = TRUE)
  cat("\n", file = fs::path(output_dir, glue("model_log.txt")), sep = "\n  ", append = TRUE)
}

logoutput_datasize <- function(x){
  nm <- deparse(substitute(x))
  logoutput(
    glue(nm, " data size = ", nrow(x)),
    glue(nm, " memory usage = ", format(object.size(x), units="GB", standard="SI", digits=3L))
  )
}

## import match data
data_matchstatus <- read_rds(here("output", "match", "match_data_matchstatus.rds"))

## import baseline data, restrict to matched individuals and derive time-to-event variables
data_matched <-
  read_rds(here("output", "data", "data_cohort.rds")) %>%
  select(
    # select only variables needed for models to save space
    patient_id, vax3_date,
    all_of(subgroup),
    all_of(paste0(c(outcome, "death", "dereg", "coviddeath", "noncoviddeath", "vax4"), "_date")),
    all_of(matching_variables[[matchset]]$all), # model formula with all variables
    all_of(all.vars(formula_allcovariates)), # model formula with all variables
  ) %>%
  #filter(patient_id %in% data_matchstatus$patient_id[data_matchstatus$matched]) %>%
  left_join(
    data_matchstatus %>% filter(matched),
    .,
    by= c("patient_id", "vax3_date")
  ) %>%
  mutate(


    treatment_date = vax3_date-1L, # -1 because we assume vax occurs at the start of the day, and so outcomes occurring on the same day as treatment are assumed "1 day" long
    outcome_date = .[[glue("{outcome}_date")]],

    # person-time is up to and including censor date
    censor_date = pmin(
      dereg_date,
      vax4_date-1, # -1 because we assume vax occurs at the start of the day
      death_date,
      study_dates$studyend_date,
      na.rm=TRUE
    ),

    noncompetingcensor_date = pmin(
      dereg_date,
      vax4_date-1, # -1 because we assume vax occurs at the start of the day
      study_dates$studyend_date,
      na.rm=TRUE
    ),

    tte_outcome = tte(treatment_date, outcome_date, censor_date, na.censor=FALSE),
    ind_outcome = censor_indicator(outcome_date, censor_date),

    # possible competing events
    tte_coviddeath = tte(treatment_date, coviddeath_date, noncompetingcensor_date, na.censor=FALSE),
    tte_noncoviddeath = tte(treatment_date, noncoviddeath_date, noncompetingcensor_date, na.censor=FALSE),
    tte_death = tte(treatment_date, death_date, noncompetingcensor_date, na.censor=FALSE),
    tte_censor = tte(treatment_date, censor_date, censor_date, na.censor=FALSE),


  )

# create dataset of one row per patient per follow-up period defined in `postbaselinecuts`

local({

  # one row per patient per post-recruitment split time
  fup_split <-
    data_matched %>%
    select(patient_id, treatment) %>%
    uncount(weights = length(postbaselinecuts)-1, .id="period_id") %>%
    mutate(
      fup_time = postbaselinecuts[period_id],
      fup_period = paste0(postbaselinecuts[period_id], "-", postbaselinecuts[period_id+1]-1)
    ) %>%
    droplevels() %>%
    select(
      patient_id, period_id, fup_time, fup_period
    )


  data_periods <<-
    # add outcome
    tmerge(
      data1 = data_matched,
      data2 = data_matched,
      id = patient_id,
      tstart = 0,
      tstop = pmin(tte_censor, tte_outcome, last(postbaselinecuts), na.rm=TRUE),
      ind_outcome = event(if_else(ind_outcome, tte_outcome, NA_real_))
    ) %>%
    # add post-treatment periods
    tmerge(
      data1 = .,
      data2 = fup_split,
      id = patient_id,
      fup_period = tdc(fup_time, fup_period),
      treatment_period_id = tdc(fup_time, period_id)
    ) %>%
    mutate(
      id = NULL,
      # time-zero is recruitment day
      tstart_calendar = tstart + vax3_date,
      tstop_calendar = tstop + vax3_date,
    ) %>%
    # create dummy variables for post-treatment periods: 1 if treatment==1 and period = period, otherwise zero.
    fastDummies::dummy_cols(select_columns = c("treatment_period_id"), remove_selected_columns = TRUE) %>%
    mutate(
      across(
        starts_with("treatment_period_id_"),
        ~if_else(treatment==1L, .x, 0L)
      )
    )

})


if(removeobjects){rm(data_matched)}

logoutput_datasize(data_periods)

write_rds(data_periods, fs::path(output_dir, "model_data_periods.rds"), compress="gz")

# outcome frequency
outcomes_per_treated <- table(days = data_periods$fup_period, outcome=data_periods$ind_outcome, treated=data_periods$treatment)


## pre-flight checks ----

### event counts within each covariate level ----


tbltab0 <-
  data_periods %>%
  select(ind_outcome, treatment, subgroup, fup_period, all_of(all.vars(formula3_pw)), -starts_with("treatment_period")) %>%
  select( -tstart, -tstop) %>%
  mutate(
    across(
      where(~ !is.factor(.x)),
      ~as.character(.)
    ),
  )

map(tbltab0, class)

event_counts <-
  tbltab0 %>%
  split(.["ind_outcome"]) %>%
  map(~select(., -ind_outcome)) %>%
  map(
    function(data){
      map(data, redacted_summary_cat, redaction_threshold=0) %>%
        bind_rows(.id="variable") %>%
        select(-redacted, -pct_nonmiss)
    }
  ) %>%
  bind_rows(.id = "event") %>%
  pivot_wider(
    id_cols=c(variable, .level),
    names_from = event,
    names_glue = "event{event}_{.value}",
    values_from = c(n, pct)
  )

write_csv(event_counts, fs::path(output_dir, "model_preflight.csv"))

### event counts within each follow up period ----

event_counts_period <-
  tbltab0 %>%
  split(.[c("fup_period", "ind_outcome")], sep="..") %>%
  map(~select(., -fup_period, -ind_outcome)) %>%
  map(
    function(data){
      map(data, redacted_summary_cat, redaction_threshold=0) %>%
        bind_rows(.id="variable") %>%
        select(-redacted, -pct_nonmiss)
    }
  ) %>%
  bind_rows(.id = "period..outcome") %>%
  separate(period..outcome, into = c("period", "event"), sep="\\.\\.") %>%
  pivot_wider(
    id_cols=c(variable, .level),
    names_from = c("period", "event"),
    names_glue = "days {period}.{event}_{.value}",
    values_from = c(n, pct)
  )

write_csv(event_counts_period, fs::path(output_dir, "model_preflight_period.csv"))

### event counts within strata levels ----

event_counts_strata <-
  data_periods %>%
  mutate(
    strata = strata(!!!syms(all.vars(formula_strata)[-1]))
  ) %>%
  select(ind_outcome, strata, fup_period) %>%
  split(.["ind_outcome"]) %>%
  map(~select(., -ind_outcome)) %>%
  map(
    function(data){
      map(data, redacted_summary_cat, redaction_threshold=0) %>%
        bind_rows(.id="variable") %>%
        select(-redacted, -pct_nonmiss)
    }
  ) %>%
  bind_rows(.id = "event") %>%
  pivot_wider(
    id_cols=c(variable, .level),
    names_from = event,
    names_glue = "event{event}_{.value}",
    values_from = c(n, pct)
  )

write_csv(event_counts_strata, fs::path(output_dir, "model_preflight_strata.csv"))


## fit models ----


opt_control <- coxph.control(iter.max = 30)


## manage parallelisation

library("doParallel")

parallel::detectCores() # how many cores available?
n_threads <- 2

cluster <- parallel::makeCluster(
  n_threads,
  type = "PSOCK" # this should work across multi-core windows or linux machines
)
print(cluster)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = cluster)





foreach::foreach(sub = subgroup) %dopar% {

  data_stratum <- data_period %>% filter(subgroup==sub)


  cox_model <- function(timesplit, number, formula_cox){

    # fit a time-dependent cox model and output summary functions
    coxmod <- coxph(
      formula = formula_cox,
      data = data_stratum,
      robust = TRUE,
      id = patient_id,
      na.action = "na.fail",
      control = opt_control
    )

    print(warnings())
    # logoutput(
    #   glue("model{number} data size = ", coxmod$n),
    #   glue("model{number} memory usage = ", format(object.size(coxmod), units="GB", standard="SI", digits=3L)),
    #   glue("convergence status: ", coxmod$info[["convergence"]])
    # )

    tidy <-
      broom.helpers::tidy_plus_plus(
        coxmod,
        exponentiate = FALSE
      ) %>%
      add_column(
        model = number,
        .before=1
      )

    glance <-
      broom::glance(coxmod) %>%
      add_column(
        model = number,
        convergence = coxmod$info[["convergence"]],
        ram = format(object.size(coxmod), units="GB", standard="SI", digits=3L),
        .before = 1
      )

    # remove data to save space (it's already saved above)
    coxmod$data <- NULL
    write_rds(coxmod, fs::path(output_dir, glue("obj{number}.rds")), compress="gz")

    lst(glance, tidy)
  }

  summary0 <- cox_model("", 0, formula0_pw)
  #summary1 <- cox_model("", 1, formula1_pw)
  #summary2 <- cox_model("", 2, formula2_pw)
  summary3 <- cox_model("", 3, formula3_pw)

  # combine results
  model_glance <-
    bind_rows(
      summary0$glance,
      #summary1$glance,
      #summary2$glance,
      summary3$glance,
    ) %>%
    mutate(
      model_descr = fct_recoderelevel(as.character(model), model_descr)
    )
  write_csv(model_glance, fs::path(output_dir, "glance.csv"))

  model_tidy <-
    bind_rows(
      summary0$tidy,
      #summary1$tidy,
      #summary2$tidy,
      summary3$tidy,
    ) %>%
    mutate(
      model_descr = fct_recoderelevel(as.character(model), model_descr)
    )
  write_csv(model_tidy, fs::path(output_dir, "tidy.csv"))


  if(removeobjects){rm(summary0, summary1, summary2, summary3)}
}


parallel::stopCluster(cl = cluster)
