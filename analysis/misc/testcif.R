
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

matchset <- "A"
subgroup <- "all"
#subgroup <- "vax12_type"
outcome <- "covidadmitted"

cif.se <- function(time, ci, n.risk, n.event, kmsurv, kmsummand){
  # from https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Cumulative_Incidence.pdf
  # also here: https://onlinelibrary.wiley.com/doi/full/10.1002/bimj.200900039?saml_referrer
  timeindex <- seq_along(time)
  lapply(timeindex, function(i) {
    cii <- ci[1:i]
    bi <- ((n.risk - n.event)/n.risk)[1:i]
    di <- (n.event/(n.risk^2))[1:i]
    lagkmi <- lag(kmsurv,1,1)[1:i]
    kmsummandi <- kmsummand[1:i]
    vt <-
      sum(((cii[i] - cii)^2) * kmsummandi) +
      sum((lagkmi^2) * bi * di) +
      -2* sum((cii[i] - cii) * lagkmi * di)

    sqrt(vt)
  })
}


output_dir <- here("output", "match", matchset, "ci", subgroup, outcome)

## import match data
data_matchstatus <- read_rds(here("output", "match", matchset, "data_matchstatus.rds"))

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
    data_matchstatus %>% filter(matched) %>% select(-vax3_date),
    .,
    by= c("patient_id")
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
      treatment_date + maxfup,
      na.rm=TRUE
    ),

    noncompetingcensor_date = pmin(
      dereg_date,
      vax4_date-1, # -1 because we assume vax occurs at the start of the day
      study_dates$studyend_date,
      treatment_date + maxfup,
      na.rm=TRUE
    ),

    tte_outcome = tte(treatment_date, outcome_date, censor_date, na.censor=FALSE),
    ind_outcome = censor_indicator(outcome_date, censor_date),

    # possible competing events
    tte_coviddeath = tte(treatment_date, coviddeath_date, noncompetingcensor_date, na.censor=FALSE),
    tte_noncoviddeath = tte(treatment_date, noncoviddeath_date, noncompetingcensor_date, na.censor=FALSE),
    tte_death = tte(treatment_date, death_date, noncompetingcensor_date, na.censor=FALSE),
    tte_censor = tte(treatment_date, censor_date, censor_date, na.censor=FALSE),

  ) %>%
  rowwise() %>%
  mutate(
    status = which.min(c(outcome_date, coviddeath_date, noncoviddeath_date, noncompetingcensor_date))
  ) %>%
  ungroup() %>%
  mutate(
    status = factor(as.character(status), levels=c("4", "1", "2", "3"), labels = c("censored", outcome, "coviddeath", "noncoviddeath"))
  )


testdata = data_matched %>%
  mutate(
    statuskm = fct_recode(status, "censored"="noncoviddeath", "censored"="coviddeath"),
    statusci = status
  )

with(testdata, table(statuskm, statusci))

testfitkm <- survfit(Surv(tte_outcome, ind_outcome) ~ 1, data = testdata, conf.type="log")
testfitstatuskm <- survfit(Surv(tte_outcome, statuskm) ~ 1, data = testdata)
testfitstatusci <- survfit(Surv(tte_outcome, statusci) ~ 1, data = testdata)

testdatakm <- broom::tidy(testfitkm) %>%
  mutate(
    summand = n.event / ((n.risk - n.event) * n.risk),
    surv=cumprod(1 - n.event / n.risk),
    #surv.ll = conf.low,
    #surv.ul = conf.high,
    surv.se = surv * sqrt(cumsum(summand))
  )


testdatastatuskm0 <- broom::tidy(testfitstatuskm)
testdatastatuskm <-
  left_join(
    testdatastatuskm0 %>% filter(state=="(s0)") %>% select(time, n.risk, n.censor),
    testdatastatuskm0 %>% filter(state==outcome) %>% select(-n.censor, -n.risk),
    by="time"
  ) %>%
  mutate(
    inv.estimate=1-estimate, inv.conf.low=1-conf.high, inv.conf.high=1-conf.low,
    summand = n.event / ((n.risk - n.event) * n.risk),
    surv = cumprod(1 - n.event / n.risk),
    #surv.ll = conf.low,
    #surv.ul = conf.high,
    surv.se = surv * sqrt(cumsum(summand)),

    risk.se = cif.se(time, estimate, n.risk, n.event, surv, summand)
  )

testdatastatusci0 <- broom::tidy(testfitstatusci)
testdatastatusci <-
  bind_cols(
    testdatastatusci0 %>% filter(state=="(s0)") %>% select(time, n.risk, n.censor),
    testdatastatusci0 %>% filter(state!="(s0)") %>% group_by(time) %>% summarise(n.allevents=sum(n.event)) %>% ungroup() %>% select(-time),
    testdatastatusci0 %>% filter(state!=outcome) %>% group_by(time) %>% summarise(n.kmcensor=mean(n.censor) + sum(n.event)) %>% ungroup() %>% select(-time),
    testdatastatusci0 %>% filter(state==outcome) %>% select(-time, -n.censor, -n.risk, -state),
  ) %>%
  mutate(
    risk = estimate,
    surv = 1-risk,
    risk.se = std.error,
    surv.se = std.error,
    surv.ll = conf.low,
    surv.ul = conf.high,

    allsummand = n.allevents / ((n.risk - n.allevents) * n.risk),
    allsurv = cumprod(1 - n.allevents / n.risk),

    kmsummand = n.event / ((n.risk - n.event) * n.risk),
    kmsurv = cumprod(1 - n.event / n.risk),
    kmsurv.se = kmsurv * sqrt(cumsum(kmsummand)), #greenwood's formula

    summand = (n.event / n.risk) * lag(allsurv, 1, 1),
    risk = cumsum(summand),


    risk.se = cif.se(time, estimate, n.risk, n.event, allsurv, allsummand)
  )

