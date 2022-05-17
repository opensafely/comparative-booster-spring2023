
# # # # # # # # # # # # # # # # # # # # #
# Purpose: Get Kaplan-Meier estimates for specified outcome, and derive risk differences
#  - import matched data
#  - adds outcome variable and restricts follow-up
#  - gets KM estimates
#  - The script must be accompanied by three arguments:
#    `matchset` - the matching set used for matching
#    `subgroup` - the subgroup variable, which is used to stratify KM estimates
#    `outcome` - the dependent variable

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
  outcome <- "covidadmitted"

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

subgroup_sym <- sym(subgroup)
#subgroup_variable <-  str_split_fixed(subgroup,"-",2)[,1]
#subgroup_level <- str_split_fixed(subgroup,"-",2)[,2]
#subgroup_dummy <- paste(c(subgroup_variable,subgroup_level), collapse="_")

# create output directories ----

output_dir <- here("output", "match", matchset, "km", subgroup, outcome)
fs::dir_create(output_dir)

## create special log file ----
cat(glue("## script info for {outcome} ##"), "  \n", file = fs::path(output_dir, glue("log.txt")), append = FALSE)

## functions to pass additional log info to seperate file
logoutput <- function(...){
  cat(..., file = fs::path(output_dir, glue("log.txt")), sep = "\n  ", append = TRUE)
  cat("\n", file = fs::path(output_dir, glue("log.txt")), sep = "\n  ", append = TRUE)
}

logoutput_datasize <- function(x){
  nm <- deparse(substitute(x))
  logoutput(
    glue(nm, " data size = ", nrow(x)),
    glue(nm, " memory usage = ", format(object.size(x), units="GB", standard="SI", digits=3L))
  )
}

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

  )


## TODO -- consider bootstrapping  ----
# consider bootstrapping data_matched dataset (by matched pairs) to simplify derivation of relative risk confidence limits


# outcome frequency
outcomes_per_treated <- table(outcome=data_matched$ind_outcome, treated=data_matched$treatment)


## pre-flight checks ----

### event counts within each covariate level ----


tbltab0 <-
  data_matched %>%
  select(ind_outcome, treatment, subgroup, all_of(matching_variables[[matchset]]$all)) %>%
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

## redaction threshold ----

threshold <- 7

## kaplan meier cumulative risk differences ----

data_surv <-
  data_matched %>%
  group_by(!!subgroup_sym, treatment) %>%
  nest() %>%
  mutate(
    n_events = map_int(data, ~sum(.x$ind_outcome, na.rm=TRUE)),
    surv_obj = map(data, ~{
      survfit(Surv(tte_outcome, ind_outcome) ~ 1, data = .x, conf.type="log-log")
    }),
    surv_obj_tidy = map(surv_obj, ~tidy_surv(.x, times=seq_len(maxfup))), # return survival table for each day of follow up
  ) %>%
  select(!!subgroup_sym, treatment, n_events, surv_obj_tidy) %>%
  unnest(surv_obj_tidy) %>%
  mutate(
    treatment_descr = fct_recoderelevel(as.character(treatment), recoder$treatment)
  )

data_surv_rounded <-
  data_surv %>%
  mutate(
    # Use ceiling not round. This is slightly biased upwards,
    # but means there's no disclosure risk at the boundaries (0 and 1) where masking would otherwise be threshold/2
    #
    # Explanation:
    # ensure every "step" in the KM survival curve is based on no fewer than `threshold` outcome+censoring events
    # N = max(n.risk, na.rm=TRUE) is the number at risk at time zero.
    # N/threshold is the inverse of the minimum `step` size on the survival scale (0-1)
    # floor(N/threshold) rounds down to nearest integer.
    # 1/floor(N) is the minimum step size on the survival scale (0-1), ensuring increments no fewer than `threshold` on the events scale
    # ceiling_any(x, min_increment) rounds up values of x on the survival scale, so that they lie on the grid of width `min_increment`.



    N = max(n.risk, na.rm=TRUE),
    cml.event = ceiling_any(cumsum(replace_na(n.event, 0)), threshold),
    cml.censor = ceiling_any(cumsum(replace_na(n.censor, 0)), threshold),
    n.event = diff(c(0,cml.event)),
    n.censor = diff(c(0,cml.censor)),
    n.risk = ceiling_any(N, threshold) - lag(cml.event + cml.censor,1,0),
    summand = n.event / ((n.risk - n.event) * n.risk),

    ## calculate surv based on rounded event counts
    surv = cumprod(1 - n.event / n.risk),
    surv.se = surv * sqrt(cumsum(replace_na(summand, 0))),
    llsurv = log(-log(surv)),
    llsurv.se = sqrt((1 / log(surv)^2) * cumsum(summand)),
    surv.ll = exp(-exp(llsurv + qnorm(0.025)*llsurv.se)),
    surv.ul = exp(-exp(llsurv + qnorm(0.975)*llsurv.se)),

    # Or round surv based on a grid of values representing increments of `threshold`
    #surv = ceiling_any(surv, 1/floor(N/threshold)),
    #surv.ll = ceiling_any(surv.ll, 1/floor(N/threshold)),
    #surv.ul = ceiling_any(surv.ul, 1/floor(N/threshold)),

    haz = -(surv-lag(surv, 1, 1))/lag(surv, 1, 1), # n.event / (n.risk * interval),
    haz.se = haz * sqrt((n.risk - n.event) / (n.risk * n.event)),
    cml.haz = cumsum(haz),
    cmlhaz.se = surv.se/surv,
  ) %>%
  select(
    !!subgroup_sym, treatment, treatment_descr, time, lagtime, leadtime, interval,
    n.risk, n.event, n.censor, summand,
    surv, surv.se, surv.ll, surv.ul,
    haz, haz.se,
    cml.haz, cml.haz.se
  )


write_csv(data_surv_rounded, fs::path(output_dir, "km_estimates.csv"))

plot_km <- data_surv %>%
  ggplot(aes(group=treatment_descr, colour=treatment_descr, fill=treatment_descr)) +
  geom_step(aes(x=time, y=1-surv))+
  geom_rect(aes(xmin=lagtime, xmax=time, ymin=1-surv.ll, ymax=1-surv.ul), alpha=0.1, colour="transparent")+
  facet_grid(rows=vars(!!subgroup_sym))+
  scale_color_brewer(type="qual", palette="Set1", na.value="grey") +
  scale_fill_brewer(type="qual", palette="Set1", guide="none", na.value="grey") +
  scale_x_continuous(breaks = seq(0,600,14))+
  scale_y_continuous(expand = expansion(mult=c(0,0.01)))+
  coord_cartesian(xlim=c(0, NA))+
  labs(
    x="Days",
    y="Cumulative incidence",
    colour=NULL,
    title=NULL
  )+
  theme_minimal()+
  theme(
    axis.line.x = element_line(colour = "black"),
    panel.grid.minor.x = element_blank(),
    legend.position=c(.05,.95),
    legend.justification = c(0,1),
  )

plot_km

ggsave(filename=fs::path(output_dir, "km_plot.png"), plot_km, width=20, height=15, units="cm")


plot_km_rounded <- data_surv_rounded %>%
  ggplot(aes(group=treatment_descr, colour=treatment_descr, fill=treatment_descr)) +
  geom_step(aes(x=time, y=1-surv))+
  geom_rect(aes(xmin=lagtime, xmax=time, ymin=1-surv.ll, ymax=1-surv.ul), alpha=0.1, colour="transparent")+
  facet_grid(rows=vars(!!subgroup_sym))+
  scale_color_brewer(type="qual", palette="Set1", na.value="grey") +
  scale_fill_brewer(type="qual", palette="Set1", guide="none", na.value="grey") +
  scale_x_continuous(breaks = seq(0,600,14))+
  scale_y_continuous(expand = expansion(mult=c(0,0.01)))+
  coord_cartesian(xlim=c(0, NA))+
  labs(
    x="Days",
    y="Cumulative incidence",
    colour=NULL,
    title=NULL
  )+
  theme_minimal()+
  theme(
    axis.line.x = element_line(colour = "black"),
    panel.grid.minor.x = element_blank(),
    legend.position=c(.05,.95),
    legend.justification = c(0,1),
  )

plot_km_rounded

ggsave(filename=fs::path(output_dir, "km_plot_rounded.png"), plot_km_rounded, width=20, height=15, units="cm")


## calculate quantities relating to kaplan-meier curve and their ratio / difference / etc

kmcontrast <- function(data, cuts=NULL){

  if(is.null(cuts)){cuts <- unique(c(0,data$time))}

  data %>%
    filter(time!=0) %>%
    transmute(
      !!subgroup_sym,
      treatment,

      time, lagtime, interval,
      period_start = as.integer(as.character(cut(time, cuts, right=TRUE, label=cuts[-length(cuts)]))),
      period_end = as.integer(as.character(cut(time, cuts, right=TRUE, label=cuts[-1]))),
      period = cut(time, cuts, right=TRUE, label=paste0(cuts[-length(cuts)]+1, " - ", cuts[-1])),

      n.atrisk = n.risk,
      n.event, n.censor, summand,

      cml.persontime = cumsum(n.atrisk*interval),
      cml.event = cumsum(replace_na(n.event, 0)),
      cml.censor = cumsum(replace_na(n.censor, 0)),
      cml.summand = cumsum(summand),

      rate = n.event / n.atrisk,
      cml.rate = cml.event / cml.persontime,

      surv, surv.se, surv.ll, surv.ul,

      risk = 1 - surv,
      risk.se = surv.se,
      risk.ll = 1 - surv.ul,
      risk.ul = 1 - surv.ll,

      haz, haz.se,
      cml.haz, cml.haz.se

    ) %>%
    group_by(!!subgroup_sym, treatment, period_start, period_end, period) %>%
    summarise(

      ## time-period-specific quantities

      persontime = sum(n.atrisk*interval), # total person-time at risk within time period

      n.atrisk = first(n.atrisk), # number at risk at start of time period
      n.event = sum(n.event, na.rm=TRUE), # number of events within time period
      n.censor = sum(n.censor, na.rm=TRUE), # number censored within time period

      rate = n.event/persontime, # = weighted.mean(haz, n.atrisk*interval), incidence rate. this is equivalent to a weighted average of the hazard ratio, with time-exposed as the weights

      interval = sum(interval), # width of time period

      ## quantities calculated from time zero until end of time period
      # these should be the same as the daily values as at the end of the time period

      surv = last(surv),
      surv.se = last(surv.se),
      surv.ll = last(surv.ll),
      surv.ul = last(surv.ul),

      risk = last(risk),
      risk.se = last(risk.se),
      risk.ll = last(risk.ul),
      risk.ul = last(risk.ll),

      cml.haz = last(cml.haz),  # cumulative hazard from time zero to end of time period

      cml.rate = last(cml.rate), # event rate from time zero to end of time period

      # cml.persontime = last(cml.persontime), # total person-time at risk from time zero to end of time period
       cml.event = last(cml.event), # number of events from time zero to end of time period
      # cml.censor = last(cml.censor), # number censored from time zero to end of time period

      # cml.summand = last(cml.summand), # summand used for estimation of SE of survival

      .groups="drop"
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols= c(subgroup, "period_start", "period_end", "period",  "interval"),
      names_from=treatment,
      names_glue="{.value}_{treatment}",
      values_from=c(

        persontime, n.atrisk, n.event, n.censor,
        rate,

        cml.haz,
        surv, surv.se, surv.ll, surv.ul,
        risk, risk.se, risk.ll, risk.ul,

        cml.event, cml.rate
        )
    ) %>%
    mutate(
      n.nonevent_0 = n.atrisk_0 - n.event_0,
      n.nonevent_1 = n.atrisk_1 - n.event_1,


      ## time-period-specific quantities

      # hazard ratio, standard error and confidence limits

      # incidence rate ratio
      irr = rate_1 / rate_0,
      irr.ln.se = sqrt((1/n.event_0) + (1/n.event_1)),
      irr.ll = exp(log(irr) + qnorm(0.025)*irr.ln.se),
      irr.ul = exp(log(irr) + qnorm(0.975)*irr.ln.se),

      # incidence rate difference
      #ird = rate_1 - rate_0,

      ## quantities calculated from time zero until end of time period
      # these should be the same as values calculated on each day of follow up

      # survival ratio, standard error, and confidence limits
      kmsr = surv_1 / surv_0,
      #kmsr.ln = log(kmsr),
      kmsr.ln.se = (surv.se_0/surv_0) + (surv.se_1/surv_1), #because cmlhaz = -log(surv) and cmlhaz.se = surv.se/surv
      kmsr.ll = exp(log(kmsr) + qnorm(0.025)*kmsr.ln.se),
      kmsr.ul = exp(log(kmsr) + qnorm(0.975)*kmsr.ln.se),

      # risk ratio, standard error, and confidence limits, using delta method
      kmrr = risk_1 / risk_0,
      #kmrr.ln = log(kmrr),
      kmrr.ln.se = sqrt((risk.se_1/risk_1)^2 + (risk.se_0/risk_0)^2),
      kmrr.ll = exp(log(kmrr) + qnorm(0.025)*kmrr.ln.se),
      kmrr.ul = exp(log(kmrr) + qnorm(0.975)*kmrr.ln.se),

      #kmrr.se = (kmrr^2)*((risk.se_1/risk_1)^2 + (risk.se_0/risk_0)^2),
      #kmrr.ll2 = kmrr + qnorm(0.025)*kmrr.se,
      #kmrr.ul2 = kmrr + qnorm(0.975)*kmrr.se,


      # risk difference, standard error and confidence limits
      kmrd = risk_1 - risk_0,
      #kmrd.se = sqrt( ((n.event_1*n.nonevent_1)/(n.atrisk_1^3)) + ((n.event_0*n.nonevent_0)/(n.atrisk_0^3)) ), # ignores censoring
      kmrd.se = sqrt( (risk.se_0^2) + (risk.se_1^2) ), # combining SEs from greenwood's formula
      kmrd.ll = kmrd + qnorm(0.025)*kmrd.se,
      kmrd.ul = kmrd + qnorm(0.975)*kmrd.se,


      # cumulative incidence rate ratio
      cmlirr = cml.rate_1 / cml.rate_0,
      cmlirr.ln.se = sqrt((1/cml.event_0) + (1/cml.event_1)),
      cmlirr.ll = exp(log(cmlirr) + qnorm(0.025)*cmlirr.ln.se),
      cmlirr.ul = exp(log(cmlirr) + qnorm(0.975)*cmlirr.ln.se),

      # cumulative incidence rate difference
      #cmlird = cml.rate_1 - cml.rate_0
    )
}

#km_contrasts_daily <- kmcontrast(data_surv)
#km_contrasts_cuts <- kmcontrast(data_surv, postbaselinecuts)
#km_contrasts_overall <- kmcontrast(data_surv, c(0,maxfup))


km_contrasts_rounded_daily <- kmcontrast(data_surv_rounded)
km_contrasts_rounded_cuts <- kmcontrast(data_surv_rounded, postbaselinecuts)
km_contrasts_rounded_overall <- kmcontrast(data_surv_rounded, c(0,maxfup))



## Cox models ----

coxcontrast <- function(data, cuts=NULL){

  if(is.null(cuts)){cuts <- unique(c(0,data$time))}

  fup_split <-
    data %>%
    select(patient_id, treatment) %>%
    uncount(weights = length(cuts)-1, .id="period_id") %>%
    mutate(
      fup_time = cuts[period_id],
      fup_period = paste0(cuts[period_id], "-", cuts[period_id+1]-1)
    ) %>%
    droplevels() %>%
    select(
      patient_id, period_id, fup_time, fup_period
    )

  data_split <-
    tmerge(
      data1 = data,
      data2 = data,
      id = patient_id,
      tstart = 0,
      tstop = tte_outcome,
      ind_outcome = event(if_else(ind_outcome, tte_outcome, NA_real_))
    ) %>%
    # add post-treatment periods
    tmerge(
      data1 = .,
      data2 = fup_split,
      id = patient_id,
      period_id = tdc(fup_time, period_id)
    ) %>%
    mutate(
      period_start = postbaselinecuts[period_id],
      period_end = postbaselinecuts[period_id+1],
    )

  data_cox <-
    data_split %>%
    group_by(!!subgroup_sym, period_start, period_end) %>%
    nest() %>%
    mutate(
      cox_obj = map(data, ~{
        coxph(Surv(tstart, tstop, ind_outcome) ~ treatment, data = .x, y=FALSE, robust=TRUE, id=patient_id, na.action="na.fail")
      }),
      cox_obj_tidy = map(cox_obj, ~broom::tidy(.x)),
    ) %>%
    select(!!subgroup_sym, period_start, period_end, cox_obj_tidy) %>%
    unnest(cox_obj_tidy) %>%
    transmute(
      !!subgroup_sym,
      period_start,
      period_end,
      coxhazr = exp(estimate),
      coxhr.se = robust.se,
      coxhr.ll = exp(estimate + qnorm(0.025)*robust.se),
      coxhr.ul = exp(estimate + qnorm(0.975)*robust.se),
    )
  data_cox

}

cox_contrasts_cuts <- coxcontrast(data_matched, postbaselinecuts)
cox_contrasts_overall <- coxcontrast(data_matched, c(0,maxfup))

# cox HR is a safe statistic so no need to redact/round
contrasts_rounded_daily <-  km_contrasts_rounded_daily # don't bother with cox as HR within daily intervals will be imprecisely estimated
contrasts_rounded_cuts <-  left_join(km_contrasts_rounded_cuts, cox_contrasts_cuts, by=c(subgroup, "period_start", "period_end"))
contrasts_rounded_overall <-  left_join(km_contrasts_rounded_overall, cox_contrasts_overall, by=c(subgroup, "period_start", "period_end"))


write_csv(contrasts_rounded_daily, fs::path(output_dir, "contrasts_daily.csv"))
write_csv(contrasts_rounded_cuts, fs::path(output_dir, "contrasts_cuts.csv"))
write_csv(contrasts_rounded_overall, fs::path(output_dir, "contrasts_overall.csv"))
