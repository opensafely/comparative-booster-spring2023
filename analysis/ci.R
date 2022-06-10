
# # # # # # # # # # # # # # # # # # # # #
# Purpose: Get cumulative incidence estimates for specified outcome, and derive risk differences
#  - import matched data
#  - adds outcome variable and restricts follow-up
#  - gets CI estimates, with covid and non covid death as competing risks
#  - The script must be accompanied by three arguments:
#    `matchset` - the matching set used for matching
#    `subgroup` - the subgroup variable, which is used to stratify CI estimates
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
  outcome <- "coviddeath"

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

output_dir <- here("output", "match", matchset, "ci", subgroup, outcome)
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
  ) %>%
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
    status = factor(
      as.character(status),
      levels=c("4","1","2","3"),
      labels = c("censored", outcome, "coviddeath", "noncoviddeath") # censor event must be first as that's how it's treated in survfit
    )
  )



# calculate standard error of cumulative incidence function non-parametrically using
# event counts over time
cif.se <- function(time, ci, n.risk, n.event, kmsurv, kmsummand){
  # from https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Cumulative_Incidence.pdf
  # also here: https://onlinelibrary.wiley.com/doi/full/10.1002/bimj.200900039?saml_referrer
  timeindex <- seq_along(time)
  sapply(timeindex, function(i) {
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


# outcome frequency
outcomes_per_treated <- table(outcome=data_matched$ind_outcome, treated=data_matched$treatment)


## redaction threshold ----

threshold <- 6

## competing risks cumulative risk differences ----

data_surv <-
  data_matched %>%
  group_by(!!subgroup_sym, treatment) %>%
  nest() %>%
  mutate(
    surv_obj = map(data, ~{
      survfit(Surv(tte_outcome, status) ~ 1, data = .x)
    }),
    surv_obj_tidy = map(surv_obj, broom::tidy), # return survival table for each day of follow up
    surv_obj_tidy = map(surv_obj_tidy, function(x){

      xx <-
        bind_cols(
          x %>% filter(state=="(s0)") %>% select(time, n.risk, n.censor),
          x %>% filter(state!="(s0)") %>% group_by(time) %>% summarise(n.allevents=sum(n.event)) %>% ungroup() %>% select(-time),
          x %>% filter(state==outcome) %>% select(-time, -n.censor, -n.risk, -state),
        ) %>%
        transmute(
          time, lagtime=lag(time,1,0), leadtime=lead(time), interval=time-lagtime,
          n.risk, n.allevents, n.event, n.censor,

          kmsummand = (1/(n.risk-n.event)) - (1/n.risk), # = n.event / ((n.risk - n.event) * n.risk) but re-written to prevent integer overflow
          kmsurv = cumprod(1 - n.event / n.risk),
          kmsurv.se = kmsurv * sqrt(cumsum(kmsummand)),

          risk = estimate,
          risk.se = std.error,
          risk.ll = conf.low,
          risk.ul = conf.high,
          surv = 1 - risk,
          surv.se = std.error,
          surv.ll = 1 - risk.ul,
          surv.ul = 1 - risk.ll
        )

      xxcomplete <-
        xx %>%
        complete(
          time = seq_len(max(xx$time)),
          fill = list(n.event = 0, n.allevents = 0, n.censor = 0)
        ) %>%
        fill(n.risk, .direction = c("up"))

      xxcomplete
    })
  ) %>%
  select(!!subgroup_sym, treatment, surv_obj_tidy) %>%
  unnest(surv_obj_tidy) %>%
  mutate(
    treatment_descr = fct_recoderelevel(as.character(treatment), recoder$treatment)
  )

data_surv_rounded <-
  data_surv %>%
  mutate(
    # Round cumulative counts up to `threshold`, then deduct half of threshold to remove bias

    N = max(n.risk, na.rm=TRUE),
    cml.compevents = roundmid_any(cumsum(n.allevents-n.event), threshold),
    cml.event = roundmid_any(cumsum(n.event), threshold),
    cml.censor = roundmid_any(cumsum(n.censor), threshold),
    cml.allevents = cml.compevents + cml.event,

    n.allevents = diff(c(0, cml.allevents)),
    n.event = diff(c(0, cml.event)),
    n.censor = diff(c(0, cml.censor)),
    n.risk = roundmid_any(N, threshold) - lag(cml.allevents + cml.censor, 1, 0),


    ## calculate surv based on rounded event counts

    # KM estimate for event of interest, combining censored and competing events as censored
    kmsummand = (1/(n.risk-n.event)) - (1/n.risk), # = n.event / ((n.risk - n.event) * n.risk) but re-written to prevent integer overflow
    kmsurv = cumprod(1 - n.event / n.risk),
    kmsurv.se = kmsurv * sqrt(cumsum(kmsummand)), #greenwood's formula
    kmsurv.ln.se = kmsurv.se/kmsurv,
    kmsurv.ll = exp(log(risk) + qnorm(0.025)*kmsurv.ln.se),
    kmsurv.ul = exp(log(risk) + qnorm(0.975)*kmsurv.ln.se),

    # CI estimate, treating comepting events as competing events
    allsummand = (1/(n.risk-n.allevents)) - (1/n.risk), # n.allevents / ((n.risk - n.allevents) * n.risk) but re-written to prevent integer overflow
    allsurv = cumprod(1 - n.allevents / n.risk),
    summand = (n.event / n.risk) * lag(allsurv, 1, 1),
    risk = cumsum(summand),
    risk.se = cif.se(time, risk, n.risk, n.event, allsurv, allsummand),
    risk.ln.se = risk.se/risk,
    risk.ll = exp(log(risk) + qnorm(0.025)*risk.ln.se),
    risk.ul = exp(log(risk) + qnorm(0.975)*risk.ln.se),

    surv = 1 - risk,
    surv.se = risk.se,
    surv.ll = 1 - risk.ul,
    surv.ul = 1 - risk.ll
  ) %>%
  select(
    !!subgroup_sym, treatment, treatment_descr, time, lagtime, leadtime, interval,
    n.risk, n.allevents, n.event, n.censor,
    kmsurv, kmsurv.se, kmsurv.ll, kmsurv.ul,
    risk, risk.se, risk.ll, risk.ul,
    surv, surv.se, surv.ll, surv.ul,
  )

write_csv(data_surv_rounded, fs::path(output_dir, "ci_estimates.csv"))

plot_ci <- data_surv %>%
  group_modify(
    ~add_row(
      .x,
      time=0,
      lagtime=0,
      leadtime=1,
      interval=1,
      kmsurv=1,
      surv=1,
      surv.ll=1,
      surv.ul=1,
      .before=0
    ) %>%
    fill(treatment_descr, .direction="up")
  ) %>%
  ggplot(aes(group=treatment_descr, colour=treatment_descr, fill=treatment_descr)) +
  geom_step(aes(x=time, y=1-surv), direction="vh")+
  geom_step(aes(x=time, y=1-kmsurv), direction="vh", linetype="dashed", alpha=0.5)+
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

plot_ci

ggsave(filename=fs::path(output_dir, "ci_plot.png"), plot_ci, width=20, height=15, units="cm")


plot_ci_rounded <- data_surv_rounded %>%
  group_modify(
    ~add_row(
      .x,
      time=0,
      lagtime=0,
      leadtime=1,
      interval=1,
      kmsurv=1,
      surv=1,
      surv.ll=1,
      surv.ul=1,
      .before=0
    ) %>%
      fill(treatment_descr, .direction="up")
  ) %>%
  ggplot(aes(group=treatment_descr, colour=treatment_descr, fill=treatment_descr)) +
  geom_step(aes(x=time, y=1-surv), direction="vh")+
  geom_step(aes(x=time, y=1-kmsurv), direction="vh", linetype="dashed", alpha=0.5)+
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

plot_ci_rounded

ggsave(filename=fs::path(output_dir, "ci_plot_rounded.png"), plot_ci_rounded, width=20, height=15, units="cm")


## calculate quantities relating to cumulative incidence curve and their ratio / difference / etc

cicontrast <- function(data, cuts=NULL){

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
      n.event, n.censor, n.allevents,

      cml.persontime = cumsum(n.atrisk*interval),
      cml.event = cumsum(replace_na(n.event, 0)),
      cml.censor = cumsum(replace_na(n.censor, 0)),

      kmrate = n.event / n.atrisk,
      cml.rate = cml.event / cml.persontime,

      kmsurv, kmsurv.se, kmsurv.ll, kmsurv.ul,
      kmrisk = 1-kmsurv, kmrisk.se = kmsurv.se, kmrisk.ll = 1-kmsurv.ul, kmrisk.ul = 1-kmsurv.ll,
      kmhaz = -(kmsurv-lag(kmsurv,1,1))/lag(kmsurv,1,1),


      surv, surv.se, surv.ll, surv.ul,
      risk, risk.se, risk.ll, risk.ul,

      ciinc = -(surv-lag(surv,1,1))/lag(surv,1,1),

      ciinc2 = diff(c(0,-log(surv)))

    ) %>%
    group_by(!!subgroup_sym, treatment, period_start, period_end, period) %>%
    summarise(

      ## time-period-specific quantities

      persontime = sum(n.atrisk*interval), # total person-time at risk within time period

      ciinc = weighted.mean(ciinc, n.atrisk*interval),
      ciinc2 = weighted.mean(ciinc2, n.atrisk*interval),

      n.atrisk = first(n.atrisk), # number at risk at start of time period
      n.event = sum(n.event, na.rm=TRUE), # number of events within time period
      n.censor = sum(n.censor, na.rm=TRUE), # number censored within time period

      kminc = n.event/persontime, # = weighted.mean(kmhaz, n.atrisk*interval), incidence rate. this is equivalent to a weighted average of the hazard ratio, with time-exposed as the weights

      interval = sum(interval), # width of time period

      ## quantities calculated from time zero until end of time period
      # these should be the same as the daily values as at the end of the time period


      kmsurv = last(surv),
      kmsurv.se = last(kmsurv.se),
      kmsurv.ll = last(kmsurv.ll),
      kmsurv.ul = last(kmsurv.ul),

      kmrisk = last(risk),
      kmrisk.se = last(kmrisk.se),
      kmrisk.ll = last(kmrisk.ll),
      kmrisk.ul = last(kmrisk.ul),

      surv = last(surv),
      surv.se = last(surv.se),
      surv.ll = last(surv.ll),
      surv.ul = last(surv.ul),

      risk = last(risk),
      risk.se = last(risk.se),
      risk.ll = last(risk.ul),
      risk.ul = last(risk.ll),

      #cml.haz = last(cml.haz),  # cumulative hazard from time zero to end of time period

      cml.rate = last(cml.rate), # event rate from time zero to end of time period

      # cml.persontime = last(cml.persontime), # total person-time at risk from time zero to end of time period
       cml.event = last(cml.event), # number of events from time zero to end of time period
      # cml.censor = last(cml.censor), # number censored from time zero to end of time period

      # cml.summand = last(cml.summand), # summand used for estimation of SE of survival

      .groups="drop"
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols= all_of(c(subgroup, "period_start", "period_end", "period",  "interval")),
      names_from=treatment,
      names_glue="{.value}_{treatment}",
      values_from=c(

        persontime, n.atrisk, n.event, n.censor,
        kminc, ciinc, ciinc2,

        kmsurv, kmsurv.se, kmsurv.ll, kmsurv.ul,
        kmrisk, kmrisk.se, kmrisk.ll, kmrisk.ul,
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
      kmirr = kminc_1 / kminc_0,
      kmirr.ln.se = sqrt((1/n.event_0) + (1/n.event_1)),
      kmirr.ll = exp(log(kmirr) + qnorm(0.025)*kmirr.ln.se),
      kmirr.ul = exp(log(kmirr) + qnorm(0.975)*kmirr.ln.se),


      # incidence rate ratio, derived from cumulative incidence, not KM
      ciirr = ciinc_1 / ciinc_0,
      ciirr.ln.se = sqrt((1/n.event_0) + (1/n.event_1)),
      ciirr.ll = exp(log(ciirr) + qnorm(0.025)*ciirr.ln.se),
      ciirr.ul = exp(log(ciirr) + qnorm(0.975)*ciirr.ln.se),

      # incidence rate ratio, derived from cumulative incidence, not KM
      ciirr2 = ciinc2_1 / ciinc2_0,
      ciirr2.ln.se = sqrt((1/n.event_0) + (1/n.event_1)),
      ciirr2.ll = exp(log(ciirr2) + qnorm(0.025)*ciirr2.ln.se),
      ciirr2.ul = exp(log(ciirr2) + qnorm(0.975)*ciirr2.ln.se),

      # incidence rate difference
      #ird = rate_1 - rate_0,

      ## quantities calculated from time zero until end of time period
      # these should be the same as values calculated on each day of follow up


      # cumulative incidence rate ratio
      cmlirr = cml.rate_1 / cml.rate_0,
      cmlirr.ln.se = sqrt((1/cml.event_0) + (1/cml.event_1)),
      cmlirr.ll = exp(log(cmlirr) + qnorm(0.025)*cmlirr.ln.se),
      cmlirr.ul = exp(log(cmlirr) + qnorm(0.975)*cmlirr.ln.se),

      # survival ratio, standard error, and confidence limits, treating cause-specific death as a competing event
      cisr = surv_1 / surv_0,
      #cisr.ln = log(cisr),
      cisr.ln.se = (surv.se_0/surv_0) + (surv.se_1/surv_1), #because cmlhaz = -log(surv) and cmlhaz.se = surv.se/surv
      cisr.ll = exp(log(cisr) + qnorm(0.025)*cisr.ln.se),
      cisr.ul = exp(log(cisr) + qnorm(0.975)*cisr.ln.se),

      # risk ratio, standard error, and confidence limits, using delta method, , treating cause-specific death as a competing event
      cirr = risk_1 / risk_0,
      #cirr.ln = log(cirr),
      cirr.ln.se = sqrt((risk.se_1/risk_1)^2 + (risk.se_0/risk_0)^2),
      cirr.ll = exp(log(cirr) + qnorm(0.025)*cirr.ln.se),
      cirr.ul = exp(log(cirr) + qnorm(0.975)*cirr.ln.se),

      # risk difference, standard error and confidence limits, , treating cause-specific death as a competing event
      cird = risk_1 - risk_0,
      cird.se = sqrt( (risk.se_0^2) + (risk.se_1^2) ),
      cird.ll = cird + qnorm(0.025)*cird.se,
      cird.ul = cird + qnorm(0.975)*cird.se,




      # survival ratio, standard error, and confidence limits, treating cause-specific death as a censoring event
      kmsr = kmsurv_1 / kmsurv_0,
      #kmsr.ln = log(kmsr),
      kmsr.ln.se = (kmsurv.se_0/kmsurv_0) + (kmsurv.se_1/kmsurv_1), #because cmlhaz = -log(surv) and cmlhaz.se = surv.se/surv
      kmsr.ll = exp(log(kmsr) + qnorm(0.025)*kmsr.ln.se),
      kmsr.ul = exp(log(kmsr) + qnorm(0.975)*kmsr.ln.se),

      # risk ratio, standard error, and confidence limits, using delta method, treating cause-specific death as a censoring event
      kmrr = kmrisk_1 / kmrisk_0,
      #kmrr.ln = log(kmrr),
      kmrr.ln.se = sqrt((kmrisk.se_1/kmrisk_1)^2 + (kmrisk.se_0/kmrisk_0)^2),
      kmrr.ll = exp(log(kmrr) + qnorm(0.025)*kmrr.ln.se),
      kmrr.ul = exp(log(kmrr) + qnorm(0.975)*kmrr.ln.se),

      # risk difference, standard error and confidence limits, treating cause-specific death as a censoring event
      kmrd = kmrisk_1 - kmrisk_0,
      kmrd.se = sqrt( (kmrisk.se_0^2) + (kmrisk.se_1^2) ),
      kmrd.ll = kmrd + qnorm(0.025)*kmrd.se,
      kmrd.ul = kmrd + qnorm(0.975)*kmrd.se,



      # cumulative incidence rate difference
      #cmlird = cml.rate_1 - cml.rate_0
    )
}

#ci_contrasts_daily <- cicontrast(data_surv)
#ci_contrasts_cuts <- cicontrast(data_surv, postbaselinecuts)
#ci_contrasts_overall <- cicontrast(data_surv, c(0,maxfup))


ci_contrasts_rounded_daily <- cicontrast(data_surv_rounded)
ci_contrasts_rounded_cuts <- cicontrast(data_surv_rounded, postbaselinecuts)
ci_contrasts_rounded_overall <- cicontrast(data_surv_rounded, c(0,maxfup))







## period-specific Cox models ----
# note there is no rounding or redaction applied here because only HRs are reported and these are safe statistics
# they will be Inf or zero or NaN if there are no events in one or more treatment groups,
# but underlying counts are not recoverable if non-degenerate
# because there is no rounding, there may be some minor inconsistencies with nonparametric estimates above

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
      period_start = as.integer(cuts[period_id]),
      period_end = as.integer(cuts[period_id+1]),
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
      coxhr = exp(estimate),
      coxhr.se = robust.se,
      coxhr.ll = exp(estimate + qnorm(0.025)*robust.se),
      coxhr.ul = exp(estimate + qnorm(0.975)*robust.se),
    )
  data_cox

}

cox_contrasts_cuts <- coxcontrast(data_matched, postbaselinecuts)
cox_contrasts_overall <- coxcontrast(data_matched, c(0,maxfup))


# cox HR is a safe statistic so no need to redact/round
contrasts_rounded_daily <-  ci_contrasts_rounded_daily # don't bother with cox as HR within daily intervals will be imprecisely estimated
contrasts_rounded_cuts <-  left_join(ci_contrasts_rounded_cuts, cox_contrasts_cuts, by=c(subgroup, "period_start", "period_end"))
contrasts_rounded_overall <-  left_join(ci_contrasts_rounded_overall, cox_contrasts_overall, by=c(subgroup, "period_start", "period_end"))


write_csv(contrasts_rounded_daily, fs::path(output_dir, "contrasts_daily.csv"))
write_csv(contrasts_rounded_cuts, fs::path(output_dir, "contrasts_cuts.csv"))
write_csv(contrasts_rounded_overall, fs::path(output_dir, "contrasts_overall.csv"))




