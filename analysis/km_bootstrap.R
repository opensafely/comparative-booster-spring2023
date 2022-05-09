
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
library("furrr")

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
    #all_of(matching_variables[[matchset]]$all), # model formula with all variables
    #all_of(all.vars(formula_allcovariates)), # model formula with all variables
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

# outcome frequency
outcomes_per_treated <- table(outcome=data_matched$ind_outcome, treated=data_matched$treatment)


## redaction threshold ----

threshold <- 7


## get bootstrap samples ----

quantile_bs <- partial(quantile, na.rm = TRUE, names=FALSE)

boot_n <-
  if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){
    100L
  } else {
    500L
  }

# look-up for bootstrap samples ----
# boot_id = 0 is the unsampled (ie, sample everyone once) id list

boot_samples <- read_rds(here("output", "match", matchset, "boot_samples.rds")) %>%
  filter(boot_id <= boot_n) %>%
  bind_rows(
    data_matched %>% transmute(boot_id=0L, match_id),
    .
  )

boot_ids <- unique(boot_samples$boot_id)

## calculate confidence limits with boot strapping ----

### parallelisation preliminaries ----

library("doParallel")

# how many cores available?
#parallel::detectCores()

n_threads <- 8

cluster <- parallel::makeCluster(
  n_threads,
  type = "PSOCK" # this should work across multi-core windows or linux machines
)
#print(cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = cluster)

#### survival table in parallel ----
data_surv_boot <-
  foreach(
    boot_id = boot_ids,
    .combine = 'bind_rows',
    .packages = c("dplyr", "tibble", "tidyr", "purrr", "lubridate", "survival")
  ) %dopar% {
    boot_id0 <- boot_id

    data_surv <-
      boot_samples %>%
      filter(boot_id==boot_id0) %>%
      left_join(data_matched, by="match_id") %>%
      group_by(boot_id, !!subgroup_sym, treatment) %>%
      nest() %>%
      mutate(
        n_events = map_int(data, ~sum(.x$ind_outcome, na.rm=TRUE)),
        surv_obj = map(data, ~{
          survfit(Surv(tte_outcome, ind_outcome) ~ 1, data = .x, conf.type="log-log")
        }),
        surv_obj_tidy = map(surv_obj, ~tidy_surv(.x, times=seq_len(maxfup))), # return survival table for each day of follow up -- this is important to avoid some events times dropping during the bootstrap
      ) %>%
      select(boot_id, !!subgroup_sym, treatment, n_events, surv_obj_tidy) %>%
      unnest(surv_obj_tidy) %>%
      mutate(
        treatment_descr = fct_recoderelevel(as.character(treatment), recoder$treatment)
      )

    data_surv
  }

parallel::stopCluster(cl = cluster)

data_surv_boot_CL <-
  data_surv_boot %>%
  filter(boot_id != 0) %>%
  group_by(!!subgroup_sym, treatment, time) %>%
  summarise(
    surv.median = quantile_bs(surv, 0.5),
    surv.ll = quantile_bs(surv, 0.025),
    surv.ul = quantile_bs(surv, 0.975),
    surv.se = sd(surv),
    haz.median = quantile_bs(haz, 0.5),
    haz.ll = quantile_bs(haz, 0.025),
    haz.ul = quantile_bs(haz, 0.975),
    haz.se = sd(haz),
    cml.haz.median = quantile_bs(cml.haz, 0.5),
    cml.haz.ll = quantile_bs(cml.haz, 0.025),
    cml.haz.ul = quantile_bs(cml.haz, 0.975),
    cml.haz.se = sd(cml.haz, 0.975),
    .groups= "drop"
  )

## join complete survival estimates with bootstrap CIs ----

data_surv <-
  left_join(
    # point estimates
    data_surv_boot %>%
      ungroup() %>%
    filter(boot_id==0L) %>%
    select(
      !!subgroup_sym, treatment, treatment_descr,
      time, lagtime, interval,
      n.risk, n.event, n.censor, surv, haz, cml.haz
    ),
    # booststrap CLs
    data_surv_boot_CL %>% select(
      !!subgroup_sym, treatment, time,
      surv.ll, surv.ul, surv.se,
      haz.ll, haz.ul, haz.se,
      cml.haz.ll, cml.haz.ul, cml.haz.se
    ),
    by=c(subgroup, "treatment", "time")
  )


## round estimates for disclosure control ----
data_surv_rounded <-
  data_surv %>%
  group_by(!!subgroup_sym, treatment) %>%
  mutate(
    # Use ceiling not round. This is slightly biased upwards,
    # but means there's no disclosure risk at the boundaries (0 and 1) where masking would otherwise be threshold/2
    #
    # Explanation:
    # ensure every "step" in the KM survival curve is based on no fewer than `threshold` outcome+censoring events
    # max(n.risk, na.rm=TRUE) is the number at risk at time zero.
    # max(n.risk, na.rm=TRUE)/threshold is the inverse of the minimum `step` size on the survival scale (0-1)
    # floor(max(n.risk, na.rm=TRUE)/threshold) rounds down to nearest integer.
    # 1/floor(max(n.risk, na.rm=TRUE)) is the minimum step size on the survival scale (0-1), ensuring increments no fewer than `threshold` on the events scale
    # ceiling_any(x, min_increment) rounds up values of x on the survival scale, so that they lie on the grid of width `min_increment`.

    surv = ceiling_any(surv, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
    surv.ll = ceiling_any(surv.ll, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
    surv.ul = ceiling_any(surv.ul, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
    cml.event = ceiling_any(cumsum(replace_na(n.event, 0)), threshold),
    cml.censor = ceiling_any(cumsum(replace_na(n.censor, 0)), threshold),
    n.event = diff(c(0,cml.event)),
    n.censor = diff(c(0,cml.censor)),
    n.risk = ceiling_any(max(n.risk, na.rm=TRUE), threshold) - lag(cml.event + cml.censor,1,0),
    haz = n.event / (n.risk * interval),
    cml.haz = cumsum(haz),
  ) %>%
  select(!!subgroup_sym, treatment, treatment_descr, time, lagtime, interval, n.risk, n.event, n.censor, surv, surv.se, surv.ll, surv.ul, haz, cml.haz)


write_csv(data_surv_rounded, fs::path(output_dir, "km_estimates.csv"))

plot_km <- data_surv_rounded %>%
  ggplot(aes(group=treatment_descr, colour=treatment_descr, fill=treatment_descr)) +
  geom_step(aes(x=time, y=1-surv), direction = "vh")+
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


## calculate quantities relating to kaplan-meier curve and their ratio / difference / etc

kmcontrast <- function(data_surv_boot, cuts=NULL, round_values=FALSE){

  if(is.null(cuts)){cuts <- unique(c(0,data_surv_boot$time))}

  data_contrast_boot <-
    data_surv_boot %>%
    group_by(boot_id, !!subgroup_sym, treatment) %>%
    {if(round_values==FALSE) . else {
        mutate(
          # round KM survival estimates (as described above) and base all contrasts on these values
          .,
          cml.event = ceiling_any(cumsum(replace_na(n.event, 0)), round_values),
          cml.censor = ceiling_any(cumsum(replace_na(n.censor, 0)), round_values),
          n.event = diff(c(0,cml.event)),
          n.censor = diff(c(0,cml.censor)),
          n.risk = ceiling_any(max(n.risk, na.rm=TRUE), round_values) - lag(cml.event + cml.censor,1,0),
        )
    }
    } %>%
    transmute(
      boot_id,
      !!subgroup_sym,
      treatment,

      time, lagtime, interval,
      period_start = as.integer(as.character(cut(time, cuts, right=TRUE, label=cuts[-length(cuts)]))),
      period_end = as.integer(as.character(cut(time, cuts, right=TRUE, label=cuts[-1]))),
      period = paste0(period_start, "-", period_end),

      n.atrisk = n.risk,
      n.event,
      n.censor,

      cml.persontime = cumsum(n.atrisk*interval),
      cml.event = cumsum(replace_na(n.event, 0)),
      cml.censor = cumsum(replace_na(n.censor, 0)),

      surv = cumprod(1 - n.event / n.atrisk),
      risk = 1-surv,

      haz = n.event / (n.atrisk * interval),
      cml.haz = cumsum(haz),

      cml.persontime = cumsum(n.atrisk*interval),
      rate = n.event / (n.atrisk*interval),
      cml.rate = cml.event / cml.persontime,

    ) %>%
    group_by(boot_id, !!subgroup_sym, treatment, period_start, period_end, period) %>%
    summarise(

      ## time-period-specific quantities
      ## as defined by period_start and period_end

      persontime = sum(n.atrisk*interval), # total person-time at risk within time period

      n.atrisk = first(n.atrisk), # number at risk at start of time period
      n.event = sum(n.event, na.rm=TRUE), # number of events within time period
      n.censor = sum(n.censor, na.rm=TRUE), # number censored within time period

      rate = n.event/persontime, # = weighted.mean(haz, n.atrisk*interval), incidence rate. this is equivalent to a weighted average of the hazard ratio, with time-exposed as the weights

      interval = sum(interval), # width of time period

      ## quantities calculated from time zero until end of time period
      ## these should be the same as the daily values as at the end of the time period

      surv = last(surv),
      risk = last(risk),

      cml.haz = last(cml.haz),  # cumulative hazard from time zero to end of time period

      cml.rate = last(cml.rate), # event rate from time zero to end of time period

      # cml.persontime = last(cml.persontime), # total person-time at risk from time zero to end of time period
       cml.event = last(cml.event), # number of events from time zero to end of time period
      # cml.censor = last(cml.censor), # number censored from time zero to end of time period

      # cml.summand = last(cml.summand), # summand used for estimation of SE of survival

      .groups="drop"
    ) %>%
    pivot_wider(
      id_cols= all_of(c("boot_id", subgroup, "period_start", "period_end", "period",  "interval")),
      names_from=treatment,
      names_glue="{.value}_{treatment}",
      values_from=c(

        persontime, n.atrisk, n.event, n.censor,
        rate,

        cml.haz,
        surv, risk,
        cml.event, cml.rate
        )
    ) %>%
    mutate(

      ## time-period-specific quantities

      # incidence rate ratio (equivalent to hazard ratio)
      irr = rate_1 / rate_0,

      # incidence rate difference
      ird = rate_1 - rate_0,

      ## quantities calculated from time zero until end of time period
      # these should be the same as values calculated on each day of follow up
      #eg compare final values in "daily" dataset with final values in "cut" or "overall" dataset and they should be identical

      # survival ratio, standard error, and confidence limits
      kmsr = surv_1 / surv_0,

      # risk ratio, standard error, and confidence limits
      kmrr = risk_1 / risk_0,

      # risk difference, standard error and confidence limits
      kmrd = risk_1 - risk_0,

      # cumulative incidence rate ratio
      cmlirr = cml.rate_1 / cml.rate_0,

      # cumulative incidence rate difference
      cmlird = cml.rate_1 - cml.rate_0
    )

  data_contrast_CL <-
    data_contrast_boot %>%
    filter(boot_id!=0) %>%
    group_by(!!subgroup_sym, period_start, period_end, period, interval) %>%
    summarise(

      surv.median_0 = quantile_bs(surv_0, 0.5),
      surv.ll_0 = quantile_bs(surv_0, 0.025),
      surv.ul_0 = quantile_bs(surv_0, 0.975),
      surv.se_0 = sd(surv_0),

      surv.median_1 = quantile_bs(surv_1, 0.5),
      surv.ll_1 = quantile_bs(surv_1, 0.025),
      surv.ul_1 = quantile_bs(surv_1, 0.975),
      surv.se_1 = sd(surv_1),

      irr.median = quantile_bs(irr, 0.5),
      irr.ll = quantile_bs(irr, 0.025),
      irr.ul = quantile_bs(irr, 0.975),
      irr.se = sd(irr),

      ird.median = quantile_bs(ird, 0.5),
      ird.ll = quantile_bs(ird, 0.025),
      ird.ul = quantile_bs(ird, 0.975),
      ird.se = sd(ird),

      kmsr.median = quantile_bs(kmsr, 0.5),
      kmsr.ll = quantile_bs(kmsr, 0.025),
      kmsr.ul = quantile_bs(kmsr, 0.975),
      kmsr.se = sd(kmsr),

      kmrr.median = quantile_bs(kmrr, 0.5),
      kmrr.ll = quantile_bs(kmrr, 0.025),
      kmrr.ul = quantile_bs(kmrr, 0.975),
      kmrr.se = sd(kmrr),

      kmrd.median = quantile_bs(kmrd, 0.5),
      kmrd.ll = quantile_bs(kmrd, 0.025),
      kmrd.ul = quantile_bs(kmrd, 0.975),
      kmrd.se = sd(kmrd),

      cmlirr.median = quantile_bs(cmlirr, 0.5),
      cmlirr.ll = quantile_bs(cmlirr, 0.025),
      cmlirr.ul = quantile_bs(cmlirr, 0.975),
      cmlirr.se = sd(cmlirr),

      cmlird.median = quantile_bs(cmlird, 0.5),
      cmlird.ll = quantile_bs(cmlird, 0.025),
      cmlird.ul = quantile_bs(cmlird, 0.975),
      cmlird.se = sd(cmlird),

      .groups="drop"
    )

    left_join(
      data_contrast_boot %>% filter(boot_id==0),
      data_contrast_CL,
      by=c(subgroup, "period_start", "period_end", "period", "interval")
    )

}

km_contrasts_daily <- kmcontrast(data_surv_boot, cuts=NULL, round_values=FALSE)
km_contrasts_cuts <- kmcontrast(data_surv_boot, cuts=postbaselinecuts, round_values=FALSE)
km_contrasts_overall <- kmcontrast(data_surv_boot, cuts=c(0,maxfup), round_values=FALSE)

km_contrasts_rounded_daily <- kmcontrast(data_surv_boot, cuts=NULL, round_values=threshold)
km_contrasts_rounded_cuts <- kmcontrast(data_surv_boot, cuts=postbaselinecuts, round_values=threshold)
km_contrasts_rounded_overall <- kmcontrast(data_surv_boot, cuts=c(0,maxfup), round_values=threshold)

write_csv(km_contrasts_rounded_daily, fs::path(output_dir, "contrasts_daily.csv"))
write_csv(km_contrasts_rounded_cuts, fs::path(output_dir, "contrasts_cuts.csv"))
write_csv(km_contrasts_rounded_overall, fs::path(output_dir, "contrasts_overall.csv"))


## Cox models ----

# Not done, because it will be slow
# use incidence rate ratio (IRR) instead, which is similar
# The Cox model assumes proportional hazards within each follow-up period, so the "shape" (in some sense) of the hazard across treatment groups is the same
# The IRR makes no such assumption, instead it just calculates the person-time-weighted hazard within each follow up period and takes the ratio
