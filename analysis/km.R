
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
    surv_obj_tidy = map(surv_obj, ~tidy_surv(.x, times=seq_len(maxfup))),
  ) %>%
  select(!!subgroup_sym, treatment, n_events, surv_obj_tidy) %>%
  unnest(surv_obj_tidy) %>%
  #ungroup() %>%
  mutate(
    treatment_descr = fct_recode(as.character(treatment), !!!recoder$treatment)
  )

data_surv_rounded <-
  data_surv %>%
  mutate(
    # Use ceiling not round. This is slightly biased upwards,
    # but means there's no disclosure risk at the boundaries (0 and 1) where masking would otherwise be threshold/2
    surv = ceiling_any(surv, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
    surv.ll = ceiling_any(surv.ll, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
    surv.ul = ceiling_any(surv.ul, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
    cml.event = ceiling_any(cumsum(replace_na(n.event, 0)), threshold),
    cml.censor = ceiling_any(cumsum(replace_na(n.censor, 0)), threshold),
    n.event = c(NA, diff(cml.event)),
    n.censor = c(NA, diff(cml.censor)),
    n.risk = lag(ceiling_any(max(n.risk, na.rm=TRUE), threshold) - (cml.event + cml.censor)),
    sumerand = n.event / ((n.risk - n.event) * n.risk),
    surv.se = surv * sqrt(cumsum(sumerand)),
  ) %>%
  select(!!subgroup_sym, treatment, treatment_descr, time, leadtime, interval, surv, surv.se, surv.ll, surv.ul, n.risk, n.event, n.censor, sumerand)


write_csv(data_surv_rounded, fs::path(output_dir, "km_estimates.csv"))

plot_km <- data_surv_rounded %>%
  ggplot(aes(group=treatment_descr, colour=treatment_descr, fill=treatment_descr)) +
  geom_step(aes(x=time, y=1-surv))+
  geom_rect(aes(xmin=time, xmax=leadtime, ymin=1-surv.ll, ymax=1-surv.ul), alpha=0.1, colour="transparent")+
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



## incidence via km risk table ----

km_incidence <-
  data_surv %>%
  mutate(
    n.atrisk = n.risk,
    cml.atrisk = cumsum(replace_na(n.atrisk, 0)),
    cml.event = cumsum(replace_na(n.event, 0)),
    cml.censor = cumsum(replace_na(n.censor, 0)),
    cml.sumerand = cumsum(sumerand),
    rate = n.event / n.atrisk,
    cml.rate = cml.event / cml.atrisk,

    risk = 1 - surv,
    risk.ll = 1 - surv.ul,
    risk.ul = 1 - surv.ll
  ) %>%
  select(
    !!subgroup_sym,
    treatment,
    time, interval,
    surv, surv.se, surv.ll, surv.ul, risk, risk.ll, risk.ul, n.atrisk, n.event, n.censor, sumerand, rate, cml.atrisk, cml.event, cml.censor, cml.sumerand, cml.rate
  )



kmcontrast <- function(data, cuts=NULL){

  if(is.null(cuts)){cuts <- unique(c(0,data$time))}

  data %>%
    filter(time!=0) %>%
    mutate(
      period_start = cut(time, cuts, right=TRUE, label=cuts[-length(cuts)]),
      period_end = cut(time, cuts, right=TRUE, label=cuts[-1]),
      period = cut(time, cuts, right=TRUE, label=paste0(cuts[-length(cuts)]+1, " - ", cuts[-1]))
    ) %>%
    group_by(!!subgroup_sym, treatment, period_start, period_end, period) %>%
    summarise(
      interval = last(time) - first(time) + 1,
      cml.atrisk = last(cml.atrisk),
      cml.event = last(cml.event),
      cml.censor = last(cml.censor),
      cml.rate = last(cml.rate),
      cml.sumerand = last(cml.sumerand),
      persontime = sum(n.atrisk),
      n.atrisk = first(n.atrisk),
      n.event = sum(n.event, na.rm=TRUE),
      n.censor = sum(n.censor, na.rm=TRUE),
      sumerand = sum(sumerand),
      surv = last(surv),
      surv.se = surv * sqrt(cml.sumerand), #greenwood standard error
      surv.ll = last(surv.ll),
      surv.ul = last(surv.ul),
      risk = last(risk),
      risk.ll = last(risk.ul),
      risk.ul = last(risk.ll),
      rate = n.event/persontime,
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols= c(subgroup, "period_start", "period_end", "period",  "interval"),
      names_from=treatment,
      names_glue="{.value}_{treatment}",
      values_from=c(surv, surv.se, surv.ll, surv.ul, risk, risk.ll, risk.ul, n.atrisk, n.event, n.censor, rate, cml.atrisk, cml.event, cml.censor, cml.rate)
    ) %>%
    mutate(
      n.nonevent_0 = n.atrisk_0 - n.event_0,
      n.nonevent_1 = n.atrisk_1 - n.event_1,

      # relative risk, standard error, and confidence limits
      kmrr = risk_1 / risk_0,
        # ignoring censoring
      # kmlnrr.se = sqrt( (1/n.event_1) +  (1/n.event_0) - (1/(n.atrisk_1)) - (1/(n.atrisk_0))),
      # kmrr.ll = exp(log(kmrr) + qnorm(0.025)*kmlnrr.se),
      # kmrr.ul = exp(log(kmrr) + qnorm(0.975)*kmlnrr.se),

      # risk difference, standard error and confidence limits
      kmrd = risk_1 - risk_0,
      #kmrd.se = sqrt( ((n.event_1*n.nonevent_1)/(n.atrisk_1^3)) + ((n.event_0*n.nonevent_0)/(n.atrisk_0^3)) ), # ignores censoring
      kmrd.se = sqrt( (surv.se_0^2) + (surv.se_1^2) ), # combining SEs from greenwood's formula
      kmrd.ll = kmrd + qnorm(0.025)*kmrd.se,
      kmrd.ul = kmrd + qnorm(0.975)*kmrd.se,

      # incidence rate ratio
      irr = rate_1 / rate_0,

      # incidence rate difference
      ird = rate_1 - rate_0,

      # cumulative incidence rate ratio
      cmlirr = cml.rate_1 / cml.rate_0,

      # cumulative incidence rate difference
      cmlird = cml.rate_1 - cml.rate_0
    )
}

km_contrasts_daily <- kmcontrast(km_incidence)
km_contrasts_cuts <- kmcontrast(km_incidence, postbaselinecuts)
km_contrasts_overall <- kmcontrast(km_incidence, c(0,maxfup))

write_csv(km_contrasts_daily, fs::path(output_dir, "km_contrasts_daily.csv"))
write_csv(km_contrasts_overall, fs::path(output_dir, "km_contrasts_overall.csv"))
