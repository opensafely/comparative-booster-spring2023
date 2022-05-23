
# # # # # # # # # # # # # # # # # # # # #
# This script:
# imports fitted cox models
# outputs time-dependent effect estimates for booster
#
# The script must be accompanied by three arguments:
# `treatment` - the exposure variable in the regression model
# `outcome` - the dependent variable in the regression model
# `subgroup` - the subgroup variable for the regression model followed by a hyphen and the level of the subgroup
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  removeobs <- FALSE
  treatment <- "pfizer"
  outcome <- "postest"
  subgroup <- "none"
} else {
  removeobs <- TRUE
  treatment <- args[[1]]
  outcome <- args[[2]]
  subgroup <- args[[3]]
}


## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')
library('cmprsk')

## Import custom user functions
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "survival.R"))
source(here("lib", "functions", "redaction.R"))

output_dir_matched <- here("output", "match", treatment)
output_dir <- here("output", "models", "seqtrialcox", treatment, outcome, subgroup)

data_seqtrialcox <- read_rds(fs::path(output_dir, "model_data_seqtrialcox.rds"))

## import globally defined study dates and convert to "Date"
study_dates <-
  jsonlite::read_json(path=here("lib", "design", "study-dates.json")) %>%
  map(as.Date)


# report model info ----

## report model diagnostics ----

fs::file_copy(fs::path(output_dir, "model_glance.csv"), fs::path(output_dir, "report_glance.csv"), overwrite = TRUE)


## report model effects ----

postbaselinecuts <- read_rds(here("lib", "design", "postbaselinecuts.rds"))

model_tidy <- read_csv(fs::path(output_dir, "model_tidy.csv"))

model_effects <- model_tidy %>%
  filter(str_detect(term, fixed("treated"))) %>%
  mutate(
    period_id = str_replace(term, pattern=fixed("treated_period_"), ""),
    term_left = postbaselinecuts[as.numeric(period_id)],
    term_right = postbaselinecuts[as.numeric(period_id)+1],
    term_midpoint = (term_right+term_left)/2,
    model_descr = fct_inorder(model_descr)
  )
write_csv(model_effects, path = fs::path(output_dir, "report_effects.csv"))

plot_effects <-
  ggplot(data = model_effects) +
  geom_point(aes(y=exp(estimate), x=term_midpoint, colour=model_descr), position = position_dodge(width = 1.8))+
  geom_linerange(aes(ymin=exp(conf.low), ymax=exp(conf.high), x=term_midpoint, colour=model_descr), position = position_dodge(width = 1.8))+
  geom_hline(aes(yintercept=1), colour='grey')+
  scale_y_log10(
    breaks=c(0.01, 0.02, 0.05, 0.2, 0.1, 0.5, 1, 2),
    sec.axis = dup_axis(name="<--  favours not boosting  /  favours boosting  -->", breaks = NULL)
  )+
  scale_x_continuous(breaks=postbaselinecuts, limits=c(min(postbaselinecuts), max(postbaselinecuts)+1), expand = c(0, 0))+
  scale_colour_brewer(type="qual", palette="Set2", guide=guide_legend(ncol=1))+
  labs(
    y="Hazard ratio",
    x="Days since booster dose",
    colour=NULL
   ) +
  theme_bw()+
  theme(
    panel.border = element_blank(),
    axis.line.y = element_line(colour = "black"),

    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0),

    panel.spacing = unit(0.8, "lines"),

    plot.title = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, face= "italic"),

    legend.position = "bottom"
   ) +
 NULL
plot_effects
## save plot

ggsave(filename=fs::path(output_dir, "report_effectsplot.svg"), plot_effects, width=20, height=15, units="cm")
ggsave(filename=fs::path(output_dir, "report_effectsplot.png"), plot_effects, width=20, height=15, units="cm")



## overall effects ----

model_overalltidy <- read_csv(fs::path(output_dir, "model_overalltidy.csv"))
model_overalleffects <- model_overalltidy %>%
  filter(str_detect(term, fixed("treated"))) %>%
  mutate(model_descr = fct_inorder(model_descr))

write_csv(model_overalleffects, path = fs::path(output_dir, "report_overalleffects.csv"))



## meta analysis of period-specific hazards ----

model_metaeffects <- model_effects %>%
  group_by(
    model, model_descr
  ) %>%
  summarise(
    estimate = weighted.mean(estimate, robust.se^-2),
    std.error = sqrt(1/sum(std.error^-2)),
    robust.se = sqrt(1/sum(robust.se^-2)),
    statistic = estimate/robust.se,
    p.value = 2 * pmin(pnorm(statistic), pnorm(-statistic)),
    conf.low = estimate + qnorm(0.025)*robust.se,
    conf.high = estimate + qnorm(0.975)*robust.se,
  )

write_csv(model_metaeffects, path = fs::path(output_dir, "report_metaeffects.csv"))



## meta analysis of period-specific hazards ----

model_meta2effects <- model_effects %>%
  mutate(
    period = (term_right>28) + 1,
  ) %>%
  group_by(
    period, model, model_descr,
  ) %>%
  summarise(
    term_left = min(term_left),
    term_right = max(term_right),
    estimate = weighted.mean(estimate, robust.se^-2),
    std.error = sqrt(1/sum(std.error^-2)),
    robust.se = sqrt(1/sum(robust.se^-2)),
    statistic = estimate/robust.se,
    p.value = 2 * pmin(pnorm(statistic), pnorm(-statistic)),
    conf.low = estimate + qnorm(0.025)*robust.se,
    conf.high = estimate + qnorm(0.975)*robust.se,
  ) %>%
  mutate(
    term_midpoint = (term_left+term_right)/2,
  )

write_csv(model_meta2effects, path = fs::path(output_dir, "report_meta2effects.csv"))


## incidence rates ----

### Event incidence following recruitment

rrCI_exact <- function(n, pt, ref_n, ref_pt, accuracy=0.001){

  # use exact methods if incidence is very low for immediate post-vaccine outcomes
  rate <- n/pt
  ref_rate <- ref_n/ref_pt
  rr <- rate/ref_rate

  ll = ref_pt/pt * (n/(ref_n+1)) * 1/qf(2*(ref_n+1), 2*n, p = 0.05/2, lower.tail = FALSE)
  ul = ref_pt/pt * ((n+1)/ref_n) * qf(2*(n+1), 2*ref_n, p = 0.05/2, lower.tail = FALSE)

  paste0("(", scales::number_format(accuracy=accuracy)(ll), "-", scales::number_format(accuracy=accuracy)(ul), ")")

}

format_ratio = function(numer,denom, width=7){
  paste0(
    replace_na(scales::comma_format(accuracy=1)(numer), "--"),
    " /",
    str_pad(replace_na(scales::comma_format(accuracy=1)(denom),"--"), width=width, pad=" ")
  )
}

threshold <- 5

local({

  incidence_treated <-
    data_seqtrialcox %>%
    group_by(treated, fup_period) %>%
    summarise(
      n = n_distinct(patient_id),
      yearsatrisk = sum(tstop-tstart)/365.25,
      events = sum(ind_outcome),
      rate = events/yearsatrisk
    ) %>%
    ungroup()


  incidence_all <-
    data_seqtrialcox %>%
    group_by(treated) %>%
    summarise(
      n = n_distinct(patient_id),
      yearsatrisk = sum(tstop-tstart)/365.25,
      events = sum(ind_outcome),
      rate = events/yearsatrisk
    ) %>%
    ungroup()

  incidence <-
    bind_rows(
      incidence_treated,
      incidence_all
    ) %>%
    mutate(
      fup_period = fct_explicit_na(fup_period, na_level="All")
    ) %>%
    arrange(
      treated, fup_period
    )

  incidence_rate_unrounded <<-
    incidence %>%
    pivot_wider(
      id_cols =c(fup_period),
      names_from = treated,
      values_from = c(n, yearsatrisk, events, rate),
      names_glue = "{.value}_{treated}"
    ) %>%
    mutate(
      rr = rate_1 / rate_0,
      rrE = scales::label_number(accuracy=0.01, trim=FALSE)(rr),
      rrCI = rrCI_exact(events_1, yearsatrisk_1, events_0, yearsatrisk_0, 0.01),
      rrECI = paste0(rrE, " ", rrCI)
    )

  incidence_rate_rounded <<-
    incidence_rate_unrounded %>%
    mutate(
      n_0 = ceiling_any(n_0, threshold+1),
      n_1 = ceiling_any(n_1, threshold+1),

      rate_0 = events_0/yearsatrisk_0,
      rate_1 = events_1/yearsatrisk_1,

      rr =  rate_1 / rate_0,
      rrE = scales::label_number(accuracy=0.01, trim=FALSE)(rr),
      rrCI = rrCI_exact(events_1, yearsatrisk_1, events_0, yearsatrisk_0, 0.01),
      rrECI = paste0(rrE, " ", rrCI),

      events_0 = ceiling_any(events_0, threshold+1),
      events_1 = ceiling_any(events_1, threshold+1),

      q_0 = format_ratio(events_0, yearsatrisk_0),
      q_1 = format_ratio(events_1, yearsatrisk_1),
    )

})

write_csv(incidence_rate_rounded, fs::path(output_dir, "report_incidence.csv"))


## kaplan meier cumulative risk differences ----


data_surv <-
  data_seqtrialcox %>%
  group_by(patient_id, treated) %>%
  summarise(
    tte_outcome = max(tstop) - min(tstart),
    ind_outcome = as.integer(last(ind_outcome))
  ) %>%
  group_by(treated) %>%
  nest() %>%
  mutate(
    n_events = map_int(data, ~sum(.x$ind_outcome, na.rm=TRUE)),
    surv_obj = map(data, ~{
      survfit(Surv(tte_outcome, ind_outcome) ~ 1, data = .x, conf.type="log-log")
    }),
    surv_obj_tidy = map(surv_obj, ~tidy_surv(.x, addtimezero = TRUE)),
  ) %>%
  select(treated, n_events, surv_obj_tidy) %>%
  unnest(surv_obj_tidy) %>%
  mutate(
    treated_descr = if_else(treated==1L, "Boosted", "Unboosted"),
  )

data_surv_rounded <-
  data_surv %>%
  mutate(
    # Use ceiling not round. This is slightly biased upwards,
    # but means there's no disclosure risk at the boundaries (0 and 1) where masking would otherwise be threshold/2
    surv = ceiling_any(surv, 1/floor(max(n.risk, na.rm=TRUE)/(threshold+1))),
    surv.ll = ceiling_any(surv.ll, 1/floor(max(n.risk, na.rm=TRUE)/(threshold+1))),
    surv.ul = ceiling_any(surv.ul, 1/floor(max(n.risk, na.rm=TRUE)/(threshold+1))),
    cml.event = ceiling_any(cumsum(replace_na(n.event, 0)), threshold+1),
    cml.censor = ceiling_any(cumsum(replace_na(n.censor, 0)), threshold+1),
    n.event = c(NA, diff(cml.event)),
    n.censor = c(NA, diff(cml.censor)),
    n.risk = lag(ceiling_any(max(n.risk, na.rm=TRUE), threshold+1) - (cml.event + cml.censor))
  ) %>%
  select(treated, treated_descr, time, interval, surv, surv.ll, surv.ul, n.risk, n.event, n.censor)


write_csv(data_surv_rounded, fs::path(output_dir, "report_km.csv"))

plot_km <- data_surv_rounded %>%
  ggplot(aes(group=treated_descr, colour=treated_descr, fill=treated_descr)) +
  geom_step(aes(x=time, y=1-surv))+
  geom_rect(aes(xmin=time, xmax=time+1, ymin=1-surv.ll, ymax=1-surv.ul), alpha=0.1, colour="transparent")+
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

ggsave(filename=fs::path(output_dir, "report_kmplot.svg"), plot_km, width=20, height=15, units="cm")
ggsave(filename=fs::path(output_dir, "report_kmplot.png"), plot_km, width=20, height=15, units="cm")



## incidence via km risk table ----

km_incidence <-
  data_surv %>%
  mutate(
    n.atrisk = n.risk,
    cml.atrisk = cumsum(replace_na(n.atrisk, 0)),
    cml.event = cumsum(replace_na(n.event, 0)),
    cml.censor = cumsum(replace_na(n.censor, 0)),
    rate = n.event/n.atrisk,
    cml.rate = cml.event / cml.atrisk,
    risk = 1 - surv,
  ) %>%
  select(
    treated,
    time, interval,
    surv, risk, n.atrisk,  n.event, n.censor, rate, cml.atrisk, cml.event, cml.censor, cml.rate
  )

kmdiff <- function(data, cuts=NULL){

  if(is.null(cuts)){cuts <- unique(data$time)}

  data %>%
    filter(time!=0) %>%
    mutate(
      period_start = cut(time, cuts, right=TRUE, label=cuts[-length(cuts)]),
      period_end = cut(time, cuts, right=TRUE, label=cuts[-1]),
      period = cut(time, cuts, right=TRUE, label=paste0(cuts[-length(cuts)]+1, " - ", cuts[-1]))
    ) %>%
    group_by(treated, period_start, period_end, period) %>%
    summarise(
      interval = last(time) - first(time) + 1,
      cml.atrisk = last(cml.atrisk),
      cml.event = last(cml.event),
      cml.censor = last(cml.censor),
      cml.rate = last(cml.rate),
      persontime = sum(n.atrisk),
      n.atrisk = first(n.atrisk),
      n.event = sum(n.event, na.rm=TRUE),
      n.censor = sum(n.censor, na.rm=TRUE),
      surv = last(surv),
      risk = last(risk),
      rate = n.event/persontime,
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols= c("period_start", "period_end", "period",  "interval"),
      names_from=treated,
      names_glue="{.value}_{treated}",
      values_from=c(surv, risk, n.atrisk, n.event, n.censor, rate, cml.atrisk, cml.event, cml.censor, cml.rate)
    ) %>%
    mutate(
      kmrr = risk_1 / risk_0,
      kmrd = risk_1 - risk_0,
      irr = rate_1 / rate_0,
      ird = rate_1 - rate_0,
      cmlirr = cml.rate_1 / cml.rate_0,
      cmlird = cml.rate_1 - cml.rate_0
    )
}

kmdiff0 <- kmdiff(km_incidence)
kmdiffcuts <- kmdiff(km_incidence, postbaselinecuts)
kmdiffall <- kmdiff(km_incidence, c(0,last(postbaselinecuts)))



## Cumulative incidence function (accountign for competing risks ----

data_tte <- read_rds(fs::path(output_dir_matched, "match_data_tte.rds"))

competing_event<-"death"
if(outcome == "death"){
  competing_event<-NULL
}
if(outcome=="coviddeath"){
  competing_event<-"noncoviddeath"
}
if(outcome=="noncoviddeath"){
  competing_event<-"coviddeath"
}

data_cif0 <-
data_seqtrialcox %>%
  group_by(patient_id, treated) %>%
  summarise(
    time_outcome = max(tstop) - min(tstart),
    ind_outcome = as.integer(last(ind_outcome)),
    tte_recruitment = first(tte_recruitment),
    tte_controlistreated = first(tte_controlistreated),
  ) %>%
  left_join(data_tte, by="patient_id") %>%
  mutate(
    tte_competingevent = .data[[paste0("tte_",competing_event)]],
    tte_censor=pmin(tte_censor, tte_controlistreated, na.rm=TRUE),
    status_outcome = case_when(
      ind_outcome==1 ~ outcome,
      !is.na(tte_competingevent) & (tte_competingevent-tte_recruitment==time_outcome) & (tte_competingevent<=tte_censor) ~ competing_event,
      TRUE ~ "censored",
    )
  )


cif_obj <- cuminc(ftime = data_cif0$time_outcome, fstatus = data_cif0$status_outcome, group=data_cif0$treated, cencode="censored")

cif_mat <- timepoints(cif_obj, seq(0, max(postbaselinecuts), 1))

data_atrisk <-
  data_cif0 %>%
  group_by(treated) %>%
  summarise(n.risk=n())

data_cif <-
  left_join(
    pivot_longer(
      as_tibble(t(cif_mat$est), rownames="time"),
      cols=-any_of("time"),
      names_to=c("treated","event"),
      names_sep="\\s",
      values_to="cmlinc"
    ),
    pivot_longer(
      as_tibble(t(cif_mat$var), rownames="time"),
      cols=-any_of("time"),
      names_to=c("treated","event"),
      names_sep="\\s",
      values_to="var"
    ),
    by=c("treated", "event", "time")
  ) %>%
  mutate(
    treated = as.integer(as.character(treated)),
    time=as.integer(time)+0L,
    treated_descr = if_else(treated==1L, "Boosted", "Unboosted"),
    cmlinc.ll = cmlinc + qnorm(0.025)*sqrt(var),
    cmlinc.ul = cmlinc + qnorm(0.975)*sqrt(var),
  )%>%
  arrange(
    desc(treated), event, time
  )

data_cif_rounded <-
  data_cif %>%
  left_join(data_atrisk, by="treated") %>%
  group_by(treated, event) %>%
  mutate(
    cmlinc = ceiling_any(cmlinc, 1/floor(max(n.risk, na.rm=TRUE)/(threshold+1))),
    cmlinc.ll = ceiling_any(cmlinc.ll, 1/floor(max(n.risk, na.rm=TRUE)/(threshold+1))),
    cmlinc.ul = ceiling_any(cmlinc.ul, 1/floor(max(n.risk, na.rm=TRUE)/(threshold+1))),
  )

write_csv(data_cif_rounded, fs::path(output_dir, "report_cif.csv"))

plot_cif <- data_cif_rounded %>%
  filter(event==outcome) %>%
  ggplot(aes(group=treated_descr, colour=treated_descr, fill=treated_descr)) +
  geom_step(aes(x=time, y=cmlinc))+
  geom_rect(aes(xmin=time, xmax=time+1, ymin=cmlinc.ll, ymax=cmlinc.ul), alpha=0.1, colour="transparent")+
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

plot_cif

ggsave(filename=fs::path(output_dir, "report_cifplot.png"), plot_cif, width=20, height=15, units="cm")


## marginal cumulative risk differences ----

# do not run for now--
if(FALSE){

  model3 <- read_rds(fs::path(output_dir, "model_obj3.rds"))

  data_tidy_surv <- broom::tidy(survfit(model3)) %>%
    group_by(strata) %>%
    summarise(
      n.event=sum(n.event),
      n.risk=first(n.risk),
      n.censor = sum(n.censor),
    )

  if(any(data_tidy_surv$n.event==0)){
    message("some strata have zero events -- cannot calculate marginalised risk.", "\n")
  } else {


    #
    # formula1_pw
    #
    # model3 <- coxph(
    #   formula = Surv(tstop, ind_outcome) ~ treated_period_1 + strata(region),# +
    #     #strata(region) + strata(jcvi_group) + strata(vax12_type),
    #   data = data_seqtrialcox,
    #  # robust = TRUE,
    #  # id = patient_id,
    #   na.action = "na.fail"
    # )


    ## need to redo treatment-time indicators too, as overwriting `treated`` variable not enough

    data_seqtrialcox1 <-
      data_seqtrialcox %>%
      select(-starts_with("treated_period_")) %>%
      rename(treated_period=fup_period) %>%
      fastDummies::dummy_cols(select_columns = c("treated_period")) %>%
      mutate(treated=1L)

    data_seqtrialcox0 <-
      data_seqtrialcox %>%
      mutate(
        across(
          starts_with("treated_period_"),
          ~0L
        ),
        treated = 0L
      )

    times <- seq_len(last(postbaselinecuts)+1)

    surv1 <- survexp(~1, data=data_seqtrialcox1, ratetable = model3, method="ederer", times=times)# times=seq_len(end(postbaselinecuts)))
    surv0 <-  survexp(~1, data=data_seqtrialcox0, ratetable = model3, method="ederer", times=times)# times=seq_len(end(postbaselinecuts)))

    cumulrisk <-
      tibble(
        times,
        surv1 = surv1$surv,
        surv0 = surv0$surv,
        #diff = surv1 - surv0
      ) %>%
      pivot_longer(
        cols=starts_with("surv"),
        names_to="treated",
        values_to="surv"
      ) %>%
      mutate(
        treated_descr = fct_recode(treated, `Boosted`="surv1", `Not boosted`="surv0")
      )


    plot_cumulrisk <- ggplot(cumulrisk)+
      geom_step(aes(x=times, y=1-surv, group=treated_descr, colour=treated_descr))+
      geom_hline(yintercept=0)+ geom_vline(xintercept=0)+
      scale_x_continuous(
        breaks = seq(0,7*52,by=14),
        expand = expansion(0)
      )+
      scale_y_continuous(
        expand = expansion(0)
      )+
      scale_colour_brewer(type="qual", palette="Set1")+
      scale_fill_brewer(type="qual", palette="Set1", guide="none")+
      labs(
        x="Days since booster",
        y="Cumulative incidence",
        colour=NULL,
        fill=NULL
      )+
      theme_minimal()+
      theme(
        legend.position=c(.05,.95),
        legend.justification = c(0,1),
      )


    ggsave(filename=fs::path(output_dir, "report_cumulriskplot.svg"), plot_cumulrisk, width=20, height=15, units="cm")
    ggsave(filename=fs::path(output_dir, "report_cumulriskplot.png"), plot_cumulrisk, width=20, height=15, units="cm")

  }

}
