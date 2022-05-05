library('tidyverse')
library('survival')
#library('flexsurv')

censor <- function(event_date, censor_date, na.censor=TRUE){
  # censors event_date to on or before censor_date
  # if na.censor = TRUE then returns NA if event_date>censor_date, otherwise returns min(event_date, censor_date)
  if (na.censor)
    dplyr::if_else(event_date>censor_date, as.Date(NA_character_), as.Date(event_date))
  else
    dplyr::if_else(event_date>censor_date, as.Date(censor_date), as.Date(event_date))
}

censor_indicator <- function(event_date, censor_date){
  # returns 0 if event_date is censored by censor_date, or if event_date is NA. Otherwise 1
  dplyr::if_else((event_date>censor_date) | is.na(event_date), FALSE, TRUE)
}

tte <- function(origin_date, event_date, censor_date, na.censor=FALSE){
  # returns time-to-event date or time to censor date, which is earlier

  if (na.censor)
    time <- dplyr::if_else(censor_date>=event_date, event_date-origin_date, as.Date(NA)-origin_date)
  else
    time <- pmin(event_date-origin_date, censor_date-origin_date, na.rm=TRUE)
  as.numeric(time)
}



round_tte <- function(time, width=7){
  # group follow-up time to be in periods of size `width`
  # eg, convert to weekly instead of dail with width=7
  # follow-up time of zero is always mapped to zero
  # then first period is mapped to `1`, second period is mapped to `2`, etc
  ceiling(time/width)
}

tidy_surv <-
  function(
    survfit,
    times = NULL,
    addtimezero=FALSE
  ) {

    # tidy post-fit survival dataset, with extra estimates than provided by broom::tidy.coxph

    mintime <- min(survfit$time)
    timezero <- min(0, mintime-1)


    if (is.null(times)) {
      output <-
        survfit %>%
        broom::tidy() %>%
        transmute(
          time,
          lagtime = lag(time, 1L, default=timezero),
          leadtime = lead(time),
          interval = time - lagtime,

          n.risk,
          n.event,
          n.censor,

          summand = n.event / ((n.risk - n.event) * n.risk),

          surv=cumprod(1 - n.event / n.risk),
          surv.ll = conf.low,
          surv.ul = conf.high,
          surv.se = surv * sqrt(cumsum(summand)), #greenwood's formula

          lsurv = log(surv),
          lsurv.se = sqrt(cumsum(summand)),

          # kaplan meier hazard estimates
          haz = n.event / (n.risk * interval), # =-(surv-lag(surv))/lag(surv)
          cml.haz = cumsum(haz),
          cml.haz.se = std.error, # = surv.se/surv,
          haz.se = haz * sqrt((n.risk - n.event) / (n.risk * n.event)),


          # actuarial hazard estimates
          haz_ac = n.event / ((n.risk - (n.censor / 2) - (n.event / 2)) * interval), # =(cml.haz-lag(cml.haz))/interval
          cml.haz_ac = -log(surv), #=cumsum(haz_ac)
          haz_ac.se = (haz_ac * sqrt(1 - (haz_ac * interval / 2)^2)) / sqrt(n.event),

          # log(-log()) scale

          llsurv = log(-log(surv)),
          llsurv.se = sqrt((1 / log(surv)^2) * cumsum(summand)),

      )
    }

    else {

      output <-
        survfit %>%
        broom::tidy() %>%
        complete(
          time = times,
          fill = list(n.event = 0, n.censor = 0)
        ) %>%
        fill(n.risk, .direction = c("up")) %>%
        transmute(
          time,
          lagtime = lag(time, 1L, default = timezero),
          leadtime = lead(time),
          interval = time - lagtime,

          n.risk,
          n.event,
          n.censor,

          summand = n.event / ((n.risk - n.event) * n.risk),

          surv=cumprod(1 - n.event / n.risk),

          surv.ll = conf.low,
          surv.ul = conf.high,
          surv.se = surv * sqrt(cumsum(summand)), #greenwood's formula

          lsurv = log(surv),
          lsurv.se = sqrt(cumsum(summand)),

          # kaplan meier hazard estimates
          haz = n.event / (n.risk * interval), # =-(surv-lag(surv))/lag(surv)
          cml.haz.se = std.error, #  = surv.se/surv
          cml.haz = cumsum(haz), # =cumsum(haz_km)
          haz.se = haz * sqrt((n.risk - n.event) / (n.risk * n.event)),

          # actuarial hazard estimates
          haz_ac = n.event / ((n.risk - (n.censor / 2) - (n.event / 2)) * interval), # =(cml.haz-lag(cml.haz))/interval
          cml.haz_ac = -log(surv), #=cumsum(haz_ac)
          haz_ac.se = (haz_ac * sqrt(1 - (haz_ac * interval / 2)^2)) / sqrt(n.event),

          # log(-log()) scale

          llsurv = log(-log(surv)),
          llsurv.se = sqrt((1 / log(surv)^2) * cumsum(summand)),
        )
    }

    if(addtimezero){
      output <- output %>%
        add_row(
          time = timezero,
          lagtime = NA_real_,
          leadtime = mintime,
          interval = leadtime-time,
          summand=0,

          #estimate=1, std.error=0, conf.high=1, conf.low=1,

          surv=1,
          surv.se = 0,
          surv.ll=1,
          surv.ul=1,
          surv.se=0,

          haz_km=0, haz_km.se=0, cml.haz_km=0,
          haz_ac=0, haz_ac.se=0, cml.haz_ac=0,
          .before=1
        )
    }

    return(output)
  }
#
#
#
# tidy_flexsurvspline <- function(
#   flexsurvsplinefit,
#   times=NULL,
#   addtimezero=FALSE
# ){
#
#   # tidy post-fit flexsurv dataset
#
#   if(is.null(times)){
#     times <- unique(flexsurvsplinefit$data$Y[,"time"])
#   }
#
#   if(addtimezero){
#     mintime <- min(times)
#     timezero <- min(0, mintime-1)
#     times <- unique(c(timezero, times))
#   }
#
#
#   summaryfs_survival <- summary(flexsurvsplinefit, type="survival", t=times, tidy=TRUE)
#   names(summaryfs_survival)[1:4] <- c("time", "smooth_surv", "smooth_surv.ll", "smooth_surv.ul")
#
#   summaryfs_hazard <- summary(flexsurvsplinefit, type="hazard", t=times, tidy=TRUE)
#   names(summaryfs_hazard)[1:4] <- c("time", "smooth_haz", "smooth_haz.ll", "smooth_haz.ul")
#
#   summaryfs_cmlhazard <- summary(flexsurvsplinefit, type="cumhaz", t=times, tidy=TRUE)
#   names(summaryfs_cmlhazard)[1:4] <- c("time", "smooth_cml.haz", "smooth_cml.haz.ll", "smooth_cml.haz.ul")
#
#   output <- bind_cols(summaryfs_survival, summaryfs_hazard[,c(2,3,4)], summaryfs_cmlhazard[,c(2,3,4)])
#
#   return(output)
#
# }
#
#
# get_hr <- function(tidy_flexsurv_group, group){
#
#   hrdat <- tidy_flexsurv_group %>%
#     mutate(
#       .trt = tidy_flexsurv[[group]]
#     ) %>%
#     select(time, .trt, smooth_haz) %>%
#     pivot_wider(
#       id_cols=c(time),
#       names_from = .trt,
#       values_from=c(smooth_haz)
#     )
#
#   hrdat
# }
#
#
#
