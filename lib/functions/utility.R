


ceiling_any <- function(x, to=1){
  # round to nearest 100 millionth to avoid floating point errors
  ceiling(plyr::round_any(x/to, 1/100000000))*to
}



fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

#
# specify_decimal <- function(x, k, trim=FALSE) {
#
#   fmtd <- format(round(x, k), nsmall = k)
#   if (trim) {fmtd <- trimws(fmtd)}
#   return(fmtd)
# }
#
# print_est1bracket <- function(x, b, round=1){
#   paste0(specify_decimal(x, round), " (", specify_decimal(b, round), ")")
# }
#
# print_est2bracket <- function(x, b1, b2, round=1){
#   paste0(specify_decimal(x, round), " (", specify_decimal(b1, round), ", ", specify_decimal(b2, round), ")")
# }
#
# print_2bracket <- function(b1, b2, round=1){
#   paste0("(", specify_decimal(b1, round), ", ", specify_decimal(b2, round), ")")
# }
#
# print_pval <- function(pval, k=3){
#   ifelse(pval < 1/(10^k), paste0("p<", 1/(10^k)), paste0("p=", specify_decimal(pval, k = 3)))
# }
#
#
#
#
#

postvax_cut <- function(event_time, time, breaks, prelabel="pre", prefix=""){

  # this function defines post-vaccination time-periods at `time`,
  # for a vaccination occurring at time `event_time`
  # delimited by `breaks`

  # note, intervals are open on the left and closed on the right
  # so at the exact time point the vaccination occurred, it will be classed as "pre-dose".

  event_time <- as.numeric(event_time)
  event_time <- if_else(!is.na(event_time), event_time, Inf)

  diff <- time - event_time
  breaks_aug <- unique(c(-Inf, breaks, Inf))
  labels0 <- cut(c(breaks, Inf), breaks_aug)
  labels <- paste0(prefix, c(prelabel, as.character(labels0[-1])))
  period <- cut(diff, breaks=breaks_aug, labels=labels, include.lowest=TRUE)


  period
}
#
#
#
#
#
# define post-vaccination time periods for piece-wise constant hazards (ie time-varying effects / time-varying coefficients)
# eg c(0, 10, 21) will create 4 periods
# pre-vaccination, [0, 10), [10, 21), and [21, inf)
# can use eg c(3, 10, 21) to treat first 3 days post-vaccination the same as pre-vaccination
# note that the exact vaccination date is set to the first "pre-vax" period,
# because in survival analysis, intervals are open-left and closed-right.


timesince_cut <- function(time_since, breaks, prefix=""){

  # this function defines post-vaccination time-periods at `time_since`,
  # delimited by `breaks`

  # note, intervals are open on the left and closed on the right
  # so at the exact time point the vaccination occurred, it will be classed as "pre-dose".

  stopifnot("time_since should be strictly non-negative" = time_since>=0)
  time_since <- as.numeric(time_since)
  time_since <- if_else(!is.na(time_since), time_since, Inf)

  breaks_aug <- unique(c(breaks, Inf))

  lab_left <- breaks+1
  lab_right <- lead(breaks)
  label <- paste0(lab_left, "-", lab_right)
  label <- str_replace(label,"-NA", "+")
  labels <- paste0(prefix, label)

  #labels0 <- cut(c(breaks, Inf), breaks_aug)
  #labels <- paste0(prefix, c(prelabel, as.character(labels0[-1])))
  period <- cut(time_since, breaks=breaks_aug, labels=labels, include.lowest=TRUE)

  period
}

# timesince_cut_end <- function(time_since, breaks, prefix=""){
#
#   # this function defines post-vaccination time-periods at `time_since`,
#   # delimited by `breaks`
#
#   # note, intervals are open on the left and closed on the right
#   # so at the exact time point the vaccination occurred, it will be classed as "pre-dose".
#
#   stopifnot("time_since should be strictly non-negative" = time_since>=0)
#   time_since <- as.numeric(time_since)
#   time_since <- if_else(!is.na(time_since), time_since, Inf)
#
#   lab_left <- breaks[-length(breaks)]+1
#   lab_right <- breaks[-1]
#   label <- paste0(lab_left, "-", lab_right)
#   labels <- paste0(prefix, label)
#
#   #labels0 <- cut(c(breaks, Inf), breaks_aug)
#   #labels <- paste0(prefix, c(prelabel, as.character(labels0[-1])))
#   period <- cut(time_since, breaks=breaks, labels=labels, include.lowest=FALSE)
#
#   period
# }
#
#
# # tidy functions for specific objects ----
#
#
# tidy_parglm <- function(x, conf.int = FALSE, conf.level = .95,
#                         exponentiate = FALSE, ...) {
#
#   # nicked from https://github.com/tidymodels/broom/blob/443ebd995760c6674f122d75ceb2e6b82f055439/R/stats-glm-tidiers.R#L12
#   # and adapted for parglm glm objects
#
#   ret <- tibble::as_tibble(summary(x)$coefficients, rownames = "term")
#   colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
#
#   # summary(x)$coefficients misses rank deficient rows (i.e. coefs that summary.lm() sets to NA)
#   # catch them here and add them back
#
#   coefs <- tibble::enframe(stats::coef(x), name = "term", value = "estimate")
#   ret <-  dplyr::left_join(coefs, ret, by = c("term", "estimate"))
#
#   if (conf.int) {
#     ci <- confint.default(x, level = conf.level) # not ideal -- change for more robust conf intervals! - see tidy_plr below
#     ci <- tibble::as_tibble(ci, rownames = "term")
#     names(ci) <- c("term", "conf.low", "conf.high")
#
#     ret <- dplyr::left_join(ret, ci, by = "term")
#   }
#
#   if (exponentiate) {
#     stop("cannot use exponentiate=TRUE yet")
#   }
#
#   ret
# }
#
#
# tidy_wald <- function(x, conf.int = TRUE, conf.level = .95, exponentiate = TRUE, ...) {
#
#   # to use Wald CIs instead of profile CIs.
#   ret <- broom::tidy(x, conf.int = FALSE, conf.level = conf.level, exponentiate = exponentiate)
#
#   if(conf.int){
#     ci <- confint.default(x, level = conf.level)
#     if(exponentiate){ci = exp(ci)}
#     ci <- tibble::as_tibble(ci, rownames = "term")
#     names(ci) <- c("term", "conf.low", "conf.high")
#
#     ret <- dplyr::left_join(ret, ci, by = "term")
#   }
#   ret
# }
#
# tidy_plr <- function(model, conf.int=TRUE, conf.level=0.95, exponentiate=FALSE, cluster){
#
#   # create tidy dataframe for coefficients of pooled logistic regression
#   # using robust standard errors
#   robustSEs <- lmtest::coeftest(model, vcov. = sandwich::vcovCL(model, cluster = cluster, type = "HC0")) %>% broom::tidy(conf.int=FALSE, exponentiate=exponentiate)
#   robustCIs <- lmtest::coefci(model, vcov. = sandwich::vcovCL(model, cluster = cluster, type = "HC0"), level = conf.level) %>% tibble::as_tibble(rownames="term")
#   robust <- dplyr::inner_join(robustSEs, robustCIs, by="term")
#
#   robust %>%
#     rename(
#       conf.low=`2.5 %`,
#       conf.high=`97.5 %`
#     ) %>%
#     mutate(
#       or = exp(estimate),
#       or.ll = exp(conf.low),
#       or.ul = exp(conf.high),
#     )
#
# }
#
#
# glance_plr <- function(model){
#   tibble(
#     AIC=model$aic,
#     df.null=model$df.null,
#     df.residual=model$df.residual,
#     deviance=model$deviance,
#     null.deviance=model$null.deviance,
#     nobs=length(model$y)
#   )
# }
#
# tidypp_plr <- function(model, model_name, cluster, ...){
#   broom.helpers::tidy_plus_plus(
#     model,
#     tidy_fun = tidy_plr,
#     exponentiate=FALSE,
#     cluster = cluster,
#     ...
#   ) %>%
#     add_column(
#       model_name = model_name,
#       .before=1
#     )
# }
#
#
#
# tidy_custom.glm  <- function(model, conf.int=TRUE, conf.level=0.95, exponentiate=FALSE, cluster){
#   # create tidy dataframe for coefficients of pooled logistic regression
#   #mod_tidy <- tidy_parglm(model, conf.int=conf.int, conf.level=conf.level, exponentiate=exponentiate)
#   robustSEs <- coeftest(model, vcov. = vcovCL(model, cluster = cluster, type = "HC0")) %>% broom::tidy()
#   robustCIs <- coefci(model, vcov. = vcovCL(model, cluster = cluster, type = "HC0")) %>% as_tibble(rownames="term")
#   robust <- inner_join(robustSEs, robustCIs, by="term")
#
#   output <- robust %>%
#     rename(
#       conf.low=`2.5 %`,
#       conf.high=`97.5 %`
#     )
#
#   if(exponentiate){
#     output <- output %>%
#       mutate(
#         estimate = exp(estimate),
#         conf.low = exp(conf.low),
#         conf.high = exp(conf.high),
#       )
#   }
#
#   output
# }
#
# plr.predict <- function(model, clcov, newdata, type=c("link", "response")){
#
#   # from https://stackoverflow.com/questions/3790116/using-clustered-covariance-matrix-in-predict-lm
#
#   if(missing(newdata)){ newdata <- model$model }
#   tt <- terms(model) # this helpfully grabs the correct spline basis from the model, rather than recalculating based on `newdata`
#   Terms <- delete.response(tt)
#   m.mat <- model.matrix(Terms, data=newdata)
#   m.coef <- model$coef
#
#   nu <- as.vector(m.mat %*% m.coef) # n x 1
#
#   if(type=="link"){
#     fit <- nu
#     fit.se <- sqrt( rowSums(m.mat * (m.mat %*% clcov)) )
#   }
#
#   if(type=="response"){
#
#     logit2nu <- (exp(nu)/((1+exp(nu))^2)) # n x 1
#     deriv_nu <- t(m.mat) * c(logit2nu)  # k x n
#
#     stopifnot("term indices are not aligned" = rownames(clcov)==names(deriv_nu))
#
#     fit <- plogis(nu)
#     fit.se <- sqrt(rowSums((t(deriv_nu) %*% vcov) * t(deriv_nu)))
#   }
#
#   return(list(fit=fit, fit.se=fit.se))
# }
#
#
# # functions for IRR confidence intervals ----
#
# rrCI_normal <- function(n, pt, ref_n, ref_pt, group, accuracy=0.001){
#   rate <- n/pt
#   ref_rate <- ref_n/ref_pt
#   rr <- rate/ref_rate
#   log_rr <- log(rr)
#   selog_rr <- sqrt((1/n)+(1/ref_n))
#   log_ll <- log_rr - qnorm(0.975)*selog_rr
#   log_ul <- log_rr + qnorm(0.975)*selog_rr
#   ll <- exp(log_ll)
#   ul <- exp(log_ul)
#
#   if_else(
#     group==levels(group)[1],
#     NA_character_,
#     paste0("(", scales::number_format(accuracy=accuracy)(ll), "-", scales::number_format(accuracy=accuracy)(ul), ")")
#   )
# }
#
# rrCI_exact <- function(n, pt, ref_n, ref_pt, accuracy=0.001){
#
#   # use exact methods if incidence is very low for immediate post-vaccine outcomes
#
#   rate <- n/pt
#   ref_rate <- ref_n/ref_pt
#   rr <- rate/ref_rate
#
#   ll = ref_pt/pt * (n/(ref_n+1)) * 1/qf(2*(ref_n+1), 2*n, p = 0.05/2, lower.tail = FALSE)
#   ul = ref_pt/pt * ((n+1)/ref_n) * qf(2*(n+1), 2*ref_n, p = 0.05/2, lower.tail = FALSE)
#
#   paste0("(", scales::number_format(accuracy=accuracy)(ll), "-", scales::number_format(accuracy=accuracy)(ul), ")")
#
# }
#
# # get confidence intervals for rate ratio using unadjusted poisson GLM
# # uses gtsummary not broom::tidy to make it easier to paste onto original data
#
# rrCI_glm <- function(n, pt, x, accuracy=0.001){
#
#   dat<-tibble(n=n, pt=pt, x=x)
#
#   poismod <- glm(
#     formula = n ~ x + offset(log(pt*365.25)),
#     family=poisson,
#     data=dat
#   )
#
#   gtmodel <- tbl_regression(poismod, exponentiate=TRUE)$table_body %>%
#     filter(reference_row %in% FALSE) %>%
#     select(label, conf.low, conf.high)
#
#   dat2 <- left_join(dat, gtmodel, by=c("x"="label"))
#
#   paste0("(", scales::number_format(accuracy=accuracy)(dat2$conf.low), "-", scales::number_format(accuracy=accuracy)(dat2$conf.high), ")")
#
# }
#
#
#
