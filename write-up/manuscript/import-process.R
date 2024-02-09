

## Collection of formatting functions that may be used ----

perN <- 1000
perN_format <- label_number(1, 1, big.mark=",")(perN)

label_number_n <- label_number(1, 1, big.mark=",")

label_number_risk <- function(x, scale=perN){
  x_scale <- x*scale
  x_abs <- abs(x_scale)
  case_when(
    x_abs>=100 ~ label_number(0.1, scale, style_negative="minus", big.mark=",")(x),
    x_abs<100 & x_abs>=10 ~ label_number(0.1, scale, style_negative="minus", big.mark=",")(x),
    x_abs<10 & x_abs>=1 ~ label_number(0.01, scale, style_negative="minus", big.mark=",")(x),
    x_abs<1 & x_abs>=0.1 ~ label_number(0.01, scale, style_negative="minus", big.mark=",")(x),
    x_abs<0.1 ~ label_number(0.001, scale, style_negative="minus", big.mark=",")(x),
    NA ~ NA_character_
  )
}

label_number_rr <- function(x, scale=1){
  x_scale <- x*scale
  x_abs <- abs(x_scale)
  case_when(
    x_abs>=100 ~ label_number(0.01, scale, style_negative="minus", big.mark=",")(x),
    x_abs<100 & x_abs>=10 ~ label_number(0.01, scale, style_negative="minus", big.mark=",")(x),
    x_abs<10 & x_abs>=1 ~ label_number(0.01, scale, style_negative="minus", big.mark=",")(x),
    x_abs<1 & x_abs>=0.1 ~ label_number(0.01, scale, style_negative="minus", big.mark=",")(x),
    x_abs<0.1 & x_abs>=0.01 ~ label_number(0.01, scale, style_negative="minus", big.mark=",")(x),
    x_abs<0.01 ~ label_number(0.0001, scale, style_negative="minus", big.mark=",")(x),
    NA ~ NA_character_
  )
}



flowchart_totals_allcohorts <- read_csv(path(output_dir_os, "flowchart_totals_allcohorts_rounded.csv"))

# store results for each cohort in a list,
# accessed for example as follows:  rst$cv$contrasts

# initialise list
rst <- lst(cv=NULL, age75plus=NULL)

for(cohort in c("age75plus", "cv")){

  # new temporary environment
  env <- new.env()

  # import data within new environment
  evalq({


    cohort_dir <- path(output_dir_os, cohort)

    ## study flowchart ----

    flowchart_totals <-
      read_csv(path(cohort_dir, "flowchart_totals_rounded.csv"))

    flowchart <-
      read_csv(path(cohort_dir, "flowchart_rounded.csv")) %>%
      filter(matchset =="B")

    ## matching coverage over time ----

    match_coverage <-
      read_csv(path(cohort_dir, "coverage_rounded.csv")) %>%
      filter(matchset =="B") %>%
      mutate(
        treatment_descr = fct_recoderelevel(as.character(treatment),  recoder$treatment),
      )

    ## follow-up summary data ----

    followup <-
      read_csv(path(cohort_dir, "followup_rounded.csv"))  %>%
      filter(matchset =="B")

    followup_treatment <-
      read_csv(path(cohort_dir, "followup_treatment_rounded.csv")) %>%
      filter(matchset =="B")

    followup_table <-
      bind_rows(
        followup_treatment,
        followup %>% mutate(treatment=2)
      ) %>%
      mutate(
        treatment_descr = fct_recoderelevel(as.character(treatment), c(recoder$treatment, "Both" ="2")),
        outcome_descr = fct_recoderelevel(as.character(outcome),  recoder$outcome),
        subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroup),
        subgroup_level_descr = replace_na(subgroup_level_descr, ""),
      ) %>%
      filter(subgroup=="all") %>%
      mutate(
        total_weeks = label_number(0.1, 1/7)(persontime),
        mean_weeks = label_number(0.1, 1/7)(exittime_mean),
        median_weeks = label_number(0.1, 1/7)(exittime_median),
        Q1_weeks = label_number(0.1, 1/7)(exittime_Q1),
        Q3_weeks = label_number(0.1, 1/7)(exittime_Q3),
      ) %>%
      arrange(treatment_descr, outcome_descr, subgroup_descr, subgroup_level_descr)


    ## event counts data ----

    eventcounts <-
      read_csv(path(cohort_dir, "eventcounts_rounded.csv")) %>%
      filter(matchset=="B")


    ## baseline characteristics ("table 1") data ----
    match_table1 <-
      read_csv(path(cohort_dir, "table1_rounded.csv"))  %>%
      filter(matchset %in% c("prematch", "B")) %>%
      mutate(
        matchset = if_else(matchset=="B", "postmatch", matchset),
        by=case_when(
          by=="pfizer/BA.4-5" ~ 0L,
          by=="Sanofi" ~ 1L,
        ),
        variable_levels = coalesce(variable_levels, ""), # NA to ""
        var_label = if_else(variable=="N", "N", var_label),
        var_label = fct_inorder(var_label),
        variable = fct_inorder(variable),
        level = fct_rev(fct_inorder(str_replace(paste(var_label, variable_levels, sep=": "),  "\\:\\s$", ""))), # unique var * level value
      )


    data_smd <-
      match_table1 %>%
      filter(
        variable != "N",
      ) %>%
      group_by(matchset, var_label, variable, variable_levels, level) %>%
      mutate(
        mean = coalesce(mean,p),
        sd = coalesce(sd,sqrt(p*(1-p)))
      ) %>%
      summarise(
        diff = diff(mean),
        sd = sqrt(mean(sd^2)),
        smd = diff/sd
      ) %>%
      group_by(matchset) %>%
      mutate(
        variable_card = as.numeric(var_label)%%2,
      ) %>%
      arrange(matchset, desc(level)) %>%
      mutate(
        level = fct_rev(level)
      )

    match_table1_wide <-
      match_table1 %>%
      # one column per treatment group
      pivot_wider(
        id_cols = c(matchset, var_label, variable, variable_levels, level),
        names_from = by,
        values_from = c(N, n, p, mean, sd, stat_display)
      ) %>%
      # add SMD stat
      left_join(
        data_smd,
        by=c("matchset", "var_label", "variable", "variable_levels", "level")
      ) %>%
     # format columns
      filter(!is.na(`n_1`)) %>%
      mutate(
        stat_0 = case_when(
          stat_display_0=="{n} ({p}%)" ~ glue("{n_0} ({scales::label_number(0.1, 100)(p_0)})"),
          stat_display_0=="{mean} ({sd})" ~ glue("{scales::label_number(0.1)(mean_0)} ({scales::label_number(0.1)(sd_0)})"),
        ),
        stat_1 = case_when(
          stat_display_1=="{n} ({p}%)" ~ glue("{n_1} ({scales::label_number(0.1, 100)(p_1)})"),
          stat_display_1=="{mean} ({sd})" ~ glue("{scales::label_number(0.1)(mean_1)} ({scales::label_number(0.1)(sd_1)})"),
        ),
        smd = scales::label_number(0.001)(smd),
      )



    ## main estimates ----

    estimates <-
      read_csv(path(cohort_dir, "km_estimates_rounded.csv")) %>%
      mutate(
        treatment_descr = fct_recoderelevel(as.character(treatment),  recoder$treatment),
        outcome_descr = fct_recoderelevel(as.character(outcome),  recoder$outcome),
        subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroup),
        subgroup_level_descr = replace_na(subgroup_level_descr, "")
      ) %>%
      filter(matchset =="B")

    contrasts_daily <-
      read_csv(path(cohort_dir, "contrasts_daily_rounded.csv")) %>%
      mutate(
        outcome_descr = fct_recoderelevel(as.character(outcome),  recoder$outcome),
        subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroup),
        subgroup_level_descr = replace_na(subgroup_level_descr, "")
      ) %>%
      filter(matchset =="B")

    contrasts_cuts <-
      read_csv(path(cohort_dir, "contrasts_cuts_rounded.csv")) %>%
      mutate(
        outcome_descr = fct_recoderelevel(as.character(outcome),  recoder$outcome),
        subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroup),
        subgroup_level_descr = replace_na(subgroup_level_descr, "")
      ) %>%
      filter(matchset =="B")

    contrasts_overall <-
      read_csv(path(cohort_dir, "contrasts_overall_rounded.csv"))  %>%
      filter(matchset =="B")


    contrasts_overall <-
      contrasts_overall %>%
      mutate(
        outcome_descr = fct_recoderelevel(as.character(outcome),  recoder$outcome),
        subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroup),
        subgroup_level_descr = fct_inorder(replace_na(subgroup_level_descr, "")),

        # deal with inestimable estimates
        irr = if_else(irr>10000, Inf, irr),
        irr.ll = if_else(irr.ll>10000, Inf, irr.ll),
        irr.ul = if_else(irr.ul>10000, Inf, irr.ul),
      ) %>% group_by(
        outcome,
        outcome_descr,
        subgroup,
        subgroup_descr
      ) %>%
      mutate(

        # z-tests for subgroup heterogeneity
        # rd_diff = if_else(row_number()!=1, (rd - first(rd)), NA_real_),
        # rd_diff.se = sqrt((rd.se^2)+(first(rd.se)^2)),
        # rr.ln_diff = if_else(row_number()!=1, (log(rr)-log(first(rr))), NA_real_),
        # rr.ln_diff.se = sqrt((rr.ln.se^2)+(first(rr.ln.se)^2)),
        # rr_diff.p = pnorm(-abs(rr.ln_diff / rr.ln_diff.se))*2,
        # coxhr.ln_diff = if_else(row_number()!=1, (log(coxhr)-first(log(coxhr))), NA_real_ ),
        # coxhr.ln_diff.se = sqrt((coxhr.se^2)+(first(coxhr.se)^2)),
        # coxhr.ln_diff.p = pnorm(-abs(coxhr.ln_diff / coxhr.ln_diff.se))*2,

        # heterogeneity tests
        rd.Q = sum((1/(rd.se^2))*((rd-weighted.mean(rd, 1/(rd.se)^2))^2)),
        rd.p = pchisq(rd.Q, df=n()-1, lower.tail=FALSE),
        rr.ln.Q = sum((1/(rr.ln.se^2))*((log(rr)-weighted.mean(log(rr), 1/(rr.ln.se)^2))^2)),
        rr.ln.p = pchisq(rr.ln.Q, df=n()-1, lower.tail=FALSE),
        #coxhr.ln.Q = sum((1/(coxhr.se^2))*((log(coxhr)-weighted.mean(log(coxhr), 1/(coxhr.se)^2))^2)),
        #coxhr.ln.p = pchisq(coxhr.ln.Q, df=n()-1, lower.tail=FALSE),
        irr.ln.Q = sum((1/(irr.ln.se^2))*((log(irr)-weighted.mean(log(irr), 1/(irr.ln.se)^2))^2)),
        irr.ln.p = pchisq(irr.ln.Q, df=n()-1, lower.tail=FALSE),
      ) %>%
      ungroup()

    ## formatted for the tables
    contrasts_table <-
      contrasts_overall %>%
      group_by(outcome) %>%
      mutate(
        outcome,
        outcome_descr,
        subgroup,
        subgroup_descr,
        subgroup_level_descr,
        n.event = label_number_n(n.event_0+n.event_1),
        persontime_weeks = label_number_n((persontime_0 + persontime_1) / 7),
        n.atrisk = label_number_n(n.atrisk_0+n.atrisk_1),
        persontime_0=label_number_n(persontime_0/7),
        persontime_1=label_number_n(persontime_1/7),
        n.event_0=label_number_n(n.event_0),
        n.event_1=label_number_n(n.event_1),

        irr = if_else(irr>10000, Inf, irr),
        irr.ll = if_else(irr.ll>10000, Inf, irr.ll),
        irr.ul = if_else(irr.ul>10000, Inf, irr.ul),

        riskCI_0 = glue("{label_number_risk(risk_0)} ({label_number_risk(risk.ll_0)} to {label_number_risk(risk.ul_0)})"),
        riskCI_0_95 = glue("{label_number_risk(risk_0)} (95%CI {label_number_risk(risk.ll_0)} to {label_number_risk(risk.ul_0)})"),
        riskCI_1 = glue("{label_number_risk(risk_1)} ({label_number_risk(risk.ll_1)} to {label_number_risk(risk.ul_1)})"),
        rdCI = glue("{label_number_risk(rd)} ({label_number_risk(rd.ll)} to {label_number_risk(rd.ul)})"),
        rdCI_95 = glue("{label_number_risk(rd)} (95%CI {label_number_risk(rd.ll)} to {label_number_risk(rd.ul)})"),
        rrCI = glue("{label_number_rr(rr)} ({label_number_rr(rr.ll)}-{label_number_rr(rr.ul)})"),
        irrCI = glue("{label_number_rr(irr)} ({label_number_rr(irr.ll)}-{label_number_rr(irr.ul)})"),
        irrCI_95 = glue("{label_number_rr(irr)} (95%CI {label_number_rr(irr.ll)} to {label_number_rr(irr.ul)})"),
        #coxhrCI = glue("{label_number_rr(coxhr)} ({label_number_rr(coxhr.ll)} to {label_number_rr(coxhr.ul)})"),
        #coxhrCI_95 = glue("{label_number_rr(coxhr)} (95%CI {label_number_rr(coxhr.ll)} to {label_number_rr(coxhr.ul)})"),

        rd.p = if_else(rd.p<0.001, "<0.001", label_number(0.001, trim=FALSE)(rd.p)),
        rr.ln.p = if_else(rr.ln.p<0.001, "<0.001", label_number(0.001, trim=FALSE)(rr.ln.p)),
        #coxhr.ln.p = if_else(coxhr.ln.p<0.001, "<0.001", label_number(0.001, trim=FALSE)(coxhr.ln.p)),
        irr.ln.p = if_else(irr.ln.p<0.001, "<0.001", label_number(0.001, trim=FALSE)(irr.ln.p)),
      ) %>%
      ungroup()


    contrasts_cuts_table <-
      contrasts_cuts %>%
      filter(
        outcome %in% outcomes,
        subgroup == "all"
      ) %>%
      mutate(
        irr = if_else(irr>10000, Inf, irr),
        irr.ll = if_else(irr.ll>10000, Inf, irr.ll),
        irr.ul = if_else(irr.ul>10000, Inf, irr.ul),
        irrCI = glue("{label_number_rr(irr)} ({label_number_rr(irr.ll)}-{label_number_rr(irr.ul)})"),
        irrCI_95 = glue("{label_number_rr(irr)} (95%CI {label_number_rr(irr.ll)} to {label_number_rr(irr.ul)})"),
      )

  }, envir = env)


  # convert temp environment to a list within rst
  rst[[cohort]] <- as.list(env)
}



match_coverage <-
  bind_rows(
    rst$cv$match_coverage %>% mutate(cohort="cv"),
    rst$age75plus$match_coverage %>% mutate(cohort="age75plus"),
  )


match_table1 <-
  bind_rows(
    rst$cv$match_table1 %>% mutate(cohort="cv"),
    rst$age75plus$match_table1 %>% mutate(cohort="age75plus"),
  )

match_table1_wide <-
  bind_rows(
    rst$cv$match_table1_wide %>% mutate(cohort="cv"),
    rst$age75plus$match_table1_wide %>% mutate(cohort="age75plus"),
  )

data_smd <-
  bind_rows(
  rst$cv$data_smd %>% mutate(cohort="cv"),
  rst$age75plus$data_smd %>% mutate(cohort="age75plus"),
)


contrasts_overall <-
  bind_rows(
    rst$cv$contrasts_overall %>% mutate(cohort="cv", .before = 1),
    rst$age75plus$contrasts_overall %>% mutate(cohort="age75plus", .before = 1),
  ) %>%
  mutate(
    cohort_descr = fct_recoderelevel(as.character(cohort), recoder$cohort),
    .after = 1
  )


contrasts_table <-
  bind_rows(
    rst$cv$contrasts_table %>% mutate(cohort="cv", .before = 1),
    rst$age75plus$contrasts_table %>% mutate(cohort="age75plus", .before = 1),
  ) %>%
  mutate(
    cohort_descr = fct_recoderelevel(as.character(cohort), recoder$cohort),
    .after = 1
  )

flowchart <- bind_rows(
  rst$cv$flowchart %>% mutate(cohort="cv", .before = 1),
  rst$age75plus$flowchart %>% mutate(cohort="age75plus", .before = 1),
) %>%
mutate(
  cohort_descr = fct_recoderelevel(as.character(cohort), recoder$cohort),
  .after = 1
)

