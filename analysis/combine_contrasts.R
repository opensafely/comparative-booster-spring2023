
# # # # # # # # # # # # # # # # # # # # #
# Purpose: Combine ci estimates from different outcomes
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----


## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')
library("fs")

## Import custom user functions from lib
source(here("analysis", "functions", "utility.R"))

## Import design elements
source(here("analysis", "design", "design.R"))

## import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort <- "cv"
  #matchset <- "A"

} else {
  cohort <- args[[1]]
  #matchset <- args[[2]]
}


metaparams <-
  expand_grid(
    matchset = c("A", "B"),
    outcome = factor(c("covidemergency", "covidadmitted", "covidcritcare", "coviddeath", "noncoviddeath", "fracture", "pericarditis", "myocarditis")),
    subgroup = factor(recoder$subgroups),
    #outcome = factor("covidadmitted"),
    #subgroup = factor(c("all", "ageband")),
  ) %>%
  filter(
    cohort!=subgroup
  ) %>%
  mutate(
    #cohort_descr = fct_recoderelevel(cohort, recoder$cohort),
    outcome_descr = fct_recoderelevel(outcome,  recoder$outcome),
    subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroups),
  )


output_dir <- here("output", "combine", cohort, "contrasts")
dir_create(output_dir)

## KM estimates ----

km_estimates <- metaparams %>%
  mutate(
    data = pmap(list(matchset, subgroup, outcome), function(matchset, subgroup, outcome) {
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", cohort, matchset, "km", subgroup, outcome, "km_estimates_rounded.rds"))
      dat %>%
        ungroup() %>%
        add_column(
          subgroup_level = as.character(.[[subgroup]]),
          subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
          .before=1
        ) %>%
        select(-all_of(c(subgroup)))
    })
  ) %>%
  unnest(data)

write_csv(km_estimates, path(output_dir, "km_estimates_rounded.csv"))

contrasts_daily <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup){
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", cohort, matchset, "km", subgroup, outcome, "contrasts_daily_rounded.rds"))
      dat %>%
        ungroup() %>%
        add_column(
          subgroup_level = as.character(.[[subgroup]]),
          subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
          .before=1
        ) %>%
        select(-all_of(subgroup))
      }
    )
  ) %>%
  unnest(data)

write_csv(contrasts_daily, path(output_dir, "contrasts_daily_rounded.csv"))


contrasts_cuts <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup){
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", cohort, matchset, "km", subgroup, outcome, "contrasts_cuts_rounded.rds"))
      dat %>%
        ungroup() %>%
        add_column(
          subgroup_level = as.character(.[[subgroup]]),
          subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
          .before=1
        ) %>%
        select(-all_of(subgroup))
    }
    )
  ) %>%
  unnest(data)

write_csv(contrasts_cuts, path(output_dir, "contrasts_cuts_rounded.csv"))


contrasts_overall <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup) {
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", cohort, matchset, "km", subgroup, outcome, "contrasts_overall_rounded.rds"))
      dat %>%
        ungroup() %>%
        add_column(
          subgroup_level = as.character(.[[subgroup]]),
          subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
          .before=1
        ) %>%
        select(-all_of(subgroup))
      }
    )
  ) %>%
  unnest(data)

write_csv(contrasts_overall, path(output_dir, "contrasts_overall_rounded.csv"))

## follow-up duration summary statistics ----

### overall ----
followup <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup) {
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", cohort, matchset, "km", subgroup, outcome, "followup_rounded.rds"))
      dat %>%
        ungroup() %>%
        add_column(
          subgroup_level = as.character(.[[subgroup]]),
          subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
          .before=1
        ) %>%
        select(-all_of(subgroup))
    }
    )
  ) %>%
  unnest(data)

write_csv(followup, path(output_dir, "followup_rounded.csv"))


### by treatment ----
followup_treatment <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup) {
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", cohort, matchset, "km", subgroup, outcome, "followup_treatment_rounded.rds"))
      dat %>%
        ungroup() %>%
        add_column(
          subgroup_level = as.character(.[[subgroup]]),
          subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
          .before=1
        ) %>%
        select(-all_of(subgroup))
    }
    )
  ) %>%
  unnest(data)

write_csv(followup_treatment, path(output_dir, "followup_treatment_rounded.csv"))




## move km plots to single folder ----
output_dir_plots <- path(output_dir, "plots")
dir_create(output_dir_plots)

metaparams %>%
  mutate(
    kmplotdir = here("output", cohort, matchset, "km", subgroup, outcome, "km_plot_unrounded.png"),
    kmplotnewdir = path(output_dir_plots, glue("km_plot_unrounded_{matchset}_{subgroup}_{outcome}.png")),
  ) %>%
  {walk2(.$kmplotdir, .$kmplotnewdir, ~file_copy(.x, .y, overwrite = TRUE))}

metaparams %>%
  mutate(
    kmplotdir = here("output", cohort, matchset, "km", subgroup, outcome, "km_plot_rounded.png"),
    kmplotnewdir = path(output_dir_plots, glue("km_plot_rounded_{matchset}_{subgroup}_{outcome}.png")),
  ) %>%
  {walk2(.$kmplotdir, .$kmplotnewdir, ~file_copy(.x, .y, overwrite = TRUE))}


## plot overall estimates for inspection ----

plot_estimates <- function(matchset, estimate, estimate.ll, estimate.ul, name){
 cohort0 <- cohort
 matchset0 <- matchset
  plot_temp <-
    contrasts_overall %>%
    filter(cohort0==cohort, matchset0==matchset) %>%
    group_by(outcome_descr) %>%
    mutate(
      outcome_descr = fct_relabel(outcome_descr, str_wrap, width=10),
      subgroup_level_descr = fct_rev(subgroup_level_descr),
    ) %>%
    ggplot(aes(y=subgroup_level_descr)) +
    geom_vline(aes(xintercept=0), linetype="dotted", colour="darkgrey")+
    geom_point(aes(x={{estimate}}))+
    geom_linerange(aes(xmin={{estimate.ll}}, xmax={{estimate.ul}}))+
    facet_grid(rows=vars(subgroup_descr), cols=vars(outcome_descr), scales="free", space="free_y", switch="y")+
    scale_x_continuous(expand = expansion(mult=c(0,0.01)))+
    labs(y=NULL)+
    theme_minimal()+
    theme(
      legend.position="bottom",
      axis.text.x.top=element_text(hjust=0),

      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_blank(),
      strip.placement="outside",
      #strip.text.y.left = element_text(angle=0),
      strip.text.y.left = element_blank(),

      panel.border = element_blank(),
      panel.spacing = unit(0.3, "lines"),
    )


  ggsave(
    filename=path(
      path(output_dir_plots, glue("overall_plot_rounded_{matchset}_{name}.png"))
    ),
    plot_temp,
    width=20, height=15, units="cm"
  )

  plot_temp
}

for (matchset in c("A", "B")){
  plot_estimates(matchset, rd, rd.ll, rd.ul, "rd")
  plot_estimates(matchset, rr, rr.ll, rr.ul, "rr")
  plot_estimates(matchset, irr, irr.ll, irr.ul, "irr")
}

