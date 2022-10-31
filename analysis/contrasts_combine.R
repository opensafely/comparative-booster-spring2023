
# # # # # # # # # # # # # # # # # # # # #
# Purpose: Combine ci estimates from different outcomes
#  - import matched data
#  - adds outcome variable and restricts follow-up
#  - gets CI estimates
#  - The script must be accompanied by two arguments:
#    `matchset` - the matching set used for matching
#    `subgroup` - the subgroup variable, which is used to stratify CI estimates
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## import command-line arguments ----

args <- commandArgs(trailingOnly=TRUE)


if(length(args)==0){
  # use for interactive testing
  matchset <- "A"

} else {
  matchset <- args[[1]]

}

## Import libraries ----
library('tidyverse')
library('here')
library('glue')
library('survival')

## Import custom user functions from lib
source(here("lib", "functions", "utility.R"))

## Import design elements
source(here("lib", "design", "design.R"))


output_dir <- here("output", "match", matchset, "combined")
fs::dir_create(output_dir)

metaparams <-
  expand_grid(
    outcome = factor(c("postest", "covidemergency", "covidadmittedproxy1", "covidadmitted", "covidcritcare", "coviddeath", "noncoviddeath")),
    subgroup = factor(recoder$subgroups),
    #outcome = factor("covidadmitted"),
    #subgroup = factor("all"),
  ) %>%
  mutate(
    outcome_descr = fct_recoderelevel(outcome,  recoder$outcome),
    subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroups),
  )

## KM estimates ----

km_estimates <- metaparams %>%
  mutate(
    data = pmap(list(matchset, subgroup, outcome), function(matchset, subgroup, outcome) {
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", "match", matchset, "km", subgroup, outcome, "km_estimates_rounded.rds"))
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

write_rds(km_estimates, fs::path(output_dir, "km_estimates_rounded.csv"))


contrasts_daily <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup){
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", "match", matchset, "km", subgroup, outcome, "contrasts_daily_rounded.rds"))
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

write_csv(contrasts_daily, fs::path(output_dir, "contrasts_daily_rounded.csv"))


contrasts_cuts <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup){
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", "match", matchset, "km", subgroup, outcome, "contrasts_cuts_rounded.rds"))
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

write_csv(contrasts_cuts, fs::path(output_dir, "contrasts_cuts_rounded.csv"))


contrasts_overall <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup) {
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", "match", matchset, "km", subgroup, outcome, "contrasts_overall_rounded.rds"))
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

write_csv(contrasts_overall, fs::path(output_dir, "contrasts_overall_rounded.csv"))



## delayed entry KM estimates ----

delayedentry_estimates <- metaparams %>%
  filter(subgroup=="all") %>%
  mutate(
    data = pmap(list(matchset, subgroup, outcome), function(matchset, subgroup, outcome) {
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", "match", matchset, "delayedentry", subgroup, outcome, "km_estimates_rounded.rds"))
      dat %>%
        ungroup() %>%
        add_column(
          subgroup_level = as.character(.[[subgroup]]),
          subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
          .before=1
        ) %>%
        select(-all_of(subgroup))
    })
  ) %>%
  unnest(data)

write_rds(delayedentry_estimates, fs::path(output_dir, "contrasts_era_estimates_rounded.csv"))


delayedentry_contrasts_daily <- metaparams %>%
  filter(subgroup=="all") %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup){
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", "match", matchset, "delayedentry", subgroup, outcome, "contrasts_daily_rounded.rds"))
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

write_csv(delayedentry_contrasts_daily, fs::path(output_dir, "contrasts_era_daily_rounded.csv"))


delayedentry_contrasts_cuts <- metaparams %>%
  filter(subgroup=="all") %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup){
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", "match", matchset, "delayedentry", subgroup, outcome, "contrasts_cuts_rounded.rds"))
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

write_csv(delayedentry_contrasts_cuts, fs::path(output_dir, "contrasts_era_cuts_rounded.csv"))


delayedentry_contrasts_overall <- metaparams %>%
  filter(subgroup=="all") %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup) {
      subgroup <- as.character(subgroup)
      dat <- read_rds(here("output", "match", matchset, "delayedentry", subgroup, outcome, "contrasts_overall_rounded.rds"))
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

write_csv(delayedentry_contrasts_overall, fs::path(output_dir, "contrasts_era_overall_rounded.csv"))



## move km plots to single folder ----
fs::dir_create(here("output", "match", matchset, "combined", "plots"))

metaparams %>%
  mutate(
    kmplotdir = here("output", "match", matchset, "km", subgroup, outcome, "km_plot_unrounded.png"),
    kmplotnewdir = here("output", "match", matchset, "combined", "plots", glue("km_plot_unrounded_{subgroup}_{outcome}.png")),
  ) %>%
  {walk2(.$kmplotdir, .$kmplotnewdir, ~fs::file_copy(.x, .y, overwrite = TRUE))}

metaparams %>%
  mutate(
    kmplotdir = here("output", "match", matchset, "km", subgroup, outcome, "km_plot_rounded.png"),
    kmplotnewdir = here("output", "match", matchset, "combined", "plots", glue("km_plot_rounded_{subgroup}_{outcome}.png")),
  ) %>%
  {walk2(.$kmplotdir, .$kmplotnewdir, ~fs::file_copy(.x, .y, overwrite = TRUE))}


## plot overall estimates for inspection ----


plot_estimates <- function(estimate, estimate.ll, estimate.ul, name){

  plot_temp <-
    contrasts_overall %>%
    group_by(outcome_descr) %>%
    mutate(
      outcome_descr = fct_relabel(outcome_descr, str_wrap, width=10),
      subgroup_level_descr = fct_rev(subgroup_level_descr),

    ) %>%
    ggplot(aes(y=subgroup_level_descr)) +
    geom_vline(aes(xintercept=0), linetype="dotted", colour="darkgrey")+
    geom_point(aes(x={{estimate}}), position=position_dodge(width=-0.3))+
    geom_linerange(aes(xmin={{estimate.ll}}, xmax={{estimate.ul}}), position=position_dodge(width=-0.3))+
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
    filename=fs::path(
      here("output", "match", matchset, "combined", "plots", glue("overall_plot_rounded_{name}.png"))
    ),
    plot_temp,
    width=20, height=15, units="cm"
  )

  plot_temp
}

plot_estimates(rd, rd.ll, rd.ul, "rd")
plot_estimates(rr, rr.ll, rr.ul, "rr")
plot_estimates(coxhr, coxhr.ll, coxhr.ul, "coxhr")
plot_estimates(irr, irr.ll, irr.ul, "irr")

