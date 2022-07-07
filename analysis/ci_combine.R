
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


output_dir <- here("output", "match", matchset, "ci", "combined")
fs::dir_create(output_dir)

metaparams <-
  expand_grid(
    outcome = factor(c("postest", "covidemergency", "covidadmittedproxy1", "covidadmitted", "covidcritcare", "coviddeath", "noncoviddeath")),
    subgroup = factor(recoder$subgroups),
  ) %>%
  mutate(
    outcome_descr = fct_recoderelevel(outcome,  recoder$outcome),
    subgroup_descr = fct_recoderelevel(subgroup,  recoder$subgroups),
  ) %>%
  filter(
    subgroup != "variantera"
  )


ci_estimates <- metaparams %>%
  mutate(
    data = pmap(list(matchset, subgroup, outcome), function(matchset, subgroup, outcome) {
      subgroup <- as.character(subgroup)
      dat <- read_csv(here("output", "match", matchset, "ci", subgroup, outcome, "ci_estimates.csv"), na="NA", col_types = cols())
      dat %>%
      add_column(
        subgroup_level = as.character(.[[subgroup]]),
        subgroup_level_descr = fct_recoderelevel(.[[subgroup]], recoder[[subgroup]]),
        .before=1
      ) %>%
      select(-all_of(subgroup))
    })
  ) %>%
  unnest(data)

write_csv(ci_estimates, fs::path(output_dir, "ci_estimates.csv"))


contrasts_daily <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup){
      subgroup <- as.character(subgroup)
      dat <- read_csv(here("output", "match", matchset, "ci", subgroup, outcome, "contrasts_daily.csv"), na="NA", col_types = cols())
      dat %>%
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

write_csv(contrasts_daily, fs::path(output_dir, "contrasts_daily.csv"))


contrasts_cuts <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup){
      subgroup <- as.character(subgroup)
      dat <- read_csv(here("output", "match", matchset, "ci", subgroup, outcome, "contrasts_cuts.csv"), na="NA", col_types = cols())
      dat %>%
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

write_csv(contrasts_cuts, fs::path(output_dir, "contrasts_cuts.csv"))


contrasts_overall <- metaparams %>%
  mutate(
    data = pmap(list(matchset, outcome, subgroup), function(matchset, outcome, subgroup) {
      subgroup <- as.character(subgroup)
      dat <- read_csv(here("output", "match", matchset, "ci", subgroup, outcome, "contrasts_overall.csv"), na="NA", col_types = cols())
      dat %>%
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

write_csv(contrasts_overall, fs::path(output_dir, "contrasts_overall.csv"))


## move ci plots to single folder ----
fs::dir_create(here("output", "match", matchset, "ci", "combined", "plots"))

metaparams %>%
  mutate(
    ciplotdir = here("output", "match", matchset, "ci", subgroup, outcome, "ci_plot.png"),
    ciplotnewdir = here("output", "match", matchset, "ci", "combined", "plots", glue("ci_plot_{subgroup}_{outcome}.png")),
  ) %>%
  {walk2(.$ciplotdir, .$ciplotnewdir, ~fs::file_copy(.x, .y, overwrite = TRUE))}

metaparams %>%
  mutate(
    ciplotdir = here("output", "match", matchset, "ci", subgroup, outcome, "ci_plot_rounded.png"),
    ciplotnewdir = here("output", "match", matchset, "ci", "combined", "plots", glue("ci_plot_rounded_{subgroup}_{outcome}.png")),
  ) %>%
  {walk2(.$ciplotdir, .$ciplotnewdir, ~fs::file_copy(.x, .y, overwrite = TRUE))}


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
      here("output", "match", matchset, "ci", "combined", "plots", glue("overall_plot_rounded_{name}.png"))
    ),
    plot_temp,
    width=20, height=15, units="cm"
  )

  plot_temp
}

plot_estimates(cird, cird.ll, cird.ul, "cird")
plot_estimates(cirr, cirr.ll, cirr.ul, "cirr")
plot_estimates(coxhr, coxhr.ll, coxhr.ul, "coxhr")
plot_estimates(kmirr, kmirr.ll, kmirr.ul, "kmirr")

