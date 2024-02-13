source(here::here("write-up", "manuscript", "setup.R"))
source(here("write-up", "manuscript", "import-process.R"))

output_dir_qmd <- here("write-up", "manuscript", "figures_manual")

label_number_n <- label_number(1, 1, big.mark = ",")
label_perc_n <- label_percent(0.1)

# set x axis
pfizerBA45_x <- .15
pfizerBA45_exclude_x <- .35
sanofi_x <- .6
sanofi_exclude_x <- .8

# # set y axis
n_stages <- 5

y1 <- 0.8
y2 <- 0.7
y3 <- 0.6
y4 <- 0.2
y5 <- 0.1

for (i in 1:(n_stages - 1)) {
  assign(glue("exclude_y{i}"), sum(get(glue("y{i}")), get(glue("y{i+1}"))) / 2 - .01)
}

### format flowchart tibble
flowchart_format <- flowchart %>%
  summarise(
    criteria = case_when(criteria == "no missing demographic information" ~ "Missing demographic information",
      criteria == "not a health and social care worker" ~ "Health / social care worker",
      criteria == "not end-of-life" ~ "Receiving end-of-life care",
      criteria == "no documented COVID-19 infection/disease within prior 28 days" ~ "Documented COVID-19 infection/disease within prior 28 days",
      criteria == "not admitted in hospital at time of booster" ~ "Admitted in hospital at time of booster",
      criteria == "and successfully matched" ~ "Successfully matched",
      .default = str_to_sentence(criteria)
    ),
    n = label_number_n(n),
    pct_all = label_perc_n(pct_all),
    n_exclude = label_number_n(n_exclude),
    pct_exclude = label_perc_n(pct_exclude),
    boost_type,
    crit,
    cohort
  )

## create filtered tibbles
for (boost in c("pfizerBA45", "sanofi")) {
  for (coho in c("cv", "age75plus")) {
    assign(
      glue("{boost}_{coho}_tibble"),
      flowchart_format %>%
        filter(
          boost_type == boost,
          cohort == coho
        )
    )

#### create inclusion boxes
    for (i in c(1:5)) {
      assign(
        glue("{boost}_{coho}_{i}"),
        boxGrob(
          glue(get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c{i}")) %>% pull(criteria),
            get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c{i}")) %>% pull(n),
            get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c{i}")) %>% pull(pct_all),
            .sep = "\n"
          ),
          x = get(glue("{boost}_x")),
          y = get(glue("y{i}"))
        )
      )
    }


    ### exclusion boxes 1,2 & 4
    for (i in c(1, 2, 4)) {
      assign(
        glue("{boost}_{coho}_exclude_{i}"),
        boxGrob(
          glue("Excluded",
            get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c{i+1}")) %>% pull(n_exclude),
            get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c{i+1}")) %>% pull(pct_exclude),
            .sep = "\n"
          ),
          x = get(glue("{boost}_exclude_x")),
          y = get(glue("exclude_y{i}"))
        )
      )
    }

    #### Exclusion box 3
    assign(
      glue("{boost}_{coho}_exclude_3"),
      boxGrob(
        glue(
          "Excluded",
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4")) %>% pull(n_exclude),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4")) %>% pull(pct_exclude),
          "",
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_1")) %>% pull(criteria),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_1")) %>% pull(n_exclude),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_1")) %>% pull(pct_exclude),
          "",
          "OR",
          "",
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_2")) %>% pull(criteria),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_2")) %>% pull(n_exclude),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_2")) %>% pull(pct_exclude),
          "",
          "OR",
          "",
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_3")) %>% pull(criteria),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_3")) %>% pull(n_exclude),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_3")) %>% pull(pct_exclude),
          "",
          "OR",
          "",
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_4")) %>% pull(criteria),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_4")) %>% pull(n_exclude),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_4")) %>% pull(pct_exclude),
          "",
          "OR",
          "",
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_5")) %>% pull(criteria),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_5")) %>% pull(n_exclude),
          get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_5")) %>% pull(pct_exclude),
          .sep = "\n"
        ),
        x = get(glue("{boost}_exclude_x")),
        y = exclude_y3
      )
    )
  }
}


## print boxes
for (coho in c("cv", "age75plus")) {
  png(glue("{output_dir_qmd}/flowchart_{coho}.png"), width = 1800, height = 1800, units = "px")
  grid.newpage()

  ### add headings

  ifelse(coho == "cv", heading <- "Clinically vulnerable",
    heading <- "Aged 75 years or over"
  )
  heading_cv <- grid.text(heading,
    x = .48,
    y = .94,
    gp = gpar(fontsize = 30)
  )

  heading_pfizerBA45 <- grid.text("Pfizer BA.4-5",
    x = pfizerBA45_x,
    y = .88,
    gp = gpar(fontsize = 30)
  )

  heading_sanofi <- grid.text("Sanofi",
    x = sanofi_x,
    y = .88,
    gp = gpar(fontsize = 30)
  )

  for (boost in c("pfizerBA45", "sanofi")) {
    # add inclusion boxes
    for (i in 1:5) {
      get(glue("{boost}_{coho}_{i}")) %>%
        print()
    }

    # add exclusion boxes
    for (i in 1:4) {
      get(glue("{boost}_{coho}_exclude_{i}")) %>%
        print()
    }

    # Add connectors
    for (i in 1:4) {
      connectGrob(get(glue("{boost}_{coho}_{i}")), get(glue("{boost}_{coho}_{i+1}")), type = "N") %>%
        print()
      connectGrob(get(glue("{boost}_{coho}_{i}")), get(glue("{boost}_{coho}_exclude_{i}")), type = "N") %>%
        print()
    }
  }
  # print
  dev.off()
}
