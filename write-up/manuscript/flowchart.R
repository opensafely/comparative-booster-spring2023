source(here::here("write-up", "manuscript", "setup.R"))
# source(here("write-up", "manuscript", "import-process.R"))

output_dir_qmd <- here("write-up", "manuscript", "figures_manual")

label_number_n <- label_number(1, 1, big.mark = ",")
label_perc_n <- label_percent(0.1)

fontsize <- 40
fontsize_heading <- 60
fontsize_heading_2 <- 45

# set x axis
lefttextx <-0.3
pfizerBA45_x <- .35
pfizerBA45_exclude_x <- .45
sanofi_x <- .55
sanofi_exclude_x <- .65
righttextx <- 0.7

# # set y axis
n_stages <- 5

y1 <- 0.85
y2 <- 0.75
y3 <- 0.65
y4 <- 0.15
y5 <- 0.05

included<-c(1,2,3,9,10)
excluded<-c(4,5,6,7,8)

for(i in included){
  assign(glue("textx_{i}"),lefttextx)
  assign(glue("justx_{i}"),"right")
}

for(i in excluded){
  assign(glue("textx_{i}"),righttextx)
  assign(glue("justx_{i}"),"left")
}

texty_1 <- y1
texty_2 <- y2
texty_3 <- y3
texty_4 <- 0.51
texty_5 <- 0.44
texty_6 <- 0.36
texty_7 <- 0.28
texty_8 <- 0.2
texty_9 <- y4
texty_10 <- y5

for (i in 1:5){
  assign(glue("excluded_3_{i}"),get(glue("texty_{i+3}")))
}

for (i in 1:(n_stages - 1)) {
  assign(glue("exclude_y{i}"), sum(get(glue("y{i}")), get(glue("y{i+1}"))) / 2 - .015)
}

exclude_y3 <- y3 - (exclude_y1-exclude_y2)/2 -.015

### format flowchart tibble
flowchart_format <- flowchart %>%
  summarise(
    criteria = case_when(
      criteria == "with no prior vaccine within 14 days and no prior PfizerBA45 or Sanofi" ~ "With no prior vaccine within 14 days and no prior Pfizer BA.4-5 or Sanofi",
      criteria == "no missing demographic information" ~ "Missing demographic information",
      criteria == "Received COVID-19 vaccine between 1 April and 30 June 2023" ~ "Received COVID-19 vaccine between 1 April and 30 June 2023",
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
          glue(
            get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c{i}")) %>% pull(n),
            get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c{i}")) %>% pull(pct_all),
            .sep = "\n"
          ),
          x = get(glue("{boost}_x")),
          y = get(glue("y{i}")),
          txt_gp = gpar(fontsize = fontsize)
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
          y = get(glue("exclude_y{i}")),
          txt_gp = gpar(fontsize = fontsize)
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
          .sep = "\n"),
        x = get(glue("{boost}_exclude_x")),
        y = exclude_y3,
        txt_gp = gpar(fontsize = fontsize)))
    for (i in 1:5){
      assign(
        glue("{boost}_{coho}_exclude_3_{i}"),
        boxGrob(
          glue(
            get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_1")) %>% pull(n_exclude),
            get(glue("{boost}_{coho}_tibble")) %>% filter(crit == glue("c4_1")) %>% pull(pct_exclude),
            .sep = "\n"),
          x = get(glue("{boost}_exclude_x")),
          y = get(glue("excluded_3_{i}")),
          txt_gp = gpar(fontsize = fontsize)))
    }

  }
}


## print boxes
for (coho in c("cv", "age75plus")) {
  png(glue("{output_dir_qmd}/flowchart_{coho}.png"), width = 2800, height = 2400, units = "px")
  grid.newpage()

  ### add headings

  ifelse(coho == "cv", heading <- "Clinically vulnerable",
         heading <- "Aged 75 years or over"
  )
  heading_cv <- grid.text(heading,
                          x = .48,
                          y = .94,
                          gp = gpar(fontsize = fontsize_heading)
  )

  heading_pfizerBA45 <- grid.text("Pfizer BA.4-5",
                                  x = pfizerBA45_x,
                                  y = .9,
                                  gp = gpar(fontsize = fontsize_heading_2)
  )

  heading_sanofi <- grid.text("Sanofi",
                              x = sanofi_x,
                              y = .9,
                              gp = gpar(fontsize = fontsize_heading_2)
  )

  #add text
  #### create text boxes
  for (i in c(1:nrow(pfizerBA45_cv_tibble))) {
    assign(
      glue("text_{i}"),
      grid.text(str_wrap(pfizerBA45_cv_tibble %>% slice(i) %>% pull(criteria),38),
                x = get(glue("textx_{i}")),
                y = get(glue("texty_{i}")),
                just = get(glue("justx_{i}")),
                gp = gpar(fontsize = fontsize_heading_2)
      )
    )
  }

  for (i in 1:4){
    grid.text("OR",
              x = righttextx+.08,
              y = mean(c(get(glue("excluded_3_{i}")),get(glue("excluded_3_{i+1}")))),
              just = "left",
              gp = gpar(fontsize = fontsize)
    )
  }
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

    for (i in 1:5){
      get(glue("{boost}_{coho}_exclude_3_{i}")) %>%
        print()
    }
    # Add connectors

    connectGrob(get(glue("{boost}_{coho}_exclude_3")), get(glue("{boost}_{coho}_exclude_3_1")), type = "N") %>%
      print()



    for (i in 1:4) {
      connectGrob(get(glue("{boost}_{coho}_exclude_3_{i}")), get(glue("{boost}_{coho}_exclude_3_{i+1}")), type = "N") %>%
        print()
      connectGrob(get(glue("{boost}_{coho}_{i}")), get(glue("{boost}_{coho}_{i+1}")), type = "N") %>%
        print()
      connectGrob(get(glue("{boost}_{coho}_{i}")), get(glue("{boost}_{coho}_exclude_{i}")), type = "N") %>%
        print()
    }
  }
  # print
  dev.off()
}
