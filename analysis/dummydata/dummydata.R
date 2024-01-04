
library('tidyverse')
library('arrow')
library('here')
library('glue')


remotes::install_github("https://github.com/wjchulme/dd4d")
library('dd4d')


population_size <- 100000

# get nth largest value from list
nthmax <- function(x, n=1){
  dplyr::nth(sort(x, decreasing=TRUE), n)
}

nthmin <- function(x, n=1){
  dplyr::nth(sort(x, decreasing=FALSE), n)
}


source(here("analysis", "design", "design.R"))


covidvaxstart_date <- as.Date("2020-12-08")
studystart_date <- as.Date(study_dates$studystart_date)
studyend_date <- as.Date(study_dates$studyend_date)
followupend_date <- as.Date(study_dates$followupend_date)
index_date <- studystart_date

index_day <- 0L
covidvaxstart_day <- as.integer(covidvaxstart_date - index_date)
studystart_day <- as.integer(studystart_date - index_date)
studyend_day <- as.integer(studyend_date - index_date)


known_variables <- c(
  "index_date", "studystart_date", "studyend_date", "covidvaxstart_date",
  "index_day",  "studystart_day", "studyend_day", "covidvaxstart_day",
  NULL
)

sim_list = lst(

  boost_day = bn_node(
    ~(runif(n=..n, studystart_day, studyend_day)),
  ),

  boost_type = bn_node(
    ~rcat(n=..n, c("pfizerBA45","sanofi"), c(0.5,0.5)),
    needs = "boost_day",
  ),

  region = bn_node(
    variable_formula = ~rfactor(n=..n, levels=c(
      "North East",
      "North West",
      "Yorkshire and The Humber",
      "East Midlands",
      "West Midlands",
      "East",
      "London",
      "South East",
      "South West"
    ), p = c(0.2, 0.2, 0.3, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05))
  ),

  stp = bn_node(
    ~factor(as.integer(runif(n=..n, 1, 36)), levels=1:36)
  ),

  # msoa = bn_node(
  #   ~factor(as.integer(runif(n=..n, 1, 100)), levels=1:100),
  #   missing_rate = ~ 0.005
  # ),

  dereg_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+120)),
    missing_rate = ~0.99
  ),

  has_follow_up_previous_6weeks = bn_node(
    ~rbernoulli(n=..n, p=0.999)
  ),

  # practice_id = bn_node(
  #   ~as.integer(runif(n=..n, 1, 200))
  # ),

  age = bn_node(
    ~as.integer(rnorm(n=..n, mean=80, sd=14))
  ),

  age_july2023 = bn_node(~age),

  sex = bn_node(
    ~rfactor(n=..n, levels = c("female", "male", "intersex", "unknown"), p = c(0.51, 0.49, 0, 0)),
    missing_rate = ~0.001 # this is shorthand for ~(rbernoulli(n=..n, p = 0.2))
  ),

  ethnicity = bn_node(
    ~rfactor(n=..n, levels = c(1,2,3,4,5), p = c(0.8, 0.05, 0.05, 0.05, 0.05)),
    missing_rate = ~ 0.25
  ),

  # ethnicity_6_sus = bn_node(
  #   ~rfactor(n=..n, levels = c(0,1,2,3,4,5), p = c(0.1, 0.7, 0.05, 0.05, 0.05, 0.05)),
  #   missing_rate = ~ 0
  # ),

  imd = bn_node(
    ~as.integer(plyr::round_any(runif(n=..n, 1, 32000), 100)),
    missing_rate = ~0.02
  ),

  rural_urban = bn_node(
    ~as.integer(runif(n=..n, 1, 9+1)),
    missing_rate = ~ 0.1
  ),


  ### vaccination variables

  # covid vax any
  covid_vax_1_day = bn_node(
    ~runif(n=..n, covidvaxstart_day, covidvaxstart_day+365),
  ),
  covid_vax_2_day = bn_node(
    ~runif(n=..n, covid_vax_1_day+14, covid_vax_1_day+100),
    needs = "covid_vax_1_day"
  ),
  covid_vax_3_day = bn_node(
    ~runif(n=..n, covid_vax_2_day+250, covid_vax_2_day+350),
    missing_rate = ~0.3,
    needs = "covid_vax_2_day"
  ),
  covid_vax_4_day = bn_node(
    ~runif(n=..n, covid_vax_3_day+300, covid_vax_3_day+400),
    missing_rate = ~0.7,
    needs = "covid_vax_3_day"
  ),
  covid_vax_5_day = bn_node(
    ~runif(n=..n, covid_vax_4_day+300, covid_vax_4_day+400),
    missing_rate = ~0.9,
    needs = "covid_vax_4_day"
  ),
  covid_vax_6_day = bn_node(
    ~runif(n=..n, covid_vax_5_day+300, covid_vax_5_day+400),
    missing_rate = ~0.99,
    needs = "covid_vax_5_day"
  ),
  covid_vax_7_day = bn_node(
    ~runif(n=..n, covid_vax_6_day+300, covid_vax_6_day+400),
    missing_rate = ~0.99,
    needs = "covid_vax_6_day"
  ),
  covid_vax_8_day = bn_node(
    ~runif(n=..n, covid_vax_7_day+300, covid_vax_7_day+400),
    missing_rate = ~0.99,
    needs = "covid_vax_7_day"
  ),
  covid_vax_9_day = bn_node(
    ~runif(n=..n, covid_vax_8_day+300, covid_vax_8_day+400),
    missing_rate = ~0.99,
    needs = "covid_vax_8_day"
  ),
  covid_vax_10_day = bn_node(
    ~runif(n=..n, covid_vax_9_day+300, covid_vax_9_day+400),
    missing_rate = ~0.99,
    needs = "covid_vax_9_day"
  ),

  covid_vax_type_1 = bn_node(~rcat(n=..n, c("pfizer","az"), c(0.5,0.5)), needs = "covid_vax_1_day"),
  covid_vax_type_2 = bn_node(~if_else(runif(..n)<0.98, covid_vax_type_1, "az"), needs = "covid_vax_2_day"),
  covid_vax_type_3 = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), needs = "covid_vax_3_day"),
  covid_vax_type_4 = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), needs = "covid_vax_4_day"),
  covid_vax_type_5 = bn_node(~rcat(n=..n, c("pfizer","sanofi"), c(0.5,0.5)), needs = "covid_vax_5_day"),
  covid_vax_type_6 = bn_node(~rcat(n=..n, c("pfizer","sanofi"), c(0.5,0.5)), needs = "covid_vax_6_day"),
  covid_vax_type_7 = bn_node(~rcat(n=..n, c("pfizer","sanofi"), c(0.5,0.5)), needs = "covid_vax_7_day"),
  covid_vax_type_8 = bn_node(~rcat(n=..n, c("pfizer","sanofi"), c(0.5,0.5)), needs = "covid_vax_8_day"),
  covid_vax_type_9 = bn_node(~rcat(n=..n, c("pfizer","sanofi"), c(0.5,0.5)), needs = "covid_vax_9_day"),
  covid_vax_type_10 = bn_node(~rcat(n=..n, c("pfizer","sanofi"), c(0.5,0.5)), needs = "covid_vax_10_day"),


  ## occupation / residency


  hscworker = bn_node(
    ~rbernoulli(n=..n, p=0.01)
  ),
  care_home_tpp = bn_node(
    ~rbernoulli(n=..n, p = 0.01)
  ),

  care_home_code = bn_node(
    ~rbernoulli(n=..n, p = 0.01)
  ),


  ## baseline clinical variables

  bmi = bn_node(
    ~rfactor(n=..n, levels = c("Not obese", "Obese I (30-34.9)", "Obese II (35-39.9)", "Obese III (40+)"), p = c(0.5, 0.2, 0.2, 0.1)),
  ),

  asthma = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  chronic_neuro_disease = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  chronic_resp_disease = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  sev_obesity = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  diabetes = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  sev_mental = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  chronic_heart_disease = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  chronic_kidney_disease = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  chronic_liver_disease = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  cancer = bn_node( ~rbernoulli(n=..n, p = 0.01)),

  preg22_group = bn_node( ~rbernoulli(n = ..n, p = 0.001)),

  immdx = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  immrx = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  immunosuppressed = bn_node( ~immdx | immrx),
  asplenia = bn_node( ~rbernoulli(n=..n, p = 0.02)),
  solid_organ_transplant = bn_node( ~rbernoulli(n=..n, p = 0.01)),
  hiv_aids = bn_node( ~rbernoulli(n=..n, p = 0.01)),

  learndis = bn_node( ~rbernoulli(n=..n, p = 0.02)),

  endoflife = bn_node( ~rbernoulli(n=..n, p = 0.001)),
  housebound = bn_node( ~rbernoulli(n=..n, p = 0.001)),

  prior_covid_test_frequency = bn_node(
    ~as.integer(rpois(n=..n, lambda=3)),
    missing_rate = ~0
  ),

  inhospital = bn_node( ~rbernoulli(n=..n, p = 0.01)),

  ## pre-baseline events where event date is relevant

  primary_care_covid_case_0_day = bn_node(
    ~as.integer(runif(n=..n, boost_day-100, boost_day-1)),
    missing_rate = ~0.7
  ),

  covid_test_0_day = bn_node(
    ~as.integer(runif(n=..n, boost_day-100, boost_day-1)),
    missing_rate = ~0.7
  ),

  postest_0_day = bn_node(
    ~as.integer(runif(n=..n, boost_day-100, boost_day-1)),
    missing_rate = ~0.9
  ),

  covidemergency_0_day = bn_node(
    ~as.integer(runif(n=..n, boost_day-100, boost_day-1)),
    missing_rate = ~0.99
  ),


  covidadmitted_0_day = bn_node(
    ~as.integer(runif(n=..n, boost_day-100, boost_day-1)),
    missing_rate = ~0.99
  ),

  ## post-baseline events (outcomes)

  primary_care_covid_case_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+100)),
    missing_rate = ~0.7
  ),

  covid_test_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+100)),
    missing_rate = ~0.7
  ),

  postest_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+100)),
    missing_rate = ~0.7
  ),

  emergency_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+100)),
    missing_rate = ~0.8
  ),

  covidemergency_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+100)),
    missing_rate = ~0.8
  ),

  covidemergencyhosp_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+100)),
    missing_rate = ~0.85
  ),

  covidadmitted_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+100)),
    missing_rate = ~0.7
  ),

  covidcritcare_day = bn_node(
    ~covidadmitted_day,
    needs = "covidadmitted_day",
    missing_rate = ~0.7
  ),

  # coviddeath_day = bn_node(
  #   ~death_day,
  #   missing_rate = ~0.7,
  #   needs = "death_day"
  # ),

  death_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+100)),
    missing_rate = ~0.90
  ),

  death_cause_covid = bn_node(
    ~rbernoulli(n=..n, p = 0.3),
    needs="death_day"
  ),
  death_cause_fracture = bn_node(
    ~rbernoulli(n=..n, p = 0.05),
    needs="death_day"
  ),

  # fractures
  fractureemergency_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+100)),
    missing_rate = ~0.95
  ),

  fractureadmitted_day = bn_node(
    ~as.integer(runif(n=..n, boost_day, boost_day+100)),
    missing_rate = ~0.97
  ),


  # fracturedeath_day = bn_node(
  #   ~death_day,
  #   missing_rate = ~0.95,
  #   needs = "death_day"
  # ),

  # censor_date = bn_node(
  #   ~pmin(death_date, dereg_date, boost_date + 7*20, followupend_date, na.rm=TRUE)
  # ),

  covid_test_frequency = bn_node(
    ~as.integer(rpois(n=..n, lambda=1)),
    missing_rate = ~0
  ),

)
bn <- bn_create(sim_list, known_variables = known_variables)

bn_plot(bn)
bn_plot(bn, connected_only=TRUE)

set.seed(10)

dummydata <-bn_simulate(bn, pop_size = population_size, keep_all = FALSE, .id="patient_id")

dummydata_processed <- dummydata %>%
  #convert integer days to dates since index date and rename vars
  mutate(across(ends_with("_day"), ~ as.Date(as.character(index_date + .)))) %>%
  # rename covid vaccine products to tpp names
  mutate(across(starts_with("covid_vax_type_"), ~factor(., names(vax_product_lookup), vax_product_lookup))) %>%
  mutate(boost_type = factor(boost_type, names(vax_product_lookup), vax_product_lookup)) %>%
  rename_with(~str_replace(., "_day", "_date"), ends_with("_day"))


fs::dir_create(here("analysis", "dummydata"))
write_feather(dummydata_processed, sink = here("analysis", "dummydata", "dummyextract.arrow"))
