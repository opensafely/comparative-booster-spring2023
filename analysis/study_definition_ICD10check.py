
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
)

# Import codelists from codelists.py
import codelists

# import json module
import json

# import study dates defined in "./lib/design/study-dates.R" script
with open("./lib/design/study-dates.json") as f:
  study_dates = json.load(f)

# change these in design.R if necessary
studystart_date = study_dates["studystart_date"] 
studyend_date = study_dates["studyend_date"]


## Functions for extracting a series of time dependent variables
# These define study defintion variable signatures such that
# variable_1_date is the the first event date on or after the index date
# variable_2_date is the first event date strictly after variable_2_date
# ...
# variable_n_date is the first event date strictly after variable_n-1_date


def vaccination_date_X(name, index_date, n, product_name_matches=None, target_disease_matches=None):
  # vaccination date, given product_name
  def var_signature(
    name,
    on_or_after,
    product_name_matches,
    target_disease_matches
  ):
    return {
      name: patients.with_tpp_vaccination_record(
        product_name_matches=product_name_matches,
        target_disease_matches=target_disease_matches,
        on_or_after=on_or_after,
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD"
      ),
    }
    
  variables = var_signature(f"{name}_1_date", index_date, product_name_matches, target_disease_matches)
  for i in range(2, n+1):
    variables.update(var_signature(
      f"{name}_{i}_date", 
      f"{name}_{i-1}_date + 1 days",
      # pick up subsequent vaccines occurring one day or later -- people with unrealistic dosing intervals are later excluded
      product_name_matches,
      target_disease_matches
    ))
  return variables



# Specify study defeinition
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "2021-01-01", "latest": studyend_date},
    "rate": "uniform",
    "incidence": 0.2,
    "int": {"distribution": "normal", "mean": 1000, "stddev": 100},
    "float": {"distribution": "normal", "mean": 25, "stddev": 5},
  },
  
  #index_date = index_date,
  
  # This line defines the study population
  population=patients.satisfying(
    f"""
      registered
      AND
      age >= 18
      AND
      NOT has_died
      AND 
      anycovidvax_3_date >= startdate
      AND
      anycovidvax_3_date <= enddate
    """,
    # we define baseline variables on the day _before_ the study date (start date = day of first possible booster vaccination)
    registered=patients.registered_as_of(
      "anycovidvax_3_date - 1 day",
    ),
    has_died=patients.died_from_any_cause(
      on_or_before="anycovidvax_3_date - 1 day",
      returning="binary_flag",
    ), 
   
    startdate = patients.fixed_value(studystart_date),
    enddate = patients.fixed_value(studyend_date),
  
  ),
  
  
  #################################################################
  ## Covid vaccine dates
  #################################################################
  
  # any covid vaccine
  **vaccination_date_X(
    name = "anycovidvax",
    index_date = "1900-01-01",
    n = 4,
    target_disease_matches="SARS-2 CORONAVIRUS"
  ),
  
  
  
  
  ###############################################################################
  ## Admin and demographics
  ###############################################################################
  
  dereg_date=patients.date_deregistered_from_all_supported_practices(
    on_or_after="anycovidvax_3_date",
    date_format="YYYY-MM-DD",
  ),
  
  death_date=patients.died_from_any_cause(
    on_or_after="anycovidvax_3_date",
    returning="date_of_death",
    date_format="YYYY-MM-DD",
  ),

  
  age=patients.age_as_of( 
    "anycovidvax_3_date - 1 day",
  ),
  
  sex=patients.sex(
    return_expectations={
      "rate": "universal",
      "category": {"ratios": {"M": 0.49, "F": 0.51}},
      "incidence": 1,
    }
  ),
  
  # ######################################################
  # # To ascertain prior covid-related hospital admissions
  # # anywhere on reason for admission
  # ######################################################
  # 
  # covidadmitted_U071_date=patients.admitted_to_hospital(
  #   returning="date_admitted",
  #   with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
  #   with_these_diagnoses=codelist(["U071"], system="icd10"),
  #   on_or_before="anycovidvax_3_date - 1 day",
  #   date_format="YYYY-MM-DD",
  #   find_last_match_in_period=True,
  # ),
  # 
  # covidadmitted_U072_date=patients.admitted_to_hospital(
  #   returning="date_admitted",
  #   with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
  #   with_these_diagnoses=codelist(["U072"], system="icd10"),
  #   on_or_before="anycovidvax_3_date - 1 day",
  #   date_format="YYYY-MM-DD",
  #   find_last_match_in_period=True,
  # ),
  # 
  # covidadmitted_U099_date=patients.admitted_to_hospital(
  #   returning="date_admitted",
  #   with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
  #   with_these_diagnoses=codelist(["U099"], system="icd10"),
  #   on_or_before="anycovidvax_3_date - 1 day",
  #   date_format="YYYY-MM-DD",
  #   find_last_match_in_period=True,
  # ),
  # 
  # covidadmitted_U109_date=patients.admitted_to_hospital(
  #   returning="date_admitted",
  #   with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
  #   with_these_diagnoses=codelist(["U109"], system="icd10"),
  #   on_or_before="anycovidvax_3_date - 1 day",
  #   date_format="YYYY-MM-DD",
  #   find_last_match_in_period=True,
  # ),
  # 
  # ######################################################
  # # To ascertain prior covid-related hospital admissions
  # # primary diagnosis only
  # ######################################################
  # 
  # covidadmitted_U071_date=patients.admitted_to_hospital(
  #   returning="date_admitted",
  #   with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
  #   with_these_primary_diagnoses=codelist(["U071"], system="icd10"),
  #   on_or_before="anycovidvax_3_date - 1 day",
  #   date_format="YYYY-MM-DD",
  #   find_last_match_in_period=True,
  # ),
  # 
  # covidadmitted_U072_date=patients.admitted_to_hospital(
  #   returning="date_admitted",
  #   with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
  #   with_these_primary_diagnoses=codelist(["U072"], system="icd10"),
  #   on_or_before="anycovidvax_3_date - 1 day",
  #   date_format="YYYY-MM-DD",
  #   find_last_match_in_period=True,
  # ),
  # 
  # covidadmitted_U099_date=patients.admitted_to_hospital(
  #   returning="date_admitted",
  #   with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
  #   with_these_primary_diagnoses=codelist(["U099"], system="icd10"),
  #   on_or_before="anycovidvax_3_date - 1 day",
  #   date_format="YYYY-MM-DD",
  #   find_last_match_in_period=True,
  # ),
  # 
  # covidadmitted_U109_date=patients.admitted_to_hospital(
  #   returning="date_admitted",
  #   with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
  #   with_these_primary_diagnoses=codelist(["U109"], system="icd10"),
  #   on_or_before="anycovidvax_3_date - 1 day",
  #   date_format="YYYY-MM-DD",
  #   find_last_match_in_period=True,
  # ),
  # 
  

  ######################################################
  # To ascertain covid-related hospital admissions
  # anywhere on reason for admission
  ######################################################
  
  admitted_any_U071_date=patients.admitted_to_hospital(
    returning="date_admitted",
    on_or_after="anycovidvax_3_date",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_patient_classification = ["1"], # ordinary admissions only
    with_these_diagnoses=codelist(["U071"], system="icd10"),
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True,
  ),
  
  admitted_any_U072_date=patients.admitted_to_hospital(
    returning="date_admitted",
    on_or_after="anycovidvax_3_date",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_patient_classification = ["1"], # ordinary admissions only
    with_these_diagnoses=codelist(["U072"], system="icd10"),
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True,
  ),
  
  admitted_any_U099_date=patients.admitted_to_hospital(
    returning="date_admitted",
    on_or_after="anycovidvax_3_date",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_patient_classification = ["1"], # ordinary admissions only
    with_these_diagnoses=codelist(["U099"], system="icd10"),
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True,
  ),
  
  admitted_any_U109_date=patients.admitted_to_hospital(
    returning="date_admitted",
    on_or_after="anycovidvax_3_date",
    # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
    # see https://docs.opensafely.org/study-def-variables/#sus for more info
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_patient_classification = ["1"], # ordinary admissions only
    with_these_diagnoses=codelist(["U109"], system="icd10"),
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True,
  ),
  
  
  ######################################################
  # To ascertain covid-related hospital admissions
  # primary reason for admission only
  ######################################################
  
  admitted_primary_U071_date=patients.admitted_to_hospital(
    returning="date_admitted",
    on_or_after="anycovidvax_3_date",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_patient_classification = ["1"], # ordinary admissions only
    with_these_primary_diagnoses=codelist(["U071"], system="icd10"),
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True,
  ),
  
  admitted_primary_U072_date=patients.admitted_to_hospital(
    returning="date_admitted",
    on_or_after="anycovidvax_3_date",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_patient_classification = ["1"], # ordinary admissions only
    with_these_primary_diagnoses=codelist(["U072"], system="icd10"),
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True,
  ),
  
  admitted_primary_U099_date=patients.admitted_to_hospital(
    returning="date_admitted",
    on_or_after="anycovidvax_3_date",
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_patient_classification = ["1"], # ordinary admissions only
    with_these_primary_diagnoses=codelist(["U099"], system="icd10"),
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True,
  ),
  
  admitted_primary_U109_date=patients.admitted_to_hospital(
    returning="date_admitted",
    on_or_after="anycovidvax_3_date",
    # see https://github.com/opensafely-core/cohort-extractor/pull/497 for codes
    # see https://docs.opensafely.org/study-def-variables/#sus for more info
    with_admission_method=["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"],
    with_patient_classification = ["1"], # ordinary admissions only
    with_these_primary_diagnoses=codelist(["U109"], system="icd10"),
    date_format="YYYY-MM-DD",
    find_first_match_in_period=True,
  ),
  
  ######################################################
  # To ascertain covid-related mortality
  # any cause of death position
  ######################################################
  
  
  # Covid-related death
  death_any_U071_date=patients.with_these_codes_on_death_certificate(
    codelist(["U071"], system="icd10"),
    returning="date_of_death",
    date_format="YYYY-MM-DD",
  ),
  
  death_any_U072_date=patients.with_these_codes_on_death_certificate(
    codelist(["U072"], system="icd10"),
    returning="date_of_death",
    date_format="YYYY-MM-DD",
  ),
  
  death_any_U099_date=patients.with_these_codes_on_death_certificate(
    codelist(["U099"], system="icd10"),
    returning="date_of_death",
    date_format="YYYY-MM-DD",
  ),
  
  death_any_U109_date=patients.with_these_codes_on_death_certificate(
    codelist(["U109"], system="icd10"),
    returning="date_of_death",
    date_format="YYYY-MM-DD",
  ),
  
  ######################################################
  # To ascertain covid-related mortality
  # underlying cause of death only
  ######################################################
  
  
  # Covid-related death
  death_primary_U071_date=patients.with_these_codes_on_death_certificate(
    codelist(["U071"], system="icd10"),
    returning="date_of_death",
    match_only_underlying_cause=True,
    date_format="YYYY-MM-DD",
  ),
  
  death_primary_U072_date=patients.with_these_codes_on_death_certificate(
    codelist(["U072"], system="icd10"),
    returning="date_of_death",
    match_only_underlying_cause=True,
    date_format="YYYY-MM-DD",
  ),
  
  death_primary_U099_date=patients.with_these_codes_on_death_certificate(
    codelist(["U099"], system="icd10"),
    returning="date_of_death",
    match_only_underlying_cause=True,
    date_format="YYYY-MM-DD",
  ),
  
  death_primary_U109_date=patients.with_these_codes_on_death_certificate(
    codelist(["U109"], system="icd10"),
    returning="date_of_death",
    match_only_underlying_cause=True,
    date_format="YYYY-MM-DD",
  ),

)
