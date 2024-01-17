import json
from pathlib import Path

from ehrql import Dataset, case, days, years, when, minimum_of
from ehrql.tables.beta.tpp import (
  patients, 
  practice_registrations, 
  addresses,
  medications, 
  clinical_events as events, 
  vaccinations, 
  occupation_on_covid_vaccine_record as vaxx_job, 
  ons_deaths,
  sgss_covid_all_tests as covid_tests,
  emergency_care_attendances as emergency,
  hospital_admissions as admissions,
)
from variables_lib import (
    create_sequential_variables,
    date_deregistered_from_all_supported_practices,
    has_a_continuous_practice_registration_spanning,
    most_recent_bmi,
    practice_registration_as_of,
)

import codelists



#######################################################################################
# Import study dates defined in "./lib/design/study-dates.R" script and then exported
# to JSON
#######################################################################################
study_dates = json.loads(
    Path("analysis/design/study-dates.json").read_text(),
)

# Change these in ./lib/design/study-dates.R if necessary
studystart_date = study_dates["studystart_date"]
studyend_date = study_dates["studyend_date"]
followupend_date = study_dates["followupend_date"]
firstpossiblevax_date = study_dates["firstpossiblevax_date"]

#######################################################################################
# Vaccine and eligibility info
#######################################################################################


covid_vaccinations = (
  vaccinations
  .where(vaccinations.target_disease.is_in(["SARS-2 CORONAVIRUS"]))
  .sort_by(vaccinations.date)
)

# first vaccinations occurring on or after study start date is taken to be the booster date
spring2023_boosters = covid_vaccinations.where(
    (covid_vaccinations.date>=studystart_date) & 
    (covid_vaccinations.date<=studyend_date)
  ).first_for_patient()

# booster dates
boost_date = spring2023_boosters.date

# We define baseline variables on the day _before_ the study date 
baseline_date = boost_date - days(1)

# address info as at booster date
address = addresses.for_patient_on(boost_date)

# registration info as at booster date
registered = practice_registrations.for_patient_on(boost_date)


#######################################################################################
# Initialise dataset
#######################################################################################

dataset = Dataset()

dataset.boost_date = boost_date

dataset.boost_type = spring2023_boosters.product_name

#######################################################################################
# Admin and demographics
#######################################################################################


# administrative region of practice
dataset.region = registered.practice_nuts1_region_name
# STP
dataset.stp = registered.practice_stp
# practice deregistration date
dataset.dereg_date = registered.end_date
#Pseudo practice ID
#dataset.practice_id = registered.practice_pseudo_id
# patient has continuous practice registration at least 6 weeks prior to boost date
dataset.has_follow_up_previous_6weeks = has_a_continuous_practice_registration_spanning(
    start_date=boost_date - days(6 * 7),
    end_date=boost_date,
)
# Middle Super Output Area
#dataset.msoa = address.msoa_code

# Age
dataset.age = patients.age_on(baseline_date)

# Age on 1 july, For eligiblity definition 
# "Operational flexibility was permitted to offer the booster to eligible individuals 
# expected to reach the target age during the spring campaign"
dataset.age_july2023 = patients.age_on("2023-07-01")
# Sex
dataset.sex = patients.sex
# Ethnicity in 6 categories
dataset.ethnicity = (
    events.where(events.ctv3_code.is_in(codelists.ethnicity))
    .sort_by(events.date)
    .last_for_patient()
    .ctv3_code.to_category(codelists.ethnicity)
)
# Rurality
dataset.rural_urban = address.rural_urban_classification
# Index of Multiple Deprevation Rank (rounded down to nearest 100)
dataset.imd = address.imd_rounded



#######################################################################################
# COVID-19 vaccination history
#######################################################################################

#  retrieve vaccination history for up to 10 vaccines

# Arbitrary date guaranteed to be before any events of interest
previous_vax_date = "1899-01-01"

for i in range(1, 10+1):

    current_vax = covid_vaccinations.where(covid_vaccinations.date>previous_vax_date).first_for_patient()
    setattr(dataset, f"covid_vax_{i}_date", current_vax.date)
    setattr(dataset, f"covid_vax_type_{i}", current_vax.product_name)
    previous_vax_date = current_vax.date


#######################################################################################
# Common functions for contructing queries
#######################################################################################


# events occurring before booster date
prior_events = events.where(events.date.is_on_or_before(baseline_date))

# query prior_events for existence of event-in-codelist
def has_prior_event(codelist, where=True):
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .exists_for_patient()
    )

# query prior_events for date of most recent event-in-codelist
def last_prior_event(codelist, where=True):
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(events.date)
        .last_for_patient()
    )

# query prior_events for date of earliest event-in-codelist
def first_prior_event(codelist, where=True):
    return (
        prior_events.where(where)
        .where(prior_events.snomedct_code.is_in(codelist))
        .sort_by(events.date)
        .first_for_patient()
    )

# meds occurring before booster date
prior_meds = medications.where(medications.date.is_on_or_before(baseline_date))

# query prior_meds for existence of event-in-codelist
def has_prior_meds(codelist, where=True):
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .exists_for_patient()
    )
    
# query prior meds for date of most recent med-in-codelist
def last_prior_meds(codelist, where=True):
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .sort_by(medications.date)
        .last_for_patient()
    )

# query prior_events for date of earliest event-in-codelist
def first_prior_meds(codelist, where=True):
    return (
        prior_meds.where(where)
        .where(prior_meds.dmd_code.is_in(codelist))
        .sort_by(medications.date)
        .first_for_patient()
    )

# prior covid test dates from SGSS
prior_tests = covid_tests.where(
    covid_tests.specimen_taken_date.is_on_or_before(baseline_date)
)

import operator
from functools import reduce
def any_of(conditions):
    return reduce(operator.or_, conditions)


from ehrql.codes import CTV3Code, ICD10Code

# query if emergency attentance diagnosis codes match a given codelist
def emergency_diagnosis_matches(codelist):
    conditions = [
        getattr(emergency, column_name).is_in(codelist)
        for column_name in [f"diagnosis_{i:02d}" for i in range(1, 25)]
    ]
    return emergency.where(any_of(conditions))


# query if hospital admission diagnosis codes (all_diagnoses) match a given codelist
def admission_diagnosis_matches(codelist):
    code_strings = set()
    for code in codelist:
        # Pass the string through the ICD10Code constructor to validate that it has
        # the expected format
        code_string = ICD10Code(code)._to_primitive_type()
        code_strings.add(code_string)
    conditions = [
        # The reason a plain substring search like this works is twofold:
        #
        # * ICD-10 codes all start with the sequence [A-Z][0-9] and do not contain
        #   such a sequence in any other part of the code. In this sense they are
        #   suffix-free and two codes will only match at they start if they match at
        #   all.
        #
        # * Although the codes are not prefix-free they are organised hierarchically
        #   such that code A0123 represents a child concept of code A01. So although
        #   the naive substring matching below will find code A01 if code A0123 is
        #   present, this happens to be the behaviour we actually want.
        #
        # Obviously this is all far from ideal though, and later we hope to be able
        # to pull these codes out in a separate table and handle the matching
        # properly.
        admissions.all_diagnoses.contains(code_str)
        for code_str in code_strings
    ]
    return admissions.where(any_of(conditions))



# query if causes of death match a given codelist
def cause_of_death_matches(codelist):
    conditions = [
        getattr(ons_deaths, column_name).is_in(codelist)
        for column_name in (["underlying_cause_of_death"]+[f"cause_of_death_{i:02d}" for i in range(1, 16)])
    ]
    return any_of(conditions)


primary_care_covid_events = events.where(
    events.ctv3_code.is_in(
        codelists.covid_primary_care_code
        + codelists.covid_primary_care_positive_test
        + codelists.covid_primary_care_sequelae
    )
)

# #######################################################################################
# # Occupation / residency
# #######################################################################################

# # Health or social care worker
dataset.hscworker = vaxx_job.where(vaxx_job.is_healthcare_worker).exists_for_patient()

# # TPP care home flag
dataset.care_home_tpp = address.care_home_is_potential_match.if_null_then(False)
# Patients in long-stay nursing and residential care
dataset.care_home_code = has_prior_event(codelists.carehome)


# #######################################################################################
# # Clinical information as at (day before) booster date
# #######################################################################################


# BMI
bmi_measurement = most_recent_bmi(
    where=events.date.is_after(baseline_date - days(5 * 365)),
    minimum_age_at_measurement=16,
)
bmi_value = bmi_measurement.numeric_value

dataset.bmi = case(
    when(bmi_value < 30).then("Not obese"), # include this here to ensure this value is the 1st level in the factor
    when((bmi_value >= 30.0) & (bmi_value < 35.0)).then("Obese I (30-34.9)"),
    when((bmi_value >= 35.0) & (bmi_value < 40.0)).then("Obese II (35-39.9)"),
    # Set maximum to avoid any impossibly extreme values being classified as obese
    when((bmi_value >= 40.0) & (bmi_value < 100.0)).then("Obese III (40+)"),
    default="Not obese", # assume missing is non-obese
)


# From PRIMIS

# Asthma Diagnosis code date
astdx = has_prior_event(codelists.ast)

# Asthma Admission codes
astadm = has_prior_event(
  codelists.astadm,
  where=events.date.is_on_or_after(baseline_date - days(730))
)

# Asthma nebuliser code date
astrxm1_date = last_prior_meds(
  codelists.astrxm1,
  where=medications.date.is_on_or_after(baseline_date - days(365))
).date

# # Asthma systemic steroid prescription code date
# astrxm2_date = prior_meds(
#   codelists.astrxm2,
#   where=medications.date.is_on_or_after(baseline_date - days(730))
# ).date

# Earliest systemic steroid prescription in first 2 year window
astrxm2e1_date = first_prior_meds(
  codelists.astrxm2, 
  where=medications.date.is_on_or_after("2018-12-01") 
  & medications.date.is_before("2020-12-01")
).date

# Latest systemic steroid prescription in first 2 year window 
astrxm2l1_date = last_prior_meds(
  codelists.astrxm2, 
  where=medications.date.is_on_or_after("2018-12-01") 
  & medications.date.is_before("2020-12-01")
).date

# Earliest systemic steroid prescription in second 2 year window
astrxm2e2_date = first_prior_meds(
  codelists.astrxm2, 
  where=medications.date.is_on_or_after("2020-12-01") 
  & medications.date.is_before("2022-12-01")
).date

# Latest systemic steroid prescription in second 2 year window 
astrxm2l2_date = last_prior_meds(
  codelists.astrxm2, 
  where=medications.date.is_on_or_after("2020-12-01") 
  & medications.date.is_before("2022-12-01")
).date

# Earliest systemic steroid prescription in third 2 year window
astrxm2e3_date = first_prior_meds(
  codelists.astrxm2, 
  where=medications.date.is_on_or_after("2022-12-01") 
  & medications.date.is_before("2024-12-01")
).date

# Latest systemic steroid prescription in third 2 year window 
astrxm2l3_date = last_prior_meds(
  codelists.astrxm2, 
  where=medications.date.is_on_or_after("2022-12-01") 
  & medications.date.is_before("2024-12-01")
).date


dataset.asthma = case(
  when(astadm).then(True),
  when((astdx & astrxm1_date.is_not_null())
    & ((astrxm2e1_date != astrxm2l1_date) & (astrxm2l1_date.is_before(astrxm2e1_date + days(730))))).then(True),
  when((astdx & astrxm1_date.is_not_null())
    & ((astrxm2e2_date != astrxm2l2_date) & (astrxm2l2_date.is_before(astrxm2e2_date + days(730))))).then(True),
  when((astdx & astrxm1_date.is_not_null())
    & ((astrxm2e3_date != astrxm2l3_date) & (astrxm2l3_date.is_before(astrxm2e3_date + days(730))))).then(True),
  when((astdx & astrxm1_date.is_not_null())
    & (astrxm2e2_date.is_before(astrxm2l1_date + days(730)))).then(True),  
  when((astdx & astrxm1_date.is_not_null())
    & (astrxm2e3_date.is_before(astrxm2l2_date + days(730)))).then(True),
  default=False
)

# redfine Asthma as per green book
# Poorly controlled asthma is defined as:
# - ≥2 courses of oral corticosteroids in the preceding 24 months OR
# - on maintenance oral corticosteroids OR
# - ≥1 hospital admission for asthma in the preceding 24 months

# Inhaled asthma prescription in previous year
astrx_inhaled = has_prior_meds(
  codelists.astrxm1,
  where=medications.date.is_on_or_after(baseline_date - days(365))
)

# count of systemic steroid prescription inpast 2 years
astrx_oral_count = (
  prior_meds
    .where(prior_meds.dmd_code.is_in(codelists.astrxm2))
    .where(prior_meds.date.is_on_or_between(baseline_date - years(2), baseline_date))
    .count_for_patient()
)

dataset.asthma_simple = case(
  when(astadm).then(True),
  #TODO add asthma admission from SUS data too?
  when(astdx & astrx_inhaled & (astrx_oral_count>=2)).then(True),
  default=False
)

# Chronic Neurological Disease including Significant Learning Disorder
dataset.chronic_neuro_disease = has_prior_event(codelists.cns_cov)

# Chronic Respiratory Disease
resp_cov = has_prior_event(codelists.resp_cov)
dataset.chronic_resp_disease = dataset.asthma | resp_cov

# Severe Obesity
bmi_stage_event = last_prior_event(codelists.bmi_stage)
sev_obesity_event = last_prior_event(
    codelists.sev_obesity,
    where=((events.date >= bmi_stage_event.date) & (events.numeric_value != 0.0)),
)
bmi_event = last_prior_event(codelists.bmi, where=(events.numeric_value != 0.0))

dataset.sev_obesity = case(
    when(sev_obesity_event.date > bmi_event.date).then(True),
    when(bmi_event.numeric_value >= 40.0).then(True),
    default=False
)

# Pregnancy delivery code date
pregAdel_date = last_prior_event(
  codelists.pregdel,
  where = (
    events.date.is_on_or_between(baseline_date - days(7*65), baseline_date - days((7*30)+1))
  )
).date

# Pregnancy date window A
pregA_date = last_prior_event(
  codelists.preg,
  where = (
    events.date.is_on_or_between(baseline_date - days((7*65)), baseline_date - days((7*30)+1))
  )
).date

# Pregnancy date window B
pregB = has_prior_event(
  codelists.preg,
  where = (
    events.date.is_on_or_between(baseline_date - days((7*30)), baseline_date) # up to 8 months prior 
  )
)

# Pregnancy group
dataset.preg_group = case(
    when(pregB).then(True),
    when(
      (pregAdel_date.is_not_null() &
      pregA_date.is_not_null() &
      pregA_date.is_on_or_after(pregAdel_date))
    ).then(True),
    default=False
)

# Diabetes and diabetes resolution code date
diab_date = last_prior_event(codelists.diab).date
dmres_date = last_prior_event(codelists.dmres).date

# Addisons and hypothyroidism code date
addis = has_prior_event(codelists.addis)

# Gestational diabetes dates and group
gdiab = has_prior_event(codelists.gdiab)
gdiab_group = gdiab & dataset.preg_group

# Diabetes group
dataset.diabetes = case(
    when(dmres_date < diab_date).then(True),
    when(diab_date.is_not_null() & dmres_date.is_null()).then(True),
    when(addis).then(True),
    when(gdiab_group).then(True),
    default=False
)

# Severe Mental Illness codes
sev_mental_date = last_prior_event(codelists.sev_mental).date
# Remission codes relating to Severe Mental Illness
smhres_date = last_prior_event(codelists.smhres).date

dataset.sev_mental = case(
    when(smhres_date < sev_mental_date).then(True),
    when(sev_mental_date.is_not_null() & smhres_date.is_null()).then(True),
    default=False
)

# Chronic heart disease codes
dataset.chronic_heart_disease = has_prior_event(codelists.chd_cov)

# Chronic kidney disease diagnostic codes
ckd = has_prior_event(codelists.ckd_cov)
# Chronic kidney disease codes - all stages
ckd15_date = last_prior_event(codelists.ckd15).date
# Chronic kidney disease codes-stages 3 - 5
ckd35_date = last_prior_event(codelists.ckd35).date

dataset.chronic_kidney_disease = case(
    when(ckd).then(True),
    when((ckd35_date >= ckd15_date)).then(True),
    default=False
)

# Chronic Liver disease codes
dataset.chronic_liver_disease = has_prior_event(codelists.cld)

# cancer within last 3 years
dataset.cancer = has_prior_event(
    codelists.cancer_nonhaem_snomed + codelists.cancer_haem_snomed,
    where=events.date.is_after(baseline_date - days(int(3 * 365.25))),
)

# Immunosuppression diagnosis 
immdx = has_prior_event(codelists.immdx_cov)
dataset.immdx = immdx

# Immunosuppression medication 
immrx = has_prior_meds(
    codelists.immrx,
    where=(medications.date.is_on_or_after(baseline_date - days(int(3 * 365.25))))
)
dataset.immrx = immrx

# Immunosuppression admin date
immadm = has_prior_event(
    codelists.immadm,
    where=(events.date.is_on_or_after(baseline_date - days(int(3 * 365.25))))
)

# Chemotherapy medication date
dxt_chemo = has_prior_event(
  codelists.dxt_chemo,
  where=(events.date.is_on_or_after(baseline_date - days(int(3 * 365.25))))
)
dataset.dxt_chemo = dxt_chemo

# Immunosuppression group
dataset.immunosuppressed = immdx | immrx | immadm | dxt_chemo

# Asplenia or Dysfunction of the Spleen codes
dataset.asplenia = has_prior_event(codelists.spln_cov)

# Organs transplant
dataset.solid_organ_transplant = has_prior_event(codelists.solid_organ_transplant)

# HIV/AIDS
dataset.hiv_aids = has_prior_event(codelists.hiv_aids)

# Wider Learning Disability
dataset.learndis = has_prior_event(codelists.learndis)


# To represent household contact of shielding individual
#dataset.hhld_imdef_dat = last_prior_event(codelists.hhld_imdef).date


#######################################################################################
# Vulnerable / high risk group eligible if under 75
#######################################################################################


# End of life
endoflife_coding = has_prior_event(codelists.eol)
midazolam = has_prior_meds(codelists.midazolam)
dataset.endoflife = midazolam | endoflife_coding

# Housebound
housebound_date = last_prior_event(codelists.housebound).date
no_longer_housebound = has_prior_event(
    codelists.no_longer_housebound,
    where=events.date.is_on_or_after(housebound_date),
)
moved_into_care_home = has_prior_event(
    codelists.carehome,
    where=events.date.is_on_or_after(housebound_date),
)

# is patient "housebound"
dataset.housebound = (
    housebound_date.is_not_null() & ~no_longer_housebound & ~moved_into_care_home
)

# number of covid test in the 6 months prior to boost date
dataset.prior_covid_test_frequency = prior_tests.where(
    prior_tests.specimen_taken_date.is_after(baseline_date - days(26 * 7))
).count_for_patient()

# Overnight hospital admission at time of 3rd / booster dose
dataset.inhospital = (
    admissions.where(admissions.admission_date.is_on_or_before(boost_date))
    .where(admissions.discharge_date.is_on_or_after(boost_date))
    .where(
        admissions.admission_method.is_in(
            [
                "11",
                "12",
                "13",
                "21",
                "2A",
                "22",
                "23",
                "24",
                "25",
                "2D",
                "28",
                "2B",
                "81",
            ]
        )
    )
    # Ordinary admissions only
    .where(admissions.patient_classification == "1")
    .exists_for_patient()
)




# #######################################################################################
# # Pre-baseline events where event date is of interest
# #######################################################################################

# covid diagnosis in primary care prior to baseline date
dataset.primary_care_covid_case_0_date = (
    primary_care_covid_events.where(events.date.is_on_or_before(baseline_date))
    .sort_by(events.date)
    .last_for_patient()
    .date
)

# most recent covid test prior to baseline date
dataset.covid_test_0_date = prior_tests.specimen_taken_date.maximum_for_patient()

# most recent positive covid test prior to baseline date
dataset.postest_0_date = prior_tests.where(
    prior_tests.is_positive
).specimen_taken_date.maximum_for_patient()


# Emergency attendance for covid  prior to baseline date
dataset.covidemergency_0_date = (
    emergency_diagnosis_matches(codelists.covid_emergency)
    .where(emergency.arrival_date.is_on_or_before(baseline_date))
    .sort_by(emergency.arrival_date)
    .last_for_patient()
    .arrival_date
)

# Positive covid admission prior to baseline date
dataset.covidadmitted_0_date = (
    admission_diagnosis_matches(codelists.covid_icd10)
    .where(admissions.admission_date.is_on_or_before(baseline_date))
    .where(
        admissions.admission_method.is_in(
            ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"]
        )
    )
    .sort_by(admissions.admission_date)
    .last_for_patient()
    .admission_date
)



#######################################################################################
# Post-baseline variables (outcomes)
#######################################################################################


### Effectiveness outcomes 

# Positive case identification after study start date
dataset.primary_care_covid_case_date = (
    primary_care_covid_events.where(events.date.is_on_or_after(boost_date))
    .sort_by(events.date)
    .first_for_patient()
    .date
)

# Covid test dates from SGSS
post_baseline_tests = covid_tests.where(
    covid_tests.specimen_taken_date.is_on_or_after(boost_date)
)

# any covid test
dataset.covid_test_date = post_baseline_tests.specimen_taken_date.minimum_for_patient()

# Positive covid test
dataset.postest_date = post_baseline_tests.where(
    post_baseline_tests.is_positive
).specimen_taken_date.minimum_for_patient()


# Post baseline date emergency care attendance
def post_baseline_ec_date(diagnoses=None, where=True):
    return (
        (
            emergency_diagnosis_matches(diagnoses)
            if diagnoses
            else emergency
        )
        .where(emergency.arrival_date.is_on_or_after(boost_date))
        .where(where)
        .sort_by(emergency.arrival_date)
        .first_for_patient()
        .arrival_date
    )


# Any emergency attendance
dataset.emergency_date = post_baseline_ec_date(diagnoses=None)

# Emergency attendance for covid, as per discharge diagnosis
dataset.covidemergency_date = post_baseline_ec_date(codelists.covid_emergency)


# Post baseline date hosptial admission
def post_baseline_admission_date(codelist=None, where=True):
    return (
        (admission_diagnosis_matches(codelist) if codelist else admissions)
        .where(admissions.admission_date.is_on_or_after(boost_date))
        # Ordinary admissions only
        .where(admissions.patient_classification == "1")
        .where(where)
        .sort_by(admissions.admission_date)
        .first_for_patient()
        .admission_date
    )

# covid-related admission 
dataset.covidadmitted_date = post_baseline_admission_date(
    codelist=codelists.covid_icd10,
    where=admissions.admission_method.is_in(
        ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"]
    ),
)

# covid-related admission to critical care
dataset.covidcritcare_date = post_baseline_admission_date(
    codelist=codelists.covid_icd10,
    where=(admissions.admission_method.is_in(
        ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"]
    )) & 
    (admissions.days_in_critical_care>0),
)

# all-cause death
dataset.death_date = ons_deaths.date

# covid-related death (stated anywhere on death certificate)
dataset.death_cause_covid = cause_of_death_matches(codelists.covid_icd10)



### Safety outcomes (within 28 days)

dataset.pericarditisemergency_date = post_baseline_ec_date(codelists.pericarditis_snomedECDS)

dataset.pericarditisadmitted_date = post_baseline_admission_date(
  codelist = codelists.pericarditis_icd10,
  where = admissions.admission_method.is_in(
    ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"]
  ),
)

dataset.myocarditisemergency_date = post_baseline_ec_date(codelists.myocarditis_snomedECDS)

dataset.myocarditisadmitted_date = post_baseline_admission_date(
  codelist = codelists.myocarditis_icd10,
  where = admissions.admission_method.is_in(
    ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"]
  ),
)


# thrombocytopaenia
# Thrombosis with Thrombocytopenia Syndrome (TTS)
# anaphalaxis
# thromboembolic events
# venous or arterial events
# Guillain-Barre syndrome ?



### Negative control outcomes 

dataset.fractureemergency_date = post_baseline_ec_date(codelists.fractures_snomedECDS)

dataset.fractureadmitted_date = post_baseline_admission_date(
    codelist=codelists.fractures_icd10,
    where=admissions.admission_method.is_in(
        ["21", "22", "23", "24", "25", "2A", "2B", "2C", "2D", "28"]
    ),
)

# fracture-related death (stated anywhere on death certificate)
#dataset.fracturedeath_date = death_cause_matches(codelists.fractures_icd10).date
dataset.death_cause_fracture = cause_of_death_matches(codelists.fractures_icd10)
dataset.death_cause_pericarditis = cause_of_death_matches(codelists.pericarditis_icd10)
dataset.death_cause_myocarditis = cause_of_death_matches(codelists.myocarditis_icd10)

## SAFETY outcomes


# censor date (minimum of deregistration or death or 20 weeks after baseline)
#dataset.censor_date = minimum_of(dataset.death_date, dataset.dereg_date, boost_date + days(7*26), followupend_date)

# number of covid tests up to 26 weeks after baseline or until end of follow-up
dataset.covid_test_frequency = post_baseline_tests.where(
    post_baseline_tests.specimen_taken_date.is_before(minimum_of(boost_date + days(7*26), followupend_date))
).count_for_patient()


# #######################################################################################
# # Population
# #######################################################################################

# From Green Book chapter 14a 
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1186479/Greenbook-chapter-14a-4September2023.pdf
# 
# The vast majority of people aged over 75 years reached an interval of around six months
# from their last dose between late March and June 2023. Operational flexibility was
# permitted to offer the booster to eligible individuals expected to reach the target age
# during the spring campaign. Boosters were offered around six months from the previous
# dose, but could be given a minimum of three months from the previous dose; this was
# particularly important to facilitate delivery of the programme to residents in care homes
# and the housebound.


# define dataset poppulation
dataset.define_population(
  spring2023_boosters.exists_for_patient() & 
  (dataset.age_july2023 >= 50) &
  (registered.exists_for_patient()) & 
  ((dataset.death_date >= dataset.boost_date) | dataset.death_date.is_null())
)
