from ehrql import codelist_from_csv


covid_icd10 = codelist_from_csv(
    "codelists/opensafely-covid-identification.csv",
    column="icd10_code",
)

# overwrite imported codelist to add additional "Multisystem inflammatory syndrome associated with COVID-19, unspecified" code
# see "Note on coding of the coronavirus (COVID-19)" here: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/methodologies/userguidetomortalitystatisticsjuly2017
covid_icd10 = ["U071", "U072", "U109"]

covid_emergency = ["1240751000000100"]


covid_primary_care_positive_test = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
    column="CTV3ID",
)

covid_primary_care_code = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-clinical-code.csv",
    column="CTV3ID",
)

covid_primary_care_sequelae = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-sequelae.csv",
    column="CTV3ID",
)

covid_primary_care_probable_combined = (
    covid_primary_care_positive_test +
    covid_primary_care_code + 
    covid_primary_care_sequelae
)

covid_primary_care_suspected_covid_advice = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-suspected-covid-advice.csv",
    column="CTV3ID",
)
covid_primary_care_suspected_covid_had_test = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-suspected-covid-had-test.csv",
    column="CTV3ID",
)
covid_primary_care_suspected_covid_isolation = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-suspected-covid-isolation-code.csv",
    column="CTV3ID",
)
covid_primary_care_suspected_covid_nonspecific_clinical_assessment = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-suspected-covid-nonspecific-clinical-assessment.csv",
    column="CTV3ID",
)
covid_primary_care_suspected_covid_exposure = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-exposure-to-disease.csv",
    column="CTV3ID",
)
primary_care_suspected_covid_combined = (
    covid_primary_care_suspected_covid_advice +
    covid_primary_care_suspected_covid_had_test +
    covid_primary_care_suspected_covid_isolation +
    covid_primary_care_suspected_covid_exposure 
)

ethnicity = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    column="Code",
    category_column="Grouping_6",
)
ethnicity_16 = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    column="Code",
    category_column="Grouping_16",
)

# 
# solid_organ_transplantation = codelist_from_csv(
#     "codelists/opensafely-solid-organ-transplantation.csv",
#     column="CTV3ID",
# )
# 
# lung_cancer = codelist_from_csv(
#     "codelists/opensafely-lung-cancer.csv", column="CTV3ID",
# )
# haematological_cancer = codelist_from_csv(
#     "codelists/opensafely-haematological-cancer.csv", column="CTV3ID",
# )
# bone_marrow_transplant = codelist_from_csv(
#     "codelists/opensafely-bone-marrow-transplant.csv", column="CTV3ID",
# )
# cystic_fibrosis = codelist_from_csv(
#     "codelists/opensafely-cystic-fibrosis.csv", column="CTV3ID",
# )
# 
# sickle_cell_disease = codelist_from_csv(
#     "codelists/opensafely-sickle-cell-disease.csv", column="CTV3ID",
# )
# 
# permanent_immunosuppression = codelist_from_csv(
#     "codelists/opensafely-permanent-immunosuppression.csv",
#     column="CTV3ID",
# )
# temporary_immunosuppression = codelist_from_csv(
#     "codelists/opensafely-temporary-immunosuppression.csv",
#     column="CTV3ID",
# )
# chronic_cardiac_disease = codelist_from_csv(
#     "codelists/opensafely-chronic-cardiac-disease.csv", column="CTV3ID",
# )
# learning_disability = codelist_from_csv(
#     "codelists/opensafely-learning-disabilities.csv",
#     column="CTV3Code",
# )
# downs_syndrome = codelist_from_csv(
#     "codelists/opensafely-down-syndrome.csv",
#     column="code",
# )
# cerebral_palsy = codelist_from_csv(
#     "codelists/opensafely-cerebral-palsy.csv",
#     column="code",
# )
# learning_disability_including_downs_syndrome_and_cerebral_palsy = combine_codelists(
#     learning_disability,
#     downs_syndrome,
#     cerebral_palsy,
# )
# dialysis = codelist_from_csv(
#     "codelists/opensafely-dialysis.csv", column="CTV3ID",
# )
# other_respiratory_conditions = codelist_from_csv(
#     "codelists/opensafely-other-respiratory-conditions.csv",
#     column="CTV3ID",
# )
# heart_failure = codelist_from_csv(
#     "codelists/opensafely-heart-failure.csv", column="CTV3ID",
# )
# other_heart_disease = codelist_from_csv(
#     "codelists/opensafely-other-heart-disease.csv", column="CTV3ID",
# )
# 
# chronic_cardiac_disease = codelist_from_csv(
#     "codelists/opensafely-chronic-cardiac-disease.csv", column="CTV3ID",
# )
# 
# chemotherapy_or_radiotherapy = codelist_from_csv(
#     "codelists/opensafely-chemotherapy-or-radiotherapy.csv",
#     column="CTV3ID",
# )
# cancer_excluding_lung_and_haematological = codelist_from_csv(
#     "codelists/opensafely-cancer-excluding-lung-and-haematological.csv",
#     column="CTV3ID",
# )
# 
# current_copd = codelist_from_csv(
#     "codelists/opensafely-current-copd.csv", column="CTV3ID"
# )
# 
# dementia = codelist_from_csv(
#     "codelists/opensafely-dementia-complete.csv", column="code"
# )
# 
# diabetes = codelist_from_csv(
#     "codelists/opensafely-diabetes.csv", column="CTV3ID"
# )
# 
# dmards = codelist_from_csv(
#     "codelists/opensafely-dmards.csv", column="snomed_id",
# )
# 
# 
# chronic_liver_disease = codelist_from_csv(
#     "codelists/opensafely-chronic-liver-disease.csv", column="CTV3ID",
# )
# other_neuro = codelist_from_csv(
#     "codelists/opensafely-other-neurological-conditions.csv",
#     column="CTV3ID",
# )
# 
# psychosis_schizophrenia_bipolar_affective_disease = codelist_from_csv(
#     "codelists/opensafely-psychosis-schizophrenia-bipolar-affective-disease.csv",
#     column="CTV3Code",
# )
# 
# asplenia = codelist_from_csv(
#     "codelists/opensafely-asplenia.csv", column="CTV3ID"
# )


## PRIMIS


# Patients in long-stay nursing and residential care
carehome = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-longres.csv", 
    column="code",
)


# High Risk from COVID-19 code
shield = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-shield.csv",
    column="code",
)

# Lower Risk from COVID-19 codes
nonshield = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-nonshield.csv",
    column="code",
)



# Asthma Diagnosis code
ast = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ast.csv",
  column="code",
)

# Asthma Admission codes
astadm = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-astadm.csv",
  column="code",
)

# Asthma systemic steroid prescription codes
astrx = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-astrx.csv",
  column="code",
)

# Chronic Respiratory Disease
resp_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-resp_cov.csv",
  column="code",
)

# Chronic heart disease codes
chd_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-chd_cov.csv",
  column="code",
)

# Chronic kidney disease diagnostic codes
ckd_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd_cov.csv",
  column="code",
)

# Chronic kidney disease codes - all stages
ckd15 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd15.csv",
  column="code",
)

# Chronic kidney disease codes-stages 3 - 5
ckd35 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-ckd35.csv",
  column="code",
)

# Chronic Liver disease codes
cld = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cld.csv",
  column="code",
)

# Diabetes diagnosis codes
diab = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-diab.csv",
  column="code",
)

# Immunosuppression diagnosis codes
immdx_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immdx_cov.csv",
  column="code",
)

# Immunosuppression medication codes
immrx = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-immrx.csv",
  column="code",
)

# Chronic Neurological Disease including Significant Learning Disorder
cns_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-cns_cov.csv",
  column="code",
)

# Asplenia or Dysfunction of the Spleen codes
spln_cov = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-spln_cov.csv",
  column="code",
)

# BMI
bmi = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-bmi.csv",
  column="code",
)

# All BMI coded terms
bmi_stage = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-bmi_stage.csv",
  column="code",
)

# Severe Obesity code recorded
sev_obesity = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-sev_obesity.csv",
  column="code",
)

# Diabetes resolved codes
dmres = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-dmres.csv",
  column="code",
)

# Severe Mental Illness codes
sev_mental = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-sev_mental.csv",
  column="code",
)

# Remission codes relating to Severe Mental Illness
smhres = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-smhres.csv",
  column="code",
)


# to represent household contact of shielding individual
hhld_imdef = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-hhld_imdef.csv",
  column="code",
)

# Wider Learning Disability
learndis = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-learndis.csv",
  column="code",
)

# Carer codes
carer = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-carer.csv",
  column="code",
)

# No longer a carer codes
notcarer = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-notcarer.csv",
  column="code",
)

# Employed by Care Home codes
carehomeemployee = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-carehome.csv",
  column="code",
)

# Employed by nursing home codes
nursehomeemployee = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-nursehome.csv",
  column="code",
)

# Employed by domiciliary care provider codes
domcareemployee = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-domcare.csv",
  column="code",
)



# Cancer

cancer_haem_snomed=codelist_from_csv(
    "codelists/opensafely-haematological-cancer-snomed.csv",
    column="id",
)

cancer_nonhaem_nonlung_snomed=codelist_from_csv(
    "codelists/opensafely-cancer-excluding-lung-and-haematological-snomed.csv",
    column="id",
)

cancer_lung_snomed=codelist_from_csv(
    "codelists/opensafely-lung-cancer-snomed.csv",
    column="id",
)

chemotherapy_radiotherapy_snomed = codelist_from_csv(
  "codelists/opensafely-chemotherapy-or-radiotherapy-snomed.csv", 
  column = "id"
)

cancer_nonhaem_snomed = (
  cancer_nonhaem_nonlung_snomed + 
  cancer_lung_snomed + 
  chemotherapy_radiotherapy_snomed
)

# solid organ transplant
solid_organ_transplant=codelist_from_csv(
    "codelists/opensafely-solid-organ-transplantation-snomed.csv",
    column="id",
)

# HIV/AIDS
hiv_aids=codelist_from_csv(
    "codelists/nhsd-hiv-aids-snomed.csv",
    column="code",
)


# end of life

eol = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-palcare_cod.csv",
    column="code",
)

midazolam = codelist_from_csv(
    "codelists/opensafely-midazolam-end-of-life.csv",
    column="dmd_id",   
)

housebound = codelist_from_csv(
    "codelists/opensafely-housebound.csv", 
    column="code"
)

no_longer_housebound = codelist_from_csv(
    "codelists/opensafely-no-longer-housebound.csv", 
    column="code"
)

discharged_to_hospital = ["306706006", "1066331000000109", "1066391000000105"]

fractures_icd10 = codelist_from_csv(
    "codelists/opensafely-fractures.csv",
    column="code"
)

fractures_snomedECDS = [
    "371162008","81639003","430984009","66112004","60667009","269062008","207938004",
    "207957008","13695006","207974008","766775007","91037003","33173003","29749002",
    "43295006","302222008","111640008","71555008","53627009","29045004","208322000",
    "208371005","9468002","208394006","208403005","704213001","24424003","359817006",
    "25415003","428151000","80756009","447139008","447395005","413877007","42188001",
    "64665009","342070009","81576005","371161001","111609001","40613008","87225004",
    "45910007","269070003","207949005","207965006","207977001","767262002","15474008",
    "111637008","47864008","89294002","302232001","42945005","37449000","81966000",
    "34578006","208341002","29014003","1370007","208420009","704236005","21698002",
    "361118003","28576007","428019004","111643005","446979005","447017008","414943006",
    "481870042","4948002","367527001", "74395007", "95851007"
  ]

