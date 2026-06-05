test_that("Diabetes simple", {
  skip_if_no_openai()
  
  expect_no_error({
    cdm <- TestGenerator::patientsCDM(
      testName = "diabetes_simple",
      cdmVersion = "5.4"
    )
    cdm$person
    cdm$condition_occurrence
    cdm$drug_exposure
  })
  
})

test_that("Diabetes workflow prompt", {
  skip_if_no_openai()
  
  # Get a glimpse of all available models 
  models <- PatientGenerator::availableModels()
  
  # Create a chat instance 
  patientGenerator <- patientChat$new(model = "gpt-5.5")

  # Prompt a detailed description of test patients
  patientGenerator$prompt(
  "Population (person table):
    - 10 adult patients
    - 5 female, use gender_concept_id = 8532
    - 5 male, use gender_concept_id = 8507

   Observation Period:
    - Start date between date of birth each person and end of observation 2025-12-31

   Condition Occurrence:
     - All patients must have Diabetes (condition_concept_id: 201826)
     - Condition start date between 2015-01-01 and 2020-12-31

   Drug Exposure:
     - All patients must have three Semaglutide (drug_concept_id: 19079450).
     - Drug exposure in a window of 0 to 30 days after index date

   Measurement:
     - All patients must have Fasting glucose (measurement_concept_id: 3018251)

   Procedure cccurrence:
     - 50% of patients (5 patients) must have Amputation of toe (procedure_concept_id: 4159766).

   Output Requirements:
    - Fill only specified tables in this prompt
    - All patients in person have an observation period
    - Fill out end dates in every table where you can"
    )
  
  # Save patients 
  patientGenerator$save("test_diabetes_patients")
  
  # JSON ready to load into TestGenerator and create a CDM reference
  cdm <- TestGenerator::patientsCDM(
    testName = "test_diabetes_patients",
    cdmVersion = "5.4"
  )
  
  cdm$person |> 
    collect() |> 
    nrow() |> 
    expect_equal(10)
  
  # Test number of females
  cdm$person |> 
    collect() |> 
    dplyr::filter(gender_concept_id == 8532) |> 
    nrow() |> 
    expect_equal(5)
  
  cdm$condition_occurrence |>
    collect() |>
    pull(condition_start_date) |>
    (\(x) all(x > as.Date("2015-01-01"), na.rm = TRUE))() |> 
    expect_true()
  
 cdm$condition_occurrence |>
    collect() |>
    pull(condition_start_date) |>
    (\(x) all(x < as.Date("2020-12-31"), na.rm = TRUE))() |> 
    expect_true()
 
 expect_no_error({
   cdm$diabetes_semaglutide <- CohortConstructor::conceptCohort(
     cdm = cdm,
     conceptSet = list(
       "diabetes" = 201826L,
       "semaglutide" = 19079450L
       ),
     name = "diabetes_semaglutide",
     exit = "event_end_date"
   )
 })
 
 expect_no_error({
   cdm$diabetes_semaglutide |> 
     CohortCharacteristics::summariseCharacteristics() |> 
     CohortCharacteristics::tableCharacteristics(
       type = "flextable",
       style = "darwin"
     )
 })
 
 expect_no_error({
   cdm$diabetes_semaglutide |> 
     CohortCharacteristics::summariseCharacteristics(
       cohortId = 1, #diabetes
       cohortIntersectFlag = list(
         targetCohortTable = "diabetes_semaglutide",
         targetCohortId = "semaglutide",
         window = c(0, 30)
       )
     ) |> 
     CohortCharacteristics::tableCharacteristics(
       type = "flextable",
       style = "darwin"
     )
 })
 
 patientGenerator$prompt(
   "Within the current records in the drug exposure tables, each drug exposure should be 30 days long"
 )
 
 patientGenerator$save("test_diabetes_patients_30")
 
 cdm <- TestGenerator::patientsCDM(
   testName = "test_diabetes_patients_30",
   cdmVersion = "5.4"
 )
 
 cdm$drug_exposure %>% 
   select(person_id,
          drug_exposure_start_date,
          drug_exposure_end_date) %>%
   mutate(days = !!CDMConnector::datediff(
     "drug_exposure_start_date",
     "drug_exposure_end_date"
     )
     ) %>%
   pull(days) %>% 
   unique() %>% 
   expect_equal(30)
 
})

test_that("Ovarian cancer stages", {
  skip_if_no_openai()
  
  patientGenerator <- PatientGenerator::patientChat$new()
  
  patientGenerator$prompt({
    "Five females (all over 18 years old) have an observation period from 2000 to 2024.

     All five have a condition occurrence of ovarian cancer (concept ID: 200051) recorded on 2012-01-01.

     Cancer stage information is recorded in the **measurement** table as follows:

      - **Female 1**:
      - Stage 1 (concept ID: 1633306) on 2012-01-01
      - Stage 2 (concept ID: 1634209) on 2012-01-02

      - **Female 2**:
      - Stage 2 (concept ID: 1634209) on 2012-01-01
      - Stage 3 (concept ID: 1633650) on 2012-01-02

      - **Female 3**:
      - Stage 3 (concept ID: 1633650) on 2012-01-01
      - Stage 4 (concept ID: 1634766) on 2012-01-02

      - **Female 4**:
      - Stage 1 (concept ID: 1633306) recorded (date not specified)

      - **Female 5**:
      - No cancer stage measurement record"
  })
  
  patientGenerator$save("patient_chat_ovarian_stages")
  
  expect_no_error({
    cdm <- TestGenerator::patientsCDM(
      testName = "patient_chat_ovarian_stages",
      cdmVersion = "5.4"
    )
  })
  
})

test_that("GvHD with intestinal involvement", {
  skip_if_no_openai()
  
  patientGenerator <- PatientGenerator::patientChat$new(
    model = "gpt-5.4"
  )
  
  patientGenerator$prompt(
  "Generate OMOP CDM test data for DARWIN EU P5-C3-003: acute GvHD with intestinal involvement.
    PERSON:
      - Create exactly 15 persons.
      - Persons 1–12 are eligible study patients.
      - Persons 13 and 14 are non-cases and should have no aGvHD diagnosis and no treatment records.
      - Person 15 is a prevalent case and should be excluded from the incident cohort.
      - Among persons 1–12:
        - 6 female using gender_concept_id = 8532.
        - 6 male using gender_concept_id = 8507.
      - Age distribution among persons 1–12:
        - 2 persons aged <18 years.
        - 8 persons aged 18–65 years.
        - 2 persons aged >65 years.
    OBSERVATION_PERIOD:
      - Create exactly one observation period for every person.
      - Use period_type_concept_id = 32880 for all observation periods.
      - Observation periods must include all diagnosis and treatment records.
      - Observation periods must end on or before 2025-12-31.
    CONDITION_OCCURRENCE:
      - For persons 1–12, create exactly one incident diagnosis of Graft versus host disease of intestine using condition_concept_id = 37167528.
      - Diagnosis dates must occur during the study period between 2015-01-01 and 2025-12-31.
      - Use condition_type_concept_id = 32020.
      - All condition_start_date values must occur within the person's observation period.
      - For person 15:
        - Create exactly one diagnosis of Graft versus host disease of intestine using condition_concept_id = 37167528.
        - Diagnosis date must occur before the study period (e.g., 2014-06-01) to represent a prevalent case.
      - Do not create any condition_occurrence records for persons 13 and 14.
    DRUG_EXPOSURE:
      - Create systemic corticosteroid treatment as first-line therapy using drug_concept_id = 21602722.
         - Exactly 11 patients should receive systemic corticosteroids.
      - Create ruxolitinib treatment as second-line therapy using drug_concept_id = 40244464.
          - Exactly 8 patients should receive ruxolitinib.
      - Treatment pathways for 3 patients from above with systemic corticosteroid and ruxolitinib:
        - One the above patients with exposure to ruxolitinib will not have recorded corticosteroid exposure.
        - Another patient of above with exposure to ruxolitinib with overlapping corticosteroid occurring within 30 days.
        - Another patient from above corticosteroids followed by rituximab without any prior ruxolitinib exposure.
      - Four patients will have athird-line therapy:
        - Assign one unique patient to each of the following third-line treatments:
        - Brentuximab vedotin: drug_concept_id = 40241969.
        - Etanercept: drug_concept_id = 1151789.
        - Rituximab: drug_concept_id = 1314273.
        - Vedolizumab: drug_concept_id = 45774639.
      - Each of the four third-line treatments listed above MUST be present at least once.
      - No third-line treatment may be omitted.
      - Each third-line treatment patient must be different from the others.
      - Each third-line treatment must occur after a prior ruxolitinib exposure.
      - Treatment pattern requirements among persons 1–12:
    DATE RULES:
      - Drug exposure dates must occur on or after the aGvHD diagnosis date unless intentionally testing the prevalent excluded patient.
      - All drug_exposure_start_date values must occur within observation period dates.
      - All drug_exposure_end_date values must occur within observation period dates.
    OUTPUT TABLES:
    - Populate only:
      - PERSON
      - OBSERVATION_PERIOD
      - CONDITION_OCCURRENCE
      - DRUG_EXPOSURE
    VALIDATION REQUIREMENTS:
      - Total persons = 15.
      - Eligible incident aGvHD patients = 12.
      - Non-cases = 2.
      - Prevalent excluded case = 1.
      - Female patients = 6.
      - Male patients = 6.
      - Corticosteroid-treated patients = 11.
      - Ruxolitinib-treated patients = 8.
      - Third-line patients = 4.
      - Brentuximab-treated patients = 1.
      - Etanercept-treated patients = 1.
      - Vedolizumab-treated patients = 1.
      - All diagnoses and drug exposures must occur within observation periods.")
  
  patientGenerator$save("acute_gvhd")
  
  expect_no_error({
    cdm <- TestGenerator::patientsCDM(
      testName = "acute_gvhd",
      cdmVersion = "5.4"
    )
  })
  
  cdm$person |> 
    collect() |> 
    nrow() |> 
    expect_equal(15)
  
  cdm$condition_occurrence |> 
    collect() |> 
    nrow() |> 
    expect_equal(13)
  
  cdm$drug_exposure |> 
    collect() |> 
    filter(
      drug_concept_id == 21602722 # corticosteroid
    ) |> 
    nrow() |> 
    expect_equal(11)
  
  cdm$drug_exposure |> 
    collect() |> 
    filter(
      drug_concept_id == 40244464 # ruxolitinib
    ) |> 
    nrow() |> 
    expect_equal(8)
  
  cdm$drug_exposure |> 
    collect() |> 
    filter(
      drug_concept_id == 40241969 # brentuximab vedotin
    ) |> 
    nrow() |> 
    expect_equal(1)
  
  cdm$drug_exposure |> 
    collect() |> 
    filter(
      drug_concept_id == 1151789 # etanercept: drug_concept_id = 1151789
    ) |> 
    nrow() |> 
    expect_equal(1)
  
  cdm$drug_exposure |> 
    collect() |> 
    filter(
      drug_concept_id == 1314273 # rituximab: drug_concept_id = 1314273
    ) |> 
    nrow() |> 
    expect_equal(2)
  
  cdm$drug_exposure |> 
    collect() |> 
    filter(
      drug_concept_id == 45774639 # vedolizumab: drug_concept_id = 45774639
    ) |> 
    nrow() |> 
    expect_equal(1)
  
  # Testing all drugs after conditions
  cdm$condition_occurrence |> 
    collect() |> 
    select(
      person_id,
      condition_start_date
    ) |> 
    inner_join(
      cdm$drug_exposure |> 
        collect() |> 
        select(
          person_id,
          drug_exposure_start_date
        ),
      by = join_by(person_id)
    ) |> 
    mutate(
      is_drug_after_condtion = drug_exposure_start_date >= condition_start_date
    ) |> 
    pull(is_drug_after_condtion) |> 
    expect_all_true()
  
})

