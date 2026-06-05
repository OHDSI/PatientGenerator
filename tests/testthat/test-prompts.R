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

