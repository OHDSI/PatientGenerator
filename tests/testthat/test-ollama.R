test_that("multiplication works", {
  skip_if_no_ollama()
  
  patientChat <- ellmer::chat_ollama(
    model = "gemma4:26b"
  )
  
  patientChat$chat(
    "Hello"
  ) |> 
    expect_no_error()
  
  jsonSchema <- system.file(
    "jsonSchemas",
    "cdm54schema-short_deprecated.json",
    package = "PatientGenerator"
  )
  
  checkmate::checkFileExists(jsonSchema)
  
  prompt <- "5 female patients;
            condition occurrence ovarian cancer with concept id 602306,
            condition between 2015 and 2020.
            All condition occurrences must end one year after index date"
  
  response_structured <- patientChat$chat_structured(
    prompt,
    type = ellmer::type_from_schema(
      path = jsonSchema
    )
  )
  
  # Instantiate patientGenerator
  expect_no_error({
    patientGenerator <- patientChat$new(
      provider = "ollama",
      model = "gemma4:26b"
      )
  })
  
  patientGenerator$save(
    name = "ollama-gemma426b-test-set",
  )
  
  cdm <- TestGenerator::patientsCDM(
    testName = "ollama-gemma426b-test-set"
  )
  
  cdm$person
  
  patientGenerator$prompt(
    "Population (PERSON table):
      - 35 persons of various ages born between 1960 and 2000
      - 18 female, use gender_concept_id = 8532
      - 17 male, use gender_concept_id = 8507

    OBSERVATION_PERIOD:
      - Start date between date of birth each person and end of observation 2025-12-31
      - All persons have a period_type_concept_id with id: 32828

    CONDITION_OCCURRENCE:
      - The patients have occurrences of 7 different types of cancer:
        - 5 patients from the PERSON table have bladder cancer with condition_concept_id: 196360
        - 5 patients from the PERSON table have breast cancer with condition_concept_id: 36556994
        - 5 patients from the PERSON table have colorectal cancer with condition_concept_id: 40481902
        - 5 patients from the PERSON table have esophageal cancer with condition_concept_id: 4181343
        - 5 patients from the PERSON table have lung cancer with condition_concept_id: 36535703
        - 5 patients from the PERSON table have prostate cancer with condition_concept_id: 4163261
        - 5 patients from the PERSON table have skin melanoma with condition_concept_id: 141232
      - Everyone has condition_type_concept_id 32817
      - For each group of 5 patients sharing the same condition_concept_id:
        - 3/5 patients have an occurrence after 2010-01-01 and they were >=18 years
          old at condition start date
          - If the condition is breast cancer, make all 3 patients Females
          - If the condition is prostate cancer, make all 3 patients Males
        - 1/5 patients has an occurrence after 2010-01-01 but they were not >=18
          years old at condition start date
        - 1/5 patients has an occurrence before 2010-01-01

    DEATH:
      - For every group of patients with same condition_concept_id:
        - Among the 3 patients that are >=18 years old and with condition occurrence after 2010-01-01:
          - 1/3 patient has a death date previous to the start date of his/her condition occurrence
          - 1/3 patient has a death date coinciding with the start date of his/her condition occurrence

    Output Requirements:
    - All records in CONDITION_OCCURRENCE, DEATH
    - Fill only specified tables in this prompt
    - All patients in PERSON have an observation period
    - Make sure there's
    - Fill out the condition end date 2024-12-31 for everyone
    - All condition occurrence records must be inside observation_period dates."
  )
  
  patientGenerator$save(
    name = "testCancerCohortsLLM"
  )
  
  cdm <- TestGenerator::patientsCDM(
    testName = "testCancerCohortsLLM",
    cdmVersion = "5.4"
  )
  
  # call createCancerCohorts to generate codelists and create cohorts
  cdm <- createCancerCohorts(
    cdm = cdm,
    concept_sets_folder = "cancer_cohorts",
    name = "cancer_cohorts"
  )
  
  # test number of patients in cdm instance
  cdm$person |>
    dplyr::collect() |>
    nrow() |>
    expect_equal(35)
  
  # test total attrition
  cdm$cancer_cohorts |>
    CohortConstructor::attrition() |>
    dplyr::select(excluded_records) |>
    sum() |>
    expect_equal(28)
  
  # test attrition after imposing age ≥18
  cdm$cancer_cohorts |>
    CohortConstructor::attrition() |>
    dplyr::filter(stringr::str_detect(reason, "Age requirement")) |>
    dplyr::pull(excluded_records) |>
    sum() |>
    expect_equal(7)
  
  # test attrition after imposing start date 2010-01-01
  cdm$cancer_cohorts |>
    CohortConstructor::attrition() |>
    dplyr::filter(stringr::str_detect(reason, "2010-01-01")) |>
    dplyr::pull(excluded_records) |>
    sum() |>
    expect_equal(7)
  
  # test attrition after excluding people with death date before index date
  cdm$cancer_cohorts |>
    CohortConstructor::attrition() |>
    dplyr::filter(stringr::str_detect(reason, "Not in table death between -Inf & -1 days")) |>
    dplyr::pull(excluded_records) |>
    sum() |>
    expect_equal(7)
  
  # test attrition after excluding people with death date on index date
  cdm$cancer_cohorts |>
    CohortConstructor::attrition() |>
    dplyr::filter(stringr::str_detect(reason, "Not in table death between 0 & 0 days")) |>
    dplyr::pull(excluded_records) |>
    sum() |>
    expect_equal(7)
  
  # test number of final patients in the cohort
  cdm$cancer_cohorts |>
    dplyr::collect() |>
    nrow() |>
    expect_equal(7)
  
  # test valid sex variable
  cdm$cancer_cohorts |>
    PatientProfiles::addSex() |>
    dplyr::pull(sex) |>
    unique() |>
    expect_in(c(
      "Male",
      "Female")
    )
  
})