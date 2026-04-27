test_that("Chaining LLM thought for synthetic patient generation", {
  skip_if_no_openai()
  
  # Instantiate patientGenerator
  expect_error({
    patientGenerator <- patientChat$new(model = "gpt")
  })
  expect_no_error({
    patientGenerator <- patientChat$new(model = "gpt-5.4-pro")
    patientGenerator <- patientChat$new(model = "gpt-5-nano")
    patientGenerator <- patientChat$new(model = "gpt-5-mini")
  })
  
  patientGenerator <- patientChat$new(model = "gpt-5.4")
  # patientGenerator$availableModels()
  
  # 1
  patientGenerator$prompt("5 female patients;
                          condition occurrence ovarian cancer with concept id 602306,
                          condition between 2015 and 2020.
                          All condition occurrences must end one year after index date")
  patientGenerator$save()
  
  expect_no_error({
    cdm <- TestGenerator::patientsCDM(testName = "patient-chat-test")
  })
  
  cdm$person |>
    pull(person_id) |>
    length() |>
    expect_equal(5)
  
  cdm$condition_occurrence |>
    pull(person_id) |>
    length() |>
    expect_equal(5)
  
  cdm$condition_occurrence |>
    pull(condition_concept_id) |>
    unique() |>
    expect_equal(602306)
  
  
  # 2
  patientGenerator$prompt("Eliminate 3 patients")
  patientGenerator$save()
  
  expect_no_error({
    cdm <- TestGenerator::patientsCDM(testName = "patient-chat-test")
  })
  
  cdm$person |>
    pull(person_id) |>
    length() |>
    expect_equal(2)
  
  cdm$condition_occurrence |>
    pull(person_id) |>
    length() |>
    expect_equal(2)
  
  cdm$condition_occurrence |>
    pull(condition_concept_id) |>
    unique() |>
    expect_equal(602306)
  
  
  # 3
  patientGenerator$prompt("Now fill out 10 more patients with ovarian cancer;
                          with 3 exposures of altretamine with concept_id code 1368823
                          to all patients in dataset;
                          Every exposure has an era of 30 days with a 6 day gap")
  patientGenerator$save()
  
  expect_no_error({
    cdm <- TestGenerator::patientsCDM(testName = "patient-chat-test")
  })
  
  cdm$person |>
    pull(person_id) |>
    length() |>
    expect_equal(12)
  
  cdm$condition_occurrence |>
    pull(person_id) |>
    length() |>
    expect_equal(12)
  
  cdm$drug_exposure |>
    pull(person_id) |>
    length() |>
    expect_equal(36)
  
  #4
  patientGenerator$prompt("Now, add 5 more patients to the previous dataset with ovarian cancer.
                          All patients must have also a measurement of stage 1 cancer with concept id code 1633306")
  patientGenerator$save()
  
  expect_no_error({
    cdm <- TestGenerator::patientsCDM(testName = "patient-chat-test")
  })
  
  cdm$person |>
    pull(person_id) |>
    length() |>
    expect_equal(17)
  
  cdm$condition_occurrence |>
    pull(condition_occurrence_id) |>
    length() |>
    expect_equal(17)
  
  cdm$measurement |>
    pull(person_id) |>
    length() |>
    expect_equal(17)
  
})


test_that("patientChat API flow creates loadable test set", {
  skip_if_no_openai()
  model <- pick_openai_model()

  expect_error(patientChat$new(model = "gpt"))

  generator <- patientChat$new(model = model, echo = "none")
  generator$prompt("Generate exactly 1 synthetic patient in OMOP-CDM v5.4 with one observation period.")

  tmp <- tempfile("pg_chat_")
  dir.create(tmp, recursive = TRUE)
  generator$save(name = "patient-chat-test", path = tmp)

  out_file <- file.path(tmp, "patient-chat-test.json")
  expect_true(file.exists(out_file))

  cdm <- new_cdm()
  expect_no_error(cdm$loadJsonTestSet(out_file))
  expect_gte(nrow(cdm$person$data()), 1)
})

test_that("patientChat with local codelist returns retrievable concepts", {
  codelist_path <- system.file("concept_sets", "ovarian_cancer_codelist.rds", package = "PatientGenerator")
  codelist_data <- readRDS(codelist_path)

  skip_if_no_openai()
  model <- pick_openai_model()

  generator <- patientChat$new(model = model, codelist_data = codelist_data, echo = "none")
  result <- generator$retrieveCodelist(concept_label = "ovarian", domain = "Condition")
  parsed <- jsonlite::fromJSON(result)

  expect_s3_class(parsed, "data.frame")
  expect_gt(nrow(parsed), 0)
})
