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
  codelist_path <- system.file("concept_sets", "ovarian_cancer_codelist.rds", package = "patientGenerator")
  codelist_data <- readRDS(codelist_path)

  skip_if_no_openai()
  model <- pick_openai_model()

  generator <- patientChat$new(model = model, codelist_data = codelist_data, echo = "none")
  result <- generator$retrieveCodelist(concept_label = "ovarian", domain = "Condition")
  parsed <- jsonlite::fromJSON(result)

  expect_s3_class(parsed, "data.frame")
  expect_gt(nrow(parsed), 0)
})
