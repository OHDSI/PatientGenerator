test_that("patientChatNaive rejects non-JSON schema files", {
  bad_schema <- tempfile(fileext = ".txt")
  writeLines("not-json", bad_schema)

  expect_error(
    patientChatNaive(prompt = "Generate 1 patient", jsonSchemaPath = bad_schema),
    "jsonSchemaPath"
  )
})

test_that("patientChatNaive API output can be loaded by cdmConstructor", {
  skip_if_no_openai()
  model <- pick_openai_model()

  schema <- system.file("jsonSchemas", "cdm54schema-complete.json", package = "PatientGenerator")
  response <- patientChatNaive(
    prompt = "Generate exactly 1 patient in OMOP-CDM v5.4.",
    model = model,
    jsonSchemaPath = schema
  )

  out_json <- jsonlite::toJSON(
    response,
    dataframe = "rows",
    pretty = TRUE,
    null = "null",
    na = "null",
    auto_unbox = TRUE
  )

  out_file <- tempfile(fileext = ".json")
  write(out_json, out_file)

  cdm <- new_cdm()
  expect_no_error(cdm$loadJsonTestSet(out_file))
  expect_gte(nrow(cdm$person$data()), 1)
})
