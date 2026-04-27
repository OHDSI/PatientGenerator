test_that("ellmer chat_structured output is loadable", {
  skip_if_no_openai()
  model <- pick_openai_model()

  schema <- system.file(
    "jsonSchemas",
    "cdm54schema-complete.json",
    package = "PatientGenerator"
    )
  chat <- ellmer::chat_openai(model = model)

  response <- chat$chat_structured(
    "Generate exactly one OMOP-CDM v5.4 patient with one condition occurrence.",
    type = ellmer::type_from_schema(path = schema)
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
