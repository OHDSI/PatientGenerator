test_that("patientDesigner exposes configurable path argument", {
  expect_true("path" %in% names(formals(patientDesigner)))
})

test_that("patientDesigner can prepare a publishable app directory", {
  prepare_publishable <- getFromNamespace(
    "preparePublishablePatientDesigner",
    "PatientGenerator"
  )

  test_set_dir <- tempfile("pg_cases_")
  publish_dir <- tempfile("pg_publish_")
  dir.create(test_set_dir, recursive = TRUE)
  writeLines("{}", file.path(test_set_dir, "example.json"))

  result <- prepare_publishable(
    path = test_set_dir,
    publishDir = publish_dir,
    overwritePublishDir = FALSE
  )

  expect_equal(result, normalizePath(publish_dir, mustWork = FALSE))
  expect_true(file.exists(file.path(publish_dir, "app.R")))
  expect_true(file.exists(file.path(
    publish_dir,
    basename(test_set_dir),
    "example.json"
  )))

  app_code <- readLines(file.path(publish_dir, "app.R"), warn = FALSE)
  expect_true(any(grepl("PatientGenerator::patientDesigner", app_code)))
  expect_true(any(grepl(basename(test_set_dir), app_code, fixed = TRUE)))
})
