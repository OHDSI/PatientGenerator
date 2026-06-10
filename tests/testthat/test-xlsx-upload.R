test_that("cdmConstructor loads supported tables from xlsx test data", {
  skip_if_not_installed("readxl")

  xlsx_path <- testthat::test_path("../../extras/testPatients.xlsx")
  skip_if_not(file.exists(xlsx_path))

  cdm <- new_cdm()
  result <- cdm$loadXlsxTestSet(xlsx_path)

  expect_in("person", result$loaded)
  expect_in("observation_period", result$loaded)
  expect_in("condition_occurrence", result$loaded)
  expect_in("visit_occurrence", result$ignored)
  expect_gt(nrow(cdm$person$data()), 0)
  expect_true("person_id" %in% names(cdm$person$data()))
  expect_true(inherits(cdm$observation_period$data()$observation_period_start_date, "Date"))
})
