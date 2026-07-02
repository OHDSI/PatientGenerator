test_that("cdmConstructor loads supported tables from xlsx test data", {
  skip_if_not_installed("readxl")

  xlsx_path <- testthat::test_path("../../extras/testPatients.xlsx")
  skip_if_not(file.exists(xlsx_path))

  cdm <- new_cdm()
  result <- cdm$loadXlsxTestSet(xlsx_path)

  expect_in("person", result$loaded)
  expect_in("observation_period", result$loaded)
  expect_in("condition_occurrence", result$loaded)
  expect_in("death", result$loaded)
  expect_in("visit_occurrence", result$ignored)
  expect_gt(nrow(cdm$person$data()), 0)
  expect_true("person_id" %in% names(cdm$person$data()))
  expect_false("death_occurrence_id" %in% names(cdm$death$data()))
  expect_true(inherits(cdm$observation_period$data()$observation_period_start_date, "Date"))
})

test_that("cdmConstructor exports xlsx test data that can be uploaded again", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("readxl")

  cdm <- new_cdm()
  cdm$person$add(
    gender_concept_id = 8532L,
    year_of_birth = 1967L,
    month_of_birth = 4L,
    day_of_birth = 12L
    )
  cdm$observation_period$add(person_id = 1L)
  cdm$condition_occurrence$add(person_id = 1L)

  xlsx_path <- tempfile(fileext = ".xlsx")
  expect_no_error(cdm$writeCdmDataXlsx(xlsx_path))

  sheets <- readxl::excel_sheets(xlsx_path)
  expect_setequal(
    sheets,
    c("person", cdm$tables)
    )

  uploaded <- new_cdm()
  expect_no_error(uploaded$loadXlsxTestSet(xlsx_path))
  expect_equal(nrow(uploaded$person$data()), nrow(cdm$person$data()))
  expect_equal(
    nrow(uploaded$condition_occurrence$data()),
    nrow(cdm$condition_occurrence$data())
    )
  expect_true(
    inherits(
      uploaded$observation_period$data()$observation_period_start_date,
      "Date"
      )
    )
})
