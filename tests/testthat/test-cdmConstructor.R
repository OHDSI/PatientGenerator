test_that("cdmConstructor reset empties core tables", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1967L)
  cdm$observation_period$add(person_id = 1L)
  cdm$drug_exposure$add(person_id = 1L)

  cdm$reset()
  empty_cdm <- jsonlite::fromJSON(cdm$getCdmData())

  expect_length(empty_cdm$person, 0)
  expect_length(empty_cdm$observation_period, 0)
  expect_length(empty_cdm$drug_exposure, 0)
})

test_that("person table add/update/delete behaves as expected", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1967L)
  cdm$person$add(gender_concept_id = 8507L, year_of_birth = 1988L)

  cdm$person$update(person_id = 1L, gender_concept_id = 8507L, year_of_birth = 1977L)
  person_table <- cdm$person$data()

  expect_equal(person_table$person_id, c(1L, 2L))
  expect_equal(person_table$gender_concept_id, c(8507L, 8507L))
  expect_equal(person_table$year_of_birth, c(1977L, 1988L))

  cdm$person$delete(person_id = 2L)
  person_table <- cdm$person$data()
  expect_equal(person_table$person_id, 1L)
})

test_that("observation/condition/drug dates can be updated", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1967L)
  cdm$observation_period$add(person_id = 1L)
  cdm$condition_occurrence$add(person_id = 1L)
  cdm$drug_exposure$add(person_id = 1L)

  cdm$observation_period$updateDates(
    person_id = 1L, event_id = 1L,
    start_date = as.Date("1997-10-29"), end_date = as.Date("1999-10-29")
  )
  cdm$condition_occurrence$updateDates(
    person_id = 1L, event_id = 1L,
    start_date = as.Date("1998-01-01"), end_date = as.Date("1998-12-31")
  )
  cdm$drug_exposure$updateDates(
    person_id = 1L, event_id = 1L,
    start_date = as.Date("2001-02-03"), end_date = as.Date("2001-03-04")
  )

  expect_equal(cdm$observation_period$data()$observation_period_start_date[[1]], as.Date("1997-10-29"))
  expect_equal(cdm$condition_occurrence$data()$condition_start_date[[1]], as.Date("1998-01-01"))
  expect_equal(cdm$drug_exposure$data()$drug_exposure_start_date[[1]], as.Date("2001-02-03"))
})

test_that("getCdmData and getCdmDataTimeline return valid structures", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1967L)
  cdm$observation_period$add(person_id = 1L)
  cdm$drug_exposure$add(person_id = 1L)

  cdm_json <- cdm$getCdmData()
  expect_true(jsonlite::validate(cdm_json))

  timeline <- cdm$getCdmDataTimeline()
  expect_s3_class(timeline, "data.table")
  expect_true(all(c("event_id", "concept_id", "person_id", "start_date", "end_date", "type", "categories") %in% names(timeline)))
})

test_that("loadJsonTestSet loads source test fixtures", {
  cdm <- new_cdm()
  path <- testthat::test_path("testCases", "objective_1_patients.json")
  expect_no_error(cdm$loadJsonTestSet(path))
  expect_gt(nrow(cdm$person$data()), 0)
})
