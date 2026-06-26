test_that("cdm table input labels remove table context repetition", {
  expect_equal(
    input_display_label("observation_period_start_date", "observation_period"),
    "Start date"
  )
  expect_equal(
    input_display_label("condition_start_date", "condition_occurrence"),
    "Start date"
  )
  expect_equal(
    input_display_label("drug_exposure_end_date", "drug_exposure"),
    "End date"
  )
  expect_equal(
    input_display_label("measurement_concept_id", "measurement"),
    "Concept id"
  )
  expect_equal(
    input_display_label("period_type_concept_id", "observation_period"),
    "Type concept id"
  )
  expect_equal(
    input_display_label("person_id", "observation_period"),
    "Person id"
  )
})

test_that("person input labels keep their full source meaning", {
  expect_equal(
    input_display_label("gender_concept_id", "person"),
    "Gender concept id"
  )
})

test_that("cdmTableServer 'add action' appends event for selected person", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1970L)
  cdm$observation_period$add(person_id = 1L)

  syncing <- shiny::reactiveVal(FALSE)

  shiny::testServer(cdm_table_server,
    args = list(
      id = "observation_period",
      cdm = cdm,
      person_id_selected = shiny::reactive("1"),
      syncing = syncing
    ), {
      session$setInputs(add = 1)
    }
  )

  expect_equal(nrow(cdm$observation_period$data()), 2L)
  expect_equal(tail(cdm$observation_period$data()$person_id, 1), 1L)
})

test_that("cdmTableServer 'add action' uses entered condition occurrence dates", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1970L)

  syncing <- shiny::reactiveVal(FALSE)

  shiny::testServer(cdm_table_server,
    args = list(
      id = "condition_occurrence",
      cdm = cdm,
      person_id_selected = shiny::reactive("1"),
      syncing = syncing,
      concept_lookup = function(concept_id) "(Condition)"
    ), {
      session$setInputs(condition_start_date = as.Date("2021-06-10"))
      session$setInputs(condition_end_date = as.Date("2021-07-11"))
      session$setInputs(add = 1)
    }
  )

  dat <- cdm$condition_occurrence$data()
  expect_equal(nrow(dat), 1L)
  expect_equal(dat$condition_start_date[[1]], as.Date("2021-06-10"))
  expect_equal(dat$condition_end_date[[1]], as.Date("2021-07-11"))
})

test_that("cdmTableServer 'add action' uses current condition occurrence concept", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1970L)
  cdm$condition_occurrence$add(
    person_id = 1L,
    condition_concept_id = 201826L
  )

  syncing <- shiny::reactiveVal(FALSE)

  shiny::testServer(cdm_table_server,
    args = list(
      id = "condition_occurrence",
      cdm = cdm,
      person_id_selected = shiny::reactive("1"),
      syncing = syncing,
      concept_lookup = function(concept_id) "(Condition)"
    ), {
      session$setInputs(condition_occurrence_id = 1L)
      session$setInputs(condition_concept_id = "201826")
      session$setInputs(add = 1)
    }
  )

  dat <- cdm$condition_occurrence$data()
  expect_equal(nrow(dat), 2L)
  expect_equal(dat$condition_concept_id[[2]], 201826L)
})

test_that("cdmTableServer 'add action' keeps default concept when current concept is empty", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1970L)

  syncing <- shiny::reactiveVal(FALSE)

  shiny::testServer(cdm_table_server,
    args = list(
      id = "condition_occurrence",
      cdm = cdm,
      person_id_selected = shiny::reactive("1"),
      syncing = syncing,
      concept_lookup = function(concept_id) "(Condition)"
    ), {
      session$setInputs(condition_concept_id = "")
      session$setInputs(add = 1)
    }
  )

  dat <- cdm$condition_occurrence$data()
  expect_equal(nrow(dat), 1L)
  expect_equal(dat$condition_concept_id[[1]], 44191562L)
})

test_that("cdmTableServer persists manually entered concept ids", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1970L)
  cdm$measurement$add(person_id = 1L)

  syncing <- shiny::reactiveVal(FALSE)

  shiny::testServer(cdm_table_server,
    args = list(
      id = "measurement",
      cdm = cdm,
      person_id_selected = shiny::reactive("1"),
      syncing = syncing,
      concept_lookup = function(concept_id) "(Fasting glucose)"
    ), {
      session$setInputs(measurement_id = 1L)
      session$setInputs(measurement_concept_id = "3018251")
    }
  )

  expect_equal(cdm$measurement$data()$measurement_concept_id[[1]], 3018251L)
})

test_that("cdmTableServer persists manually entered procedure dates", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1970L)
  cdm$procedure_occurrence$add(person_id = 1L)

  syncing <- shiny::reactiveVal(FALSE)

  shiny::testServer(cdm_table_server,
    args = list(
      id = "procedure_occurrence",
      cdm = cdm,
      person_id_selected = shiny::reactive("1"),
      syncing = syncing,
      concept_lookup = function(concept_id) "(Procedure)"
    ), {
      session$setInputs(procedure_occurrence_id = 1L)
      session$setInputs(procedure_date = as.Date("2021-04-03"))
      session$setInputs(procedure_end_date = as.Date("2021-04-05"))
    }
  )

  expect_equal(cdm$procedure_occurrence$data()$procedure_date[[1]], as.Date("2021-04-03"))
  expect_equal(cdm$procedure_occurrence$data()$procedure_end_date[[1]], as.Date("2021-04-05"))
})
