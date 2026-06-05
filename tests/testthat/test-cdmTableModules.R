test_that("cdmTableServer add action appends event for selected person", {
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
