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
