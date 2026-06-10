test_that("hecateConceptLabel returns concept names for valid concept ids", {
  testthat::local_mocked_bindings(
    hecateSearch = function(query, limit = 10) {
      data.frame(
        conceptId = c(3018251L, 201826L),
        conceptName = c("Hemoglobin A1c measurement", "Type 2 diabetes mellitus"),
        invalidReason = c(NA_character_, NA_character_)
      )
    },
    .package = "PatientGenerator"
  )

  expect_equal(
    hecate_concept_label("3018251"),
    "Hemoglobin A1c measurement"
  )
})

test_that("hecateConceptLabel returns invalid concept id for invalid or missing concepts", {
  testthat::local_mocked_bindings(
    hecateSearch = function(query, limit = 10) {
      data.frame(
        conceptId = 3018251L,
        conceptName = "Hemoglobin A1c measurement",
        invalidReason = NA_character_
      )
    },
    .package = "PatientGenerator"
  )

  expect_equal(hecate_concept_label("abc"), "(invalid concept id)")
  expect_equal(hecate_concept_label("9999999"), "(not found)")
})

test_that("hecateConceptLabel returns invalid concept id for invalid vocabulary concepts", {
  testthat::local_mocked_bindings(
    hecateSearch = function(query, limit = 10) {
      data.frame(
        conceptId = 123L,
        conceptName = "Old concept",
        invalidReason = "D"
      )
    },
    .package = "PatientGenerator"
  )

  expect_equal(hecate_concept_label("123"), "(invalid concept id)")
})
