test_that("normalizeBarEndUpdate backfills missing end dates for renderable bars", {
  condition_update <- normalize_bar_end_update(list(
    type = "condition_occurrence",
    start_date = "2020-01-10",
    end_date = NULL
  ))
  expect_equal(condition_update$end_date, "2020-01-10")

  drug_update <- normalize_bar_end_update(list(
    type = "drug_exposure",
    start_date = "2020-02-10",
    end_date = character(0)
  ))
  expect_equal(drug_update$end_date, "2020-02-10")

  procedure_update <- normalize_bar_end_update(list(
    type = "procedure_occurrence",
    start_date = "2020-03-10",
    end_date = ""
  ))
  expect_equal(procedure_update$end_date, "2020-03-10")
})

test_that("normalizeBarEndUpdate leaves measurement end date untouched", {
  measurement_update <- normalize_bar_end_update(list(
    type = "measurement",
    start_date = "2020-04-10",
    end_date = NULL
  ))

  expect_null(measurement_update$end_date)
})
