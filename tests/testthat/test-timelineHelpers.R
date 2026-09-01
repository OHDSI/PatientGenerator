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

test_that("formatDateColumns displays timeline dates as ISO dates", {
  timeline <- data.table::data.table(
    type = c("condition_occurrence", "drug_exposure", "measurement"),
    start_date = c(
      as.Date("2020-01-10"),
      as.Date("2020-02-10"),
      as.Date(NA)
    ),
    end_date = c(
      as.Date("2020-01-12"),
      as.Date("2020-02-12"),
      as.Date(NA)
    )
  )

  formatted <- format_date_columns(timeline)

  expect_equal(formatted$start_date, c("2020-01-10", "2020-02-10", NA_character_))
  expect_equal(formatted$end_date, c("2020-01-12", "2020-02-12", NA_character_))
  expect_s3_class(timeline$start_date, "Date")
})

test_that("formatDateColumns handles numeric and POSIX timeline dates", {
  timeline <- data.frame(
    start_date = as.numeric(as.Date("2020-03-10")),
    end_date = as.POSIXct("2020-03-12 00:00:00", tz = "UTC")
  )

  formatted <- format_date_columns(timeline)

  expect_equal(formatted$start_date, "2020-03-10")
  expect_equal(formatted$end_date, "2020-03-12")
})

test_that("formatDateColumns displays native CDM date columns as ISO dates", {
  drug_exposure <- data.frame(
    drug_exposure_id = 1L,
    drug_exposure_start_date = as.Date("2020-04-10"),
    drug_exposure_end_date = as.Date("2020-04-20"),
    verbatim_end_date = as.Date("2020-04-20")
  )

  formatted <- format_date_columns(drug_exposure)

  expect_equal(formatted$drug_exposure_start_date, "2020-04-10")
  expect_equal(formatted$drug_exposure_end_date, "2020-04-20")
  expect_equal(formatted$verbatim_end_date, "2020-04-20")
})
