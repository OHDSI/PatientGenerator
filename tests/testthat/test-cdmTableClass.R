test_that("cdmTable condition_occurrence supports add/reset/delete", {
  condition_tbl <- new_cdm_table("condition_occurrence")
  condition_tbl$add(person_id = 1L)
  condition_tbl$add(person_id = 2L)

  dat <- condition_tbl$data()
  expect_equal(dat$condition_occurrence_id, c(1L, 2L))
  expect_equal(dat$person_id, c(1L, 2L))

  condition_tbl$delete(event_id = 2L)
  dat <- condition_tbl$data()
  expect_equal(dat$condition_occurrence_id, 1L)

  condition_tbl$reset()
  expect_length(condition_tbl$data()$condition_occurrence_id, 0)
})

test_that("cdmTable add accepts provided condition occurrence dates", {
  condition_tbl <- new_cdm_table("condition_occurrence")

  condition_tbl$add(
    person_id = 1L,
    condition_start_date = as.Date("2022-03-04"),
    condition_end_date = as.Date("2022-03-18")
  )

  dat <- condition_tbl$data()
  expect_equal(dat$condition_start_date[[1]], as.Date("2022-03-04"))
  expect_equal(dat$condition_end_date[[1]], as.Date("2022-03-18"))
})
