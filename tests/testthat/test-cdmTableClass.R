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
