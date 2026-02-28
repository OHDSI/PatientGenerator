test_that("patientDesigner exposes configurable path argument", {
  expect_true("path" %in% names(formals(patientDesigner)))
})
