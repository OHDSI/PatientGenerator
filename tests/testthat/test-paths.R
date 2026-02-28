test_that("getTestSets supports explicit path", {
  tmp <- tempfile("pg_testsets_")
  dir.create(tmp, recursive = TRUE)
  write('{"person":[]}', file.path(tmp, "alpha.json"))
  write('{"person":[]}', file.path(tmp, "beta.json"))

  sets <- sort(getTestSets(path = tmp))
  expect_equal(sets, c("alpha", "beta"))
})
