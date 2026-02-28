test_that("retrieveCodelist helper and tool object are available", {
  retrieve_fn <- getFromNamespace("retrieveCodelist", "patientGenerator")
  tool_obj <- getFromNamespace("retrieveCodelistTool", "patientGenerator")

  result <- retrieve_fn(concept_label = "ovarian", domain = "Condition")
  parsed <- jsonlite::fromJSON(result)

  expect_s3_class(parsed, "data.frame")
  expect_gt(nrow(parsed), 0)
  expect_false(is.null(tool_obj))
})
