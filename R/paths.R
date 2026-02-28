#' Internal helper to locate where patient test sets are stored
#' @noRd
testSetDir <- function(path = NULL, create = TRUE) {
  if (is.null(path)) {
    path <- tryCatch(
      testthat::test_path("testCases"),
      error = function(e) NULL
    )
  }

  if (is.null(path)) {
    local_testthat_dir <- file.path(getwd(), "tests", "testthat", "testCases")
    if (dir.exists(dirname(local_testthat_dir))) {
      path <- local_testthat_dir
    }
  }

  if (is.null(path)) {
    path <- getOption("patientGenerator.testSetDir", NULL)
  }

  if (is.null(path)) {
    path <- file.path(tools::R_user_dir("patientGenerator", which = "data"), "testCases")
  }

  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  path
}
