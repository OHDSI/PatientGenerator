#' Useful to list available datasets in the testCases folder
#'
#' @param path Optional directory containing JSON test sets.
#' If NULL, the package resolves a default path with testthat integration.
#' @returns A list()
#' @export
#'
#' @importFrom stringr str_remove
getTestSets <- function(path = NULL) {

  testCasesDir <- testSetDir(path = path, create = TRUE)

  list.files(testCasesDir) |>
    stringr::str_remove(".json")

}
