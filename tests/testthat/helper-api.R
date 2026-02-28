skip_if_no_openai <- function() {
  testthat::skip_on_cran()
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(key)) {
    testthat::skip("OPENAI_API_KEY not set")
  }
}

pick_openai_model <- function() {
  models <- tryCatch(
    patientGenerator::availableModels(),
    error = function(e) testthat::skip(paste("Cannot list models:", conditionMessage(e)))
  )
  preferred <- c("gpt-5.2", "gpt-5", "gpt-4.1", "gpt-4o")
  model <- preferred[preferred %in% models][1]
  if (is.na(model) || is.null(model)) {
    model <- models[1]
  }
  if (is.na(model) || !nzchar(model)) {
    testthat::skip("No available OpenAI model for this key")
  }
  model
}
