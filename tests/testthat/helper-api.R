skip_if_no_openai <- function() {
  testthat::skip_on_cran()
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(key)) {
    testthat::skip("OPENAI_API_KEY not set")
  }
}

skip_if_no_ollama <- function() {
  testthat::skip_on_cran()
  if (!ellmer:::has_ollama()) {
    testthat::skip("Ollama not running")
  }
}

skip_if_no_anthropic <- function() {
  testthat::skip_on_cran()
  key <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  if (!nzchar(key)) {
    testthat::skip("ANTHROPIC_API_KEY not set")
  }
}

pick_openai_model <- function() {
  models <- tryCatch(
    PatientGenerator::availableModels("openai"),
    error = function(e) testthat::skip(paste("Cannot list models:", conditionMessage(e)))
  )
  preferred <- c(
    "gpt-5.6-luna",
    "gpt-5.4",
    "gpt-5.2",
    "gpt-5",
    "gpt-4.1",
    "gpt-4o"
    )
  model <- preferred[preferred %in% models][1]
  if (is.na(model) || is.null(model)) {
    model <- models[1]
  }
  if (is.na(model) || !nzchar(model)) {
    testthat::skip("No available OpenAI model for this key")
  }
  model
}

pick_anthropic_model <- function() {
  models <- tryCatch(
    PatientGenerator::availableModels("anthropic"),
    error = function(e) testthat::skip(paste("Cannot list models:", conditionMessage(e)))
  )
  preferred <- c(
    "claude-opus-5",
    "claude-opus-4-8",
    "claude-sonnet-5",
    "claude-opus-4-7",
    "claude-opus-4-6",
    "claude-sonnet-4-6"
    )
  model <- preferred[preferred %in% models][1]
  if (is.na(model) || is.null(model)) {
    model <- models[1]
  }
  if (is.na(model) || !nzchar(model)) {
    testthat::skip("No available Anthropic model for this key")
  }
  model
}