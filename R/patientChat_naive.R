#' `patientChatNaive()` is a grapper for the ellmer package to send prompts and send the output test set to an LLM.
#' Requires an OPENAI_API_KEY or ANTHROPIC_API_KEY in ~/.Renviron when using OpenAI or
#' Anthropic models, respectively.
#'
#' @description
#'    Priorities:
#'      - Accepts a prompt as an input.
#'      - Produces a test set in accordance to the provided JSON schema.
#'      - Utilizes tools such as CodelistGenerator or Hecate to look up for functions.
#'      - Accepts a subsequent prompt with a test set that the LLM has to use as a context
#'
#'    One function for this tasks allow us to:
#'      - Test the test sets created by the LLM.
#'      - Test prompt engineering.
#'      - Test integration of tools functionality.
#'      - Allow us to create fast a small set of patients to test analytical packages.
#'
#' @param prompt A prompt to the LLM, in character or JSON response.
#' @param provider The LLM provider; one of "openai" (default), "anthropic" or "ollama"
#' @param model Such as "gpt-5.3". For a complete list, call patientChat$availableModels(provider)
#' @param jsonSchemaPath Path to a JSON schema used to structure the response.
#'
#' @returns A JSON response that includes: the natural language answer from the LLM and a JSON with test set patients in accordance to the provided schema.
#' @importFrom ellmer chat_openai
#' @importFrom ellmer chat_anthropic
#' @importFrom ellmer chat_ollama
#' @importFrom ellmer type_from_schema
#' @importFrom jsonlite fromJSON
#' @export
patientChatNaive <- function(prompt = "### Give me a sample of five patients",
                             provider = "openai",
                             model = "gpt-5.2",
                             jsonSchemaPath = NULL) {

  # Check params ---------------------------------------------------------------
  checkmate::assertCharacter(prompt)
  checkmate::assertChoice(provider, c("openai", "anthropic", "ollama"))
  checkmate::assertCharacter(model)
  if (!is.null(jsonSchemaPath)) {
    # browser()
    checkmate::assertCharacter(jsonSchemaPath)
    tryCatch({
      jsonlite::fromJSON(jsonSchemaPath)
    }, error = function(e) {
      stop("jsonSchemaPath doesn't lead to a JSON file")
    })
  } else {
    jsonSchemaPath <- system.file("jsonSchemas",
                                  "cdm54schema-complete.json",
                                  package = "PatientGenerator")
    checkmate::assertFileExists(jsonSchemaPath)
  }

  # Check API and available models ---------------------------------------------
  api_models <- availableModels(provider)
  if (!model %in% api_models) {
    stop(
      glue::glue("{model} not available.\n"),
      "\n These are some models that are available to you:\n",
      paste0("  - ", sample(api_models, 10), collapse = "\n"),
      "\n For a complete list, call availableModels(provider)\n",
      call. = FALSE
    )
  }

  # Chat -----------------------------------------------------------------------
  chat_fns <- list(
    openai = ellmer::chat_openai,
    ollama = ellmer::chat_ollama,
    anthropic = ellmer::chat_anthropic
  )

  if (!provider %in% names(chat_fns)) {
    stop("Provider not supported. Use one of: 'openai', 'ollama', 'anthropic'")
  }

  chat <- chat_fns[[provider]](model = model)
  response <- chat$chat_structured(
    prompt,
    type = ellmer::type_from_schema(
      path = jsonSchemaPath
    )
  )
  return(response)
}

#' `availableModels(provider_name)` If the API key is valid in the system, returns available models to the user from the LLM provider.
#'
#' @description "openai", "anthropic", and "ollama" are supported providers.
#'
#' @returns A string list with the id of a vailable models.
#' @importFrom httr2 request req_headers req_perform resp_body_json ellmer
#' @export
availableModels <- function(provider_name) {
  if (provider_name == "ollama") {
    return(ellmer::models_ollama()$id)
  }

  endpoint <- switch(
    provider_name,
    anthropic = "https://api.anthropic.com/v1/models",
    openai = "https://api.openai.com/v1/models",
    stop("Provider not supported. Use one of: 'openai', 'ollama', 'anthropic'")
  )
  api_key_name <- switch(
    provider_name,
    anthropic = "ANTHROPIC_API_KEY",
    openai = "OPENAI_API_KEY",
    stop("Provider not supported. Use one of: 'openai', 'ollama', 'anthropic'")
  )

  check_api_key(api_key_name)

  response <- httr2::request(endpoint) |>
    httr2::req_headers(
      Authorization = paste("Bearer", Sys.getenv(api_key_name))
    ) |>
    httr2::req_perform()
  models <- httr2::resp_body_json(response)
  lapply(models$data, function(data) data$id) |>
    unlist()
}

check_api_key <- function(apiKeyName) {
  key <- Sys.getenv(apiKeyName, unset = "")
  if (!nzchar(key)) {
    stop(
      "API key not found.\n",
      glue::glue("Set it with Sys.setenv({apiKeyName} = 'your_key') "),
      "and/or add it to your ~/.Renviron.",
      call. = FALSE
    )
  }
}
