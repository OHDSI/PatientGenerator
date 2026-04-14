#' `patientChatNaive()` is a grapper for the ellmer package to send prompts and send the output test set to an LLM. Requires a valid API key.
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
#' @param model The model used by the LLM. Currently only OpenAI models are accepted.
#' @param jsonSchemaPath Path to a JSON schema used to structure the response.
#'
#' @returns A JSON response that includes: the natural language answer from the LLM and a JSON with test set patients in accordance to the provided schema.
#' @importFrom ellmer chat_openai
#' @importFrom ellmer type_from_schema
#' @importFrom jsonlite fromJSON
#' @export
patientChatNaive <- function(prompt = "### Give me a sample of five patients",
                             model = "gpt-5.2",
                             jsonSchemaPath = NULL) {

  # Check params ---------------------------------------------------------------
  checkmate::assertCharacter(prompt)
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
  api_models <- availableModels()
  if (!model %in% api_models) {
    stop(
      glue::glue("{model} not available.\n"),
      "\n These are some models that are available to you:\n",
      paste0("  - ", sample(api_models, 10), collapse = "\n"),
      "\n For a complete list, call availableModels()\n",
      call. = FALSE
    )
  }

  # Chat -----------------------------------------------------------------------
  chat <- ellmer::chat_openai(model = model)
  response <- chat$chat_structured(
    prompt,
    type = ellmer::type_from_schema(
      path = jsonSchemaPath
    )
  )
  return(response)
}

#' `availableModels()` If the API key is valid in the system, returns available models to the user from the LLM provider.
#'
#' @description OpenAI is the only one currently supported.
#'
#' @returns A string list with the id of a vailable models.
#' @importFrom httr2 request req_headers req_perform resp_body_json
#' @export
availableModels <- function() {
  check_api_key()
  # Retrieve and check available models
  response <- httr2::request("https://api.openai.com/v1/models") |>
    httr2::req_headers(
      Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
    ) |>
    httr2::req_perform()
  models <- resp_body_json(response)
  lapply(models$data, function(data) data$id) |>
    unlist()
}

check_api_key <- function(apiKeyName = "OPENAI_API_KEY") {
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(key)) {
    stop(
      "API key not found.\n",
      "Set it with Sys.setenv(OPENAI_API_KEY = 'your_key') ",
      "and/or add it to your ~/.Renviron.",
      call. = FALSE
    )
  }
}
