#' `patientChat()` generates synthetic patients in the OMOP-CDM using an LLM API.
#'
#' @description
#' Requires an OPEN_AI_KEY in ~/.Renviron.
#' After that just sent a prompt and save() the results.
#' The JSON file can be used as an OMOP-CDM patient test set.
#' @details
#' Accepts a prompt as input. Produces a test set using a
#' structured JSON schema. Utilizes tools such as CodelistGenerator or Hecate
#' to look up concept IDs. Accepts subsequent prompts to modify existing test
#' sets that the LLM uses as context.
#'
#' This class allows testing patient sets created by the LLM, prompt engineering,
#' integration of search tools and functionality, and creating a set of patients
#' to test analytical packages.
#
#' @returns A JSON response that includes: the natural language answer from the LLM and a JSON with test set patients in accordance to the provided schema.
#' @importFrom ellmer chat_openai
#' @importFrom ellmer type_from_schema
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_headers req_perform resp_body_json
#' @importFrom checkmate assertCharacter assertFileExists assertDirectoryExists
#' @importFrom tools file_path_sans_ext
#' @importFrom R6 R6Class
#' @export
#' @examples
#' \dontrun{
#' generator <- patientChat$new()
#' generator$prompt("Give me 5 patients")
#' generator$save("my_test")
#' }
patientChat <- R6::R6Class(
  classname = "patientChat",
  public = list(

    #' @field chat An ellmer chat instance
    chat = NULL,

    #' @field json_schema_path JSON schema to output structured results
    json_schema_path = NULL,

    #' @field response Ouput from the LLM
    response = NULL,

    #' @field codelist A codelist with details to search for concepts ids
    codelist = NULL,

    #' @description
    #' Create a new chat to create JSON test sets for OMOP-CDM.
    #' @param system_prompt Initial system prompt to impose behaviour to the LLM
    #' @param model Such as "gpt-5.2". For a complete list, call patientChat$availableModels()
    #' @param jsonSchemaPath The JSON schema to structure output from LLM
    #' @param echo How the output will be displayed in the console
    #' @param codelist_data A codelist with details to search for concepts ids
    #'
    #' @return A new `Person` object.
    initialize = function(system_prompt = NULL,
                          model = "gpt-5.2",
                          jsonSchemaPath = NULL,
                          echo = c("none", "output", "all"),
                          codelist_data = NULL) {

      # Check API and available models -----------------------
      private$.api_check(model)

      # check JSON schema file -------------------------------
      private$.json_schema_check(jsonSchemaPath)

      # System propmpt ---------------------------------------
      if (is.null(system_prompt)) {
        system_prompt <- "You are an automatic generator of synthetic patients data using exclysively the OMOP-CDM v5.4 format"
      }

      # Create chat ------------------------------------------
      self$chat <- ellmer::chat_openai(system_prompt = system_prompt,
                                       model = model,
                                       echo = echo)
      message("Chat created")

      # Codelist ---------------------------------------------
      if (!is.null(codelist_data)) {
        checkmate::assertDataFrame(codelist_data)
        if (all(c("concept_id", "concept_name", "domain_id", "vocabulary_id", "standard_concept") %in% codelist_data |> names())) {
          self$codelist <- codelist_data
          self$chat$register_tool(private$.register_codelist_tool())
          message("Codelist tool added")
        } else {
          stop("Dataframe has missing columns: 'concept_id', 'concept_name', 'domain_id', 'vocabulary_id', 'standard_concept'")
        }
      }

    },

    #' @description
    #' Prompt to request data from LLM API
    #' @param prompt A query in character.
    prompt = function(prompt) {
      checkmate::assertCharacter(prompt)
      api_respose <- self$chat$chat_structured(
        prompt,
        type = ellmer::type_from_schema(
          path = self$json_schema_path
        )
      )
      self$response <- api_respose
    },

    #' @description
    #' Output in JSON format
    json_response = function() {
      jsonlite::toJSON(self$response,
                       dataframe = "rows",
                       pretty = TRUE,
                       null = "null",
                       na = "null",
                       auto_unbox = TRUE)
    },

    #' @description
    #' Returns the chat object
    output = function() {
      return(self$chat)
    },

    #' @description
    #' Retrieves and filters data from codelist_data
    #' @param concept_label Filters the concept_name in the codelist with details
    #' @param domain Filters the domain in the codelist with details.
    retrieveCodelist = function(concept_label = "Stage 1", domain = "Measurement") {
      checkmate::assertCharacter(concept_label)
      checkmate::assertCharacter(domain)
      domain_names <- self$codelist |>
        dplyr::pull(domain_id) |>
        unique()
      if (!domain %in% domain_names) {
        warning("domain not found in codelist")
      }
      result <- self$codelist %>%
        filter(stringr::str_detect(concept_name, stringr::regex(concept_label, ignore_case = TRUE))) |>
        dplyr::filter(domain_id == domain) |>
        jsonlite::toJSON(dataframe = "rows",
                         auto_unbox = TRUE)
      return(result)
    },

    #' @description
    #' Saves the JSON test set to disk.
    #' @param name Name of the file
    #' @param path To save the file.
    #' If NULL, the package first tries `testthat::test_path("testCases")`,
    #' then checks `options(PatientGenerator.testSetDir = "...")`, and finally
    #' falls back to the package user data directory.
    save = function(name = "patient-chat-test",
                    path = NULL) {
      if (is.null(path)) {
        path <- testSetDir(create = TRUE)
      } else {
        checkmate::assertCharacter(path)
        checkmate::assertDirectoryExists(path)
      }
      name <- tools::file_path_sans_ext(name)
      test_file_path <- file.path(path, paste0(name, ".json"))
      write(self$json_response(), file = test_file_path)
    },

    #' @description
    #' Retrieves available models from the LLM API.
    availableModels = function() {
      private$.check_api_key()
      # Retrieve and check available models
      response <- httr2::request("https://api.openai.com/v1/models") |>
        httr2::req_headers(
          Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
        ) |>
        httr2::req_perform()
      models <- httr2::resp_body_json(response)
      lapply(models$data, function(data) data$id) |>
        unlist()
    }
  ),

  private = list(

    .api_check = function(model) {
      checkmate::assertCharacter(model)
      api_models <- self$availableModels()
      if (!model %in% api_models) {
        stop(
          glue::glue("{model} not available.\n"),
          "\n These are some models that are available to you:\n",
          paste0("  - ", sample(api_models, 10), collapse = "\n"),
          "\n For a complete list, call availableModels()\n",
          call. = FALSE
        )
      } else {
        message(glue::glue("{model} is a valid model"))
      }
    },

    .json_schema_check = function(jsonSchemaPath) {
      if (is.null(jsonSchemaPath)) {
        json_schema_path <- system.file("jsonSchemas",
                                        "cdm54schema-complete.json",
                                        package = "PatientGenerator")
        checkmate::assertFileExists(json_schema_path)
        self$json_schema_path <- json_schema_path
        } else {
        checkmate::assertCharacter(jsonSchemaPath)
        checkmate::assertFileExists(jsonSchemaPath)
        is_json <- tryCatch({
          jsonlite::fromJSON(jsonSchemaPath)
          message("The file at 'jsonSchemaPath' can be parsed as JSON")
          self$json_schema_path <- jsonSchemaPath
        }, error = function(e) {
          stop("The file at 'jsonSchemaPath' cannot be parsed as JSON")
        })
      }
    },

    .check_api_key = function(apiKeyName = "OPENAI_API_KEY") {
      key <- Sys.getenv("OPENAI_API_KEY", unset = "")
      if (!nzchar(key)) {
        stop(
          "API key not found.\n",
          "Set it with Sys.setenv(OPENAI_API_KEY = 'your_key') ",
          "and/or add it to your ~/.Renviron.",
          call. = FALSE
        )
      }
    },

    .register_codelist_tool = function() {
      retrieveCodelistTool <- ellmer::tool(
        fun = self$retrieveCodelist,
        description = "Retrieves data for concept_id search by concept_name and domain_id",
        name = "retrieveCodelist",
        arguments = list(
          concept_label = ellmer::type_string(description = "A regex to look up for a concept_name; the function uses stringr::regex to filter the concept_name"),
          domain = ellmer::type_string(description = "One word typically a domain from the OMOP-CDM, currently only available now: 'Drug', 'Condition', 'Measurement', 'Observation', 'Procedure'")
        )
      )
    }
  )
)
