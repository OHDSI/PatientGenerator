# Copyright 2023 DARWIN EU®
#
# Hecate vocabulary search API client and search helpers.
# Uses camelCase and namespaced calls (httr2::, dplyr::).

#' Get Hecate client configuration
#'
#' Reads configuration from \code{options("patientgenerator.hecate")} (list with
#' \code{base_url}, \code{timeout_ms}, \code{api_key}) or environment variables
#' \code{HECATE_BASE_URL}, \code{HECATE_TIMEOUT_MS}, \code{HECATE_API_KEY}.
#' If the default host is unreachable (e.g. "Could not resolve host"),
#' set \code{Sys.setenv(HECATE_BASE_URL = "https://your-hecate-server")} to
#' point to your Hecate instance.
#'
#' @return A list with \code{baseUrl}, \code{timeoutMs}, \code{apiKey}.
#' @noRd
getHecateConfig <- function() {
  cfg <- getOption("patientgenerator.hecate", list())
  list(
    baseUrl = cfg$base_url %||% Sys.getenv("HECATE_BASE_URL", "https://hecate.pantheon-hds.com/api"),
    timeoutMs = cfg$timeout_ms %||% as.integer(Sys.getenv("HECATE_TIMEOUT_MS", "10000")),
    apiKey = cfg$api_key %||% Sys.getenv("HECATE_API_KEY", "")
  )
}

#' Null coalescing operator
#'
#' @param x First value (any type).
#' @param y Fallback value when \code{x} is NULL.
#' @return \code{x} if not NULL, otherwise \code{y}.
#' @name nullOr
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create a rate limiter
#'
#' @param maxCalls Maximum calls allowed in the time window.
#' @param perSeconds Time window in seconds.
#' @return A function that blocks when the rate limit would be exceeded.
#' @noRd
createRateLimiter <- function(maxCalls = 100, perSeconds = 60) {
  callTimes <- numeric(0)
  function() {
    now <- as.numeric(Sys.time())
    callTimes <<- callTimes[callTimes > (now - perSeconds)]
    if (length(callTimes) >= maxCalls) {
      waitTime <- perSeconds - (now - min(callTimes))
      Sys.sleep(max(waitTime, 0))
      callTimes <<- numeric(0)
    }
    callTimes <<- c(callTimes, now)
    invisible(NULL)
  }
}

hecateRateLimiter <- createRateLimiter(100, 60)

#' Create a Hecate API client
#'
#' @param baseUrl Base URL of the Hecate API (default from config).
#' @param timeoutMs Timeout in milliseconds (default from config).
#' @param apiKey Optional API key for authorization (default from config).
#' @return A client object with class \code{hecate_client}.
#' @export
hecateClient <- function(baseUrl = NULL, timeoutMs = NULL, apiKey = NULL) {
  cfg <- getHecateConfig()
  baseUrl <- baseUrl %||% cfg$baseUrl
  timeoutMs <- timeoutMs %||% cfg$timeoutMs
  apiKey <- apiKey %||% cfg$apiKey

  if (nzchar(apiKey) && nchar(apiKey) < 10) {
    warning("API key appears too short. Please check HECATE_API_KEY environment variable.", call. = FALSE)
  }

  if (isTRUE(getOption("app.debug"))) {
    message("Initializing Hecate client with URL: ", baseUrl)
    message("API key ", if (nzchar(apiKey)) "provided" else "not provided")
  }

  structure(
    list(
      baseUrl = sub("/+$", "", baseUrl),
      timeoutMs = timeoutMs,
      apiKey = apiKey
    ),
    class = "hecate_client"
  )
}

#' Build a Hecate API request
#' @noRd
hecateRequest <- function(client, path, query = NULL) {
  url <- paste0(client$baseUrl, "/", sub("^/+", "", path))
  req <- httr2::request(url) |>
    httr2::req_timeout(client$timeoutMs / 1000)

  if (nzchar(client$apiKey)) {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", client$apiKey))
  }

  if (!is.null(query)) {
    query <- Filter(Negate(is.null), query)
    req <- httr2::req_url_query(req, !!!query)
  }

  req
}

#' Perform a Hecate API request and return parsed JSON or error list
#' Handles connection failures (e.g. unresolved host) and HTTP errors.
#' @noRd
hecatePerform <- function(req) {
  hecateRateLimiter()
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)

  if (inherits(resp, "error")) {
    msg <- conditionMessage(resp)
    code <- "request_failed"
    if (grepl("Could not resolve host|resolve hostname|getaddrinfo", msg, ignore.case = TRUE)) {
      code <- "host_unreachable"
    }
    return(list(error = "Request failed", code = code, message = msg))
  }

  bodyTxt <- httr2::resp_body_string(resp)
  parsed <- tryCatch(jsonlite::fromJSON(bodyTxt, simplifyVector = FALSE), error = function(e) NULL)

  if (is.null(parsed)) {
    return(list(error = "Invalid JSON response", code = "parse_error", message = bodyTxt))
  }
  parsed
}

#' Search Hecate concepts and return results as a data frame
#'
#' @param query Character(1); search query (required).
#' @param vocabularyId Character(1) or NULL; optional vocabulary filter (comma-separated).
#' @param standardConcept Character(1) or NULL; e.g. \code{"S"} (Standard), \code{"C"} (Classification).
#' @param domainId Character(1) or NULL; optional domain filter (comma-separated).
#' @param conceptClassId Character(1) or NULL; optional concept class filter.
#' @param limit Integer(1); max results (default 20, max 150).
#' @param client Hecate client (default \code{hecateClient()}).
#' @return Data frame of search results, or NULL if an error occurred (API error or bad response shape).
#'
#' @examples
#' \dontrun{
#' # Simple search
#' df <- hecateSearch("diabetes")
#'
#' # Search with filters
#' df <- hecateSearch("hypertension", domainId = "Condition", limit = 10)
#' }
#' @export
hecateSearch <- function(
    query,
    vocabularyId = NULL,
    standardConcept = NULL,
    domainId = NULL,
    conceptClassId = NULL,
    limit = 20,
    client = hecateClient()) {
  if (!is.character(query) || length(query) != 1 || is.na(query) || nchar(query) < 1) {
    stop("`query` must be a non-empty string.")
  }

  req <- hecateRequest(
    client,
    path = "search",
    query = list(
      q = query,
      vocabulary_id = vocabularyId,
      standard_concept = standardConcept,
      domain_id = domainId,
      concept_class_id = conceptClassId,
      limit = limit
    )
  )

  res <- hecatePerform(req)

  if (is.list(res) && "error" %in% names(res)) {
    detailStr <- if (!is.null(res$message)) paste(" -", res$message) else ""
    if (identical(res$code, "host_unreachable")) {
      warning(
        "Hecate API unreachable: ", res$error, detailStr, "\n",
        "  Set HECATE_BASE_URL to your Hecate instance (e.g. Sys.setenv(HECATE_BASE_URL = \"https://your-hecate-server\")).",
        call. = FALSE
      )
    } else {
      warning("API error: ", res$error, detailStr, call. = FALSE)
    }
    return(NULL)
  }

  parsed <- res

  if (!is.list(parsed) || length(parsed) == 0) {
    warning("Unexpected response format or empty response")
    return(NULL)
  }

  if (length(parsed) > 0) {
    firstElem <- parsed[[1]]
    if (!is.list(firstElem) || !"concepts" %in% names(firstElem)) {
      warning("Response does not match expected structure (missing 'concepts' field)")
      return(NULL)
    }
  }

  resultRows <- list()

  for (i in seq_along(parsed)) {
    searchResult <- parsed[[i]]

    searchConceptName <- if (is.null(searchResult$concept_name)) NA_character_ else searchResult$concept_name
    searchConceptNameLower <- if (is.null(searchResult$concept_name_lower)) NA_character_ else searchResult$concept_name_lower
    searchScore <- if (is.null(searchResult$score)) NA_real_ else searchResult$score

    conceptsList <- searchResult$concepts
    if (is.null(conceptsList) || length(conceptsList) == 0) {
      next
    }

    for (j in seq_along(conceptsList)) {
      concept <- conceptsList[[j]]

      row <- list(
        searchConceptName = searchConceptName,
        searchConceptNameLower = searchConceptNameLower,
        searchScore = searchScore,
        conceptId = if (is.null(concept$concept_id)) NA_integer_ else concept$concept_id,
        conceptName = if (is.null(concept$concept_name)) NA_character_ else concept$concept_name,
        domainId = if (is.null(concept$domain_id)) NA_character_ else concept$domain_id,
        vocabularyId = if (is.null(concept$vocabulary_id)) NA_character_ else concept$vocabulary_id,
        conceptClassId = if (is.null(concept$concept_class_id)) NA_character_ else concept$concept_class_id,
        standardConcept = if (is.null(concept$standard_concept)) NA_character_ else concept$standard_concept,
        conceptCode = if (is.null(concept$concept_code)) NA_character_ else concept$concept_code,
        invalidReason = if (is.null(concept$invalid_reason)) NA_character_ else concept$invalid_reason,
        validStartDate = if (is.null(concept$valid_start_date)) NA_character_ else concept$valid_start_date,
        validEndDate = if (is.null(concept$valid_end_date)) NA_character_ else concept$valid_end_date,
        recordCount = if (is.null(concept$record_count)) NA_integer_ else concept$record_count
      )

      resultRows <- c(resultRows, list(row))
    }
  }

  if (length(resultRows) == 0) {
    warning("No concepts found in response")
    return(NULL)
  }

  tryCatch(
    {
      df <- dplyr::bind_rows(resultRows)
      return(df)
    },
    error = function(e) {
      warning("Error converting to dataframe: ", conditionMessage(e))
      return(NULL)
    }
  )
}
