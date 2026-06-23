inputDisplayLabel <- function(col_name, type) {
  default_label <- function(x) {
    stringr::str_to_sentence(stringr::str_replace_all(x, "_", " "))
  }

  if (identical(type, "person")) {
    return(default_label(col_name))
  }

  table_prefixes <- unique(c(
    type,
    stringr::str_remove(type, "_occurrence$"),
    stringr::str_remove(type, "_exposure$"),
    stringr::str_remove(type, "_period$"),
    if (identical(type, "observation_period")) "period" else character()
  ))
  table_prefixes <- table_prefixes[nzchar(table_prefixes)]
  table_prefixes <- table_prefixes[order(nchar(table_prefixes), decreasing = TRUE)]

  display_name <- col_name
  for (prefix in table_prefixes) {
    display_name <- stringr::str_remove(display_name, paste0("^", prefix, "_"))
  }

  if (display_name == "id") {
    return("ID")
  }

  default_label(display_name)
}

createInputs <- function(ns, type, columns, inverse = FALSE) {
  if (inverse) {
    columns <- setdiff(columnNames(type) |> names(), columns)
  }
  Map(function(col_name) {
    label <- inputDisplayLabel(col_name, type)
    if (stringr::str_detect(col_name, "date")) {
      column(2,
             class = "cdm-input-col",
             dateInput(ns(col_name),
                       label = label)
      )
    } else if (stringr::str_detect(col_name, "concept") & !stringr::str_detect(col_name, "gender")) {
      column(2,
             class = "cdm-input-col",
             textInput(ns(col_name),
                       label = label),
             uiOutput(ns(paste0(col_name, "_status")))
      )
    } else {
      if (!stringr::str_detect(col_name, "person_id")) {
        choices <- choicesList(tableName = type)
      } else {
        choices <- NULL
      }
        column(2,
               class = "cdm-input-col",
               selectizeInput(ns(col_name),
                              label = label,
                              choices = choices[[col_name]],
                              selected = NULL,
                              options = list(dropdownParent = "body"))
        )
    }
  }, columns)
}

updateInputs <- function(session, ns, type, cdmTableRow, columns) {


  Map(function(col_name) {
    # browser()
    if (stringr::str_detect(col_name, "date")) {
      updateDateInput(
        session = session,
        inputId = col_name,
        value = cdmTableRow[[col_name]],
        min = NULL,
        max = NULL
       )
    } else if (stringr::str_detect(col_name, "concept") & !stringr::str_detect(col_name, "gender")) {
      updateTextInput(
        session = session,
        inputId = col_name,
        value = cdmTableRow[[col_name]]
        )
    } else {
      updateSelectizeInput(
        session = session,
        inputId = col_name,
        selected = cdmTableRow[[col_name]],
        options = list(dropdownParent = "body")
        )
    }
  }, columns)
}

listInputParameters <- function(ns, input, type, columns) {
  lapply(columns, function(col) {
    input[[col]]
  })
}

updatePersonInputs <- function(cdmPersonData,
                               input_person_id,
                               ids,
                               session) {
  personTableSelection <- cdmPersonData %>%
    filter(person_id == input_person_id)
  Map(function(id) {
    updateSelectInput(session,
                      id,
                      selected = personTableSelection[[id]])
  }, ids)
}

updateTableIds <- function(cdm, type = "drug_exposure", input_person_id, session) {

  # Access names
  table_name <- glue::glue("get_{type}_table")
  table_person_id <- glue::glue("{type}_person_id")
  table_event_id <- glue::glue("{type}_id")
  table_module <- glue::glue("{type}_module")


  # Get observation period data and filter for the selected person_id
  cdmTable <- cdm[[table_name]]() %>%
    filter(person_id == as.numeric(input_person_id))

  # Update observation period fields
  updateSelectInput(session, table_person_id,
                    choices = cdmTable[["person_id"]] %>% unique(),
                    selected = cdmTable[["person_id"]] %>% unique())
  updateSelectInput(session, table_event_id,
                    choices = cdmTable[[table_event_id]],
                    selected = cdmTable[[table_event_id]][length(cdmTable[[table_event_id]])])

}

updateTableIdsNs <- function(cdm, type = "drug_exposure", input_person_id, session) {
  # browser()
  # Access names
  table_person_id <- glue::glue("{type}_person_id")
  table_event_id <- glue::glue("{type}_id")

  # Get observation period data and filter for the selected person_id
  cdmTable <- cdm[[type]]$data() %>%
    filter(person_id == as.numeric(input_person_id()))

  ns_module <- NS(type)

  # Update observation period fields
  updateSelectInput(session, ns_module(table_person_id),
                    choices = cdmTable[["person_id"]] %>% unique(),
                    selected = cdmTable[["person_id"]] %>% unique())
  updateSelectInput(session, ns_module(table_event_id),
                    choices = cdmTable[[table_event_id]],
                    selected = cdmTable[[table_event_id]][length(cdmTable[[table_event_id]])])

}

updateTablePersonEventIdsNs <- function(cdm, type = "drug_exposure", input_person_id, input_event_id, session) {
  # browser()
  # Access names
  table_person_id <- glue::glue("{type}_person_id")
  table_event_id <- glue::glue("{type}_id")

  p_id <- if (is.function(input_person_id)) input_person_id() else input_person_id
  req(p_id)

  e_id <- if (is.function(input_event_id)) input_event_id() else input_event_id
  req(e_id)

  # Get observation period data and filter for the selected person_id
  cdmTable <- cdm[[type]]$data() %>%
    filter(person_id == as.numeric(p_id))

  ns_module <- NS(type)

  # Update observation period fields
  updateSelectInput(session, ns_module(table_person_id),
                    choices = cdmTable[["person_id"]] %>% unique(),
                    selected = cdmTable[["person_id"]] %>% unique())
  updateSelectInput(session, ns_module(table_event_id),
                    choices = cdmTable[[table_event_id]],
                    selected = e_id)

}

updateTableDatesNs <- function(cdm,
                               type = "drug_exposure",
                               input_person_id,
                               input_event_id,
                               start_date,
                               end_date,
                               session,
                               input,
                               syncing) {

  table_start_date <- cdm[[type]]$tableNameDate("start")
  table_end_date <- cdm[[type]]$tableNameDate("end")
  if (length(table_end_date) == 0) {
    table_end_date <- NULL
  }
  table_id <- glue::glue("{type}_id")

  p_id <- if (is.function(input_person_id)) { input_person_id() } else { input_person_id }
  req(p_id)

  e_id <- if (is.function(input_event_id)) input_event_id() else input_event_id
  req(e_id)

  # Get observation period data and filter for the selected person_id
  # cdmTable <- cdm[[type]]$data() %>%
  #   filter(person_id == as.numeric(p_id)) %>%
  #   filter(.data[[table_id]] == as.numeric(e_id))

  ns_module <- NS(type)
  # Update observation period fields
  freezeReactiveValue(input, ns_module(table_start_date))
  updateDateInput(session, ns_module(table_start_date), value = start_date)

  if (!is.null(table_end_date)) {
    freezeReactiveValue(input, ns_module(table_end_date))
    updateDateInput(session, ns_module(table_end_date), value = end_date)
  }

}
