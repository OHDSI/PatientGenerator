cdmTableUI <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_body(
      fluidRow(
        column(
          2,
          actionButton(
            ns("add"),
            "Add"
          )
        ),
        # column(
        # 2,
        # actionButton(
        # ns("duplicate"),
        # "Duplicate"
        # )
        # ),
        column(
          2,
          conceptSearchUI(
            ns("concept_search"),
            buttonLabel = "Search Concept"
          )
        ),
        column(
          2,
          actionButton(
            ns("delete"),
            "Delete"
          )
        )
      ),
      br(),

      tagList(
        fluidRow(
          createInputs(
            ns,
            type = id,
            columns = columnNames(name = id, limit = 5) |> names()
          )
        )
      ),
      div(
        class = "reveal-on-full-screen",
        h5("Other columns"),
        fluidRow(
          tagList(
            createInputs(
              ns,
              type = id,
              columns = columnNames(name = id, limit = 5) |> names(),
              inverse = TRUE
            )
          )
        )
      ),
      fillable = FALSE
    )
  )
}

cdmTableServer <- function(
  id,
  cdm,
  person_id_selected,
  syncing,
  concept_lookup = hecateConceptLabel
) {
  moduleServer(
    id,
    function(
      input,
      output,
      session
    ) {
      ns <- session$ns
      table_event_id <- cdm[[id]]$tableNameId()
      table_concept_id <- cdm[[id]]$tableNameConceptId()
      table_start_date <- cdm[[id]]$tableNameDate("start")
      table_end_date <- cdm[[id]]$tableNameDate("end")
      if (length(table_end_date) == 0) {
        table_end_date <- NULL
      }
      all_columns <- columnNames(
        name = id,
        limit = NULL
      ) |>
        names()
      columnList <- if (identical(table_event_id, "person_id")) {
        all_columns |> tail(-1)
      } else {
        all_columns |> tail(-2)
      }
      concept_columns <- columnList[vapply(
        columnList,
        isConceptInputColumn,
        logical(1)
      )]

      concept_status_ids <- paste0(concept_columns, "_status")

      updateConceptLabel <- function(col_name, concept_id) {
        concept_label <- concept_lookup(concept_id)
        output[[paste0(col_name, "_status")]] <- shiny::renderUI(
          shiny::tags$span(
            class = if (grepl("invalid|not found", concept_label, ignore.case = TRUE)) {
              "text-danger small"
            } else {
              "text-muted small"
            },
            concept_label
          )
        )
      }

      lapply(concept_status_ids, function(output_id) {
        output[[output_id]] <- shiny::renderUI(NULL)
      })
      field_update <- reactiveVal(0L)

      ### ADD --------------------------------------------------------------------
      observeEvent(
        input$add,
        {
          # Require add button and the person id
          req(input$add)
          req(person_id_selected)
          # Create new event for that person in object
          date_inputs <- columnList[grep("_date$", columnList)]
          add_inputs <- c(date_inputs, intersect(table_concept_id, columnList))
          input_data <- setNames(
            lapply(add_inputs, function(col) input[[col]]),
            add_inputs
          )
          input_data <- input_data[
            vapply(input_data, function(value) {
              if (is.null(value) || length(value) == 0 || all(is.na(value))) {
                return(FALSE)
              }
              any(nzchar(trimws(as.character(value))))
            }, logical(1))
          ]
          args <- c(
            list(person_id = person_id_selected() |> as.integer()),
            input_data
          )
          do.call(cdm[[id]]$add, args)
          # Pull data from that person
          cdmTable <- cdm[[id]]$data() %>%
            dplyr::filter(
              person_id ==
                as.numeric(
                  person_id_selected()
                )
            )

          # Update interface to reflect addition
          updateSelectInput(
            session,
            "person_id",
            choices = cdmTable[["person_id"]] %>% unique(),
            selected = cdmTable[["person_id"]] %>% unique()
          )
          # When creating a new event the selected option is 
          # the last in the table
          updateSelectInput(
            session,
            table_event_id,
            choices = cdmTable[[table_event_id]],
            selected = cdmTable[[table_event_id]][length(
              cdmTable[[table_event_id]]
            )]
          )
        }
      )

      ### SEARCH CONCEPT ---------------------------------------------------------
      conceptSearchServer(
        id = "concept_search",
        onConceptSelected = function(conceptId) {
          shiny::updateTextInput(
            session,
            table_concept_id,
            value = as.character(conceptId)
          )
          updateConceptLabel(table_concept_id, conceptId)
        },
        placeholderText = "e.g. Metformin"
      )

      lapply(concept_columns, function(concept_col) {
        observeEvent(input[[concept_col]], {
          updateConceptLabel(concept_col, input[[concept_col]])
        }, ignoreInit = TRUE)
      })

      # Observe the event id to update its corresponding fields in the interface
      observeEvent(
        input[[table_event_id]],
        {
          req(input[[table_event_id]])

          # Retrieve row
          cdmTable <- cdm[[id]]$data()
          cdmTableRow <- cdmTable |>
            filter(
              .data[[table_event_id]] == input[[table_event_id]] |> as.integer()
            )

          # Update
          updateInputs(
            session,
            ns,
            type = id,
            cdmTableRow = cdmTableRow,
            columns = columnList
          )
        },
        ignoreInit = TRUE
      )

      observeOtherFields <- reactive({
        inputs <- setNames(
          lapply(columnList, function(col) input[[col]]),
          columnList
        )
        inputs[!vapply(inputs, is.null, logical(1))]
      })
      
      # UPDATE all other fields
      observeEvent(observeOtherFields(), {
        table_inputs <- observeOtherFields()
        
        no_date_inputs <- table_inputs[grep(
          "date",
          names(table_inputs),
          invert = TRUE
        )]
        non_empty_inputs <- any(vapply(
          no_date_inputs,
          function(x) {
            if (length(x) == 0 || all(is.na(x))) {
              return(FALSE)
            }
            any(nzchar(trimws(as.character(x))))
          },
          logical(1)
        ))
        req(non_empty_inputs)
        cdm_table_row <- cdm[[id]]$extractRow(
          event_id = input[[table_event_id]]
        )
        new_input_data <- as.data.frame(no_date_inputs)
        cdm_table_row[is.na(cdm_table_row)] <- ""
        new_input_data[is.na(new_input_data)] <- ""
        req(any(
          cdm_table_row[, names(no_date_inputs)] != new_input_data,
          na.rm = TRUE
        ))
        args <- c(
          list(event_id = as.integer(input[[table_event_id]])),
          no_date_inputs
        )
        do.call(cdm[[id]]$update, args)
        field_update(field_update() + 1L)
      })

      # # Delete event
      observeEvent(input$delete, {
        req(input[[table_event_id]])

        # Delete function
        cdm[[id]]$delete(input[[table_event_id]])

        tableDeleted <- cdm[[id]]$data()

        if (tableDeleted[[table_event_id]] %>% length() == 0) {
          # browser()
          # Update to null all inputs...
          cdmTableRow <- tableDeleted
          updateInputs(
            session,
            ns,
            type = id,
            cdmTableRow = cdmTableRow,
            columns = columnNames(name = id, limit = 5) |> names()
          )
        } else {
          # ... if not, the value of the previous person remains after the person id is deleted
          choicesPerson <- tableDeleted[[table_event_id]][
            tableDeleted$person_id == person_id_selected()
          ]
          updateSelectInput(
            session,
            table_event_id,
            choices = choicesPerson,
            selected = as.numeric(input[[table_event_id]]) - 1
          )
        }
      })
      elongation_click <- reactiveVal(NULL)
      #
      # # Elongation
      tableDateInputs <- reactive({
        values <- list(input[[table_start_date]])
        if (!is.null(table_end_date)) {
          values <- c(values, list(input[[table_end_date]]))
        }
        values
      })

      observeEvent(
        tableDateInputs(),
        {
          req(!syncing())
          req(input[[table_event_id]])
          req(input[[table_start_date]])
          end_date <- NULL
          if (!is.null(table_end_date)) {
            req(input[[table_end_date]])
            end_date <- input[[table_end_date]]
          }

          cdm[[id]]$updateDates(
            person_id = person_id_selected(),
            event_id = input[[table_event_id]],
            start_date = input[[table_start_date]],
            end_date = end_date
          )
          elongation_click(Sys.time())
        },
        ignoreInit = TRUE
      )

      return(
        c(
          list(
            add_click = reactive(input$add),
            delete_click = reactive(input$delete),
            field_update = reactive(field_update()),
            elongation_click = reactive(elongation_click())
          ),
          setNames(list(reactive(input[[table_event_id]])), table_event_id)
        )
      )
    }
  )
}

#' UI for the OMOP fact_relationship table.
#'
#' Unlike the other Designer tables, fact_relationship is not a patient-level
#' event: it has no person_id, date, or single primary-key column.  It is
#' therefore edited as rows identified by their position in the table.
#' @noRd
factRelationshipUI <- function(id) {
  ns <- shiny::NS(id)
  columns <- names(columnNames("fact_relationship"))

  bslib::card(
    full_screen = TRUE,
    bslib::card_body(
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(ns("row_index"), "Relationship row", choices = character())),
        shiny::column(2, shiny::actionButton(ns("add"), "Add")),
        shiny::column(2, shiny::actionButton(ns("update"), "Update")),
        shiny::column(2, shiny::actionButton(ns("delete"), "Delete"))
      ),
      shiny::br(),
      shiny::fluidRow(lapply(columns, function(column_name) {
        shiny::column(
          2,
          class = "cdm-input-col",
          shiny::numericInput(
            ns(column_name),
            label = inputDisplayLabel(column_name, "fact_relationship"),
            value = NULL,
            step = 1
          )
        )
      }))
    )
  )
}

#' Server logic for the OMOP fact_relationship table.
#' @noRd
factRelationshipServer <- function(id, cdm, data_version = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    columns <- names(columnNames("fact_relationship"))
    field_update <- shiny::reactiveVal(0L)

    refreshRows <- function(selected = NULL) {
      row_count <- nrow(cdm$fact_relationship$data())
      choices <- if (row_count == 0) character() else as.character(seq_len(row_count))
      shiny::updateSelectInput(
        session,
        "row_index",
        choices = choices,
        selected = selected %||% if (length(choices)) tail(choices, 1) else character()
      )
    }

    if (!is.null(data_version)) {
      shiny::observeEvent(data_version(), {
        refreshRows()
      }, ignoreInit = FALSE)
    }

    values <- function() {
      values <- lapply(columns, function(column_name) input[[column_name]])
      names(values) <- columns
      if (any(vapply(values, function(value) is.null(value) || is.na(value), logical(1)))) {
        return(NULL)
      }
      lapply(values, as.integer)
    }

    observeEvent(input$add, {
      row_values <- values()
      shiny::req(!is.null(row_values))
      do.call(cdm$fact_relationship$addRow, row_values)
      refreshRows(as.character(nrow(cdm$fact_relationship$data())))
      field_update(field_update() + 1L)
    })

    observeEvent(input$row_index, {
      row_index <- suppressWarnings(as.integer(input$row_index))
      shiny::req(!is.na(row_index), row_index >= 1, row_index <= nrow(cdm$fact_relationship$data()))
      row <- cdm$fact_relationship$data()[row_index]
      for (column_name in columns) {
        shiny::updateNumericInput(session, column_name, value = row[[column_name]])
      }
    }, ignoreInit = TRUE)

    observeEvent(input$update, {
      row_index <- suppressWarnings(as.integer(input$row_index))
      row_values <- values()
      shiny::req(!is.na(row_index), !is.null(row_values))
      do.call(cdm$fact_relationship$updateRow, c(list(row_index = row_index), row_values))
      field_update(field_update() + 1L)
    })

    observeEvent(input$delete, {
      row_index <- suppressWarnings(as.integer(input$row_index))
      shiny::req(!is.na(row_index))
      cdm$fact_relationship$deleteRow(row_index)
      refreshRows()
      field_update(field_update() + 1L)
    })

    list(
      add_click = shiny::reactive(input$add),
      delete_click = shiny::reactive(input$delete),
      field_update = shiny::reactive(field_update())
    )
  })
}
