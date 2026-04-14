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
  syncing
) {
  moduleServer(
    id,
    function(
      input,
      output,
      session
    ) {
      ns <- session$ns
      if (id == "condition_occurrence") {
        table_id <- "condition"
      } else {
        table_id <- id
      }

      table_event_id <- paste(
        id,
        "id",
        sep = "_"
      )
      if (id == "drug_exposure") {
        table_concept_id <- "drug_concept_id"
      } else {
        table_concept_id <- paste(
          table_id,
          "concept_id",
          sep = "_"
        )
      }
      table_start_date <- paste(
        table_id,
        "start_date",
        sep = "_"
      )
      table_end_date <- paste(
        table_id,
        "end_date",
        sep = "_"
      )
      columnList <- columnNames(
        name = id,
        limit = NULL
      ) |>
        names() |>
        tail(-2)

      ### ADD --------------------------------------------------------------------
      observeEvent(
        input$add,
        {
          # browser()
          # Require add button and the person id
          req(input$add)
          req(person_id_selected)
          # Create new event for that person in object
          cdm[[id]]$add(person_id = person_id_selected() |> as.integer())
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
          # When creating a new event the selected option is the last in the table
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
        },
        placeholderText = "e.g. Metformin"
      )

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

      # UPDATE all other fields
      observe({
        inputs <- reactiveValuesToList(input)
        table_inputs <- inputs[names(inputs) %in% columnList]
        no_date_inputs <- table_inputs[grep(
          "date",
          names(table_inputs),
          invert = TRUE
        )]
        non_empty_inputs <- any(vapply(
          no_date_inputs,
          function(x) {
            is.character(x) && nzchar(x)
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
        # browser()
        do.call(cdm[[id]]$update, args)
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
      observeEvent(
        list(input[[table_start_date]], input[[table_end_date]]),
        {
          print(syncing)
          req(!syncing())
          req(input[[table_start_date]])
          req(input[[table_end_date]])

          cdm[[id]]$updateDates(
            person_id = person_id_selected(),
            event_id = input[[table_event_id]],
            start_date = input[[table_start_date]],
            end_date = input[[table_end_date]]
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
            elongation_click = reactive(elongation_click())
          ),
          setNames(list(reactive(input[[table_event_id]])), table_event_id)
        )
      )
    }
  )
}
