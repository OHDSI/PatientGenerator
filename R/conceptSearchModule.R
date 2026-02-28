#' Concept search Shiny module
#'
#' Reusable UI and server for searching the vocabulary (Hecate) and selecting
#' a concept. Shows a button that opens a modal with text input, search button,
#' and results in a DT table. Optional callback when a concept is selected.
#'
#' @param id Module namespace id.
#' @param buttonLabel Label for the trigger button (default \code{"Concept search"}).
#' @param onConceptSelected Optional function of one argument \code{conceptId}
#'   called when user selects a concept; modal is closed after.
#' @param placeholderText Character string used as placeholder text in the
#'   search input field.
#' @name conceptSearchModule
NULL

#' @describeIn conceptSearchModule UI for the concept search: a single button that opens the search modal.
#' @export
conceptSearchUI <- function(id, buttonLabel = "Concept search") {
  ns <- shiny::NS(id)
  shiny::actionButton(ns("open_concept_search"), buttonLabel)
}

#' @describeIn conceptSearchModule Server for the concept search modal (text input, search, DT, close).
#' @export
conceptSearchServer <- function(id, onConceptSelected = NULL, placeholderText = "") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    searchResults <- shiny::reactiveVal(NULL)
    standardOnlyFilter <- shiny::reactiveVal(TRUE)

    shiny::observeEvent(input$open_concept_search, {
      searchResults(NULL)
      standardOnlyFilter(TRUE)
      shiny::showModal(
        shiny::modalDialog(
          title = "Search vocabulary",
          shiny::tags$style(shiny::HTML(".modal-dialog { max-width: 90%; width: 90%; }")),
          shiny::tags$script(shiny::HTML(paste0(
            "$(document).on('keydown', '#", ns("concept_query"), "', function(e) {",
            "  if (e.which === 13) { e.preventDefault(); $('#", ns("search_vocabulary"), "').click(); }",
            "});",
            "$(document).on('keydown', function(e) {",
            "  if (e.which === 27 && $('.modal.show').length) {",
            "    Shiny.setInputValue('", ns("modal_escape"), "', Math.random(), {priority: 'event'});",
            "  }",
            "});"
          ))),
          shiny::fluidRow(
            shiny::column(2,
              shiny::textInput(ns("concept_query"), "Query", placeholder = placeholderText)
            ),
            shiny::column(1, style = "padding-top: 32px;",
              shiny::actionButton(ns("search_vocabulary"), "Search", class = "btn-primary")
            ),
            shiny::column(4, style = "padding-top: 40px;",
              shiny::checkboxInput(ns("standard_concepts_only"), "Standard Concepts only", value = TRUE)
            )
          ),
          shiny::hr(),
          DT::DTOutput(ns("concept_results_table")),
          footer = shiny::tagList(
            shiny::modalButton("Close")
          ),
          size = "l"
        )
      )
    })

    shiny::observeEvent(input$search_vocabulary, {
      query <- shiny::req(input$concept_query)
      if (nchar(trimws(query)) < 1) return()
      df <- hecateSearch(query, limit = 50)
      if (!is.null(df) && nrow(df) > 0 && "searchScore" %in% names(df)) {
        df <- df[order(-df$searchScore), , drop = FALSE]
      }
      searchResults(df)
    })

    shiny::observeEvent(input$standard_concepts_only, {
      standardOnlyFilter(isTRUE(input$standard_concepts_only))
    }, ignoreNULL = TRUE)

    output$concept_results_table <- DT::renderDT({
      df <- searchResults()
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      if (isTRUE(standardOnlyFilter()) && "standardConcept" %in% names(df)) {
        df <- df[!is.na(df$standardConcept) & as.character(df$standardConcept) == "S", , drop = FALSE]
      }
      if (nrow(df) == 0) {
        return(NULL)
      }
      # Add a Select column with links (build onclick inline to avoid htmltools::JS dependency)
      inputId <- ns("selected_concept_row")
      df$Select <- vapply(
        seq_len(nrow(df)),
        function(i) {
          rowIdx <- i - 1L
          paste0(
            "<a href=\"#\" onclick=\"Shiny.setInputValue('", inputId, "', ", rowIdx,
            ", {priority: 'event'}); return false;\">Select</a>"
          )
        },
        character(1)
      )
      # Show key columns including standardConcept; keep Select last
      cols <- c("conceptId", "conceptName", "standardConcept", "domainId", "vocabularyId", "conceptCode", "Select")
      cols <- intersect(cols, names(df))
      df <- df[, cols, drop = FALSE]
      DT::datatable(
        df,
        escape = FALSE,
        selection = "none",
        options = list(
          pageLength = 10,
          dom = "frtip",
          search = list(smart = TRUE)
        )
      )
    })

    shiny::observeEvent(input$selected_concept_row, {
      row <- as.integer(shiny::req(input$selected_concept_row)) + 1L
      df <- searchResults()
      if (is.null(df)) return()
      if (isTRUE(standardOnlyFilter()) && "standardConcept" %in% names(df)) {
        df <- df[!is.na(df$standardConcept) & as.character(df$standardConcept) == "S", , drop = FALSE]
      }
      if (row < 1 || row > nrow(df)) return()
      conceptId <- df$conceptId[row]
      if (!is.null(onConceptSelected)) {
        onConceptSelected(conceptId)
      }
      shiny::removeModal()
    })

    shiny::observeEvent(input$modal_escape, {
      shiny::removeModal()
    })
  })
}
