#' `patientDesigner()` is a visual interface based on D3 to construct test datasets for the OMOP-CDM
#'
#' @param path Optional folder containing JSON test sets.
#' If NULL, default path resolution keeps testthat integration.
#' @returns A Shiny app
#' @import r2d3 shiny bslib dplyr
#' @importFrom stats setNames
#' @importFrom utils tail
#' @importFrom data.table as.data.table set rbindlist
#' @export
patientDesigner <- function(path = NULL) {

  # bootswatch_themes <- c(
  #   "cerulean","cosmo","cyborg","darkly","flatly","journal","litera","lumen",
  #   "lux","materia","minty","morph","pulse","quartz","sandstone","simplex",
  #   "sketchy","slate","solar","spacelab","superhero","united","vapor","yeti","zephyr"
  # )

  ui <- page_fillable(
    tags$head(
      tags$style(HTML("
      /* Hide the element by default (when card is NOT full screen) */
      .bslib-card[data-full-screen='false'] .reveal-on-full-screen {
        display: none !important;
      }

      /* Optional: Add a transition or margin for smoother appearance */
      .reveal-on-full-screen {
        margin-top: 15px;
        padding-top: 15px;
        border-top: 1px solid #eee;
      }
    "))
    ),
    layout_sidebar(

    sidebar = sidebar(
        h4("PatientDesigner"),
        h6(actionLink(
            inputId = "new_test_set",
            label = strong("New Test Set"),
            icon = icon("pen-to-square"),
            class = "text-reset text-decoration-none"
          )
          ),
        fileInput(
          "upload_xlsx",
          "Upload xlsx test data",
          accept = c(
            ".xlsx",
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            )
          ),
        br(),
        br(),
        h6(strong("Test Sets")),
        uiOutput("sidebar_file_list"),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        actionButton(
          "save_current",
          "Save Test Set",
          icon = icon("floppy-disk")
          ),
        downloadButton(
          "downloadTestSet",
          "Download Test Set",
          icon = icon("download")
          ),
        position = c("left"),
        open = "open"
        # selectInput("theme", "Bootswatch theme:", bootswatch_themes, selected = "flatly")
    ),
    layout_sidebar(
      sidebar = sidebar(
        h6(actionLink(
            inputId = "new_chat",
            label = strong("New Chat"),
            icon = icon("comment-dots"),
            class = "text-reset text-decoration-none"
            )
           ),
        position = "right", open = FALSE),
      tabsetPanel(
        tabPanel(
          "Person",
          personUI(id = "person"),
          tags$style(HTML("
  .well {
    padding: 1rem 1rem .15rem 1rem
  }
  "))
        )
      ),
      tabsetPanel(
        id = "cdm_table_tabs",
        tabPanel(
          "Observation Period",
          cdmTableUI(id = "observation_period"),
          value = "observation_period_module"
          ),
        tabPanel(
          "Condition Occurrence",
          cdmTableUI(id = "condition_occurrence"),
          value = "condition_occurrence_module"
          ),
        tabPanel(
          "Drug Exposure",
          cdmTableUI(id = "drug_exposure"),
          value = "drug_exposure_module"
          ),
        tabPanel(
          "Measurement",
          cdmTableUI(id = "measurement"),
          value = "measurement_module"
          ),
        tabPanel(
          "Procedure Occurrence",
          cdmTableUI(id = "procedure_occurrence"),
          value = "procedure_occurrence_module"
          ),
        tabPanel(
          "Observation",
          cdmTableUI(id = "observation"),
          value = "observation_module"
          )
      ),
      tabsetPanel(
        tabPanel(
          "Timeline",
          br(),
          d3Output(
            "d3",
            height = "1000px"
            )
        ),
        tabPanel(
          "Test Data",
          tableOutput("cdmData"),
          tableOutput("personDataTable"),
          tableOutput("observationPeriodTable"),
          tableOutput("drugExposureTable"),
          tableOutput("conditionOccurrenceTable"),
          tableOutput("measurementTable"),
          tableOutput("procedureOccurrenceTable"),
          tableOutput("observationTable")
          )
      ),
      border = FALSE
    ),
    border_radius = FALSE,
    fillable = TRUE,
    class = "p-0"
  ),
  padding = c(0),
  title = "OHDSI - PatientGenerator - PatientDesigner",
  theme = bs_theme(version = 5, bootswatch = "zephyr")  # initial theme
  )



  server <- function(input, output, session) {

    # Swap theme in real time
    # observeEvent(input$theme, ignoreInit = TRUE, {
    session$setCurrentTheme(
      bs_theme(
        version = 5,
        bootswatch = "zephyr"
        )
      )
    # })

    # TRIGGERS
    file_refresh_trigger <- reactiveVal(0)
    loaded_listeners <- reactiveVal(character(0))
    data_version <- reactiveVal(0)

    # TestCases folder
    get_test_dir <- function() {
      testSetDir(path = path, create = TRUE)
    }

    # Create CDM object
    cdm <- cdmConstructor$new()

    # Wipe clean
    observeEvent(input$new_test_set, {
      # browser()
      cdm$reset()
      data_version(data_version() + 1)
    })

    observeEvent(input$upload_xlsx, {
      req(input$upload_xlsx)
      result <- tryCatch(
        cdm$loadXlsxTestSet(input$upload_xlsx$datapath),
        error = function(e) e
        )

      if (inherits(result, "error")) {
        showNotification(
          conditionMessage(result),
          type = "error",
          duration = 8
          )
        return(invisible(NULL))
      }

      data_version(data_version() + 1)

      if (length(result$ignored) > 0) {
        showNotification(
          glue::glue(
            "Loaded xlsx test data. Ignored unsupported sheets: {glue::glue_collapse(result$ignored, sep = ', ')}."
            ),
          type = "warning",
          duration = 8
          )
      } else {
        showNotification(
          "Loaded xlsx test data.",
          type = "message",
          duration = 5
          )
      }
    })

    ##### Update saved file in sidebar
    current_files <- reactive({
      file_refresh_trigger()
      list.files(
        get_test_dir(),
        pattern = "\\.json$",
        full.names = FALSE
        )
    })

    # Render list UI
    output$sidebar_file_list <- renderUI({
      files <- current_files()

      tagList(
        lapply(files, function(f) {
          # ID includes extension to be unique and consistent
          # Display name removes extension for looks
          actionLink(
            inputId = paste0("link_", f),
            label = tools::file_path_sans_ext(f),
            class = "text-reset text-decoration-none d-block mb-1"
          )
        })
      )
    })

    # Handles old and new files
    observe({
      files <- current_files()
      existing <- loaded_listeners()
      new_files <- setdiff(files, existing)

      lapply(new_files, function(filename) {

        id <- paste0("link_", filename)

        observeEvent(input[[id]], {
          path <- file.path(get_test_dir(), filename)
          cdm$loadJsonTestSet(path)
          data_version(data_version() + 1)
        })
      })

      if (length(new_files) > 0) {
        loaded_listeners(c(existing, new_files))
      }
      })

    observeEvent(input$save_current, {
      showModal(modalDialog(
        title = "Save Test Set",
        textInput(
          "new_filename",
          "Filename (no extension):",
          placeholder = "my_test"
          ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "confirm_save",
            "Save",
            class = "btn-primary"
            )
        )
      ))
    })

    observeEvent(input$confirm_save, {
      req(input$new_filename)

      new_name <- paste0(
        tools::file_path_sans_ext(input$new_filename),
        ".json"
        )
      path <- file.path(
        get_test_dir(),
        new_name
        )
      write(
        cdm$getCdmData(),
        path
        )

      file_refresh_trigger(file_refresh_trigger() + 1)
      removeModal()
    })


    ##### Load JSON Test Set
    lapply(
      getTestSets(
        path = get_test_dir()),
      function(filename) {
        id <- paste0("link_", filename)
        observeEvent(input[[id]], {
          path <- file.path(
            get_test_dir(),
            paste(
              filename,
              "json",
              sep = "."
              )
            )
        cdm$loadJsonTestSet(path)
        data_version(data_version() + 1)
      })
    })

    ##### PERSON TABLE

    # Person server module - Create, delete and update
    person_module <- personServer(
      id = "person",
      cdm = cdm,
      trigger = data_version
      )

    # Render person table
    output$personDataTable <- renderTable({
      data_version()
      person_module()
      cdm$person$data()
    })

    # After person selection, refresh event selectors for all
    # patient-level event tables.
    observeEvent(person_module(), {
      req(person_module())

      updateTableIdsNs(
        cdm = cdm,
        type = "observation_period",
        input_person_id = person_module,
        session = session
        )
      updateTableIdsNs(
        cdm = cdm,
        type = "condition_occurrence",
        input_person_id = person_module,
        session = session
        )
      updateTableIdsNs(
        cdm = cdm,
        type = "measurement",
        input_person_id = person_module,
        session = session
        )
      updateTableIdsNs(
        cdm = cdm,
        type = "procedure_occurrence",
        input_person_id = person_module,
        session = session
        )
      updateTableIdsNs(
        cdm = cdm,
        type = "observation",
        input_person_id = person_module,
        session = session
        )
      updateTableIdsNs(
        cdm = cdm,
        type = "drug_exposure",
        input_person_id = person_module,
        session = session
        )

    }, ignoreInit = TRUE)

    ##### OBSERVATION PERIOD
    # - Each section shows only data from the selected individual
    #   in the person section

    # Module - Create, delete and update
    observation_period_module <- cdmTableServer(
      id = "observation_period",
      cdm = cdm,
      person_id_selected = person_module,
      syncing = syncing
      )

    # Render observation period table
    output$observationPeriodTable <- renderTable({
      data_version()
      observation_period_module$add_click()
      observation_period_module$delete_click()
      observation_period_module$elongation_click()
      formatDateColumns(cdm$observation_period$data())
    })

    ##### DRUG EXPOSURE TABLE

    drug_exposure_module <- cdmTableServer(
      id = "drug_exposure",
      cdm = cdm,
      person_id_selected = person_module,
      syncing = syncing
      )


    # Render drug exposure table
    output$drugExposureTable <- renderTable({
      data_version()
      drug_exposure_module$add_click()
      drug_exposure_module$delete_click()
      drug_exposure_module$elongation_click()
      formatDateColumns(cdm$drug_exposure$data())
    })

    # CONDITION OCCURRENCE TABLE
    condition_occurrence_module <- cdmTableServer(
      id = "condition_occurrence",
      cdm = cdm,
      person_id_selected = person_module,
      syncing = syncing
      )

    # Render drug exposure table
    output$conditionOccurrenceTable <- renderTable({
      data_version()
      condition_occurrence_module$add_click()
      condition_occurrence_module$delete_click()
      condition_occurrence_module$elongation_click()
      formatDateColumns(cdm$condition_occurrence$data())
    })

    # MEASUREMENT TABLE
    measurement_module <- cdmTableServer(
      id = "measurement",
      cdm = cdm,
      person_id_selected = person_module,
      syncing = syncing
      )

    output$measurementTable <- renderTable({
      data_version()
      measurement_module$add_click()
      measurement_module$delete_click()
      measurement_module$elongation_click()
      formatDateColumns(cdm$measurement$data())
    })
    
    # PROCEDURE OCCURRENCE TABLE
    procedure_occurrence_module <- cdmTableServer(
      id = "procedure_occurrence",
      cdm = cdm,
      person_id_selected = person_module,
      syncing = syncing
    )
    
    output$procedureOccurrenceTable <- renderTable({
      data_version()
      procedure_occurrence_module$add_click()
      procedure_occurrence_module$delete_click()
      procedure_occurrence_module$elongation_click()
      formatDateColumns(cdm$procedure_occurrence$data())
    })

    # OBSERVATION TABLE
    observation_module <- cdmTableServer(
      id = "observation",
      cdm = cdm,
      person_id_selected = person_module,
      syncing = syncing
    )

    output$observationTable <- renderTable({
      data_version()
      observation_module$add_click()
      observation_module$delete_click()
      observation_module$elongation_click()
      cdm$observation$data()
    })

    # CDM Data Timeline
    cdmDataTimeline <- reactive({
      pid <- suppressWarnings(as.numeric(person_module()))
      req(!is.na(pid), length(pid) == 1)
      
      cdm$getCdmDataTimeline() %>%
        dplyr::filter(.data$person_id == pid)
    }) %>% bindEvent(
      data_version(),
      person_module(),
      observation_period_module$add_click(),
      observation_period_module$delete_click(),
      observation_period_module$elongation_click(),
      drug_exposure_module$add_click(),
      drug_exposure_module$delete_click(),
      drug_exposure_module$elongation_click(),
      condition_occurrence_module$add_click(),
      condition_occurrence_module$delete_click(),
      condition_occurrence_module$elongation_click(),
      measurement_module$add_click(),
      measurement_module$delete_click(),
      measurement_module$elongation_click(),
      procedure_occurrence_module$add_click(),
      procedure_occurrence_module$delete_click(),
      procedure_occurrence_module$elongation_click(),
      observation_module$add_click(),
      observation_module$delete_click(),
      observation_module$elongation_click(),
      ignoreInit = FALSE
      )

    # Render cdm table
    output$cdmData <- renderTable({
      req(cdmDataTimeline)
      formatDateColumns(cdmDataTimeline())
    })

    ## UPDATE DATA FROM D3

    # Drag behavior - start, move and end

    syncing <- reactiveVal(FALSE)

    observeEvent(input$bar_start, {
      # browser()
      syncing(TRUE)
      start_data <- input$bar_start
      type_module <- paste(
        start_data$type,
        "module",
        sep = "_"
        )
      updateTabsetPanel(
        session,
        "cdm_table_tabs",
        selected = type_module
        )
      updateTablePersonEventIdsNs(
        cdm,
        type = start_data$type,
        input_person_id = start_data$person_id,
        input_event_id = start_data$event_id,
        session
        )
    })

    observeEvent(input$bar_end, {
      update_data <- normalizeBarEndUpdate(input$bar_end)
      person_id <- update_data$person_id
      event_id <- update_data$event_id
      type <- update_data$type
      has_end_date <- length(cdm[[type]]$tableNameDate("end")) > 0
      end_date <- if (isTRUE(has_end_date)) update_data$end_date else NULL
      print("END DATA:")
      update_data$start_date %>% print()
      end_date %>% print()
      cdm[[type]]$updateDates(
        person_id = person_id,
        event_id = event_id,
        start_date = update_data$start_date,
        end_date = end_date
        )
      updateTableDatesNs(
        cdm = cdm,
        type = type,
        input_person_id = person_id,
        input_event_id = event_id,
        start_date = update_data$start_date,
        end_date = end_date,
        session = session,
        input = input,
        syncing = syncing
        )
      syncing(FALSE)

    })

    output$downloadTestSet <- downloadHandler(
      filename = function() {
        paste("patientDesigner", ".json", sep = "")
      },
      content = function(file) {
        write(cdm$getCdmData(), file)
      }
    )

    ##### D3 TIMELINE
    output$d3 <- renderD3({
      r2d3(
        data = cdmDataTimeline(),
        script = system.file("d3/cdm_timeline.js", package = "PatientGenerator"),
        height = 1000
      )
    })
  }
  shinyApp(ui = ui, server = server)
}
