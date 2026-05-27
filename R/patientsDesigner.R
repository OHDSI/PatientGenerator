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
        br(),
        patientDesignerChatUI("designer_chat"),
        width = "30rem",
        position = "right",
        open = FALSE
        ),
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
          # verbatimTextOutput("cdmData"),
          tableOutput("personDataTable"),
          tableOutput("observationPeriodTable"),
          tableOutput("drugExposureTable"),
          tableOutput("conditionOccurrenceTable"),
          tableOutput("measurementTable"),
          tableOutput("procedureOccurrenceTable")
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
    chat_generator <- reactiveVal(NULL)
    chat_temp_dir <- file.path(
      tempdir(),
      paste0("patientDesigner-chat-", session$token)
    )
    dir.create(chat_temp_dir, recursive = TRUE, showWarnings = FALSE)
    session$onSessionEnded(function() {
      unlink(chat_temp_dir, recursive = TRUE, force = TRUE)
    })

    # TestCases folder
    get_test_dir <- function() {
      testSetDir(path = path, create = TRUE)
    }

    list_test_set_records <- function() {
      saved_paths <- list.files(
        get_test_dir(),
        pattern = "\\.json$",
        full.names = TRUE
      )
      chat_paths <- list.files(
        chat_temp_dir,
        pattern = "\\.json$",
        full.names = TRUE
      )

      build_records <- function(paths, source) {
        if (length(paths) == 0) {
          return(data.frame(
            id = character(),
            label = character(),
            path = character(),
            source = character(),
            stringsAsFactors = FALSE
          ))
        }

        data.frame(
          id = vapply(
            paste(source, basename(paths), sep = "_"),
            function(x) gsub("[^A-Za-z0-9_]+", "_", x),
            character(1)
          ),
          label = if (identical(source, "chat")) {
            paste0("[chat] ", tools::file_path_sans_ext(basename(paths)))
          } else {
            tools::file_path_sans_ext(basename(paths))
          },
          path = paths,
          source = source,
          stringsAsFactors = FALSE
        )
      }

      rbind(
        build_records(saved_paths, "saved"),
        build_records(chat_paths, "chat")
      )
    }

    pick_chat_model <- function() {
      models <- availableModels()
      preferred <- c(
        "gpt-5.4",
        "gpt-5.2",
        "gpt-5",
        "gpt-4.1",
        "gpt-4o"
      )
      model <- preferred[preferred %in% models][1]
      if (is.na(model) || is.null(model)) {
        model <- models[[1]]
      }
      if (is.null(model) || !nzchar(model)) {
        stop("No available OpenAI model for this key.", call. = FALSE)
      }
      model
    }

    get_chat_generator <- function() {
      generator <- chat_generator()
      if (!is.null(generator)) {
        return(generator)
      }

      generator <- patientChat$new(
        model = pick_chat_model(),
        echo = "none"
      )
      chat_generator(generator)
      generator
    }

    # Create CDM object
    cdm <- cdmConstructor$new()

    # Wipe clean
    observeEvent(input$new_test_set, {
      # browser()
      cdm$reset()
      data_version(data_version() + 1)
    })

    ##### Update saved file in sidebar
    current_files <- reactive({
      file_refresh_trigger()
      list_test_set_records()
    })

    # Render list UI
    output$sidebar_file_list <- renderUI({
      files <- current_files()

      tagList(
        lapply(seq_len(nrow(files)), function(i) {
          actionLink(
            inputId = paste0("link_", files$id[[i]]),
            label = files$label[[i]],
            class = "text-reset text-decoration-none d-block mb-1"
          )
        })
      )
    })

    # Handles old and new files
    observe({
      files <- current_files()
      existing <- loaded_listeners()
      if (!nrow(files)) {
        return()
      }

      new_files <- files[!(files$id %in% existing), , drop = FALSE]

      lapply(seq_len(nrow(new_files)), function(i) {
        file_id <- new_files$id[[i]]
        file_path <- new_files$path[[i]]
        id <- paste0("link_", file_id)

        observeEvent(input[[id]], {
          cdm$loadJsonTestSet(file_path)
          data_version(data_version() + 1)
        })
      })

      if (nrow(new_files) > 0) {
        loaded_listeners(c(existing, new_files$id))
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
    patientDesignerChatServer(
      id = "designer_chat",
      reset_trigger = reactive(input$new_chat),
      on_submit = function(prompt) {
        shiny::withProgress(message = "Generating test set", value = 0, {
          incProgress(0.2)
          generator <- get_chat_generator()
          generator$prompt(prompt)
          incProgress(0.5)

          file_stub <- paste0(
            "patient-chat-",
            format(Sys.time(), "%Y%m%d-%H%M%S"),
            "-",
            sprintf("%03d", sample.int(999, 1))
          )
          generator$save(
            name = file_stub,
            path = chat_temp_dir
          )

          out_file <- file.path(chat_temp_dir, paste0(file_stub, ".json"))
          cdm$loadJsonTestSet(out_file)
          data_version(data_version() + 1)
          file_refresh_trigger(file_refresh_trigger() + 1)
          incProgress(0.3)
        })

        paste(
          "Generated a test set from your prompt, saved it in the chat temp directory,",
          "and loaded it into PatientDesigner."
        )
      },
      on_reset = function() {
        chat_generator(NULL)
      }
    )

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

    # After person selection
    # Filters and updates observation/drug exposure fields
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
      cdm$observation_period$data()
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
      cdm$drug_exposure$data()
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
      cdm$condition_occurrence$data()
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
      cdm$measurement$data()
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
      cdm$procedure_occurrence$data()
    })

    # CDM Data Timeline
    cdmDataTimeline <- reactive({
      pid <- suppressWarnings(as.numeric(person_module()))
      req(!is.na(pid), length(pid) == 1)
      # browser()
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
      ignoreInit = FALSE
      )

    # Render cdm table
    output$cdmData <- renderTable({
      req(cdmDataTimeline)
      cdmDataTimeline()
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
      end_date <- if (identical(type, "measurement")) NULL else update_data$end_date
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
