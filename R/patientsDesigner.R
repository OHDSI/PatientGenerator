#' `patientDesigner()` is a visual interface based on D3 to construct test datasets for the OMOP-CDM
#'
#' @param path Optional folder containing JSON test sets.
#' If NULL, default path resolution keeps testthat integration.
#' @param makePublishable If TRUE, copy the packaged Shiny application template
#' to `publishDir`, write an `app.R` launcher, and run the app from that folder.
#' @param publishDir Directory to create for the publishable Shiny app.
#' @param overwritePublishDir If TRUE, overwrite files in `publishDir` when it
#' already exists.
#' @param launch.browser Passed to `shiny::runApp()` when `makePublishable` is TRUE.
#' @param includeChat If TRUE, include the chat-driven dataset generator tab.
#' @returns A Shiny app
#' @import r2d3 shiny bslib dplyr
#' @importFrom stats setNames
#' @importFrom utils tail
#' @importFrom data.table as.data.table set rbindlist
#' @export
patientDesigner <- function(path = NULL,
                            makePublishable = FALSE,
                            publishDir = file.path(getwd(), "PatientGeneratorApp"),
                            overwritePublishDir = FALSE,
                            launch.browser = FALSE,
                            includeChat = FALSE) {

  if (isTRUE(makePublishable)) {
    publishDir <- preparePublishablePatientDesigner(
      path = path,
      publishDir = publishDir,
      overwritePublishDir = overwritePublishDir,
      includeChat = includeChat
    )
    if (launch.browser) {
      options(shiny.launch.browser = TRUE)
    }
    shiny::runApp(
      appDir = publishDir
    )
    return(invisible(NULL))
  }

  chatPanel <- div(
    class = "p-3",
    fluidRow(
      column(
        width = 9,
        textAreaInput(
          "chat_prompt",
          "Prompt",
          placeholder = paste(
            "Describe the synthetic OMOP-CDM patients to generate.",
            "Include required tables, patient counts, events, dates,",
            "and concept requirements."
          ),
          height = "720px",
          width = "100%"
        ),
        actionButton(
          "run_patient_chat",
          "Generate Dataset",
          icon = icon("wand-magic-sparkles"),
          class = "btn-primary"
        )
      ),
      column(
        width = 3,
        wellPanel(
          selectizeInput(
            "chat_model",
            "LLM model",
            choices = c("gpt-5.4"),
            selected = "gpt-5.4",
            options = list(
              create = TRUE,
              placeholder = "Load models or type a model id"
            )
          ),
          actionButton(
            "refresh_chat_models",
            "Load available models",
            icon = icon("rotate")
          ),
          hr(),
          textInput(
            "chat_save_name",
            "Filename (no extension):",
            value = "patient-chat-test"
          ),
          actionButton(
            "save_chat_dataset",
            "Save Chat Dataset",
            icon = icon("floppy-disk")
          ),
          actionButton(
            "load_chat_dataset",
            "Load in Designer",
            icon = icon("file-import")
          ),
          hr(),
          h5("Chat status"),
          verbatimTextOutput("chat_status")
        )
      )
    ),
    h5("Generated JSON"),
    tags$div(
      style = "max-height: 320px; overflow-y: auto;",
      verbatimTextOutput("chat_json_preview")
    )
  )

  designerPanel <- tagList(
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
      ),
      tabPanel(
        "Death",
        cdmTableUI(id = "death"),
        value = "death_module"
      ),
      tabPanel(
        "Pregnancy",
        cdmTableUI(id = "pregnancy"),
        value = "pregnancy_module"
      )
    ),
    tabsetPanel(
      id = "review_tabs",
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
        tableOutput("observationTable"),
        tableOutput("deathTable"),
        tableOutput("pregnancyTable")
      )
    )
  )

  designerTab <- tabPanel(
    "Designer",
    personUI(id = "person"),
    tags$style(HTML("
  .well {
    padding: 1rem 1rem .15rem 1rem
  }
  ")),
    designerPanel
  )
  mainTabs <- tabsetPanel(
    id = "main_tabs",
    designerTab,
    if (isTRUE(includeChat)) {
      tabPanel(
        "Chat",
        chatPanel
      )
    }
  )
  mainContent <- if (isTRUE(includeChat)) {
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
      mainTabs,
      border_radius = FALSE,
      fillable = TRUE,
      class = "p-0"
    )
  } else {
    mainTabs
  }

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

      .cdm-input-col .form-label,
      .cdm-input-col .control-label {
        min-height: 2.5rem;
        display: flex;
        align-items: flex-end;
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
          "Download Test Set as JSON",
          icon = icon("download")
          ),
        downloadButton(
          "downloadTestSetXlsx",
          "Download Test Set as XLSX",
          icon = icon("download")
        ),
        position = c("left"),
        open = "open"
        # selectInput("theme", "Bootswatch theme:", bootswatch_themes, selected = "flatly")
      ),
      mainContent,
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
    chat_json <- reactiveVal(NULL)
    chat_status <- reactiveVal("No chat dataset generated yet.")
    
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

    if (isTRUE(includeChat)) {
      observeEvent(input$new_chat, {
        chat_generator(NULL)
        chat_json(NULL)
        chat_status("No chat dataset generated yet.")
        updateTextAreaInput(session, "chat_prompt", value = "")
        updateTabsetPanel(session, "main_tabs", selected = "Chat")
      })
    }

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

    if (isTRUE(includeChat)) {
      output$chat_status <- renderText({
        chat_status()
      })

      output$chat_json_preview <- renderText({
        json <- chat_json()
        if (is.null(json)) {
          return("Generate a dataset to preview JSON here.")
        }
        json
      })

      observeEvent(input$refresh_chat_models, {
        models <- tryCatch(
          availableModels(),
          error = function(e) e
        )

        if (inherits(models, "error")) {
          chat_status(conditionMessage(models))
          showNotification(
            conditionMessage(models),
            type = "error",
            duration = 8
          )
          return(invisible(NULL))
        }

        models <- sort(unique(models))
        if (length(models) == 0) {
          chat_status("No models were returned by the LLM provider.")
          showNotification(
            "No models were returned by the LLM provider.",
            type = "warning",
            duration = 8
          )
          return(invisible(NULL))
        }

        selected_model <- input$chat_model
        if (!selected_model %in% models) {
          selected_model <- models[1]
        }
        updateSelectizeInput(
          session,
          "chat_model",
          choices = models,
          selected = selected_model,
          options = list(
            create = TRUE,
            placeholder = "Load models or type a model id"
          ),
          server = TRUE
        )
        chat_status(glue::glue("Loaded {length(models)} available models."))
      })

      observeEvent(input$run_patient_chat, {
        req(input$chat_model)
        req(input$chat_prompt)
        model <- input$chat_model
        prompt <- input$chat_prompt

        chat_status("Generating dataset...")
        result <- withProgress(
          message = "Generating patient dataset",
          value = 0,
          expr = {
            incProgress(0.2, detail = "Creating chat")
            generator <- tryCatch(
              patientChat$new(model = model, echo = "none"),
              error = function(e) e
            )
            if (inherits(generator, "error")) {
              generator
            } else {
              incProgress(0.6, detail = "Sending prompt")
              prompt_result <- tryCatch(
                generator$prompt(prompt),
                error = function(e) e
              )
              if (inherits(prompt_result, "error")) {
                prompt_result
              } else {
                incProgress(0.2, detail = "Formatting JSON")
                json <- tryCatch(
                  generator$json_response(),
                  error = function(e) e
                )
                if (inherits(json, "error")) {
                  json
                } else {
                  list(generator = generator, json = json)
                }
              }
            }
          }
        )

        if (inherits(result, "error")) {
          chat_status(conditionMessage(result))
          showNotification(
            conditionMessage(result),
            type = "error",
            duration = 8
          )
          return(invisible(NULL))
        }

        chat_generator(result$generator)
        chat_json(result$json)
        chat_status("Dataset generated. Save it or load it into the designer.")
        showNotification(
          "Chat dataset generated.",
          type = "message",
          duration = 5
        )
      })

      observeEvent(input$save_chat_dataset, {
        req(chat_json())
        req(input$chat_save_name)

        name_without_extension <- tools::file_path_sans_ext(input$chat_save_name)
        name <- paste0(name_without_extension, ".json")
        path <- file.path(get_test_dir(), name)
        generator <- chat_generator()
        result <- tryCatch(
          {
            if (is.null(generator)) {
              write(chat_json(), path)
            } else {
              generator$save(
                name = name_without_extension,
                path = get_test_dir()
              )
            }
            TRUE
          },
          error = function(e) e
        )

        if (inherits(result, "error")) {
          chat_status(conditionMessage(result))
          showNotification(
            conditionMessage(result),
            type = "error",
            duration = 8
          )
          return(invisible(NULL))
        }

        file_refresh_trigger(file_refresh_trigger() + 1)
        chat_status(glue::glue("Saved chat dataset to {path}."))
        showNotification(
          glue::glue("Saved {name}."),
          type = "message",
          duration = 5
        )
      })

      observeEvent(input$load_chat_dataset, {
        req(chat_json())
        temp_file <- tempfile(fileext = ".json")
        write(chat_json(), temp_file)
        result <- tryCatch(
          cdm$loadJsonTestSet(temp_file),
          error = function(e) e
        )

        if (inherits(result, "error")) {
          chat_status(conditionMessage(result))
          showNotification(
            conditionMessage(result),
            type = "error",
            duration = 8
          )
          return(invisible(NULL))
        }

        data_version(data_version() + 1)
        chat_status("Loaded chat dataset into the designer.")
        showNotification(
          "Loaded chat dataset into the designer.",
          type = "message",
          duration = 5
        )
      })
    }
    
    
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
        type = "death",
        input_person_id = person_module,
        session = session
        )
      updateTableIdsNs(
        cdm = cdm,
        type = "drug_exposure",
        input_person_id = person_module,
        session = session
      )
      updateTableIdsNs(
        cdm = cdm,
        type = "pregnancy",
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
      observation_period_module$field_update()
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
      drug_exposure_module$field_update()
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
      condition_occurrence_module$field_update()
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
      measurement_module$field_update()
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
      procedure_occurrence_module$field_update()
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
      observation_module$field_update()
      observation_module$elongation_click()
      formatDateColumns(cdm$observation$data())
    })

    # DEATH TABLE
    death_module <- cdmTableServer(
      id = "death",
      cdm = cdm,
      person_id_selected = person_module,
      syncing = syncing
    )

    output$deathTable <- renderTable({
      data_version()
      death_module$add_click()
      death_module$delete_click()
      death_module$elongation_click()
      formatDateColumns(cdm$death$data())
    })

    # PREGNANCY TABLE
    pregnancy_module <- cdmTableServer(
      id = "pregnancy",
      cdm = cdm,
      person_id_selected = person_module,
      syncing = syncing
    )

    output$pregnancyTable <- renderTable({
      data_version()
      pregnancy_module$add_click()
      pregnancy_module$delete_click()
      pregnancy_module$field_update()
      pregnancy_module$elongation_click()
      formatDateColumns(cdm$pregnancy$data())
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
      observation_period_module$field_update(),
      observation_period_module$elongation_click(),
      drug_exposure_module$add_click(),
      drug_exposure_module$delete_click(),
      drug_exposure_module$field_update(),
      drug_exposure_module$elongation_click(),
      condition_occurrence_module$add_click(),
      condition_occurrence_module$delete_click(),
      condition_occurrence_module$field_update(),
      condition_occurrence_module$elongation_click(),
      measurement_module$add_click(),
      measurement_module$delete_click(),
      measurement_module$field_update(),
      measurement_module$elongation_click(),
      procedure_occurrence_module$add_click(),
      procedure_occurrence_module$delete_click(),
      procedure_occurrence_module$field_update(),
      procedure_occurrence_module$elongation_click(),
      observation_module$add_click(),
      observation_module$delete_click(),
      observation_module$field_update(),
      observation_module$elongation_click(),
      death_module$add_click(),
      death_module$delete_click(),
      death_module$elongation_click(),
      pregnancy_module$add_click(),
      pregnancy_module$delete_click(),
      pregnancy_module$field_update(),
      pregnancy_module$elongation_click(),
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

    output$downloadTestSetXlsx <- downloadHandler(
      filename = function() {
        paste("patientDesigner", ".xlsx", sep = "")
      },
      content = function(file) {
        cdm$writeCdmDataXlsx(file)
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

preparePublishablePatientDesigner <- function(path,
                                              publishDir,
                                              overwritePublishDir,
                                              includeChat = FALSE) {
  templateDir <- system.file("shiny", package = "PatientGenerator")
  if (!nzchar(templateDir) || !dir.exists(templateDir)) {
    stop("Packaged Shiny template directory not found.", call. = FALSE)
  }

  publishDir <- normalizePath(publishDir, mustWork = FALSE)
  if (publishDir == normalizePath(getwd(), mustWork = TRUE)) {
    stop("Publish directory must be a new folder, not the current directory.", call. = FALSE)
  }

  if (dir.exists(publishDir) && !isTRUE(overwritePublishDir)) {
    stop(
      "Publish directory already exists: ", publishDir, "\n",
      "Use overwritePublishDir = TRUE or choose a different publishDir.",
      call. = FALSE
    )
  }

  if (!dir.exists(publishDir)) {
    dir.create(publishDir, recursive = TRUE, showWarnings = FALSE)
  }
  publishDir <- normalizePath(publishDir, mustWork = TRUE)

  templateFiles <- list.files(
    templateDir,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  copied <- file.copy(
    from = templateFiles,
    to = publishDir,
    recursive = TRUE,
    overwrite = TRUE,
    copy.date = TRUE
  )
  if (!all(copied)) {
    stop("Failed to copy all files from the packaged Shiny template.", call. = FALSE)
  }

  appPath <- preparePublishablePath(path, publishDir)
  writeLines(
    c(
      "library(PatientGenerator)",
      "",
      paste0("path <- ", deparse(appPath, width.cutoff = 500)),
      "",
      paste0(
        "PatientGenerator::patientDesigner(path = path, includeChat = ",
        if (isTRUE(includeChat)) "TRUE" else "FALSE",
        ")"
      )
    ),
    con = file.path(publishDir, "app.R"),
    useBytes = TRUE
  )

  publishDir
}

preparePublishablePath <- function(path, publishDir) {
  if (is.null(path)) {
    return(NULL)
  }

  checkmate::assertCharacter(path, len = 1, any.missing = FALSE)
  if (!dir.exists(path)) {
    return(path)
  }

  sourcePath <- normalizePath(path, mustWork = TRUE)
  targetName <- basename(sourcePath)
  targetPath <- file.path(publishDir, targetName)

  if (normalizePath(dirname(sourcePath), mustWork = TRUE) !=
      normalizePath(publishDir, mustWork = FALSE)) {
    dir.create(targetPath, recursive = TRUE, showWarnings = FALSE)
    dataFiles <- list.files(
      sourcePath,
      all.files = TRUE,
      no.. = TRUE,
      full.names = TRUE
    )
    if (length(dataFiles) > 0) {
      copied <- file.copy(
        from = dataFiles,
        to = targetPath,
        recursive = TRUE,
        overwrite = TRUE,
        copy.date = TRUE
      )
      if (!all(copied)) {
        stop("Failed to copy all files from the test set directory.", call. = FALSE)
      }
    }
  }

  targetName
}
