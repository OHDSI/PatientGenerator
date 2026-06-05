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
                            launch.browser = FALSE) {

  if (isTRUE(makePublishable)) {
    publishDir <- preparePublishablePatientDesigner(
      path = path,
      publishDir = publishDir,
      overwritePublishDir = overwritePublishDir
    )
    if (launch.browser) {
      options(shiny.launch.browser = TRUE)
    }
    shiny::runApp(
      appDir = publishDir
    )
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

preparePublishablePatientDesigner <- function(path,
                                              publishDir,
                                              overwritePublishDir) {
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
      "PatientGenerator::patientDesigner(path = path)"
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
