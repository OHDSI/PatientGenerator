cdmConstructor <- R6::R6Class(
  "cdmConstructor",
  public = list(
    tables = NULL,
    person = NULL,
    observation_period = NULL,
    drug_exposure = NULL,
    condition_occurrence = NULL,
    measurement = NULL,
    procedure_occurrence = NULL,
    observation = NULL,
    initialize = function(tables = c(
      "observation_period",
      "condition_occurrence",
      "drug_exposure",
      "measurement",
      "procedure_occurrence",
      "observation"
      )
    ) {
      self$tables <- tables
      self$person <- personTable$new()
      for (i in seq_along(tables)) {
        self[[tables[i]]] <- cdmTable$new(tables[i])
      }
    },
    add = function(person_id, ...) {
      checkmate::assertInteger(person_id)
      input_data <- list(...)
      input_data <- input_data[!vapply(input_data, is.null, logical(1))]
      if (!all(names(input_data) %in% names(private$.columnNames))) {
        stop(
          glue::glue(
            "Error: one or more column(s) from c(
              {glue::glue_collapse(
                names(
                  input_data
                  ),
                sep = ', '
                )
              }
            ) are not from the {private$.tableName} table"
            )
          )
      }
      name_event_id <- paste(
        private$.tableName,
        "id",
        sep = "_"
        )
      event_id <-  if (
        length(private$.data[[name_event_id]]) == 0) {
        1L
      } else {
          private$.data[[name_event_id]][length(
            private$.data[[name_event_id]]
            )] |> 
          as.integer() + 
          1L
      }
      new_person_data <- private$.defaultPersonData()
      if (length(input_data) > 0) {
        input_data <- Map(function(col_name, value) {
          if (length(value) == 0 || all(is.na(value))) {
            return(NULL)
          }
          if (stringr::str_detect(col_name, "date")) {
            as.Date(value)
          } else if (stringr::str_detect(col_name, "concept") & !stringr::str_detect(col_name, "gender")) {
            as.integer(value)
          } else {
            as.character(value)
          }
        }, names(input_data), input_data)
        input_data <- input_data[!vapply(input_data, is.null, logical(1))]
        new_person_data[names(input_data)] <- input_data
      }
      new_row <- private$.constructNewRow(
        name_event_id,
        event_id, c(
          person_id = person_id,
          new_person_data
          )
        )
      private$.data <- rbindlist(
        list(
          private$.data,
          new_row
          ),
        fill = TRUE
        )
      },
    update = function(event_id, ...) {
      new_data <- list(...)
      if (!all(names(new_data) %in% names(private$.columnNames))) {
        stop(
          glue::glue(
            "Error: one or more column(s) from c(
              {glue::glue_collapse(
                names(
                  new_data
                  ),
                sep = ', '
                )
              }
            ) are not from the {private$.tableName} table"
            )
          )
      }
      name_id <- paste(
        private$.tableName,
        "id",
        sep = "_"
        )
      index_table <- which(private$.data[[name_id]] == event_id)
      if (length(index_table) > 0) {
        for (col_name in names(new_data)) {
          if (stringr::str_detect(col_name, "date")) {
            new_value <- new_data[[col_name]] |> as.Date()
          } else if (stringr::str_detect(col_name, "concept") & !stringr::str_detect(col_name, "gender")) {
            new_value <- new_data[[col_name]] |> as.integer()
          } else {
            new_value <- new_data[[col_name]] |> as.character()
          }
          data.table::set(
            private$.data,
            i = index_table,
            j = col_name,
            value = new_value
            )
        }
      } else {
        warning(glue::glue("{name_id} not found in table"))
      }
      },
    extractRow = function(event_id) {
      name_event_id <- paste(
        private$.tableName,
        "id",
        sep = "_"
        )
      private$.data[private$.data[[name_event_id]] == event_id, ]
    },
    load = function(jsonData) {
      private$.data <- jsonData
    },
    # Add data from an xlsx test set. Each worksheet should represent one CDM
    # table. The app loads the CDM tables it supports and ignores other sheets.
    loadXlsxTestSet = function(path) {
      checkmate::assertFileExists(path)
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("The readxl package is required to upload xlsx test data.")
      }

      sheets <- readxl::excel_sheets(path)
      supported_tables <- c("person", self$tables)
      supported_sheets <- intersect(sheets, supported_tables)

      if (!"person" %in% sheets) {
        stop("Invalid xlsx file: the workbook must contain a 'person' sheet.")
      }
      if (!"person" %in% supported_sheets) {
        stop("Invalid xlsx file: the workbook must contain a supported 'person' sheet.")
      }

      loaded_tables <- character(0)
      imported_tables <- list()

      for (tableName in supported_sheets) {
        table_data <- readxl::read_excel(
          path,
          sheet = tableName,
          .name_repair = "minimal"
          ) |>
          data.table::as.data.table()

        table_data <- private$.validateImportedTable(
          tableName = tableName,
          table_data = table_data
          )

        imported_tables[[tableName]] <- table_data
        loaded_tables <- c(loaded_tables, tableName)
      }

      self$reset()
      for (tableName in names(imported_tables)) {
        self[[tableName]]$load(imported_tables[[tableName]])
      }

      ignored_tables <- setdiff(sheets, supported_tables)
      invisible(list(
        loaded = loaded_tables,
        ignored = ignored_tables
        ))
    },
    delete = function(event_id) {
      name_id <- paste(
        private$.tableName,
        "id",
        sep = "_"
        )
      index_table <- which(private$.data[[name_id]] == event_id)
      private$.data <- private$.data[-index_table]
    },
    # Add data from json test set
    loadJsonTestSet = function(path) {
      self$reset()
      # cdm_schema <- jsonvalidate::json_validator(
      #   system.file("jsonSchemas",
      #               "cdm54schema-complete.json",
      #               package = "TestGenerator"),
      #   engine = "ajv"
      #   )
      checkmate::assertFileExists(path)
      jsonData <- jsonlite::fromJSON(path)
      # if (!cdm_schema(toJSON(jsonData, auto_unbox = TRUE))) {
      #   stop("Invalid data structure!")
      # }
      fillMissingEndDates <- function(tableName, table_data) {
        if (tableName == "condition_occurrence") {
          start_col <- "condition_start_date"
          end_col <- "condition_end_date"
        } else if (tableName == "drug_exposure") {
          start_col <- "drug_exposure_start_date"
          end_col <- "drug_exposure_end_date"
        } else if (tableName == "procedure_occurrence") {
          start_col <- "procedure_date"
          end_col <- "procedure_end_date"
        } else {
          return(table_data)
        }

        if (!start_col %in% names(table_data)) {
          return(table_data)
        }

        if (!end_col %in% names(table_data)) {
          table_data[, (end_col) := get(start_col)]
          return(table_data)
        }

        missing_end_date <- is.na(table_data[[end_col]])
        if (any(missing_end_date)) {
          table_data[missing_end_date, (end_col) := table_data[[start_col]][missing_end_date]]
        }

        table_data
      }
      currentTables <- names(jsonData)
      # Check for the expected columns in the CDM
      for (tableName in currentTables) {
        if (tableName %in% c("person",
                             "observation_period",
                             "condition_occurrence",
                             "drug_exposure",
                             "measurement",
                             "procedure_occurrence",
                             "observation")) {
          classTable <- class(jsonData[[tableName]])
          table_data <- jsonData[[tableName]] |> as.data.table()
          if (classTable == "data.frame") {
              date_cols <- names(table_data)[grepl("_date$", names(table_data))]
              if (length(date_cols) > 0 ) {
                table_data[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]
              }
            table_data <- fillMissingEndDates(tableName, table_data)
            self[[tableName]]$load(data.table::rbindlist(list(private$.data, table_data)))
          }
        }
      }
    },
    # Reset
    reset = function() {
      self$person$reset()
      self$observation_period$reset()
      self$drug_exposure$reset()
      self$condition_occurrence$reset()
      self$measurement$reset()
      self$procedure_occurrence$reset()
      self$observation$reset()
    },

    # Delete person or event
    deletePersonEvent = function(event_id, type) {
      table_data <- glue::glue("{type}_data")
      table_column <- glue::glue("{type}_id")
      data_table <- self[[table_data]]
      self[[table_data]] <- data_table[data_table[[table_column]] != event_id, ]
    },

    # Update Dates
    updateDates = function(person_id,
                           event_id,
                           start_date,
                           end_date) {
      start_date <- as.Date(start_date)
      end_date <- as.Date(end_date)
      name_id <- private$.tableNameId()
      name_start_date <- private$.tableNameDate("start")
      name_end_date <- private$.tableNameDate("end")
      index_table <- which(private$.data[[name_id]] == event_id)
      if (length(index_table) > 0) {
        if (length(start_date) > 0) {
          data.table::set(
            private$.data,
            i = index_table,
            j = name_start_date,
            value = start_date
            )
        }
        if (length(name_end_date) > 0 && length(end_date) > 0) {
          data.table::set(
            private$.data,
            i = index_table,
            j = name_end_date,
            value = end_date)
        }
      } else {
        warning(
          glue::glue(
            "{name_id} not found in table"
            )
          )
      }
      },
    
    # Export data to json
    getCdmData = function() {
      cdm_data <- list(
        person = self$person$data(),
        observation_period = self$observation_period$data(),
        drug_exposure = self$drug_exposure$data(),
        condition_occurrence = self$condition_occurrence$data(),
        measurement = self$measurement$data(),
        procedure_occurrence = self$procedure_occurrence$data(),
        observation = self$observation$data()
        )

        cdm_data_json <- jsonlite::toJSON(
          cdm_data,
          dataframe = "rows",
          pretty = TRUE,
          na = "null")
        
        return(cdm_data_json)
        
        },
    # Export data to xlsx
    writeCdmDataXlsx = function(path) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("The openxlsx package is required to download xlsx test data.")
      }

      cdm_data <- list(
        person = self$person$data()
        )
      cdm_data <- c(
        cdm_data,
        stats::setNames(
          lapply(self$tables, function(table_name) self[[table_name]]$data()),
          self$tables
          )
        )

      workbook <- openxlsx::createWorkbook()
      for (table_name in names(cdm_data)) {
        openxlsx::addWorksheet(workbook, table_name)
        openxlsx::writeData(
          workbook,
          sheet = table_name,
          x = as.data.frame(cdm_data[[table_name]]),
          colNames = TRUE
          )
      }
      openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
      invisible(path)
      },
    # Export data to json
    getCdmDataTimeline = function() {
      if (self$person$data() |> length() > 0) {
        tables_timeline <- setNames(
          lapply(
            self$tables, 
            function(table_name) {
              cols <- c(
                self[[table_name]]$tableNameId(),
                "person_id",
                self[[table_name]]$tableNameConceptId(),
                self[[table_name]]$tableNameDate("start"),
                self[[table_name]]$tableNameDate("end")
              )
              table_data <- self[[table_name]]$data() 
              dt_timeline <- data.table::copy(
                table_data[, ..cols]
                ) 
              if (length(cols) == 5) {
                data.table::setnames(
                  dt_timeline,
                  old = cols,
                  new = c(
                    "event_id",
                    "person_id",
                    "concept_id",
                    "start_date",
                    "end_date"
                  )
                )
              } else {
                data.table::setnames(
                  dt_timeline,
                  old = cols,
                  new = c(
                    "event_id",
                    "person_id",
                    "concept_id",
                    "start_date"
                  )
                )
              }
            }
          ),
          basename(self$tables)
        )
        
        data_timeline <- rbindlist(
          tables_timeline,
          fill = TRUE,
          idcol = "type"
        ) |> 
          mutate(categories = row_number())
        
        } else {
          data_timeline <- data.table::data.table()
          }
      return(data_timeline)
      },
    # Export data to json
    getDrugExposureData = function() {
      drug_exposure_json <- jsonlite::toJSON(
        self$drug_exposure_data,
        dataframe = "rows",
        null = "null",
        na = "null",
        auto_unbox = TRUE,
        digits = getOption(
          "shiny.json.digits",
          16
          ),
        use_signif = TRUE,
        force = TRUE,
        POSIXt = "ISO8601",
        UTC = TRUE,
        rownames = FALSE,
        keep_vec_names = TRUE,
        json_verabitm = TRUE
        )
      return(drug_exposure_json)
      }
    ),
  private = list(
    .fillMissingEndDates = function(tableName, table_data) {
      if (tableName == "condition_occurrence") {
        start_col <- "condition_start_date"
        end_col <- "condition_end_date"
      } else if (tableName == "drug_exposure") {
        start_col <- "drug_exposure_start_date"
        end_col <- "drug_exposure_end_date"
      } else if (tableName == "procedure_occurrence") {
        start_col <- "procedure_date"
        end_col <- "procedure_end_date"
      } else {
        return(table_data)
      }

      if (!start_col %in% names(table_data)) {
        return(table_data)
      }

      if (!end_col %in% names(table_data)) {
        table_data[, (end_col) := get(start_col)]
        return(table_data)
      }

      missing_end_date <- is.na(table_data[[end_col]])
      if (any(missing_end_date)) {
        table_data[
          missing_end_date,
          (end_col) := table_data[[start_col]][missing_end_date]
          ]
      }

      table_data
    },
    .convertDateColumns = function(table_data) {
      date_cols <- names(table_data)[grepl("_date$", names(table_data))]
      if (length(date_cols) == 0) {
        return(table_data)
      }

      for (date_col in date_cols) {
        value <- table_data[[date_col]]
        if (inherits(value, "Date")) {
          converted_value <- value
        } else if (inherits(value, "POSIXt")) {
          converted_value <- as.Date(value)
        } else if (is.numeric(value)) {
          converted_value <- as.Date(value, origin = "1899-12-30")
        } else {
          converted_value <- suppressWarnings(as.Date(value))
        }
        data.table::set(
          table_data,
          j = date_col,
          value = converted_value
          )
      }

      table_data
    },
    .validateImportedTable = function(tableName, table_data) {
      table_data <- data.table::as.data.table(table_data)
      expected_columns <- names(columnNames(tableName))
      unknown_columns <- setdiff(names(table_data), expected_columns)

      if (length(unknown_columns) > 0) {
        stop(glue::glue(
          "Invalid xlsx file: sheet '{tableName}' contains unsupported columns: {glue::glue_collapse(unknown_columns, sep = ', ')}."
          ))
      }
      if (tableName == "person" && !"person_id" %in% names(table_data)) {
        stop("Invalid xlsx file: sheet 'person' must contain a 'person_id' column.")
      }
      if (
        tableName == "person" &&
        (nrow(table_data) == 0 || all(is.na(table_data$person_id)))
      ) {
        stop("Invalid xlsx file: sheet 'person' must contain at least one person.")
      }

      table_data <- private$.convertDateColumns(table_data)
      table_data <- private$.fillMissingEndDates(tableName, table_data)
      data.table::rbindlist(
        list(columnNames(tableName), table_data),
        fill = TRUE
        )
    },
    .getData = function() {
      return(private$.data)
      },
    .emptyTable = function() {
      private$.columnNames
      },
    .defaultPersonData = function() {
      column_names <- private$.columnNames |>
        names() |>
        tail(-2) |>
        head(3)

      if (private$.tableName == "observation_period") {
        values <- list(
          as.Date("2010-02-28"),
          as.Date("2015-02-28"),
          44191562L
          )
        } else if (private$.tableName %in% c("measurement", "observation")) {
          # this table has no end date
          column_names <- column_names |> head(2)
          values <- list(
            44191562L,
            as.Date("2010-02-28"))
        } else {
          values <- list(
            44191562L,
            as.Date("2010-02-28"),
            as.Date("2015-02-28")
            )
          }
      person_data <- Map(function(column_name, value) {
        value
        },
        column_names,
        values
        )
      return(person_data)
      },
    .constructNewRow = function(name_event_id, event_id, ...) {
      checkmate::assertCharacter(name_event_id)
      checkmate::assertInteger(event_id)
      new_data <- c(
        setNames(
          list(event_id),
          name_event_id),
        ...
        )
      empty_row <- private$.emptyTable()
      new_row <- rbindlist(
        list(
          empty_row,
          new_data
          ),
        fill = TRUE
        )
      return(new_row)
      }
    )
)
