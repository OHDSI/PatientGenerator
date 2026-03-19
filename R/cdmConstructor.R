cdmConstructor <- R6::R6Class(
  "cdmConstructor",
  public = list(
    tables = NULL,
    person = NULL,
    observation_period = NULL,
    drug_exposure = NULL,
    condition_occurrence = NULL,
    measurement = NULL,
    initialize = function(tables = c(
      "observation_period",
      "drug_exposure",
      "condition_occurrence",
      "measurement"
      )
    ) {
      self$tables <- tables
      self$person <- personTable$new()
      for (i in seq_along(tables)) {
        self[[tables[i]]] <- cdmTable$new(tables[i])
      }
    },
    add = function(person_id) {
      checkmate::assertInteger(person_id)
      name_event_id <- paste(
        private$.tableName,
        "id",
        sep = "_"
        )
      event_id <-  if (
        length(
          private$.data[[name_event_id]]
          ) == 0) {
        1L
      } else {
          private$.data[[name_event_id]][length(
            private$.data[[name_event_id]]
            )] |> 
          as.integer() + 
          1L
      }
      # browser()
      new_person_data <- private$.defaultPersonData()
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
          )
        )
      },
    update = function(event_id, ...) {
      new_data <- list(...)
      if (!all(names(new_data) %in% names(private$.columnNames))) {
        stop(
          glue::glue(
            "Error: one column from c(
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
      # browser()
      # cdm_schema <- jsonvalidate::json_validator(system.file("jsonSchemas",
      #                                                        "cdm54schema-complete.json",
      #                                                        package = "TestGenerator"),
      #                                            engine = "ajv")
      # checkmate::assertFileExists(path)
      jsonData <- jsonlite::fromJSON(path)
      # if (!cdm_schema(toJSON(jsonData, auto_unbox = TRUE))) {
      #   stop("Invalid data structure!")
      # }
      currentTables <- names(jsonData)
      # Check for the expected columns in the CDM
      for (tableName in currentTables) {
        if (tableName %in% c("person",
                             "observation_period",
                             "condition_occurrence",
                             "drug_exposure",
                             "measurement")) {
          classTable <- class(jsonData[[tableName]])
          table_data <- jsonData[[tableName]] |> as.data.table()
          if (classTable == "data.frame") {
              date_cols <- names(table_data)[grepl("_date$", names(table_data))]
              if (length(date_cols) > 0 ) {
                table_data[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]
              }
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
        if (!is.null(start_date)) {
          data.table::set(
            private$.data,
            i = index_table,
            j = name_start_date,
            value = start_date
            )
        }
        if (!is.null(end_date)) {
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
        condition_occurrence = self$condition_occurrence$data())

        cdm_data_json <- jsonlite::toJSON(
          cdm_data,
          dataframe = "rows",
          pretty = TRUE,
          na = "null")
        
        return(cdm_data_json)
        
        },
    # Export data to json
    getCdmDataTimeline = function() {
      if (self$person$data() |> length() > 0) {
        # browser()
        tables_timeline <- setNames(
          lapply(
            self$tables, 
            function(table_name) {
              # browser()
              # print(table_name)
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
        
        return(data_timeline)
        
        
                
        # data_timeline <- lapply(
        #   self$tables, 
        #   function(table_name) {
        #     browser()
        #     name_id <- self[[table_name]]$tableNameId()
        #     name_start_date <- self[[table_name]]$tableNameDate("start")
        #     name_end_date <- self[[table_name]]$tableNameDate("end")
        #     self[[table_name]]$data() 
        #     # |> 
        #     #   select(
        #     #     observation_period_id,
        #     #     concept_id,
        #     #     person_id,
        #     #     observation_period_start_date,
        #     #     observation_period_end_date,
        #     #     type
        #     #   ) 
        #       
        #   }
        # )
        

        # browser()
        # observation_period_table <- self$observation_period$data() %>%
        #   mutate(
        #     type = "observation_period",
        #     concept_id = as.integer(0),
        #     observation_period_start_date = as.Date(
        #       observation_period_start_date
        #       ),
        #     observation_period_end_date = as.Date(
        #       observation_period_end_date
        #       )
        #     ) %>%
        #   select(
        #     observation_period_id,
        #     concept_id,
        #     person_id,
        #     observation_period_start_date,
        #     observation_period_end_date,
        #     type
        #     ) %>%
        #   rename(
        #     event_id = observation_period_id,
        #     start_date = observation_period_start_date,
        #     end_date = observation_period_end_date
        #     )
        # 
        # condition_exposure_table <- self$condition_occurrence$data() %>%
        #   mutate(
        #     type = "condition_occurrence",
        #     condition_concept_id = as.integer(
        #       condition_concept_id
        #       ),
        #     condition_start_date = as.Date(
        #       condition_start_date
        #       ),
        #     condition_end_date = as.Date(
        #       condition_end_date
        #       )
        #     ) %>%
        #   select(
        #     condition_occurrence_id,
        #     condition_concept_id,
        #     person_id,
        #     condition_start_date,
        #     condition_end_date,
        #     type
        #     ) %>%
        #   rename(
        #     event_id = condition_occurrence_id,
        #     concept_id = condition_concept_id,
        #     start_date = condition_start_date,
        #     end_date = condition_end_date
        #     )
        # 
        # drug_exposure_table <- self$drug_exposure$data() %>%
        #   mutate(
        #     type = "drug_exposure",
        #     drug_concept_id = as.integer(
        #     drug_concept_id
        #     ),
        #     drug_exposure_start_date = as.Date(
        #     drug_exposure_start_date
        #     ),
        #     drug_exposure_end_date = as.Date(
        #       drug_exposure_end_date
        #       )
        #     ) %>%
        #   select(
        #     drug_exposure_id,
        #     drug_concept_id,
        #     person_id,
        #     drug_exposure_start_date,
        #     drug_exposure_end_date,
        #     type
        #     ) %>%
        #   rename(
        #     event_id = drug_exposure_id,
        #     concept_id = drug_concept_id,
        #     start_date = drug_exposure_start_date,
        #     end_date = drug_exposure_end_date
        #     )
        # 
        # cdmData <- dplyr::bind_rows(
        #   observation_period_table,
        #   condition_exposure_table,
        #   drug_exposure_table
        #   ) %>%
        #   mutate(
        #     categories = row_number()
        #     )
        } else {
          cdmData <- data.table::data.table()
          }
      return(cdmData)
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
