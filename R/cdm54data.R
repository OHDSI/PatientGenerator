choicesList <- function(tableName) {
  choicesList <- list(
    person = list(
      gender_concept_id = list(
        "Male" = 8507,
        "Female" = 8532
        ),
      year_of_birth = 1900:as.integer(format(Sys.Date(), "%Y")),
      month_of_birth = 1:12,
      day_of_birth = 1:30
    )
  )
  return(choicesList[[tableName]])
}

supportedCdmTables <- function(includePerson = FALSE) {
  tables <- c(
    "observation_period",
    "condition_occurrence",
    "drug_exposure",
    "measurement",
    "procedure_occurrence",
    "observation",
    "death",
    "pregnancy"
  )

  if (isTRUE(includePerson)) {
    tables <- c("person", tables)
  }

  tables
}

pregnancyColumnNames <- function() {
  data.table::data.table(
    person_id = integer(),
    pregnancy_id = integer(),
    pregnancy_start_date = as.Date(character()),
    pregnancy_end_date = as.Date(character()),
    gestational_length_in_day = integer(),
    pregnancy_outcome = integer(),
    pregnancy_mode_delivery = integer(),
    pregnancy_single = integer(),
    prev_pregnancy_gravidity = integer()
  )
}

#' Read an RDS file.
#' @param file Path to the RDS file.
#' @return A data frame.
#' @noRd
read_rds_file <- function(file) {
  readRDS(file)
}

columnNames <- function(
    name = NULL,
    limit = NULL,
    ommitTime = TRUE,
    cdmVersion = "5.4"
    ) {

  cdmSpecificationPath <- system.file(
    "cdmTableSpecifications",
    paste0(
      "emptycdm_",
      cdmVersion
      ),
    package = "PatientGenerator"
    )

  # supported_tables <- file.path(cdmSpecificationPath) |> 
  #    list.files() |> 
  #   stringr::str_remove(".rds")
  
  supported_tables <- supportedCdmTables(includePerson = TRUE)

  if (!is.null(name)) {

    if (length(name) != 1) {
      stop("Error: Variable 'name' must have exactly one element.")
    }

    checkmate::assertString(name)

    if (!name %in% supported_tables) {
      stop("Error: Variable 'name' should be a cdm table")
    }

    table_data <- if (identical(name, "pregnancy")) {
      pregnancyColumnNames()
    } else {
      file <- file.path(
        cdmSpecificationPath,
        paste0(
          name,
          ".rds"
          )
        )
      read_rds_file(file) |>
        data.table::as.data.table()
    }
    if (isTRUE(ommitTime)) {
      table_data <- table_data[
        ,
        .SD,
        .SDcols = !grepl(
          "time",
          names(table_data)
          )
        ]
    }
    if (!is.null(limit)) {
      table_data <- table_data[, 1:limit]
    }
    return(table_data)

  } else {

    path <- file.path(cdmSpecificationPath) |> 
      list.files(
        full.names = TRUE
        )
    cdm_tables <- list()

    for(table_name in supported_tables){
      table_data <- if (identical(table_name, "pregnancy")) {
        pregnancyColumnNames()
      } else {
        file <- file.path(
          cdmSpecificationPath,
          paste0(
            table_name,
            ".rds"
            )
          )
        read_rds_file(file) |>
          data.table::as.data.table()
      }
      if (isTRUE(ommitTime)) {
        table_data <- table_data[
          ,
          .SD,
          .SDcols = !grepl(
            "time",
            names(table_data)
            )
          ]
      }
      if (!is.null(limit)) {
        table_data <- table_data[, 1:limit]
      }
      cdm_tables[[table_name]] <- table_data
    }
    return(cdm_tables)
  }
}
