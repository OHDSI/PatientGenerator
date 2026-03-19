cdmTable <- R6::R6Class(
  "cdmTable",
  inherit = cdmConstructor,
  public = list(
  data = NULL,
  initialize = function(type) {
    private$.columnNames <- columnNames(type)
    private$.tableName <- type
    private$.data <- private$.emptyTable()
    self$data = private$.getData
  },
  reset = function() {
    private$.data = private$.emptyTable()
  },
  tableNameId = function() {
    private$.tableNameId()
  },
  tableNameConceptId = function() {
    private$.tableNameConceptId()
  },
  tableNameDate = function(name) {
    private$.tableNameDate(name)
  }
  ),
  private = list(
    .columnNames = NULL,
    .tableName = NULL,
    .data = NULL,
    .tableNameId = function() {
      name_id <- paste(
        private$.tableName,
        "id",
        sep = "_"
        )
      table_name_id <- private$.columnNames[
        ,
        .SD,
        .SDcols = grepl(
          name_id,
          names(private$.columnNames)
          )
        ] |>
        names()
      return(table_name_id)
    },
    .tableNameConceptId = function() {
      # browser()
      if (private$.tableName == "condition_occurrence") {
        table_id <- "condition"
      } else if (private$.tableName == "procedure_occurrence") {
        table_id <- "procedure"
        } else {
        table_id <- private$.tableName
      }
      if (private$.tableName == "drug_exposure") {
        table_concept_id <- "drug_concept_id"
      } else if (private$.tableName == "observation_period") {
        table_concept_id <- "period_type_concept_id"
      } else {
        table_concept_id <- paste(
          table_id,
          "concept_id",
          sep = "_"
        )
      }
      name_concept_id <- private$.columnNames[
        ,
        .SD,
        .SDcols = grepl(
          table_concept_id,
          names(private$.columnNames)
        )
      ] |>
        names()
      return(name_concept_id)
    },
    .tableNameDate = function(name) {
      # browser()
      checkmate::assertCharacter(name)
      if (!name %in% c("start", "end")) {
        stop("'name' should be 'start' or 'end'")
      }
      if (private$.tableName %in% c("condition_occurrence")) {
        table_name <- "condition"
      } else if (private$.tableName == "procedure_occurrence") {
        table_name <- "procedure"
      } else {
        table_name <- private$.tableName
      }
      name_date <- paste(
        table_name,
        name,
        "date",
        sep = "_"
        )
      if (name == "start" & private$.tableName == "measurement") {
        table_name_id <- "measurement_date"
      } else if (name == "start" & private$.tableName == "procedure_occurrence") {
        table_name_id <- "procedure_date"
      } else {
        table_name_id <- private$.columnNames[
          ,
          .SD,
          .SDcols = grepl(
            name_date,
            names(private$.columnNames)
            )
          ] |>
          names()
      }
      return(table_name_id)
    }
    )
  )