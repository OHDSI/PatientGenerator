cdmTable <- R6::R6Class("cdmTable",
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
                        }
                        ),
                        private = list(
                          .columnNames = NULL,
                          .tableName = NULL,
                          .data = NULL,
                          .tableNameId = function() {
                            name_id <- paste(private$.tableName, "id", sep = "_")
                            table_name_id <- private$.columnNames[ , .SD, .SDcols = grepl(name_id, names(private$.columnNames))] |>
                              names()
                            return(table_name_id)
                          },
                          .tableNameDate = function(name) {
                            checkmate::assertCharacter(name)
                            if (!name %in% c("start", "end")) {
                              stop("'name' should be 'start' or 'end'")
                            }
                            if (private$.tableName %in% c("condition_occurrence")) {
                              table_name <- "condition"
                            } else {
                              table_name <- private$.tableName
                            }
                            name_date <- paste(table_name, name, "date", sep = "_")
                            table_name_id <- private$.columnNames[ , .SD, .SDcols = grepl(name_date, names(private$.columnNames))] |>
                              names()
                            return(table_name_id)
                          }

                          )
                        )
