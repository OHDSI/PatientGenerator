personTable <- R6::R6Class("personTable",
                           inherit = cdmConstructor,
                           public = list(
                             data = NULL,
                             initialize = function() {
                               private$.columnNames <- columnNames("person")
                               private$.data <- private$.emptyTable()
                               self$data = private$.getData
                             },
                             add = function(...) {
                               person_id <- if (length(private$.data$person_id) == 0) {1L} else {private$.data$person_id[length(private$.data$person_id)] + 1L}
                               new_person_row <- private$.constructNewRow(person_id, ...)
                               private$.data <- rbindlist(list(private$.data, new_person_row))
                             },
                             update = function(person_id, ...) {
                               new_data <- list(...)
                               if (!all(names(new_data) %in% names(private$.columnNames))) {
                                 stop(glue::glue("Error: one column from c({glue::glue_collapse(names(new_data), sep = ', ')}) are not from the {private$.tableName} table"))
                               }
                               index_table <- which(private$.data[["person_id"]] == person_id)
                               if (length(index_table) > 0) {
                                 for (col_name in names(new_data)) {
                                   data.table::set(private$.data,
                                                   i = index_table,
                                                   j = col_name,
                                                   value = new_data[[col_name]])
                                 }
                               } else {
                                 warning(glue::glue("person_id not found in table"))
                               }
                             },
                             delete = function(person_id) {
                               index_table <- which(private$.data[["person_id"]] == person_id |> as.integer())
                               private$.data <- private$.data[-index_table]
                             },
                             reset = function() {
                               private$.data = private$.emptyTable()
                             }
                             ),
                           private = list(
                             .columnNames = NULL,
                             .data = NULL,
                             .constructNewRow = function(person_id, ...) {
                               checkmate::assertInteger(person_id)
                               new_data <- list(person_id = person_id, ...)
                               empty_row <- private$.emptyTable()
                               new_row <- rbindlist(list(empty_row, new_data), fill = TRUE)
                               return(new_row)
                             }
                             )
                           )
