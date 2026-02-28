new_cdm <- function() {
  getFromNamespace("cdmConstructor", "patientGenerator")$new()
}

new_cdm_table <- function(type) {
  getFromNamespace("cdmTable", "patientGenerator")$new(type = type)
}

cdm_table_server <- getFromNamespace("cdmTableServer", "patientGenerator")
