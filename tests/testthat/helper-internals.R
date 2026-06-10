new_cdm <- function() {
  getFromNamespace("cdmConstructor", "PatientGenerator")$new()
}

new_cdm_table <- function(type) {
  getFromNamespace("cdmTable", "PatientGenerator")$new(type = type)
}

cdm_table_server <- getFromNamespace("cdmTableServer", "PatientGenerator")
normalize_bar_end_update <- getFromNamespace("normalizeBarEndUpdate", "PatientGenerator")
hecate_concept_label <- getFromNamespace("hecateConceptLabel", "PatientGenerator")
