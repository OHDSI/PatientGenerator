normalizeBarEndUpdate <- function(update_data) {
  if (is.null(update_data)) {
    return(update_data)
  }

  needs_end_date <- update_data$type %in% c(
    "condition_occurrence",
    "drug_exposure",
    "procedure_occurrence"
  )

  missing_end_date <- is.null(update_data$end_date) ||
    length(update_data$end_date) == 0 ||
    (is.character(update_data$end_date) &&
      length(update_data$end_date) == 1 &&
      !nzchar(update_data$end_date)) ||
    (length(update_data$end_date) == 1 && is.na(update_data$end_date))

  if (isTRUE(needs_end_date) && isTRUE(missing_end_date)) {
    update_data$end_date <- update_data$start_date
  }

  update_data
}
