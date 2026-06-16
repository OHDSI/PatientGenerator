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

formatDateColumns <- function(data) {
  date_cols <- names(data)[
    names(data) %in% c("start_date", "end_date") |
      grepl("_date$", names(data))
  ]
  if (length(date_cols) == 0) {
    return(data)
  }

  data <- as.data.frame(data)
  for (date_col in date_cols) {
    data[[date_col]] <- formatDateColumn(data[[date_col]])
  }

  data
}

formatDateColumn <- function(x) {
  if (inherits(x, "Date")) {
    return(as.character(x))
  }

  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(format(as.Date(x, tz = "UTC"), "%Y-%m-%d"))
  }

  if (is.numeric(x)) {
    return(as.character(as.Date(x, origin = "1970-01-01")))
  }

  parsed <- suppressWarnings(as.Date(x))
  if (all(is.na(x) | !is.na(parsed))) {
    return(as.character(parsed))
  }

  as.character(x)
}
