library(PatientGenerator)

# path: this directory should have 1 or more json files
path <- NULL
if (exists("shinySettings") && !is.null(shinySettings$path)) {
  path <- shinySettings$path
}

PatientGenerator:::createPatientGeneratorApp(path = path)  