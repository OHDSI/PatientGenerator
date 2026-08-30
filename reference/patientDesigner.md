# `patientDesigner()` is a visual interface based on D3 to construct test datasets for the OMOP-CDM

`patientDesigner()` is a visual interface based on D3 to construct test
datasets for the OMOP-CDM

## Usage

``` r
patientDesigner(
  path = NULL,
  makePublishable = FALSE,
  publishDir = file.path(getwd(), "PatientGeneratorApp"),
  overwritePublishDir = FALSE,
  launch.browser = FALSE,
  includeChat = FALSE,
  visibleTables = NULL
)
```

## Arguments

- path:

  Optional folder containing JSON test sets. If NULL, default path
  resolution keeps testthat integration.

- makePublishable:

  If TRUE, copy the packaged Shiny application template to `publishDir`,
  write an `app.R` launcher, and run the app from that folder.

- publishDir:

  Directory to create for the publishable Shiny app.

- overwritePublishDir:

  If TRUE, overwrite files in `publishDir` when it already exists.

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) when
  `makePublishable` is TRUE.

- includeChat:

  If TRUE, include the chat-driven dataset generator tab.

- visibleTables:

  Optional character vector of event tables to show when the Designer
  opens. `NULL` shows all supported event tables.

## Value

A Shiny app
