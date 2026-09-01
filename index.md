# PatientGenerator

`PatientGenerator` facilitates the creation of synthetic test datasets
for the OMOP Common Data Model (CDM) using two complementary approaches:

- **`patientChat`**: Generates structured patient JSON files using Large
  Language Models (LLMs).
- **`patientDesigner`**: Provides a D3-based Shiny interface for
  reviewing and editing CDM test sets.

The package also includes support for Hecate-powered concept lookups to
ensure valid OMOP concept codes.

### Installation

``` r

# From CRAN
install.packages("PatientGenerator")

# install.packages("remotes")
remotes::install_github("OHDSI/PatientGenerator")
```

### Workflow Overview

1.  **Generate** an initial synthetic cohort using `patientChat`.
2.  **Save** JSON test sets to the local filesystem.
3.  **Refine** patients using
    [`patientDesigner()`](https://mi-erasmusmc.github.io/PatientGenerator/reference/patientDesigner.md).
    - Utilize built-in concept search (powered by `hecateSearch`) during
      table editing.

### Synthetic Patient Generation with `patientChat`

Set an `OPENAI_API_KEY` environment variable (e.g., via
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html))
to enable LLM access.

Available models can be listed using
[`PatientGenerator::availableModels()`](https://mi-erasmusmc.github.io/PatientGenerator/reference/availableModels.md).

``` r

library(PatientGenerator)

patientGenerator <- patientChat$new(
  model = "gpt-5.4",
  echo = "none"
)
```

### Generating Patients via Natural Language Prompts

Provide detailed prompts, including specific concept sets, for optimal
results.

``` r

patientGenerator$prompt(
  "Population (person table):
     - 10 adult patients
     - 5 female
     - 5 male
  
   Observation Period:
     - Start date between date of birth and 2025-12-31
  
   Condition Occurrence:
     - All patients must have Diabetes (condition_concept_id: 201826)
     - Start date between 2015-01-01 and 2020-12-31
  
   Drug Exposure:
     - All patients must have Semaglutide (drug_concept_id: 19079450)
     - Exposure within 30 days post-index date
  
   Measurement:
     - All patients must have Fasting glucose (measurement_concept_id: 3018251)
  
   Procedure Occurrence:
     - 50% of patients must have Amputation of toe (procedure_concept_id: 4159766)
  
   Output Requirements:
     - Populate only the tables specified in this prompt"
)
```

### Integration with `testthat`

Save the generated dataset as a JSON file and utilize
[`TestGenerator::patientsCDM`](https://darwin-eu.github.io/TestGenerator/reference/patientsCDM.html)
to instantiate a CDM reference.

``` r

patientGenerator$save(name = "diabetes-patients")

cdm <- TestGenerator::patientsCDM(
  testName = "diabetes-patients",
  cdmVersion = "5.4"
)

cdm$person |> 
  collect() |> 
  print()
```

``` R
#> cdm$person |> collect() |> head(5)
#>    person_id gender_concept_id
#>        <int>             <int>
#> 1:         1              8532
#> 2:         2              8532
#> 3:         3              8532
#> 4:         4              8532
#> 5:         5              8532
#>    year_of_birth person_source_value
#>            <int>              <char>
#> 1:          1962                P001
#> 2:          1970                P002
#> 3:          1958                P003
#> 4:          1981                P004
#> 5:          1967                P005
```

### Iterative Refinement

The LLM can be instructed to modify the current test set within the same
`patientChat` instance.

``` r

patientGenerator$prompt("Remove all male patients")
```

``` R
#> cdm$person |> collect() |> head(5)
#>    person_id gender_concept_id
#>        <int>             <int>
#> 1:         1              8532
#> 2:         2              8532
#> 3:         3              8532
#> 4:         4              8532
#> 5:         5              8532
#>    year_of_birth person_source_value
#>            <int>              <char>
#> 1:          1962                P001
#> 2:          1970                P002
#> 3:          1958                P003
#> 4:          1981                P004
#> 5:          1967                P005
```

### Visual Review and Editing with `patientDesigner()`

Launch the interactive editor to review and refine datasets:

``` r

PatientGenerator::patientDesigner()
```

The interface supports:

- Loading existing JSON test sets.
- Interactive CRUD operations (Create, Read, Update, Delete) on CDM
  tables.
- Visual timeline inspection and table previews.
- Exporting updated test sets to JSON.

### Concept Search with Hecate

`patientDesigner` integrates a concept search module powered by
[`hecateSearch()`](https://mi-erasmusmc.github.io/PatientGenerator/reference/hecateSearch.md).
This allows users to search for and insert valid OMOP concept IDs
directly into the CDM tables.

Configure Hecate globally via environment variables:

``` r

Sys.setenv(
  HECATE_BASE_URL = "https://your-hecate-server/api",
  HECATE_API_KEY = "your-api-key"
)
```

Or via package options:

``` r

options(PatientGenerator.hecate = list(
  base_url = "https://your-hecate-server/api",
  timeout_ms = 15000,
  api_key = "your-api-key"
))
```

### Further Documentation

- **Vignette**:
  [`vignette("shiny-integration", package = "PatientGenerator")`](https://mi-erasmusmc.github.io/PatientGenerator/articles/shiny-integration.md)
- **Reference**: Detailed API documentation and benchmarks are available
  on the GitHub Pages site.
