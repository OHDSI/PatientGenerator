---
layout: default
title: Generate and Review Synthetic OMOP Patients
---

# Generate and Review Synthetic OMOP Patients

This workflow combines `patientChat` for generation and `patientDesigner` for review.

## 1) Generate a cohort with `patientChat`

```r
# OPENAI_API_KEY must be available in your environment
generator <- patientChat$new(model = "gpt-5.2", echo = "none")

generator$prompt(
  paste(
    "Generate exactly 5 synthetic patients in OMOP CDM v5.4.",
    "Include one observation period and at least one condition occurrence per patient."
  )
)

generator$save(name = "five-patients-demo")
```

Optional concept list retrieval:

```r
codelist <- readRDS(
  system.file("concept_sets", "ovarian_cancer_codelist.rds", package = "patientGenerator")
)

generator <- patientChat$new(
  model = "gpt-5.2",
  codelist_data = codelist,
  echo = "none"
)

generator$retrieveCodelist(concept_label = "ovarian", domain = "Condition")
```

## 2) Review and edit in `patientDesigner`

```r
patientDesigner()
```

You can also open a specific folder:

```r
patientDesigner(path = "/path/to/my/testsets")
```

## 3) Search concept IDs with Hecate

Set config via environment variables:

```r
Sys.setenv(
  HECATE_BASE_URL = "https://your-hecate-server/api",
  HECATE_API_KEY = "your-api-key",
  HECATE_TIMEOUT_MS = "15000"
)
```

Or package options:

```r
options(patientgenerator.hecate = list(
  base_url = "https://your-hecate-server/api",
  timeout_ms = 15000,
  api_key = "your-api-key"
))
```

Direct search:

```r
res <- hecateSearch(
  query = "hypertension",
  domainId = "Condition",
  standardConcept = "S",
  limit = 20
)

head(res)
```
