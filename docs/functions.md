# Function Overview

## patientChat

R6 API to generate schema-constrained synthetic OMOP CDM test sets.

Typical flow:

1. Create generator (`patientChat$new(...)`)
2. Send prompt (`$prompt(...)`)
3. Save JSON (`$save(...)`)

Useful methods:

- `$json_response()`
- `$retrieveCodelist(...)`
- `$availableModels()`

## patientChatNaive

Simpler one-shot generation helper for structured JSON output.

Use when you want a direct function call instead of R6 workflow.

## patientDesigner

D3/Shiny application for reviewing and editing test sets:

- load JSON cohorts
- edit OMOP table rows
- save curated outputs
- inspect timeline and tabular views

## hecateSearch

Search OMOP concepts through Hecate API and retrieve concept IDs and metadata.

Configuration:

- `HECATE_BASE_URL`
- `HECATE_API_KEY`
- optional package option `patientgenerator.hecate`

## availableModels

Returns models available through the configured OpenAI API key.
