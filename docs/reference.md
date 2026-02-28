---
layout: default
title: Reference
---

# Reference

## Generation APIs

### `patientChat`
R6 generator for synthetic OMOP CDM patients using an LLM API.

```r
generator <- patientChat$new(model = "gpt-5.2", echo = "none")
generator$prompt("Generate 5 OMOP CDM patients")
generator$save("my_testset")
```

### `patientChatNaive(prompt, model, jsonSchemaPath)`
Simple function wrapper to send a prompt and request schema-constrained output.

### `availableModels()`
Returns available model IDs from the configured provider.

## Design and Curation

### `patientDesigner(path = NULL)`
Launches the D3/Shiny application to review and edit patient test sets.

### `getTestSets(path = NULL)`
Lists available JSON test sets.

### `conceptSearchUI(id, buttonLabel = "Concept search")`
UI module for concept search modal.

### `conceptSearchServer(id, onConceptSelected = NULL, placeholderText = "")`
Server module for concept search behavior.

## Vocabulary Search

### `hecateClient(baseUrl = NULL, timeoutMs = NULL, apiKey = NULL)`
Creates a configured Hecate API client.

### `hecateSearch(query, vocabularyId = NULL, standardConcept = NULL, domainId = NULL, conceptClassId = NULL, limit = 20, client = hecateClient())`
Searches Hecate and returns a data frame of matched concepts.

## Utilities

### `x %||% y`
Null-coalescing helper. Returns `x` unless `x` is `NULL`.
