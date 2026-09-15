# `patientChat()` generates synthetic patients in the OMOP-CDM using an LLM API.

Requires an OPEN_AI_KEY in ~/.Renviron. After that just sent a prompt
and save() the results. The JSON file can be used as an OMOP-CDM patient
test set.

## Value

A JSON response that includes: the natural language answer from the LLM
and a JSON with test set patients in accordance to the provided schema.

## Details

Accepts a prompt as input. Produces a test set using a structured JSON
schema. Utilizes tools such as CodelistGenerator or Hecate to look up
concept IDs. Accepts subsequent prompts to modify existing test sets
that the LLM uses as context.

This class allows testing patient sets created by the LLM, prompt
engineering, integration of search tools and functionality, and creating
a set of patients to test analytical packages.

## Public fields

- `chat`:

  An ellmer chat instance

- `json_schema_path`:

  JSON schema to output structured results

- `response`:

  Ouput from the LLM

- `codelist`:

  A codelist with details to search for concepts ids

## Methods

### Public methods

- [`patientChat$new()`](#method-patientChat-initialize)

- [`patientChat$prompt()`](#method-patientChat-prompt)

- [`patientChat$json_response()`](#method-patientChat-json_response)

- [`patientChat$output()`](#method-patientChat-output)

- [`patientChat$retrieveCodelist()`](#method-patientChat-retrieveCodelist)

- [`patientChat$save()`](#method-patientChat-save)

- [`patientChat$availableModels()`](#method-patientChat-availableModels)

- [`patientChat$clone()`](#method-patientChat-clone)

------------------------------------------------------------------------

### `patientChat$new()`

Create a new chat to create JSON test sets for OMOP-CDM.

#### Usage

    patientChat$new(
      system_prompt = NULL,
      model = "gpt-5.4",
      jsonSchemaPath = NULL,
      echo = c("none", "output", "all"),
      codelist_data = NULL
    )

#### Arguments

- `system_prompt`:

  Initial system prompt to impose behaviour to the LLM

- `model`:

  Such as "gpt-5.3". For a complete list, call
  patientChat\$availableModels()

- `jsonSchemaPath`:

  The JSON schema to structure output from LLM

- `echo`:

  How the output will be displayed in the console

- `codelist_data`:

  A codelist with details to search for concepts ids

#### Returns

A new `patientChat` object.

------------------------------------------------------------------------

### `patientChat$prompt()`

Prompt to request data from LLM API

#### Usage

    patientChat$prompt(prompt)

#### Arguments

- `prompt`:

  A query in character.

------------------------------------------------------------------------

### `patientChat$json_response()`

Output in JSON format

#### Usage

    patientChat$json_response()

------------------------------------------------------------------------

### `patientChat$output()`

Returns the chat object

#### Usage

    patientChat$output()

------------------------------------------------------------------------

### `patientChat$retrieveCodelist()`

Retrieves and filters data from codelist_data

#### Usage

    patientChat$retrieveCodelist(concept_label = "Stage 1", domain = "Measurement")

#### Arguments

- `concept_label`:

  Filters the concept_name in the codelist with details

- `domain`:

  Filters the domain in the codelist with details.

------------------------------------------------------------------------

### `patientChat$save()`

Saves the JSON test set to disk.

#### Usage

    patientChat$save(name = "patient-chat-test", path = NULL)

#### Arguments

- `name`:

  Name of the file

- `path`:

  To save the file. If NULL, the package first tries
  `testthat::test_path("testCases")`, then checks
  `options(PatientGenerator.testSetDir = "...")`, and finally falls back
  to the package user data directory.

------------------------------------------------------------------------

### `patientChat$availableModels()`

Retrieves available models from the LLM API.

#### Usage

    patientChat$availableModels()

------------------------------------------------------------------------

### `patientChat$clone()`

The objects of this class are cloneable with this method.

#### Usage

    patientChat$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
generator <- patientChat$new()
generator$prompt("Give me 5 patients")
generator$save("my_test")
} # }
```
