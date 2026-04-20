# `patientChatNaive()` is a grapper for the ellmer package to send prompts and send the output test set to an LLM. Requires a valid API key.

Priorities: - Accepts a prompt as an input. - Produces a test set in
accordance to the provided JSON schema. - Utilizes tools such as
CodelistGenerator or Hecate to look up for functions. - Accepts a
subsequent prompt with a test set that the LLM has to use as a context

One function for this tasks allow us to: - Test the test sets created by
the LLM. - Test prompt engineering. - Test integration of tools
functionality. - Allow us to create fast a small set of patients to test
analytical packages.

## Usage

``` r
patientChatNaive(
  prompt = "### Give me a sample of five patients",
  model = "gpt-5.2",
  jsonSchemaPath = NULL
)
```

## Arguments

- prompt:

  A prompt to the LLM, in character or JSON response.

- model:

  The model used by the LLM. Currently only OpenAI models are accepted.

- jsonSchemaPath:

  Path to a JSON schema used to structure the response.

## Value

A JSON response that includes: the natural language answer from the LLM
and a JSON with test set patients in accordance to the provided schema.
