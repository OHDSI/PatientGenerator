# Search Hecate concepts and return results as a data frame

Search Hecate concepts and return results as a data frame

## Usage

``` r
hecateSearch(
  query,
  vocabularyId = NULL,
  standardConcept = NULL,
  domainId = NULL,
  conceptClassId = NULL,
  limit = 20,
  client = hecateClient()
)
```

## Arguments

- query:

  Character(1); search query (required).

- vocabularyId:

  Character(1) or NULL; optional vocabulary filter (comma-separated).

- standardConcept:

  Character(1) or NULL; e.g. `"S"` (Standard), `"C"` (Classification).

- domainId:

  Character(1) or NULL; optional domain filter (comma-separated).

- conceptClassId:

  Character(1) or NULL; optional concept class filter.

- limit:

  Integer(1); max results (default 20, max 150).

- client:

  Hecate client (default
  [`hecateClient()`](https://mi-erasmusmc.github.io/PatientGenerator/reference/hecateClient.md)).

## Value

Data frame of search results, or NULL if an error occurred (API error or
bad response shape).

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple search
df <- hecateSearch("diabetes")

# Search with filters
df <- hecateSearch("hypertension", domainId = "Condition", limit = 10)
} # }
```
