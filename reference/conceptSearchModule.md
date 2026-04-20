# Concept search Shiny module

Reusable UI and server for searching the vocabulary (Hecate) and
selecting a concept. Shows a button that opens a modal with text input,
search button, and results in a DT table. Optional callback when a
concept is selected.

## Usage

``` r
conceptSearchUI(id, buttonLabel = "Concept search")

conceptSearchServer(id, onConceptSelected = NULL, placeholderText = "")
```

## Arguments

- id:

  Module namespace id.

- buttonLabel:

  Label for the trigger button (default `"Concept search"`).

- onConceptSelected:

  Optional function of one argument `conceptId` called when user selects
  a concept; modal is closed after.

- placeholderText:

  Character string used as placeholder text in the search input field.

## Functions

- `conceptSearchUI()`: UI for the concept search: a single button that
  opens the search modal.

- `conceptSearchServer()`: Server for the concept search modal (text
  input, search, DT, close).
