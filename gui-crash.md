# Proposal for issue \#47: open-ended condition_occurrence must be selectable without creating an end date or crashing

## Scope

This is a proposal only. It does not change package code.

The focus is the test-data case in
[mod_test_file_updated.json](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/tests/testthat/testCases/mod_test_file_updated.json),
where many `condition_occurrence` rows have:

``` json
"condition_end_date": null
```

Target behavior:

1.  clicking an open-ended condition occurrence in the D3 timeline does
    not crash
2.  the row opens in the GUI
3.  `condition_end_date` stays empty until the user chooses to set it
4.  the user can add the first end date later

## Observed failure

The runtime error is:

``` text
Warning: Error in data.table::set: RHS of assignment to existing column
'condition_end_date' is zero length but not NULL.
```

Relevant stack:

1.  `observeEvent(input$bar_end)` in
    [patientsDesigner.R](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/R/patientsDesigner.R#L492)
2.  `cdm[[type]]$updateDates(...)` in
    [patientsDesigner.R](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/R/patientsDesigner.R#L500)
3.  `data.table::set(...)` in
    [cdmConstructor.R](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/R/cdmConstructor.R#L195)

Current implementation detail:

``` r

updateDates = function(person_id, event_id, start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  ...
  if (!is.null(end_date)) {
    data.table::set(
      private$.data,
      i = index_table,
      j = name_end_date,
      value = end_date
    )
  }
}
```

If `end_date` arrives as `character(0)`, `NULL`, or an omitted browser
value that becomes zero-length after coercion:

``` r

as.Date(character(0))
```

the result is:

1.  not `NULL`
2.  length `0`
3.  invalid as the RHS for
    [`data.table::set()`](https://rdrr.io/pkg/data.table/man/assign.html)
    on an existing column

So the specific crash is a zero-length optional-date assignment bug.

## Important clarification about the interaction

The current error is raised from `input$bar_end`, which is emitted at
the end of a drag interaction, not from a plain click.

In
[cdm_timeline.js](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/inst/d3/cdm_timeline.js):

``` js
function dragStart(event, d) {
  Shiny.setInputValue("bar_start", d, {priority: "event"});
}
```

``` js
function dragEnd(event, d) {
  Shiny.setInputValue("bar_end", d, {priority: "event"});
}
```

Also, the current whole-bar drag mutates both dates:

``` js
d.start_date = xScaleUpdated.invert(event.x)
d.end_date = xScaleUpdated.invert(end_date_position)
```

That matters because any visual fallback that gives an open-ended row a
fake width will also make whole-bar dragging implicitly create a real
`end_date`.

That is not the desired UX. The requirement is:

1.  selecting the row must not crash
2.  adding the first end date must be explicit, not a side effect of
    moving the row

## Evidence in the fixture

The file
[mod_test_file_updated.json](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/tests/testthat/testCases/mod_test_file_updated.json)
contains multiple open-ended conditions, for example:

``` json
{
  "condition_occurrence_id": 1,
  "person_id": 1,
  "condition_concept_id": 201826,
  "condition_start_date": "2016-05-10",
  "condition_start_datetime": "2016-05-10T00:00:00Z",
  "condition_end_date": null,
  "condition_end_datetime": null
}
```

This is a valid clinical state. The GUI should support it directly.

## Root cause

There are three separate problems.

### 1. Optional `end_date` is handled like a required date in D3 geometry

In
[cdm_timeline.js](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/inst/d3/cdm_timeline.js):

``` js
.attr("width", d => xScale(new Date(d.end_date)) - xScale(new Date(d.start_date)))
```

``` js
.attr("cx", d => xScale(new Date(d.end_date)))
```

``` js
return {
  x: xScaleUpdated(new Date(d.end_date)),
};
```

### 2. Optional `end_date` is not normalized before assignment

In
[cdmConstructor.R](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/R/cdmConstructor.R#L179):

``` r

start_date <- as.Date(start_date)
end_date <- as.Date(end_date)
```

and then:

``` r

if (!is.null(end_date)) {
  data.table::set(..., value = end_date)
}
```

That check is too weak. A zero-length `Date` vector passes it.

### 3. The interaction model for open-ended rows is not defined

If we render open-ended rows as short pseudo-bars, the current drag
handlers would let users:

1.  move the row
2.  left-drag the row
3.  accidentally materialize a real end date

That would violate the intended behavior that the first end date is
added only when the user wants it.

## Proposal

### Step 1. Treat `start_date` and `end_date` differently

`start_date` is required.

`end_date` is optional.

The proposal should not normalize both the same way.

Use two different contracts:

1.  required dates must validate to a length-1 non-missing `Date`
2.  optional dates may normalize to `as.Date(NA)`

Example:

``` r

normalizeRequiredDate <- function(x) {
  out <- as.Date(x)

  if (length(out) != 1 || is.na(out)) {
    stop("Required date is missing or invalid", call. = FALSE)
  }

  out
}

normalizeOptionalDate <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(as.Date(NA))
  }

  if (is.character(x) && length(x) == 1 && !nzchar(x)) {
    return(as.Date(NA))
  }

  out <- as.Date(x)

  if (length(out) == 0) {
    return(as.Date(NA))
  }

  out
}
```

This avoids the earlier mistake of silently converting a broken
`start_date` into `NA`.

### Step 2. Sanitize the `bar_end` payload at the observer boundary

The crash is happening at the D3-to-server boundary, so that boundary
should be normalized explicitly.

Instead of letting `observeEvent(input$bar_end)` call `updateDates()`
with raw browser values, convert the payload first.

Example direction:

``` r

normalizeBarEndPayload <- function(update_data) {
  list(
    person_id = update_data$person_id,
    event_id = update_data$event_id,
    type = update_data$type,
    start_date = normalizeRequiredDate(update_data$start_date),
    end_date = normalizeOptionalDate(update_data$end_date)
  )
}
```

``` r

observeEvent(input$bar_end, {
  update_data <- normalizeBarEndPayload(input$bar_end)

  cdm[[update_data$type]]$updateDates(
    person_id = update_data$person_id,
    event_id = update_data$event_id,
    start_date = update_data$start_date,
    end_date = update_data$end_date
  )
})
```

This is the most important fix because it directly addresses the
observed stack trace.

### Step 3. Harden `updateDates()` as a second line of defense

`updateDates()` should still defend itself.

Example direction:

``` r

updateDates = function(person_id, event_id, start_date, end_date) {
  start_date <- normalizeRequiredDate(start_date)
  end_date <- normalizeOptionalDate(end_date)

  name_id <- private$.tableNameId()
  name_start_date <- private$.tableNameDate("start")
  name_end_date <- private$.tableNameDate("end")
  index_table <- which(private$.data[[name_id]] == event_id)

  if (length(index_table) > 0) {
    data.table::set(
      private$.data,
      i = index_table,
      j = name_start_date,
      value = start_date
    )

    if (length(end_date) == 1) {
      data.table::set(
        private$.data,
        i = index_table,
        j = name_end_date,
        value = end_date
      )
    }
  }
}
```

The key rule is:

`updateDates()` must only ever assign a length-1 `Date`, including
`as.Date(NA)` for the optional end date.

### Step 4. Render open-ended rows with display geometry only

The timeline still needs something clickable when `end_date` is missing.

Do not persist a fake end date into the CDM table. Instead, derive
display-only fields.

Example direction:

``` r

timelineData <- cdm$getCdmDataTimeline() |>
  dplyr::mutate(
    end_date_missing = is.na(.data$end_date),
    display_end_date = dplyr::if_else(
      end_date_missing,
      .data$start_date + 1,
      .data$end_date
    )
  )
```

That is only for rendering.

The underlying row must still keep `end_date = NA`.

### Step 5. Do not allow whole-bar drag or left-drag for open-ended rows

This is the biggest behavioral change needed in the proposal.

If an open-ended row is drawn with a short visual fallback, then the
current handlers would make these actions implicitly create an end date:

1.  moving the whole bar
2.  dragging the left handle

That should not happen.

Recommended interaction model for open-ended rows:

1.  row body: clickable for selection
2.  whole-bar drag: disabled
3.  left-handle drag: disabled
4.  right-handle drag: optional, and if enabled it explicitly creates
    the first end date

This keeps “select row” separate from “set first end date”.

Example direction in D3:

``` js
function isOpenEnded(d) {
  return d.end_date_missing || d.end_date == null;
}
```

``` js
g.selectAll("rect")
  .data(data)
  .enter()
  .append("rect")
  .on("click", function(event, d) {
    Shiny.setInputValue("bar_start", d, {priority: "event"});
  })
  .each(function(d) {
    if (!isOpenEnded(d)) {
      d3.select(this).call(moveBar);
    }
  })
```

And similarly:

``` js
if (!isOpenEnded(d)) {
  d3.select(this).call(elongLeft);
}
```

For open-ended rows, the right-side interaction can be one of two
designs:

1.  manual only: user sets `condition_end_date` in the form
2.  manual or right-drag: right-drag creates the first end date
    explicitly

Either is acceptable, but the proposal should choose one explicitly. The
safer initial fix is manual entry only.

### Step 6. Make selection independent from drag completion

A plain row selection should not depend on `bar_end`.

Selection should happen on:

``` js
Shiny.setInputValue("bar_start", d, {priority: "event"});
```

and should remain valid even when the row has no real `end_date`.

Server behavior should be:

1.  `bar_start` selects the row and opens the tab
2.  `bar_end` only runs after a permitted drag interaction

That separation reduces the chance that a simple selection path triggers
date mutation logic.

### Step 7. Add a testable helper for the failing boundary

The previous proposal did not test the real failure path closely enough.

To make the boundary testable, extract the coercion into a pure helper
like:

``` r

normalizeBarEndPayload <- function(update_data) {
  list(
    person_id = update_data$person_id,
    event_id = update_data$event_id,
    type = update_data$type,
    start_date = normalizeRequiredDate(update_data$start_date),
    end_date = normalizeOptionalDate(update_data$end_date)
  )
}
```

Then test exactly the problematic input shape:

``` r

test_that("bar_end payload with zero-length end date is normalized safely", {
  payload <- normalizeBarEndPayload(list(
    person_id = 1L,
    event_id = 1L,
    type = "condition_occurrence",
    start_date = "2016-05-10",
    end_date = character(0)
  ))

  expect_equal(payload$start_date, as.Date("2016-05-10"))
  expect_equal(length(payload$end_date), 1L)
  expect_true(is.na(payload$end_date))
})
```

That test covers the actual server boundary the stack trace points to.

### Step 8. Support explicit creation of the first end date

Open-ended means:

1.  the row starts with `condition_end_date = NA`
2.  the user may later set the first real end date

Example target behavior:

``` r

test_that("open-ended condition can receive its first end date", {
  cdm <- new_cdm()
  cdm$condition_occurrence$load(data.frame(
    condition_occurrence_id = 1L,
    person_id = 1L,
    condition_concept_id = 201826L,
    condition_start_date = as.Date("2016-05-10"),
    condition_end_date = as.Date(NA)
  ))

  cdm$condition_occurrence$updateDates(
    person_id = 1L,
    event_id = 1L,
    start_date = as.Date("2016-05-10"),
    end_date = as.Date("2016-06-01")
  )

  expect_equal(
    cdm$condition_occurrence$data()$condition_end_date[[1]],
    as.Date("2016-06-01")
  )
})
```

## Recommended implementation order

1.  Reproduce with
    [mod_test_file_updated.json](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/tests/testthat/testCases/mod_test_file_updated.json).
2.  Add `normalizeRequiredDate()`, `normalizeOptionalDate()`, and
    `normalizeBarEndPayload()`.
3.  Use `normalizeBarEndPayload()` inside the `bar_end` observer.
4.  Harden `updateDates()` so it only assigns length-1 dates.
5.  Add `display_end_date` and `end_date_missing` to timeline data.
6.  Disable whole-bar drag and left-drag for open-ended rows.
7.  Keep row selection working through `bar_start`.
8.  Decide whether the first end date is added manually only or also via
    right-drag.
9.  Add regression tests for the observer boundary and the open-ended
    workflow.

## Tests to add

### 1. Fixture regression

``` r

test_that("open-ended condition occurrences survive JSON load", {
  cdm <- cdmConstructor$new()
  cdm$loadJsonTestSet(test_path("testCases", "mod_test_file_updated.json"))

  dat <- cdm$condition_occurrence$data()

  expect_true(any(is.na(dat$condition_end_date)))
  expect_true(any(!is.na(dat$condition_start_date)))
})
```

### 2. Boundary normalization regression

This directly covers the observed stack trace.

``` r

test_that("bar_end payload with zero-length end date is normalized safely", {
  payload <- normalizeBarEndPayload(list(
    person_id = 1L,
    event_id = 1L,
    type = "condition_occurrence",
    start_date = "2016-05-10",
    end_date = character(0)
  ))

  expect_equal(payload$start_date, as.Date("2016-05-10"))
  expect_equal(length(payload$end_date), 1L)
  expect_true(is.na(payload$end_date))
})
```

### 3. `updateDates()` no-crash regression

``` r

test_that("updateDates accepts missing optional end date", {
  cdm <- new_cdm()
  cdm$condition_occurrence$add(person_id = 1L)

  expect_no_error(
    cdm$condition_occurrence$updateDates(
      person_id = 1L,
      event_id = 1L,
      start_date = as.Date("2016-05-10"),
      end_date = as.Date(NA)
    )
  )
})
```

### 4. Required start-date regression

``` r

test_that("missing required start date is rejected", {
  expect_error(
    normalizeBarEndPayload(list(
      person_id = 1L,
      event_id = 1L,
      type = "condition_occurrence",
      start_date = character(0),
      end_date = character(0)
    ))
  )
})
```

### 5. GUI behavior regression

The critical end-to-end assertions are:

1.  load
    [mod_test_file_updated.json](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/tests/testthat/testCases/mod_test_file_updated.json)
2.  click an open-ended `condition_occurrence` in the timeline
3.  app does not crash
4.  `Condition Occurrence` tab opens
5.  `condition_end_date` input is empty
6.  whole-bar drag does not implicitly create an end date
7.  user sets an end date explicitly
8.  row updates successfully

## Manual verification checklist

1.  Start
    [`patientDesigner()`](https://mi-erasmusmc.github.io/PatientGenerator/reference/patientDesigner.md).
2.  Load
    [mod_test_file_updated.json](https://mi-erasmusmc.github.io/Users/cbarboza/Documents/mi-erasmusmc/PatientGenerator/tests/testthat/testCases/mod_test_file_updated.json).
3.  Select a patient with `condition_end_date = null`.
4.  Open the `Timeline` tab.
5.  Click that condition occurrence.
6.  Confirm the app does not crash.
7.  Confirm the `Condition Occurrence` tab opens.
8.  Confirm `condition_start_date` is filled.
9.  Confirm `condition_end_date` is empty.
10. Confirm moving the row body does not create an end date.
11. Confirm left-drag is disabled for open-ended rows.
12. Enter an end date manually.
13. Confirm the row updates without error.
14. Confirm the timeline re-renders correctly.

## Expected outcome

After implementing this proposal:

1.  open-ended `condition_occurrence` rows from JSON fixtures remain
    valid
2.  selecting them in D3 no longer crashes the app
3.  `updateDates()` no longer receives a zero-length RHS for
    `condition_end_date`
4.  a broken `start_date` is rejected instead of silently overwritten
    with `NA`
5.  whole-bar drag does not implicitly create an end date
6.  the first end date can be added later, explicitly
