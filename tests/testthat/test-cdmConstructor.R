test_that("Initialize correctly tables defined in the parameter", {
  tables <- c(
    "observation_period"
  )
  cdm <- cdmConstructor$new(tables = tables)
  for (i in seq_along(tables)) {
    expect_r6_class(
      cdm[[tables[i]]],
      "cdmTable"
    )
  }
  tables <- c(
    "observation_period",
    "drug_exposure",
    "condition_occurrence",
    "measurement",
    "procedure_occurrence",
    "observation"
    )
  cdm <- cdmConstructor$new(tables = tables)
  for (i in seq_along(tables)) {
    expect_r6_class(
      cdm[[tables[i]]],
      "cdmTable"
    )
  }
})

test_that("cdmConstructor reset empties core tables", {
  cdm <- new_cdm()
  cdm$person$add(
    gender_concept_id = 8532L,
    year_of_birth = 1967L
    )
  # Add 1 person to all data tables
  cdm_tables <- c(
    "observation_period",
    "condition_occurrence",
    "drug_exposure",
    "measurement",
    "procedure_occurrence",
    "observation",
    "death"
  )
  for (i in seq_along(cdm_tables)) {
    expect_no_error({
      cdm[[cdm_tables[i]]]$add(
        person_id = 1L
        )
    })
  }
  # Check info was added to all tables
  for (i in seq_along(cdm_tables)) {
    cdm[[cdm_tables[i]]]$data() |> 
      pull(person_id) |> 
      length() |> 
      expect_equal(1)
  }
  ###
  cdm$reset()
  # Empty data after reset
  # Check info was added to all tables
  for (i in seq_along(cdm_tables)) {
    cdm[[cdm_tables[i]]]$data() |> 
      pull(person_id) |> 
      length() |> 
      expect_equal(0)
  }
})

test_that("person table add/update/delete behaves as expected", {
  cdm <- new_cdm()
  # ADD
  cdm$person$add(
    gender_concept_id = 8532L,
    year_of_birth = 1967L
    )
  cdm$person$add(
    gender_concept_id = 8507L,
    year_of_birth = 1988L
    )
  # UPDATE
  cdm$person$update(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 1977L
    )
  # TEST
  person_table <- cdm$person$data()
  expect_equal(person_table$person_id, c(1L, 2L))
  expect_equal(person_table$gender_concept_id, c(8507L, 8507L))
  expect_equal(person_table$year_of_birth, c(1977L, 1988L))
  # DELETE
  cdm$person$delete(person_id = 2L)
  person_table <- cdm$person$data()
  expect_equal(person_table$person_id, 1L)
})

test_that("observation/condition/drug/measurement/procedure_occurrence update dates", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1967L)
  cdm_tables <- c(
    "observation_period",
    "condition_occurrence",
    "drug_exposure",
    "measurement",
    "procedure_occurrence",
    "observation"
  )
  # Add 1 person to all data tables
  for (i in seq_along(cdm_tables)) {
    expect_no_error({
      cdm[[cdm_tables[i]]]$add(
        person_id = 1L
      )
    })
  }
  # Default dates
  for (i in seq_along(cdm_tables)) {

    # Check default start date
    cdm[[cdm_tables[i]]]$data()[,.SD, .SDcols = patterns("date")] |> 
      select(1) |> 
      pull() |>
      as.character() |> 
      expect_equal("2010-02-28")
    
    # Update dates
    expect_no_error({  
      cdm[[cdm_tables[i]]]$updateDates(
        person_id = 1L,
        event_id = 1L,
        start_date = as.Date("1997-10-29"),
        end_date = as.Date("1999-10-29")
      )
    })   
      # Check new date
    cdm[[cdm_tables[i]]]$data()[,.SD, .SDcols = patterns("date")] |> 
      select(1) |> 
      pull() |>
      as.character() |> 
      expect_equal("1997-10-29")
    
  }
})

test_that("getCdmData and getCdmDataTimeline return valid structures", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1967L)  
  cdm_tables <- c(
    "observation_period",
    "condition_occurrence",
    "drug_exposure",
    "measurement",
    "procedure_occurrence",
    "observation"
  )
  # Add 1 person to all data tables
  for (i in seq_along(cdm_tables)) {
    expect_no_error({
      cdm[[cdm_tables[i]]]$add(
        person_id = 1L
      )
    })
  }
  cdm_json <- cdm$getCdmData()
  expect_true(
    jsonlite::validate(cdm_json)
    )
  expect_true("measurement" %in% names(jsonlite::fromJSON(cdm_json)))
  expect_true("observation" %in% names(jsonlite::fromJSON(cdm_json)))
  timeline <- cdm$getCdmDataTimeline()
  expect_s3_class(
    timeline,
    "data.table"
    )
  expect_in(
    names(timeline),
    c("event_id",
      "concept_id",
      "person_id",
      "start_date",
      "end_date",
      "type",
      "categories")
    )
  expect_in(
    unique(timeline$type),
    c("observation_period",
      "drug_exposure",
      "condition_occurrence",
      "measurement",
      "procedure_occurrence",
      "observation"
      )
  )
})

test_that("death table can be added, exported, and shown on the timeline", {
  cdm <- new_cdm()
  cdm$person$add(gender_concept_id = 8532L, year_of_birth = 1967L)
  expect_no_error(cdm$death$add(person_id = 1L))
  expect_equal(nrow(cdm$death$data()), 1L)
  expect_false("death_id" %in% names(cdm$death$data()))
  expect_equal(cdm$death$data()$person_id[[1]], 1L)
  expect_equal(cdm$death$data()$death_date[[1]], as.Date("2010-02-28"))
  cdm$death$updateDates(
    person_id = 1L,
    event_id = 1L,
    start_date = as.Date("2020-05-20"),
    end_date = NULL
  )
  expect_equal(cdm$death$data()$death_date[[1]], as.Date("2020-05-20"))
  cdm_json <- jsonlite::fromJSON(cdm$getCdmData())
  expect_true("death" %in% names(cdm_json))
  expect_false("death_id" %in% names(cdm_json$death))
  timeline <- cdm$getCdmDataTimeline()
  death_row <- timeline[timeline$type == "death", ]
  expect_equal(nrow(death_row), 1L)
  expect_equal(death_row$event_id[[1]], 1L)
  expect_equal(death_row$person_id[[1]], 1L)
  expect_equal(death_row$start_date[[1]], as.Date("2020-05-20"))
})

test_that("loadJsonTestSet completes beta blocker death table schema", {
  cdm <- new_cdm()
  path <- testthat::test_path("testCases", "beta_blocker.json")
  skip_if_not(file.exists(path))
  expect_no_error(cdm$loadJsonTestSet(path))
  expect_equal(nrow(cdm$person$data()), 10L)
  expect_equal(length(unique(cdm$condition_occurrence$data()$condition_concept_id)), 19L)
  expect_equal(nrow(cdm$death$data()), 3L)
  expect_true("cause_source_value" %in% names(cdm$death$data()))
  expect_false("death_id" %in% names(cdm$death$data()))
  expect_false("death_occurrence_id" %in% names(cdm$death$data()))
  expect_equal(cdm$death$data()[person_id == 1L]$death_date[[1]], as.Date("2017-10-07"))
  expect_equal(sum(cdm$getCdmDataTimeline()$type == "death"), 3L)
})

test_that("loadJsonTestSet loads source test fixtures", {
  cdm <- new_cdm()
  path <- testthat::test_path("testCases", "objective_1_patients.json")
  expect_no_error(cdm$loadJsonTestSet(path))
  expect_gt(nrow(cdm$person$data()), 0)
})

test_that("loadJsonTestSet backfills missing end dates for renderable tables", {
  cdm <- new_cdm()
  path <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      person = list(
        list(
          person_id = 1L,
          gender_concept_id = 8507L,
          year_of_birth = 1970L,
          month_of_birth = 1L,
          day_of_birth = 1L
        )
      ),
      condition_occurrence = list(
        list(
          condition_occurrence_id = 1L,
          person_id = 1L,
          condition_concept_id = 201826L,
          condition_start_date = "2020-01-10",
          condition_end_date = NULL
        )
      ),
      drug_exposure = list(
        list(
          drug_exposure_id = 1L,
          person_id = 1L,
          drug_concept_id = 19079450L,
          drug_exposure_start_date = "2020-02-10",
          drug_exposure_end_date = NULL
        )
      ),
      procedure_occurrence = list(
        list(
          procedure_occurrence_id = 1L,
          person_id = 1L,
          procedure_concept_id = 4159766L,
          procedure_date = "2020-03-10",
          procedure_end_date = NULL
        )
      )
    ),
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  expect_no_error(cdm$loadJsonTestSet(path))
  expect_equal(
    cdm$condition_occurrence$data()$condition_end_date[[1]],
    as.Date("2020-01-10")
  )
  expect_equal(
    cdm$drug_exposure$data()$drug_exposure_end_date[[1]],
    as.Date("2020-02-10")
  )
  expect_equal(
    cdm$procedure_occurrence$data()$procedure_end_date[[1]],
    as.Date("2020-03-10")
  )
})

test_that("Testing methods on LLM testset", {
  testthat::skip_on_cran()
  # An LLM testset for this test
  # model <- pick_openai_model()
  # patientGenerator <- patientChat$new(model = "gpt-5.6-luna")
  
  # ### Test set description for this test:
  # patientGenerator$prompt(
  # "Population (person table):
  #   - 10 adult patients
  #   - 5 female
  #   - 5 male
  
  #  Observation Period:
  #   - Start date between date of birth each person and end of observation 2025-12-31
  
  #  Condition Occurrence:
  #    - All patients must have Diabetes (condition_concept_id: 201826)
  #    - Condition start date between 2015-01-01 and 2020-12-31
  
  #  Drug Exposure:
  #    - All patients must have Semaglutide (drug_concept_id: 19079450)
  #    - Drug exposure in a window of 0 to 30 days after index date
  
  #  Measurement:
  #    - All patients must have Fasting glucose (measurement_concept_id: 3018251)
  
  #  Procedure cccurrence:
  #    - 50% of patients (5 patients) must have Amputation of toe (procedure_concept_id: 4159766)
  
  #  Output Requirements:
  #   - Fill only specified tables in this prompt"
  #   )
  # patientGenerator$save("test_diabetes_patients")
  ### Check testset
  cdm <- TestGenerator::patientsCDM(
    testName = "test_diabetes_patients",
    cdmVersion = "5.4"
    )
  cdm$person |>
    collect() |>
    nrow() |>
    expect_equal(10)
  cdm$procedure_occurrence |>
    collect() |>
    nrow() |>
    expect_equal(5)
  path <- testthat::test_path(
    "testCases",
    "test_diabetes_patients.json"
    )
  cdm_tables <- c(
    "observation_period",
    "condition_occurrence",
    "drug_exposure",
    "measurement",
    "procedure_occurrence",
    "observation"
  )
  expect_no_error({
    cdm <- new_cdm()
    cdm$loadJsonTestSet(path)
    for (i in seq_along(cdm_tables)) {
      expect_no_error({
        cdm[[cdm_tables[i]]]$add(
          person_id = 1L
        )
      })
    }
  })
  cdm$condition_occurrence$data() |> 
    pull(person_id) |> 
    expect_length(11)
  cdm$drug_exposure$data() |> 
    pull(person_id) |> 
    expect_length(11)
  cdm$measurement$data() |> 
    pull(person_id) |> 
    expect_length(11)
  cdm$procedure_occurrence$data() |> 
    pull(person_id) |> 
    expect_length(6)
})

test_that("Testing modified test from LLM can be inserted back to TestGenerator", {
  testthat::skip_on_cran()
  # Using the same diabetes test from previous test
  path <- testthat::test_path(
    "testCases",
    "test_diabetes_patients.json"
  )
  cdm_tables <- c(
    "observation_period",
    "condition_occurrence",
    "drug_exposure",
    "measurement",
    "procedure_occurrence",
    "observation"
  )  
  # Load into memory and modify all fields
  expect_no_error({
    cdm <- new_cdm()
    cdm$loadJsonTestSet(path)
  
    for (i in seq_along(cdm_tables)) {
      expect_no_error({
        cdm[[cdm_tables[i]]]$add(
          person_id = 1L
        )
      })
    }
    
    # Update dates
    cdm[[cdm_tables[i]]]$updateDates(
      person_id = 1L,
      event_id = 1L,
      start_date = as.Date("1997-10-29"),
      end_date = as.Date("1999-10-29")
    )
  })
  mod_test_file <- testthat::test_path(
    "testCases",
    "mod_test_file.json"
  )
  write(
    cdm$getCdmData(),
    file = mod_test_file
  )
  expect_no_error({
    cdm <- TestGenerator::patientsCDM(
      testName = "mod_test_file",
      cdmVersion = "5.4"
      )
    cdm$person |>
      collect() |>
      nrow() |>
      expect_equal(10)
    cdm$procedure_occurrence |>
      collect() |>
      nrow() |>
      expect_equal(6)
  })
  unlink(mod_test_file, recursive = TRUE)
})

test_that("Testing methods on LLM testset 'objective_1_patient'", {
  path <- testthat::test_path(
    "testCases",
    "objective_1_patients.json"
  )
  cdm_tables <- c(
    "observation_period",
    "condition_occurrence",
    "drug_exposure",
    "measurement",
    "procedure_occurrence",
    "observation"
  )
  expect_no_error({
    cdm <- new_cdm()
    cdm$loadJsonTestSet(path)
    for (i in seq_along(cdm_tables)) {
      expect_no_error({
        cdm[[cdm_tables[i]]]$add(
          person_id = 1L
        )
      })
    }
  })
  cdm$condition_occurrence$data() |> 
    pull(person_id) |> 
    expect_length(5)
  cdm$drug_exposure$data() |> 
    pull(person_id) |> 
    expect_length(5)
  cdm$measurement$data() |> 
    pull(person_id) |> 
    expect_length(11)
  cdm$procedure_occurrence$data() |> 
    pull(person_id) |> 
    expect_length(1)
})
