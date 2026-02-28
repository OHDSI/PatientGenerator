test_that("patientChat benchmark across all available models for diabetes + semaglutide cohort", {
  # ---------------------------------------------------------------------------
  # PLAN (manual benchmark, disabled by default):
  # 1) List all available models via availableModels().
  # 2) For each model, run patientChat generation with a fixed representative task:
  #    - exactly 10 synthetic patients
  #    - diabetes condition_occurrence
  #    - semaglutide drug_exposure
  # 3) Measure elapsed time per model.
  # 4) Validate generated JSON by loading it with cdmConstructor.
  # 5) Save benchmark results to CSV for later comparison.
  # ---------------------------------------------------------------------------

  skip_if_no_openai()
  testthat::skip_on_cran()

  run_benchmark <- tolower(Sys.getenv("PATIENTGENERATOR_RUN_MODEL_BENCHMARK", "false")) %in%
    c("1", "true", "yes")
  if (!run_benchmark) {
    testthat::skip(
      "Benchmark disabled. Set PATIENTGENERATOR_RUN_MODEL_BENCHMARK=true to run."
    )
  }

  models <- unique(patientGenerator::availableModels())
  if (length(models) == 0) {
    testthat::skip("No models returned by availableModels().")
  }

  # Optional filtering for targeted benchmark runs.
  # Example:
  #   PATIENTGENERATOR_MODEL_BENCHMARK_REGEX='^(gpt-5|gpt-5-mini|gpt-5-nano)$'
  model_regex <- Sys.getenv("PATIENTGENERATOR_MODEL_BENCHMARK_REGEX", unset = "")
  if (nzchar(model_regex)) {
    models <- models[grepl(model_regex, models)]
  }

  if (length(models) == 0) {
    testthat::skip("No models matched PATIENTGENERATOR_MODEL_BENCHMARK_REGEX.")
  }

  benchmark_prompt <- paste(
    "Generate exactly 10 synthetic patients in OMOP-CDM v5.4.",
    "For each patient, include at least one condition_occurrence for diabetes mellitus.",
    "For each patient, include at least one drug_exposure for semaglutide.",
    "Return valid JSON following the provided schema."
  )

  sanitize_model_id <- function(x) {
    gsub("[^A-Za-z0-9._-]", "_", x)
  }

  out_dir_env <- Sys.getenv("PATIENTGENERATOR_MODEL_BENCHMARK_DIR", unset = "")
  out_dir <- if (nzchar(out_dir_env)) {
    out_dir_env
  } else {
    file.path(tempdir(), paste0("patientChat_model_benchmark_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  results <- data.frame(
    model = character(),
    status = character(),
    elapsed_seconds = numeric(),
    person_count = integer(),
    output_file = character(),
    error = character(),
    stringsAsFactors = FALSE
  )

  for (model in models) {
    safe_model <- sanitize_model_id(model)
    output_name <- paste0("patient-chat-", safe_model)
    output_file <- file.path(out_dir, paste0(output_name, ".json"))

    started_at <- Sys.time()
    model_result <- tryCatch(
      {
        generator <- patientChat$new(model = model, echo = "none")
        generator$prompt(benchmark_prompt)
        generator$save(name = output_name, path = out_dir)

        cdm <- new_cdm()
        cdm$loadJsonTestSet(output_file)
        n_person <- nrow(cdm$person$data())

        status <- if (isTRUE(!is.na(n_person) && n_person == 10L)) "success" else "success_non_10"
        list(
          status = status,
          person_count = as.integer(n_person),
          error = NA_character_
        )
      },
      error = function(e) {
        list(
          status = "error",
          person_count = NA_integer_,
          error = conditionMessage(e)
        )
      }
    )
    elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))

    results <- rbind(
      results,
      data.frame(
        model = model,
        status = model_result$status,
        elapsed_seconds = elapsed,
        person_count = model_result$person_count,
        output_file = output_file,
        error = model_result$error,
        stringsAsFactors = FALSE
      )
    )
  }

  results_file <- file.path(out_dir, "patientChat_model_benchmark_results.csv")
  utils::write.csv(results, results_file, row.names = FALSE)

  message("Model benchmark output directory: ", out_dir)
  message("Model benchmark results file: ", results_file)
  print(results)

  # Keep assertions minimal: this test is intended to collect benchmark data.
  testthat::expect_equal(nrow(results), length(models))
  testthat::expect_true(file.exists(results_file))
})
