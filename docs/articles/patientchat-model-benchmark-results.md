---
layout: default
title: patientChat Model Benchmark Results
---

# patientChat Model Benchmark Results

Published snapshot date: **2026-02-28**.

Benchmark task:

- Generate exactly 10 synthetic OMOP CDM v5.4 patients.
- Include diabetes in `condition_occurrence`.
- Include semaglutide in `drug_exposure`.
- Record elapsed generation time per model.

## Results

| Model      | Status  | Elapsed Seconds | Person Count | Notes |
|:-----------|:--------|----------------:|-------------:|:------|
| gpt-5-nano | success | 93.07           | 10           | Fastest successful run |
| gpt-5      | success | 129.06          | 10           | Stable output |
| gpt-5-mini | success | 140.62          | 10           | Stable output |
| gpt-5.1    | error   | 72.76           | NA           | Invalid JSON text returned |
| gpt-5.2    | error   | 75.57           | NA           | Invalid JSON text returned |

Summary:

- Successful runs: 3/5 models.
- Failed runs: 2/5 models.
- Fastest successful model: `gpt-5-nano`.

Source CSV in repository:

- [`docs/data/patientChat_model_benchmark_results_gpt5_main_2026-02-28.csv`](../data/patientChat_model_benchmark_results_gpt5_main_2026-02-28.csv)

## Reproducibility script (manual)

```r
Sys.setenv(
  OPENAI_API_KEY = "<your-key>",
  PATIENTGENERATOR_RUN_MODEL_BENCHMARK = "true",
  PATIENTGENERATOR_MODEL_BENCHMARK_DIR = "/path/to/benchmark-output"
)

testthat::test_file("tests/testthat/test-patientChat-model-benchmark.R")
```
