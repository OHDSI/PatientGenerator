# Benchmark Results: GPT-5 Main Models

Run date: 2026-02-28  
Task: Generate exactly 10 OMOP CDM v5.4 synthetic patients with:

- diabetes in `condition_occurrence`
- semaglutide in `drug_exposure`

Model set benchmarked:

- `gpt-5`
- `gpt-5-mini`
- `gpt-5-nano`
- `gpt-5.1`
- `gpt-5.2`

## Results table

| Model | Status | Elapsed Seconds | Person Count | Notes |
|---|---:|---:|---:|---|
| gpt-5 | success | 129.06 | 10 | Valid JSON and loaded correctly |
| gpt-5-mini | success | 140.62 | 10 | Valid JSON and loaded correctly |
| gpt-5-nano | success | 93.07 | 10 | Fastest successful model |
| gpt-5.1 | error | 72.76 | NA | JSON parse error in downstream load path |
| gpt-5.2 | error | 75.57 | NA | JSON parse error in downstream load path |

## Summary

- Successful runs: 3 / 5
- Fastest successful model: `gpt-5-nano` (93.07s)
- Mean successful latency: 120.91s
- Median successful latency: 129.06s

## Raw data

- [CSV download](data/patientChat_model_benchmark_results_gpt5_main_2026-02-28.csv)
