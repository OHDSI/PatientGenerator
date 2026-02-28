---
layout: default
title: Home
---

# patientGenerator

`patientGenerator` helps you build OMOP CDM synthetic test sets in two complementary ways:

- `patientChat`: generate structured patient JSON with an LLM.
- `patientDesigner`: review and edit those patients in a D3/Shiny interface.
- Hecate-backed concept search: find OMOP concept codes while curating patients.

## Install

```r
# install.packages("pak")
pak::pak("mi-erasmusmc/patientGenerator")
```

## Quick Start

1. Generate a cohort with `patientChat`.
2. Save it to JSON.
3. Open `patientDesigner()` to review and edit.
4. Use Hecate search to resolve concept IDs.

## Documentation

- [Reference](reference)
- [Generate and Review Synthetic OMOP Patients](articles/synthetic-patient-workflow)
- [patientChat Model Benchmark Results](articles/patientchat-model-benchmark-results)
