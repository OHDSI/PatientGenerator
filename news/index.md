# Changelog

## PatientGenerator 0.2.4

- [`patientDesigner()`](https://mi-erasmusmc.github.io/PatientGenerator/reference/patientDesigner.md)
  now has an option parameter to select tables to hide.

- Pregnancy table support.

## PatientGenerator 0.2.3

- [`patientDesigner()`](https://mi-erasmusmc.github.io/PatientGenerator/reference/patientDesigner.md)
  now includes arameter `includeChat`.

## PatientGenerator 0.2.2

- `patients/conditions/procedures` creation of concept ids fix.

## PatientGenerator 0.2.1

- [`patientDesigner()`](https://mi-erasmusmc.github.io/PatientGenerator/reference/patientDesigner.md)
  now includes ‘Death’, a requirement of survival analysis.

## PatientGenerator 0.2.0

- D3 GUI works now with if there’s no empty end dates. The GUI will fill
  them out automatically.

- Added tooltip, a vertical line to track elongation, labels and now
  measurements are represented only with one triangle since there’s no
  end date for this type of record.

## PatientGenerator 0.1.4

CRAN release: 2026-05-04

- Prepared the package for CRAN submission.

- Updated package title and description to follow CRAN metadata style.

- `cran-comments.md` added to document local CRAN check results.

- Excluded local session and operating-system artifacts from source
  builds.

- Skipped network-dependent `TestGenerator` checks on CRAN.

## PatientGenerator 0.1.3

- Bug fixes for PatientDesigner.

- Documentation and site update.

## PatientGenerator 0.1.2

- Added .github actions for check-standard and sync-to-projects

## PatientGenerator 0.1.1

- Documentation update.

## PatientGenerator 0.1.0

- Initial working version.
