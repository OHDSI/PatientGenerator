retrieveCodelist <- function(concept_label = "Stage 1", domain = "Measurement") {
  codelist <- readRDS(file = system.file("concept_sets", "ovarian_cancer_codelist.rds", package = "PatientGenerator"))
  checkmate::assertCharacter(concept_label)
  checkmate::assertCharacter(domain)
  domain_names <- codelist |>
    dplyr::pull(domain_id) |>
    unique()
  if (!domain %in% domain_names) {
    warning("domain not found in codelist")
  }
  result <- codelist %>%
    filter(stringr::str_detect(concept_name, stringr::regex(concept_label, ignore_case = TRUE))) |>
    dplyr::filter(domain_id == domain) |>
    jsonlite::toJSON(dataframe = "rows",
                     auto_unbox = TRUE)
  return(result)
}

retrieveCodelistTool <- ellmer::tool(
  fun = retrieveCodelist,
  description = "Retrieves data for concept_id search by concept_name and domain_id",
  name = "retrieveCodelist",
  arguments = list(
    concept_label = ellmer::type_string(description = "A regex to look up for a concept_name; the function uses stringr::regex to filter the concept_name"),
    domain = ellmer::type_string(description = "One word typically a domain from the OMOP-CDM, currently only available now: 'Drug', 'Condition', 'Measurement', 'Observation', 'Procedure'")
  )
)
