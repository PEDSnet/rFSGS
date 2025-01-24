#' Get kidney transplant procedures and conditions, restricted to a cohort
#' if provided, from condition_occurrence and procedure_occurrence CDM tables
#'
#' @param cohort Cohort
#' @param kidney_transplant_codeset Kidney transplant codeset
#' @param age_ub_years Age upper bound
#' @param min_date_cutoff Date before which visits are excluded
#' @param max_date_cutoff Date after which visits are excluded
#'
#' @return Table with the following fields: person_id, site, concept_id,
#' source_value, concept_name, record_type (procedure/condition), and date
#' for kidney transplant procedures and conditions
#'
get_kidney_transplant_broad <- function(cohort,
                                        kidney_transplant_codeset,
                                        age_ub_years = 30L,
                                        min_date_cutoff = as.Date("2009-01-01"),
                                        max_date_cutoff = as.Date("2021-01-01")) {
  kidney_transplant_proc <- get_procedures(provided_cohort = cohort,
                                           procedure_codeset = kidney_transplant_codeset) %>%
    mutate(table_name = as.character("procedure"),
           approach = as.character("proc")) %>%
    select(
      person_id,
      site,
      occurrence_id = procedure_occurrence_id,
      concept_id = procedure_concept_id,
      source_value = procedure_source_value,
      concept_name = procedure_concept_name,
      table_name,
      approach,
      transplant_date = procedure_date,
      age_in_days = procedure_age_days
    )
  
  kidney_transplant_cond <- get_conditions(provided_cohort = cohort,
                                           condition_codeset = kidney_transplant_codeset) %>%
    mutate(table_name = as.character("condition"),
           approach = as.character("cond")) %>%
    select(
      person_id,
      site,
      occurrence_id = condition_occurrence_id,
      concept_id = condition_concept_id,
      source_value = condition_source_value,
      concept_name = condition_concept_name,
      table_name,
      approach,
      transplant_date = condition_start_date,
      age_in_days = condition_age_days
    )
  
  kidney_transplant_occurrences <- kidney_transplant_cond %>%
    dplyr::union(kidney_transplant_proc) %>% 
    filter(age_in_days < 365.25 * age_ub_years,
           transplant_date >= min_date_cutoff,
           transplant_date <= max_date_cutoff)
  
  return(kidney_transplant_occurrences)
  
}