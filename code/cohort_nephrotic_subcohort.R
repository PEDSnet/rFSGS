#' Get nephrotic subcohort
#' 
#' Get members of GLEAN cohort who have more nephrotic syndrome diagnoses than
#' nephritis diagnoses
#'
#' @param cohort GLEAN cohort
#' @param nephrotic_codeset Nephrotic syndrome codeset
#' @param nephritic_codeset Nephritis codeset
#'
#' @return Patients in GLEAN cohort who have more nephrotic syndrome diagnoses
#'  than nephritis diagnoses
#' 
get_nephrotic_subcohort <- function(cohort,
                                    condition_tbl = cdm_tbl("condition_occurrence"),
                                    nephrotic_codeset,
                                    nephritic_codeset) {
  conditions_glomerular_cohort <- condition_tbl %>%
    inner_join(select(cohort, person_id), by = "person_id")
  
  cohort_nephrotic_dx <- conditions_glomerular_cohort %>%
    inner_join(select(nephrotic_codeset, concept_id),
               by = c("condition_concept_id" = "concept_id")) %>%
    group_by(person_id) %>%
    summarise(n_nephrotic_dx = n_distinct(condition_start_date)) %>%
    ungroup()
  
  cohort_nephritic_dx <- conditions_glomerular_cohort %>%
    inner_join(
      select(nephritic_codeset, concept_id),
      by = c("condition_concept_id" = "concept_id")
    ) %>%
    group_by(person_id) %>%
    summarise(n_nephritic_dx = n_distinct(condition_start_date)) %>%
    ungroup()
  
  nephrotic_nephritic_counts <- cohort %>%
    select(person_id) %>%
    left_join(cohort_nephrotic_dx, by = "person_id") %>%
    left_join(cohort_nephritic_dx, by = "person_id") %>%
    mutate(
      n_nephrotic_dx = if_else(is.na(n_nephrotic_dx), 0, n_nephrotic_dx),
      n_nephritic_dx = if_else(is.na(n_nephritic_dx), 0, n_nephritic_dx)
    )
  
  nephrotic_subcohort <- nephrotic_nephritic_counts %>% 
    filter(n_nephrotic_dx > n_nephritic_dx)
  
  return(nephrotic_subcohort)
  
}