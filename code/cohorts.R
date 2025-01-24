#'
#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.
#'

#' Get patients with n_visit_req visits of visit_type in visit_type_codeset with
#' a care site or provider specialty in the spec_codeset,
#' with at least 3 visits separated by atleast days_sep days
#'
#' @param spec_codeset Codeset of provider and/or care_site specialty
#' @param visit_type_codeset Visit types
#' @param min_date_cutoff Date before which visits are excluded
#' @param max_date_cutoff Date after which visits are excluded
#' @param n_visit_req Number of visits required
#' @param days_sep Number of days to separate each visit
#' @param visit_tbl Visit CDM table
#' @param care_site_tbl Care Site CDM table
#' @param provider_tbl Provider CDM table
#'
#' @return Visits which meet requirements
#' 
get_spec_visits <- function(spec_codeset,
                            visit_type_codeset,
                            min_date_cutoff = as.Date("2009-01-01"),
                            max_date_cutoff = as.Date("2021-01-01"),
                            n_visit_req = 3L,
                            days_sep = 7L,
                            visit_tbl = cdm_tbl("visit_occurrence"),
                            care_site_tbl = cdm_tbl("care_site"),
                            provider_tbl = cdm_tbl("provider")) {
  
  visit_type_vctr <- visit_type_codeset %>% select(concept_id) %>% pull()
  
  specialty_visits <- get_spec_cdm_tbl(
    cdm_tbl = visit_tbl,
    provider_tbl = provider_tbl,
    care_site_tbl = care_site_tbl,
    spec_codeset = spec_codeset
  ) %>%
    filter(visit_start_date >= min_date_cutoff,
           visit_start_date <= max_date_cutoff,
           visit_concept_id %in% visit_type_vctr) %>%
    compute_new(name = "specialty_visits")
  
  distinct_visit_summary <-
    specialty_visits %>%
    group_by(person_id) %>%
    summarize(
      distinct_dates = n_distinct(visit_start_date),
      first_visit_date = min(visit_start_date, na.rm = TRUE),
      last_visit_date = max(visit_start_date, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(distinct_dates >= n_visit_req)
  
  distinct_visit_requirement_pats <-
    specialty_visits %>%
    inner_join(distinct_visit_summary, by = "person_id") %>%
    mutate(
      days_sep_from_first = visit_start_date - first_visit_date,
      days_sep_from_last = last_visit_date - visit_start_date
    ) %>%
    filter(
      visit_start_date != first_visit_date &
        visit_start_date != last_visit_date &
        (
          days_sep_from_first >= days_sep &
            days_sep_from_last >= days_sep
        )
    ) %>%
    distinct(person_id, distinct_dates)
  
  distinct_visit_requirement_visits <- specialty_visits %>%
    inner_join(distinct_visit_requirement_pats, by = "person_id") %>%
    select(person_id, visit_occurrence_id, distinct_dates) %>%
    inner_join(visit_tbl, by = c("person_id", "visit_occurrence_id")) %>%
    arrange(person_id, visit_start_date)
  
  return(distinct_visit_requirement_visits)
}


#' Get CDM table, restricted to rows for which the care_site or provider
#' has the specialty from the spec_codeset
#'
#' @param cdm_tbl CDM table (must be table with provider_id or care_site field)
#' @param provider_tbl Provider CDM table
#' @param care_site_tbl Care site CDM table
#' @param spec_codeset Codeset of provider and/or care_site specialty
#'
#' @return CDM tbl rows with specialty of interest
#' 
get_spec_cdm_tbl <- function(cdm_tbl = cdm_tbl("visit_occurrence"),
                             provider_tbl = cdm_tbl("provider"),
                             care_site_tbl = cdm_tbl("care_site"),
                             spec_codeset) {
  spec_codes <- spec_codeset %>%
    select(concept_id) %>%
    pull() %>% 
    as.integer()
  
  spec_provider_vctr <- provider_tbl %>%
    filter(specialty_concept_id %in% spec_codes) %>%
    distinct(provider_id) %>%
    pull() %>% 
    as.integer()
  
  spec_care_site_vctr <- care_site_tbl %>%
    filter(specialty_concept_id %in% spec_codes) %>%
    distinct(care_site_id) %>%
    pull() %>% 
    as.integer()
  
  provider_tbl_spec_cdm_tbl <- cdm_tbl %>%
    filter(provider_id %in% spec_provider_vctr)
  
  care_site_spec_cdm_tbl <- cdm_tbl %>%
    filter(care_site_id %in% spec_care_site_vctr)
  
  spec_cdm_tbl <-
    provider_tbl_spec_cdm_tbl %>%
    dplyr::union(care_site_spec_cdm_tbl)
  
  return(spec_cdm_tbl)
  
}

#' Get table of procedures in codeset, restricted to cohort if provided,
#' from CDM procedure table
#'
#' @param provided_cohort Cohort
#' @param procedure_codeset procedure codeset
#'
#' @return Table of procedures in codeset, restricted to
#' cohort if provided
#'
get_procedures <- function(provided_cohort,
                           procedure_codeset,
                           procedure_tbl = cdm_tbl("procedure_occurrence")) {
  procedure_vctr <-
    procedure_codeset %>% distinct(concept_id) %>% pull()
  
  if (missing(provided_cohort)) {
    cohort_birth_dates <- cdm_tbl("person") %>%
      select(person_id, birth_date)
  }
  else {
    cohort_birth_dates <- cdm_tbl("person") %>%
      inner_join(select(provided_cohort, person_id), by = "person_id") %>%
      select(person_id, birth_date)
  }
  
  procedures <- procedure_tbl %>%
    inner_join(select(cohort_birth_dates, person_id, birth_date), by = "person_id") %>%
    filter(procedure_concept_id %in% procedure_vctr) %>%
    mutate(procedure_age_days = procedure_date - birth_date)
  
  return(procedures)
}

#' Get table of conditions in codeset, restricted to cohort if provided,
#' from CDM condition table
#'
#' @param provided_cohort Cohort
#' @param condition_codeset Condition codeset
#'
#' @return Table of conditions in codeset, restricted to
#' cohort if provided
#'
get_conditions <- function(provided_cohort,
                           condition_codeset,
                           condition_tbl = cdm_tbl("condition_occurrence")) {
  condition_vctr <-
    condition_codeset %>% distinct(concept_id) %>% pull()
  
  if (missing(provided_cohort)) {
    cohort_birth_dates <- cdm_tbl("person") %>%
      select(person_id, birth_date)
  }
  else {
    cohort_birth_dates <- cdm_tbl("person") %>%
      inner_join(select(provided_cohort, person_id), by = "person_id") %>%
      select(person_id, birth_date)
  }
  
  conditions <- condition_tbl %>%
    inner_join(select(cohort_birth_dates, person_id, birth_date), by = "person_id") %>%
    filter(condition_concept_id %in% condition_vctr) %>%
    mutate(condition_age_days = condition_start_date - birth_date)
  
  return(conditions)
}