#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.

#' Get eligible condition info for patients with codes in the
#' inclusion codeset and no codes in the exclusion codeset
#'
#' @param incl_codeset_name
#' @param excl_codeset_name
#'
#' @return Table of condition info for patients with codes in the
#' inclusion codeset and no codes in the exclusion codeset
get_elig_conds_for_i_e_pats <- function(incl_codeset_name,
                                        excl_codeset_name) {
  pats_incl <- cdm_tbl('condition_occurrence') %>%
    inner_join(load_codeset(incl_codeset_name),
               by = c("condition_concept_id" = "concept_id")) %>%
    distinct(
      condition_occurrence_id,
      person_id,
      site,
      visit_occurrence_id,
      condition_concept_id,
      condition_concept_name,
      condition_type_concept_id,
      condition_start_date,
      condition_source_value
    ) %>%
    compute_new()
  
  pats_excl <- cdm_tbl('condition_occurrence') %>%
    inner_join(load_codeset(excl_codeset_name),
               by = c("condition_concept_id" = "concept_id")) %>%
    distinct(person_id) %>%
    compute_new()
  
  elig_conds_for_i_e_pats <- pats_incl %>%
    anti_join(pats_excl, by = "person_id") %>%
    compute_new()
  
  return(elig_conds_for_i_e_pats)
  
}

#' Get information about both provider level and care site level
#' specialty for cohort
#'
#' @param elig_conds_for_i_e_pats Patients with any of the inclusion criteria and none of the exclusion criteria
#'
#' @return All visit information for patients in cohort with specialty information from both
#' care_site and provider
get_visits_w_specialty <- function(elig_conds_for_i_e_pats) {
  visits_for_i_e_pats <-
    cdm_tbl('visit_occurrence') %>%
    inner_join(select(elig_conds_for_i_e_pats, person_id), by = "person_id") %>%
    distinct(
      person_id,
      site,
      visit_occurrence_id,
      visit_start_date,
      visit_concept_id,
      provider_id,
      care_site_id
    ) %>%
    compute_new(indexes = list('provider_id', 'care_site_id'))
  
  visits_provider_spec <-
    visits_for_i_e_pats %>%
    inner_join(select(cdm_tbl('provider'), provider_id, specialty_concept_id),
               by = "provider_id") %>%
    distinct(
      person_id,
      site,
      visit_occurrence_id,
      visit_start_date,
      specialty_concept_id,
      visit_concept_id
    ) %>%
    mutate(spec_flag = as.character("provider")) %>%
    compute_new()
  
  visits_care_site_spec <-
    visits_for_i_e_pats %>%
    inner_join(select(cdm_tbl('care_site'), care_site_id, specialty_concept_id),
               by = "care_site_id") %>%
    distinct(
      person_id,
      site,
      visit_occurrence_id,
      visit_start_date,
      specialty_concept_id,
      visit_concept_id
    ) %>%
    mutate(spec_flag = as.character("care_site")) %>%
    compute_new()
  
  visits <- visits_provider_spec %>%
    dplyr::union(visits_care_site_spec) %>%
    distinct(
      person_id,
      site,
      visit_occurrence_id,
      visit_start_date,
      specialty_concept_id,
      spec_flag,
      visit_concept_id
    ) %>%
    compute_new(indexes = list('specialty_concept_id', 'visit_concept_id'))
  
  return(visits)
}

#' Get information about both provider level and care site level
#' specialty for cohort
#'
#' @param elig_conds_for_i_e_pats Patients with any of the inclusion criteria and none of the exclusion criteria
#'
#' @return All visit information for patients in cohort with specialty information from both
#' care_site and provider
get_specialty_for_visit_table <- function(cohort,
                                          visit_table) {
  visits_for_cohort <-
    visit_table %>%
    inner_join(select(cohort, person_id), by = "person_id",
               copy = !same_src(visit_table, cohort)) %>%
    distinct(
      person_id,
      site,
      visit_occurrence_id,
      visit_start_date,
      visit_concept_id,
      visit_concept_name,
      provider_id,
      care_site_id
    )
  
  visits_provider_spec <-
    visits_for_cohort %>%
    inner_join(select(cdm_tbl('provider'), provider_id, specialty_concept_id),
               by = "provider_id") %>%
    distinct(
      person_id,
      site,
      visit_occurrence_id,
      visit_start_date,
      specialty_concept_id,
      visit_concept_id,
      visit_concept_name
    ) %>%
    mutate(spec_flag = as.character("provider"))
  
  visits_care_site_spec <-
    visits_for_cohort %>%
    inner_join(select(cdm_tbl('care_site'), care_site_id, specialty_concept_id),
               by = "care_site_id") %>%
    distinct(
      person_id,
      site,
      visit_occurrence_id,
      visit_start_date,
      specialty_concept_id,
      visit_concept_id,
      visit_concept_name
    ) %>%
    mutate(spec_flag = as.character("care_site"))
  
  visits_spec <- visits_provider_spec %>%
    dplyr::union(visits_care_site_spec) %>%
    distinct(
      person_id,
      site,
      visit_occurrence_id,
      visit_start_date,
      specialty_concept_id,
      spec_flag,
      visit_concept_id,
      visit_concept_name
    )
  
  return(visits_spec)
}

#' For a cohort, get eligible visits
#'
#' @param incl_provider_spec_name Name of provider specialty codeset
#' @param incl_visit_name Name of visit inclusion codeset
#' @param cohort Cohort for which to return the eligible visits
#'
#' @return Visits which meet the specialty concept criteria and visit criteria
#' and for which the patient had more than one visit
get_elig_visits_for_cohort <- function(incl_spec_codeset,
                                       incl_visit_codeset,
                                       cohort) {
  visits <- get_visits_w_specialty(cohort) %>%
    compute_new()
  
  visits_ip_w_spec <- visits %>%
    inner_join(select(incl_spec_codeset, concept_id),
               by = c("specialty_concept_id" = "concept_id")) %>%
    inner_join(select(incl_visit_codeset, concept_id),
               by = c("visit_concept_id" = "concept_id")) %>%
    distinct(
      person_id,
      site,
      visit_occurrence_id,
      specialty_concept_id,
      spec_flag,
      visit_concept_id
    ) %>%
    compute_new()
  
  two_or_more_visits <- visits_ip_w_spec %>%
    group_by(person_id) %>%
    summarise(n_visits = n_distinct(visit_occurrence_id)) %>% # is this sufficient?
    ungroup() %>%
    filter(n_visits >= 2) %>% # Ensure 2 or more visits
    compute_new()
  
  elig_visits <- visits_ip_w_spec %>%
    inner_join(two_or_more_visits, by = "person_id") %>% # Ensure 2 or more visits
    compute_new()
  
  return(elig_visits)
  
}

#'  Get condition table with flags for patients with inclusion conditions who do not have
#'  conditions of exclusion AND who have eligible visits (more than one visit with a nephrology
#'  or rheumotology provider)
#'
#' @param elig_conds_for_i_e_pats Patients with any of the inclusion criteria and none of the exclusion criteria
#' @param incl_provider_spec_name Name of provider specialty codeset
#' @param incl_visit_name Name of visit type codeset
#'
#' @return Get condition table with flags for patients with inclusion conditions who do not have
#'  conditions of exclusion AND who have eligible visits (more than one visit with a nephrology
#'  or rheumotology provider)
get_elig_conds_for_i_e_pats_w_elig_visits <-
  function(incl_spec_codeset,
           incl_visit_codeset,
           elig_conds_for_i_e_pats) {
    
    i_e_pats <- elig_conds_for_i_e_pats %>% 
      distinct(person_id) %>% 
      compute_new()
    
    elig_visits <-
      get_elig_visits_for_cohort(
        incl_spec_codeset = incl_spec_codeset,
        incl_visit_codeset = incl_visit_codeset,
        cohort = i_e_pats
      ) %>% 
      compute_new()
    
    elig_visits_pats <- elig_visits %>% 
      distinct(person_id) %>% 
      compute_new()
    
    elig_conds_for_i_e_pats_w_elig_visits <-
      elig_conds_for_i_e_pats %>%
      inner_join(select(elig_visits, person_id),
                 by = "person_id") %>%
      distinct(
        condition_occurrence_id,
        person_id,
        site,
        visit_occurrence_id,
        condition_concept_id,
        condition_concept_name,
        condition_type_concept_id,
        condition_start_date,
        condition_source_value
      ) %>%
      compute_new()
    
    return(elig_conds_for_i_e_pats_w_elig_visits)
    
  }

#' Apply "ALGORITHM A" to eligible patients cohort
#' At least 2 codes separated by >= 60 days
#'
#' @param elig_conds_for_i_e_pats_w_elig_visits
#' @param cohort_name
#'
#' @return tbl with cohort according to "ALGORITHM A"
get_alg_a <- function(elig_conds_for_i_e_pats_w_elig_visits,
                      cohort_name) {
  alg_a_elig <-
    elig_conds_for_i_e_pats_w_elig_visits %>%
    distinct(person_id,
             site,
             condition_start_date) %>%
    group_by(person_id,
             site) %>%
    arrange(condition_start_date, .by_group = TRUE) %>%
    mutate(
      first_date = min(condition_start_date),
      last_date = max(condition_start_date),
      days_sep = last_date - first_date
    ) %>%
    ungroup() %>%
    compute_new()
  
  alg_a <-
    alg_a_elig %>%
    filter(days_sep >= 60) %>%
    select(person_id,
           site,
           condition_start_date) %>%
    distinct() %>%
    compute_new()
  
  return(alg_a)
  
}


#' Apply "ALGORITHM B" to initial cohort
#'
#' At least 3 codes separated by >= 30 days each
#'
#' First, filter for visits which are not the first or
#' last visit date.
#' Next, from this filtered data, filter visits for which the
#' separation in days is  >= 30 days from the first visit
#' AND >= 30 days from the last visit (can be same visit)
#'
#' @param elig_conds_for_i_e_pats_w_elig_visits
#' @param cohort_name
#'
#' @return tbl with cohort according to "ALGORITHM B"
get_alg_b <- function(elig_conds_for_i_e_pats_w_elig_visits,
                      cohort_name) {
  alg_b_elig <-
    elig_conds_for_i_e_pats_w_elig_visits %>%
    distinct(person_id,
             site,
             condition_start_date) %>%
    group_by(person_id,
             site) %>%
    arrange(condition_start_date, .by_group = TRUE) %>%
    mutate(
      first_date = min(condition_start_date),
      last_date = max(condition_start_date),
      days_sep_from_first = condition_start_date - first_date,
      days_sep_from_last = last_date - condition_start_date
    ) %>%
    ungroup() %>%
    compute_new()
  
  alg_b <-
    alg_b_elig %>%
    filter(
      condition_start_date != first_date &
        condition_start_date != last_date &
        (days_sep_from_first >= 30 &
           days_sep_from_last >= 30)
    ) %>%
    select(person_id,
           site,
           condition_start_date) %>%
    distinct() %>%
    compute_new()
  
  return(alg_b)
}

#' Get patients with kidney biopsy procedure from a cohort
#'
#' @param cohort Cohort
#' @param procedure_table Procedure table
#'
#' 2109566L: Renal biopsy; percutaneous, by trocar or needle
#' 2003588L: Closed percutaneous needle biopsy of kidney
#' 35608187L: Closed renal biopsy
#' 36717689L: Evaluation of kidney biopsy specimen
#'
#' The following code must be accompanied by search for "renal" or "kidney"
#' in the source value:
#' 2211783L: Ultrasonic guidance for needle placement (eg, biopsy, aspiration,
#' injection, localization device), imaging supervision and interpretation
#'
#' @return Patients from cohort with kidney biopsy procedure
#'
get_patients_with_kidney_biopsy <- function(cohort,
                                            procedure_table = cdm_tbl("procedure_occurrence")) {
  procedure_table %>%
    inner_join(select(cohort, person_id), by = "person_id") %>%
    filter(
      procedure_concept_id %in% c(2109566L, 2003588L, 35608187L, 36717689L) |
        (procedure_concept_id == 2211783L &
           (
             str_detect(tolower(procedure_source_value), "renal") |
               str_detect(tolower(procedure_source_value), "kidney")
           ))
    ) %>%
    select(
      procedure_occurrence_id,
      person_id,
      site,
      procedure_date,
      procedure_concept_id,
      procedure_concept_name,
      procedure_source_value
    )
}

#' Get patients with a kidney-related and SLE-related diagnosis on the same date
#' The purpose is to find patients with lupus nephritis who are not
#' receiving a lupus nephritis code.
#'
#' @param condition_table Condition table
#' @param codeset Codeset with 2 additional columns ("sle" and "kidney_related")
#' which flag whether conditions are SLE or kidney-based
#'
#' @return
#'
get_kidney_rel_and_sle_same_date <-
  function(condition_table = cdm_tbl("condition_occurrence"),
           codeset) {
    glomerular_and_sle_condition_table <- condition_table %>%
      inner_join(codeset,
                 by = c("condition_concept_id" = "concept_id")) %>%
      distinct(
        condition_occurrence_id,
        person_id,
        site,
        condition_start_date,
        condition_concept_id,
        condition_concept_name,
        condition_source_concept_id,
        condition_source_concept_name,
        condition_source_value,
        visit_occurrence_id,
        kidney_related,
        sle
      ) %>%
      compute_new()
    
    glomerular_and_sle_condition_table %>%
      group_by(person_id, condition_start_date) %>% # condition_source_value
      mutate(
        any_kidney_related = if_else(any(kidney_related == 1L), 1L, 0L),
        any_sle = if_else(any(sle == 1L), 1L, 0L),
        kidney_related_and_sle = if_else(any_kidney_related == 1L &
                                           any_sle == 1L, 1L, 0L)
      ) %>%
      ungroup() %>%
      filter(kidney_related_and_sle == 1L) %>%
      compute_new()
  }

#' For a given cohort, get condition table information where a patient received a a kidney-related
#' and SLE-related diagnosis on the same date
#'
#' @param cohort Cohort
#' @param codeset Codeset with 2 additional columns ("sle" and "kidney_related")
#' which flag whether conditions are SLE or kidney-based
#'
#' @return Condition table information where a patient received a a kidney-related
#' and SLE-related diagnosis on the same date
#'
get_conds_w_kidney_rel_and_sle_same_date <- function(cohort,
                                                     codeset) {
  cohort <- cohort %>%
    distinct(person_id) %>%
    compute_new()
  
  cohort_condition_table <- cdm_tbl("condition_occurrence") %>%
    inner_join(select(cohort, person_id), by = "person_id") %>%
    compute_new()
  
  cohort_condition_table %>%
    get_kidney_rel_and_sle_same_date(codeset) %>%
    compute_new()
}

#' Get patients in cohort with any diagnosis from codeset in provided
#' condition table
#'
#' @param cohort Cohort of patients 
#' @param condition_table Condition table
#' @param codeset Codeset of diagnoses
#'
#' @return Patients in cohort with any diagnosis from codeset in provided
#' condition table
#' 
get_patients_with_any_dx <- function(cohort,
                                     condition_table =
                                       cdm_tbl("condition_occurrence"),
                                     codeset) {
  cohort_dx_condition_table <- condition_table %>%
    inner_join(select(cohort, person_id), by = "person_id") %>%
    inner_join(codeset, by = c("condition_concept_id" = "concept_id")) %>%
    compute_new()
  
  patients_with_any_dx <- cohort_dx_condition_table %>%
    distinct(person_id) %>%
    add_site()
}

#' Get any patient in PEDSnet with two or more eligible visits
#' where eligible visits are in-person visits where either the provider or
#' the care site is associated with a speciality from the provided specialty
#' codeset
#'
#' @param incl_spec_codeset Specialty codeset
#' @param incl_visit_codeset Visit type codeset
#'
#' @return Patients with two or more eligible visits
#'
get_two_or_more_elig_visits <-
  function(incl_spec_codeset,
           incl_visit_codeset) {
    elig_provider <- cdm_tbl("provider") %>%
      select(provider_id, specialty_concept_id) %>%
      inner_join(incl_spec_codeset,
                 by = c("specialty_concept_id" = "concept_id")) %>%
      compute_new()
    
    elig_visits_provider <- cdm_tbl("visit_occurrence") %>%
      inner_join(incl_visit_codeset,
                 by = c("visit_concept_id" = "concept_id")) %>%
      select(
        person_id,
        visit_start_date,
        visit_occurrence_id,
        visit_concept_id,
        provider_id,
        care_site_id
      ) %>%
      inner_join(elig_provider, by = c("provider_id")) %>%
      mutate(spec_flag = as.character("provider")) %>%
      compute_new()
    
    elig_care_sites <- cdm_tbl("care_site") %>%
      select(care_site_id, specialty_concept_id) %>%
      inner_join(incl_spec_codeset,
                 by = c("specialty_concept_id" = "concept_id")) %>%
      compute_new()
    
    elig_visits_care_site <- cdm_tbl("visit_occurrence") %>%
      inner_join(incl_visit_codeset,
                 by = c("visit_concept_id" = "concept_id")) %>%
      select(
        person_id,
        visit_start_date,
        visit_occurrence_id,
        visit_concept_id,
        provider_id,
        care_site_id
      ) %>%
      inner_join(elig_care_sites, by = c("care_site_id")) %>%
      mutate(spec_flag = as.character("care_site")) %>%
      compute_new()
    
    combined_visits <- elig_visits_provider %>%
      dplyr::union(elig_visits_care_site) %>%
      select(
        person_id,
        visit_start_date,
        visit_occurrence_id,
        visit_concept_id,
        provider_id,
        care_site_id,
        spec_flag
      ) %>%
      compute_new()
    
    patients_w_two_or_more_elig_visits <- combined_visits %>%
      group_by(person_id) %>%
      summarise(n_visits = n_distinct(visit_occurrence_id)) %>% # date?
      ungroup() %>%
      filter(n_visits >= 2) %>%
      compute_new()
    
    two_or_more_elig_visits <- combined_visits %>%
      inner_join(patients_w_two_or_more_elig_visits, by = "person_id") %>%
      compute_new()
    
    return(two_or_more_elig_visits)
  }

#' Get patients within cohort with 60 or more days follow-up (no restrictions
#' on visit type)
#'
#' @param cohort Cohort
#'
#' @return Patients with >= 60 days follow-up
#' 
get_patients_w_60_or_more_days_followup <- function(cohort) {
  min_max_visits <- cdm_tbl("visit_occurrence") %>%
    select(person_id, visit_occurrence_id, visit_start_date) %>%
    inner_join(cohort, by = "person_id") %>%
    group_by(person_id) %>%
    summarise(max_date = max(visit_start_date),
              min_date = min(visit_start_date)) %>%
    ungroup() %>%
    compute_new()
  
  patients_w_60_or_more_days_followup <- min_max_visits %>%
    mutate(days_sep = max_date - min_date) %>%
    filter(days_sep >= 60) %>%
    compute_new()
  
  return(patients_w_60_or_more_days_followup)
  
}

#' Get patients in cohort with one or more exposure to any drug in the
#' provided drug codeset
#'
#' @param cohort Cohort
#' @param drug_codeset Drug codeset
#'
#' @return Patients with >= 1 drug exposure
#' 
get_patients_w_drug_exposure <- function(cohort,
                                         drug_codeset) {
  any_patients_w_drug_exposure <- cdm_tbl("drug_exposure") %>%
    inner_join(drug_codeset,
               by = c("drug_concept_id" = "concept_id")) %>%
    distinct(person_id) %>% 
    compute_new()
  
  cohort_patients_w_drug_exposure <- cohort %>%
    inner_join(any_patients_w_drug_exposure, by = "person_id") %>% 
    distinct(person_id) %>%
    compute_new()
}



#' Get SLE cohort
#'
#' 2 or more in-person visits with a nephrology or rheumatology provider or care
#' site, AND
#' 60 or more days follow-up, AND
#' No occurrence of neonatal lupus erythematosus, AND
#' 1 or more hydroxychloroquine drug exposure at any time, AND
#' 3 or more SLE inclusion codes associated with any visit type each separated 30
#' or more days, OR
#' 1 or more SLE inclusion code associated with any visit type at any time AND 1
#' or more kidney biopsy procedure code at any time
#'
#' @param elig_conds_sle_i_e_pats_w_elig_visits SLE condition rows for patients
#' who meet other criteria
#' @param drug_codeset Drug codeset
#'
#' @return Patients who meet SLE cohort criteria
#' 
get_sle_cohort <- function(elig_conds_sle_i_e_pats_w_elig_visits,
                           drug_codeset) {
  sle_alg_b <- get_alg_b(elig_conds_sle_i_e_pats_w_elig_visits) %>%
    compute_new()
  
  sle_i_e_pats_w_elig_visits <- elig_conds_sle_i_e_pats_w_elig_visits %>% 
    distinct(person_id)
  
  kidney_biopsy <-
    get_patients_with_kidney_biopsy(sle_i_e_pats_w_elig_visits) %>%
    get_patients_w_60_or_more_days_followup() %>% 
    compute_new()
  
  sle_cohort <- sle_alg_b %>%
    dplyr::union(kidney_biopsy) %>%
    compute_new() %>%
    get_patients_w_drug_exposure(drug_codeset) %>%
    add_site() %>% 
    compute_new()
  
}

#' Get patients who meet the SLE cohort definition with the exclusion of
#' patients who enter the cohort through kidney biopsy
#'
#' @param elig_conds_sle_i_e_pats_w_elig_visits SLE condition rows for patients
#' who meet other criteria
#' @param drug_codeset Drug codeset
#'
#' @return Patients who meet the SLE cohort definition with the exclusion of
#' patients who enter the cohort through kidney biopsy
get_sle_cohort_non_biopsy <-
  function(elig_conds_sle_i_e_pats_w_elig_visits,
           drug_codeset) {
    sle_alg_b <- get_alg_b(elig_conds_sle_i_e_pats_w_elig_visits) %>%
      compute_new()
    
    sle_i_e_pats_w_elig_visits <-
      elig_conds_sle_i_e_pats_w_elig_visits %>%
      distinct(person_id)
    
    sle_cohort <- sle_alg_b %>%
      compute_new() %>%
      get_patients_w_drug_exposure(drug_codeset) %>%
      add_site() %>%
      compute_new()
  }



#' Get LN cohort
#'
#' 2 or more in-person visits with a nephrology or rheumatology provider or care
#' site, AND
#' 60 or more days follow-up, AND
#' No occurrence of neonatal lupus erythematosus, AND
#' 1 or more hydroxychloroquine drug exposure at any time, AND
#' 3 or more (lupus nephritis inclusion codes associated with any visit type) or
#' (SLE code and glomerular/kidney disease on the same date associated with any
#' visit type) each separated 30 or more days, OR
#' 1 or more SLE inclusion code associated with any visit type at any
#' time AND 1 or more kidney biopsy procedure code at any time
#'
#' @param elig_conds LN condition rows for patients
#' who meet criteria
#' @param patients_w_one_or_more_sle Patients with 1 or more SLE code
#'  @param drug_codeset Drug codeset
#'
#' @return Patients who meet LN cohort criteria
#' 
get_ln_cohort <- function(elig_conds,
                          patients_w_one_or_more_sle, 
                          drug_codeset){
  
  ln_alg_b <- get_alg_b(elig_conds) %>%
    compute_new()
  
  kidney_biopsy <-
    get_patients_with_kidney_biopsy(patients_w_one_or_more_sle) %>%
    get_patients_w_60_or_more_days_followup() %>% 
    compute_new()
  
  ln_cohort <- ln_alg_b %>%
    dplyr::union(kidney_biopsy) %>%
    compute_new() %>%
    get_patients_w_drug_exposure(drug_codeset) %>%
    distinct(person_id) %>% 
    add_site() %>% 
    compute_new()
}

#' Get patients who meet the LN cohort definition with the exclusion of
#' patients who enter the cohort through kidney biopsy
#'
#' @param elig_conds_ln_i_e_pats_w_elig_visits LN condition rows for patients
#' who meet other criteria
#' @param drug_codeset Drug codeset
#'
#' @return Patients who meet the LN cohort definition with the exclusion of
#' patients who enter the cohort through kidney biopsy
get_ln_cohort_non_biopsy <- function(elig_conds,
                                        drug_codeset) {
  ln_alg_b <- get_alg_b(elig_conds) %>%
    compute_new()
  
  ln_cohort <- ln_alg_b %>%
    compute_new() %>%
    get_patients_w_drug_exposure(drug_codeset) %>%
    distinct(person_id) %>%
    add_site() %>%
    compute_new()
}