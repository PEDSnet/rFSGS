# Top-level code for execution of data request

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(readr))

# Required for execution using Rscript
suppressPackageStartupMessages(library(methods))

#' Set up the execution environment
#'
#' The .load() function sources the R files needed to execute the query
#' and sets up the execution environment.  In particular, all of the base
#' framework files, as well as files inthe code_dir with names matching
#' `cohort_*.R` or `analyze_*.R` will be sourced.
#'
#' This function is usually run automatically when the `run.R` file is sourced
#' to execute the request.  It may also be executed manually during an
#' interactive session to re-source changed code or to re-establish a connection
#' to the database.
#'
#' **N.B.** You will almost never have to edit this function.
#'
#' @param here The name of the top-level directory for the request.  The default
#'   is `config('base_dir')` if the config function has been set up, or the
#'   global variable `base_dir` if not.
#'
#' @return The value of `here`.
#' @md
.load <- function(here = ifelse(typeof(get('config')) == 'closure',
                                config('base_dir'), base_dir)) {
    source(file.path(here, 'code', 'config.R'))
    source(file.path(here, 'code', 'req_info.R'))
    source(config('site_info'))
    source(file.path(here, config('subdirs')$code_dir, 'setup.R'))
    source(file.path(here, config('subdirs')$code_dir, 'codesets.R'))
    for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                        'util_.+\\.R', full.names = TRUE))
      source(fn)
    for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                          'cohort_.+\\.R', full.names = TRUE))
      source(fn)
    for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                        'analyze_.+\\.R', full.names = TRUE))
      source(fn)
    source(file.path(here, config('subdirs')$code_dir, 'cohorts.R'))

    .env_setup()

    for (def in c('retain_intermediates', 'results_schema')) {
      if (is.na(config(def)))
        config(def, config(paste0('default_', def)))
    }

    here
}

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' This function is also typically executed automatically, but is separated from
#' the setup done in [.load()] to facilitate direct invocation during
#' development and debugging.
#'
#' @param base_dir The name of the top-level directory for the request.  The default
#'   is `config('base_dir')`, which should always be valid after execution of
#'   [.load()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run  <- function(base_dir = config('base_dir')) {

  message("Starting execution with framework version ",
          config("framework_version"))

  # Set up log file
  fsgs_log <- init_sum(cohort = "start", persons = 0)

  # NEPHROLOGY VISITS ----
  message("Identify cohort with 3 nephrology visits at least 7 days apart")
  # Create table of visits for which either the provider or the care site
  # has a nephrology specialty AND for which the patient has at least 3
  # visits separated by at least 7 days
  nephrology_visits <-
    get_spec_visits(
      spec_codeset = load_codeset("nephrology_specialty"),
      visit_type_codeset = load_codeset("in_person"),
      n_visit_req = 3,
      days_sep = 7
    )

  nephrology_visits %>% output_tbl("nephrology_visits",
                                   indexes = list("person_id", "visit_occurrence_id"))

  # Create cohort table for nephrology visits
  nephrology_visit_cohort <- nephrology_visits %>%
    distinct(person_id, site) %>%
    compute_new(name = "nephrology_visit_cohort")

  nephrology_visit_cohort %>% output_tbl("nephrology_visit_cohort",
                                         indexes = list("person_id"))

  fsgs_log <- append_sum(cohort = "nephrology_visit_cohort",
                         persons = nephrology_visit_cohort %>% distinct_ct())


  # NEPHROLOGY VISITS WITH EXPANDED NOTION OF IN-PERSON VISIT ----
  message("Identify cohort with 3 nephrology visits at least 7 days apart")
  # Create table of visits for which either the provider or the care site
  # has a nephrology specialty AND for which the patient has at least 3
  # visits separated by at least 7 days
  # In-person codeset includes additional codes, incl. telemedicine
  nephrology_visits_exp <-
    get_spec_visits(
      spec_codeset = load_codeset("nephrology_specialty"),
      visit_type_codeset = load_codeset("in_person_exp"),
      n_visit_req = 3,
      days_sep = 7
    )

  nephrology_visits_exp %>% output_tbl("nephrology_visits_exp",
                                   indexes = list("person_id", "visit_occurrence_id"))

  # Create cohort table for nephrology visits
  nephrology_visit_cohort_exp <- nephrology_visits_exp %>%
    distinct(person_id, site) %>%
    compute_new(name = "nephrology_visit_cohort_exp")

  nephrology_visit_cohort_exp %>% output_tbl("nephrology_visit_cohort_exp",
                                         indexes = list("person_id"))

  fsgs_log <- append_sum(cohort = "nephrology_visit_cohort_exp",
                         persons = nephrology_visit_cohort_exp %>% distinct_ct())

  # GLOMERULAR DISEASE COHORT ---
  message("Identify glomerular disease cohort")

  # Glomerular disease codeset
  glomerular_disease <-
    load_codeset("glomerular_disease", col_types = "icccicii")
  glomerular_disease_other_code_req_vctr <-
    glomerular_disease %>% filter(other_code_req == 1) %>% select(concept_id) %>% pull()
  glomerular_disease_neph_req_vctr <-
    glomerular_disease %>% filter(neph_req == 1) %>% select(concept_id) %>% pull()

  # Identify glomerular disease cohort
  # 2 broad pathways into the cohort:
  # -- 2 or more glomerular inclusion diagnoses on different dates
  # -- 1 or more glomerular inclusion diagnosis AND 1 or more kidney biopsy which is not post-transplant
  # 2 glomerular inclusion diagnosis groups had special requirements:
  # -- "other_code_req" inclusion diagnoses alone are not sufficient for a patient to meet the computational phenotype:
  # these codes are only included if they have 1 or more other glomerular inclusion diagnoses
  # (i.e. NOT from "other_code_req" list) on a different date or if they have 1 or more kidney biopsy which is not post-transplant
  # -- "neph_req" diagnoses are only included for the algorithm if associated with a nephrology visit
  glomerular_disease_cohort <-
    get_glean_cohort(
      kidney_transplant_proc_codeset = load_codeset("kidney_transplant_proc"),
      kidney_biopsy_proc_codeset = load_codeset("kidney_biopsy_proc"),
      biopsy_proc_codeset = load_codeset("biopsy_proc"),
      glomerular_disease_codeset = glomerular_disease,
      other_code_req_vctr = glomerular_disease_other_code_req_vctr,
      neph_req_vctr = glomerular_disease_neph_req_vctr,
      nephrology_spec_codeset  = load_codeset("nephrology_specialty")
    )

  glomerular_disease_cohort %>% output_tbl("glomerular_disease_cohort",
                                           indexes = list("person_id"))

  fsgs_log <- append_sum(cohort = "glomerular_disease_cohort",
                         persons = glomerular_disease_cohort %>% distinct_ct())

  # Identify glomerular disease cohort who meet nephrology visit requirement
  glomerular_disease_cohort_w_neph <- glomerular_disease_cohort %>%
    inner_join(select(nephrology_visit_cohort, person_id), by = "person_id")

  glomerular_disease_cohort_w_neph %>% output_tbl("glomerular_disease_cohort_w_neph",
                                                  indexes = list("person_id"))

  fsgs_log <-
    append_sum(cohort = "glomerular_disease_cohort_w_neph",
               persons = glomerular_disease_cohort_w_neph %>% distinct_ct())

  # NEPHROTIC SUBCOHORT ----
  message("Nephrotic subcohort")

  # Among glomerular disease cohort, identify the nephrotic subcohort -
  # patients with more inclusion diagnoses categorized as nephrotic than
  # inclusion diagnoses marked as nephritic
  nephrotic_subcohort <-
    get_nephrotic_subcohort(
      cohort = glomerular_disease_cohort,
      nephrotic_codeset = glomerular_disease %>% filter(codeset_category_code == 1),
      nephritic_codeset = glomerular_disease %>% filter(codeset_category_code == 2)
    )

  nephrotic_subcohort %>% output_tbl("nephrotic_subcohort",
                                     indexes = list("person_id"))

  fsgs_log <- append_sum(cohort = "nephrotic_subcohort",
                         persons = nephrotic_subcohort %>% distinct_ct())

  # Identify nephrotic subcohort who meet nephrology visit requirement
  nephrotic_subcohort_w_neph <- nephrotic_subcohort %>%
    inner_join(select(nephrology_visit_cohort, person_id), by = "person_id")

  nephrotic_subcohort_w_neph %>% output_tbl("nephrotic_subcohort_w_neph",
                                            indexes = list("person_id"))

  fsgs_log <- append_sum(cohort = "nephrotic_subcohort_w_neph",
                         persons = nephrotic_subcohort_w_neph %>% distinct_ct())

  # KIDNEY TRANSPLANT ----
  message("Identify nephrotic subcohort with kidney transplant")
  # Identify kidney transplant for nephrotic subcohort using broad approach which incorporates both
  # procedure and condition codes (not that within the glomerular disease cohort
  # definition, only transplant procedure codes are used because date is critical)
  nephrotic_kt <-
    get_kidney_transplant_broad(
      cohort = nephrotic_subcohort,
      kidney_transplant_codeset = load_codeset("kidney_transplant_broad", col_types = "icccc")
    )

  nephrotic_kt %>% output_tbl("nephrotic_kt",
                              indexes = list("person_id", "occurrence_id"))

  # Identify cohort of patients in nephrotic subcohort with kidney transplant
  nephrotic_kt_cohort <-
    nephrotic_kt %>%
    distinct(person_id, site)

  nephrotic_kt_cohort %>%
    output_tbl("nephrotic_kt_cohort",
               indexes = list("person_id"))

  fsgs_log <- append_sum(cohort = "nephrotic_kt",
                         persons = results_tbl("nephrotic_kt") %>% distinct_ct())

  # Identify nephrotic subcohort with kidney transplant who meet nephrology
  # visit requirement
  nephrotic_kt_cohort_w_neph <-
    nephrotic_kt_cohort %>%
    inner_join(select(nephrology_visit_cohort, person_id), by = "person_id")

  nephrotic_kt_cohort_w_neph %>% output_tbl("nephrotic_kt_cohort_w_neph",
                                            indexes = list("person_id"))

  fsgs_log <- append_sum(
    cohort = "nephrotic_kt_cohort_w_neph",
    persons = results_tbl("nephrotic_kt_cohort_w_neph") %>% distinct_ct()
  )

  # KIDNEY TRANSPLANT SOURCE CONCEPTS ----
  message("Identify nephrotic subcohort with kidney transplant, according to source concepts")
  nephrotic_kt_src <- cdm_tbl("condition_occurrence") %>%
    inner_join(select(results_tbl("nephrotic_subcohort"), person_id), by = "person_id") %>%
    filter(condition_source_concept_id %in% c(44821546L, 35225404L, 45595522L))

  nephrotic_kt_src %>% output_tbl("nephrotic_kt_src",
                                  indexes = list("person_id", "condition_occurrence_id"))

  # Identify cohort of patients in nephrotic subcohort with kidney transplant, according to source concepts
  nephrotic_kt_src_cohort <-
    nephrotic_kt_src %>%
    distinct(person_id, site)

  nephrotic_kt_src_cohort %>%
    output_tbl("nephrotic_kt_src_cohort",
               indexes = list("person_id"))

  fsgs_log <- append_sum(
    cohort = "nephrotic_kt_src_cohort",
    persons = results_tbl("nephrotic_kt_src_cohort") %>% distinct_ct()
  )

  # Identify nephrotic subcohort with kidney transplant, according to source concepts
  # who meet nephrology visit requirement
  nephrotic_kt_src_cohort_w_neph <-
    nephrotic_kt_src_cohort %>%
    inner_join(select(nephrology_visit_cohort, person_id), by = "person_id")

  nephrotic_kt_src_cohort_w_neph %>% output_tbl("nephrotic_kt_src_cohort_w_neph",
                                            indexes = list("person_id"))

  fsgs_log <- append_sum(
    cohort = "nephrotic_kt_src_cohort_w_neph",
    persons = results_tbl("nephrotic_kt_src_cohort_w_neph") %>% distinct_ct()
  )

  # SLE COHORT ----

  # Identify SLE cohort which will be applied as an exclusion criterion
  message("Get conditions for patients with any SLE inclusion diagnosis and no
          exclusion diagnosis")
  elig_conds_sle_i_e_pats <-
    get_elig_conds_for_i_e_pats(incl_codeset_name = "codeset_sle_incl",
                                excl_codeset_name = "codeset_sle_excl")

  message("Get patients with any SLE inclusion diagnosis and no
          exclusion diagnosis")
  sle_i_e_pats <- elig_conds_sle_i_e_pats %>%
    distinct(person_id, site) %>%
    compute_new(name = "sle_i_e_pats")

  fsgs_log <-
    append_sum(cohort = "sle_i_e_pats",
               persons = distinct_ct(sle_i_e_pats))

  message(
    "Get eligible conditions for patients with any SLE inclusion diagnosis
          and no exclusion diagnosis and 2 or more in-person visits with a
          nephrology or rheumatology care_site or provider"
  )
  elig_conds_sle_i_e_pats_w_elig_visits <-
    get_elig_conds_for_i_e_pats_w_elig_visits(
      incl_spec_codeset = load_codeset("codeset_rheum_nephr_spec"),
      incl_visit_codeset = load_codeset("in_person"),
      elig_conds_for_i_e_pats = elig_conds_sle_i_e_pats
    )

  message(
    "Get patients with any SLE inclusion diagnosis
          and no exclusion diagnosis and 2 or more in-person visits with a
          nephrology or rheumatology care_site or provider"
  )
  sle_i_e_pats_w_elig_visits <-
    elig_conds_sle_i_e_pats_w_elig_visits %>%
    distinct(person_id, site) %>%
    compute_new(name = "sle_i_e_pats_w_elig_visits")

  fsgs_log <- append_sum(cohort = "sle_i_e_pats_w_elig_visits",
                         persons = distinct_ct(sle_i_e_pats_w_elig_visits))

  message("Get patients who meet criteria for SLE algorithm")
  sle_cohort <-
    get_sle_cohort(
      elig_conds_sle_i_e_pats_w_elig_visits =
        elig_conds_sle_i_e_pats_w_elig_visits,
      drug_codeset = load_codeset("codeset_hydroxychloroquine")
    )

  sle_cohort %>% output_tbl(name = "sle_cohort",
                            indexes = list("person_id"))

  fsgs_log <- append_sum(cohort = "sle_cohort",
                         persons = distinct_ct(sle_cohort))

  # SLE EXCLUSION ----
  message("Apply SLE exclusion criterion")

  nephrotic_kt_no_sle <-
    nephrotic_kt_cohort %>%
    anti_join(sle_cohort, by = "person_id")

  nephrotic_kt_no_sle %>% output_tbl("nephrotic_kt_no_sle",
                                     indexes = list("person_id"))

  fsgs_log <-
    append_sum(cohort = "nephrotic_kt_no_sle",
               persons = distinct_ct(nephrotic_kt_no_sle))


  message("Apply SLE exclusion criterion to kt source concept cohort")

  nephrotic_kt_src_no_sle <-
    nephrotic_kt_src_cohort %>%
    anti_join(sle_cohort, by = "person_id")

  nephrotic_kt_src_no_sle %>% output_tbl("nephrotic_kt_src_no_sle",
                                     indexes = list("person_id"))

  fsgs_log <-
    append_sum(cohort = "nephrotic_kt_src_no_sle",
               persons = distinct_ct(nephrotic_kt_src_no_sle))
 
  
  # Note: Kidney transplant patients identified by condition_concept_ids
  # (nephrotic_kt_no_sle) and condition_source_concept_ids (nephrotic_kt_src_no_sle)
  # were combined and restricted to patients meeting the nephrology visit
  # criteria (nephrology_visit_cohort_exp) in subsequent analytic steps
   
}

#' Set up and execute a data request
#'
#' This function encapsulates a "production" run of the data request.  It sets
#' up the environment, executes the request, and cleans up the environment.
#'
#' Typically, the `run.R` file calls run_request() when in a production mode.
#'
#' @param base_dir Path to the top of the data request files.  This is
#'   typically specified in `run.R`.
#'
#' @return The result of [.run()].
#' @md
run_request <- function(base_dir) {
    base_dir <- .load(base_dir)
    on.exit(.env_cleanup())
    .run(base_dir)
}
