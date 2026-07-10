# lazy_module_ui_demo.R
#
# Prototype app: teal.lazy_module_ui = TRUE with 20 clinical modules.
# All modules use official tmc_ex_* data and exact parameters from package examples.
#
# Run from the teal fork directory:
#   pkgload::load_all()
#   source("inst/examples/lazy_module_ui/lazy_module_ui_demo.R")

pkgload::load_all(quiet = TRUE)

options(teal.lazy_module_ui = TRUE)

suppressPackageStartupMessages({
  library(teal.modules.clinical)
  library(dplyr)
  library(tern)
  library(formatters)
  library(rtables)
})

# ---- Data (built inside teal_data via within() — exactly as in official examples) ------
data <- teal_data()
data <- within(data, {
  library(teal.modules.clinical)
  library(dplyr)
  library(tern)
  library(formatters)

  ADSL  <- tmc_ex_adsl %>%
    mutate(
      DTHFL = case_when(!is.na(DTHDT) ~ "Y", TRUE ~ "") %>%
        formatters::with_label("Subject Death Flag")
    )

  # ADAE: convert character cols to factor so grade cols are factors
  .lbls_adae  <- col_labels(tmc_ex_adae)
  ADAE <- tmc_ex_adae %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      TMPFL_SER = AESER == "Y",
      TMPFL_REL = AEREL == "Y",
      TMPFL_GR5 = AETOXGR == "5",
      TMP_SMQ01 = !is.na(SMQ01NAM),
      TMP_SMQ02 = !is.na(SMQ02NAM),
      TMP_CQ01  = !is.na(CQ01NAM)
    )
  col_labels(ADAE) <- c(.lbls_adae,
    TMPFL_SER = "Serious AE",    TMPFL_REL = "Related AE",
    TMPFL_GR5 = "Grade 5 AE",   TMP_SMQ01 = "SMQ01",
    TMP_SMQ02 = "SMQ02",         TMP_CQ01  = "CQ01"
  )

  ADTTE <- tmc_ex_adtte
  ADLB  <- tmc_ex_adlb
  ADQS  <- tmc_ex_adqs
  ADMH  <- tmc_ex_admh
  # ADCM: add CMASTDTM/CMAENDTM needed by tm_t_pp_prior_medication
  ADCM  <- tmc_ex_adcm
  ADCM$CMASTDTM <- ADCM$ASTDTM
  ADCM$CMAENDTM <- ADCM$AENDTM
  ADVS  <- tmc_ex_advs

  # ADCM needs self-join keys for tm_t_mult_events
  .adcm_keys <- c("STUDYID","USUBJID","ASTDTM","CMSEQ","ATC1","ATC2","ATC3","ATC4")
})
join_keys(data) <- default_cdisc_join_keys[
  c("ADSL","ADAE","ADTTE","ADLB","ADQS","ADMH","ADCM","ADVS")
]
join_keys(data)["ADCM","ADCM"] <- c("STUDYID","USUBJID","ASTDTM","CMSEQ","ATC1","ATC2","ATC3","ATC4")

# Local copies for choices_selected definitions
ADSL  <- data[["ADSL"]]
ADAE  <- data[["ADAE"]]
ADTTE <- data[["ADTTE"]]
ADLB  <- data[["ADLB"]]
ADQS  <- data[["ADQS"]]
ADMH  <- data[["ADMH"]]
ADCM  <- data[["ADCM"]]

# arm_ref_comp needed by TTE/KM/CoxReg/ANCOVA
arm_ref_comp <- list(
  ARM     = list(ref = "B: Placebo", comp = c("A: Drug X", "C: Combination")),
  ACTARMCD = list(ref = "ARM B",     comp = c("ARM A",     "ARM C"))
)

# Dynamic SMQ basket/scope choices
.names_baskets <- grep("^(SMQ|CQ).*NAM$", names(ADAE), value = TRUE)
.names_scopes  <- grep("^SMQ.*SC$",        names(ADAE), value = TRUE)
cs_baskets <- choices_selected(variable_choices(ADAE, .names_baskets), .names_baskets)
cs_scopes  <- choices_selected(variable_choices(ADAE, .names_scopes),  .names_scopes, fixed = TRUE)

# ---- Modules ---------------------------------------------------------------
mods <- modules(

  # === Adverse Events ===
  modules(
    label = "Adverse Events",

    tm_t_events(
      label    = "AE Overview",
      dataname = "ADAE",
      arm_var  = choices_selected(c("ARM","ARMCD"), "ARM"),
      llt      = choices_selected(variable_choices(ADAE, c("AEDECOD","AELLT")), "AEDECOD"),
      hlt      = choices_selected(variable_choices(ADAE, c("AEBODSYS","AEHLT")), "AEBODSYS")
    ),

    tm_t_events_by_grade(
      label    = "AE by Grade",
      dataname = "ADAE",
      arm_var  = choices_selected(c("ARM","ARMCD"), "ARM"),
      llt      = choices_selected(variable_choices(ADAE, c("AETERM","AEDECOD")), "AEDECOD"),
      hlt      = choices_selected(variable_choices(ADAE, c("AEBODSYS","AESOC")), "AEBODSYS"),
      grade    = choices_selected(variable_choices(ADAE, c("AETOXGR","AESEV")), "AETOXGR")
    ),

    tm_t_events_summary(
      label        = "AE Summary",
      dataname     = "ADAE",
      arm_var      = choices_selected(c("ARM","ARMCD"), "ARM"),
      flag_var_anl = choices_selected(
        variable_choices(ADAE, c("TMPFL_SER","TMPFL_REL","TMPFL_GR5","TMP_SMQ01","TMP_SMQ02","TMP_CQ01")),
        c("TMPFL_SER","TMPFL_REL","TMPFL_GR5")
      )
    ),

    tm_t_mult_events(
      label     = "Medications by Class",
      dataname  = "ADCM",
      arm_var   = choices_selected(c("ARM","ARMCD"), "ARM"),
      seq_var   = choices_selected("CMSEQ", "CMSEQ", fixed = TRUE),
      hlt       = choices_selected(variable_choices(ADCM, c("ATC1","ATC2","ATC3","ATC4")), c("ATC1","ATC2","ATC3","ATC4")),
      llt       = choices_selected(variable_choices(ADCM, "CMDECOD"), "CMDECOD"),
      event_type = "treatment"
    ),

    tm_t_smq(
      label    = "SMQ Table",
      dataname = "ADAE",
      arm_var  = choices_selected(variable_choices(ADSL, c("ARM","ARMCD")), "ARM"),
      llt      = choices_selected(variable_choices(ADAE, c("AEDECOD","AELLT")), "AEDECOD"),
      baskets  = cs_baskets,
      scopes   = cs_scopes
    )
  ),

  # === Efficacy ===
  modules(
    label = "Efficacy",

    tm_t_tte(
      label         = "Time to Event",
      dataname      = "ADTTE",
      arm_var       = choices_selected(variable_choices(ADSL, c("ARM","ARMCD","ACTARMCD")), "ARM"),
      arm_ref_comp  = arm_ref_comp,
      paramcd       = choices_selected(value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"),
      strata_var    = choices_selected(variable_choices(ADSL, c("SEX","BMRKR2")), "SEX"),
      time_points   = choices_selected(c(182, 243), 182),
      event_desc_var = choices_selected(variable_choices(ADTTE, "EVNTDESC"), "EVNTDESC", fixed = TRUE)
    ),

    tm_t_coxreg(
      label        = "Cox Regression",
      dataname     = "ADTTE",
      arm_var      = choices_selected(c("ARM","ARMCD","ACTARMCD"), "ARM"),
      arm_ref_comp = arm_ref_comp,
      paramcd      = choices_selected(value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"),
      cov_var      = choices_selected(variable_choices(ADSL, c("AGE","SEX","RACE","BMRKR1","BMRKR2")), c("AGE","SEX")),
      strata_var   = choices_selected(variable_choices(ADSL, c("SEX","STRATA2")), "SEX")
    ),

    tm_g_km(
      label        = "Kaplan-Meier",
      dataname     = "ADTTE",
      arm_var      = choices_selected(variable_choices(ADSL, c("ARM","ARMCD","ACTARMCD")), "ARM"),
      arm_ref_comp = arm_ref_comp,
      paramcd      = choices_selected(value_choices(ADTTE, "PARAMCD", "PARAM"), "OS"),
      strata_var   = choices_selected(variable_choices(ADSL, c("SEX","BMRKR2")), "SEX"),
      facet_var    = choices_selected(variable_choices(ADSL, c("SEX","BMRKR2","ARM")), NULL)
    )
  ),

  # === Labs ===
  modules(
    label = "Labs",

    tm_t_abnormality_by_worst_grade(
      label    = "Lab Worst Grade",
      dataname = "ADLB",
      arm_var  = choices_selected(variable_choices(ADSL, c("ARM","ARMCD")), "ARM"),
      paramcd  = choices_selected(value_choices(ADLB, "PARAMCD", "PARAM"), "ALT")
    ),

    tm_t_shift_by_grade(
      label              = "Lab Shift by Grade",
      dataname           = "ADLB",
      arm_var            = choices_selected(variable_choices(ADSL, c("ARM","ARMCD")), "ARM"),
      paramcd            = choices_selected(value_choices(ADLB, "PARAMCD", "PARAM"), "ALT"),
      worst_flag_var     = choices_selected(
        variable_choices(ADLB, c("WGRLOVFL","WGRLOFL","WGRHIVFL","WGRHIFL")), "WGRLOVFL"),
      worst_flag_indicator = choices_selected(value_choices(ADLB, "WGRLOVFL"), "Y", fixed = TRUE),
      anl_toxgrade_var   = choices_selected(variable_choices(ADLB, "ATOXGR"), "ATOXGR", fixed = TRUE),
      base_toxgrade_var  = choices_selected(variable_choices(ADLB, "BTOXGR"), "BTOXGR", fixed = TRUE)
    ),

    tm_t_ancova(
      label        = "ANCOVA",
      dataname     = "ADQS",
      avisit       = choices_selected(value_choices(ADQS, "AVISIT"), "WEEK 1 DAY 8"),
      arm_var      = choices_selected(variable_choices(ADSL, c("ARM","ACTARMCD","ARMCD")), "ARMCD"),
      arm_ref_comp = arm_ref_comp,
      aval_var     = choices_selected(variable_choices(ADQS, c("AVAL","CHG")), "CHG"),
      paramcd      = choices_selected(value_choices(ADQS, "PARAMCD", "PARAM"), "FKSI-FWB"),
      cov_var      = choices_selected(variable_choices(ADSL, c("SEX","AGE","RACE","STRATA1","BMRKR1","BMRKR2")), c("SEX","AGE"))
    )
  ),

  # === Demographics ===
  modules(
    label = "Demographics",

    tm_t_summary(
      label          = "Patient Characteristics",
      dataname       = "ADSL",
      arm_var        = choices_selected(c("ARM","ARMCD"), "ARM"),
      summarize_vars = choices_selected(
        variable_choices(ADSL, c("SEX","AGE","RACE","COUNTRY","STRATA1","STRATA2","EOSSTT")),
        c("SEX","AGE","RACE")
      )
    ),

    tm_t_summary_by(
      label          = "Lab Summary by Visit",
      dataname       = "ADLB",
      arm_var        = choices_selected(variable_choices(ADSL, c("ARM","ARMCD")), "ARM"),
      by_vars        = choices_selected(variable_choices(ADLB, c("PARAM","AVISIT")), "AVISIT"),
      summarize_vars = choices_selected(variable_choices(ADLB, c("AVAL","CHG")), "AVAL"),
      id_var         = choices_selected(variable_choices(ADSL, "USUBJID"), "USUBJID"),
      paramcd        = choices_selected(value_choices(ADLB, "PARAMCD", "PARAM"), "ALT")
    )
  ),

  # === Patient Profiles ===
  modules(
    label = "Patient Profiles",

    tm_t_pp_basic_info(
      label       = "Basic Info",
      dataname    = "ADSL",
      patient_col = "USUBJID",
      vars        = choices_selected(variable_choices(ADSL), c("ARM","AGE","SEX","COUNTRY","RACE","EOSSTT"))
    ),

    tm_t_pp_medical_history(
      label       = "Medical History",
      dataname    = "ADMH",
      parentname  = "ADSL",
      patient_col = "USUBJID",
      mhterm      = choices_selected(variable_choices(ADMH, "MHTERM"),   "MHTERM"),
      mhbodsys    = choices_selected(variable_choices(ADMH, "MHBODSYS"), "MHBODSYS"),
      mhdistat    = choices_selected(variable_choices(ADMH, "MHDISTAT"), "MHDISTAT")
    ),

    tm_t_pp_prior_medication(
      label       = "Prior Medication",
      dataname    = "ADCM",
      parentname  = "ADSL",
      patient_col = "USUBJID",
      atirel      = choices_selected(variable_choices(ADCM, "ATIREL"),  "ATIREL"),
      cmdecod     = choices_selected(variable_choices(ADCM, "CMDECOD"), "CMDECOD"),
      cmindc      = choices_selected(variable_choices(ADCM, "CMINDC"),  "CMINDC"),
      cmstdy      = choices_selected(variable_choices(ADCM, c("ASTDY","AENDY")), "ASTDY")
    ),

    tm_t_pp_laboratory(
      label       = "Lab Values",
      dataname    = "ADLB",
      parentname  = "ADSL",
      patient_col = "USUBJID"
    ),

    tm_g_pp_adverse_events(
      label       = "AE Timeline",
      dataname    = "ADAE",
      parentname  = "ADSL",
      patient_col = "USUBJID",
      aeterm      = choices_selected(variable_choices(ADAE, "AETERM"),   "AETERM"),
      tox_grade   = choices_selected(variable_choices(ADAE, "AETOXGR"),  "AETOXGR"),
      causality   = choices_selected(variable_choices(ADAE, "AEREL"),    "AEREL"),
      outcome     = choices_selected(variable_choices(ADAE, "AEOUT"),    "AEOUT"),
      action      = choices_selected(variable_choices(ADAE, "AEACN"),    "AEACN"),
      time        = choices_selected(variable_choices(ADAE, c("ASTDY","AENDY")), "ASTDY"),
      decod       = choices_selected(variable_choices(ADAE, "AEDECOD"),  "AEDECOD")
    ),

    tm_g_pp_therapy(
      label       = "Therapy",
      dataname    = "ADCM",
      parentname  = "ADSL",
      patient_col = "USUBJID"
    ),

    tm_g_pp_vitals(
      label       = "Vitals",
      dataname    = "ADVS",
      parentname  = "ADSL",
      patient_col = "USUBJID"
    )
  )
)

# ---- App -------------------------------------------------------------------
total_mods <- length(unlist(teal:::modules_slot(mods, "label")))
message(sprintf(">>> lazy_module_ui=TRUE — %d modules", total_mods))

app <- teal::init(data = data, modules = mods)
shinyApp(app$ui, app$server)
