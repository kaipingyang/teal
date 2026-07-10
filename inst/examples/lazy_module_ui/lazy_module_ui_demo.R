# lazy_module_ui_demo.R
#
# Prototype app: teal.lazy_module_ui = TRUE with 21 clinical modules.
# Uses official tmc_ex_* data from teal.modules.clinical.
#
# Run from the teal fork directory:
#   pkgload::load_all()
#   source("inst/examples/lazy_module_ui/lazy_module_ui_demo.R")

pkgload::load_all(quiet = TRUE)

options(teal.lazy_module_ui = TRUE)

library(teal.modules.clinical)

# ---- Data ---------------------------------------------------------------
ADSL  <- tmc_ex_adsl
ADAE  <- tmc_ex_adae
ADTTE <- tmc_ex_adtte
ADLB  <- tmc_ex_adlb
ADMH  <- tmc_ex_admh
ADCM  <- tmc_ex_adcm
ADVS  <- tmc_ex_advs

data <- teal_data(
  ADSL = ADSL, ADAE = ADAE, ADTTE = ADTTE,
  ADLB = ADLB, ADMH = ADMH, ADCM = ADCM, ADVS = ADVS
)
join_keys(data) <- default_cdisc_join_keys[
  c("ADSL","ADAE","ADTTE","ADLB","ADMH","ADCM","ADVS")
]

# ---- Helpers ------------------------------------------------------------
cs  <- function(all, sel = all[1]) choices_selected(all, sel)
vcv <- function(df, col) variable_choices(df, col)
vc  <- function(df, param, label) value_choices(df, param, label)

tte_params <- vc(ADTTE, "PARAMCD", "PARAM")
lb_params  <- vc(ADLB,  "PARAMCD", "PARAM")
lb_visits  <- vc(ADLB,  "AVISIT",  "AVISIT")

# ADSL available columns
adsl_arm    <- c("ARM","ARMCD")
adsl_strata <- c("STRATA1","STRATA2")
adsl_cat    <- c("SEX","RACE","STRATA1","STRATA2","EOSSTT","DCSREAS","SAFFL")
adsl_cont   <- c("AGE","BMRKR1","BMRKR2")

# ---- 21 Modules ---------------------------------------------------------
mods <- modules(

  modules(
    label = "Adverse Events",

    tm_t_events(
      label = "AE Overview", dataname = "ADAE",
      arm_var  = cs(adsl_arm),
      llt      = cs(c("AEDECOD","AELLT")),
      hlt      = cs(c("AEBODSYS","AEHLT"))
    ),

    tm_t_events_by_grade(
      label = "AE by Grade", dataname = "ADAE",
      arm_var        = cs(adsl_arm),
      hlt            = cs(c("AEBODSYS","AEHLT"), "AEBODSYS"),
      llt            = cs(c("AEDECOD","AELLT"),  "AEDECOD"),
      grade          = cs(c("AETOXGR","AESEV"),  "AETOXGR"),
      grading_groups = list("Gr 1-2"=c("1","2"), "Gr 3-4"=c("3","4"), "Gr 5"="5")
    ),

    tm_t_events_summary(
      label = "AE Summary", dataname = "ADAE",
      arm_var      = cs(adsl_arm),
      flag_var_anl = cs(c("AEREL","AESER","AESDTH","AESLIFE","AESHOSP"), c("AEREL","AESER"))
    ),

    tm_t_mult_events(
      label = "Multiple Events", dataname = "ADAE",
      arm_var = cs(adsl_arm),
      seq_var = cs("ASEQ"),
      hlt     = cs(c("AEBODSYS","AEHLT"), "AEBODSYS"),
      llt     = cs(c("AEDECOD","AELLT"),  "AEDECOD")
    ),

    tm_t_smq(
      label = "SMQ Table", dataname = "ADAE",
      arm_var = cs(adsl_arm),
      llt     = cs(c("AEDECOD","AELLT"), "AEDECOD"),
      baskets = cs(c("SMQ01NAM","SMQ02NAM"), "SMQ01NAM"),
      scopes  = cs(c("SMQ01SC","SMQ02SC"),   "SMQ01SC")
    )
  ),

  modules(
    label = "Efficacy",

    tm_t_tte(
      label = "Time to Event", dataname = "ADTTE",
      arm_var       = cs(adsl_arm),
      paramcd       = choices_selected(tte_params, "OS"),
      strata_var    = cs(vcv(ADSL, adsl_strata), "STRATA1"),
      time_points   = choices_selected(c(182, 365), 182),
      time_unit_var = cs(vcv(ADTTE, "AVALU"), "AVALU")
    ),

    tm_t_coxreg(
      label = "Cox Regression", dataname = "ADTTE",
      arm_var    = cs(adsl_arm),
      paramcd    = choices_selected(tte_params, "OS"),
      cov_var    = cs(c("SEX","RACE","STRATA1"), c("SEX","STRATA1")),
      strata_var = cs(vcv(ADSL, adsl_strata), "STRATA1")
    ),

    tm_g_km(
      label = "Kaplan-Meier", dataname = "ADTTE",
      arm_var    = cs(adsl_arm),
      paramcd    = choices_selected(tte_params, "OS"),
      strata_var = cs(vcv(ADSL, adsl_strata), "STRATA1"),
      facet_var  = cs(c("SEX","RACE"), NULL)
    )
  ),

  modules(
    label = "Labs",

    tm_t_abnormality_by_worst_grade(
      label = "Lab Worst Grade", dataname = "ADLB",
      arm_var = cs(adsl_arm),
      paramcd = choices_selected(lb_params, lb_params[1])
    ),

    tm_t_shift_by_grade(
      label = "Lab Shift by Grade", dataname = "ADLB",
      arm_var = cs(adsl_arm),
      paramcd = choices_selected(lb_params, lb_params[1])
    ),

    tm_t_ancova(
      label = "ANCOVA", dataname = "ADLB",
      arm_var  = cs(adsl_arm),
      aval_var = cs(vcv(ADLB, c("AVAL","CHG","PCHG")), "CHG"),
      paramcd  = choices_selected(lb_params, lb_params[1]),
      cov_var  = cs(c("SEX","RACE","STRATA1"), "SEX"),
      avisit   = choices_selected(lb_visits, lb_visits[1])
    )
  ),

  modules(
    label = "Demographics",

    tm_t_summary(
      label = "Patient Characteristics", dataname = "ADSL",
      arm_var        = cs(adsl_arm),
      summarize_vars = cs(adsl_cat, adsl_cat[1:3])
    ),

    tm_t_summary_by(
      label = "Summary by Sex", dataname = "ADSL",
      arm_var        = cs(adsl_arm),
      by_vars        = cs(adsl_cat, "SEX"),
      summarize_vars = cs(adsl_cont, "AGE")
    ),

    tm_t_summary_by(
      label = "Summary by Strata", dataname = "ADSL",
      arm_var        = cs(adsl_arm),
      by_vars        = cs(adsl_strata, "STRATA1"),
      summarize_vars = cs(adsl_cont, "AGE")
    )
  ),

  modules(
    label = "Patient Profiles",

    tm_t_pp_basic_info(
      label       = "Basic Info",
      dataname    = "ADSL",
      patient_col = "USUBJID",
      vars        = choices_selected(variable_choices(ADSL), c("ARM","SEX","RACE","AGE","COUNTRY"))
    ),

    tm_t_pp_medical_history(
      label       = "Medical History",
      dataname    = "ADMH",
      patient_col = "USUBJID"
    ),

    tm_t_pp_prior_medication(
      label       = "Prior Medication",
      dataname    = "ADCM",
      patient_col = "USUBJID"
    ),

    tm_t_pp_laboratory(
      label       = "Lab Values",
      dataname    = "ADLB",
      patient_col = "USUBJID"
    ),

    tm_g_pp_adverse_events(
      label       = "AE Timeline",
      dataname    = "ADAE",
      parentname  = "ADSL",
      patient_col = "USUBJID",
      aeterm      = choices_selected(vcv(ADAE, "AETERM"),    "AETERM"),
      tox_grade   = choices_selected(vcv(ADAE, "AETOXGR"),   "AETOXGR"),
      causality   = choices_selected(vcv(ADAE, "AEREL"),     "AEREL"),
      outcome     = choices_selected(vcv(ADAE, "AEOUT"),     "AEOUT"),
      action      = choices_selected(vcv(ADAE, "AEACN"),     "AEACN"),
      time        = choices_selected(vcv(ADAE, c("ASTDY","AENDY")), "ASTDY"),
      decod       = choices_selected(vcv(ADAE, "AEDECOD"),   "AEDECOD")
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

# ---- App ----------------------------------------------------------------
total_mods <- length(unlist(teal:::modules_slot(mods, "label")))
message(sprintf(">>> lazy_module_ui=TRUE — %d modules", total_mods))

app <- teal::init(data = data, modules = mods)
shinyApp(app$ui, app$server)
