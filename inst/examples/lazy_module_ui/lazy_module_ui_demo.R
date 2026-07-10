# lazy_module_ui_demo.R
#
# Prototype app demonstrating teal.lazy_module_ui = TRUE.
# 24 real teal.modules.clinical modules using official tmc_ex_* data.
#
# MUST be run via Rscript (not inside an active renv session):
#
#   /opt/R/4.4.3/bin/Rscript \
#     /usrfiles/shared-projects/users/kaiping_yang/teal/inst/examples/lazy_module_ui/lazy_module_ui_demo.R
#
# Running inside ERP_TEST renv will load the wrong teal — use Rscript above.

FORK_LIB <- "/mnt/usrfiles/bgcrh/support/sp_app/project/ERP_TEST/tests/lazy_module_ui_prototype/lib"
ERP_LIB  <- "/mnt/usrfiles/bgcrh/support/sp_app/project/ERP_TEST/renv/library/linux-ubuntu-jammy/R-4.4/x86_64-pc-linux-gnu"

# Fork teal first so library(teal) picks up the lazy-module-ui version
.libPaths(c(FORK_LIB, ERP_LIB, "/opt/R/4.4.3/lib/R/library"))

options(teal.lazy_module_ui = TRUE)

library(teal)
library(teal.modules.clinical)

# Verify fork loaded (must contain lazy_module_ui_prototype in path)
teal_path <- find.package("teal")
if (!grepl("lazy_module_ui_prototype", teal_path)) {
  stop(
    "Wrong teal loaded: ", teal_path,
    "\nRun via Rscript, not inside an active renv session."
  )
}
message("teal OK: ", teal_path)

# ---- Data (official tmc_ex_* — correct PARAMCD strings) -----------------
ADSL  <- tmc_ex_adsl
ADAE  <- tmc_ex_adae
ADTTE <- tmc_ex_adtte
ADLB  <- tmc_ex_adlb
ADMH  <- tmc_ex_admh
ADCM  <- tmc_ex_adcm
ADEX  <- tmc_ex_adex
ADVS  <- tmc_ex_advs
ADRS  <- tmc_ex_adrs

data <- teal_data(
  ADSL = ADSL, ADAE = ADAE, ADTTE = ADTTE, ADLB = ADLB,
  ADMH = ADMH, ADCM = ADCM, ADEX = ADEX, ADVS = ADVS, ADRS = ADRS
)
join_keys(data) <- default_cdisc_join_keys[
  c("ADSL","ADAE","ADTTE","ADLB","ADMH","ADCM","ADEX","ADVS","ADRS")
]

# ---- Helpers ------------------------------------------------------------
cs  <- function(all, sel = all[1]) choices_selected(all, sel)
vcv <- function(df, col) variable_choices(df, col)
vv  <- function(df, col) value_choices(df, col, col)

tte_params <- vv(ADTTE, "PARAMCD")
lb_params  <- vv(ADLB,  "PARAMCD")
lb_visits  <- vv(ADLB,  "AVISIT")

# ---- 24 Modules ---------------------------------------------------------
mods <- modules(

  modules(
    label = "Adverse Events",
    tm_t_events(
      label = "AE Overview", dataname = "ADAE",
      arm_var = cs(c("ARM","ARMCD")),
      llt = cs(c("AEDECOD","AELLT")),
      hlt = cs(c("AEBODSYS","AEHLT"))
    ),
    tm_t_events_by_grade(
      label = "AE by Grade", dataname = "ADAE",
      arm_var = cs(c("ARM","ARMCD")),
      hlt = cs(c("AEBODSYS","AEHLT"), "AEBODSYS"),
      llt = cs(c("AEDECOD","AELLT"),  "AEDECOD"),
      grade = cs(c("AETOXGR","AESEV"), "AETOXGR"),
      grading_groups = list("Gr 1-2"=c("1","2"), "Gr 3-4"=c("3","4"), "Gr 5"="5")
    ),
    tm_t_events_summary(
      label = "AE Summary", dataname = "ADAE",
      arm_var = cs(c("ARM","ARMCD")),
      flag_var_anl = cs(c("AEREL","AESER","AESDTH","AESLIFE","AESHOSP"), c("AEREL","AESER"))
    ),
    tm_t_mult_events(
      label = "Multiple Events", dataname = "ADAE",
      arm_var = cs(c("ARM","ARMCD")),
      seq_var = cs("ASEQ"),
      hlt = cs(c("AEBODSYS","AEHLT"), "AEBODSYS"),
      llt = cs(c("AEDECOD","AELLT"),  "AEDECOD")
    ),
    tm_t_smq(
      label = "SMQ Table", dataname = "ADAE",
      arm_var = cs(c("ARM","ARMCD")),
      llt = cs(c("AEDECOD","AELLT"), "AEDECOD"),
      baskets = cs(c("SMQ01NAM","SMQ02NAM"), "SMQ01NAM"),
      scopes  = cs(c("SMQ01SC","SMQ02SC"),   "SMQ01SC")
    )
  ),

  modules(
    label = "Efficacy",
    tm_t_tte(
      label = "Time to Event", dataname = "ADTTE",
      arm_var = cs(c("ARM","ARMCD")),
      paramcd = choices_selected(tte_params, "OS"),
      strata_var = cs(vcv(ADSL, c("SEX","STRATA1","STRATA2")), "STRATA1"),
      time_points = choices_selected(c(182, 365), 182),
      time_unit_var = cs(vcv(ADTTE, "AVALU"), "AVALU")
    ),
    tm_t_coxreg(
      label = "Cox Regression", dataname = "ADTTE",
      arm_var = cs(c("ARM","ARMCD")),
      paramcd = choices_selected(tte_params, "OS"),
      cov_var = cs(c("SEX","RACE","AGEGR1","STRATA1"), c("SEX","AGEGR1")),
      strata_var = cs(vcv(ADSL, c("SEX","STRATA1","STRATA2")), "STRATA1")
    ),
    tm_g_km(
      label = "Kaplan-Meier", dataname = "ADTTE",
      arm_var = cs(c("ARM","ARMCD")),
      paramcd = choices_selected(tte_params, "OS"),
      strata_var = cs(vcv(ADSL, c("SEX","STRATA1","STRATA2")), "STRATA1"),
      facet_var = cs(c("SEX","RACE","AGEGR1"), NULL)
    )
  ),

  modules(
    label = "Labs",
    tm_t_abnormality_by_worst_grade(
      label = "Lab Worst Grade", dataname = "ADLB",
      arm_var = cs(c("ARM","ARMCD")),
      paramcd = choices_selected(lb_params, lb_params[1])
    ),
    tm_t_shift_by_grade(
      label = "Lab Shift by Grade", dataname = "ADLB",
      arm_var = cs(c("ARM","ARMCD")),
      paramcd = choices_selected(lb_params, lb_params[1])
    ),
    tm_t_ancova(
      label = "ANCOVA", dataname = "ADLB",
      arm_var = cs(c("ARM","ARMCD")),
      aval_var = cs(vcv(ADLB, c("AVAL","CHG","PCHG")), "CHG"),
      paramcd = choices_selected(lb_params, lb_params[1]),
      cov_var = cs(c("SEX","RACE","AGEGR1","STRATA1"), "SEX"),
      avisit  = choices_selected(lb_visits, lb_visits[1])
    )
  ),

  modules(
    label = "Demographics",
    tm_t_summary(
      label = "Patient Characteristics", dataname = "ADSL",
      arm_var = cs(c("ARM","ARMCD")),
      summarize_vars = cs(c("SEX","RACE","ETHNIC","AGEGR1","COUNTRY"), c("SEX","RACE","AGEGR1"))
    ),
    tm_t_summary_by(
      label = "Summary by Sex", dataname = "ADSL",
      arm_var = cs(c("ARM","ARMCD")),
      by_vars = cs(c("SEX","RACE","AGEGR1"), "SEX"),
      summarize_vars = cs(c("AGE","BMRKR1"), "AGE")
    ),
    tm_t_summary_by(
      label = "Summary by Age Group", dataname = "ADSL",
      arm_var = cs(c("ARM","ARMCD")),
      by_vars = cs(c("SEX","RACE","AGEGR1"), "AGEGR1"),
      summarize_vars = cs(c("AGE","BMRKR1"), "AGE")
    ),
    tm_t_summary_by(
      label = "Summary by Race", dataname = "ADSL",
      arm_var = cs(c("ARM","ARMCD")),
      by_vars = cs(c("SEX","RACE","AGEGR1"), "RACE"),
      summarize_vars = cs(c("AGE","BMRKR1"), "AGE")
    )
  ),

  modules(
    label = "Patient Profiles",
    tm_t_pp_basic_info(label = "Basic Info", dataname = "ADSL", patient_col = "USUBJID"),
    tm_t_pp_medical_history(label = "Medical History", dataname = "ADMH", patient_col = "USUBJID"),
    tm_t_pp_prior_medication(label = "Prior Medication", dataname = "ADCM", patient_col = "USUBJID"),
    tm_t_pp_laboratory(label = "Lab Values", dataname = "ADLB", patient_col = "USUBJID"),
    tm_g_pp_adverse_events(label = "AE Timeline", dataname = "ADAE", patient_col = "USUBJID"),
    tm_g_pp_therapy(label = "Therapy", dataname = "ADCM", patient_col = "USUBJID"),
    tm_g_pp_vitals(label = "Vitals", dataname = "ADVS", patient_col = "USUBJID"),
    tm_t_summary(
      label = "Disposition Summary", dataname = "ADSL",
      arm_var = cs(c("ARM","ARMCD")),
      summarize_vars = cs(c("EOSSTT","DCSREAS","DTHFL"), "EOSSTT")
    ),
    tm_t_summary(
      label = "Biomarker Summary", dataname = "ADSL",
      arm_var = cs(c("ARM","ARMCD")),
      summarize_vars = cs(c("BMRKR1","BMRKR2","ITTFL"), "BMRKR1")
    )
  )
)

app <- teal::init(data = data, modules = mods)
shinyApp(app$ui, app$server)
