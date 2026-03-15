# =============================================================================
# 03_panel_regressions.R
# TFP and Public Expenditures in Brazilian States (2003–2018)
# Step 3: Panel data regressions (two-way fixed effects, robust SE)
# =============================================================================
# Model (Eq. 12 in the paper):
#   ptf_it = alpha_i + lambda_t + beta1*desp_it + beta2*vak_it +
#            beta3*educ_it + beta4*pop_it + beta5*agua_it +
#            beta6*trade_it + beta7*energ_it + epsilon_it
#
# Estimator : within (two-way FE)
# Std errors: White-Arellano HC0 clustered by state
# =============================================================================

rm(list = ls())

library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
library(stargazer)

# ---- Load panel -------------------------------------------------------------
dados_raw <- readRDS("data/interim/painel_final.rds")

# pdata.frame: individual = Estado, time = Ano
dados <- pdata.frame(
  dados_raw %>% select(Estado, Ano, ptf, starts_with("ln"), educ,
                       starts_with("desp")),
  index     = c("Estado", "Ano"),
  drop.index = FALSE,
  row.names  = TRUE
)

pdim(dados)
cat("Balanced panel:", is.pbalanced(dados), "\n")

# ---- Helper: estimate model + robust SE ------------------------------------
# `desp_var` : character, the log-expenditure variable name (e.g. "lndespjud")
# `data`     : pdata.frame
# Returns a list(model, coeftest_robust)

fit_fe <- function(desp_var, data) {
  fml <- as.formula(
    paste0("ptf ~ ", desp_var,
           " + educ + lnvak + lntrade + lnagua + lnenergia + lnpopdens")
  )
  m <- plm(fml, data = data, effect = "twoways", model = "within")
  ct <- coeftest(m, vcov = vcovHC, method = "arellano",
                 type = "HC0", cluster = "group")
  list(model = m, robust = ct)
}

# ---- Expenditure variables (in levels, log) ---------------------------------
# Proportion of total expenditure (used in main specification)
desp_vars_prop <- c(
  judiciario   = "lndespjudp",
  legislativo  = "lndesplegp",
  administracao = "lndespadmp",
  educacao     = "lndespeducp",
  ics          = "lndespicsp",
  tecnologia   = "lndesptecp",
  assistencia  = "lndespassp",
  saude        = "lndespsaudep"
)

# Absolute values (R$ 2018, log) – for robustness
desp_vars_abs <- c(
  judiciario   = "lndespjud",
  legislativo  = "lndespleg",
  administracao = "lndespadm",
  educacao     = "lndespeduc",
  ics          = "lndespics",
  tecnologia   = "lndesptec",
  assistencia  = "lndespass",
  saude        = "lndespsaude"
)

# ---- Main results (proportions) --------------------------------------------
results_prop <- lapply(desp_vars_prop, fit_fe, data = dados)

cat("\n========= MAIN RESULTS (expenditure as share of total) ==========\n")
for (nm in names(results_prop)) {
  cat("\n---", nm, "---\n")
  print(results_prop[[nm]]$robust)
}

# ---- Robustness: absolute values -------------------------------------------
results_abs <- lapply(desp_vars_abs, fit_fe, data = dados)

cat("\n========= ROBUSTNESS CHECK (absolute expenditure, R$ 2018) ======\n")
for (nm in names(results_abs)) {
  cat("\n---", nm, "---\n")
  print(results_abs[[nm]]$robust)
}

# ---- Hausman test (FE vs RE) for judiciário share --------------------------
jud_fe <- plm(ptf ~ lndespjudp + educ + lnvak + lntrade + lnagua +
                lnenergia + lnpopdens,
              data = dados, effect = "twoways", model = "within")
jud_re <- plm(ptf ~ lndespjudp + educ + lnvak + lntrade + lnagua +
                lnenergia + lnpopdens,
              data = dados, effect = "twoways", model = "random")
cat("\n--- Hausman test (FE vs RE) – judiciary expenditure share ---\n")
print(phtest(jud_fe, jud_re))

# ---- Poolability test (Breusch-Pagan / BP LM test) ------------------------
jud_pool <- plm(ptf ~ lndespjudp + educ + lnvak + lntrade + lnagua +
                  lnenergia + lnpopdens,
                data = dados, model = "pooling")
cat("\n--- Breusch-Pagan LM test (pooling vs RE) ---\n")
print(plmtest(jud_pool, type = "bp"))

# ---- Publication-quality table (stargazer) ---------------------------------
# Extract only the main expenditure coefficient for each model
main_models <- lapply(results_prop, `[[`, "model")

stargazer(
  main_models,
  type          = "latex",
  out           = "output/table3_results.tex",
  title         = "Fixed Effects Estimates: Determinants of State TFP",
  dep.var.label = "log(TFP)",
  column.labels = c("Judiciary","Legislative","Admin.",
                    "Education","ICS","Technology","Assistance","Health"),
  covariate.labels = c("log(Expenditure share)",
                       "Years of schooling",
                       "log(Ind. VA / GDP)",
                       "log(Trade openness)",
                       "log(Water access)",
                       "log(Energy p.c.)",
                       "log(Pop. density)"),
  omit.stat     = c("f","ser","adj.rsq"),
  notes         = paste0("Robust standard errors (White-Arellano HC0, ",
                         "clustered by state) in parentheses. ",
                         "Two-way (state + year) fixed effects. ",
                         "*** p<0.01, ** p<0.05, * p<0.10."),
  notes.align   = "l"
)
message("Saved: output/table3_results.tex")

# ---- Optional: elasticity table for the two significant models -------------
cat("\n=== Significant results summary ===\n")
cat("Judiciary (share):\n")
print(results_prop[["judiciario"]]$robust)
cat("\nHealth (share):\n")
print(results_prop[["saude"]]$robust)
