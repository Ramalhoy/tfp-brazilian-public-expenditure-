# =============================================================================
# 02_tfp_estimation.R
# TFP and Public Expenditures in Brazilian States (2003–2018)
# Step 2: Price deflation + TFP estimation via perpetual inventory method
# =============================================================================
# References:
#   Ferreira (2010) – Ensaios Econômicos EPGE n.705
#   Gollin (2002)   – Journal of Political Economy
#   Elgin & Çakir (2015) – Economics of Innovation and New Technology
# =============================================================================

rm(list = ls())
library(dplyr)
library(tidyr)
library(ipeadatar)
library(lubridate)

# ---- Parameters (Ferreira 2010; Gollin 2002; Elgin & Çakir 2015) -----------
alpha   <- 0.4     # capital elasticity (Gollin 2002)
beta    <- 0.6     # labour elasticity  (Gollin 2002)
phi     <- 0.1     # Mincerian return per year of schooling (Ferreira 2010)
delta   <- 0.0596  # annual depreciation rate (Ferreira 2010)
g       <- 0.087   # technical growth rate – growth of scientific articles
                   # (Elgin & Çakir 2015; computed over 2003–2018 from CNPq data)
anos    <- 2003:2018

# =============================================================================
# A. PRICE DEFLATION
# =============================================================================
# Strategy: deflate all monetary series (R$ values) to 2018 constant prices
# using the implicit GDP deflator (IBGE national accounts).
# Alternative: IPCA – acceptable but the GDP deflator is preferable for
# aggregated production series.
#
# IPEA series: "IBGE12_DEFPIBPM12"  (GDP deflator, annual, 2010=100)
# Or equivalently: "SCN_DEFPIB"

deflator_raw <- ipeadatar::ipeadata("IBGE12_DEFPIBPM12")

deflator <- deflator_raw %>%
  filter(year(date) %in% c(1998:2018)) %>%   # need pre-2003 for K0
  transmute(Ano = year(date), deflator = value) %>%
  group_by(Ano) %>%
  summarise(deflator = mean(deflator))

# Index relative to 2018 (base year = 2018)
base2018 <- deflator %>% filter(Ano == 2018) %>% pull(deflator)
deflator  <- deflator %>% mutate(defl_idx = deflator / base2018)

# Function to deflate a nominal R$ series to 2018 prices
deflate_to_2018 <- function(df, valor_col, ano_col = "Ano") {
  df %>%
    left_join(deflator %>% select(Ano, defl_idx), by = setNames("Ano", ano_col)) %>%
    mutate({{ valor_col }} := .data[[valor_col]] / defl_idx) %>%
    select(-defl_idx)
}

# Load nominal panel (from 01_data_collection.R)
painel_nom <- readRDS("data/interim/painel_nominal.rds")

# Deflate PIB (and expenditure columns when available)
painel_real <- painel_nom %>%
  deflate_to_2018(valor_col = "PIB")
# When FINBRA expenditure columns are present, add:
# painel_real <- painel_real %>%
#   mutate(across(starts_with("desp") & !ends_with("p"),
#                 ~ .x / defl_idx))   # defl_idx joined per Ano

# =============================================================================
# B. NATIONAL INVESTMENT SHARE
# =============================================================================
# I_state,t = share_inv_Brazil_t * PIB_state,t  (Ferreira 2010 hypothesis)
# share_inv is in the panel from step 01.

painel_real <- painel_real %>%
  mutate(I = share_inv * PIB)   # Gross Fixed Capital Formation proxy (R$ 2018)

# =============================================================================
# C. CAPITAL STOCK – PERPETUAL INVENTORY METHOD (Ferreira 2010)
# =============================================================================
# Step C1: Initial investment I_0 = mean of first 5 years of the series
# Step C2: K_0 = I_0 / (g + delta)
# Step C3: K_{t+1} = I_t + (1 - delta) * K_t

compute_capital <- function(df_state) {
  df_state <- df_state %>% arrange(Ano)
  n  <- nrow(df_state)
  I  <- df_state$I
  K  <- numeric(n)

  I0    <- mean(I[1:min(5, n)], na.rm = TRUE)
  K[1]  <- I0 / (g + delta)                  # Eq. K_0 = I_0 / (g + delta)

  for (t in 2:n) {
    K[t] <- I[t - 1] + (1 - delta) * K[t - 1]  # Kt+1 = It + (1-d)*Kt
  }
  df_state$K <- K
  df_state
}

painel_k <- painel_real %>%
  group_by(Estado) %>%
  group_modify(~ compute_capital(.x)) %>%
  ungroup()

# =============================================================================
# D. PER-WORKER VARIABLES
# =============================================================================
painel_pw <- painel_k %>%
  mutate(
    y = PIB / L,   # output per worker  (R$ 2018)
    k = K   / L    # capital per worker (R$ 2018)
  )

# =============================================================================
# E. HUMAN CAPITAL (Mincerian – Ferreira 2010)
# H_it = exp(phi * h_it),  phi = 0.1
# =============================================================================
painel_pw <- painel_pw %>%
  mutate(H = exp(phi * educ))

# =============================================================================
# F. TOTAL FACTOR PRODUCTIVITY
# A_it = y_it / (k_it^alpha * H_it^beta)    [Eq. 7 in the paper]
# Then ptf = log(A)  for regression use
# =============================================================================
painel_pw <- painel_pw %>%
  mutate(
    A   = y / (k^alpha * H^beta),
    ptf = log(A)
  )

# Sanity checks
cat("\n--- TFP summary by state (mean over 2003–2018) ---\n")
painel_pw %>%
  filter(Ano %in% anos) %>%
  group_by(Estado) %>%
  summarise(mean_A = mean(A, na.rm=TRUE),
            sd_A   = sd(A, na.rm=TRUE)) %>%
  print(n=26)

cat("\n--- Cross-sectional correlation: TFP vs years of schooling ---\n")
with(painel_pw %>% filter(Ano == 2010),
     cor(ptf, educ, use="complete.obs")) %>% print()

# =============================================================================
# G. LOG-TRANSFORM CONTROL VARIABLES
# =============================================================================
painel_final <- painel_pw %>%
  filter(Ano %in% anos) %>%
  mutate(
    lnPIB      = log(PIB),
    lnK        = log(K),
    lnL        = log(L),
    lnkl       = log(k),          # capital per worker (Kl in original script)
    lnvak      = log(vak),
    lnpopdens  = log(popdens),
    lnagua     = log(agua),
    lnenergia  = log(energiapc)
    # lntrade  = log(trade)       # uncomment when trade is available
  )

# When expenditure variables are in the panel, add proportion variables:
# painel_final <- painel_final %>%
#   mutate(across(starts_with("desp") & !ends_with("p") & !starts_with("despgov"),
#                 .names = "ln_{.col}",
#                 ~ log(.x))) %>%
#   mutate(across(ends_with("p") & starts_with("desp"),
#                 .names = "ln_{.col}",
#                 ~ log(.x)))

saveRDS(painel_final, "data/interim/painel_final.rds")
message("Saved: data/interim/painel_final.rds  (", nrow(painel_final), " obs)")

# =============================================================================
# H. OPTIONAL: PLOT TFP EVOLUTION (replicates Figure 1 in the paper)
# =============================================================================
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  p <- ggplot(painel_final, aes(x = Ano, y = ptf)) +
    geom_line(colour = "black", linewidth = 0.4) +
    geom_smooth(method = "lm", se = FALSE, colour = "steelblue", linewidth = 0.6) +
    facet_wrap(~ Estado, ncol = 6, scales = "free_y") +
    labs(x = "Ano", y = "PTF (log)",
         caption = "Linha azul: tendência linear. Fonte: elaboração própria.") +
    theme_minimal(base_size = 8)

  ggsave("output/fig1_ptf_evolution.pdf", p, width = 14, height = 10)
  message("Saved: output/fig1_ptf_evolution.pdf")
}
