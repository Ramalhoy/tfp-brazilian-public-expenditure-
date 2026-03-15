# =============================================================================
# 01_data_collection.R
# TFP and Public Expenditures in Brazilian States (2003–2018)
# Data collection from official Brazilian sources
# =============================================================================
# Required packages:
#   sidrar     – IBGE SIDRA API wrapper
#   ipeadatar  – IPEA Data API wrapper
#   comexstatr – ComexStat trade API wrapper (or manual download)
#   readxl, dplyr, tidyr, stringr, lubridate
#
# Install once:
#   install.packages(c("sidrar","ipeadatar","comexstatr",
#                      "readxl","dplyr","tidyr","stringr","lubridate"))
# =============================================================================

rm(list = ls())

library(sidrar)
library(ipeadatar)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# ---- Helper: state codes (IBGE) used across sources -------------------------
state_codes <- c(
  "11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
  "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL",
  "28"="SE","29"="BA","31"="MG","32"="ES","33"="RJ","35"="SP",
  "41"="PR","42"="SC","43"="RS","50"="MS","51"="MT","52"="GO"
  # "53"="DF" excluded (missing data for some variables)
)
anos <- 2003:2018

# =============================================================================
# 1. STATE GDP (PIB) – IBGE Contas Regionais  [SIDRA table 5938]
# =============================================================================
# Values in R$ thousand, current prices – deflation applied in 02_deflation.R
pib_raw <- sidrar::get_sidra(
  api = "/t/5938/n3/all/v/37/p/2003,2004,2005,2006,2007,2008,2009,2010,
         2011,2012,2013,2014,2015,2016,2017,2018/c11255/90707/d/v37%202"
)

pib <- pib_raw %>%
  transmute(
    Estado = `Unidade da Federação (Código)`,
    Ano    = as.integer(`Ano`),
    PIB    = as.numeric(`Valor`)         # R$ thousand
  ) %>%
  filter(Estado %in% names(state_codes)) %>%
  mutate(Estado = state_codes[Estado],
         PIB    = PIB * 1e3)             # convert to R$

# =============================================================================
# 2. INDUSTRIAL VALUE ADDED SHARE (VAK) – IBGE Contas Regionais [SIDRA 5938]
#    Variable: value added of industry / GDP
# =============================================================================
# Code 90691 = Agropecuária; 90692 = Indústria; 90693 = Serviços
va_raw <- sidrar::get_sidra(
  api = "/t/5938/n3/all/v/37/p/2003-2018/c11255/90692,90707/d/v37%202"
)

va <- va_raw %>%
  transmute(
    Estado   = `Unidade da Federação (Código)`,
    Ano      = as.integer(`Ano`),
    Categoria = `Componentes do Produto Interno Bruto`,
    Valor     = as.numeric(`Valor`) * 1e3
  ) %>%
  filter(Estado %in% names(state_codes)) %>%
  mutate(Estado = state_codes[Estado]) %>%
  pivot_wider(names_from = Categoria, values_from = Valor) %>%
  rename(VA_Industria = 3, PIB_va = 4) %>%
  mutate(vak = VA_Industria / PIB_va)

# =============================================================================
# 3. EMPLOYED POPULATION (L) – IBGE SIDRA (PNAD/Census)
#    Table 6318: people employed aged 14+ (annual PNAD 2003–2015)
#    Table 6461: PNADC 2016–2018 (use with caution; methodological break)
# =============================================================================
# PNAD (2003–2015)
pop_ocu_pnad <- sidrar::get_sidra(
  api = "/t/6318/n3/all/v/10/p/2003-2015/c2/6794"
) %>%
  transmute(
    Estado = `Unidade da Federação (Código)`,
    Ano    = as.integer(`Ano`),
    L      = as.numeric(`Valor`) * 1e3
  ) %>%
  filter(Estado %in% names(state_codes)) %>%
  mutate(Estado = state_codes[Estado])

# PNADC (2016–2018) – annual average of quarterly data
pop_ocu_pnadc <- sidrar::get_sidra(
  api = "/t/6461/n3/all/v/4090/p/2016,2017,2018/c2/6794"
) %>%
  transmute(
    Estado = `Unidade da Federação (Código)`,
    Ano    = as.integer(`Ano`),
    L      = as.numeric(`Valor`) * 1e3
  ) %>%
  filter(Estado %in% names(state_codes)) %>%
  mutate(Estado = state_codes[Estado])

pop_ocupada <- bind_rows(pop_ocu_pnad, pop_ocu_pnadc)

# NOTE: A methodological correction factor may be needed at the 2015–2016
# break between PNAD and PNADC. A common approach is to scale PNADC values by
# the ratio of overlapping year estimates. See IBGE technical notes.

# =============================================================================
# 4. MEAN YEARS OF SCHOOLING – IPEA Data
#    Series: EDUC12ANEST  (mean years of schooling, 25+, by state)
# =============================================================================
educ_raw <- ipeadatar::ipeadata("EDUC12ANEST")

educ <- educ_raw %>%
  filter(
    year(date) %in% anos,
    !is.na(value),
    tcode %in% state_codes   # tcode carries state IBGE code as string
  ) %>%
  transmute(
    Estado = state_codes[tcode],
    Ano    = year(date),
    educ   = value
  )
# IPEA interpolates Census-based series; values in non-Census years are
# linear interpolations. Accept as-is (consistent with Ferreira 2010).

# =============================================================================
# 5. ACCESS TO SAFE WATER – IPEA Data
#    Series: IBGE_CSAN12  (% households with piped water, state level)
# =============================================================================
agua_raw <- ipeadatar::ipeadata("IBGE_CSAN12")

agua <- agua_raw %>%
  filter(year(date) %in% anos, tcode %in% state_codes) %>%
  transmute(
    Estado = state_codes[tcode],
    Ano    = year(date),
    agua   = value / 100     # convert percentage to proportion
  )

# =============================================================================
# 6. ELECTRICITY CONSUMPTION PER CAPITA – IPEA Data
#    Series: ANEEL_CONSPC  (GWh per capita, state level)
# =============================================================================
energ_raw <- ipeadatar::ipeadata("ANEEL_CONSPC")

energiapc <- energ_raw %>%
  filter(year(date) %in% anos, tcode %in% state_codes) %>%
  transmute(
    Estado    = state_codes[tcode],
    Ano       = year(date),
    energiapc = value
  )

# =============================================================================
# 7. POPULATION DENSITY – IBGE SIDRA + area
#    Table 202: total population by state (Census/estimates)
# =============================================================================
pop_raw <- sidrar::get_sidra(
  api = "/t/6579/n3/all/v/9324/p/2003-2018"
)

pop <- pop_raw %>%
  transmute(
    Estado = `Unidade da Federação (Código)`,
    Ano    = as.integer(`Ano`),
    Pop    = as.numeric(`Valor`)
  ) %>%
  filter(Estado %in% names(state_codes)) %>%
  mutate(Estado = state_codes[Estado])

# State areas (km²) – fixed, from IBGE
state_area <- tribble(
  ~Estado, ~area_km2,
  "RO",238512.8, "AC",164123.7, "AM",1559159.1, "RR",224301.0, "PA",1247954.7,
  "AP",142828.5, "TO",277720.5, "MA",331983.3,  "PI",251616.8, "CE",148920.5,
  "RN",52811.1,  "PB",56469.8,  "PE",98149.1,   "AL",27778.5,  "SE",21926.4,
  "BA",564732.5, "MG",586522.1, "ES",46095.6,   "RJ",43780.2,  "SP",248219.5,
  "PR",199307.9, "SC",95730.7,  "RS",281748.5,  "MS",357145.6, "MT",903357.9,
  "GO",340111.8
)

popdens <- pop %>%
  left_join(state_area, by = "Estado") %>%
  mutate(popdens = Pop / area_km2) %>%
  select(Estado, Ano, popdens)

# =============================================================================
# 8. TRADE OPENNESS – ComexStat (MDIC)
#    Download: https://www.gov.br/produtividade-e-comercio-exterior/pt-br/
#              assuntos/comercio-exterior/estatisticas/base-de-dados-bruta
#    File: EXP_COMPLETA.zip and IMP_COMPLETA.zip (annual, by state, FOB USD)
#    Convert USD to BRL using BCB exchange rate series (SGS 3698)
# -----------------------------------------------------------------------------
# Because ComexStat bulk files are large (~500 MB), the recommended workflow
# is to download them once and filter. The code below assumes the files have
# been extracted to data/raw/.

message("NOTE: Download EXP_COMPLETA.zip and IMP_COMPLETA.zip from ComexStat")
message("      https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/")
message("      Place extracted CSVs in data/raw/comexstat/")

# Example stub – replace with actual paths after download:
# exp <- read.csv2("data/raw/comexstat/EXP_COMPLETA.csv") %>%
#   filter(CO_ANO %in% anos) %>%
#   group_by(Estado = SG_UF_NCM, Ano = CO_ANO) %>%
#   summarise(EXP_USD = sum(VL_FOB, na.rm=TRUE), .groups="drop")
#
# imp <- read.csv2("data/raw/comexstat/IMP_COMPLETA.csv") %>%
#   filter(CO_ANO %in% anos) %>%
#   group_by(Estado = SG_UF_NCM, Ano = CO_ANO) %>%
#   summarise(IMP_USD = sum(VL_FOB, na.rm=TRUE), .groups="drop")
#
# # Exchange rate: BCB SGS series 3698 (USD/BRL annual average)
# exrate <- ipeadatar::ipeadata("BCB_CAMBIO1") %>%
#   filter(year(date) %in% anos) %>%
#   transmute(Ano = year(date), exrate = value) %>%
#   group_by(Ano) %>% summarise(exrate = mean(exrate))
#
# trade_raw <- exp %>%
#   left_join(imp, by = c("Estado","Ano")) %>%
#   left_join(exrate, by = "Ano") %>%
#   mutate(EXP_BRL = EXP_USD * exrate,
#          IMP_BRL = IMP_USD * exrate)
#
# trade <- trade_raw %>%
#   left_join(pib, by = c("Estado","Ano")) %>%
#   mutate(trade = (EXP_BRL + IMP_BRL) / PIB) %>%
#   select(Estado, Ano, trade)

# =============================================================================
# 9. PUBLIC EXPENDITURE BY FUNCTION – STN / SICONFI (FINBRA)
#    Source: https://siconfi.tesouro.gov.br/siconfi/pages/public/consulta_finbra/
#    Download annual "Despesas por Função" tables for all states (2003–2018).
#    File format: CSV with separator ";"
#
#    Function codes used (GND 3 = Outras Despesas Correntes + Capital, OR total):
#      01 = Legislativa          02 = Judiciária
#      04 = Administração        08 = Assistência Social + Previdência
#      10 = Saúde                12 = Educação
#      22 = Indústria/Comércio   19 = Ciência e Tecnologia
# =============================================================================
message("NOTE: Download FINBRA state expenditure files (2003–2018) from:")
message("      https://siconfi.tesouro.gov.br/siconfi/pages/public/")
message("      consulta_finbra/finbra_list.jsf")
message("      Select: Demonstrativo de Despesas por Função, Estados, all years")
message("      Place files in data/raw/finbra/ named finbra_YYYY.csv")

# Stub for reading FINBRA CSVs:
# read_finbra <- function(year) {
#   path <- sprintf("data/raw/finbra/finbra_%d.csv", year)
#   df <- read.csv2(path, fileEncoding = "latin1") %>%
#     filter(Esfera == "E",                          # E = Estado
#            !UF %in% c("DF","BR")) %>%
#     select(Estado = UF, Funcao = Cod.Funcao,
#            Despesa = Liquidadas..R..) %>%
#     mutate(Ano     = year,
#            Despesa = as.numeric(gsub("[^0-9,]","", Despesa)) / 100)
# }
# finbra_all <- bind_rows(lapply(2003:2018, read_finbra))
#
# # Pivot to wide format
# desp_wide <- finbra_all %>%
#   mutate(varname = case_when(
#     Funcao == "01" ~ "despleg",
#     Funcao == "02" ~ "despjud",
#     Funcao == "04" ~ "despadm",
#     Funcao == "08" ~ "despass",
#     Funcao == "10" ~ "despsaude",
#     Funcao == "12" ~ "despeduc",
#     Funcao == "22" ~ "despics",
#     Funcao == "19" ~ "desptec",
#     TRUE           ~ NA_character_
#   )) %>%
#   filter(!is.na(varname)) %>%
#   group_by(Estado, Ano, varname) %>%
#   summarise(Despesa = sum(Despesa, na.rm=TRUE), .groups="drop") %>%
#   pivot_wider(names_from = varname, values_from = Despesa)
#
# # Total expenditure (for proportion variables)
# desp_total <- finbra_all %>%
#   group_by(Estado, Ano) %>%
#   summarise(despgov = sum(Despesa, na.rm=TRUE), .groups="drop")
#
# desp_wide <- desp_wide %>%
#   left_join(desp_total, by=c("Estado","Ano")) %>%
#   mutate(across(starts_with("desp"),
#                 .names = "{.col}p",
#                 ~ .x / despgov))

# =============================================================================
# 10. NATIONAL INVESTMENT SHARE (for perpetual inventory)
#     IBGE National Accounts: Gross Fixed Capital Formation / GDP
#     IPEA series: GAC12_FBKFPIB12  or  SCN_FBKF
# =============================================================================
inv_share_raw <- ipeadatar::ipeadata("GAC12_FBKFPIB12")

inv_share <- inv_share_raw %>%
  filter(year(date) %in% c((min(anos)-5):max(anos))) %>%
  transmute(Ano = year(date), share_inv = value / 100) %>%
  group_by(Ano) %>%
  summarise(share_inv = mean(share_inv))

# =============================================================================
# 11. MERGE ALL SERIES
# =============================================================================
painel_final <- pib %>%
  left_join(pop_ocupada,  by = c("Estado","Ano")) %>%
  left_join(educ,         by = c("Estado","Ano")) %>%
  left_join(agua,         by = c("Estado","Ano")) %>%
  left_join(energiapc,    by = c("Estado","Ano")) %>%
  left_join(popdens,      by = c("Estado","Ano")) %>%
  left_join(va %>% select(Estado, Ano, vak), by = c("Estado","Ano")) %>%
  # left_join(trade,       by = c("Estado","Ano")) %>%   # uncomment after ComexStat step
  # left_join(desp_wide,   by = c("Estado","Ano")) %>%   # uncomment after FINBRA step
  left_join(inv_share,    by = "Ano")

# Save raw (nominal) panel for deflation step
saveRDS(painel_final, "data/interim/painel_nominal.rds")
message("Saved: data/interim/painel_nominal.rds")
