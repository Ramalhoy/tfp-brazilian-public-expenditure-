# TFP and Public Expenditures in Brazilian States (2003–2018)

**Bachelor's thesis** — Department of Economics, Federal University of Espírito Santo (UFES)  
**Author:** Yago Ramalho Silva | **Advisor:** Prof. Dr. Edson Zambon Monte | **Year:** 2023

---

## Overview

This project estimates **Total Factor Productivity (TFP)** for the 26 Brazilian states over 2003–2018 and evaluates the impact of **disaggregated public expenditures** (by government function) on those productivity levels, using a **two-way fixed effects panel data** approach.

The eight expenditure categories examined are:

| Function | Variable |
|---|---|
| Judiciary | `despjud` |
| Legislative | `despleg` |
| Administration & Planning | `despadm` |
| Education & Culture | `despeduc` |
| Industry, Commerce & Services | `despics` |
| Science & Technology | `desptec` |
| Social Assistance & Welfare | `despass` |
| Health | `despsaude` |

---

## Main Findings

- **Health** and **Judiciary** expenditure shares are the only categories with a statistically significant positive effect on state TFP (significant at 1%).
- All other expenditure categories show small and insignificant coefficients — consistent with the hypothesis that most public spending does not translate into productivity gains at the sub-national level.
- No expenditure category produced a negative and significant effect.
- Among control variables, **access to safe water**, **industrial value added share**, **energy consumption per capita**, and **population density** behave as expected by the literature.
- The negative coefficient on **years of schooling** is discussed in the paper as potentially arising from education quality issues and measurement choices — a result corroborated by other empirical studies on education-TFP links in developing countries.

---

## Methodology

### TFP Estimation

TFP is computed via the **perpetual inventory method** following Ferreira (2010), using a Cobb-Douglas production function with constant returns to scale:

$$A_{it} = \frac{y_{it}}{k_{it}^{\alpha} \cdot H_{it}^{\beta}}$$

where:
- $y_{it}$ = GDP per worker (state $i$, year $t$, R$ 2018)
- $k_{it}$ = physical capital stock per worker
- $H_{it} = \exp(0.1 \cdot h_{it})$ = Mincerian human capital index
- $\alpha = 0.4$, $\beta = 0.6$ (Gollin 2002)
- Depreciation rate $\delta = 5.96\%$; technical growth rate $g = 8.7\%$

### Econometric Model

$$\text{ptf}_{it} = \alpha_i + \lambda_t + \beta_1 \text{desp}_{it} + \beta_2 \text{vak}_{it} + \beta_3 \text{educ}_{it} + \beta_4 \text{pop}_{it} + \beta_5 \text{agua}_{it} + \beta_6 \text{trade}_{it} + \beta_7 \text{energ}_{it} + \epsilon_{it}$$

- **Estimator:** within (two-way fixed effects — state + year)
- **Standard errors:** White-Arellano HC0, clustered by state
- **Period:** 2003–2018, annual frequency, balanced panel ($N=26$, $T=16$, 416 obs.)

---

## Repository Structure

```
.
├── 01_data_collection.R      # Download from SIDRA-IBGE, IPEAdata, ComexStat, STN
├── 02_tfp_estimation.R       # Price deflation + TFP via perpetual inventory
├── 03_panel_regressions.R    # Two-way FE regressions + robust SE + tables
├── data/
│   ├── raw/                  # Raw downloads (not tracked in git — see note below)
│   │   ├── comexstat/        # EXP_COMPLETA.csv, IMP_COMPLETA.csv
│   │   └── finbra/           # finbra_YYYY.csv (STN/SICONFI)
│   └── interim/              # Intermediate .rds files produced by scripts
├── output/
│   ├── fig1_ptf_evolution.pdf
│   └── table3_results.tex
└── README.md
```

> **Data note:** Raw files from ComexStat (large bulk CSVs) and STN/FINBRA are not committed to this repository due to file size and licensing. See **Data Sources** below for direct download links.

---

## Data Sources

| Variable | Source | URL |
|---|---|---|
| State GDP | IBGE SIDRA (table 5938) | https://sidra.ibge.gov.br/tabela/5938 |
| Employed population | IBGE SIDRA / PNAD, PNADC | https://sidra.ibge.gov.br |
| Years of schooling | IPEAdata (`EDUC12ANEST`) | http://www.ipeadata.gov.br |
| Access to water | IPEAdata (`IBGE_CSAN12`) | http://www.ipeadata.gov.br |
| Electricity consumption p.c. | IPEAdata (`ANEEL_CONSPC`) | http://www.ipeadata.gov.br |
| Industrial value added share | IBGE Contas Regionais | https://sidra.ibge.gov.br/tabela/5938 |
| Trade openness | ComexStat / MDIC | https://comexstat.mdic.gov.br |
| Public expenditure by function | STN / SICONFI (FINBRA) | https://siconfi.tesouro.gov.br |
| GDP deflator | IPEAdata (`IBGE12_DEFPIBPM12`) | http://www.ipeadata.gov.br |
| Exchange rate USD/BRL | BCB SGS (series 3698) | https://www3.bcb.gov.br/sgspub |

---

## Requirements

```r
install.packages(c(
  "plm", "lmtest", "sandwich",     # panel econometrics
  "sidrar", "ipeadatar",           # data APIs
  "dplyr", "tidyr", "stringr",     # data wrangling
  "lubridate", "readxl",           # utilities
  "stargazer", "ggplot2"           # output
))
```

R version ≥ 4.1.0 is recommended.

---

## Replication

```bash
# 1. Clone the repository
git clone https://github.com/<your-username>/tfp-brazilian-states.git
cd tfp-brazilian-states

# 2. Create directories
mkdir -p data/raw/comexstat data/raw/finbra data/interim output

# 3. Download manual files (see Data Sources table above)
#    - ComexStat bulk CSVs → data/raw/comexstat/
#    - FINBRA annual CSVs  → data/raw/finbra/

# 4. Run scripts in order
Rscript 01_data_collection.R
Rscript 02_tfp_estimation.R
Rscript 03_panel_regressions.R
```

---

## Citation

```
Silva, Y. R. (2023). Produtividade Total dos Fatores e Despesas Públicas dos
Estados Brasileiros: Uma Aplicação de Dados em Painel. Bachelor's thesis,
Federal University of Espírito Santo (UFES), Vitória.
```

---

## References (selected)

- Ferreira, P. C. (2010). Eficiência e produtividade total dos fatores em Minas Gerais. *Ensaios Econômicos* EPGE, n. 705.
- Gollin, D. (2002). Getting income shares right. *Journal of Political Economy*, 110(2), 458–472.
- Elgin, C., & Çakir, S. (2015). Technological progress and scientific indicators. *Economics of Innovation and New Technology*, 24(3), 263–281.
- Neduziak, L. C. R., & Correia, F. M. (2017). Alocação dos gastos públicos e crescimento econômico. *Revista de Administração Pública*, 51(4).
- Yu, Y. et al. (2022). Investigating the role of health, education, energy and pollution for explaining TFP. *Humanities and Social Sciences Communications*, 9, 1–7.
