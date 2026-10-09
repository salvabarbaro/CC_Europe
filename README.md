# Cigarette Prices and Illicit Cigarette Consumption in Europe

## Overview

This repository contains R code and data for an empirical analysis of the relationship between cigarette prices and illicit cigarette consumption (CC) across European countries.

The analysis focuses on the role of domestic cigarette prices relative to prices in other European countries, accounting for country-specific characteristics and common time effects.

## Data

The analysis combines several data sources:

- **KPMG:** Country-level estimates of illicit cigarette consumption (CC).
- **Eurostat:** Cigarette prices (weighted average price, WAP) and population data.
- **WHO:** Tobacco smoking prevalence.
- **CEPII:** Bilateral geographic distances used to construct distance-weighted foreign cigarette prices.

Preprocessed datasets are stored as `.rds` files in the `data/` directory. The analysis additionally requires `Europe_comparison.xlsx` in the repository's root directory.

## Empirical Analysis

The main specifications use panel regressions with country and year fixed effects and standard errors clustered at the country level.

The analyses include:

- Relative domestic-to-foreign cigarette prices.
- Separate domestic and foreign price effects.
- Market size as a control variable.
- Alternative log-linear and log-log specifications.
- Lagged price effects (0–4 years).
- Robustness checks, including nonlinear specifications and COVID-19 effects.

Foreign cigarette prices are calculated using inverse-squared geographic distance weights.

## Reproducibility

The main analysis is implemented in `main.R`.

Run the script from the repository's root directory:

```r
source("main.R")
```

Required R packages include `tidyverse`, `readxl`, `countrycode`, `fixest`, `modelsummary`, `cepiigeodist`, `ggrepel`, `giscoR`, `jsonlite`, `purrr`, and `car`.

The script produces regression estimates, statistical tests, and model comparison tables.
