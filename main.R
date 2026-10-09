### Regressions and other analyses to explain the CC-shares
library(tidyverse)
library(readxl)
library(countrycode)
library(ggrepel)
library(giscoR)
library(jsonlite)
library(purrr)
library(fixest)
library(modelsummary)
library(cepiigeodist)


## Get and merge data
## Basic df and pivot_longer
df <- read_xlsx(path = "Europe_comparison.xlsx") %>%
  mutate(across(-1, as.numeric)) %>%
  mutate(iso2c = countrycode(Country, origin = "country.name", destination = "iso2c" ) )

df.long <- df %>%
  pivot_longer(
    cols = matches("^(CC|TCS|WAP|Tax|never)_?\\d{4}$"),
    names_to = c(".value", "year"),
    names_pattern = "^(CC|TCS|WAP|Tax)_?(\\d{4})$"
  ) %>%
  mutate(year = as.integer(year)) %>%
  dplyr::select(-starts_with("seized")) %>%
  dplyr::select(c("Country", "iso2c", "year", "CC", "TCS")) %>%
  filter(., year < 2026)    ### adjust in future year

rm(df)

# add WHO data on smoking prevalence (a covariate)
who.df <- readRDS("data/WHO_imp.rds")   ## see: code/WHO_Data.R

df2 <- df.long %>% 
  left_join(x = ., y = who.df, by = c("iso2c", "year")) %>%
  rename("tob.prev" = "NumericValue") %>%
  filter(., gender == "SEX_BTSX", IndicatorCode == "M_Est_tob_curr_std")

rm(df.long, who.df)

# add data on cigarette prices ## see Eurostat.R
wap.df <- readRDS("data/eurostatWAP.rds") %>%
  mutate(geo = ifelse(geo=="UK", "GB", geo)) %>%
  mutate(iso2c = geo) 


df3 <- df2 %>% 
  left_join(x = ., 
    y = wap.df %>% dplyr::select(., c("iso2c", "geo", "year", "wap_pkg", "ipe_wap")),
    by = c("iso2c", "year")  )

rm(df2, wap.df)

### add distances and calculate weighed foreign prices
## adjust iso3c codes to comply with cepii (cepiigeodist)
#data(dist_cepii) 
ctry <- df3 %>% distinct(iso2c) %>%
  mutate(iso3 = countrycode(iso2c, "iso2c", "iso3c"),
         iso3 = recode(iso3, ROU = "ROM", SRB = "YUG"))      # CEPII codes  # Bei Bedarf dplyr::recode
# test: 
length(intersect(ctry$iso3, unique(dist_cepii$iso_o)))
######
#https://www.cepii.fr/cepii/en/bdd_modele/bdd_modele.asp
#head(dist_cepii) dataset comes with library
# Note: distw := Weighted distance (pop-wt, km) with theta=1 (theta measures the sensitivity of trade flows to bilateral distance dkl)
#       contig := Variable coded as 1 when the two countries are next to each other, 0 otherwise
# see ?dist_cepii
## in case you encounter an issue with hte package:
#dist_cepii <- readRDS("data/dist_cepii.RDS")

w_all <- dist_cepii %>%
  filter(iso_o != iso_d) %>%    # removes lines like iso_o = iso_d == "ITA"
  inner_join(x = ., y = ctry %>% dplyr::select(iso_o = iso3, iso2c),           by = "iso_o") %>%
  inner_join(x = ., y = ctry %>% dplyr::select(iso_d = iso3, iso2c_j = iso2c), by = "iso_d") %>%
  dplyr::select(iso2c, iso2c_j, contig, distw) %>%
  mutate(
    contig = suppressWarnings(as.numeric(as.character(contig))),
    distw  = suppressWarnings(as.numeric(as.character(distw)))
  )

anti_join(x = ctry, y = w_all, by = "iso2c")  ## 2nd test on completeness. anti_join should be empty
#
#w_contig <- w_all %>% filter(contig == 1) %>% ## keeps neighbours
#  mutate(iso2c, iso2c_j, w_ij = 1, .keep = "none")
w_dist <- w_all %>% 
  mutate(w_ij = 1 / distw^2, sqrt.w_ij = 1/distw) %>% 
  dplyr::select(iso2c, iso2c_j, w_ij, sqrt.w_ij)
# Note on w_dist --> w_ij:
# squared-distance weights (distw = population-weighted distance in km, CEPII).
# Twice the distance -> a quarter of the weight, so nearby countries dominate wap_nb.
# weighted.mean() normalises weights, so only relative size within country i matters.
rm(w_all, ctry)
#########################################################################################
## first make_nb: consider 1/wdist (== sqrt.w_ij) instead of w_ij
#make_nb <- function(w) {
#  w %>%
#    inner_join(x = ., y = distinct(df3, iso2c_j = iso2c, year, wap_j = wap_pkg),
#               by = "iso2c_j", relationship = "many-to-many") %>%
#    group_by(iso2c, year) %>%
#    summarise(wap_nb = weighted.mean(wap_j, sqrt.w_ij, na.rm = TRUE), .groups = "drop")
#}

make_nb <- function(w) {
  w %>%
    inner_join(x = ., y = distinct(df3, iso2c_j = iso2c, year, wap_j = wap_pkg),
               by = "iso2c_j", relationship = "many-to-many") %>%
    group_by(iso2c, year) %>%
    summarise(wap_nb = weighted.mean(wap_j, w_ij, na.rm = TRUE), .groups = "drop")
}
## we next add the variable wap_nb into the dataset! Hint: IE and GB separately
d_nb <- df3 %>% 
  left_join(x = ., y = make_nb(w_dist), by = c("iso2c", "year")) 
rm(df3, w_dist, make_nb)
###
# add population sizes
pop.df <- readRDS("data/popsize.RDS") %>% 
  dplyr::select(c("geo", "year", "values")) %>%
  rename("pop_size" = "values")
d_pop <- d_nb %>% 
  left_join(x = ., y = pop.df, by = c("geo", "year") ) %>%
  mutate(market_size = 0.00001 *tob.prev * pop_size)
d_nb <- d_pop
rm(d_pop)
###################################################################################################
#### Regression analyses  #########################################################################
###################################################################################################
fml.list <- list(
  "main"  = CC ~ log(wap_pkg / wap_nb) + log(market_size) | Country + year,
  "RC_01" = CC ~ log(wap_pkg) + log(wap_nb) + log(market_size) | Country + year,
  "RC_02" = CC ~ log(wap_pkg) + log(market_size) | Country + year)

feols.fun <- function(f){
  fixest::feols(
    fml = f, 
    data = d_nb,  #%>% filter(., !geo == "GB")
    vcov =~Country  )   #
}

feols.res <- lapply(fml.list, feols.fun)

#performance::check_model(feols.res$main)

etable(feols.res)
esttex(feols.res, fixef.group = T)
cm <- c(
  "log(wap_pkg/wap_nb)" = "Relative Log-Price",
  "log(wap_pkg)" = "WAP (log)", "log(wap_nb)" = "WAP (other, log)", 
  "tob.prev" = "WHO-prevalence", "log(market_size)" = "Market size (log)"
)

modelsummary(feols.res, stars = T, 
  vcov = ~Country,
  coef_map = cm, 
  gof_omit = "AIC|BIC|RMSE" 
)


###log-log analyses mr
fml.log.list <- list(
  "main.log"  = log(CC) ~ log(wap_pkg / wap_nb) + log(market_size) | Country + year,
  "RC_01.log" = log(CC) ~ log(wap_pkg) + log(wap_nb) + log(market_size) | Country + year,
  "RC_02.log" = log(CC) ~ log(wap_pkg) + log(market_size) | Country + year)

feols.fun <- function(f){
  fixest::feols(
    fml = f, 
    data = d_nb,  #%>% filter(., !geo == "GB")
    vcov =~Country  )   #
}

feols.res.log <- lapply(fml.log.list, feols.fun)

etable(feols.res.log)
esttable(feols.res.log, view = T)
esttex(feols.res.log, fixef.group = T)
cm <- c(
  "log(wap_pkg/wap_nb)" = "Relative Log-Price",
  "log(wap_pkg)" = "WAP (log)", 
  "log(wap_nb)" = "WAP (other, log)", 
  "tob.prev" = "WHO-prevalence", 
  "log(market_size)" = "Market size (log)",
  "l(log(wap_pkg/wap_nb), 2)" = "Relative Log-Price",
  "l(log(wap_nb), 2)" = "WAP (other, log)",
  "l(log(wap_pkg), 2)" = "WAP (log)"
)

modelsummary(feols.res.log, stars = T, 
  vcov = ~ Country,
  coef_map = cm, 
  gof_omit = "AIC|BIC|RMSE" 
)

####mr ende


## Additional analyses (AA):
# AA.1 Run a Wald test (linear-hypothesis-test) on main specification: log(wap_pkg) + log(wap_other) = 0
car::linearHypothesis(
  feols.res$RC_01,
  "log(wap_pkg) + log(wap_nb) = 0",
  vcov. = vcov(feols.res$RC_01)
)

## AA.2: Covid-shock: 
covid <- feols(CC ~ log(wap_pkg / wap_nb) + tob.prev | Country + year,
      data = d_nb %>% filter(!year %in% c(2020, 2021)), vcov = ~Country)
summary(covid)
## covid with interaction
covid2 <- feols(CC ~ log(wap_pkg / wap_nb) + log(wap_pkg / wap_nb):I(year %in% 2020:2021) + tob.prev | Country + year,
      d_nb, vcov = ~Country)
summary(covid2)
rm(covid, covid2)
## interaction term small and insignificant: results not driven by covid-years.
###################################################################################################################
## Explaining the differences between main and RC01:
feols(CC ~ I(log(wap_pkg) - log(wap_nb)) + log(wap_nb) + tob.prev | Country + year, d_nb, vcov = ~Country) %>% summary(.)
## Adding a non-linear element:
main.q <- feols(CC ~ log(wap_pkg / wap_nb) + I(log(wap_pkg / wap_nb)^2) + tob.prev | Country + year,
  cluster = ~Country, data = d_nb )
etable(main.q)
wald(main.q, keep = "I\\(")


### Model with time-lag
## define panel structure for fixest::
d_nb <- fixest::panel(d_nb, panel.id = ~ Country + year )

fml.list_lag <- list(
  "main"  = CC ~ l(log(wap_pkg / wap_nb), 2) +
    log(market_size) | Country + year,

  "RC_01" = CC ~ l(log(wap_pkg), 2) +
    l(log(wap_nb), 2) +
    log(market_size) | Country + year,

  "RC_02" = CC ~ l(log(wap_pkg), 2) +
    log(market_size) | Country + year
)

feols.fun <- function(f){
  fixest::feols(
    fml  = f,
    data = d_nb,
    vcov = ~ Country
  )
}

feols.res_lag <- lapply(fml.list_lag, feols.fun)
esttex(feols.res_lag, fixef.group = T)

cm <- c(
  "log(wap_pkg/wap_nb)" = "Relative Log-Price",
  "log(wap_pkg)" = "WAP (log)", 
  "log(wap_nb)" = "WAP (other, log)", 
  "tob.prev" = "WHO-prevalence", 
  "log(market_size)" = "Market size (log)",
  "l(log(wap_pkg/wap_nb), 2)" = "Relative Log-Price",
  "l(log(wap_nb), 2)" = "WAP (other, log)",
  "l(log(wap_pkg), 2)" = "WAP (log)",
  "l(log(wap_pkg/wap_nb), 1)" = "Relative Log-Price",
  "l(log(wap_nb), 1)" = "WAP (other, log)",
  "l(log(wap_pkg), 1)" = "WAP (log)"
)


modelsummary(
  list(
    "Main" = feols.res[[1]], 
    "RC1"  = feols.res[[2]],
    "Time Lag (Main)" =feols.res_lag[[1]], 
    "Time Lag (RC1)" =feols.res_lag[[2]],
        "Log-Log" = feols.res.log[[1]]
  ),
  stars = T,
  vcov = ~ Country,
  coef_map = cm, 
  gof_omit = "AIC|BIC|RMSE" 
)

car::linearHypothesis(
  feols.res_lag$RC_01,
  "l(log(wap_pkg), 2) + l(log(wap_nb), 2) = 0",
  vcov. = vcov(feols.res_lag$RC_01)
)


## Test for different time lags
# Panel structure
# Generate Lags
d_nb <- panel(d_nb, panel.id = ~ Country + year)
## hint: we use dplyr::lag() in order to avoid conflicts with fixest::lag()
d_nb <- d_nb %>%
  group_by(Country) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    rel_price = log(wap_pkg / wap_nb),
    lag0 = rel_price,
    lag1 = dplyr::lag(rel_price, 1),
    lag2 = dplyr::lag(rel_price, 2),
    lag3 = dplyr::lag(rel_price, 3),
    lag4 = dplyr::lag(rel_price, 4)
  ) %>%
  ungroup()

# Merge alltogther
d_lags <- d_nb %>%
  filter(if_all(c(CC, lag0:lag4, market_size),
                ~ !is.na(.x)),
         market_size > 0)

# Regressions
models <- lapply(0:4, function(k) {
  feols(
    as.formula(paste0(
      "CC ~ lag", k,
      " + log(market_size) | Country + year"
    )),
    data = d_lags,
    vcov = ~ Country
  )
})

names(models) <- paste0("Lag ", 0:4)

# lag = 1 and lag = 2 better. 

modelsummary(models)


