###########################
library(tidyverse)
library(jsonlite)

###########################
# get WHO data on prevalences
## start lines that explain who who.tobacco is generated
who_indicator <- function(code) {
  url <- paste0("https://ghoapi.azureedge.net/api/",code)
  fromJSON(url)$value %>%
    as_tibble() %>% mutate(indicator = code)
}
#
codes <- c(
 tobacco   = "M_Est_tob_curr_std",
  smoking   = "M_Est_smk_curr_std",
  cigarette = "M_Est_cig_curr_std"
)
#
who.tobacco <- map_dfr(codes, who_indicator, .id = "type" )
#saveRDS(who.tobacco, file = "data/who_tobacco.RDS")
## end lines to explain who.tobacco generation
## comment above lines to directly import data with:
who.tobacco <- readRDS(file = "data/who_tobacco.RDS")

who.df <- who.tobacco %>%
  rename(
    "iso3c"  = "SpatialDim",
    "gender" = "Dim1",
    "year"   = "TimeDimensionValue"
  ) %>%
  dplyr::select(
    c("IndicatorCode", "iso3c", "gender", "year", "NumericValue")) %>% 
   mutate(
    iso2c = countrycode(iso3c, origin = "iso3c", destination = "iso2c", warn = F ), 
    .after = "iso3c" ) %>%
  mutate(year = as.integer(year))

rm(who.tobacco)
# interpolation of missing years
who.imp <- who.df %>%
  group_by(IndicatorCode, iso3c, iso2c, gender) %>%
  complete(year = 2015:2022) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    imputed      = is.na(NumericValue),
    NumericValue = approx(year, NumericValue, xout = year, rule = 1)$y
  ) %>%
  ungroup()

who.df <- who.imp
rm(who.imp)
saveRDS(who.df, file = "data/WHO_imp.rds")
