library(eurostat)
library(tidyverse)

tobacco <- get_eurostat(
  "prc_hicp_midx",
  time_format = "date"
)

es.nbs <- tobacco %>%
  distinct(coicop) %>%
  collect()

es.labels <- label_eurostat(tobacco) %>%
  distinct(coicop)

cig.df <- tobacco %>% filter(., coicop == "CP02201")


cig.year <- tobacco %>%
  filter(coicop == "CP02201") %>%
  mutate(year = year(TIME_PERIOD)) %>%
  group_by(geo, year) %>%
  summarise(
    cig_price_index = mean(values, na.rm = TRUE),
    .groups = "drop"
  )

sort(unique(cig.year$geo))
to.remove <- c("EA", "EA19", "EA20", "EEA", "EU", "EU27_2020", "EU28")
cig_year.df <- cig.year %>% filter(., !geo %in% to.remove)
rm(cig.year)

#take WAP-values from web-scrapped data (origin: Europ Comm)
ipe_wap.df <- read.csv("data/WAP_adj.csv", header = T) %>%
  rename(year = Year) %>% 
  mutate(wap_pkg = wap_eur_per_1000 / 1000 * 20) %>%
  dplyr::select(., c("iso2c", "year", "wap_eur_per_1000", "wap_pkg")) %>%
  mutate(geo = iso2c) %>%   # to avoid confusion across datasets
  mutate(geo = recode(geo, "GR" = "EL", "GB" = "UK"))


cig_year.df2 <- cig_year.df %>%
  filter(., year > 2015) %>%
  left_join(x = ., 
    y = ipe_wap.df %>% select(geo, year, wap_pkg),
    by = c("geo", "year")) %>%
  group_by(geo) %>%
  mutate(
    hicp_2024 = cig_price_index[year == 2024][1],
    wap_2024  = wap_pkg[year == 2024][1],
    price_est = wap_2024 * cig_price_index / hicp_2024
  ) %>% ungroup() %>%
  mutate(
    price_diff = wap_pkg - price_est,
    price_diff_pct = 100 * (wap_pkg / price_est - 1)
  ) %>%
  filter(., !geo == "AL") %>%
  mutate(Country = countrycode::countrycode(geo, 
    origin = "iso2c", destination = "country.name", 
    custom_match = c('EL' = 'Greece', 'UK'='GB', 'XK' = 'Kosovo'))) %>%
  mutate(price_est = ifelse(geo == "UK", wap_pkg, price_est)) %>%
  rename(., "ipe_wap" = "wap_pkg", "wap_pkg" = "price_est")

#cig_year.df2 <-  cig_year.df2 %>%
#  add_row(
#    geo = "UK",
#    year = 2021:2025,
#    wap_pkg = c(11.0, 12.10, 13.50, 15.40, 16.80)
#  )

cig_year.df2 %>% filter(., geo == "UK")


saveRDS(cig_year.df2, file = "data/eurostatWAP.rds")

#########################################################
#########################################################
pop<- get_eurostat("tps00001", time_format = "date") %>%
  mutate(year = lubridate::year(TIME_PERIOD))
saveRDS(pop, "data/popsize.RDS")
