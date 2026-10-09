library(tidyverse)
library(haven)

eb25.df <- read_dta("~/Documents/Research/Maike/Eurobarometer/ZA9129_v1-0-0.dta")
france.df <- eb25.df %>% filter(., isocntry == "FR")
### left-right: d1
print_labels(france.df$d1)
prop.table(table(france.df$d1[france.df$d1 %in% 1:10]))
hist(france.df$d1[france.df$d1 %in% 1:10])
### educational attainment level: d9a
print_labels(france.df$d9a)
prop.table(table(france.df$d9a, useNA = "always"))
