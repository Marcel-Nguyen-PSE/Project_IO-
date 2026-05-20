library(tidyverse)
library(readxl)
library(rio)
library(xtable)
library(here)
library(gtsummary)
library(glue)
library(scales)
library(patchwork)
library(stargazer)
library(sandwich)
library(lmtest)
library(AER)
library(car)
library(haven)
library(fixest) 
library(sf)
library(did)
library(rdrobust)
library(TwoWayFEWeights)
library(Synth)
library(fredr)
library(typstable)
library(broom)

df <- read_dta('Data/cement-steen-roller.dta')
df_price_us <- read_csv('Data/WPU132_PCH-2.csv')

df_price_us <- df_price_us %>%
  mutate(year = as.numeric(year(observation_date))) %>%
  select(-observation_date)

df_pct <- df %>%
  select(year, pn, pexp) %>%
  mutate(pct_change = 100 * (pexp / lag(pexp)-1)) %>%
  inner_join(df_price_us, by = 'year')