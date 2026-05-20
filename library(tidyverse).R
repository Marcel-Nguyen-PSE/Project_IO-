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

df <- read_dta('/Users/marcel/Library/Mobile Documents/com~apple~CloudDocs/M1 APE/S2/IO/IO projects/Norvegian cartel/cement-steen-roller.dta')
df_price_us <- read_csv('/Users/marcel/Library/Mobile Documents/com~apple~CloudDocs/M1 APE/S2/IO/IO projects/Norvegian cartel/WPU132_PCH-2.csv')

df_price_us <- df_price_us %>%
  mutate(year = as.numeric(year(observation_date))) %>%
  select(-observation_date)

df_pct <- df %>%
  select(year, pn, pexp) %>%
  mutate(pct_change = 100 * (pexp / lag(pexp)-1)) %>%
  inner_join(df_price_us, by = 'year')

plot_us_nr_cem <- ggplot(df_pct, aes(x = year)) +
  geom_line(aes(y = pct_change, color = "Norwegian Cement Price"), size = 1) +
  geom_line(aes(y = WPU132_PCH, color = "U.S. Cement Price"), size = 1) +
  geom_vline(xintercept = 1968, linetype = "dashed", color = "black", linewidth = 0.8) +
  labs(x = "Year", y = "Percent change", color = "Series") +
  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.position = c(0.98, 0.98),
    legend.position.inside = c(1, 1),      # top-right
    legend.justification = c(1, 1)  # anchor to corner
  ) 

plot_us_nr_cem

ggsave('plot of US v. Norway cement price.jpeg', plot = plot_us_nr_cem, width = 6.5, height = 4, units = "in", dpi = 300)

df <- df %>%
  mutate(
    pmr = pm/cpi*100,
    per = pe/cpi*100,
    wayr = way/cpi*100,
    pexpr = pexp/cpi*100,
    pnr = pn/cpi*100,
  )

plot_demand_prod_nor <- ggplot(data = df, aes(x = year)) + 
  geom_line(aes(y = log(y), color = 'Domestic Consumption')) + 
  geom_line(aes(y = log(qn), color = 'Domestic Production')) + theme_minimal() +
  labs(x = "Year", y = "Linearised Variables", color = "Series") +
  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.position = c(0.98, 0.98),
    legend.position.inside = c(1, 1),      # top-right
    legend.justification = c(1, 1)  # anchor to corner
  ) 

ggsave('plot of demand and production norway.jpeg', plot = plot_demand_prod_nor, width = 6.5, height = 4, units = "in", dpi = 300)

df_pre <- df %>%
  filter(year <= 1968) %>%
  summarise(
    across(
      c(y, pnr, exp, pexpr, pmr, per, wayr, qn),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE),
        min  = ~min(.x, na.rm = TRUE),
        max  = ~max(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "_") %>% mutate(across(where(is.numeric), ~round(.x, 2)))



table_1_sum_stat_pre <- tt(df_pre, rownames = FALSE) 
tt_save(table_1_sum_stat_pre, 'table1_sum_stat_pre.typ')


df_post <- df %>%
  filter(year > 1968) %>%
  summarise(
    across(
      c(y, pnr, exp, pexpr, pmr, per, wayr, qn),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE),
        min  = ~min(.x, na.rm = TRUE),
        max  = ~max(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "_") %>% mutate(across(where(is.numeric), ~round(.x, 2))) %>% ungroup()



table_1_sum_stat_post <- tt(df_post, rownames = FALSE) 
tt_save(table_1_sum_stat_post, 'table1_sum_stat_post.typ')

df_pool <- df %>%
  summarise(
    across(
      c(y, pnr, exp, pexpr, pmr, per, wayr, qn),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE),
        min  = ~min(.x, na.rm = TRUE),
        max  = ~max(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "_") %>% mutate(across(where(is.numeric), ~round(.x, 2)))



table_1_sum_stat_pool <- tt(df_pool, rownames = FALSE) 
tt_save(table_1_sum_stat_pool, 'table1_sum_stat_pool.typ')


max_lag <- 6

# Prepare data

df_lags <- df %>%

  arrange(year)

# Create price lags

for (k in 1:max_lag) {

  df_lags[[paste0("pnr_lag", k)]] <- dplyr::lag(df_lags$pnr, k)

}

# Function to estimate AR(k) model and BG test residual autocorrelation

test_price_lags <- function(k) {

  

  lag_vars <- paste0("pnr_lag", 1:k)

  

  formula_k <- as.formula(

    paste("pnr ~", paste(lag_vars, collapse = " + "))

  )

  

  model_k <- lm(formula_k, data = df_lags)

  

  bg <- bgtest(model_k, order = k)

  

  tibble(

    lag_order = k,

    AIC = AIC(model_k),

    BIC = BIC(model_k),

    BG_p_value = bg$p.value,

    adj_R2 = summary(model_k)$adj.r.squared

  )

}

# Run for lag orders 1 to max_lag

lag_results <- bind_rows(lapply(1:max_lag, test_price_lags))

lag_results <- lag_results %>% mutate(across(where(is.numeric), ~round(.x, 2)))

tt_save(tt(lag_results, rownames = FALSE), 'tabel_price_auto.typ')






for (k in 1:max_lag) {

  df_lags[[paste0("byan_lag", k)]] <- dplyr::lag(df_lags$byan, k)

}

test_byan_lags <- function(k) {

  

  lag_vars <- paste0("byan_lag", 1:k)

  

  formula_k <- as.formula(

    paste("byan ~", paste(lag_vars, collapse = " + "))

  )


  

  model_k <- lm(formula_k, data = df_lags)

  

  bg <- bgtest(model_k, order = k)

  

  tibble(

    lag_order = k,

    AIC = AIC(model_k),

    BIC = BIC(model_k),

    BG_p_value = bg$p.value,

    adj_R2 = summary(model_k)$adj.r.squared

  )

}

lag_results_byan <- bind_rows(lapply(1:max_lag, test_byan_lags))

lag_results_byan <- lag_results_byan %>% mutate(across(where(is.numeric), ~round(.x, 2)))

tt_save(tt(lag_results_byan, rownames = FALSE), 'tabel_byan_auto.typ')



for (k in 1:max_lag) {

  df_lags[[paste0("qn_lag", k)]] <- dplyr::lag(df_lags$qn, k)

}

test_qn_lags <- function(k) {

  

  lag_vars <- paste0("qn_lag", 1:k)

  

  formula_k <- as.formula(

    paste("qn ~", paste(lag_vars, collapse = " + "))

  )

  

  model_k <- lm(formula_k, data = df_lags)

  

  bg <- bgtest(model_k, order = k)

  

  tibble(

    lag_order = k,

    AIC = AIC(model_k),

    BIC = BIC(model_k),

    BG_p_value = bg$p.value,

    adj_R2 = summary(model_k)$adj.r.squared

  )

}

lag_results_qn <- bind_rows(lapply(1:max_lag, test_qn_lags))

lag_results_qn <- lag_results_qn %>% mutate(across(where(is.numeric), ~round(.x, 2)))

tt_save(tt(lag_results_qn, rownames = FALSE), 'tabel_qn_auto.typ')


df <- df %>%

  arrange(year) %>%

  mutate(

    pnr_lag = lag(pnr),

    qn_lag = lag(qn),

    byan_lag = lag(byan)

  )

iv_model <- ivreg(

  pnr ~ qn + byan + pnr_lag + qn_lag + byan_lag |
    byan + pnr_lag + qn_lag + byan_lag +
    wayr + pmr + per + pexpr,
  data = df

)
summary(iv_model, diagnostics = TRUE)

stargazer(iv_model, type = 'latex')

df <- df %>%
  arrange(year) %>%   
  mutate(
    qn_lag = lag(qn, 1),
    R_lag  = lag(pexpr, 1)
  )
 

summary(iv_model_exp, diagnostics = TRUE)

stargazer(iv_model_exp, type = 'latex')




df <- df %>%

  mutate(

    log_pnr = log(pnr),

    log_qn  = log(qn),

    period  = ifelse(year < 1968, "Before 1968", "After 1968")

  )

plot_reg_dem <- ggplot(df, aes(x = log_qn, y = log_pnr, color = period)) +

  geom_point() +

  geom_smooth(method = "lm", se = FALSE) +

  labs(

    x = "log(Quantity)",

    y = "log(Price)",

    color = ""

  ) +

  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.position = c(0.98, 0.98),
    legend.position.inside = c(0, 1),      # top-right
    legend.justification = c(1, 1)  # anchor to corner
  ) 

ggsave('reg-price_qn.jpeg', plot = plot_reg_dem, width = 6.5, height = 4, units = "in", dpi = 300)


df <- df %>%

  mutate(

    log_pnr = log(pnr),

    log_qn = log(qn),

    post1968 = ifelse(year >= 1968, 1, 0)

  )

iv_split <- ivreg(

  pnr ~ qn * post1968 + byan + pnr_lag + qn_lag + byan_lag |
    byan + pnr_lag + qn_lag + byan_lag +
    wayr + pmr + per + pexpr +
    post1968 + post1968:wayr + post1968:pmr + post1968:per + post1968:pexpr,

  data = df

)

summary(iv_split, diagnostics = TRUE)

diag <- summary(iv_split, diagnostics = TRUE)$diagnostics

diag_table <- data.frame(

  Test = rownames(diag),

  Statistic = round(diag[, "statistic"], 3),

  P_Value = round(diag[, "p-value"], 4)

)

print(diag_table)

extract_iv_diagnostics <- function(model, model_name) {

  diag <- summary(model, diagnostics = TRUE)$diagnostics

  

  as.data.frame(diag) %>%

    rownames_to_column("Test") %>%

    mutate(

      Model = model_name,

      df1 = round(df1, 0),

      df2 = ifelse(is.na(df2), NA, round(df2, 0)),

      statistic = round(statistic, 3),

      `p-value` = round(`p-value`, 4)

    ) %>%

    select(Model, Test, df1, df2, statistic, `p-value`)

}

# --- Combine diagnostics from both models ---

diag_combined <- bind_rows(

  extract_iv_diagnostics(iv_model, "Baseline IV"),

  extract_iv_diagnostics(iv_split, "Post-1968 interaction IV")

)

# Optional: add stars

diag_combined <- diag_combined %>%

  mutate(

    stars = case_when(

      `p-value` < 0.01 ~ "***",

      `p-value` < 0.05 ~ "**",

      `p-value` < 0.10 ~ "*",

      TRUE ~ ""

    ),

    `p-value` = paste0(formatC(`p-value`, format = "f", digits = 4), stars)

  )

# --- Create Typst table ---

diag_typst <- diag_combined %>%
  tt(
    rownames = FALSE,
    col_names = c("Model", "Test", "df1", "df2", "Statistic", "p-value"),
    align = c("left", "left", "right", "right", "right", "right")
  ) %>%
  tt_style(
    stroke = TRUE,
    striped = TRUE,
    inset = "5pt"
  ) %>%
  tt_row(
    0,
    bold = TRUE
  ) %>%
  tt_pack_rows(
    index = c(
      "Baseline IV" = nrow(extract_iv_diagnostics(iv_model, "Baseline IV")),
      "Post-1968 interaction IV" = nrow(extract_iv_diagnostics(iv_split, "Post-1968 interaction IV"))
    )
  )
# Print Typst markup
diag_typst
# Export as Typst source
tt_save(diag_typst, "iv_diagnostics_comparison.typ")










stargazer(iv_model, iv_split, type = 'latex')

df$running <- df$year - 1968

rd <- rdrobust(

  y = df$pnr,

  x = df$running,

  c = 0

)

summary(rd)

p <- rdplot(

  y = df$pnr,

  x = df$running,

  c = 0,
  ci = 0.95

, x.label = "", y.label = "", title = "")

conduct_model <- lm(

  pnr ~ pmr + per + wayr + pexpr + markup_term,

  data = df

)

summary(conduct_model)




