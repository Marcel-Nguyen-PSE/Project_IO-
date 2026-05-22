library(dplyr)
library(AER)
library(lmtest)
library(purrr)
library(tibble)
library(car)

df <- read_dta(
  "Data/cement-steen-roller.dta"
)

df <- df %>%
  arrange(year) %>%
  mutate(
    pmr        = pm / cpi * 100,
    per        = pe / cpi * 100,
    wayr       = way / cpi * 100,
    pexpr      = pexp / cpi * 100,
    pnr        = pn / cpi * 100,
    shortrunAC = mc1 / cpi * 100,
    mc         = predictedmc/ cpi * 100,
    pnr_lag    = lag(pnr, 1),
    qn_lag     = lag(qn, 1),
    byan_lag   = lag(byan, 1),
    
    post1968   = ifelse(year >= 1968, 1, 0)
  )


# Does pexpr makes big diff? => first stage comparison==================

fs_nopexp <-  lm(
  qn ~ byan + pnr_lag + qn_lag + byan_lag +
    wayr + pmr + per,
  data = df
)

summary(fs_nopexp)

fs_withpexp <- lm(
  qn ~ byan + pexpr + qn_lag + byan_lag + wayr +pmr + per,
  data = df
)

summary(fs_withpexp)


linearHypothesis(
  lm(
    qn ~ byan + pnr_lag + qn_lag + byan_lag +
      wayr + pmr + per +pexpr,
    data = df
  ),
  c(
    "wayr = 0",
    "pmr = 0",
    "per = 0",
    "pexpr = 0"
  )
)

# Testing which ADL specification is the best===============================
max_lag <- 6

df_lags <- df %>%
  arrange(year)

# Create lags

for (k in 1:max_lag) {
  df_lags[[paste0("pnr_lag", k)]]  <- lag(df_lags$pnr, k)
  df_lags[[paste0("qn_lag", k)]]   <- lag(df_lags$qn, k)
  df_lags[[paste0("byan_lag", k)]] <- lag(df_lags$byan, k)
}


needed_vars <- c(
  "pnr", "qn", "byan",
  "wayr", "pmr", "per",
  paste0("pnr_lag", 1:max_lag),
  paste0("qn_lag", 1:max_lag),
  paste0("byan_lag", 1:max_lag)
)

df_common <- df_lags %>%
  filter(if_all(all_of(needed_vars), ~ !is.na(.)))

# ----------------------------
# Function to estimate IV-ADL(k,k)
# ----------------------------

test_iv_adl <- function(k){
  
  p_lags <- paste0("pnr_lag", 1:k)
  q_lags <- paste0("qn_lag", 1:k)
  z_lags <- paste0("byan_lag", 1:k)
  
  rhs_reg <- c("qn", "byan", p_lags, q_lags, z_lags)
  
  rhs_iv <- c(
    "byan",
    p_lags,
    q_lags,
    z_lags,
    "wayr", "pmr", "per"
  )
  
  formula_k <- as.formula(
    paste(
      "pnr ~", paste(rhs_reg, collapse = " + "),
      "|",
      paste(rhs_iv, collapse = " + ")
    )
  )
  
  model_k <- ivreg(formula_k, data = df_common)
  
  u <- residuals(model_k)
  n <- length(u)
  rss <- sum(u^2)
  k_params <- length(coef(model_k))
  
  pseudo_AIC <- n * log(rss / n) + 2 * k_params
  pseudo_BIC <- n * log(rss / n) + log(n) * k_params
  
  bg <- bgtest(u ~ 1, order = k)
  
  tibble(
    lag_order = k,
    pseudo_AIC = pseudo_AIC,
    pseudo_BIC = pseudo_BIC,
    BG_p_value = bg$p.value,
    adj_R2 = summary(model_k)$adj.r.squared,
    nobs = n
  )
}

iv_adl_results <- bind_rows(
  map(1:max_lag, test_iv_adl)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

iv_adl_results

iv_adl_results %>%
  filter(BG_p_value > 0.05) %>%
  arrange(pseudo_BIC)

#result: ADL 2,2 would be the best!


#Comparing ADL 1 and 2 ================================
iv_adl00 <- ivreg(
  pnr ~ qn + byan   |
    byan  +
    wayr + pmr + per + pexpr,
  data = df
)

iv_adl0 <- ivreg(
  pnr ~ qn + byan  + qn_lag + byan_lag |
    byan + pnr_lag + qn_lag + byan_lag +
    wayr + pmr + per + pexpr,
  data = df
)

iv_adl1 <- ivreg(
  pnr ~ qn + byan + pnr_lag + qn_lag + byan_lag |
    byan + pnr_lag + qn_lag + byan_lag +
    wayr + pmr + per + pexpr,
  data = df
)

summary(iv_adl1, diagnostics = TRUE)


iv_adl2 <- ivreg(
  pnr ~ 
    qn + byan +
    pnr_lag + pnr_lag2 +
    qn_lag + qn_lag2 +
    byan_lag1 + byan_lag2
  |
    byan +
    pnr_lag + pnr_lag2 +
    qn_lag + qn_lag2 +
    byan_lag1 + byan_lag2 +
    wayr + pmr + per,
  data = df_common
)

stargazer(iv_adl00,iv_adl0, iv_adl1, iv_adl2, iv_split, type = 'latex')

summary(iv_adl2, diagnostics = TRUE)

#Bad Sargan... => ADL 1 is reasonable?


# New attempt: maybe put 2nd lag of Qn only (based on AIC result)=========================

max_lag <- 2  

df <- df %>%
  arrange(year)

for(k in 1:max_lag){
  
  df[[paste0("pexpr_lag", k)]] <-
    dplyr::lag(df$pexpr, k)
  
}

iv_model_q2 <- ivreg(
  pnr ~
    qn + byan +
    pnr_lag +
    qn_lag + qn_lag2 +
    byan_lag
  |
    byan +
    pnr_lag +
    qn_lag + qn_lag2 +
    byan_lag +
    wayr + pmr + per + pexpr,
  data = df_lags
)


summary(iv_model_q2, diagnostics = TRUE)

#not good!

#CHOW TEST!!==============================================

## I DON'T KNOW WHICH ONE IS CORRECT!!!!! OPPOSITE RESULTS
df$post1968 <- ifelse(df$year >= 1969, 1, 0)

iv_break <- ivreg(
  pnr ~
    qn +
    qn:post1968 +
    byan +
    pnr_lag +
    qn_lag +
    byan_lag +
    post1968
  |
    byan +
    pnr_lag +
    qn_lag +
    byan_lag +
    post1968 +
    wayr + pmr + per + pexpr +
    I(pexpr * post1968),
  data = df
)

summary(iv_break, diagnostics = TRUE)


iv_split <- ivreg(
  pnr ~ qn * post1968 + byan + qn_lag + byan_lag |
    byan + qn_lag + byan_lag +
    wayr + pmr + per + pexpr +
    post1968 + post1968:wayr + post1968:pmr + post1968:per + post1968:pexpr,
  data = df
)
summary(iv_split, diagnostics = TRUE)
