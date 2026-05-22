library(tidyverse)
library(haven)
library(AER)
library(lmtest)
library(sandwich)
library(stargazer)

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




# 1. Pooled IV ============================================================


pooled <- ivreg(
  pnr ~ qn + byan  |
    byan + wayr + pmr + per,
  data = df
)
summary(pooled, diagnostics = TRUE)

pooled <- ivreg(
  pnr ~ qn + byan + pnr_lag + qn_lag + byan_lag |
    byan + pnr_lag + qn_lag + byan_lag +
    wayr + pmr + per,
  data = df
)

summary(pooled, diagnostics = TRUE)

beta_sr  <- coef(pooled)["qn"]
beta_lag <- coef(pooled)["qn_lag"]
rho      <- coef(pooled)["pnr_lag"]
beta_lr <- (beta_sr + beta_lag) / (1 - rho)

df_conduct <- df %>%
  mutate(
    markup_sr = -qn * beta_sr,
    markup_lr = -qn * beta_lr,
    price_gap_pexpr = pnr - pexpr,
    price_gap_pmc = pnr - predictedmc,
    shortAC = shortrunAC/cpi *100,
    price_gap_sac = pnr - shortAC
    
  ) %>%
  drop_na(price_gap_pexpr, price_gap_pmc, price_gap_sac, markup_sr, markup_lr)




cartel_sr_pexpr <- lm(
  pnr ~ pexpr + markup_sr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_lr_pexpr <- lm(
  pnr ~ pexpr + markup_lr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_sr_2c_pexpr <- lm(
  I(pnr - pexpr) ~ 0 + markup_sr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_lr_2c_pexpr <- lm(
  I(pnr - pexpr) ~ 0 + markup_lr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_sr_1c_pexpr <- lm(
  pnr ~ 0 + pexpr + markup_sr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_lr_1c_pexpr <- lm(
  pnr ~ 0 + pexpr + markup_lr,
  data = df_conduct %>% filter(year <= 1968)
)
#################################################################
cartel_sr_pmc <- lm(
  pnr ~ predictedmc + markup_sr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_lr_pmc <- lm(
  pnr ~ predictedmc + markup_lr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_sr_2c_pmc <- lm(
  I(pnr - predictedmc) ~ 0 + markup_sr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_lr_2c_pmc <- lm(
  I(pnr - predictedmc) ~ 0 + markup_lr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_sr_1c_pmc <- lm(
  pnr ~ 0 + predictedmc + markup_sr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_lr_1c_pmc <- lm(
  pnr ~ 0 + predictedmc + markup_lr,
  data = df_conduct %>% filter(year <= 1968)
)

###################################################

cartel_sr_sac <- lm(
  pnr ~ shortAC + markup_sr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_lr_sac <- lm(
  pnr ~ shortAC + markup_lr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_sr_2c_sac<- lm(
  I(pnr - shortAC) ~ 0 + markup_sr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_lr_2c_sac <- lm(
  I(pnr - shortAC) ~ 0 + markup_lr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_sr_1c_sac <- lm(
  pnr ~ 0 + shortAC + markup_sr,
  data = df_conduct %>% filter(year <= 1968)
)

cartel_lr_1c_sac <- lm(
  pnr ~ 0 + shortAC + markup_lr,
  data = df_conduct %>% filter(year <= 1968)
)

#####################################################

stargazer(cartel_sr_pexpr, cartel_lr_pexpr, cartel_sr_2c_pexpr, cartel_lr_2c_pexpr, cartel_sr_1c_pexpr, cartel_sr_pmc, cartel_lr_pmc, cartel_sr_2c_pmc, cartel_lr_2c_pmc, cartel_sr_1c_pmc, cartel_sr_sac, cartel_lr_sac, cartel_sr_2c_sac, cartel_lr_2c_sac, cartel_sr_1c_sac, type = 'latex')
summary(conduct_cartel1.1)

conduct_robust1.1 <- coeftest(
  conduct_cartel1.1,
  vcov = vcovHC(conduct_cartel1.1, type = "HC1")
)

conduct_robust1.1

lambda_cartel_sr <- coef(cartel_sr)["markup_sr"]
lambda_cartel_lr <- coef(cartel_lr)["markup_lr"]
lambda_cartel_sr_1c <- coef(cartel_sr_1c)["markup_sr"]
lambda_cartel_lr_1c <- coef(cartel_lr_1c)["markup_lr"]
lambda_cartel_sr_2c <- coef(cartel_sr_2c)["markup_sr"]
lambda_cartel_lr_2c <- coef(cartel_lr_2c)["markup_lr"]
lambda_cartel_sr
lambda_cartel_lr 
lambda_cartel_sr_1c 
lambda_cartel_lr_1c 
lambda_cartel_sr_2c 
lambda_cartel_lr_2c 




# 1-2. Monopoly

conduct_mono1 <- lm(
  pnr ~ shortrunAC + markup_term,
  data = df_conduct %>% filter(year >= 1968)
)

summary(conduct_mono1)

conduct_robust_mono1 <- coeftest(
  conduct_mono1,
  vcov = vcovHC(conduct_mono1, type = "HC1")
)

conduct_robust_mono1

lambda_mono1 <- coef(conduct_mono1)["markup_term"]
lambda_mono1


# ============================================================
# 2. Separate IVreg
# ============================================================

# 2-1. Cartel

df_cartel <- df %>%
  filter(year < 1968)

pooled_cartel <- ivreg(
  pnr ~ qn + byan + pnr_lag + qn_lag + byan_lag |
    byan + pnr_lag + qn_lag + byan_lag +
    wayr + pmr + per,
  data = df_cartel
)



summary(pooled_cartel, diagnostics = TRUE)

beta_cartel <- coef(pooled_cartel)["qn"]
beta_cartel

df_cartel <- df_cartel %>%
  mutate(
    markup_term = -qn * beta_cartel
  ) %>%
  drop_na(pnr, pexpr, shortrunAC, markup_term)

conduct_cartel2 <- lm(
  pnr ~ pexpr + markup_term,
  data = df_cartel
)

summary(conduct_cartel2)

conduct_robust2 <- coeftest(
  conduct_cartel2,
  vcov = vcovHC(conduct_cartel2, type = "HC1")
)

conduct_robust2

lambda_cartel2 <- coef(conduct_cartel2)["markup_term"]
lambda_cartel2


# 2-2. Monopoly

df_mono2 <- df %>%
  filter(year >= 1968)

pooled_mono <- ivreg(
  pnr ~ qn + byan + pnr_lag + qn_lag + byan_lag |
    byan + pnr_lag + qn_lag + byan_lag +
    wayr + pmr + per,
  data = df_mono2
)

summary(pooled_mono, diagnostics = TRUE)

beta_mono2 <- coef(pooled_mono)["qn"]
beta_mono2

df_mono2 <- df_mono2 %>%
  mutate(
    markup_term = -qn * beta_mono2
  ) %>%
  drop_na(pnr, shortrunAC, markup_term)

conduct_mono2 <- lm(
  pnr ~ shortrunAC + markup_term,
  data = df_mono2
)

summary(conduct_mono2)

conduct_robust_mono2 <- coeftest(
  conduct_mono2,
  vcov = vcovHC(conduct_mono2, type = "HC1")
)

conduct_robust_mono2

lambda_mono2 <- coef(conduct_mono2)["markup_term"]
lambda_mono2


# ============================================================
# 3. Interaction term method
# ============================================================

iv_split <- ivreg(
  pnr ~ qn * post1968 + byan + pnr_lag + qn_lag + byan_lag |
    byan + pnr_lag + qn_lag + byan_lag +
    wayr + pmr + per +
    post1968 +
    post1968:wayr + post1968:pmr + post1968:per,
  data = df
)

summary(iv_split, diagnostics = TRUE)

# 3-1. Cartel

df_cartel3 <- df %>%
  filter(year < 1968)

beta_cartel3 <- coef(iv_split)["qn"]
beta_cartel3

df_cartel3 <- df_cartel3 %>%
  mutate(
    markup_term = -qn * beta_cartel3
  ) %>%
  drop_na(pnr, pexpr, markup_term)

"""
conduct_cartel3 <- lm(
  pnr ~ pexpr + markup_term,
  data = df_cartel3
)
"""
conduct_cartel3 <- lm(
  pnr ~ 0 + pexpr + markup_term,
  data = df_cartel3
)


summary(conduct_cartel3)
summary(conduct_cartel3)

conduct_robust_cartel3 <- coeftest(
  conduct_cartel3,
  vcov = vcovHC(conduct_cartel3, type = "HC1")
)

conduct_robust_cartel3

lambda_cartel3 <- coef(conduct_cartel3)["markup_term"]
lambda_cartel3


# 3-2. Monopoly

df_mono3 <- df %>%
  filter(year >= 1968)

beta_mono3 <- coef(iv_split)["qn"] + coef(iv_split)["qn:post1968"]
beta_mono3

df_mono3 <- df_mono3 %>%
  mutate(
    markup_term = -qn * beta_mono3
  ) %>%
  drop_na(pnr, shortrunAC, markup_term)

conduct_mono3 <- lm(
  pnr ~ shortrunAC + markup_term,
  data = df_mono3
)

summary(conduct_mono3)

conduct_robust_mono3 <- coeftest(
  conduct_mono3,
  vcov = vcovHC(conduct_mono3, type = "HC1")
)

conduct_robust_mono3

lambda_mono3 <- coef(conduct_mono3)["markup_term"]
lambda_mono3


# ============================================================
# Diagnostics
# ============================================================

beta_table <- tibble(
  cartel = c(
    "Pooled IV",
    "Separate IV - Cartel",
    "Separate IV - Monopoly",
    "Interaction IV - Cartel",
    "Interaction IV - Monopoly"
  ),
  beta_q = c(
    beta_q,
    beta_cartel,
    beta_mono2,
    beta_cartel3,
    beta_mono3
  )
)

beta_table

lambda_table <- tibble(
  cartel = c(
    "Pooled IV + Cartel Conduct",
    "Pooled IV + Monopoly Conduct",
    "Separate IV + Cartel Conduct",
    "Separate IV + Monopoly Conduct",
    "Interaction IV + Cartel Conduct",
    "Interaction IV + Monopoly Conduct"
  ),
  lambda = c(
    lambda_cartel1,
    lambda_mono1,
    lambda_cartel2,
    lambda_mono2,
    lambda_cartel3,
    lambda_mono3
  )
)

lambda_table








# ============================================================
# new Split IV with long run betas
# ============================================================

iv_split <- ivreg(
  pnr ~ qn * post1968 + byan + pnr_lag + qn_lag + byan_lag |
    byan + pnr_lag + qn_lag + byan_lag +
    wayr + pmr + per +
    post1968 +
    post1968:wayr +
    post1968:pmr +
    post1968:per,
  data = df
)

summary(iv_split, diagnostics = TRUE)

# ============================================================
# 2.1 Extract period-specific demand slopes
# ============================================================

# --- cartel (pre-1968)
beta_sr_cartel <- coef(iv_split)["qn"]

# --- monopoly (post-1968)
beta_sr_monopoly <-
  coef(iv_split)["qn"] +
  coef(iv_split)["qn:post1968"]

# common lag terms
beta_lag <- coef(iv_split)["qn_lag"]
rho      <- coef(iv_split)["pnr_lag"]

# long-run slopes
beta_lr_cartel <-
  (beta_sr_cartel + beta_lag) /
  (1 - rho)

beta_lr_monopoly <-
  (beta_sr_monopoly + beta_lag) /
  (1 - rho)

beta_sr_cartel
beta_sr_monopoly
beta_lr_cartel
beta_lr_monopoly

# ============================================================
# 2.2 Create conduct dataset
# ============================================================

df_conduct_split <- df %>%
  mutate(
    
    # cartel markup
    markup_sr_cartel =
      -qn * beta_sr_cartel,
    
    markup_lr_cartel =
      -qn * beta_lr_cartel,
    
    # monopoly markup
    markup_sr_monopoly =
      -qn * beta_sr_monopoly,
    
    markup_lr_monopoly =
      -qn * beta_lr_monopoly,
    
    price_gap =
      pnr - pexpr
    
  ) %>%
  drop_na()

# ============================================================
# 2.3 CARTEL conduct (year < 1968)
# ============================================================

df_cartel <- df_conduct_split %>%
  filter(year < 1968)

# -------- unconstrained

cartel_sr <- lm(
  pnr ~ pexpr + markup_sr_cartel,
  data = df_cartel
)

cartel_lr <- lm(
  pnr ~ pexpr + markup_lr_cartel,
  data = df_cartel
)

# -------- coefficient constrained
# P - R = lambda * markup

cartel_sr_2c <- lm(
  I(pnr - pexpr) ~ 0 + markup_sr_cartel,
  data = df_cartel
)

cartel_lr_2c <- lm(
  I(pnr - pexpr) ~ 0 + markup_lr_cartel,
  data = df_cartel
)

# -------- intercept constrained
# P = R + lambda * markup

cartel_sr_1c <- lm(
  pnr ~ 0 + pexpr + markup_sr_cartel,
  data = df_cartel
)

cartel_lr_1c <- lm(
  pnr ~ 0 + pexpr + markup_lr_cartel,
  data = df_cartel
)

# ============================================================
# 2.4 MONOPOLY conduct (year >= 1968)
# ============================================================

df_monopoly <- df_conduct_split %>%
  filter(year >= 1968)

# -------- unconstrained

monopoly_sr <- lm(
  pnr ~ pexpr + markup_sr_monopoly,
  data = df_monopoly
)

monopoly_lr <- lm(
  pnr ~ pexpr + markup_lr_monopoly,
  data = df_monopoly
)

# -------- coefficient constrained

monopoly_sr_2c <- lm(
  I(pnr - pexpr) ~ 0 + markup_sr_monopoly,
  data = df_monopoly
)

monopoly_lr_2c <- lm(
  I(pnr - pexpr) ~ 0 + markup_lr_monopoly,
  data = df_monopoly
)

# -------- intercept constrained

monopoly_sr_1c <- lm(
  pnr ~ 0 + pexpr + markup_sr_monopoly,
  data = df_monopoly
)

monopoly_lr_1c <- lm(
  pnr ~ 0 + pexpr + markup_lr_monopoly,
  data = df_monopoly
)

# ============================================================
# 2.5 Extract lambdas
# ============================================================

# cartel
lambda_cartel_sr <- coef(cartel_sr)["markup_sr_cartel"]
lambda_cartel_lr <- coef(cartel_lr)["markup_lr_cartel"]

lambda_cartel_sr_1c <-
  coef(cartel_sr_1c)["markup_sr_cartel"]

lambda_cartel_lr_1c <-
  coef(cartel_lr_1c)["markup_lr_cartel"]

lambda_cartel_sr_2c <-
  coef(cartel_sr_2c)["markup_sr_cartel"]

lambda_cartel_lr_2c <-
  coef(cartel_lr_2c)["markup_lr_cartel"]

# monopoly
lambda_monopoly_sr <-
  coef(monopoly_sr)["markup_sr_monopoly"]

lambda_monopoly_lr <-
  coef(monopoly_lr)["markup_lr_monopoly"]

lambda_monopoly_sr_1c <-
  coef(monopoly_sr_1c)["markup_sr_monopoly"]

lambda_monopoly_lr_1c <-
  coef(monopoly_lr_1c)["markup_lr_monopoly"]

lambda_monopoly_sr_2c <-
  coef(monopoly_sr_2c)["markup_sr_monopoly"]

lambda_monopoly_lr_2c <-
  coef(monopoly_lr_2c)["markup_lr_monopoly"]

# ============================================================
# 2.6 Print results
# ============================================================

lambda_cartel_sr
lambda_cartel_lr
lambda_cartel_sr_1c
lambda_cartel_lr_1c
lambda_cartel_sr_2c
lambda_cartel_lr_2c

lambda_monopoly_sr
lambda_monopoly_lr
lambda_monopoly_sr_1c
lambda_monopoly_lr_1c
lambda_monopoly_sr_2c
lambda_monopoly_lr_2c


lm_sr_pexpr <- lm(price_gap_pexpr ~ 0 + markup_sr, data = df_conduct)
lm_lr_pexpr <- lm(price_gap ~ 0 + markup_lr, data = df_conduct)
lm_lr_loose <- lm(pnr ~ 0 + markup_lr+pexpr, data = df_conduct)


summary(lm_sr)
summary(lm_lr)
summary(lm_lr_loose)
mean(df_conduct$price_gap / df_conduct$markup_sr)
mean(df_conduct$price_gap / df_conduct$markup_lr)



df_conduct <- df %>%
  mutate(
    markup_sr       = -qn * beta_sr,
    markup_lr       = -qn * beta_lr,
    price_gap_pexpr = pnr - pexpr,
    price_gap_pmc   = pnr - predictedmc,
    shortAC         = shortrunAC / cpi * 100,
    price_gap_sac   = pnr - shortAC,
    gap_pexpr       = pnr - pexpr,
    gap_pmc         = pnr - predictedmc,
    gap_sac         = pnr - shortAC
  ) %>%
  filter(
    !is.na(price_gap_pexpr),
    !is.na(price_gap_pmc),
    !is.na(price_gap_sac),
    !is.na(markup_sr),
    !is.na(markup_lr)
  )

# --- pexpr models ---
cartel_sr_pexpr <- lm(pnr ~ pexpr + markup_sr, data = df_conduct %>% filter(year <= 1968))
cartel_lr_pexpr <- lm(pnr ~ pexpr + markup_lr, data = df_conduct %>% filter(year <= 1968))

cartel_sr_2c_pexpr <- lm(gap_pexpr ~ 0 + markup_sr, data = df_conduct %>% filter(year <= 1968))
cartel_lr_2c_pexpr <- lm(gap_pexpr ~ 0 + markup_lr, data = df_conduct %>% filter(year <= 1968))

cartel_sr_1c_pexpr <- lm(pnr ~ 0 + pexpr + markup_sr, data = df_conduct %>% filter(year <= 1968))
cartel_lr_1c_pexpr <- lm(pnr ~ 0 + pexpr + markup_lr, data = df_conduct %>% filter(year <= 1968))

# --- predictedmc models ---
cartel_sr_pmc <- lm(pnr ~ predictedmc + markup_sr, data = df_conduct %>% filter(year <= 1968))
cartel_lr_pmc <- lm(pnr ~ predictedmc + markup_lr, data = df_conduct %>% filter(year <= 1968))

cartel_sr_2c_pmc <- lm(gap_pmc ~ 0 + markup_sr, data = df_conduct %>% filter(year <= 1968))
cartel_lr_2c_pmc <- lm(gap_pmc ~ 0 + markup_lr, data = df_conduct %>% filter(year <= 1968))

cartel_sr_1c_pmc <- lm(pnr ~ 0 + predictedmc + markup_sr, data = df_conduct %>% filter(year <= 1968))
cartel_lr_1c_pmc <- lm(pnr ~ 0 + predictedmc + markup_lr, data = df_conduct %>% filter(year <= 1968))

# --- shortAC models ---
cartel_sr_sac <- lm(pnr ~ shortAC + markup_sr, data = df_conduct %>% filter(year <= 1968))
cartel_lr_sac <- lm(pnr ~ shortAC + markup_lr, data = df_conduct %>% filter(year <= 1968))

cartel_sr_2c_sac <- lm(gap_sac ~ 0 + markup_sr, data = df_conduct %>% filter(year <= 1968))
cartel_lr_2c_sac <- lm(gap_sac ~ 0 + markup_lr, data = df_conduct %>% filter(year <= 1968))

cartel_sr_1c_sac <- lm(pnr ~ 0 + shortAC + markup_sr, data = df_conduct %>% filter(year <= 1968))
cartel_lr_1c_sac <- lm(pnr ~ 0 + shortAC + markup_lr, data = df_conduct %>% filter(year <= 1968))

# --- output ---
stargazer(
  cartel_sr_pexpr, cartel_lr_pexpr, cartel_sr_2c_pexpr, 
  type = "latex"
)
stargazer(cartel_lr_2c_pexpr, cartel_sr_1c_pexpr, cartel_lr_1c_pexpr,
   type = 'latex')
stargazer(cartel_sr_pmc,   cartel_lr_pmc,   cartel_sr_2c_pmc,  type = 'latex')
stargazer(cartel_lr_2c_pmc,   cartel_sr_1c_pmc,   cartel_lr_1c_pmc,
  type = 'latex')
stargazer(cartel_sr_sac,   cartel_lr_sac,   cartel_sr_2c_sac,  type = 'latex')
stargazer(cartel_lr_2c_sac,   cartel_sr_1c_sac,   cartel_lr_1c_sac, type = 'latex')


df_conduct_post <- df %>%
  mutate(
    markup_sr = -qn * beta_sr,
    markup_lr = -qn * beta_lr,
    shortAC   = shortrunAC / cpi * 100,
    gap_pexpr = pnr - pexpr,
    gap_sac   = pnr - shortAC
  ) %>%
  filter(year > 1968) %>%
  filter(
    !is.na(markup_sr),
    !is.na(markup_lr),
    !is.na(gap_pexpr),
    !is.na(gap_sac)
  )

# --- pexpr models ---
mono_sr_pexpr     <- lm(pnr       ~     pexpr + markup_sr, data = df_conduct_post)
mono_lr_pexpr     <- lm(pnr       ~     pexpr + markup_lr, data = df_conduct_post)

mono_sr_2c_pexpr  <- lm(gap_pexpr ~ 0 + markup_sr,         data = df_conduct_post)
mono_lr_2c_pexpr  <- lm(gap_pexpr ~ 0 + markup_lr,         data = df_conduct_post)

mono_sr_1c_pexpr  <- lm(pnr       ~ 0 + pexpr + markup_sr, data = df_conduct_post)
mono_lr_1c_pexpr  <- lm(pnr       ~ 0 + pexpr + markup_lr, data = df_conduct_post)

# --- shortAC models ---
mono_sr_sac       <- lm(pnr     ~     shortAC + markup_sr, data = df_conduct_post)
mono_lr_sac       <- lm(pnr     ~     shortAC + markup_lr, data = df_conduct_post)

mono_sr_2c_sac    <- lm(gap_sac ~ 0 + markup_sr,           data = df_conduct_post)
mono_lr_2c_sac    <- lm(gap_sac ~ 0 + markup_lr,           data = df_conduct_post)

mono_sr_1c_sac    <- lm(pnr     ~ 0 + shortAC + markup_sr, data = df_conduct_post)
mono_lr_1c_sac    <- lm(pnr     ~ 0 + shortAC + markup_lr, data = df_conduct_post)


# --- pexpr: free intercept ---
stargazer(
  mono_sr_pexpr, mono_lr_pexpr,
  mono_sr_2c_pexpr,
  type = "latex"
)

# --- pexpr: restricted intercept ---
stargazer(
  mono_lr_2c_pexpr,
  mono_sr_1c_pexpr, mono_lr_1c_pexpr,
  type = "latex"
)

# --- shortAC: free intercept ---
stargazer(
  mono_sr_sac, mono_lr_sac,
  mono_sr_2c_sac,
  type = "latex"
)

# --- shortAC: restricted intercept ---
stargazer(
  mono_lr_2c_sac,
  mono_sr_1c_sac, mono_lr_1c_sac,
  type = "latex"
)
