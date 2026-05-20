df <- read_dta("Data/cement-steen-roller.dta") %>%
  arrange(year) %>%
  mutate(
    pnr = pn / cpi * 100,
    wayr = way / cpi * 100,
    pmr = pm / cpi * 100,
    per = pe / cpi * 100,
    pexpr = pexp / cpi * 100,
    mc1r = mc1/cpi *100
  )

dem <- df %>%
  mutate(
    l_pnr = lag(pnr),
    l_qn = lag(qn),
    l_byan = lag(byan)
  ) %>%
  drop_na(pnr, qn, byan, l_pnr, l_qn, l_byan,
          wayr, pmr, per, pexpr, predictedmc)

demand_iv <- ivreg(
  pnr ~ qn + byan + l_pnr + l_qn + l_byan |
    wayr + pmr + per + pexpr + byan + l_pnr + l_qn + l_byan,
  data = dem
)

summary(demand_iv, diagnostics = TRUE)
stargazer(demand_iv, type = 'latex')
coeftest(demand_iv, vcov = vcovHC(demand_iv, type = "HC1"))

sr_elas <- (1 / coef(iv_model)["qn"]) * (mean(dem$pexpr) / mean(dem$exp))
sr_elas

dem <- dem %>%
  mutate(
    lerner = (pexpr - mc1r) / pexpr
  )

theta_cartel <- mean(dem$lerner, na.rm = TRUE) * abs(sr_elas)
sr_elas
mean_lerner <- mean(dem$lerner, na.rm = TRUE)
theta_cartel



