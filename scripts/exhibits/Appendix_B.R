# ---- Appendix B. Skin tone gaps in income and human capital ------------------
# -- Figure B1: Household asset index (z-score) ----
ame_weights <- 
  lapop_analysis |> 
  group_by(colorr) |> 
  count() |> 
  ungroup() |> 
  mutate(
    n = n/sum(n)
  ) |> 
  filter(colorr > 1) |> 
  select(n)

ame_weights <- c(0, ame_weights$n)


ind_riq <- 
  feols(
    scale(ind_riq) ~  i(colorr) 
    | cluster_enumerator_id + mothers_ed_isced + sex + age + hh_total,
    weights = ~ weight1500,
    lapop_analysis, cluster = ~ cluster_enumerator_id
  )

ind_riq_linear <- 
  feols(
    scale(ind_riq) ~ colorr 
    | cluster_enumerator_id + mothers_ed_isced + sex + age + hh_total,
    weights = ~ weight1500,
    lapop_analysis, cluster = ~ cluster_enumerator_id
  )

ind_riq_tibble <- est_meff_tibble(ind_riq)

plot_skin_tone_gaps(
  data_plot = ind_riq_tibble, 
  feols_linear_est = ind_riq_linear,
  ame_weights = ame_weights,
  estimate_type = " (z-score)"
)

ggsave(
  filename = "plots/FigB1.pdf",
  dpi = 300,
  width = 7.5,
  height = 5
)

#

# -- Table B1: Income ----
specifications <- list(
  # Model 1: Base model with colorr
  hhincomepc_mx0 ~ colorr,
  
  # Model 2: Country times year fixed effects
  hhincomepc_mx0 ~ colorr | countrycode_year,
  
  # Model 3: Municipality times year fixed effects
  hhincomepc_mx0 ~ colorr | country_mun^year,
  
  # Model 4: Cluster fixed effects
  hhincomepc_mx0 ~ colorr | cluster_id,
  
  # Model 5: Cluster + Enumerator FE
  hhincomepc_mx0 ~ colorr | cluster_enumerator_id,
  
  # Model 6: Cluster + Enumerator FE + HH size FE
  hhincomepc_mx0 ~ colorr | cluster_enumerator_id + hh_total,
  
  # Model 7: Cluster + Enumerator FE + HH size FE + Age + Sex
  hhincomepc_mx0 ~ colorr + age + age^2 + i(sex) | cluster_enumerator_id + hh_total,
  
  # Model 8: Cluster + Enumerator FE + HH size FE + Age + Sex + Mothers Ed
  hhincomepc_mx0 ~ colorr + age + age^2  + i(sex) + i(mothers_ed_isced) | cluster_enumerator_id + hh_total,
  
  # Model 9: Cluster + Enumerator FE + HH size FE + Age + Sex + Mothers Ed + Quadratic term
  hhincomepc_mx0 ~ colorr + colorr^2 + age + age^2  + i(sex) + i(mothers_ed_isced) | cluster_enumerator_id + hh_total,
  
  # Model 10: Cluster + Enumerator FE + HH size + Age + Sex + Mothers Ed + Education
  hhincomepc_mx0 ~ colorr + age + age^2 + i(sex) + i(mothers_ed_isced) + edc | cluster_enumerator_id + hh_total,
  
  # Model 11: Cluster + Enumerator FE + Age + Sex + Mothers Ed + Education + Bad controls
  hhincomepc_mx0 ~ colorr + age + age^2  + i(sex) + i(mothers_ed_isced) + edc +
    i(etnia) + i(occupa_status) + i(salary_status) + i(marital_status) + i(religion) | cluster_enumerator_id + hh_total
)

inc_table <- lapply(specifications, function(fml) {
  feols(fml, weights = ~ weight1500, data = lapop_analysis, cluster = ~ cluster_enumerator_id)
})

# Generate etable with custom fit statistics
etable_output <- etable(
  inc_table,
  drop = c("Constant", "Employment status", "Religion", "Marital status", "Salary status"),
  tex = TRUE, tpt = TRUE,
  title = "Skin tone gaps: Household income per capita (log-points)",
  digits = "r3", digits.stats = 3,
  fitstat = ~ my + n + r2 + ar2 + partial_r2_ty + rv_q + rv_qa,
  signif.code = NA,
  notes = 
    "\\footnotesize \\justify \\noindent \\textit{Notes}: 
        Estimates using specification \\ref{eq:baseline-specification}.
        Dependent variable is log-transformation of household income per capita, shutting off the extensive margin following \\citet{Chen2023}.
        Mean monthly household income per capita is 260.74 (PPP).
        Skin tone stands for the PERLA color palette including scales from 1 to 9, top coding skin tones above 9 given their small share.
        Clustered Within-municipality $\\times$ Year $\\times$ Enumerator standard errors in parentheses.
        The omitted category in Gender is Female.
        The omitted category in Mother's education is No education.
        The omitted category in Ethnoracial identity is White.
        Bad or unclean controls include: occupational status (i.e. working, unemployed), salary status (i.e. self-employed, owner, worker in private sector, etc.), marital status (i.e. married, single, divorced, etc.), and religion.
        Sensitivity statistics following \\citet{Cinelli2020}. $RV_{q=1}$ represents the share of residual variance in treatment and outcome that unobserved confounders must explain to nullify the observed effect. $RV_{q=1, \\alpha=0.05}$ indicates the percentage needed to bring the lower bound of the confidence interval to zero at a 5\\% significance level. The partial $R^2$ of the treatment with the outcome sets a lower bound on the treatment variance unobserved confounders must explain if they fully account for the effect."
)

etable_output
#

# -- Table B2: Years of schooling ----
specifications <- list(
  # Model 1: Base model with colorr
  scale(edc) ~ colorr,
  
  # Model 2: Country times year fixed effects
  scale(edc) ~ colorr | countrycode_year,
  
  # Model 3: Municipality times year fixed effects
  scale(edc) ~ colorr | country_mun^year,
  
  # Model 4: Cluster fixed effects
  scale(edc) ~ colorr | cluster_id,
  
  # Model 5: Cluster + Enumerator FE
  scale(edc) ~ colorr | cluster_enumerator_id,
  
  # Model 6: Cluster + Enumerator FE + HH size FE + Age + Sex
  scale(edc) ~ colorr + age + age^2 + i(sex) | cluster_enumerator_id,
  
  # Model 7: Cluster + Enumerator FE + HH size FE + Age + Sex + Mothers Ed
  scale(edc) ~ colorr + age + age^2  + i(sex) + i(mothers_ed_isced) | cluster_enumerator_id,
  
  # Model 8: Cluster + Enumerator FE + HH size FE + Age + Sex + Mothers Ed + Quadratic term
  scale(edc) ~ colorr + colorr^2 + age + age^2  + i(sex) + i(mothers_ed_isced) | cluster_enumerator_id,
  
  # Model 9: Cluster + Enumerator FE + HH size + Age + Sex + Mothers Ed + Income
  scale(edc) ~ colorr + age + age^2 + i(sex) + i(mothers_ed_isced) + hhincomepc_mx0 | cluster_enumerator_id,
  
  # Model 10: Cluster + Enumerator FE + Age + Sex + Mothers Ed + Income + Bad controls
  scale(edc) ~ colorr + age + age^2  + i(sex) + i(mothers_ed_isced) + hhincomepc_mx0 +
    i(etnia) + i(occupa_status) + i(salary_status) + i(marital_status) + i(religion) | cluster_enumerator_id
)

edc_table <- lapply(specifications, function(fml) {
  feols(fml, weights = ~ weight1500, data = lapop_analysis, cluster = ~ cluster_enumerator_id)
})

# Generate etable with custom fit statistics
etable_output <- etable(
  edc_table,
  drop = c("Constant", "Employment status", "Religion", "Marital status", "Salary status"),
  tex = TRUE, tpt = TRUE,
  title = "Skin tone gaps: Years of scholing (z-score)",
  digits = "r3", digits.stats = 3,
  fitstat = ~ my + n + r2 + ar2 + partial_r2_ty + rv_q + rv_qa,
  signif.code = NA,
  notes = "
  \\footnotesize
  \\textit{Notes:}
  Estimates using specification \\ref{eq:baseline-specification}.
  Dependent variable is standardized years of schooling.
  Mean dependent variable is 9.95 years of schooling with a standard deviation of 4.27.
  Skin tone stands for the PERLA color palette including scales from 1 to 9, top coding skin tones above 9 given their small share.
  Clustered Within-municipality $\\times$ Year $\\times$ Enumerator standard errors in parentheses.
  The omitted category in Gender is Female.
  The omitted category in Mother's education is No education.
  The omitted category in Ethnoracial identity is White.
  Bad or unclean controls include: occupational status (i.e. working, unemployed), salary status (i.e. self-employed, owner, worker in private sector, etc.), marital status (i.e. married, single, divorced, etc.), and religion.
  Sensitivity statistics following \\citet{Cinelli2020}. 
  $RV_{q=1}$ represents the share of residual variance in treatment and outcome that unobserved confounders must explain to nullify the observed effect. $RV_{q=1, \\alpha=0.05}$ indicates the percentage needed to bring the lower bound of the confidence interval to zero at a 5\\% significance level. The partial $R^2$ of the treatment with the outcome sets a lower bound on the treatment variance unobserved confounders must explain if they fully account for the effect.
  "
)

etable_output
#

# -- Table B3: Testing Enumerator FE ----
foo <- 
  lapop_analysis |> 
  filter(!is.na(intid))

# Actual enumerator ID is only avaialble in survey wave 2016/2017

# Income
# Actual enumerator ID
income_enumer_id <- feols(hhincomepc_mx0 ~ colorr | cluster_id^intid, foo, weights = ~ weight1500, cluster = ~ cluster_id^intid)
# Proxy enumerator ID
income_proxy_id <- feols(hhincomepc_mx0 ~ colorr | cluster_enumerator_id, foo, weights = ~ weight1500, cluster = ~ cluster_enumerator_id)

# Years of schooling
# Actual enumerator ID
ed_enumer_id <- feols(scale(edc) ~ colorr | cluster_id^intid, foo, weights = ~ weight1500, cluster = ~ cluster_id^intid)
# Proxy enumerator ID
ed_proxy_id <- feols(scale(edc) ~ colorr | cluster_enumerator_id, foo, weights = ~ weight1500, cluster = ~ cluster_enumerator_id)

# Extract coefficients and standard errors
coef_income_enumer_id <- coef(income_enumer_id)["colorr"]
se_income_enumer_id <- sqrt(vcov(income_enumer_id)["colorr", "colorr"])

coef_income_proxy_id <- coef(income_proxy_id)["colorr"]
se_income_proxy_id <- sqrt(vcov(income_proxy_id)["colorr", "colorr"])

coef_ed_enumer_id <- coef(ed_enumer_id)["colorr"]
se_ed_enumer_id <- sqrt(vcov(ed_enumer_id)["colorr", "colorr"])

coef_ed_proxy_id <- coef(ed_proxy_id)["colorr"]
se_ed_proxy_id <- sqrt(vcov(ed_proxy_id)["colorr", "colorr"])

# Differences in coefficients
diff_income <- coef_income_enumer_id - coef_income_proxy_id
diff_ed <- coef_ed_enumer_id - coef_ed_proxy_id

# Standard errors of the differences
se_diff_income <- sqrt(se_income_enumer_id^2 + se_income_proxy_id^2)
se_diff_ed <- sqrt(se_ed_enumer_id^2 + se_ed_proxy_id^2)

# Test statistics
t_income <- diff_income / se_diff_income
t_ed <- diff_ed / se_diff_ed

# Degrees of freedom for income models
df_income_enumer_id <- income_enumer_id$nobs - income_enumer_id$nparams
df_income_proxy_id <- income_proxy_id$nobs - income_proxy_id$nparams

# Degrees of freedom for education models
df_ed_enumer_id <- ed_enumer_id$nobs - ed_enumer_id$nparams
df_ed_proxy_id <- ed_proxy_id$nobs - ed_proxy_id$nparams

# p-values
p_value_income <- 2 * pt(-abs(t_income), df = df_income_enumer_id)
p_value_ed <- 2 * pt(-abs(t_ed), df = df_ed_enumer_id)

# Table: Testing Enumerator fixed effects
etable(
  income_enumer_id, income_proxy_id,
  ed_enumer_id, ed_proxy_id,
  drop = c("Constant", "Employment status", "Religion", "Marital status", "Salary status"),
  tex = T, tpt = T,
  title = "Testing Enumerator fixed effects",
  label = "tab:test-enumerator-fe",
  digits = 3, digits.stats = 3,
  fitstat = ~ my + n + r2 + ar2,
  signif.code=NA,
  adjustbox = ".95 tw",
  notes = 
    "\\footnotesize \\textit{Notes:}
    Estimates using specification \\ref{eq:baseline-specification}.
    Based on AmericasBarometer LAPOP data for wave 2016/2017, which includes the actual enumerator ID.
    Columns 1 and 3 use the actual enumerator ID interacted with then within-municipality times year fixed effects.
    Columns 2 and 4 use the proxy enumerator ID (the enumerators gender times its self-declared skin tone) interacted with then within-municipality times year fixed effects.
    The fourth row shows the p-value for each dependent variable of testing the difference in coefficients between the specification using the actual versus the proxy enumerator ID.
    Dependent variable in columns 1 and 2 is log-transformation of household income per capita, shutting off the extensive margin following Chen and Roth (2023).
    Dependent variable in columns 3 and 4 is standardized years of schooling.
    Skin tone stands for the PERLA color palette including scales from 1 to 9, top coding skin tones above 9 given their small share.
    Clustered standard errors at the fixed effect level in parentheses.
"
)

#

# -- Figure B2: Variance decomposition by country ----
# Model 1: Correlation of skin tone on income
income_skin_tone <- 
  feols(
    hhincomepc_mx0 ~ i(colorr) | cluster_enumerator_id + hh_total,  # Fixed effects for country and year
    weights = ~ weight1500,                          # Applying weights
    lapop_analysis,                                             # Data source
    cluster = ~ cluster_enumerator_id,               # Clustered standard errors
    split = ~ countrycode
  )

# Model 2: Correlation of Ethnoracial cat. on income
income_race <- 
  feols(
    hhincomepc_mx0 ~ etnia | cluster_enumerator_id + hh_total,     # Fixed effects for country and year
    weights = ~ weight1500,                          # Applying weights
    lapop_analysis,                                             # Data source
    cluster = ~ cluster_enumerator_id,               # Clustered standard errors
    split = ~ countrycode
  )

# Model 3: Correlation of Skin tone x Ethnoracial cat. on income
income_skin_tone_by_race <- 
  feols(
    hhincomepc_mx0 ~ colorr*etnia | cluster_enumerator_id + hh_total,     # Fixed effects for country and year
    weights = ~ weight1500,                          # Applying weights
    lapop_analysis,                                             # Data source
    cluster = ~ cluster_enumerator_id,               # Clustered standard errors
    split = ~ countrycode
  )

# Model 4: Correlation of skin tone on years of schooling
ed_skin_tone <- 
  feols(
    scale(edc) ~ i(colorr) | cluster_enumerator_id,  # Fixed effects for country and year
    weights = ~ weight1500,                          # Applying weights
    lapop_analysis,                                             # Data source
    cluster = ~ cluster_enumerator_id,               # Clustered standard errors
    split = ~ countrycode
  )

# Model 5: Correlation of Ethnoracial cat. on years of schooling
ed_race <- 
  feols(
    scale(edc) ~ etnia | cluster_enumerator_id,     # Fixed effects for country and year
    weights = ~ weight1500,                          # Applying weights
    lapop_analysis,                                             # Data source
    cluster = ~ cluster_enumerator_id,               # Clustered standard errors
    split = ~ countrycode
  )

# Model 6: Correlation of Skin tone x Ethnoracial cat. on years of schooling
ed_skin_tone_by_race <- 
  feols(
    scale(edc) ~ colorr*etnia | cluster_enumerator_id,     # Fixed effects for country and year
    weights = ~ weight1500,                          # Applying weights
    lapop_analysis,                                             # Data source
    cluster = ~ cluster_enumerator_id,               # Clustered standard errors
    split = ~ countrycode
  )

# Initialize an empty tibble to store the results for the split models
var_decomp_country <- tibble()

# Income
# Loop through elements 1 to 24 (split models by country) and calculate the within adjusted R-squared (war2)
for (i in 1:length(income_skin_tone)) {
  var_decomp_country <- 
    var_decomp_country |> 
    rbind(
      tibble(
        countrycode = income_skin_tone[[i]]$model_info$sample$value,
        r2_skin_tone = r2(income_skin_tone[[i]], "war2"),
        r2_etnia =  r2(income_race[[i]], "war2"),
        r2_skin_tone_etnia = r2(income_skin_tone_by_race[[i]], "war2"),
        outcome = "Hh. income pc",                                       # Outcome variable
      )
    )
}

# Years of schooling
# Loop through elements 1 to 24 (split models by country) and calculate the within adjusted R-squared (war2)
for (i in 1:length(ed_skin_tone)) {
  var_decomp_country <- 
    var_decomp_country |> 
    rbind(
      tibble(
        countrycode = ed_skin_tone[[i]]$model_info$sample$value,
        r2_skin_tone = r2(ed_skin_tone[[i]], "war2"),
        r2_etnia =  r2(ed_race[[i]], "war2"),
        r2_skin_tone_etnia = r2(ed_skin_tone_by_race[[i]], "war2"),
        outcome = "Years of schooling",
      )
    )
}

# Plot
# Pivot to longer tibble
var_decomp_country |> 
  pivot_longer(
    cols = c(r2_skin_tone, r2_etnia, r2_skin_tone_etnia),
    names_to = "type",
    values_to = "war2"
  ) |> 
  mutate(
    # Rename
    type = case_when(
      type == "r2_skin_tone" ~ "Skin tone",
      type == "r2_etnia" ~ "Ethnoracial cat.",
      T ~ "Skin tone x Ethnoracial cat",
    ),
    type = fct_relevel(
      type, "Skin tone x Ethnoracial cat", "Ethnoracial cat.", "Skin tone"
    ),
    # Reorder the levels of the 'outcome' factor
    outcome = fct_relevel(
      outcome, "Years of schooling", "Hh. income pc"
    ),
  ) |> 
  ggplot(aes(war2, outcome, color = type, shape = type)) +
  geom_point(
    position = position_dodge(width = .5), size = 1.75
  ) +
  scale_color_viridis(option = "magma", discrete = TRUE, direction = 1, begin = .25, end = .75) +
  # scale_x_continuous(breaks = scales::pretty_breaks(4)) +
  labs(x = expression(Within ~ Adjusted ~ R^2), y = "", color = "", shape = "") + 
  theme(
    legend.position = "bottom",            # Position legend below the plot
    legend.direction = "horizontal",       # Set legend direction to horizontal
    legend.box = "horizontal",              # Arrange the legend items in a single row
    legend.text = element_text(size = 10),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_text(size = 8),
    panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
    panel.grid.major.y = element_blank()
  ) +
  facet_wrap(~ countrycode)


ggsave(
  filename = "plots/FigB2.pdf",
  dpi = 300,
  width = 12, 
  height = 9,
)

#

# -- Figure B3: Mestizo Subsample ----
foo <- 
  lapop_analysis |> 
  filter(etnia == "Mestizo")

# --- Income: Saturated model - Plot ----

ame_weights <- 
  foo |> 
  filter(!is.na(hhincomepc_mx0)) |> 
  group_by(colorr) |> 
  count() |> 
  ungroup() |> 
  mutate(
    n = n/sum(n)
  ) |> 
  filter(colorr > 1) |> 
  select(n)

ame_weights <- c(0, ame_weights$n)


income <- 
  feols(
    hhincomepc_mx0 ~  i(colorr) 
    | cluster_enumerator_id + mothers_ed_isced + sex + age + hh_total,
    weights = ~ weight1500,
    foo, cluster = ~ cluster_enumerator_id
  )

income_linear <- 
  feols(
    hhincomepc_mx0 ~ colorr
    | cluster_enumerator_id + mothers_ed_isced + sex + age + hh_total,
    weights = ~ weight1500,
    foo, cluster = ~ cluster_enumerator_id
  )

income_tibble <- est_meff_tibble(income)

plot_skin_tone_gaps(
  data_plot = income_tibble, 
  feols_linear_est = income_linear,
  ame_weights = ame_weights,
  estimate_type = " (log-points)"
)

# Save figure only when run outside RMarkdown
if (!knitr::is_html_output() && !knitr::is_latex_output()) {
  ggsave(
    filename = "plots/FigB3a.pdf",
    dpi = 300,
    width = 7.5,
    height = 5
  )
}

#
# --- Years of schooling: Saturated model - Plot ----

ame_weights <- 
  foo |> 
  group_by(colorr) |> 
  count() |> 
  ungroup() |> 
  mutate(
    n = n/sum(n)
  ) |> 
  filter(colorr > 1) |> 
  select(n)

ame_weights <- c(0, ame_weights$n)

ed <- 
  feols(
    scale(edc) ~ i(colorr) 
    | cluster_enumerator_id + mothers_ed_isced + sex + age,
    weights = ~ weight1500,
    foo, cluster = ~  cluster_enumerator_id
  )

ed_linear <- 
  feols(
    scale(edc) ~ colorr 
    | cluster_enumerator_id + mothers_ed_isced + sex + age,
    weights = ~ weight1500,
    foo, cluster = ~  cluster_enumerator_id
  )


ed_tibble <- est_meff_tibble(ed)

plot_skin_tone_gaps(
  data_plot = ed_tibble, 
  feols_linear_est = ed_linear,
  ame_weights = ame_weights,
  estimate_type = " (z-score)"
)

# Save figure only when run outside RMarkdown
if (!knitr::is_html_output() && !knitr::is_latex_output()) {
  ggsave(
    filename = "plots/FigB3b.pdf",
    dpi = 300,
    width = 7.5,
    height = 5
  )
}

#
# -- Figure B4: Heterogeneous effects by ethnoracial identity and gender ----
# --- Human capital accumulation ----
no_obs_group <- 
  lapop_analysis |> 
  filter(!is.na(edc)) |> 
  group_by(etnia, sex) |> 
  summarise(
    n_obs = n()
  ) |> 
  ungroup()

hc_benchmark <- 
  feols(
    scale(edc) ~ colorr
    | cluster_enumerator_id + sex + age + mothers_ed_isced + etnia,
    weights = ~ weight1500,
    lapop_analysis, cluster = ~ cluster_enumerator_id,
  )

hc_het_etnia_sex <- 
  feols(
    scale(edc) ~ colorr:etnia:sex + etnia * sex
    | cluster_enumerator_id + age + mothers_ed_isced,
    weights = ~ weight1500,
    lapop_analysis, cluster = ~ cluster_enumerator_id,
  )

# Skin tone estimates by group
p1 <- 
  tidy(hc_het_etnia_sex) |> 
  filter(str_detect(term, "colorr:")) |> 
  mutate(
    term = str_remove_all(term, "colorr:"),
    etnia = str_split(term, ":", simplify = T)[,1],
    etnia = str_remove_all(etnia, "etnia"),
    etnia = fct_relevel(etnia, "White", "Mestizo", "Indigenous", "Black", "Other"),
    sex = str_split(term, ":", simplify = T)[,2],
    sex = str_remove_all(sex, "sex"),
    uci95 = estimate + 1.96*std.error,
    lci95 = estimate - 1.96*std.error,
    uci90 = estimate + 1.64*std.error,
    lci90 = estimate - 1.64*std.error,
  ) |> 
  left_join(
    no_obs_group
  ) |> 
  ggplot(aes(estimate, etnia, color = etnia, shape = sex)) +
  geom_vline(xintercept = 0) +
  # Mean estimate with CI
  geom_vline(xintercept = hc_benchmark$coefficients[1], color = "grey") +
  annotate(
    "rect", ymin = -Inf, ymax = Inf, 
    xmin = hc_benchmark$coefficients[1] - 1.96*hc_benchmark$se[1], 
    xmax = hc_benchmark$coefficients[1] + 1.96*hc_benchmark$se[1], 
    alpha = .3,fill = "grey"
  ) +
  geom_linerange(aes(xmin = lci95, xmax = uci95), lwd = .5, width = .1, position = position_dodge(width = .5)) +
  geom_linerange(aes(xmin = lci90, xmax = uci90), lwd = 1, position = position_dodge(width = .5)) +
  geom_point(size = 3.5, position = position_dodge(width = .5)) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  scale_x_continuous(breaks = seq(.2, -.2, -0.02)) +
  labs(x = "", y = "Skin tone estimate (z-score)", shape = "") +
  guides(color = F) +
  # Add number of observations by group on the right side
  geom_text(
    aes(x = Inf, label = n_obs), 
    position = position_dodge(width = .5),
    hjust = .5, color = "black", size = 2.25
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme(
    axis.text.y = element_text(size = 10)
  )


# Constant estimates by group
p2 <-
  tidy(hc_het_etnia_sex) |> 
  filter(!str_detect(term, "colorr:")) |> 
  mutate(
    sex = if_else(str_detect(term, "sex") == T, "Male", "Female"),
    etnia = str_remove_all(term, ":sexMale"),
    etnia = str_remove_all(etnia, "etnia"),
    etnia = if_else(etnia == "sexMale", "White", etnia)
  ) |> 
  select(etnia, sex, everything()) |> 
  select(-term, -statistic, -p.value) |> 
  bind_rows(
    tibble(
      sex = "Female",
      etnia = "White",
      estimate = 0
    )
  ) |> 
  arrange(etnia, sex) |> 
  group_by(etnia) |> 
  mutate(
    estimate1 = estimate + lag(estimate),
  ) |> 
  ungroup() |> 
  mutate(
    estimate = if_else(is.na(estimate1), estimate, estimate1),
    etnia = fct_relevel(etnia, "White", "Mestizo", "Indigenous", "Black", "Other"),
    uci95 = estimate + 1.96*std.error,
    lci95 = estimate - 1.96*std.error,
    uci90 = estimate + 1.64*std.error,
    lci90 = estimate - 1.64*std.error,
  ) |> 
  select(-estimate1) |> 
  left_join(
    no_obs_group
  ) |> 
  ggplot(aes(estimate, etnia, color = etnia, shape = sex)) +
  geom_vline(xintercept = 0) +
  geom_linerange(aes(xmin = lci95, xmax = uci95), lwd = .5, width = .1, position = position_dodge(width = .5)) +
  geom_linerange(aes(xmin = lci90, xmax = uci90), lwd = 1, position = position_dodge(width = .5)) +
  geom_point(size = 3.5, position = position_dodge(width = .5)) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  scale_x_continuous(breaks = seq(.4, -.4, -0.1)) +
  labs(x = "", y = "Constant term by group (z-score)", shape = "") +
  guides(color = F) +
  # Add number of observations by group on the right side
  geom_text(
    aes(x = Inf, label = n_obs), 
    position = position_dodge(width = .5),
    hjust = .5, color = "black", size = 2.25
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme(
    axis.text.y = element_text(size = 10)
  )

cowplot::plot_grid(
  p1, NULL, p2,
  labels = c("Skin tone estimate", "", "Constant term"),
  label_fontface = "plain",
  label_size = 11,
  label_x = 0.25, 
  nrow = 1, ncol = 3,
  rel_widths = c(1, 0.1, 1)
)

ggsave("plots/FigB4a.pdf", dpi = 300, width = 10.8, height = 6.6)

#

# --- Income ----
no_obs_group <- 
  lapop_analysis |> 
  filter(!is.na(hhincomepc_mx0)) |> 
  group_by(etnia, sex) |> 
  summarise(
    n_obs = n()
  ) |> 
  ungroup()

inc_benchmark <- 
  feols(
    hhincomepc_mx0 ~ colorr + edc
    | cluster_enumerator_id + age + sex + mothers_ed_isced + etnia,
    weights = ~ weight1500,
    lapop_analysis, cluster = ~ cluster_enumerator_id,
  )

inc_het_etnia_sex <- 
  feols(
    hhincomepc_mx0 ~ colorr:etnia:sex + etnia * sex + edc
    | cluster_enumerator_id + age + mothers_ed_isced,
    weights = ~ weight1500,
    lapop_analysis, cluster = ~ cluster_enumerator_id,
  )

# Skin tone estimates by group
p1 <- 
  tidy(inc_het_etnia_sex) |> 
  filter(str_detect(term, "colorr:")) |> 
  mutate(
    term = str_remove_all(term, "colorr:"),
    etnia = str_split(term, ":", simplify = T)[,1],
    etnia = str_remove_all(etnia, "etnia"),
    etnia = fct_relevel(etnia, "White", "Mestizo", "Indigenous", "Black", "Other"),
    sex = str_split(term, ":", simplify = T)[,2],
    sex = str_remove_all(sex, "sex"),
    uci95 = estimate + 1.96*std.error,
    lci95 = estimate - 1.96*std.error,
    uci90 = estimate + 1.64*std.error,
    lci90 = estimate - 1.64*std.error,
  ) |> 
  left_join(
    no_obs_group
  ) |> 
  ggplot(aes(estimate, etnia, color = etnia, shape = sex)) +
  geom_vline(xintercept = 0) +
  # Mean estimate with CI
  geom_vline(xintercept = inc_benchmark$coefficients[1], color = "grey") +
  annotate(
    "rect", ymin = -Inf, ymax = Inf, 
    xmin = inc_benchmark$coefficients[1] - 1.96*inc_benchmark$se[1], 
    xmax = inc_benchmark$coefficients[1] + 1.96*inc_benchmark$se[1], 
    alpha = .3,fill = "grey"
  ) +
  geom_linerange(aes(xmin = lci95, xmax = uci95), lwd = .5, width = .1, position = position_dodge(width = .5)) +
  geom_linerange(aes(xmin = lci90, xmax = uci90), lwd = 1, position = position_dodge(width = .5)) +
  geom_point(size = 3, position = position_dodge(width = .5)) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  scale_x_continuous(breaks = seq(.2, -.2, -0.02)) +
  labs(x = "", y = "Skin tone estimate (log-points)", shape = "") +
  guides(color = F) +
  # Add number of observations by group on the right side
  geom_text(
    aes(x = Inf, label = n_obs), 
    position = position_dodge(width = .5),
    hjust = .5, color = "black", size = 2.25
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme(
    axis.text.y = element_text(size = 10)
  )

# Constant estimates by group
p2 <-
  tidy(inc_het_etnia_sex) |> 
  filter(!str_detect(term, "colorr:")) |> 
  filter(!str_detect(term, "edc")) |> 
  mutate(
    sex = if_else(str_detect(term, "sex") == T, "Male", "Female"),
    etnia = str_remove_all(term, ":sexMale"),
    etnia = str_remove_all(etnia, "etnia"),
    etnia = if_else(etnia == "sexMale", "White", etnia)
  ) |> 
  select(etnia, sex, everything()) |> 
  select(-term, -statistic, -p.value) |> 
  bind_rows(
    tibble(
      sex = "Female",
      etnia = "White",
      estimate = 0
    )
  ) |> 
  arrange(etnia, sex) |> 
  group_by(etnia) |> 
  mutate(
    estimate1 = estimate + lag(estimate),
  ) |> 
  ungroup() |> 
  mutate(
    estimate = if_else(is.na(estimate1), estimate, estimate1),
    etnia = fct_relevel(etnia, "White", "Mestizo", "Indigenous", "Black", "Other"),
    uci95 = estimate + 1.96*std.error,
    lci95 = estimate - 1.96*std.error,
    uci90 = estimate + 1.64*std.error,
    lci90 = estimate - 1.64*std.error,
  ) |> 
  select(-estimate1) |> 
  left_join(
    no_obs_group
  ) |> 
  ggplot(aes(estimate, etnia, color = etnia, shape = sex)) +
  geom_vline(xintercept = 0) +
  geom_linerange(aes(xmin = lci95, xmax = uci95), lwd = .5, width = .1, position = position_dodge(width = .5)) +
  geom_linerange(aes(xmin = lci90, xmax = uci90), lwd = 1, position = position_dodge(width = .5)) +
  geom_point(size = 3, position = position_dodge(width = .5)) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  scale_x_continuous(breaks = seq(.4, -.4, -0.1)) +
  labs(x = "", y = "Constant term by group (log-points)", shape = "") +
  guides(color = F) +
  # Add number of observations by group on the right side
  geom_text(
    aes(x = Inf, label = n_obs), 
    position = position_dodge(width = .5),
    hjust = .5, color = "black", size = 2.25
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme(
    axis.text.y = element_text(size = 10)
  )

cowplot::plot_grid(
  p1, NULL, p2,
  labels = c("Skin tone estimate", "", "Constant term"),
  label_fontface = "plain",
  label_size = 11,
  label_x = 0.25, 
  nrow = 1, ncol = 3,
  rel_widths = c(1, 0.1, 1)
)

ggsave("plots/FigB4b.pdf", dpi = 300, width = 10.8, height = 6.6)

#

# -- Figure B5: Heterogeneous effects by country in Human capital ----
hc_benchmark <- 
  feols(
    scale(edc) ~ colorr 
    | cluster_enumerator_id + age + sex + mothers_ed_isced,
    weights = ~ weight1500,
    lapop_analysis, cluster = ~ cluster_enumerator_id,
  )

hc_het_country <- 
  feols(
    scale(edc) ~ colorr:countrycode 
    | cluster_enumerator_id + age + sex + mothers_ed_isced,
    weights = ~ weight1500,
    lapop_analysis, cluster = ~ cluster_enumerator_id,
  )

no_obs_by_country <- 
  lapop_analysis |> 
  group_by(countrycode) |> 
  summarise(
    n_obs = n()
  ) |> 
  ungroup()

# Skin tone estimates by group
tidy(hc_het_country) |> 
  filter(str_detect(term, "colorr:")) |> 
  mutate(
    countrycode = str_remove_all(term, "colorr:"),
    countrycode = str_remove_all(countrycode, "countrycode"),
    uci95 = estimate + 1.96*std.error,
    lci95 = estimate - 1.96*std.error,
    uci90 = estimate + 1.64*std.error,
    lci90 = estimate - 1.64*std.error,
  ) |> 
  left_join(
    lapop_analysis |> 
      group_by(countrycode, region) |> summarise()
  ) |> 
  left_join(
    no_obs_by_country
  ) |> 
  ggplot(aes(estimate, reorder(countrycode, estimate), color = region, shape = region)) +
  geom_vline(xintercept = 0) +
  # Mean estimate with CI
  geom_vline(xintercept = hc_benchmark$coefficients[1], color = "grey") +
  annotate(
    "rect", ymin = -Inf, ymax = Inf, 
    xmin = hc_benchmark$coefficients[1] - 1.96*hc_benchmark$se[1], 
    xmax = hc_benchmark$coefficients[1] + 1.96*hc_benchmark$se[1], 
    alpha = .3, fill = "grey"
  ) +
  geom_point(size = 3, position = position_dodge(width = .5)) +
  geom_linerange(aes(xmin = lci95, xmax = uci95), lwd = .5, width = .1, position = position_dodge(width = .5)) +
  geom_linerange(aes(xmin = lci90, xmax = uci90), lwd = 1, position = position_dodge(width = .5)) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  scale_x_continuous(breaks = seq(-.2, .2, 0.02)) +
  labs(x = "Skin tone estimate (z-score)", y = "", shape = "", color = "") +
  # Add number of observations by country on the right side
  geom_text(
    aes(x = Inf, label = n_obs), 
    hjust = .5, color = "black", size = 2.5
  ) +
  coord_cartesian(
    clip = "off"
  )

ggsave(
  filename = "plots/FigB5.pdf",
  dpi = 300,
  width = 8.1,
  height = 10.4
)

#
# --- Figure B6: Heterogeneous effects by country in Income----
no_obs_by_country <- 
  lapop_analysis |> 
  filter(!is.na(hhincomepc_mx0)) |> 
  group_by(countrycode) |> 
  summarise(
    n_obs = n()
  ) |> 
  ungroup()

inc_benchmark <- 
  feols(
    hhincomepc_mx0 ~ colorr + edc
    | cluster_enumerator_id + age + sex + mothers_ed_isced,
    weights = ~ weight1500,
    lapop_analysis, cluster = ~ cluster_enumerator_id,
  )

inc_het_country <- 
  feols(
    hhincomepc_mx0 ~ colorr:countrycode + edc:countrycode 
    | cluster_enumerator_id + age + sex + mothers_ed_isced,
    weights = ~ weight1500,
    lapop_analysis, cluster = ~ cluster_enumerator_id,
  )

# Skin tone estimates by group
tidy(inc_het_country) |> 
  filter(str_detect(term, "colorr:")) |> 
  mutate(
    countrycode = str_remove_all(term, "colorr:"),
    countrycode = str_remove_all(countrycode, "countrycode"),
    uci95 = estimate + 1.96*std.error,
    lci95 = estimate - 1.96*std.error,
    uci90 = estimate + 1.64*std.error,
    lci90 = estimate - 1.64*std.error,
  ) |> 
  left_join(
    lapop_analysis |> 
      group_by(countrycode, region) |> summarise()
  ) |> 
  left_join(
    no_obs_by_country
  ) |> 
  ggplot(aes((estimate), reorder(countrycode, estimate), color = region, shape = region)) +
  geom_vline(xintercept = 0) +
  # Mean estimate with CI
  geom_vline(xintercept = inc_benchmark$coefficients[1], color = "grey") +
  annotate(
    "rect", ymin = -Inf, ymax = Inf, 
    xmin = inc_benchmark$coefficients[1] - 1.96*inc_benchmark$se[1], 
    xmax = inc_benchmark$coefficients[1] + 1.96*inc_benchmark$se[1], 
    alpha = .3,fill = "grey"
  ) +
  geom_point(size = 3, position = position_dodge(width = .5)) +
  geom_linerange(aes(xmin = lci95, xmax = uci95), lwd = .5, width = .1, position = position_dodge(width = .5)) +
  geom_linerange(aes(xmin = lci90, xmax = uci90), lwd = 1, position = position_dodge(width = .5)) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  scale_x_continuous(breaks = seq(-.1, .1, 0.02)) +
  labs(x = "Skin tone estimate (log-points)", y = "", shape = "", color = "") +
  # Add number of observations by country on the right side
  geom_text(
    aes(x = Inf, label = n_obs), 
    hjust = .5, color = "black", size = 2.5
  ) +
  coord_cartesian(
    clip = "off"
  )

ggsave(
  filename = "plots/FigB6.pdf",
  dpi = 300,
  width = 8.1,
  height = 10.4
)

#
