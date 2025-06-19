# ------------------------------------------------------------------------------
# Table 1: Comparing Estimates Across Datasets and Skin Tone Measures
# This script produces 3 panels of results:
#   - Panel A: Enumerator-perceived skin tone (LAPOP + PRODER)
#   - Panel B: Respondent-perceived skin tone (PRODER)
#   - Panel C: Machine-assessed skin tone (PRODER)
# Each panel reports results for:
#   - Years of schooling (z-score)
#   - Absolute educational mobility (p.p.)
# Outputs: Three separate LaTeX tables via etable()
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

lapop_mex <- lapop_analysis |> filter(countrycode == "MEX")
proder <- read_rds("data/derived/proder.rds")

# ------------------------------------------------------------------------------
# PANEL A: Enumerator-perceived skin tone (LAPOP and PRODER)
# ------------------------------------------------------------------------------

# --- LAPOP models ---
edc_lapop <- feols(
  scale(edc) ~ scale(colorr) | cluster_enumerator_id + age + sex + mothers_ed_isced, 
  data = lapop_mex,
  weights = ~ weight1500,
  cluster = ~ cluster_enumerator_id
)

abs_ed_lapop <- feols(
  abs_ed_im ~ scale(colorr) | cluster_enumerator_id + age + sex + mothers_ed_isced, 
  data = lapop_mex,
  weights = ~ weight1500,
  cluster = ~ cluster_enumerator_id
)

# --- PRODER models (enumerator perception) ---
# Baseline controls only
edc_proder_int <- feols(
  scale(edc) ~ scale(color_int) | cp_mun^id_encuestador + edc_padres + edad + mujer, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)
abs_ed_proder_int <- feols(
  abs_ed_im ~ scale(color_int) | cp_mun^id_encuestador + edc_padres + edad + mujer, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)

# Extended controls (extra covariates)
edc_proder_int_c <- feols(
  scale(edc) ~ scale(color_int) | cp_mun^id_encuestador + edc_padres + edad + mujer + barrio14yo + father_ocup14yo, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)
abs_ed_proder_int_c <- feols(
  abs_ed_im ~ scale(color_int) | cp_mun^id_encuestador + edc_padres + edad + mujer + barrio14yo + father_ocup14yo, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)

# --- Panel A output ---
setFixest_dict(c(
  "scale(edc)" = "Years of Schooling (z-score)",
  "abs_ed_im" = "Absolute Educational Mobility (p.p.)",
  "scale(colorr)" = "Skin tone (z-score)",
  "scale(color_int)" = "Skin tone (z-score)"
))

etable(
  title = "Panel A. Enumerator’s Perception",
  edc_lapop, edc_proder_int, edc_proder_int_c,
  abs_ed_lapop, abs_ed_proder_int, abs_ed_proder_int_c,
  tex = TRUE, tpt = TRUE, signif.code = NA, digits = 3, digits.stats = 4,
  fitstat = ~ my + n + r2 + ar2,
  drop.section = "fixef", style.tex = style.tex("aer"),
  extralines = list(
    "__Baseline controls" = rep("Yes", 6),
    "__Extra controls" = rep(c(rep("No", 2), "Yes"), 2)
  ),
  notes = 
    "\\footnotesize \\textit{Notes:}
        Specifications use LAPOP and PRODER data. 
        LAPOP specifications use the PERLA color palette, which includes scales from 1 to 9, with skin tones above 9 top-coded due to their small share.
        PRODER specifications for the enumerator's and respondent's perception use the inverse PRODER color palette, which includes scales from 1 to 11, where 1 represents the lightest skin tone and 11 the darkest, consistent with the PERLA interpretation.
        The PRODER specification using the colorimeter measures uses the inverse ITA from the respondent's hand.
        Baseline controls include gender, age (saturated), parental years of schooling (saturated), and clustering within municipality $\times$ year $\times$ enumerator ID.
        Note that in PRODER, parental years of schooling is the average of both father’s and mother’s schooling, while LAPOP only uses the mother's schooling.
        In PRODER, within-municipality clustering is measured at the ZIP code level.
        Extra controls in the PRODER dataset include the respondent's neighborhood of residence at age 14 and the father's 3-digit Standard Occupational Classification at the same age.
  "
)

# ------------------------------------------------------------------------------
# PANEL B: Respondent-perceived skin tone (PRODER only)
# ------------------------------------------------------------------------------

edc_proder_resp <- feols(
  scale(edc) ~ scale(color_resp) | cp_mun^id_encuestador + edc_padres + edad + mujer, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)
edc_proder_resp_c <- feols(
  scale(edc) ~ scale(color_resp) | cp_mun^id_encuestador + edc_padres + edad + mujer + barrio14yo + father_ocup14yo, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)

abs_ed_proder_resp <- feols(
  abs_ed_im ~ scale(color_resp) | cp_mun^id_encuestador + edc_padres + edad + mujer, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)
abs_ed_proder_resp_c <- feols(
  abs_ed_im ~ scale(color_resp) | cp_mun^id_encuestador + edc_padres + edad + mujer + barrio14yo + father_ocup14yo, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)

etable(
  title = "Panel B. Respondent’s Perception",
  edc_proder_resp, edc_proder_resp_c,
  abs_ed_proder_resp, abs_ed_proder_resp_c,
  tex = TRUE, tpt = TRUE, signif.code = NA, digits = 3, digits.stats = 3,
  fitstat = ~ my + n + r2 + ar2,
  drop.section = "fixef", style.tex = style.tex("aer"),
  extralines = list(
    "__Baseline controls" = rep("Yes", 4),
    "__Extra controls" = rep(c(rep("No", 1), "Yes"), 2)
  ),
  notes = 
    "\\footnotesize \\textit{Notes:}
        Specifications use LAPOP and PRODER data. 
        LAPOP specifications use the PERLA color palette, which includes scales from 1 to 9, with skin tones above 9 top-coded due to their small share.
        PRODER specifications for the enumerator's and respondent's perception use the inverse PRODER color palette, which includes scales from 1 to 11, where 1 represents the lightest skin tone and 11 the darkest, consistent with the PERLA interpretation.
        The PRODER specification using the colorimeter measures uses the inverse ITA from the respondent's hand.
        Baseline controls include gender, age (saturated), parental years of schooling (saturated), and clustering within municipality $\times$ year $\times$ enumerator ID.
        Note that in PRODER, parental years of schooling is the average of both father’s and mother’s schooling, while LAPOP only uses the mother's schooling.
        In PRODER, within-municipality clustering is measured at the ZIP code level.
        Extra controls in the PRODER dataset include the respondent's neighborhood of residence at age 14 and the father's 3-digit Standard Occupational Classification at the same age.
  "
)

# ------------------------------------------------------------------------------
# PANEL C: Machine-assessed skin tone (inverse ITA, PRODER only)
# ------------------------------------------------------------------------------

edc_proder_ita <- feols(
  scale(edc) ~ scale(ita_invsd) | cp_mun^id_encuestador + edc_padres + edad + mujer, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)
edc_proder_ita_c <- feols(
  scale(edc) ~ scale(ita_invsd) | cp_mun^id_encuestador + edc_padres + edad + mujer + barrio14yo + father_ocup14yo, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)

abs_ed_proder_ita <- feols(
  abs_ed_im ~ scale(ita_invsd) | cp_mun^id_encuestador + edc_padres + edad + mujer, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)
abs_ed_proder_ita_c <- feols(
  abs_ed_im ~ scale(ita_invsd) | cp_mun^id_encuestador + edc_padres + edad + mujer + barrio14yo + father_ocup14yo, 
  data = proder, weights = ~ facinp, cluster = ~ cp_mun^id_encuestador
)

etable(
  title = "Panel C. Colorimeter Measure – Inverse ITA",
  edc_proder_ita, edc_proder_ita_c,
  abs_ed_proder_ita, abs_ed_proder_ita_c,
  tex = TRUE, tpt = TRUE, signif.code = NA, digits = 3, digits.stats = 3,
  fitstat = ~ my + n + r2 + ar2,
  drop.section = "fixef", style.tex = style.tex("aer"),
  extralines = list(
    "__Baseline controls" = rep("Yes", 4),
    "__Extra controls" = rep(c(rep("No", 1), "Yes"), 2)
  ),
  notes = 
    "\\footnotesize \\textit{Notes:}
        Specifications use LAPOP and PRODER data. 
        LAPOP specifications use the PERLA color palette, which includes scales from 1 to 9, with skin tones above 9 top-coded due to their small share.
        PRODER specifications for the enumerator's and respondent's perception use the inverse PRODER color palette, which includes scales from 1 to 11, where 1 represents the lightest skin tone and 11 the darkest, consistent with the PERLA interpretation.
        The PRODER specification using the colorimeter measures uses the inverse ITA from the respondent's hand.
        Baseline controls include gender, age (saturated), parental years of schooling (saturated), and clustering within municipality $\times$ year $\times$ enumerator ID.
        Note that in PRODER, parental years of schooling is the average of both father’s and mother’s schooling, while LAPOP only uses the mother's schooling.
        In PRODER, within-municipality clustering is measured at the ZIP code level.
        Extra controls in the PRODER dataset include the respondent's neighborhood of residence at age 14 and the father's 3-digit Standard Occupational Classification at the same age.
  "
)

# ------------------------------------------------------------------------------
# Additional Statistics
# ------------------------------------------------------------------------------
# Means and SDs of dependent variables (for completeness)
round(mean(lapop_mex$edc, na.rm = TRUE), 2)
round(mean(proder$edc, na.rm = TRUE), 2)
round(mean(lapop_mex$abs_ed_im, na.rm = TRUE), 2)
round(mean(proder$abs_ed_im, na.rm = TRUE), 2)

round(sd(lapop_mex$edc, na.rm = TRUE), 2)
round(sd(proder$edc, na.rm = TRUE), 2)
round(sd(lapop_mex$abs_ed_im, na.rm = TRUE), 2)
round(sd(proder$abs_ed_im, na.rm = TRUE), 2)

# ------------------------------------------------------------------------------
# Compare LAPOP vs PRODER coefficient differences (statistical tests)
# ------------------------------------------------------------------------------
compare_fixest_coefficients <- function(model1, model2) {
  beta1 <- model1$coefficients[1]
  se1 <- model1$se[1]
  beta2 <- model2$coefficients[1]
  se2 <- model2$se[1]
  z_stat <- (beta1 - beta2) / sqrt(se1^2 + se2^2)
  p_value <- round(2 * pnorm(-abs(z_stat)), 4)
  return(list(z_stat = unname(z_stat), p_value = unname(p_value)))
}

# Print p-values comparing LAPOP and PRODER estimates
compare_fixest_coefficients(edc_lapop, edc_proder_int)$p_value
compare_fixest_coefficients(abs_ed_lapop, abs_ed_proder_int)$p_value

#