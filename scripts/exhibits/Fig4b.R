# ------------------------------------------------------------------------------
# Figure 4b: Relative Educational Mobility – Full Sample
# Description:
#   - Shows the relationship between a respondent’s educational percentile rank
#     and their mother's percentile rank.
#   - Includes both a non-parametric binned fit (binsreg) and linear slopes from:
#       (a) Between-country specification (FE at country-year level)
#       (b) Within-country specification (FE at enumerator-cluster level)
# Output:
#   - Plot object
#   - PDF saved as 'plots/Fig4b.pdf' if run outside RMarkdown
# Dependencies:
#   - Requires `binsreg`, `fixest`, and `ggplot2`
# ------------------------------------------------------------------------------
# --- 1. Non-parametric regression using binsreg (data-driven binning) ---
social_mob <- binsreg(
  y = lapop_analysis$ed_pct,
  x = lapop_analysis$mothers_ed_pct,
  w = ~ factor(countrycode) + factor(year),
  data = lapop_analysis,
  polyreg = 1,             # linear polynomial
  binsmethod = "rot",      # rule-of-thumb binning
  nbinsrot = 25,           # maximum number of bins
  weights = lapop_analysis$weight1500
)

# --- 2. FEOLS: Linear regression with country-year fixed effects ---
edu_country <- feols(
  ed_pct ~ mothers_ed_pct + sex + age | countrycode^year,
  data = lapop_analysis,
  weights = ~ weight1500
)

# --- 3. FEOLS: Linear regression with enumerator-cluster fixed effects ---
edu_cluster <- feols(
  ed_pct ~ mothers_ed_pct + sex + age | cluster_enumerator_id,
  data = lapop_analysis,
  weights = ~ weight1500
)

# --- 4. Construct ggplot combining binsreg + regression slopes ---
fig4b <- ggplot() +
  # Add binned means as dots
  geom_point(
    data = social_mob$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit), color = "#2A788EFF", size = 2.75
  ) +
  # Add non-parametric trend line
  geom_line(
    data = social_mob$data.plot$`Group Full Sample`$data.poly,
    aes(x, fit), color = "#2A788EFF"
  ) +
  # Labels, scales, and theme
  theme_clean +  # assumes you have a predefined theme_clean object
  labs(
    x = "Mother's educational percentile rank",
    y = "Individual's educational percentile rank"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  
  # Annotation: between-country slope
  geom_label(
    aes(
      x = 85, y = 50,
      label = str_c(
        "Slope between countries:\n",
        round(edu_country$coefficients[1], 3),
        " (", round(edu_country$se[1], 3), ")"
      )
    ),
    size = 3.5, color = "#2A788EFF"
  ) +
  
  # Annotation: within-country slope
  geom_label(
    aes(
      x = 85, y = 35,
      label = str_c(
        "Slope within countries:\n",
        round(edu_cluster$coefficients[1], 3),
        " (", round(edu_cluster$se[1], 3), ")"
      )
    ),
    size = 3.5, color = "#2A788EFF"
  )

# --- 5. Save plot if not running in RMarkdown ---
if (!knitr::is_html_output() && !knitr::is_latex_output()) {
  ggsave(
    filename = "plots/Fig4b.pdf",
    plot = fig4b,
    dpi = 300,
    width = 7.5,
    height = 5
  )
}
