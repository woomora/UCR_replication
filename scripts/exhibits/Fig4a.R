# ------------------------------------------------------------------------------
# Figure 4a: Absolute Educational Mobility by Skin Tone
# Description:
#   - Estimates absolute intergenerational mobility (IM) in education using skin tone.
#   - Controls for cluster FE, sex, age, and mother's education level.
#   - Compares categorical specification (indicators for skin tone) and linear.
#   - Outputs marginal effects and saves the plot, unless inside RMarkdown.
# Output:
#   - PDF file saved as 'plots/Fig4a.pdf' when run outside RMarkdown.
# Dependencies:
#   - Requires `est_meff_tibble()` and `plot_skin_tone_gaps()` defined elsewhere.
#   - Variable `abs_ed_im` should represent absolute educational mobility outcome.
# ------------------------------------------------------------------------------

# --- 1. Compute weights for Average Marginal Effect (AME) ---
# These weights reflect the skin tone distribution in the estimation sample.
ame_weights <- lapop_analysis |> 
  filter(!is.na(abs_ed_im)) |> 
  group_by(colorr) |> 
  count() |> 
  ungroup() |> 
  mutate(n = n / sum(n)) |> 
  filter(colorr > 1) |> 
  select(n)

# Add a zero for the baseline category (tone 1)
ame_weights <- c(0, ame_weights$n)

# --- 2. Estimate saturated model with skin tone indicators ---
# Cluster fixed effects include triple interaction for geography, enumerator tone, and enumerator sex.
abs_ed_im <- feols(
  abs_ed_im ~ i(colorr) |
    cluster_id^colori^sexi + mothers_ed_isced + sex + age,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_id^colori^sexi
)

# --- 3. Estimate linear skin tone model for comparison ---
abs_ed_im_linear <- feols(
  abs_ed_im ~ colorr |
    cluster_id^colori^sexi + mothers_ed_isced + sex + age,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_id^colori^sexi
)

# --- 4. Extract marginal effects for plotting ---
abs_ed_im_tibble <- est_meff_tibble(abs_ed_im)

# --- 5. Generate plot of categorical and linear effects ---
plot_skin_tone_gaps(
  data_plot = abs_ed_im_tibble, 
  feols_linear_est = abs_ed_im_linear,
  ame_weights = ame_weights,
  estimate_type = " (p.p.)"  # percentage point scale
)

# --- 6. Save plot only if not running inside RMarkdown ---
if (!knitr::is_html_output() && !knitr::is_latex_output()) {
  ggsave(
    filename = "plots/Fig4a.pdf",
    dpi = 300,
    width = 7.5,
    height = 5
  )
}
#