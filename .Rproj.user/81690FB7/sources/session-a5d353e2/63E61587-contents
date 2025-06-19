# ------------------------------------------------------------------------------
# Figure 2b: Education Gaps by Skin Tone (Saturated Model)
# Description:
#   - Estimates z-score standardized years of schooling across PERLA skin tone scale.
#   - Uses fixed effects for cluster, age, sex, and mother's education level.
#   - Computes and plots Average Marginal Effect (AME) with skin tone weights.
# Output: Saved as 'plots/Fig2b.pdf' unless run inside RMarkdown
# ------------------------------------------------------------------------------

# Compute AME weights based on observed skin tone distribution
ame_weights <- lapop_analysis |> 
  group_by(colorr) |> 
  count() |> 
  ungroup() |> 
  mutate(n = n / sum(n)) |> 
  filter(colorr > 1) |> 
  select(n)

# Add weight zero for base category (tone 1)
ame_weights <- c(0, ame_weights$n)

# Run saturated model with indicator terms for each tone (excluding baseline)
ed <- feols(
  scale(edc) ~ i(colorr) |
    cluster_enumerator_id + mothers_ed_isced + sex + age,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_enumerator_id
)

# Run linear model with continuous skin tone
ed_linear <- feols(
  scale(edc) ~ colorr |
    cluster_enumerator_id + mothers_ed_isced + sex + age,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_enumerator_id
)

# Extract estimated marginal effects for plotting
ed_tibble <- est_meff_tibble(ed)

# Optional: compute AME (not used in plot directly, but useful for annotation)
avg_meff <- ed_tibble |> 
  select(estimate_mg) |> 
  mutate(weight = ame_weights) |> 
  summarise(
    estimate_mg = weighted.mean(estimate_mg, weight, na.rm = TRUE)
  )

# Plot education gaps across skin tone scale (Figure 2b in manuscript)
plot_skin_tone_gaps(
  data_plot = ed_tibble,
  feols_linear_est = ed_linear,
  ame_weights = ame_weights,
  estimate_type = " (z-score)"
)

# Save plot only if not knitting to RMarkdown
if (!knitr::is_html_output() && !knitr::is_latex_output()) {
  ggsave(
    filename = "plots/Fig2b.pdf",
    dpi = 300,
    width = 7.5,
    height = 5
  )
}
