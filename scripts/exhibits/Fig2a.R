## ------------------------------------------------------------------------------
# Figure 2a: Income Gaps by Skin Tone (Saturated Model)
# Description:
#   - Estimates skin tone gaps in household income per capita using FEOLS.
#   - Applies cluster fixed effects and relevant controls.
#   - Computes Average Marginal Effect (AME) weighted by skin tone distribution.
#   - Generates a plot comparing categorical and linear specifications.
# Output: Saved as 'plots/Fig2a.pdf' unless run inside RMarkdown
# ------------------------------------------------------------------------------

# Compute weights for AME based on skin tone distribution (excluding tone 1)
ame_weights <- lapop_analysis |> 
  filter(!is.na(hhincomepc_mx0)) |> 
  group_by(colorr) |> 
  count() |> 
  ungroup() |> 
  mutate(n = n / sum(n)) |> 
  filter(colorr > 1) |> 
  select(n)

# Add zero weight for baseline category (tone 1)
ame_weights <- c(0, ame_weights$n)

# Run saturated FEOLS regression with cluster and demographic controls
income <- feols(
  hhincomepc_mx0 ~ i(colorr) |
    cluster_enumerator_id + mothers_ed_isced + sex + age + hh_total,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_enumerator_id
)

# Run linear skin tone model for comparison (Figure label: “OLS”)
income_linear <- feols(
  hhincomepc_mx0 ~ colorr |
    cluster_enumerator_id + mothers_ed_isced + sex + age + hh_total,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_enumerator_id
)

# Extract marginal effects and prepare for plotting
income_tibble <- est_meff_tibble(income)

# Plot categorical estimates vs. linear model (Figure 2a in manuscript)
plot_skin_tone_gaps(
  data_plot = income_tibble,
  feols_linear_est = income_linear,
  ame_weights = ame_weights,
  estimate_type = " (log-points)"
)

# Save figure only when run outside RMarkdown
if (!knitr::is_html_output() && !knitr::is_latex_output()) {
  ggsave(
    filename = "plots/Fig2a.pdf",
    dpi = 300,
    width = 7.5,
    height = 5
  )
}
