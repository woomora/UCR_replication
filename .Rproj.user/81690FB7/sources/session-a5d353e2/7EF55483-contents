# ------------------------------------------------------------------------------
# Figure 4c: Relative Intergenerational Mobility by Skin Tone Groups
# Description:
#   - Bootstraps slope coefficients (rank-to-rank) for each skin tone group
#   - Visualizes smoothed trends using binsreg
#   - Annotates slopes with bootstrapped SDs
# Output:
#   - 'Results/bootstraped_educ_mobility_slopes.csv'
#   - Figure saved as 'plots/Fig4c.pdf' (if not in RMarkdown)
# ------------------------------------------------------------------------------

# Set reproducibility seed
set.seed(1234)

# --- 1. Define bootstrap parameters ---
nboots <- 1000
perla_levels <- levels(factor(lapop_analysis$colorr))

# Create cluster unit (enumerator-sex-skin tone) identifiers
clusters <- lapop_analysis |>
  group_by(cluster_id, colori, sexi) |>
  summarise(.groups = "drop")

# Draw Dirichlet-distributed weights (Bayesian bootstrap)
weights <- matrix(rexp(nrow(clusters) * nboots, 1), nrow = nboots)
weights <- weights / rowSums(weights)

# Initialize results containers
slopes_boot <- tibble()
gaps_boot <- tibble()

# Progress bar
pb <- progress_bar$new(total = nboots)
pb$tick(0)

# --- 2. Bootstrap loop over 1,000 draws ---
for (i in 1:nboots) {
  pb$tick()
  
  tryCatch({
    # Apply bootstrap weights per cluster
    reg <- feols(
      ed_pct ~ mothers_ed_pct * i(colorr) + sex + i(age) |
        cluster_enumerator_id,
      data = lapop_analysis,
      weights = lapop_analysis$weight1500 * weights[i,],
      cluster = ~ cluster_enumerator_id
    )
    
    # Get baseline intercept
    intercept <- mean(fixef(reg)$`cluster_enumerator_id`, na.rm = TRUE)
    
    # Extract skin tone intercepts (for expected gaps)
    constants <- broom::tidy(reg) |>
      filter(str_detect(term, "colorr"), !str_detect(term, "mothers_ed_pct")) |>
      select(term, estimate)
    
    # Extract marginal effects
    slopes <- broom::tidy(reg) |>
      filter(str_detect(term, "mothers_ed_pct")) |>
      mutate(
        colorr = perla_levels,
        slope = ifelse(term == "mothers_ed_pct", estimate, estimate + .data$estimate[1])
      ) |>
      select(colorr, slope)
    
    # Compute expected ranks at 5%, 25%, 50%, 75%, 95%
    expected_gaps <- tibble()
    for (j in seq_along(perla_levels)) {
      offset <- ifelse(j == 1, 0, constants$estimate[j - 1])
      expected_gaps <- expected_gaps |>
        bind_rows(tibble(
          colorr = perla_levels[j],
          epct05 = intercept + offset + slopes$slope[j] * 5,
          epct25 = intercept + offset + slopes$slope[j] * 25,
          epct50 = intercept + offset + slopes$slope[j] * 50,
          epct75 = intercept + offset + slopes$slope[j] * 75,
          epct95 = intercept + offset + slopes$slope[j] * 95
        ))
    }
    
    # Save slopes and gaps
    slopes_boot <- bind_rows(slopes_boot, slopes |> mutate(boot = i))
    gaps_boot <- bind_rows(gaps_boot, expected_gaps |> mutate(boot = i))
    
  }, error = function(e) {})
}

# Save results
write_csv(slopes_boot, "Results/bootstraped_educ_mobility_slopes.csv")
write_csv(gaps_boot, "Results/bootstraped_educ_mobility_gaps.csv")

# --- 3. Smooth estimates via binsreg ---
p1 <- binsreg(
  y = lapop_analysis$ed_pct,
  x = lapop_analysis$mothers_ed_pct,
  w = ~ factor(countrycode) + factor(cohort) + factor(sex),
  by = factor(lapop_analysis$colorr),
  weights = lapop_analysis$weight1500,
  data = lapop_analysis,
  polyreg = 1,
  binsmethod = "rot",
  nbinsrot = 10
)

# Combine binned data (dots + fitted lines) from all 9 groups
data_dots <- bind_rows(lapply(1:9, function(g) {
  p1$data.plot[[paste0("Group ", g)]]$data.dots |> tibble()
}))

data_poly <- bind_rows(lapply(1:9, function(g) {
  p1$data.plot[[paste0("Group ", g)]]$data.poly |> tibble()
}))

# --- 4. Load and summarize slopes ---
slopes_summary <- read_csv("Results/bootstraped_educ_mobility_slopes.csv") |>
  group_by(colorr) |>
  summarise(
    mean = mean(slope),
    sd = sd(slope),
    .groups = "drop"
  ) |>
  mutate(
    colorr_num = 1:9,
    mean = formatC(mean, format = "f", digits = 3),
    sd = formatC(sd, format = "f", digits = 3),
    x = if_else(colorr_num <= 4, 15, 87.5),
    y = if_else(colorr_num <= 4, 95 - colorr_num * 7.5, 75 - colorr_num * 7.5)
  )

# --- 5. Construct the plot with slope labels ---
fig4c <- 
  ggplot() +
  geom_line(data = data_poly, aes(x, fit, color = factor(group))) +
  geom_point(data = data_dots, aes(x, fit), size = 2.75, shape = 21, stroke = 0.75, color = "#7f7f7f", fill = NA) +
  geom_point(data = data_dots, aes(x, fit, color = factor(group)), size = 2.5) +
  geom_vline(xintercept = c(25, 75), linetype = "dotted") +
  geom_abline(intercept = 0, linetype = "dashed") +
  scale_color_manual(values = perla_palette(9)) +
  coord_cartesian(ylim = c(0, 100), clip = "off") +
  labs(
    x = "Mother's educational percentile rank",
    y = "Individual's educational percentile rank"
  ) +
  theme(legend.position = "none") +
  geom_label(
    data = slopes_summary,
    aes(
      x = x, y = y, fill = factor(colorr),
      label = str_c("Slope PERLA ", colorr, ": ", mean, " (", sd, ")")
    ),
    size = 3.25, alpha = 0.8
  ) +
  scale_fill_manual(values = perla_palette(9)) +
  guides(fill = "none") 

# --- 6. Save plot if not in RMarkdown ---
if (!knitr::is_html_output() && !knitr::is_latex_output()) {
  ggsave("plots/Fig4c.pdf", fig4c, dpi = 300, width = 7.5, height = 5)
}
