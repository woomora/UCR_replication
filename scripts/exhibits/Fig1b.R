# ------------------------------------------------------------------------------
# Figure 1b: Skin Tone Distribution by Ethnoracial Identity
# Description: Computes the distribution of enumerator-assessed skin tone (PERLA scale)
# within each self-declared ethnoracial category. Excludes missing values.
# Output: Saved as 'plots/Fig1b.pdf'
# ------------------------------------------------------------------------------

# Compute skin tone share within each ethnoracial category
lapop_analysis |>  
  group_by(etnia, colorr) |>  
  summarize(n = n(), .groups = "drop") |>  # count observations per tone-identity pair
  left_join(
    lapop |>  
      filter(!is.na(etnia), !is.na(colorr)) |>  
      group_by(etnia) |>  
      summarize(n_etnia = n(), .groups = "drop")  # total per ethnoracial group
  ) |>  
  mutate(
    shr = n / n_etnia  # share of each skin tone within group
  ) |>  
  # Create grouped bar chart of skin tone shares by ethnoracial group
  ggplot(aes(etnia, shr, color = factor(colorr))) + 
  geom_col(
    aes(fill = factor(colorr)),
    position = position_dodge2(width = 0.9, preserve = "single"),
    color = "#e5e5e5", lwd = .01
  ) +
  scale_fill_manual(values = perla_palette(n = 11)) +
  scale_color_manual(values = perla_palette(n = 11)) +
  scale_y_continuous(
    breaks = seq(0, 0.35, 0.05),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Ethnoracial identity",
    y = "Share of skin tone",
    fill = "PERLA scale",
    color = "PERLA scale"
  ) +
  theme(
    axis.text.y = element_text(size = 13, angle = 0),
    axis.text.x = element_text(size = 14, angle = 0),
    axis.title = element_text(size = 15),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

# Save plot only if not knitting to RMarkdown
if (!knitr::is_html_output() && !knitr::is_latex_output()) {
  ggsave(
    filename = "plots/Fig1b.pdf",
    dpi = 300,
    width = 7.5,
    height = 5.75
  )
}
