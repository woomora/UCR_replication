# ------------------------------------------------------------------------------
# Figure 1a: Ethnoracial Identity Shares in the Population
# Description: Computes the weighted share of each self-declared ethnoracial 
# identity using LAPOP survey weights.
# Output: Saved as 'plots/Fig1a.pdf'
# ------------------------------------------------------------------------------

# Calculate weighted population shares by ethnoracial identity
lapop_analysis |>  
  group_by(etnia) |> 
  summarize(
    n_etnia = sum(weight1500, na.rm = TRUE)  # weighted count per group
  ) |> 
  mutate(
    n_total = (lapop_analysis |> summarize(n = sum(weight1500, na.rm = TRUE)))$n,  # total weighted count
    sh_etnia = n_etnia / n_total  # share of each ethnoracial identity
  ) |>  
  # Create bar plot of ethnoracial identity shares
  ggplot(aes(etnia, sh_etnia, color = etnia)) +
  geom_col(aes(fill = etnia), width = 0.66) +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  scale_y_continuous(
    breaks = seq(0, 0.9, 0.05),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Ethnoracial identity",
    y = "Share in population"
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
    filename = "plots/Fig1a.pdf",
    dpi = 300,
    width = 7.5,
    height = 5.75
  )
}
