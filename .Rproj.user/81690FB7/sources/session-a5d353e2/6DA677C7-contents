# ------------------------------------------------------------------------------
# Figure 3: Variance Decomposition by Ethnoracial Identity and Skin Tone
# Description:
#   - Compares the within-adjusted R^2 for household income and years of schooling
#     using skin tone, ethnoracial categories, and their interaction.
#   - Panel A includes across-group models.
#   - Panel B includes skin tone regressions within each ethnoracial category.
# Output: 'plots/Fig3a.pdf' and 'plots/Fig3b.pdf' (if run outside RMarkdown)
# ------------------------------------------------------------------------------
# --- Variance decomposition: Household income ----

# Model 1: Skin tone only
income_skin_tone <- feols(
  hhincomepc_mx0 ~ i(colorr) | cluster_enumerator_id + hh_total,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_enumerator_id
)

# Model 2: Ethnoracial category only
income_race <- feols(
  hhincomepc_mx0 ~ etnia | cluster_enumerator_id + hh_total,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_enumerator_id
)

# Model 3: Skin tone × Ethnoracial category (split by group)
income_skin_tone_by_race <- feols(
  hhincomepc_mx0 ~ colorr * etnia | cluster_enumerator_id + hh_total,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_enumerator_id,
  fsplit = ~ etnia
)

# Collect within adjusted R² for within-group regressions
income_war2 <- sapply(income_skin_tone_by_race, r2, type = "war2")

# Store income decomposition results
var_decomp <- tibble(
  outcome = "Household income per capita",
  type = c(
    "Skin tone", "Ethnoracial category", "Skin tone x Ethnoracial category",
    "Within White", "Within Mestizo",
    "Within Indigenous", "Within Black", "Within Other"
  ),
  war2 = c(
    r2(income_skin_tone, "war2"),
    r2(income_race, "war2"),
    income_war2
  )
)

# --- Variance decomposition: Years of schooling ----

ed_skin_tone <- feols(
  edc ~ i(colorr) | cluster_enumerator_id,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_enumerator_id
)

ed_race <- feols(
  edc ~ etnia | cluster_enumerator_id,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_enumerator_id
)

ed_skin_tone_by_race <- feols(
  edc ~ colorr * etnia | cluster_enumerator_id,
  weights = ~ weight1500,
  data = lapop_analysis,
  cluster = ~ cluster_enumerator_id,
  fsplit = ~ etnia
)

ed_war2 <- sapply(ed_skin_tone_by_race, r2, type = "war2")

# Append education results to var_decomp
var_decomp <- bind_rows(
  var_decomp,
  tibble(
    outcome = "Years of schooling",
    type = c(
      "Skin tone", "Ethnoracial category", "Skin tone x Ethnoracial category",
      "Within White", "Within Mestizo",
      "Within Indigenous", "Within Black", "Within Other"
    ),
    war2 = c(
      r2(ed_skin_tone, "war2"),
      r2(ed_race, "war2"),
      ed_war2
    )
  )
)

# --- Prepare for plotting ----

foo <- var_decomp |>
  mutate(
    outcome = recode(outcome, "Household income per capita" = "HH income pc"),
    type = fct_relevel(
      type,
      "Within Other", "Within Black", "Within Indigenous",
      "Within Mestizo", "Within White",
      "Skin tone x Ethnoracial category", "Ethnoracial category", "Skin tone"
    ),
    outcome = fct_relevel(outcome, "Years of schooling", "HH income pc"),
    category = case_when(
      type %in% c("Skin tone", "Ethnoracial category", "Skin tone x Ethnoracial category") ~ "Across Ethnoracial categories",
      TRUE ~ "Within Ethnoracial categories"
    )
  )

# --- Panel A: Between-group R² plot ----

fig3a <- 
  foo |> 
  filter(category == "Across Ethnoracial categories") |> 
  ggplot(aes(war2, outcome, color = type, fill = type)) +
  geom_col(position = position_dodge(width = 1), width = .8) +
  # geom_point(position = position_dodge(width = 0.5), size = 6) +
  scale_color_viridis(
    option = "magma", discrete = TRUE, direction = 1, begin = 0.25, end = 0.75,
    guide = guide_legend(nrow = 3)
  ) +
  scale_fill_viridis(
    option = "magma", discrete = TRUE, direction = 1, begin = 0.25, end = 0.75,
    guide = guide_legend(nrow = 3)
  ) +
  scale_shape(guide = guide_legend(nrow = 2)) +
  scale_x_continuous(breaks = scales::pretty_breaks(8), limits = c(0, 0.025)) +
  labs(
    x = expression(Within ~ Adjusted ~ R^2),
    y = "",
    color = "",
    fill = ""
  ) +
  coord_cartesian(clip = "off", xlim = c(0.00, 0.026)) +
  facet_wrap(~outcome, scales = "free_y", nrow = 2) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 16),
    strip.text.x = element_blank()
  ) 

# Save Panel A
if (!knitr::is_html_output() && !knitr::is_latex_output()) {
  ggsave("plots/Fig3a.pdf", fig3a, dpi = 300, width = 9, height = 6)
}

# --- Panel B: Within-group R² plot ----

fig3b <- 
  foo |> 
  filter(category == "Within Ethnoracial categories") |> 
  ggplot(aes(war2, outcome, color = type, fill = type)) +
  geom_col(position = position_dodge(width = 1), width = .8) +
  # geom_point(position = position_dodge(width = 0.5), size = 6) +
  scale_color_viridis(
    discrete = TRUE, 
    guide = guide_legend(nrow = 3)
  ) +
  scale_fill_viridis(
    discrete = TRUE,
    guide = guide_legend(nrow = 3)
  ) +
  scale_shape(guide = guide_legend(nrow = 3)) +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  labs(
    x = expression(Within ~ Adjusted ~ R^2),
    y = "",
    color = "Skin Tone ",
    fill = "Skin Tone "
  ) +
  coord_cartesian(clip = "off", xlim = c(-0.001, 0.041)) +
  facet_wrap(~outcome, scales = "free_y", nrow = 2) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_line(color = "grey", linetype = "dotted"),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 16),
    strip.text.x = element_blank()
  ) 

# Save Panel B
if (!knitr::is_html_output() && !knitr::is_latex_output()) {
  ggsave("plots/Fig3b.pdf", fig3b, dpi = 300, width = 9, height = 6)
}
