# ---- Appendix D. Machine-rated Skin Tone Measures in Mexico ------------------
# -- Figure D.1: PRODER Skin tone measures ----
# Read (clean) PRODER data
# Download data from: https://discriminacion.colmex.mx/documentacion-y-base-de-datos-de-la-encuesta-proder/

proder <- read_rds("data/derived/proder.rds")

# --- Panel (a) Enumerator vs. Respondent perceptions ----
binsreg1 <- binsreg(
  x = proder$color_int,
  y = proder$color_resp,
  polyreg = 1,
  weights = proder$facinp
)

est1 <- feols(
  color_int ~ color_resp,
  data = proder, weights = ~facinp,
  cluster = ~ cp_mun^id_encuestador
)

# Extract coefficients and R-squared from the model
coef_est1 <- round(coef(est1), 3)
se_est1 <- round(se(est1), 3)
r2_est1 <- round(glance(est1)$r.squared, 3)
rmse1 <- round(as.double(fitstat(est1, "rmse")), 3)

ggplot() +
  geom_line(
    data = binsreg1$data.plot$`Group Full Sample`$data.poly,
    aes(x, fit)
  ) +
  geom_point(
    data = binsreg1$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit, color = factor(x)),
    size = 4
  ) +
  geom_point(
    data = binsreg1$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit),
    size = 4, shape = 21, stroke = 0.75, color = "#7f7f7f", fill = NA
  ) +
  scale_color_manual(values = proder_palette(11), guide = "none") +
  scale_x_continuous(breaks = seq(1:11)) +
  scale_y_continuous(breaks = seq(1:11)) +
  labs(
    x = "Enumerator's perception (PRODER scale)",
    y = "Respondent's perception (PRODER scale)"
  ) +
  annotate(
    "label", x = Inf, y = 2.75, hjust = 1, vjust = 1,
    label = paste0(
      coef_est1[1], " (", se_est1[1], ")\n",
      "R2: ", r2_est1, "\n",
      "RMSE: ", rmse1
    ),
    size = 3, color = "black"
  ) 

ggsave(
  filename = "plots/FigD1a.pdf",
  dpi = 300, width = 7.5, height = 5
)

#
# --- Panel (b) Colorimeter vs. Respondent’s perception ----
binsreg2 <- binsreg(
  y = proder$color_resp,
  x = proder$ita_invsd,
  polyreg = 1,
  weights = proder$facinp,
  nbins = 11
)

est2 <- feols(
  color_resp ~ ita_invsd,
  data = proder, weights = ~facinp,
  cluster = ~ cp_mun^id_encuestador
)

# Extract coefficients and R-squared from the model
coef_est2 <- round(coef(est2), 3)
se_est2 <- round(se(est2), 3)
r2_est2 <- round(glance(est2)$r.squared, 3)
rmse2 <- round(as.double(fitstat(est2, "rmse")), 3)

# Colorimeter vs Respondent
ggplot() +
  geom_line(
    data = binsreg2$data.plot$`Group Full Sample`$data.poly,
    aes(x, fit)
  ) +
  geom_point(
    data = binsreg2$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit, color = factor(x)),
    size = 4
  ) +
  geom_point(
    data = binsreg2$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit),
    size = 4, shape = 21, stroke = 0.75, color = "#7f7f7f", fill = NA
  ) +
  scale_color_manual(values = proder_palette(11), guide = "none") +
  scale_y_continuous(breaks = seq(1:11)) +
  labs(
    x = "Colorimeter inverse ITA (z-score)",
    y = "Respondent's perception (PRODER scale)"
  ) +
  annotate(
    "label", x = Inf, y = 3.75, hjust = 1, vjust = 1,
    label = paste0(
      coef_est2[1], " (", se_est2[1], ")\n",
      "R2: ", r2_est2, "\n",
      "RMSE: ", rmse2
    ),
    size = 3, color = "black"
  ) 

ggsave(
  filename = "plots/FigD1b.pdf",
  dpi = 300, width = 7.5, height = 5
)

#
# --- Panel (c) Colorimeter vs. Enumerator’s perception ----
binsreg3 <- binsreg(
  y = proder$color_int,
  x = proder$ita_invsd,
  polyreg = 1,
  weights = proder$facinp,
  nbins = 11
)

est3 <- feols(
  color_int ~ ita_invsd,
  data = proder, weights = ~facinp,
  cluster = ~ cp_mun^id_encuestador
)

# Extract coefficients and R-squared from the model
coef_est3 <- round(coef(est3), 3)
se_est3 <- round(se(est3), 3)
r2_est3 <- round(glance(est3)$r.squared, 3)
rmse3 <- round(as.double(fitstat(est3, "rmse")), 3)

# Colorimeter vs Enumerator
ggplot() +
  geom_line(
    data = binsreg3$data.plot$`Group Full Sample`$data.poly,
    aes(x, fit)
  ) +
  geom_point(
    data = binsreg3$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit, color = factor(x)),
    size = 4
  ) +
  geom_point(
    data = binsreg3$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit),
    size = 4, shape = 21, stroke = 0.75, color = "#7f7f7f", fill = NA
  ) +
  scale_color_manual(values = proder_palette(11), guide = "none") +
  scale_y_continuous(breaks = seq(1:11)) +
  labs(
    x = "Colorimeter inverse ITA (z-score)",
    y = "Enumerator's perception (PRODER scale)"
  ) +
  annotate(
    "label", x = Inf, y = 4.25, hjust = 1, vjust = 1,
    label = paste0(
      coef_est3[1], " (", se_est3[1], ")\n",
      "R2: ", r2_est3, "\n",
      "RMSE: ", rmse3
    ),
    size = 3, color = "black"
  ) 

ggsave(
  filename = "plots/FigD1c.pdf",
  dpi = 300, width = 7.5, height = 5
)

#
