# ---- Appendix A. Data --------------------------------------------------------------------
# -- Table A1: Summary statistics table ----

table(lapop_analysis$year)

sumstats_table <- 
  lapop_analysis|> 
  group_by(colorr)|> 
  summarise(
    n = n(),
    hhincomepc_p25 = quantile(hhincomepc, .25, na.rm = T),
    hhincomepc_median = median(hhincomepc, na.rm = T),
    hhincomepc_mean = mean(hhincomepc, na.rm = T),
    hhincomepc_p75 = quantile(hhincomepc, .75, na.rm = T),
    
    female = sum(sex == "Female", na.rm = T)/n,
    age = mean(age, na.rm = T),
    ed = mean(edc, na.rm = T),
    hh_total = mean(hh_total, na.rm = T),
    
    central_america = sum(region == "Central_America", na.rm = T)/n,
    caribbean = sum(region == "Caribbean", na.rm = T)/n,
    south_america = sum(region == "South America", na.rm = T)/n,
    
    metro_area = sum(tamano == "National Capital (Metropolitan Area)", na.rm = T)/n,
    city_big = sum(tamano == "Large City", na.rm = T)/n,
    city_medium = sum(tamano == "Medium City", na.rm = T)/n,
    city_small = sum(tamano == "Small City", na.rm = T)/n,
    rural_area = sum(tamano == "Rural Area", na.rm = T)/n,
    
    working = sum(occupa_status == "Working", na.rm = T)/sum(!is.na(occupa_status)),
    unemployed = sum(occupa_status == "Unemployed", na.rm = T)/sum(!is.na(occupa_status)),
    
    salary_self_empl = sum(salary_status == "Self-employed", na.rm = T)/sum(!is.na(salary_status)),
    salary_gov = sum(salary_status == "Salaried - Government", na.rm = T)/sum(!is.na(salary_status)),
    salary_private = sum(salary_status == "Salaried - Private sector", na.rm = T)/sum(!is.na(salary_status)),
    salary_owner = sum(salary_status == "Owner", na.rm = T)/sum(!is.na(salary_status)),
    
    moed_less_primary = sum(mothers_ed_isced == "Less than primary", na.rm = T)/n,
    moed_primary = sum(mothers_ed_isced == "Primary", na.rm = T)/n,
    moed_lower_secondary = sum(mothers_ed_isced == "Lower secondary", na.rm = T)/n,
    moed_upper_secondary = sum(mothers_ed_isced == "Upper secondary", na.rm = T)/n,
    moed_tiertiary = sum(mothers_ed_isced == "Tertiary", na.rm = T)/n,
    
    n = n(),
  ) |> 
  mutate_if(
    is.double,
    funs(round(., 2))
  ) |> 
  mutate(
    n = as.character(round(n)),
    hex = perla_palette(9)
  ) |> 
  select(-n, hex, everything(), n)

sumstats_table_mean <- 
  lapop_analysis|> 
  summarise(
    n = n(),
    hhincomepc_p25 = quantile(hhincomepc, .25, na.rm = T),
    hhincomepc_median = median(hhincomepc, na.rm = T),
    hhincomepc_mean = mean(hhincomepc, na.rm = T),
    hhincomepc_p75 = quantile(hhincomepc, .75, na.rm = T),
    
    female = sum(sex == "Female", na.rm = T)/n,
    age = mean(age, na.rm = T),
    ed = mean(edc, na.rm = T),
    hh_total = mean(hh_total, na.rm = T),
    
    central_america = sum(region == "Central_America", na.rm = T)/n,
    caribbean = sum(region == "Caribbean", na.rm = T)/n,
    south_america = sum(region == "South America", na.rm = T)/n,
    
    metro_area = sum(tamano == "National Capital (Metropolitan Area)", na.rm = T)/n,
    city_big = sum(tamano == "Large City", na.rm = T)/n,
    city_medium = sum(tamano == "Medium City", na.rm = T)/n,
    city_small = sum(tamano == "Small City", na.rm = T)/n,
    rural_area = sum(tamano == "Rural Area", na.rm = T)/n,
    
    working = sum(occupa_status == "Working", na.rm = T)/sum(!is.na(occupa_status)),
    unemployed = sum(occupa_status == "Unemployed", na.rm = T)/sum(!is.na(occupa_status)),
    
    salary_self_empl = sum(salary_status == "Self-employed", na.rm = T)/sum(!is.na(salary_status)),
    salary_gov = sum(salary_status == "Salaried - Government", na.rm = T)/sum(!is.na(salary_status)),
    salary_private = sum(salary_status == "Salaried - Private sector", na.rm = T)/sum(!is.na(salary_status)),
    salary_owner = sum(salary_status == "Owner", na.rm = T)/sum(!is.na(salary_status)),
    
    moed_less_primary = sum(mothers_ed_isced == "Less than primary", na.rm = T)/n,
    moed_primary = sum(mothers_ed_isced == "Primary", na.rm = T)/n,
    moed_lower_secondary = sum(mothers_ed_isced == "Lower secondary", na.rm = T)/n,
    moed_upper_secondary = sum(mothers_ed_isced == "Upper secondary", na.rm = T)/n,
    moed_tiertiary = sum(mothers_ed_isced == "Tertiary", na.rm = T)/n,
    
    n = n(),
  ) |> 
  mutate_if(
    is.double,
    funs(round(., 2))
  ) |> 
  mutate(
    n = as.character(round(n)),
  ) |> 
  select(-n, everything(), n)

sumstats_table <- 
  sumstats_table |> 
  bind_rows(sumstats_table_mean)

sumstats_table_t <- 
  sumstats_table |> 
  mutate(
    colorr = NULL
  ) |> 
  t() 



colnames(sumstats_table_t) <- c(as.character(seq(1,9)), "Sample mean")

rownames(sumstats_table_t) <- 
  c(
    "Percentile 25",
    "Median",
    "Mean",
    "Percentile 75",
    
    "Sex = Female (%)",
    "Age",
    "Years of schooling",
    "People per household",
    
    "Region = Central America (%)",
    "Region = Caribbean (%)",
    "Region = South America (%)",
    
    "Locality size = Metro area (%)",
    "Locality size = Big city (%)",
    "Locality size = Medium city (%)",
    "Locality size = Small city (%)",
    "Locality size = Rural area (%)",
    
    "Work status = Working (%)",
    "Work status = Unemployed (%)",
    
    "Salary status = Self employed (%)",
    "Salary status = Employed public sector (%)",
    "Salary status = Employed private sector (%)",
    "Salary status = Owner (%)",
    
    "Mother's education = Less than primary (%)", 
    "Mother's education = Primary (%)",
    "Mother's education = Lower secondary (%)",
    "Mother's education = Upper secondary (%)",
    "Mother's education = Tiertiary (%)",
    
    "Hexcode",
    "No. Observations"
  )

sumstats_table_t |> 
  kbl(
    format = "latex", table.envir = "threeparttable",  
    booktabs = TRUE,
    align = c("l", rep("c", 12))
  )

###
# -- Figure A1: Educational intergenerational persistence (sd-to-level) ----

ed <- 
  feols(
    scale(edc) ~ mothers_ed_isced + sex + age | cluster_enumerator_id ,
    lapop_analysis, weights = ~ weight1500
  )

mean_ed <- mean(fixef(ed)$`cluster_enumerator_id`, na.rm = T)

lct_ed <- tibble()

for (i in 1:length(names(ed$coefficients))) {
  
  # print(i)
  
  if (i < length(names(ed$coefficients))){
    
    var1 <- names(ed$coefficients)[i]
    var2 <- names(ed$coefficients)[i+1]
    
    df <- fixest::degrees_freedom(ed, "resid")
    diff <- unname(ed$coefficients[var2] - ed$coefficients[var1])
    den <- unname(sqrt((ed$se[var1])^2 + (ed$se[var2])^2 - 2*ed$cov.unscaled[var1, var2]))
    t <- abs(diff/den)
    p <- 2*pt(q=t, df=df, lower.tail=FALSE)
    
  }else{
    
    var2 <- "mothers_ed_iscedPrimary"
    
    diff <- (tidy(ed) |> 
               filter(term == var2))$estimate
    
    p <-  (tidy(ed) |> 
             filter(term == var2))$p.value
    
  }
  
  lct_ed <- 
    lct_ed |> 
    bind_rows(
      tibble(
        term = var2,
        diff = diff,
        lctpval = p
      )
    )
  
}

lct_ed <- 
  lct_ed |> 
  mutate(
    lctpval_corr =  q_val(lctpval)
  )  |> 
  filter(str_detect(term, "mothers_ed"))


ed_tibble <- 
  broom::tidy(ed) |> 
  filter(str_detect(term, "mothers_ed")) |> 
  select(term, estimate, std.error) |> 
  left_join(
    lct_ed
  ) |> 
  mutate(
    term = (str_remove_all(term, "mothers_ed_isced")),
    level = 1:nrow(lct_ed),
    
    uci95 = estimate + 1.96*std.error,
    lci95 = estimate - 1.96*std.error,
    
    uci90 = estimate + 1.64*std.error,
    lci90 = estimate - 1.64*std.error,
    
    lctpval_corr = formatC(lctpval_corr, format="f", digits=3),
    group1 = level - 1,
    group2 = level
  ) |> 
  bind_rows(
    tibble(
      term = "None",
      estimate = 0,
      level = 0
    )
  ) |> 
  arrange(level) |> 
  mutate(
    term = str_replace_all(term, " ", "\n")
  ) 

diffs <- round(ed_tibble$diff[2:nrow(ed_tibble)], 3)
pvals <- ed_tibble$lctpval_corr[2:nrow(ed_tibble)]
pvals <- str_c("(", pvals, ")")
adj_diff_labs <- str_c(diffs, "\n", pvals)

ed_tibble |> 
  ggplot(aes(x = level, y = estimate, color = factor(term))) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = lci95, ymax = uci95), lwd = .5, width = .15, color = "#2A788EFF") +
  geom_linerange(aes(ymin = lci90, ymax = uci90), lwd = 1, color = "#2A788EFF") +
  geom_point(
    size = 3.5,
    color = "#2A788EFF"
  ) +
  scale_x_continuous(
    breaks = seq(0,4),
    labels = ed_tibble$term
  ) +
  labs(x = "Mother's educational attaintment", y = "Estimate (z-score)") +
  theme(
    legend.position = "none",
    plot.margin = unit(c(.75,0.25,0.25,0.25), "cm")
  ) +
  ggnewscale::new_scale_color() +
  geom_signif(
    annotations = adj_diff_labs,
    y_position = .9, 
    xmin = seq(0.05, 3 + .05, by = .9), xmax = seq(0.95, 3 + .95, by = .9),
    tip_length = 0,
    step_increase = -0.025, textsize = 2
  ) +
  coord_cartesian(clip = "off")

ggsave(
  filename = "plots/FigA1.pdf",
  dpi = 300,
  width = 7.5,
  height = 5.75
)

# -- Educational IM by country and cohort ----

# Only run the loop if all three results files do NOT already exist
if (!file.exists("results/relative_mobility_data_points.csv") & 
    !file.exists("results/relative_mobility_data_poly.csv") & 
    !file.exists("results/relative_mobility_data_coefs.csv")){

  # Initialize empty tibbles to collect results across iterations
  data_points <- tibble()
  data_poly <- tibble()
  data_coefs <- tibble()
  
  # Loop through each country in the dataset
  for (c in levels(factor(lapop_analysis$countrycode))) {
    
    print(c)
    
    # Filter data for the current country
    lapop_analysis1 <- 
      lapop_analysis |> 
      filter(countrycode == c)
    
    # Loop through each cohort within that country
    for (coh in levels(factor(lapop_analysis1$cohort))) {
      
      # Filter data for the current cohort
      lapop_analysis2 <- 
        lapop_analysis1 |> 
        filter(cohort == coh)
      
      # Binscatter: Estimate conditional relationship between mother's and child's education
      social_mob <- 
        binsreg(
          y = lapop_analysis2$ed_pct, x = lapop_analysis2$mothers_ed_pct, 
          w = ~ factor(sex), data=lapop_analysis2,
          polyreg = 1, binsmethod="rot", nbinsrot = 10, weights = lapop_analysis2$weight1500
        )
      
      # Store binscatter point estimates
      data_points <- 
        data_points |> 
        bind_rows(
          social_mob$data.plot$`Group Full Sample`$data.dots |> 
            tibble() |> 
            mutate(
              group = c,
              cohort = coh
            )
        )
      
      # Store fitted polynomial line estimates
      data_poly <- 
        data_poly |> 
        bind_rows(
          social_mob$data.plot$`Group Full Sample`$data.poly |> 
            tibble() |> 
            mutate(
              group = c,
              cohort = coh
            )
        )
      
      # Estimate levels-to-levels mobility via linear regression
      edu_country <- 
        feols(
          edc ~ mothers_edc + sex + age,
          lapop_analysis2,
          cluster = ~ cluster_id, weights = ~ weight1500
        )
      
      mean_edu <- edu_country$coefficients[1]
      
      # Estimate rank-to-rank mobility
      edu_country_rank <- 
        feols(
          ed_pct ~ mothers_ed_pct + sex + age,
          lapop_analysis2,
          cluster = ~ cluster_id, weights = ~ weight1500
        )
      
      # Store coefficients and summary statistics for this group/cohort
      data_coefs <- 
        data_coefs |> 
        bind_rows(
          tibble(
            group = c,
            cohort = coh,
            cohort_size = sum(lapop_analysis2$weight1500, na.rm = T),
            relative_rank = edu_country_rank$coefficients[2],
            relative_rank_se = edu_country_rank$se[2],
            relative_country = edu_country$coefficients[2],
            relative_country_se = edu_country$se[2],
            mean_country = mean_edu,
            ed = mean(lapop_analysis2$edc, na.rm = T),
            ed_mothed = mean(lapop_analysis2$mothers_edc, na.rm = T),
            abs_ed_im = sum(lapop_analysis2$abs_ed_im, na.rm = T)/sum(lapop_analysis2$wt, na.rm = T)
          ) |> 
            mutate(
              abs_ed_im = if_else(abs_ed_im >= 1, 1, abs_ed_im)
            )
        )
      
    }
    
  }
  
  # Save the resulting datasets to CSV files
  data_points |> 
    write_csv("results/relative_mobility_data_points.csv")
  
  data_poly |> 
    write_csv("results/relative_mobility_data_poly.csv")
  
  data_coefs |> 
    write_csv("results/relative_mobility_data_coefs.csv")
  
}


# - Plot by country ----

# Average the binned point estimates across cohorts for each country
data_points <- 
  read_csv("results/relative_mobility_data_points.csv") |> 
  group_by(group, bin) |> 
  summarise(
    fit = mean(fit, na.rm = T)
  ) |> 
  ungroup()

# Average the fitted line (polynomial) values across cohorts for each country
data_poly <- 
  read_csv("results/relative_mobility_data_poly.csv") |> 
  group_by(group, bin) |> 
  summarise(
    fit = mean(fit, na.rm = T)
  ) |> 
  ungroup()

# Compute weighted average of relative mobility coefficients across cohorts
data_coefs <- 
  read_csv("results/relative_mobility_data_coefs.csv") |> 
  group_by(group) |> 
  summarise(
    relative_country = weighted.mean(relative_rank, cohort_size, na.rm = T),
    relative_country_se = weighted.mean(relative_rank_se, cohort_size, na.rm = T)
  ) |> 
  ungroup()

# --- Create the plot: within-country mobility patterns ---
ggplot() +
  # Add average data points (from binned scatter)
  geom_point(
    data = data_points,
    aes(10*bin, fit), color = "#2A788EFF",
    size = 1
  ) +
  # Add smoothed fit lines (from polynomial regression)
  geom_line(
    data = data_poly,
    aes(10*bin, fit), color = "#2A788EFF"
  ) +
  # Apply clean minimal theme
  theme_clean +
  # Axis labels
  labs(
    x = "Mother's educational percentile rank",
    y = "Individual's educational percentile rank",
  ) +
  # Constrain y-axis to valid percentile range
  coord_cartesian(
    ylim = c(0,100)
  ) +
  # Display relative mobility coefficient for each country as text in top-left
  geom_text(
    data = data_coefs,
    aes(
      x = -Inf, y = Inf,
      hjust = -.2, vjust = 1,
      label = str_c(
        "Slope: ",
        round(relative_country, 3),
        " (", round(relative_country_se, 3), ")"
      )
    ),
    size = 2, color = "#2A788EFF"
  ) +
  # Create a separate panel for each country
  facet_wrap(~group)


#
# - Figure A3: Plot by country and cohort ----
data_coefs <- 
  read_csv("results/relative_mobility_data_coefs.csv")

educational_persistence <- 
  data_coefs |> 
  rename(
    countrycode = group
  ) |> 
  left_join(
    lapop |> 
      group_by(countrycode, pais, region) |> 
      summarise() |> 
      ungroup()
  ) |> 
  mutate(
    relative_rank = if_else(relative_rank >= 1, 1, relative_rank),
    relative_country = if_else(relative_country >= 1, 1, relative_country),
    cohort = fct_relevel(
      cohort, "Before 1950s", "1950s", "1960s", "1970s", "1980s", "1990s"
    ),
    cohort_num = as.numeric(as.factor(cohort)),
    region = str_replace_all(region, "_", " ")
  ) 


educational_persistence |> 
  filter(region == "Caribbean") |> 
  ggplot(aes(cohort_num, relative_country, color = pais)) +
  geom_line() +
  geom_point(size = 1.25) +
  scale_x_continuous(
    breaks = 1:6,
    labels = levels(educational_persistence$cohort)
  ) +
  scale_y_continuous(
    breaks = seq(0,1,.2),
    limits = c(0,1)
  ) +
  labs(x = "Year of Birth", y = "Educational Persistence Estimate", color = "") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 15),
  ) +
  coord_cartesian(clip = "off")

ggsave(
  filename = "plots/FigA3a.pdf",
  dpi = 300,
  width = 10,
  height = 5.75
)

educational_persistence |> 
  filter(region == "Central America") |> 
  ggplot(aes(cohort_num, relative_country, color = pais)) +
  geom_line() +
  geom_point(size = 1.25) +
  scale_x_continuous(
    breaks = 1:6,
    labels = levels(educational_persistence$cohort)
  ) +
  scale_y_continuous(
    breaks = seq(0,1,.2),
    limits = c(0,1)
  ) +
  labs(x = "Year of Birth", y = "Educational Persistence Estimate", color = "") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 15),
  ) +
  coord_cartesian(clip = "off")

ggsave(
  filename = "plots/FigA3b.pdf",
  dpi = 300,
  width = 10,
  height = 5.75
)

educational_persistence |> 
  filter(region == "South America") |> 
  ggplot(aes(cohort_num, relative_country, color = pais)) +
  geom_line() +
  geom_point(size = 1.25) +
  scale_x_continuous(
    breaks = 1:6,
    labels = levels(educational_persistence$cohort)
  ) +
  scale_y_continuous(
    breaks = seq(0,1,.2),
    limits = c(0,1)
  ) +
  labs(x = "Year of Birth", y = "Educational Persistence Estimate", color = "") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 15),
  ) +
  coord_cartesian(clip = "off")

ggsave(
  filename = "plots/FigA3c.pdf",
  dpi = 300,
  width = 10,
  height = 5.75
)

#
# -- Comparison to GDIM ----
data_coefs <- 
  read_csv("results/relative_mobility_data_coefs.csv") |> 
  mutate(
    cohort = case_when(
      cohort %in% c("1980s", "1990s") ~ "1980s and 1990s",
      T ~ cohort
    )
  )

gdim <- 
  read_csv("data/source/GDIM/gdim.csv") |> 
  filter(code %in% data_coefs$group) |> 
  mutate(
    cohort = case_when(
      cohort == 1940 ~ "Before 1950s",
      cohort == 1950 ~ "1950s",
      cohort == 1960 ~ "1960s",
      cohort == 1970 ~ "1970s",
      cohort >= 1980 ~ "1980s and 1990s",
      
    )
  )

# table(gdim$cohort)

gdim_grouped <- 
  gdim |> 
  group_by(code, cohort) |> 
  summarise(
    edp = weighted.mean(MEANp),
    edc = weighted.mean(MEANc),
    beta = weighted.mean(BETA),
    mix = weighted.mean(MIX),
    cat = weighted.mean(CAT),
  ) |> 
  ungroup() |> 
  rename(group = code)

foo <- 
  data_coefs |> 
  left_join(
    gdim_grouped 
  )

#

# - Figure A2a: Child years of schooling ----

binsreg_plot <- 
binsreg(
  y = foo$ed,
  x = foo$edc,
  polyreg = 1,
  bycolors = "#2A788EFF",
  nbins = 20,
)

ggplot() +
  geom_line(
    data = binsreg_plot$data.plot$`Group Full Sample`$data.poly,
    aes(x, fit), color = "#2A788EFF", lwd = .8
  ) +
  geom_point(
    data = binsreg_plot$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit), color = "#2A788EFF", size = 3.5
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = .5) +
  labs(
    y = "GDIM (van der Weide et al. 2023)\nRespondent's years of schooling",
    x = "LAPOP\nRespondent's years of schooling"
  ) +
  scale_x_continuous(breaks = seq(2, 12, by=2)) +
  theme_clean

ggsave(
  filename = "plots/FigA2a.pdf",
  dpi = 300,
  width = 7.5,
  height = 5.75
)

# - Figure A2b: Parental years of schooling ----

binsreg_plot <- 
  binsreg(
    y = foo$edp,
    x = foo$ed_mothed,
    polyreg = 1,
    bycolors = "#2A788EFF",
    nbins = 20
  )

ggplot() +
  geom_line(
    data = binsreg_plot$data.plot$`Group Full Sample`$data.poly,
    aes(x, fit), color = "#2A788EFF", lwd = 0.8
  ) +
  geom_point(
    data = binsreg_plot$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit), color = "#2A788EFF", size = 3.5
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
  labs(
    y = "GDIM (van der Weide et al. 2023)\nParental years of schooling",
    x = "LAPOP\nMother's years of schooling"
  ) +
  scale_x_continuous(breaks = seq(2, 12, by = 2)) +
  theme_clean

ggsave(
  filename = "plots/FigA2b.pdf",
  dpi = 300,
  width = 7.5,
  height = 5.75
)

#
# - Figure A2c: Relative mobility ----

binsreg_plot <- 
  binsreg(
    y = foo$beta,
    x = foo$relative_country,
    polyreg = 1,
    bycolors = "#2A788EFF",
    nbins = 20
  )

ggplot() +
  geom_line(
    data = binsreg_plot$data.plot$`Group Full Sample`$data.poly,
    aes(x, fit), color = "#2A788EFF", lwd = 0.8
  ) +
  geom_point(
    data = binsreg_plot$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit), color = "#2A788EFF", size = 3.5
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
  labs(
    y = "GDIM (van der Weide et al. 2023)\nIntergenerational relative mobility (beta)",
    x = "LAPOP\nIntergenerational relative mobility (beta)"
  ) +
  theme_clean

ggsave(
  filename = "plots/FigA2c.pdf",
  dpi = 300,
  width = 7.5,
  height = 5.75
)

#
# - Figure A2d: Absolute mobility ----

binsreg_plot <- 
  binsreg(
    y = foo$mix,
    x = foo$abs_ed_im,
    polyreg = 1,
    bycolors = "#2A788EFF",
    nbins = 15
  )

ggplot() +
  geom_line(
    data = binsreg_plot$data.plot$`Group Full Sample`$data.poly,
    aes(x, fit), color = "#2A788EFF", lwd = 0.8
  ) +
  geom_point(
    data = binsreg_plot$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit), color = "#2A788EFF", size = 3.5
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
  labs(
    y = "GDIM (van der Weide et al. 2023)\nAbsolute mobility",
    x = "LAPOP\nAbsolute mobility"
  ) +
  theme_clean

ggsave(
  filename = "plots/FigA2d.pdf",
  dpi = 300,
  width = 7.5,
  height = 5.75
)

#
# -- Figure A4: Household asset index and income ----

p1 <- 
  binsreg(
    lapop_analysis$hhincomepc, scale(lapop_analysis$ind_riq), 
    w = ~ factor(countrycode) + factor(year), lapop_analysis,
    weights = lapop_analysis$weight1500,
    nbins = 20
  )

m1 <- 
  feols(
    hhincomepc_mx0 ~ ind_riq | countrycode + year,
    lapop_analysis,
    cluster = ~ cluster_enumerator_id,
    weights = ~ weight1500
  )

ggplot() +
  geom_smooth(
    data = p1$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit), method = 'lm', formula = 'y ~ x', 
    color = "#2A788EFF", se = F, lwd = .75
  ) +
  geom_point(
    data = p1$data.plot$`Group Full Sample`$data.dots,
    aes(x, fit), color = "#2A788EFF", size = 3
  ) +
  labs(
    x = "Household asset index (z-score)",
    y = "Household income per capita (PPP)"
  ) +
  theme_clean +
  annotate(
    "label",
    label = str_c("Log-points\nestimate (se):\n", round(m1$coefficients[1], 3), " (", round(m1$se[1], 3), ")"),
    x = 1.5, y = 350, size = 2
  )

ggsave(
  filename = "plots/FigA4.pdf",
  dpi = 300,
  width = 7.5,
  height = 5.75
)

#
# -- No. Obs. by Ethnoracial category ----
lapop_analysis |>  
  group_by(etnia) |> 
  summarize(
    n = sum(weight1500, na.rm = T)
  ) |> 
  ggplot(aes(etnia, n, color = etnia)) +
  geom_col(aes(fill = etnia), width = .66) +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x = "Ethnoracial identity", y = "Number of observations") +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        legend.position = "none") 

# -- No. Obs. by Skin tone ----
lapop_analysis |>  
  group_by(colorr) |> 
  summarize(
    n = sum(weight1500, na.rm = T)
  ) |> 
  ggplot(aes(colorr, n, color = factor(colorr))) +
  geom_col(aes(fill = factor(colorr)), width = .66) +
  scale_fill_manual(values = perla_palette(n=11)) +
  scale_color_manual(values = perla_palette(n=11)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x = "Skin tone (PERLA Scale)", y = "Number of observations") +
  scale_x_continuous(
    breaks = 1:9, 
    labels = c(as.character(seq(1,8)), str_c(9, " or more"))  # Customize x-axis labels
  ) +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        legend.position = "none") 

# -- Figure A5: Ethnoracial distribution by country ----
lapop_analysis |> 
  mutate(region = str_replace_all(region, "_", " ")) |>  
  group_by(countrycode, etnia, region) |> 
  summarize(n_etnia = n()) |> 
  left_join(lapop |> 
              filter(!is.na(etnia)) |>  
              group_by(countrycode) |> 
              summarize(n_pais = n())) |> 
  mutate(sh_etnia = n_etnia/n_pais) |>  
  ggplot(aes(countrycode, sh_etnia, color = etnia)) +
  geom_col(aes(fill = etnia), position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  scale_y_continuous(breaks = seq(0,1,.1), labels = scales::percent_format()) +
  labs(x = "", y = "Percentage", fill = "Ethnoracial identity", color = "Ethnoracial identity") +
  theme(
    axis.text.x = element_text(angle = 0),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 13)
  ) + 
  facet_wrap(factor(region) ~., scales = "free",
             nrow = 3)

ggsave(
  filename = "plots/FigA5.pdf",
  dpi = 300,
  width = 8.1,
  height = 10.4
)


# -- Figure A6: Skin tone distribution by country ----
lapop_analysis |>  
  group_by(countrycode, colorr, region) |>  
  summarize(n_color = n()) |>  
  left_join(lapop |>  
              filter(!is.na(colorr)) |>  
              group_by(countrycode) |>  
              summarize(n_pais = n())
  ) |>  
  mutate(sh_color = n_color/n_pais,
         colorr = as.factor(colorr)) |>  
  ggplot(aes(countrycode, sh_color, color = colorr)) + 
  geom_col(aes(fill = colorr), position = position_dodge2(width = 0.9, preserve = "single"),
           color = "#e5e5e5", lwd = .05) +
  scale_fill_manual(values = perla_palette(n=11)) +
  scale_color_manual(values = perla_palette(n=11)) +
  scale_y_continuous(breaks = seq(0,.4,.05), labels = scales::percent_format()) +
  labs(x = "", y = "Percentage", fill = "PERLA\nscale", color = "PERLA\nscale") +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  #coord_cartesian(ylim = c(0,.3)) +
  facet_wrap(factor(region) ~., scales = "free",
             nrow = 3)

ggsave(
  filename = "plots/FigA6.pdf",
  dpi = 300,
  width = 8.1,
  height = 10.4
)

# -- Figure A7: Predict ethnoracial identities ----
# Recode variables and ensure factors are correctly set
lapop_analysis <- lapop_analysis  |> 
  mutate(
    # Recode 'lengua_most_common' to a factor with meaningful labels
    lengua_most_common_f = case_when(
      lengua_most_common == 1 ~ "Most common language",
      TRUE ~ "Other language or dialect"
    ),
    # Recode 'ur' to have a label 'Urban' for 'Urbano'
    ur = case_when(
      ur == "Urbano" ~ "Urban",
      TRUE ~ ur
    ),
    # Convert 'year' to a factor variable
    year_f = as.factor(year),
  )

# Optional: Check the distribution of the ethnoracial categories
table(lapop_analysis$etnia)

# Fit the null model (intercept-only) for baseline comparison 
model_null <- 
  nnet::multinom(
    etnia ~ 1,  # No predictors, just the intercept
    data = lapop_analysis,
    weights = lapop_analysis$weight1500  # Apply sampling weights
  )

# Calculate log-likelihood of the null model
log_lik_null <- logLik(model_null)

# Fit the model with skin tone as the only predictor 
model_skin_tone <- 
  nnet::multinom(
    etnia ~ colorr,  # Skin tone as the only predictor
    data = lapop_analysis,
    weights = lapop_analysis$weight1500  # Apply sampling weights
  )

# Calculate log-likelihood for the skin tone model
log_lik_simple <- logLik(model_skin_tone)

# Calculate McFadden's R^2 between the null model and the skin tone model
mcfadden_r2_null_simple <- 1 - (as.numeric(log_lik_simple) / as.numeric(log_lik_null))
# McFadden's R^2 between the null model and the skin tone model: 0.1814195

# Fit the full model with all predictors including skin tone and language
model_skin_tone_full <- 
  nnet::multinom(
    etnia ~ colorr + lengua_most_common_f + 
      tamano + age + sex + countrycode + year_f + mothers_ed_isced,  # Full model with additional predictors
    data = lapop_analysis,
    weights = lapop_analysis$weight1500  # Apply sampling weights
  )

# Calculate log-likelihood for the full model
log_lik_full <- logLik(model_skin_tone_full)

# Calculate McFadden's R^2 between the null model and the full model-
mcfadden_r2_null_full <- 1 - (as.numeric(log_lik_full) / as.numeric(log_lik_null))
# McFadden's R^2 between the null model and the full model: 0.553088

# Plot predicted probabilities by skin tone and language group
# Use ggeffects to calculate predicted probabilities and plot them
prob_plot_lang <- 
  ggeffect(
    model_skin_tone_full, terms = c("colorr", "lengua_most_common_f"),  # Interaction terms for prediction
  ) |> 
  plot(show_ci = TRUE)  # Plot with confidence intervals

# Customize the plot appearance
prob_plot_lang +
  theme_clean +  # Apply a clean theme
  labs(x = "PERLA scale", y = "Probability", title = "", group = "", color = "") +  # Add axis labels and remove legend titles
  scale_x_continuous(
    breaks = 1:9, 
    labels = c(as.character(seq(1,8)), str_c(9, "+"))  # Customize x-axis labels
  ) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = 0.666, end = .333) +  # Apply viridis color scale for fill
  viridis::scale_color_viridis(discrete = TRUE, begin = 0.666, end = .333) +  # Apply viridis color scale for lines
  theme(
    axis.text = element_text(size = 9)  # Adjust axis text size
  )

# Save the plot to a file
ggsave(
  filename = "plots/FigA7.pdf",
  dpi = 300,
  width = 7.5,
  height = 5.75
)

