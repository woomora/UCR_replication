# ------------------------------------------------------------------------------
# UCR Functions ----
# ------------------------------------------------------------------------------
# -- PERlA palette ----
perla_rgb <- col2rgb(c("#fcf2f3", "#f3dfda", "#e7bfb5", "#e4c8a3",
                       "#bea07e", "#9c7b52", "#83654d", "#6f4f3a",
                       "#513b2e", "#422811", "#373028"))

perla_palette <- function (n, name = c("perla")) {
  perla = rgb(perla_rgb[1,], perla_rgb[2,], perla_rgb[3,], maxColorValue = 252)
  name = match.arg(name)
  orig = eval(parse(text = name))
  rgb = t(col2rgb(orig))
  temp = matrix(NA, ncol = 3, nrow = n)
  x = seq(0, 1, , length(orig))
  xg = seq(0, 1, , n)
  for (k in 1:3) {
    hold = spline(x, rgb[, k], n = n)$y
    hold[hold < 0] = 0
    hold[hold > 255] = 255
    temp[, k] = round(hold)
  }
  palette = rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
  palette
}

perla_palette(11)

# -- PRODER palette ----
# Reverse the order of the colors for lighter skin first, darker after
proder_rgb <- col2rgb(rev(c("#654D3E", "#775741", "#876249", "#946C51", "#A0765A", 
                            "#A87F64", "#B1886C", "#B69279", "#BE9D86", "#C5A691", "#C8AC99"))) # Reversed palette

proder_palette <- function (n, name = "proder") {
  proder = rgb(proder_rgb[1,], proder_rgb[2,], proder_rgb[3,], maxColorValue = 252)
  orig = eval(parse(text = name))
  rgb = t(col2rgb(orig))
  temp = matrix(NA, ncol = 3, nrow = n)
  x = seq(0, 1, , length(orig))
  xg = seq(0, 1, , n)
  for (k in 1:3) {
    hold = spline(x, rgb[, k], n = n)$y
    hold[hold < 0] = 0
    hold[hold > 255] = 255
    temp[, k] = round(hold)
  }
  palette = rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
  palette
}

proder_palette(10)

# -- Oster beta adjusted ----
beta_adj <-
  function(delta = 1, coef_long = coef_long, coef_short = coef_short, rmax = rmax, cor2_long = cor2_long, cor2_short = cor2_short){
    
    beta_adj = coef_long - delta*((coef_short - coef_long)*((rmax - cor2_long)/(cor2_long - cor2_short)))
    
    return(beta_adj)
    
  }

# -- Linear combination test ----
linear_comb_test <- 
  function(x, var1, var2){
    
    require(fixest)
    
    df <- fixest::degrees_freedom(x, "resid")
    num <- unname(x$coefficients[var2] - x$coefficients[var1])
    den <- unname(sqrt((x$se[var1])^2 + (x$se[var2])^2 - 2*x$cov.scaled[var1, var2]))
    t <- abs(num/den)
    p <- 2*pt(q=t, df=df, lower.tail=FALSE)
    
    foo <- 
      tibble(
        diff = num,
        pval = p
      )
    
    return(foo)
  }
# -- Calibrate extensive margin (Chen and Roth, 2023) ----
calibrate_ext_margin <- function(y = y, x = 0){
  
  y_pos_min <- min(y[y!=0], na.rm = T)
  
  y_transf <- 
    tibble(
      y = y
    ) |> 
    mutate(
      y_transf = if_else(y == 0, -x, log(y/y_pos_min))
    )
  
  return(y_transf$y_transf)
  
}

#
# -- Skin tone gaps estimates and marginal effects ----
est_meff_tibble <- 
  function(feols_sat_est = feols_sat_est){
    
    require(tidyverse)
    require(fixest)
    require(marginaleffects)
    require(stringr)
    require(qval)
    require(marginaleffects)
    require(broom)
    require(ggnewscale)
    
    # Marginal effects
    marginal_effects <- 
      tibble(hypotheses(feols_sat_est)[1,]) |> 
      janitor::clean_names() |> 
      bind_rows(
        tibble(hypotheses(feols_sat_est, ~ sequential)) |> 
          janitor::clean_names() |> 
          mutate(
            term = str_split(hypothesis, " - ", simplify = T)[,1],
            term = str_remove_all(term, "[()]")
          ) 
      ) |> 
      select(-hypothesis, -c(s_value:conf_high)) |> 
      mutate(
        q_value = q_val(p_value),
      ) |> 
      rename_at(vars(-term), ~ paste0(.,"_mg")) |> 
      filter(str_detect(term, "colorr")) |> 
      filter(!str_detect(term, "mothers_ed"))
    
    
    # Convert estimates to tibble via broom
    est_meff_tibble <- 
      broom::tidy(feols_sat_est) |> 
      janitor::clean_names() |> 
      select(term, estimate, std_error) |> 
      filter(str_detect(term, "colorr")) |> 
      filter(!str_detect(term, "mothers_ed")) |> 
      # Join marginal eggects
      left_join(
        marginal_effects, by = "term"
      ) |> 
      mutate(
        term = as.numeric(str_remove_all(term, "colorr::")),
        uci95 = estimate + 1.96*std_error,
        lci95 = estimate - 1.96*std_error,
        uci90 = estimate + 1.64*std_error,
        lci90 = estimate - 1.64*std_error,
        q_value_mg = formatC(q_value_mg, format="f", digits=3),
        group1 = term - 1,
        group2 = term
      ) |> 
      bind_rows(
        tibble(
          term = 1,
          estimate = 0
        )
      ) |> 
      arrange(term)
    
    return(est_meff_tibble)
    
  }

#
# -- Plot skin tone gaps ----
plot_skin_tone_gaps <-  
  function(data_plot = data_plot, feols_linear_est = NULL, ame_weights = NULL, estimate_type = ""){
    
    require(tidyverse)
    require(fixest)
    require(marginaleffects)
    require(stringr)
    require(qval)
    require(marginaleffects)
    require(broom)
    require(ggnewscale)
    
    max_skin_tone <- nrow(data_plot)
    
    # Average marginal effect 
    # (weighted)
    if(!is.null(ame_weights)){
      
      avg_meff <- 
        data_plot |> 
        select(estimate_mg) |> 
        mutate(
          weight = ame_weights
        ) |> 
        summarise(
          estimate_mg = weighted.mean(estimate_mg, weight, na.rm = T)
        )
      
    }else{
      # (unweighted)
      avg_meff <- 
        data_plot |> 
        select(estimate_mg) |> 
        mutate(
          weight = 1
        ) |> 
        summarise(
          estimate_mg = weighted.mean(estimate_mg, weight, na.rm = T)
        )
      
    }

    avg_meff <- avg_meff$estimate_mg
    
    if(!is.null(feols_linear_est)){
      
      # Linear effect
      avg_linear <- feols_linear_est$coefficients[1]
      df <- fixest::degrees_freedom(feols_linear_est, "resid")
      num <- unname(avg_linear - avg_meff)
      den <- unname(sqrt((feols_linear_est$se[1])^2))
      t <- abs(num/den)
      p_ame_linear <- 2*pt(q=t, df=df, lower.tail=FALSE)
      
    }
    
    # Marginal effects: Difference between adjacent skin tones
    diffs <- formatC(data_plot$estimate_mg[2:max_skin_tone], digits = 3, format = "f")
    qvals <- data_plot$q_value_mg[2:max_skin_tone]
    qvals <- str_c("[", qvals, "]")
    adj_diff_labs <- str_c(diffs, "\n", qvals)
    
    # Plot
    final_plot <- 
      data_plot |> 
      ggplot(aes(x = term, y = estimate, color = factor(term))) +
      geom_hline(yintercept = 0) +
      geom_linerange(aes(ymin = lci95, ymax = uci95), lwd = .75, width = .1) +
      geom_linerange(aes(ymin = lci90, ymax = uci90), lwd = 1.25) +
      geom_point(size = 4, shape = 21, stroke = 0.75, color = "#7f7f7f", fill = NA) +
      geom_point(size = 3.5) +
      scale_color_manual(values = perla_palette(max_skin_tone)) +
      scale_x_continuous(
        breaks = seq(1,max_skin_tone),
        labels = c(as.character(seq(1,max_skin_tone-1)), str_c(max_skin_tone, " or more"))
      ) +
      labs(x = "Skin tone (PERLA scale)", y = str_c("Estimate ", estimate_type), subtitle = "") +
      theme(
        legend.position = "none", 
        # axis.title = element_text(size = 11),
        axis.text = element_text(size = 12),
      ) 
    
    y_scale_max <- layer_scales(final_plot)$y$range$range[2]
    y_scale_min <- layer_scales(final_plot)$y$range$range[1]
    
    final_plot <- 
      final_plot +
      ggnewscale::new_scale_color() +
      # Marginal effects
      geom_signif(
        annotations = adj_diff_labs,
        y_position = Inf, 
        xmin = seq(1.05, max_skin_tone - 1 + .05, by = 1), xmax = seq(1.95, max_skin_tone - 1 + .95, by = 1),
        tip_length = 0,
        step_increase = -0.025, textsize = 3, vjust = -0.15
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(8)
      ) +
      coord_cartesian(clip = "off") 
    
    
    if(!is.null(feols_linear_est)){
      
      final_plot <- 
        final_plot + 
        geom_label(
          y = 0, x = max_skin_tone-.5, size = 3, hjust = .5, vjust = 1.5,
          label = str_c(
            "AME: ", formatC(avg_meff, format="f", digits=3), "\n",
            "OLS: ", formatC(avg_linear, format="f", digits=3), "\n",
            "p-value: ",  formatC(p_ame_linear, format="f", digits=3)
          )
        )
      
    }else{
      
      final_plot <- 
        final_plot + 
        geom_label(
          y = 0, x = max_skin_tone-.5, size = 3, hjust = .5, vjust = 2,
          label = str_c(
            "AME: ", formatC(avg_meff, format="f", digits=3)
          )
        )
      
    }

    return(final_plot)
    
  }

#
# -- Plot sensemakr bounds ----
plot_bounds <- 
  function(sensemakr_obj = sensemakr_obj, covariate = "covariate", ylims = c(-.065,.015)){
    
    bounds <- 
      tibble(sensemakr_obj$bounds) |> 
      mutate(
        mbar = as.double(str_split(bound_label, "x", simplify = T)[,1]),
        estimate = "Adjusted",
        uci95 = adjusted_estimate + 1.96*adjusted_se,
        lci95 = adjusted_estimate - 1.96*adjusted_se,
        uci90 = adjusted_estimate + 1.64*adjusted_se,
        lci90 = adjusted_estimate - 1.64*adjusted_se,
      ) |> 
      select(mbar, estimate, adjusted_estimate, uci95, lci95, uci90, lci90) |> 
      bind_rows(
        tibble(
          adjusted_estimate = sensemakr_obj$sensitivity_stats$estimate,
          adjusted_se = sensemakr_obj$sensitivity_stats$se,
          mbar = 0,
          estimate = "Original"
        ) |> 
          mutate(
            uci95 = adjusted_estimate + 1.96*adjusted_se,
            lci95 = adjusted_estimate - 1.96*adjusted_se,
            uci90 = adjusted_estimate + 1.64*adjusted_se,
            lci90 = adjusted_estimate - 1.64*adjusted_se,
          )
      ) |> 
      arrange(mbar)
    
    plot <- 
      bounds |> 
      ggplot(aes(mbar, adjusted_estimate, color = estimate)) +
      geom_hline(yintercept = 0) +
      geom_linerange(aes(ymin = lci95, ymax = uci95), lwd = .5, width = .1) +
      geom_linerange(aes(ymin = lci90, ymax = uci90), lwd = 1) +
      geom_point(size = 4) +
      scale_y_continuous(limits = ylims, breaks = seq(-0.1, .1, 0.01)) +
      viridis::scale_color_viridis(discrete = T, begin = .3, end = .6) +
      labs(
        x = str_c("confounder is as X times stronger as ", covariate),
        y = "Skin tone adjusted estimate",
        color = ""
      )
    
    return(plot)
    
  }

# -- Setting a dictionary for fixest ----
setFixest_dict(
  c(
    # Dependent variable
    hhincomepc_mx0 = "Houshold income per capita (log)",
    # Treatment
    colorr = "Skin tone (PERLA)",
    colorr2 = "Skin tone squared (PERLA)",
    # Fixed effects
    year = "Year FE",
    countrycode = "Country FE",
    countrycode_year = "Country $\\times$ Year FE",
    country_region = "Country region FE",
    country_prov = "State/Province FE",
    country_mun = "Municipality FE",
    country_mun_year = "Municipality $\\times$ Year FE",
    cluster_id = "Within-municipality $\\times$ Year FE",
    cluster_enumerator_id = "Within-municipality $\\times$ Year $\\times$ Enumerator FE",
    # Covariates
    sex = "Gender",
    age = "Age",
    edc = "Years of schooling",
    mothers_ed_isced = "Mother's education",
    occupa_status = "Employment status",
    salary_status = "Salary status",
    religion = "Religion",
    etnia = "Ethno-racial identity",
    marital_status = "Marital status",
    intid = "Interviewer FE",
    hh_total = "Household size"
  )
)


# -- Set macros for fixest ----

cov_names <- 
  c(
    "sex", "i(age)", "mothers_ed_pct"
  )

covs_labels <- 
  c(
    "Age", "Years of schooling", "Mother's education"
  )

fe_labels <- 
  c(
    "Geo. strata x Year x Interviewer ID"
  )

setFixest_fml(
  ..covs = ~ sex + i(age) + mothers_ed_c_pct,
  ..fe = ~ cluster_enumerator_id
)


# -- Define custom sensitivity statistics ----
senstat_partial_r2_ty <- function(x) {
  return(sensitivity_stats(x, treatment = "colorr")$r2yd.x[1])
}

senstat_rv_q <- function(x) {
  return(sensitivity_stats(x, treatment = "colorr")$rv_q[1])
}

senstat_rv_qa <- function(x) {
  return(sensitivity_stats(x, treatment = "colorr")$rv_qa[1])
}

fitstat_register(
  type = "partial_r2_ty", alias = "Partial R$^2$ of treatment with outcome",
  fun = function(x) senstat_partial_r2_ty(x)
)

fitstat_register(
  type = "rv_q", alias = "Robustness Value, q = 1",
  fun = function(x) senstat_rv_q(x)
)

fitstat_register(
  type = "rv_qa", alias = "Robustness Value, q = 1 alpha = 0.05",
  fun = function(x) senstat_rv_qa(x)
)

fitstat_register(
  type = "sdy",                     # Internal name of the fitstat
  alias = "Dependent variable s.d.", # Display name in etable
  fun = function(x) {
    sd(model.frame(x)[[1]], na.rm = TRUE)  # Apply sd to the dependent variable
  }
)

# We first reset the default values set in the previous sections
setFixest_etable(reset = TRUE)

#
