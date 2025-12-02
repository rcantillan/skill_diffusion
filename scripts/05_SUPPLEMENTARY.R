# ==============================================================================
# 05_SUPPLEMENTARY.R
# Tablas y figuras para Supplementary Materials
# ==============================================================================

message("\n")
message("================================================================")
message("05_SUPPLEMENTARY. R")
message("================================================================")

gc()

# ==============================================================================
# TABLE S1: DISTRIBUCIÓN DOMAIN × NESTEDNESS
# ==============================================================================

message("\n>>> Table S1: Distribución Domain × Nestedness <<<\n")

cross_tab <- dt_model[, .N, by = .(domain, nestedness_tercile)]
cross_tab_wide <- dcast(cross_tab, domain ~ nestedness_tercile, value.var = "N")
cross_tab_wide[, Total := Low + Mid + High]
cross_tab_wide[, `Low%` := round(Low/Total * 100, 1)]
cross_tab_wide[, `Mid%` := round(Mid/Total * 100, 1)]
cross_tab_wide[, `High%` := round(High/Total * 100, 1)]

print(cross_tab_wide)

chi_test <- chisq.test(table(dt_model$domain, dt_model$nestedness_tercile))
cramers_v <- sqrt(chi_test$statistic / (nrow(dt_model) * 1))
message("\nCramer's V = ", round(cramers_v, 3))

# ==============================================================================
# TABLE S2: MODEL COMPARISON
# ==============================================================================

message("\n>>> Table S2: Model Comparison <<<\n")

etable(m_simple, m_only_nest, m_additive, m_full,
       headers = c("Domain Only", "Nestedness Only", "Additive", "Interaction"),
       fitstat = c("pr2", "bic", "n"),
       file = file.path(output_tables_dir, "table_s2_model_comparison.tex"))

etable(m_simple, m_only_nest, m_additive, m_full,
       headers = c("Domain Only", "Nestedness Only", "Additive", "Interaction"),
       fitstat = c("pr2", "bic", "n"))

# ==============================================================================
# TABLE S3: STRATIFIED MODELS
# ==============================================================================

message("\n>>> Table S3: Stratified Models <<<\n")

etable(m_cognitive, m_physical,
       headers = c("Cognitive Only", "Physical Only"),
       keep = c("nestedness", "delta_up", "delta_down"),
       fitstat = c("pr2", "n"),
       file = file.path(output_tables_dir, "table_s3_stratified. tex"))

etable(m_cognitive, m_physical,
       headers = c("Cognitive Only", "Physical Only"),
       keep = c("nestedness", "delta_up", "delta_down"),
       fitstat = c("pr2", "n"))

# ==============================================================================
# TABLE S4: PREDICTED PROBABILITIES
# ==============================================================================

message("\n>>> Table S4: Predicted Probabilities <<<\n")

pred_summary <- pred_nest[x %in% c(-2, 0, 2), 
                          .(Probability = paste0(round(mean(predicted) * 100, 1), "%")), 
                          by = .(Domain = domain, Nestedness = nestedness, 
                                 Gap = ifelse(x == -2, "Downward (-2)", 
                                              ifelse(x == 2, "Upward (+2)", "Baseline (0)")))]

pred_summary_wide <- dcast(pred_summary, Domain + Nestedness ~ Gap, value.var = "Probability")
print(pred_summary_wide)

# ==============================================================================
# TABLE S5: ROBUSTNESS (CEM)
# ==============================================================================

message("\n>>> Table S5: Robustness (CEM) <<<\n")

# Cargar resultados CEM si existen
if (exists("m_cem")) {
  etable(m_full, m_cem,
         headers = c("Full Sample", "CEM Matched"),
         fitstat = c("pr2", "n"),
         file = file. path(output_tables_dir, "table_s5_cem. tex"))
  
  etable(m_full, m_cem,
         headers = c("Full Sample", "CEM Matched"),
         fitstat = c("pr2", "n"))
}

# ==============================================================================
# TABLE S6: ROBUSTNESS SUMMARY
# ==============================================================================

message("\n>>> Table S6: Robustness Summary <<<\n")

robustness_table <- data.table(
  Method = c("Full Sample", "CEM Matched", "Physical Only", "Cognitive Only"),
  N = c(
    format(nrow(dt_model), big.mark = ","),
    ifelse(exists("dt_cem_balanced"), format(nrow(dt_cem_balanced), big.mark = ","), "—"),
    format(nrow(dt_model[domain == "Physical"]), big.mark = ","),
    format(nrow(dt_model[domain == "Cognitive"]), big.mark = ",")
  ),
  `β(Nest_High × Down)` = c(
    paste0(round(coef(m_full)["nestedness_tercileHigh:delta_down_wage"], 3), "***"),
    ifelse(exists("m_cem"), 
           paste0(round(coef(m_cem)["nestedness_tercileHigh:delta_down_wage"], 3), "***"), 
           "—"),
    paste0(round(coef(m_physical)["nestedness_tercileHigh:delta_down_wage"], 3), "**"),
    paste0(round(coef(m_cognitive)["nestedness_tercileHigh:delta_down_wage"], 3), "***")
  ),
  `Pseudo R²` = c(
    round(fitstat(m_full, "pr2")[[1]], 4),
    ifelse(exists("m_cem"), round(fitstat(m_cem, "pr2")[[1]], 4), NA),
    round(fitstat(m_physical, "pr2")[[1]], 4),
    round(fitstat(m_cognitive, "pr2")[[1]], 4)
  )
)

print(robustness_table)

# ==============================================================================
# FIGURE S1: CROSS-DISTRIBUTION
# ==============================================================================

message("\n>>> Figure S1: Cross-distribution <<<")

cross_tab[, prop := N / sum(N), by = domain]

fig_s1a <- ggplot(cross_tab, aes(x = nestedness_tercile, y = N, fill = domain)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = scales::comma(N)), 
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = pal_domain, name = "Skill Domain") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "A.  Distribution by Domain × Nestedness",
    subtitle = paste0("Cramer's V = ", round(cramers_v, 3)),
    x = "Nestedness Tercile",
    y = "N observations"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

fig_s1b <- ggplot(cross_tab, aes(x = domain, y = prop, fill = nestedness_tercile)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  scale_fill_manual(values = pal_nest, name = "Nestedness") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "B.  Composition within Domain",
    x = "Skill Domain",
    y = "Proportion"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

fig_s1 <- fig_s1a + fig_s1b

print(fig_s1)

ggsave(file.path(output_data_dir, "fig_s1_cross_distribution.png"), 
       fig_s1, width = 14, height = 6, dpi = 300)

message("Figure S1 guardada")

# ==============================================================================
# FIGURE S2: CEM BALANCE
# ==============================================================================

if (exists("cramers_v_post") && exists("smd_struct_post")) {
  message("\n>>> Figure S2: CEM Balance <<<")
  
  balance_data <- data.table(
    Method = rep(c("Pre-matching", "Post-CEM"), each = 2),
    Variable = rep(c("Nestedness\n(Cramer's V)", "Structural\nDistance (SMD)"), 2),
    Value = c(cramers_v_pre, smd_struct_pre, cramers_v_post, smd_struct_post)
  )
  
  balance_data[, Method := factor(Method, levels = c("Pre-matching", "Post-CEM"))]
  
  fig_s2 <- ggplot(balance_data, aes(x = Variable, y = abs(Value), fill = Method)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_text(aes(label = round(abs(Value), 3)), 
              position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
    scale_fill_manual(values = c("Pre-matching" = "#d62728", "Post-CEM" = "#2ca02c")) +
    scale_y_continuous(limits = c(0, 0.6), expand = expansion(mult = c(0, 0.1))) +
    labs(
      title = "Covariate Balance: Pre vs Post CEM",
      subtitle = "Red dashed line = conventional threshold (< 0.1)",
      x = "",
      y = "|Imbalance Measure|"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  print(fig_s2)
  
  ggsave(file.path(output_data_dir, "fig_s2_cem_balance.png"), 
         fig_s2, width = 9, height = 6, dpi = 300)
  
  message("Figure S2 guardada")
}

# ==============================================================================
# FIGURE S3: STRATIFIED PREDICTIONS (PHYSICAL ONLY)
# ==============================================================================

message("\n>>> Figure S3: Stratified Predictions (Physical Only) <<<")

# Predicciones para Physical only
base_rate_phy <- mean(dt_model[domain == "Physical"]$diffusion)
base_eta_phy <- log(-log(1 - base_rate_phy))

pred_phy_strat <- data.table(expand.grid(
  x = seq(-2, 2, by = 0.02),
  nestedness = c("Low", "Mid", "High"),
  stringsAsFactors = FALSE
))

pred_phy_strat[, delta_up := pmax(0, x)]
pred_phy_strat[, delta_down := pmax(0, -x)]
pred_phy_strat[, nestedness := factor(nestedness, levels = c("Low", "Mid", "High"))]

pred_phy_strat[, lp := mapply(calc_lp_stratified, 
                              delta_up = delta_up, 
                              delta_down = delta_down, 
                              nestedness = as.character(nestedness),
                              MoreArgs = list(coefs = coefs_physical, 
                                              struct_dist = struct_dist_median))]

pred_phy_strat[, predicted := 1 - exp(-exp(base_eta_phy + lp))]

# Límites
y_min_s3 <- min(pred_phy_strat$predicted, na.rm = TRUE) * 0.9
y_max_s3 <- max(pred_phy_strat$predicted, na.rm = TRUE) * 1.1

fig_s3 <- ggplot(pred_phy_strat, aes(x = x, y = predicted, color = nestedness)) +
  geom_line(linewidth = 1.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.7) +
  geom_point(data = pred_phy_strat[abs(x) < 0.02], size = 3, shape = 21, 
             fill = "white", stroke = 1.5) +
  scale_color_manual(values = pal_nest, name = "Nestedness") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(y_min_s3, y_max_s3)) +
  scale_x_continuous(breaks = seq(-2, 2, 1)) +
  labs(
    title = "Physical Skills Only: Nestedness Effect",
    subtitle = "Stratified model (no domain confounding) | Two-way FE",
    x = "Occupational Status Gap (Target − Source)",
    y = "Predicted Adoption Probability"
  ) +
  annotate("text", x = -1.5, y = y_max_s3, label = "← Downward", 
           fontface = "italic", color = "grey40", size = 4) +
  annotate("text", x = 1.5, y = y_max_s3, label = "Upward →", 
           fontface = "italic", color = "grey40", size = 4) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey40", size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(fig_s3)

ggsave(file. path(output_data_dir, "fig_s3_physical_stratified.png"), 
       fig_s3, width = 10, height = 7, dpi = 300)

message("Figure S3 guardada")

# ==============================================================================
# GUARDAR TODO
# ==============================================================================

supplementary_results <- list(
  tables = list(
    cross_tab = cross_tab_wide,
    pred_summary = pred_summary_wide,
    robustness = robustness_table
  ),
  diagnostics = list(
    cramers_v = cramers_v
  )
)

saveRDS(supplementary_results, file.path(output_data_dir, "supplementary_results. rds"))

message("\n>>> 05_supplementary.R completado <<<")
gc()