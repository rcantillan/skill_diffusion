# ==============================================================================
# 04_FIGURES_MAIN.R
# Figuras para el texto principal del paper
# ==============================================================================

message("\n")
message("================================================================")
message("04_FIGURES_MAIN.R")
message("================================================================")

gc()

# Z para intervalos de confianza al 90%
z_90 <- 1.645

# ==============================================================================
# PREPARAR DATOS PARA HISTOGRAMAS
# ==============================================================================

dt_model[, x_hist := delta_up_wage - delta_down_wage]
hist_data <- dt_model[, .(x = x_hist, domain)]

# ==============================================================================
# FIGURA 1: ATC BY DOMAIN
# ==============================================================================

message("\n>>> Generando Figura 1: ATC by Domain <<<")

# Predicciones
pred_domain <- data.table(expand.grid(
  x = seq(-2, 2, by = 0.02),
  domain = c("Cognitive", "Physical"),
  stringsAsFactors = FALSE
))

pred_domain[, delta_up := pmax(0, x)]
pred_domain[, delta_down := pmax(0, -x)]

pred_domain[, lp := mapply(calc_lp, 
                           delta_up = delta_up, 
                           delta_down = delta_down, 
                           domain = domain, 
                           nestedness = "Low",
                           MoreArgs = list(coefs = coefs_simple, 
                                           struct_dist = struct_dist_median))]

pred_domain[, predicted := 1 - exp(-exp(base_eta + lp))]

# CIs
pred_domain[, se_lp := mapply(calc_se, 
                              delta_up = delta_up, 
                              delta_down = delta_down, 
                              domain = domain, 
                              nestedness = "Low",
                              MoreArgs = list(vcov_m = vcov_simple, 
                                              struct_dist = struct_dist_median))]

pred_domain[, eta := base_eta + lp]
pred_domain[, ci_low := 1 - exp(-exp(eta - z_90 * se_lp))]
pred_domain[, ci_high := 1 - exp(-exp(eta + z_90 * se_lp))]

# Límites del eje Y
y_min_1 <- min(pred_domain$ci_low, na.rm = TRUE) * 0.9
y_max_1 <- max(pred_domain$ci_high, na.rm = TRUE) * 1.1

# Panel principal
p1_main <- ggplot(pred_domain, aes(x = x, y = predicted, color = domain, fill = domain)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) +
  geom_point(data = pred_domain[abs(x) < 0.02], size = 3.5, shape = 21, stroke = 1.5) +
  facet_wrap(~domain, labeller = labeller(
    domain = c(Cognitive = "Cognitive Skills", Physical = "Physical Skills")
  )) +
  scale_color_manual(values = pal_domain, guide = "none") +
  scale_fill_manual(values = pal_domain, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(y_min_1, y_max_1),
                     expand = expansion(mult = c(0.02, 0.15))) +
  scale_x_continuous(breaks = seq(-2, 2, 1)) +
  labs(
    title = "Asymmetric Trajectory Channeling by Skill Domain",
    subtitle = "Predicted adoption probability | ClogLog discrete-time hazard | Two-way FE | 90% CI",
    y = "Predicted Adoption Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey40", size = 11),
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12)
  )

# Anotaciones
annot_1 <- data.frame(
  x = c(-1.5, 1.5, -1.5, 1.5),
  y = rep(y_max_1, 4),
  label = c("← Downward", "Upward →", "← Downward", "Upward →"),
  domain = c("Cognitive", "Cognitive", "Physical", "Physical")
)

p1_main <- p1_main +
  geom_text(data = annot_1, aes(x = x, y = y, label = label),
            size = 4, fontface = "italic", color = "grey40",
            vjust = -0.5, inherit.aes = FALSE)

# Histograma
p1_hist <- ggplot(hist_data[abs(x) <= 2], aes(x = x, fill = domain)) +
  geom_histogram(bins = 60, alpha = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  facet_wrap(~domain) +
  scale_fill_manual(values = pal_domain) +
  scale_x_continuous(limits = c(-2.1, 2.1), breaks = seq(-2, 2, 1)) +
  labs(x = "Occupational Status Gap (Target − Source)") +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

# Combinar
fig1 <- p1_main / p1_hist + plot_layout(heights = c(4, 1))

print(fig1)

#ggsave(file.path(output_data_dir, "fig1_atc_domain.png"), 
#       fig1, width = 11, height = 7, dpi = 300)

message("Figura 1 guardada: fig1_atc_domain.png")

# ==============================================================================
# FIGURA 2: ATC × NESTEDNESS
# ==============================================================================

message("\n>>> Generando Figura 2: ATC × Nestedness <<<")

# Predicciones
pred_nest <- data.table(expand.grid(
  x = seq(-2, 2, by = 0.02),
  domain = c("Cognitive", "Physical"),
  nestedness = c("Low", "Mid", "High"),
  stringsAsFactors = FALSE
))

pred_nest[, delta_up := pmax(0, x)]
pred_nest[, delta_down := pmax(0, -x)]
pred_nest[, nestedness := factor(nestedness, levels = c("Low", "Mid", "High"))]

pred_nest[, lp := mapply(calc_lp, 
                         delta_up = delta_up, 
                         delta_down = delta_down, 
                         domain = domain, 
                         nestedness = as.character(nestedness),
                         MoreArgs = list(coefs = coefs_full, 
                                         struct_dist = struct_dist_median))]

pred_nest[, predicted := 1 - exp(-exp(base_eta + lp))]

# CIs
pred_nest[, se_lp := mapply(calc_se, 
                            delta_up = delta_up, 
                            delta_down = delta_down, 
                            domain = domain, 
                            nestedness = as.character(nestedness),
                            MoreArgs = list(vcov_m = vcov_full, 
                                            struct_dist = struct_dist_median))]

pred_nest[, eta := base_eta + lp]
pred_nest[, ci_low := 1 - exp(-exp(eta - z_90 * se_lp))]
pred_nest[, ci_high := 1 - exp(-exp(eta + z_90 * se_lp))]

# Límites del eje Y
y_min_2 <- min(pred_nest$ci_low, na.rm = TRUE) * 0.9
y_max_2 <- max(pred_nest$ci_high, na.rm = TRUE) * 1.1

# Panel principal
p2_main <- ggplot(pred_nest, aes(x = x, y = predicted, color = nestedness, fill = nestedness)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.7) +
  geom_point(data = pred_nest[abs(x) < 0.02], size = 2.5, shape = 21, stroke = 1.2) +
  facet_wrap(~domain, labeller = labeller(
    domain = c(Cognitive = "Cognitive Skills", Physical = "Physical Skills")
  )) +
  scale_color_manual(values = pal_nest, name = "Nestedness") +
  scale_fill_manual(values = pal_nest, name = "Nestedness") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(y_min_2, y_max_2),
                     expand = expansion(mult = c(0.02, 0.15))) +
  scale_x_continuous(breaks = seq(-2, 2, 1)) +
  labs(
    title = "Nestedness Moderates Trajectory Channeling",
    subtitle = "Predicted adoption probability | ClogLog discrete-time hazard | Two-way FE | 90% CI",
    y = "Predicted Adoption Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey40", size = 11),
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12)
  )

# Anotaciones
annot_2 <- data.frame(
  x = c(-1.5, 1.5, -1.5, 1.5),
  y = rep(y_max_2, 4),
  label = c("← Downward", "Upward →", "← Downward", "Upward →"),
  domain = c("Cognitive", "Cognitive", "Physical", "Physical")
)

p2_main <- p2_main +
  geom_text(data = annot_2, aes(x = x, y = y, label = label),
            size = 4, fontface = "italic", color = "grey40",
            vjust = -0.5, inherit.aes = FALSE)

# Histograma
p2_hist <- ggplot(hist_data[abs(x) <= 2], aes(x = x, fill = domain)) +
  geom_histogram(bins = 60, alpha = 0.5, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  facet_wrap(~domain) +
  scale_fill_manual(values = pal_domain) +
  scale_x_continuous(limits = c(-2.1, 2.1), breaks = seq(-2, 2, 1)) +
  labs(x = "Occupational Status Gap (Target − Source)") +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

# Combinar
fig2 <- p2_main / p2_hist + plot_layout(heights = c(4, 1))

print(fig2)

#ggsave(file.path(output_data_dir, "fig2_atc_nestedness.png"), 
#       fig2, width = 11, height = 7, dpi = 300)

message("Figura 2 guardada: fig2_atc_nestedness. png")

# ==============================================================================
# FIGURA 3: ASYMMETRY DECOMPOSITION
# ==============================================================================

message("\n>>> Generando Figura 3: Asymmetry Decomposition <<<")

# Calcular asimetría por celda
effect_within <- pred_nest[x %in% c(-2, 2), 
                           .(prob = mean(predicted) * 100), 
                           by = .(domain, nestedness, 
                                  Direction = ifelse(x < 0, "Downward", "Upward"))]

effect_within_wide <- dcast(effect_within, domain + nestedness ~ Direction, value.var = "prob")
effect_within_wide[, Asymmetry := Downward - Upward]
effect_within_wide[, nestedness := factor(nestedness, levels = c("Low", "Mid", "High"))]

# Figura
fig3 <- ggplot(effect_within_wide, aes(x = nestedness, y = Asymmetry, 
                                       color = domain, group = domain)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 5) +
  geom_text(aes(label = paste0(round(Asymmetry, 1))), 
            vjust = -1.5, size = 4.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = pal_domain, name = "Skill Domain") +
  scale_y_continuous(limits = c(-5, 12), breaks = seq(-4, 12, 2)) +
  labs(
    title = "Nestedness Effect on Diffusion Asymmetry",
    subtitle = "Asymmetry = P(Downward) − P(Upward) at ±2 SD | Positive = more downward diffusion",
    x = "Skill Nestedness",
    y = "Asymmetry (percentage points)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey40", size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(fig3)

#ggsave(file.path(output_data_dir, "fig3_asymmetry_decomposition.png"), 
#       fig3, width = 9, height = 7, dpi = 300)

message("Figura 3 guardada: fig3_asymmetry_decomposition.png")

# ==============================================================================
# GUARDAR PREDICCIONES
# ==============================================================================

#saveRDS(list(
#  pred_domain = pred_domain,
#  pred_nest = pred_nest,
#  effect_within = effect_within_wide
#), file.path(output_data_dir, "predictions_main.rds"))

message("\n>>> 04_figures_main.R completado <<<")
gc()
