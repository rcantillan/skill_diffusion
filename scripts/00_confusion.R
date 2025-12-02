# ==============================================================================
# DIAGNÓSTICO COMPLETO: ¿DOMAIN Y NESTEDNESS ESTÁN CONFUNDIDOS?
# ==============================================================================

library(data.table)
library(fixest)
library(ggplot2)
library(scales)

gc()

message(">>> DIAGNÓSTICO DE CONFUSIÓN DOMAIN × NESTEDNESS <<<\n")

# ==============================================================================
# 0. FUNCIÓN calc_lp (necesaria para predicciones)
# ==============================================================================

calc_lp <- function(delta_up, delta_down, domain, nestedness, coefs, struct_dist) {
  
  lp <- 0
  
  if ("delta_up_wage" %in% names(coefs)) {
    lp <- lp + coefs["delta_up_wage"] * delta_up
  }
  if ("delta_down_wage" %in% names(coefs)) {
    lp <- lp + coefs["delta_down_wage"] * delta_down
  }
  if ("structural_distance" %in% names(coefs)) {
    lp <- lp + coefs["structural_distance"] * struct_dist
  }
  
  if (domain == "Physical") {
    if ("domainPhysical" %in% names(coefs)) {
      lp <- lp + coefs["domainPhysical"]
    }
    if ("delta_up_wage:domainPhysical" %in% names(coefs)) {
      lp <- lp + coefs["delta_up_wage:domainPhysical"] * delta_up
    }
    if ("domainPhysical:delta_down_wage" %in% names(coefs)) {
      lp <- lp + coefs["domainPhysical:delta_down_wage"] * delta_down
    }
    if ("domainPhysical:structural_distance" %in% names(coefs)) {
      lp <- lp + coefs["domainPhysical:structural_distance"] * struct_dist
    }
  }
  
  if (nestedness == "Mid") {
    if ("nestedness_tercileMid" %in% names(coefs)) {
      lp <- lp + coefs["nestedness_tercileMid"]
    }
    if ("delta_up_wage:nestedness_tercileMid" %in% names(coefs)) {
      lp <- lp + coefs["delta_up_wage:nestedness_tercileMid"] * delta_up
    }
    if ("nestedness_tercileMid:delta_down_wage" %in% names(coefs)) {
      lp <- lp + coefs["nestedness_tercileMid:delta_down_wage"] * delta_down
    }
    if (domain == "Physical") {
      if ("domainPhysical:nestedness_tercileMid" %in% names(coefs)) {
        lp <- lp + coefs["domainPhysical:nestedness_tercileMid"]
      }
      if ("delta_up_wage:domainPhysical:nestedness_tercileMid" %in% names(coefs)) {
        lp <- lp + coefs["delta_up_wage:domainPhysical:nestedness_tercileMid"] * delta_up
      }
      if ("domainPhysical:nestedness_tercileMid:delta_down_wage" %in% names(coefs)) {
        lp <- lp + coefs["domainPhysical:nestedness_tercileMid:delta_down_wage"] * delta_down
      }
    }
  }
  
  if (nestedness == "High") {
    if ("nestedness_tercileHigh" %in% names(coefs)) {
      lp <- lp + coefs["nestedness_tercileHigh"]
    }
    if ("delta_up_wage:nestedness_tercileHigh" %in% names(coefs)) {
      lp <- lp + coefs["delta_up_wage:nestedness_tercileHigh"] * delta_up
    }
    if ("nestedness_tercileHigh:delta_down_wage" %in% names(coefs)) {
      lp <- lp + coefs["nestedness_tercileHigh:delta_down_wage"] * delta_down
    }
    if (domain == "Physical") {
      if ("domainPhysical:nestedness_tercileHigh" %in% names(coefs)) {
        lp <- lp + coefs["domainPhysical:nestedness_tercileHigh"]
      }
      if ("delta_up_wage:domainPhysical:nestedness_tercileHigh" %in% names(coefs)) {
        lp <- lp + coefs["delta_up_wage:domainPhysical:nestedness_tercileHigh"] * delta_up
      }
      if ("domainPhysical:nestedness_tercileHigh:delta_down_wage" %in% names(coefs)) {
        lp <- lp + coefs["domainPhysical:nestedness_tercileHigh:delta_down_wage"] * delta_down
      }
    }
  }
  
  return(lp)
}

# ==============================================================================
# 1. DISTRIBUCIÓN CRUZADA
# ==============================================================================

message("=== 1. TABLA CRUZADA: DOMAIN × NESTEDNESS ===\n")

cross_tab <- dt_model[, .N, by = .(domain, nestedness_tercile)]
cross_tab_wide <- dcast(cross_tab, domain ~ nestedness_tercile, value.var = "N")
cross_tab_wide[, Total := Low + Mid + High]
cross_tab_wide[, Low_pct := round(Low/Total * 100, 1)]
cross_tab_wide[, Mid_pct := round(Mid/Total * 100, 1)]
cross_tab_wide[, High_pct := round(High/Total * 100, 1)]

message("Distribución de Nestedness DENTRO de cada Domain:")
print(cross_tab_wide[, .(domain, Low, Mid, High, Total, 
                         `Low%` = Low_pct, `Mid%` = Mid_pct, `High%` = High_pct)])

# Test Chi-cuadrado
chi_test <- chisq.test(table(dt_model$domain, dt_model$nestedness_tercile))
message("\nChi-squared test (independencia):")
message("  X² = ", round(chi_test$statistic, 1))
message("  p-value = ", format(chi_test$p.value, scientific = TRUE, digits = 3))

# Cramer's V
n <- nrow(dt_model)
k <- min(2, 3)
cramers_v <- sqrt(chi_test$statistic / (n * (k - 1)))
message("  Cramer's V = ", round(cramers_v, 4), 
        " (", ifelse(cramers_v < 0.1, "DÉBIL", ifelse(cramers_v < 0.3, "MODERADA", "FUERTE")), ")")

# ==============================================================================
# 2.  VISUALIZACIÓN DE LA DISTRIBUCIÓN CRUZADA
# ==============================================================================

message("\n=== 2. GENERANDO VISUALIZACIONES ===\n")

cross_tab[, prop := N / sum(N), by = domain]

p_cross <- ggplot(cross_tab, aes(x = nestedness_tercile, y = N, fill = domain)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = scales::comma(N)), 
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c(Cognitive = "#1f77b4", Physical = "#D55E00"), 
                    name = "Skill Domain") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Distribución Cruzada: Domain × Nestedness",
    subtitle = paste0("Cramer's V = ", round(cramers_v, 4), " (", 
                      ifelse(cramers_v < 0.1, "asociación débil", 
                             ifelse(cramers_v < 0.3, "asociación moderada", "asociación fuerte")), ")"),
    x = "Nestedness Tercile",
    y = "N observaciones"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_cross)

ggsave(file. path(output_data_dir, "diag_cross_distribution.png"), 
       p_cross, width = 10, height = 6, dpi = 300)

# Proporciones dentro de cada domain
p_prop <- ggplot(cross_tab, aes(x = domain, y = prop, fill = nestedness_tercile)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(prop * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  scale_fill_manual(values = c(Low = "#E69F00", Mid = "#009E73", High = "#0072B2"),
                    name = "Nestedness") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Composición de Nestedness dentro de cada Domain",
    x = "Skill Domain",
    y = "Proporción"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_prop)

ggsave(file. path(output_data_dir, "diag_nestedness_composition.png"), 
       p_prop, width = 8, height = 6, dpi = 300)

# ==============================================================================
# 3. MODELOS SEPARADOS PARA DISTINGUIR EFECTOS
# ==============================================================================

message("\n=== 3. DESCOMPOSICIÓN DE EFECTOS ===\n")

# Modelo A: Solo Domain
m_only_domain <- feglm(
  diffusion ~ delta_up_wage * domain + delta_down_wage * domain + 
    structural_distance * domain,
  data = dt_model,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)

# Modelo B: Solo Nestedness
m_only_nest <- feglm(
  diffusion ~ delta_up_wage * nestedness_tercile + delta_down_wage * nestedness_tercile + 
    structural_distance,
  data = dt_model,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)

# Modelo C: Aditivo
m_additive <- feglm(
  diffusion ~ delta_up_wage * domain + delta_down_wage * domain + 
    delta_up_wage * nestedness_tercile + delta_down_wage * nestedness_tercile +
    structural_distance * domain,
  data = dt_model,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)

# Modelo D: Interacción triple
m_full <- feglm(
  diffusion ~ delta_up_wage * domain * nestedness_tercile + 
    delta_down_wage * domain * nestedness_tercile + 
    structural_distance * domain,
  data = dt_model,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)

message("Comparación de modelos:")
etable(m_only_domain, m_only_nest, m_additive, m_full,
       headers = c("Solo Domain", "Solo Nestedness", "Aditivo", "Interacción"),
       fitstat = c("pr2", "bic", "n"))

# ==============================================================================
# 4.  ESTADÍSTICOS DE AJUSTE
# ==============================================================================

message("\n=== 4.  COMPARACIÓN DE AJUSTE ===\n")

pr2_domain <- fitstat(m_only_domain, "pr2")[[1]]
pr2_nest <- fitstat(m_only_nest, "pr2")[[1]]
pr2_additive <- fitstat(m_additive, "pr2")[[1]]
pr2_full <- fitstat(m_full, "pr2")[[1]]

bic_domain <- fitstat(m_only_domain, "bic")[[1]]
bic_nest <- fitstat(m_only_nest, "bic")[[1]]
bic_additive <- fitstat(m_additive, "bic")[[1]]
bic_full <- fitstat(m_full, "bic")[[1]]

message("Pseudo R²:")
message("  Solo Domain:     ", round(pr2_domain, 5))
message("  Solo Nestedness: ", round(pr2_nest, 5))
message("  Aditivo:         ", round(pr2_additive, 5))
message("  Interacción:     ", round(pr2_full, 5))

message("\nBIC (menor es mejor):")
message("  Solo Domain:     ", format(round(bic_domain, 0), big.mark = ","))
message("  Solo Nestedness: ", format(round(bic_nest, 0), big.mark = ","))
message("  Aditivo:         ", format(round(bic_additive, 0), big.mark = ","))
message("  Interacción:     ", format(round(bic_full, 0), big.mark = ","))

best_model <- which.min(c(bic_domain, bic_nest, bic_additive, bic_full))
model_names <- c("Solo Domain", "Solo Nestedness", "Aditivo", "Interacción")
message("\nMejor modelo según BIC: ", model_names[best_model])

# ==============================================================================
# 5. CONTRIBUCIÓN INCREMENTAL
# ==============================================================================

message("\n=== 5. CONTRIBUCIÓN INCREMENTAL ===\n")

message("Contribución al Pseudo R²:")
message("  Domain solo:                    ", round(pr2_domain * 100, 3), "%")
message("  Nestedness solo:                ", round(pr2_nest * 100, 3), "%")
message("  Añadir Nestedness a Domain:     +", round((pr2_additive - pr2_domain) * 100, 3), "%")
message("  Añadir Domain a Nestedness:     +", round((pr2_additive - pr2_nest) * 100, 3), "%")
message("  Añadir interacción triple:      +", round((pr2_full - pr2_additive) * 100, 3), "%")

# ==============================================================================
# 6. PREDICCIONES
# ==============================================================================

message("\n=== 6. CALCULANDO PREDICCIONES ===\n")

coefs_full <- coef(m_full)
base_rate <- mean(dt_model$diffusion)
struct_dist_median <- median(dt_model$structural_distance)
base_eta <- log(-log(1 - base_rate))

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
                         MoreArgs = list(coefs = coefs_full, struct_dist = struct_dist_median))]

pred_nest[, predicted := 1 - exp(-exp(base_eta + lp))]

# ==============================================================================
# 7. EFECTOS MARGINALES
# ==============================================================================

message("\n=== 7.  EFECTOS MARGINALES ===\n")

# Efecto de Domain (promediando sobre Nestedness)
effect_domain <- pred_nest[x %in% c(-2, 2), 
                           .(prob = mean(predicted) * 100), 
                           by = .(domain, Direction = ifelse(x < 0, "Downward", "Upward"))]
effect_domain_wide <- dcast(effect_domain, domain ~ Direction, value.var = "prob")
effect_domain_wide[, Asymmetry := Downward - Upward]

message("Efecto de DOMAIN (promediando Nestedness):")
print(effect_domain_wide)

# Efecto de Nestedness (promediando sobre Domain)
effect_nest <- pred_nest[x %in% c(-2, 2), 
                         .(prob = mean(predicted) * 100), 
                         by = .(nestedness, Direction = ifelse(x < 0, "Downward", "Upward"))]
effect_nest_wide <- dcast(effect_nest, nestedness ~ Direction, value.var = "prob")
effect_nest_wide[, Asymmetry := Downward - Upward]

message("\nEfecto de NESTEDNESS (promediando Domain):")
print(effect_nest_wide)

domain_contribution <- abs(effect_domain_wide[domain == "Physical", Asymmetry] - 
                             effect_domain_wide[domain == "Cognitive", Asymmetry])

nest_contribution <- abs(effect_nest_wide[nestedness == "Low", Asymmetry] - 
                           effect_nest_wide[nestedness == "High", Asymmetry])

message("\n→ Domain contribuye:     ", round(domain_contribution, 2), " p.p.  de diferencia")
message("→ Nestedness contribuye: ", round(nest_contribution, 2), " p.p.  de diferencia")

# ==============================================================================
# 8.  EFECTO DENTRO DE CADA DOMINIO
# ==============================================================================

message("\n=== 8.  EFECTO DE NESTEDNESS DENTRO DE CADA DOMAIN ===\n")

effect_within <- pred_nest[x %in% c(-2, 2), 
                           .(prob = mean(predicted) * 100), 
                           by = .(domain, nestedness, Direction = ifelse(x < 0, "Downward", "Upward"))]

effect_within_wide <- dcast(effect_within, domain + nestedness ~ Direction, value.var = "prob")
effect_within_wide[, Asymmetry := Downward - Upward]

message("Asimetría (Downward - Upward) por Domain × Nestedness:")
print(effect_within_wide[order(domain, nestedness)])

nest_effect_cog <- effect_within_wide[domain == "Cognitive" & nestedness == "Low", Asymmetry] -
  effect_within_wide[domain == "Cognitive" & nestedness == "High", Asymmetry]

nest_effect_phy <- effect_within_wide[domain == "Physical" & nestedness == "Low", Asymmetry] -
  effect_within_wide[domain == "Physical" & nestedness == "High", Asymmetry]

message("\nEfecto de Nestedness (Low→High) DENTRO de cada Domain:")
message("  Cognitive: Δasimetría = ", round(nest_effect_cog, 2), " p.p.")
message("  Physical:  Δasimetría = ", round(nest_effect_phy, 2), " p.p.")

# ==============================================================================
# 9. FIGURA RESUMEN
# ==============================================================================

message("\n=== 9.  FIGURA RESUMEN ===\n")

effect_within_wide[, nestedness := factor(nestedness, levels = c("Low", "Mid", "High"))]

p_summary <- ggplot(effect_within_wide, aes(x = nestedness, y = Asymmetry, 
                                            color = domain, group = domain)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1.5) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(Asymmetry, 1), " p.p.")), 
            vjust = -1.2, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c(Cognitive = "#1f77b4", Physical = "#D55E00"),
                     name = "Skill Domain") +
  labs(
    title = "Efecto de Nestedness sobre la Asimetría del Flujo",
    subtitle = "Asimetría = P(Downward) - P(Upward) | Valores positivos = más difusión hacia abajo",
    x = "Nestedness Tercile",
    y = "Asimetría (puntos porcentuales)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom"
  )

print(p_summary)

ggsave(file.path(output_data_dir, "diag_nestedness_effect_by_domain.png"), 
       p_summary, width = 10, height = 7, dpi = 300)

# ==============================================================================
# 10. CONCLUSIÓN
# ==============================================================================

message("\n")
message("================================================================")
message("CONCLUSIÓN: ¿DOMAIN Y NESTEDNESS APORTAN INDEPENDIENTEMENTE? ")
message("================================================================")
message("")
message("1. ASOCIACIÓN ENTRE DOMAIN Y NESTEDNESS:")
message("   Cramer's V = ", round(cramers_v, 4))
if (cramers_v < 0.1) {
  message("   ✅ Asociación DÉBIL → Los efectos son independientes")
} else if (cramers_v < 0.3) {
  message("   ⚠️  Asociación MODERADA → Algo de confusión pero ambos aportan")
} else {
  message("   ❌ Asociación FUERTE → Los efectos pueden estar confundidos")
}

message("")
message("2. CONTRIBUCIÓN AL MODELO (Pseudo R²):")
message("   Solo Domain:      ", round(pr2_domain * 100, 3), "%")
message("   Solo Nestedness:  ", round(pr2_nest * 100, 3), "%")
message("   Aditivo:          ", round(pr2_additive * 100, 3), "%")
message("   Con interacción:  ", round(pr2_full * 100, 3), "%")

message("")
message("3. CONTRIBUCIÓN A LA ASIMETRÍA:")
message("   Domain aporta:     ", round(domain_contribution, 2), " p.p.")
message("   Nestedness aporta: ", round(nest_contribution, 2), " p.p.")

message("")
message("4. EFECTO DE NESTEDNESS DENTRO DE CADA DOMAIN:")
message("   En Cognitive: ", round(nest_effect_cog, 2), " p. p.")
message("   En Physical:  ", round(nest_effect_phy, 2), " p.p.")
if (abs(nest_effect_phy) > abs(nest_effect_cog)) {
  message("   → Nestedness tiene MAYOR efecto en Physical skills")
} else {
  message("   → Nestedness tiene MAYOR efecto en Cognitive skills")
}

message("")
message("5.  RECOMENDACIÓN PARA EL PAPER:")
if (best_model == 4) {
  message("   ✅ USAR modelo con interacción triple")
  message("   → Domain Y Nestedness contribuyen de forma NO aditiva")
  message("   → El efecto de nestedness DEPENDE del domain")
} else if (best_model == 3) {
  message("   ⚠️  Considerar modelo aditivo (más parsimonioso)")
  message("   → Los efectos son aproximadamente independientes")
} else {
  message("   ⚠️  Solo uno de los factores parece relevante")
}
message("================================================================")

gc()
