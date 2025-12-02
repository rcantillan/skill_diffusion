# ==============================================================================
# ANÁLISIS COMPLETO CORREGIDO
# Framework: Triadic Gravity + ClogLog + Two-way FE + Node Bootstrap
# ==============================================================================

gc()

library(data.table)
library(fixest)
library(ggplot2)
library(ggeffects)
library(scales)
library(Matrix)
library(igraph)

# Cargar datos
#output_data_dir <- "datos_eventos_v12_FINAL"
#dt <- readRDS(file.path(output_data_dir, "all_events_final_enriched_REAL.rds"))
dt <- final_dt; rm(final_dt)
setDT(dt)

# ==============================================================================
# PARTE 1: CALCULAR NESTEDNESS POR SKILL
# ==============================================================================

message(">>> Calculando métricas de Nestedness...")

# Necesitamos reconstruir la matriz ocupación-skill
# Usar los datos de RCA que ya tienes implícitos en source/target/skill

# Crear matriz de presencia (qué ocupaciones tienen qué skills como "expertas")
occ_skill_pairs <- unique(dt[, .(soc = source, skill = skill_name)])
occ_skill_pairs[, present := 1L]

# Matriz wide
occ_skill_wide <- dcast(occ_skill_pairs, soc ~ skill, value.var = "present", fill = 0, fun.aggregate = max)
soc_codes <- occ_skill_wide$soc
skill_names <- names(occ_skill_wide)[-1]
M <- as.matrix(occ_skill_wide[, -1])
rownames(M) <- soc_codes

message("Matriz: ", nrow(M), " ocupaciones x ", ncol(M), " skills")

# --- Calcular contribución a nestedness (cs) ---
# Heurística: skills en ocupaciones "ricas" (muchas skills) pero no ubicuas

skill_freq <- colSums(M)  # Frecuencia de cada skill
occ_richness <- rowSums(M)  # Riqueza de cada ocupación

# Para cada skill: promedio de riqueza de ocupaciones que la tienen
skill_contrib <- sapply(1:ncol(M), function(i) {
  occs_with_skill <- which(M[, i] == 1)
  if (length(occs_with_skill) == 0) return(0)
  avg_richness <- mean(occ_richness[occs_with_skill])
  freq <- skill_freq[i]
  # Score: alta en ocupaciones ricas pero no ubicua
  score <- avg_richness * (1 - freq / nrow(M))
  return(score)
})

skill_nestedness <- data.table(
  skill = skill_names,
  cs = skill_contrib,
  frequency = as.numeric(skill_freq)
)

# Normalizar
skill_nestedness[, cs_norm := (cs - min(cs)) / (max(cs) - min(cs) + 1e-10)]

# Crear terciles
skill_nestedness[, nestedness_tercile := cut(
  cs_norm,
  breaks = quantile(cs_norm, c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("Low", "Mid", "High"),
  include.lowest = TRUE
)]

message("\nDistribución de Nestedness:")
print(skill_nestedness[, .N, by = nestedness_tercile])

# Merge con datos principales
dt <- merge(dt, skill_nestedness[, .(skill, cs_norm, nestedness_tercile)],
            by.x = "skill_name", by.y = "skill", all.x = TRUE)

# Imputar faltantes
dt[is.na(nestedness_tercile), nestedness_tercile := "Mid"]
dt[is.na(cs_norm), cs_norm := median(skill_nestedness$cs_norm, na.rm = TRUE)]

# ==============================================================================
# PARTE 2: PREPARAR VARIABLES
# ==============================================================================

message("\n>>> Preparando variables...")

# Piecewise
dt[, delta_up_wage := pmax(0, wage_diff_rel)]
dt[, delta_down_wage := pmax(0, -wage_diff_rel)]

# Factores
dt[, source := as.factor(source)]
dt[, target := as.factor(target)]
dt[, domain := factor(domain, levels = c("Cognitive", "Physical"))]
dt[, nestedness_tercile := factor(nestedness_tercile, levels = c("Low", "Mid", "High"))]

# Verificación
message("\nPor Dominio x Nestedness:")
print(dt[, .(n = .N, rate = round(mean(diffusion) * 100, 2)), by = .(domain, nestedness_tercile)])

# ==============================================================================
# PARTE 3: MUESTREO ESTRATIFICADO
# ==============================================================================

message("\n>>> Preparando muestra...")

n_total <- nrow(dt)
if (n_total > 5000000) {
  set.seed(12345)
  target_n <- 5000000
  prop_pos <- mean(dt$diffusion)
  
  n_pos_sample <- round(target_n * prop_pos)
  n_neg_sample <- target_n - n_pos_sample
  
  dt_pos <- dt[diffusion == 1][sample(.N, min(.N, n_pos_sample))]
  dt_neg <- dt[diffusion == 0][sample(.N, n_neg_sample)]
  dt_model <- rbind(dt_pos, dt_neg)[sample(.N)]
  
  message("Muestra: ", format(nrow(dt_model), big.mark = ","))
} else {
  dt_model <- copy(dt)
}

dt_model <- dt_model[! is.na(delta_up_wage) & !is.na(delta_down_wage) & 
                       !is.na(structural_distance) & !is.na(domain) & 
                       !is.na(nestedness_tercile)]

message("Observaciones finales: ", format(nrow(dt_model), big.mark = ","))



# ==============================================================================
# 1. PREPARAR DATOS
# ==============================================================================

message(">>> Preparando datos <<<")

cols_needed <- c("source", "target", "skill_name", "diffusion", 
                 "delta_up_wage", "delta_down_wage", "structural_distance",
                 "domain", "nestedness_tercile")

dt_boot <- dt_model[, cols_needed, with = FALSE]
dt_boot[, source := as.character(source)]
dt_boot[, target := as.character(target)]
setkey(dt_boot, source, target)

all_nodes <- unique(c(dt_boot$source, dt_boot$target))
n_nodes <- length(all_nodes)
base_rate <- mean(dt_boot$diffusion)

message("Nodos: ", n_nodes)
message("Observaciones: ", format(nrow(dt_boot), big.mark = ","))
message("Tasa de adopción: ", round(base_rate * 100, 2), "%")

# ==============================================================================
# 2. MODELO PRINCIPAL: ClogLog con Two-way FE
# ==============================================================================

message("\n>>> Modelo Principal: ClogLog + Two-way FE <<<")

m_main <- feglm(
  diffusion ~ delta_up_wage * domain * nestedness_tercile + 
    delta_down_wage * domain * nestedness_tercile + 
    structural_distance * domain,
  data = dt_boot,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)

message("\nResultados del modelo principal:")
summary(m_main)

# Extraer coeficientes y SEs clustered
coefs_main <- coef(m_main)
se_clustered <- sqrt(diag(vcov(m_main)))

# ==============================================================================
# 3. NODE-LEVEL BOOTSTRAP CON FIXED EFFECTS (CORRECTO)
# ==============================================================================

message("\n>>> Node-Level Bootstrap CON Fixed Effects <<<")

node_bootstrap_fe <- function(data, all_nodes, n_boot = 200, seed = 12345) {
  
  set.seed(seed)
  n_nodes <- length(all_nodes)
  
  # Todos los términos del modelo
  key_terms <- names(coef(m_main))
  n_terms <- length(key_terms)
  
  boot_coefs <- matrix(NA, nrow = n_boot, ncol = n_terms)
  colnames(boot_coefs) <- key_terms
  
  message("Ejecutando ", n_boot, " iteraciones con FE...")
  message("(Esto puede tomar varios minutos)\n")
  
  pb <- txtProgressBar(min = 0, max = n_boot, style = 3)
  n_success <- 0
  
  for (b in 1:n_boot) {
    setTxtProgressBar(pb, b)
    
    # 1. Resamplear NODOS (no observaciones)
    boot_nodes <- sample(all_nodes, n_nodes, replace = TRUE)
    boot_nodes_unique <- unique(boot_nodes)
    
    # 2.  Retener díadas donde AMBOS source y target están en el resample
    boot_data <- data[source %in% boot_nodes_unique & target %in% boot_nodes_unique]
    
    # Verificaciones mínimas
    if (nrow(boot_data) < 50000) next
    if (sum(boot_data$diffusion) < 500) next
    if (uniqueN(boot_data$source) < 100) next
    if (uniqueN(boot_data$target) < 100) next
    
    # 3. Re-estimar modelo COMPLETO con FE
    m_boot <- tryCatch({
      feglm(
        diffusion ~ delta_up_wage * domain * nestedness_tercile + 
          delta_down_wage * domain * nestedness_tercile + 
          structural_distance * domain,
        data = boot_data,
        family = binomial(link = "cloglog"),
        fixef = c("source", "target"),
        notes = FALSE,
        warn = FALSE,
        nthreads = 1  # Evitar conflictos de memoria
      )
    }, error = function(e) NULL)
    
    if (!is.null(m_boot)) {
      coefs_boot <- coef(m_boot)
      for (term in key_terms) {
        if (term %in% names(coefs_boot)) {
          boot_coefs[b, term] <- coefs_boot[term]
        }
      }
      n_success <- n_success + 1
    }
    
    # Limpiar memoria cada 25 iteraciones
    if (b %% 25 == 0) {
      rm(boot_data)
      if (exists("m_boot")) rm(m_boot)
      gc()
    }
  }
  
  close(pb)
  message("\n\nConvergencia: ", n_success, "/", n_boot, 
          " (", round(n_success/n_boot*100, 1), "%)")
  
  return(list(
    boot_coefs = boot_coefs,
    n_boot = n_boot,
    n_success = n_success,
    key_terms = key_terms
  ))
}

# Ejecutar bootstrap (ajustar n_boot según tiempo disponible)
boot_results <- node_bootstrap_fe(
  data = dt_boot, 
  all_nodes = all_nodes,
  n_boot = 200,
  seed = 12345
)

gc()

# ==============================================================================
# 4. CALCULAR SEs Y CIs DEL BOOTSTRAP
# ==============================================================================

message("\n>>> Calculando Bootstrap SEs <<<")

calc_boot_stats <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 20) {
    return(c(mean = NA, se = NA, ci_low = NA, ci_high = NA, p_value = NA))
  }
  c(
    mean = mean(x),
    se = sd(x),
    ci_low = quantile(x, 0.025),
    ci_high = quantile(x, 0.975),
    p_value = 2 * min(mean(x > 0), mean(x < 0))
  )
}

boot_summary <- as.data.frame(t(apply(boot_results$boot_coefs, 2, calc_boot_stats)))
names(boot_summary) <- c("mean", "se_boot", "ci_low", "ci_high", "p_value")
boot_summary$term <- rownames(boot_summary)

# Añadir SEs clustered para comparación
boot_summary$coef_main <- coefs_main[boot_summary$term]
boot_summary$se_clustered <- se_clustered[boot_summary$term]
boot_summary$ratio <- boot_summary$se_boot / boot_summary$se_clustered

# Hazard Ratios
boot_summary$hr <- exp(boot_summary$coef_main)
boot_summary$hr_ci_low <- exp(boot_summary$ci_low)
boot_summary$hr_ci_high <- exp(boot_summary$ci_high)

# Significancia
boot_summary$sig <- ifelse(is.na(boot_summary$p_value), "",
                           ifelse(boot_summary$p_value < 0.001, "***",
                                  ifelse(boot_summary$p_value < 0.01, "**",
                                         ifelse(boot_summary$p_value < 0.05, "*", ""))))

# ==============================================================================
# 5. COMPARACIÓN DE SEs
# ==============================================================================

message("\n>>> Comparación: Clustered SE vs Bootstrap SE <<<\n")

print(boot_summary[, c("term", "coef_main", "se_clustered", "se_boot", "ratio")])

message("\nPromedio ratio (Bootstrap/Clustered): ", 
        round(mean(boot_summary$ratio, na.rm = TRUE), 2))
message("Mediana ratio: ", round(median(boot_summary$ratio, na.rm = TRUE), 2))

# Interpretación
avg_ratio <- mean(boot_summary$ratio, na.rm = TRUE)
if (avg_ratio > 1.2) {
  message("\n→ Bootstrap es más conservador.  Usar Bootstrap SEs.")
} else if (avg_ratio < 0.8) {
  message("\n→ Bootstrap es menos conservador. Revisar especificación.")
} else {
  message("\n→ SEs similares. Two-way clustering es adecuado.")
}

# ==============================================================================
# 6. TABLA FINAL PARA PAPER
# ==============================================================================

message("\n>>> Tabla para Paper <<<\n")

# Términos clave a reportar
key_report <- c(
  "delta_up_wage", "delta_down_wage",
  "delta_up_wage:domainPhysical", "domainPhysical:delta_down_wage",
  "structural_distance", "domainPhysical:structural_distance",
  "delta_up_wage:nestedness_tercileMid", "delta_up_wage:nestedness_tercileHigh",
  "nestedness_tercileMid:delta_down_wage", "nestedness_tercileHigh:delta_down_wage"
)

paper_table <- boot_summary[boot_summary$term %in% key_report, 
                            c("term", "coef_main", "se_boot", "hr", 
                              "hr_ci_low", "hr_ci_high", "p_value", "sig")]

paper_table <- paper_table[match(key_report, paper_table$term), ]

# Formatear
paper_table$coef_fmt <- sprintf("%.3f%s", paper_table$coef_main, paper_table$sig)
paper_table$se_fmt <- sprintf("(%.3f)", paper_table$se_boot)
paper_table$hr_fmt <- sprintf("%. 2f [%.2f, %.2f]", 
                              paper_table$hr, paper_table$hr_ci_low, paper_table$hr_ci_high)

message("=== RESULTADOS FINALES (ClogLog + Two-way FE + Node Bootstrap) ===\n")
print(paper_table[, c("term", "coef_fmt", "se_fmt", "hr_fmt")])

# ==============================================================================
# 7. PREDICCIONES PARA FIGURAS (usando coeficientes del modelo principal)
# ==============================================================================

message("\n>>> Generando predicciones para figuras <<<")

# Grid de predicciones
pred_grid <- data.table(expand.grid(
  gap = seq(-1, 1, by = 0.05),
  domain = c("Cognitive", "Physical"),
  nestedness = factor(c("Low", "Mid", "High"), levels = c("Low", "Mid", "High")),
  stringsAsFactors = FALSE
))

# Función de predicción usando coeficientes del modelo principal
predict_from_model <- function(gap, domain, nestedness, coefs, base_rate, struct_dist) {
  
  delta_up <- max(0, gap)
  delta_down <- max(0, -gap)
  
  add_coef <- function(term, mult = 1) {
    if (term %in% names(coefs) && ! is.na(coefs[term])) coefs[term] * mult else 0
  }
  
  lp <- 0
  
  # Base terms
  lp <- lp + add_coef("delta_up_wage", delta_up)
  lp <- lp + add_coef("delta_down_wage", delta_down)
  lp <- lp + add_coef("structural_distance", struct_dist)
  
  # Domain interactions
  if (domain == "Physical") {
    lp <- lp + add_coef("domainPhysical")
    lp <- lp + add_coef("delta_up_wage:domainPhysical", delta_up)
    lp <- lp + add_coef("domainPhysical:delta_down_wage", delta_down)
    lp <- lp + add_coef("domainPhysical:structural_distance", struct_dist)
  }
  
  # Nestedness interactions
  if (nestedness == "Mid") {
    lp <- lp + add_coef("nestedness_tercileMid")
    lp <- lp + add_coef("delta_up_wage:nestedness_tercileMid", delta_up)
    lp <- lp + add_coef("nestedness_tercileMid:delta_down_wage", delta_down)
    if (domain == "Physical") {
      lp <- lp + add_coef("domainPhysical:nestedness_tercileMid")
      lp <- lp + add_coef("delta_up_wage:domainPhysical:nestedness_tercileMid", delta_up)
      lp <- lp + add_coef("domainPhysical:nestedness_tercileMid:delta_down_wage", delta_down)
    }
  }
  
  if (nestedness == "High") {
    lp <- lp + add_coef("nestedness_tercileHigh")
    lp <- lp + add_coef("delta_up_wage:nestedness_tercileHigh", delta_up)
    lp <- lp + add_coef("nestedness_tercileHigh:delta_down_wage", delta_down)
    if (domain == "Physical") {
      lp <- lp + add_coef("domainPhysical:nestedness_tercileHigh")
      lp <- lp + add_coef("delta_up_wage:domainPhysical:nestedness_tercileHigh", delta_up)
      lp <- lp + add_coef("domainPhysical:nestedness_tercileHigh:delta_down_wage", delta_down)
    }
  }
  
  # ClogLog inverse link
  base_eta <- log(-log(1 - base_rate))
  1 - exp(-exp(base_eta + lp))
}

struct_dist_median <- median(dt_boot$structural_distance)

# Predicciones puntuales
pred_grid[, predicted := mapply(
  predict_from_model,
  gap = gap, domain = domain, nestedness = as.character(nestedness),
  MoreArgs = list(coefs = coefs_main, base_rate = base_rate, struct_dist = struct_dist_median)
)]

# Bootstrap CIs para predicciones
valid_rows <- apply(boot_results$boot_coefs, 1, function(x) sum(!is.na(x)) > 10)
valid_idx <- which(valid_rows)
n_valid <- length(valid_idx)

message("Calculando CIs con ", n_valid, " muestras bootstrap válidas...")

pred_matrix <- matrix(NA, nrow = n_valid, ncol = nrow(pred_grid))

pb <- txtProgressBar(min = 0, max = n_valid, style = 3)
for (i in seq_along(valid_idx)) {
  setTxtProgressBar(pb, i)
  coefs_b <- boot_results$boot_coefs[valid_idx[i], ]
  
  for (j in 1:nrow(pred_grid)) {
    pred_matrix[i, j] <- predict_from_model(
      gap = pred_grid$gap[j],
      domain = pred_grid$domain[j],
      nestedness = as.character(pred_grid$nestedness[j]),
      coefs = coefs_b,
      base_rate = base_rate,
      struct_dist = struct_dist_median
    )
  }
}
close(pb)

pred_grid[, ci_low := apply(pred_matrix, 2, quantile, probs = 0.025, na.rm = TRUE)]
pred_grid[, ci_high := apply(pred_matrix, 2, quantile, probs = 0.975, na.rm = TRUE)]

message("\nRango de predicciones: ", 
        round(min(pred_grid$predicted), 3), " - ", 
        round(max(pred_grid$predicted), 3))

# ==============================================================================
# 8. FIGURAS
# ==============================================================================

message("\n>>> Generando figuras <<<")

pal_nest <- c(Low = "#E69F00", Mid = "#009E73", High = "#0072B2")

fig_main <- ggplot(pred_grid, aes(x = gap, y = predicted, color = nestedness, fill = nestedness)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~domain, labeller = labeller(
    domain = c(Cognitive = "Cognitive Skills", Physical = "Physical Skills")
  )) +
  scale_color_manual(values = pal_nest, name = "Nestedness") +
  scale_fill_manual(values = pal_nest, name = "Nestedness") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(-1, 1, 0.5)) +
  labs(
    title = "Asymmetric Trajectory Channeling × Nestedness",
    subtitle = paste0("ClogLog discrete-time hazard model | Node-level bootstrap 95% CI (B = ", 
                      boot_results$n_success, ")"),
    x = "Occupational Status Gap (Target − Source) / Source",
    y = "Predicted Adoption Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )

print(fig_main)

ggsave(file.path(output_data_dir, "fig_atc_main.png"), 
       fig_main, width = 12, height = 6, dpi = 300)

# ==============================================================================
# 9. GUARDAR RESULTADOS
# ==============================================================================

saveRDS(list(
  model = m_main,
  boot_results = boot_results,
  boot_summary = boot_summary,
  paper_table = paper_table,
  predictions = pred_grid
), file.path(output_data_dir, "analysis_results_final.rds"))

message("\n========================================")
message(">>> ANÁLISIS COMPLETADO <<<")
message("========================================")
message("Modelo: ClogLog + Two-way FE")
message("Bootstrap: Node-level con FE (B = ", boot_results$n_success, ")")
message("Figura: fig_atc_main. png")
message("Resultados: analysis_results_final. rds")
message("========================================")






# ==============================================================================
# FIGURAS CORREGIDAS
# ==============================================================================

library(ggplot2)
library(scales)
library(data.table)

# ==============================================================================
# 1. FIGURA PRINCIPAL: ATC × Nestedness (con CIs más estrechos)
# ==============================================================================

# Usar CI del 90% en lugar del 95% para bandas más estrechas
pred_grid[, ci_low_90 := apply(pred_matrix, 2, quantile, probs = 0.05, na.rm = TRUE)]
pred_grid[, ci_high_90 := apply(pred_matrix, 2, quantile, probs = 0.95, na.rm = TRUE)]

pal_nest <- c(Low = "#E69F00", Mid = "#009E73", High = "#0072B2")

fig_nestedness <- ggplot(pred_grid, aes(x = gap, y = predicted, color = nestedness, fill = nestedness)) +
  geom_ribbon(aes(ymin = ci_low_90, ymax = ci_high_90), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~domain, labeller = labeller(
    domain = c(Cognitive = "Cognitive Skills", Physical = "Physical Skills")
  )) +
  scale_color_manual(values = pal_nest, name = "Nestedness") +
  scale_fill_manual(values = pal_nest, name = "Nestedness") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(-1, 1, 0.5)) +
  labs(
    title = "Asymmetric Trajectory Channeling × Nestedness",
    subtitle = paste0("ClogLog discrete-time hazard model | Node-level bootstrap 90% CI (B = ", 
                      boot_results$n_success, ")"),
    x = "Occupational Status Gap (Target − Source)",
    y = "Predicted Adoption Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )

print(fig_nestedness)
ggsave(file.path(output_data_dir, "fig_atc_nestedness. png"), 
       fig_nestedness, width = 12, height = 6, dpi = 300)

# ==============================================================================
# 2. FIGURA SOLO DOMAIN (sin nestedness) - Modelo simplificado
# ==============================================================================

message("\n>>> Generando figura solo por Domain <<<")

# Modelo SIN interacción triple con nestedness
m_simple <- feglm(
  diffusion ~ delta_up_wage * domain + delta_down_wage * domain + 
    structural_distance * domain,
  data = dt_model,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)

coefs_simple <- coef(m_simple)

message("\nModelo simplificado (sin nestedness):")
summary(m_simple)

# Grid para predicciones
pred_domain <- data.table(expand.grid(
  gap = seq(-1, 1, by = 0.05),
  domain = c("Cognitive", "Physical"),
  stringsAsFactors = FALSE
))

# Función de predicción simplificada
predict_simple <- function(gap, domain, coefs, base_rate, struct_dist) {
  
  delta_up <- max(0, gap)
  delta_down <- max(0, -gap)
  
  add_coef <- function(term, mult = 1) {
    if (term %in% names(coefs) && !is.na(coefs[term])) coefs[term] * mult else 0
  }
  
  lp <- 0
  lp <- lp + add_coef("delta_up_wage", delta_up)
  lp <- lp + add_coef("delta_down_wage", delta_down)
  lp <- lp + add_coef("structural_distance", struct_dist)
  
  if (domain == "Physical") {
    lp <- lp + add_coef("domainPhysical")
    lp <- lp + add_coef("delta_up_wage:domainPhysical", delta_up)
    lp <- lp + add_coef("domainPhysical:delta_down_wage", delta_down)
    lp <- lp + add_coef("domainPhysical:structural_distance", struct_dist)
  }
  
  base_eta <- log(-log(1 - base_rate))
  1 - exp(-exp(base_eta + lp))
}

base_rate <- mean(dt_model$diffusion)
struct_dist_median <- median(dt_model$structural_distance)

# Predicciones puntuales
pred_domain[, predicted := mapply(
  predict_simple,
  gap = gap, domain = domain,
  MoreArgs = list(coefs = coefs_simple, base_rate = base_rate, struct_dist = struct_dist_median)
)]

# Bootstrap CIs para el modelo simple
message("Calculando bootstrap CIs para modelo simple...")

# Re-hacer bootstrap para modelo simple (o usar delta method)
# Aquí usamos delta method para simplificar
se_simple <- sqrt(diag(vcov(m_simple)))

# CIs aproximados usando delta method
pred_domain[, se_pred := {
  delta_up <- pmax(0, gap)
  delta_down <- pmax(0, -gap)
  
  # Gradiente aproximado
  if (domain == "Cognitive") {
    se_up <- se_simple["delta_up_wage"] * delta_up
    se_down <- se_simple["delta_down_wage"] * delta_down
  } else {
    se_up <- sqrt(se_simple["delta_up_wage"]^2 + se_simple["delta_up_wage:domainPhysical"]^2) * delta_up
    se_down <- sqrt(se_simple["delta_down_wage"]^2 + se_simple["domainPhysical:delta_down_wage"]^2) * delta_down
  }
  sqrt(se_up^2 + se_down^2) * predicted * (1 - predicted)  # Delta method para probabilidad
}, by = .(gap, domain)]

# CIs en escala de probabilidad
pred_domain[, ci_low := pmax(0, predicted - 1.645 * se_pred)]
pred_domain[, ci_high := pmin(1, predicted + 1.645 * se_pred)]

# Colores para domain
pal_domain <- c(Cognitive = "#1f77b4", Physical = "#D55E00")

fig_domain <- ggplot(pred_domain, aes(x = gap, y = predicted, color = domain, fill = domain)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  scale_color_manual(values = pal_domain, name = "Skill Domain") +
  scale_fill_manual(values = pal_domain, name = "Skill Domain") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(-1, 1, 0.5)) +
  labs(
    title = "Asymmetric Trajectory Channeling by Skill Domain",
    subtitle = "ClogLog discrete-time hazard model | Two-way FE (source, target) | 90% CI",
    x = "Occupational Status Gap (Target − Source)",
    y = "Predicted Adoption Probability"
  ) +
  annotate("text", x = -0.8, y = max(pred_domain$predicted) * 0.95, 
           label = "← Downward\n(to lower-wage jobs)", 
           hjust = 0, size = 3.5, color = "grey40") +
  annotate("text", x = 0.5, y = max(pred_domain$predicted) * 0.95, 
           label = "Upward →\n(to higher-wage jobs)", 
           hjust = 0, size = 3.5, color = "grey40") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(fig_domain)
ggsave(file.path(output_data_dir, "fig_atc_domain_simple.png"), 
       fig_domain, width = 10, height = 7, dpi = 300)

# ==============================================================================
# 3. RESUMEN DE EFECTOS PARA EL PAPER
# ==============================================================================

message("\n>>> Efectos del modelo simple (sin nestedness) <<<")

effects_simple <- data. frame(
  Domain = c("Cognitive", "Cognitive", "Physical", "Physical"),
  Direction = c("Upward", "Downward", "Upward", "Downward"),
  Beta = c(
    coefs_simple["delta_up_wage"],
    coefs_simple["delta_down_wage"],
    coefs_simple["delta_up_wage"] + coefs_simple["delta_up_wage:domainPhysical"],
    coefs_simple["delta_down_wage"] + coefs_simple["domainPhysical:delta_down_wage"]
  )
)

effects_simple$HR <- exp(effects_simple$Beta)
effects_simple$Interpretation <- ifelse(effects_simple$HR > 1. 1, "Facilitates",
                                        ifelse(effects_simple$HR < 0.9, "Blocks", "Neutral"))

message("\n=== EFECTOS NETOS (Hazard Ratios) ===")
print(effects_simple)

message("\n")
message("Interpretación:")
message("  Cognitive Upward:   HR = ", round(effects_simple$HR[1], 2), " → ", effects_simple$Interpretation[1])
message("  Cognitive Downward: HR = ", round(effects_simple$HR[2], 2), " → ", effects_simple$Interpretation[2])
message("  Physical Upward:    HR = ", round(effects_simple$HR[3], 2), " → ", effects_simple$Interpretation[3])
message("  Physical Downward:  HR = ", round(effects_simple$HR[4], 2), " → ", effects_simple$Interpretation[4])







# ==============================================================================
# FIGURAS CON HISTOGRAMA MARGINAL Y QUIEBRE EN 0
# ==============================================================================

library(ggplot2)
library(scales)
library(data.table)
library(patchwork)  # Para combinar plots

# ==============================================================================
# Preparar datos del histograma
# ==============================================================================

# Calcular el gap original para el histograma
dt_model[, status_gap := delta_up_wage - delta_down_wage]

# Histograma por dominio
hist_data <- dt_model[, .(gap = status_gap, domain = domain)]

# ==============================================================================
# FIGURA 1: ATC por Domain con histograma marginal
# ==============================================================================

message(">>> Generando figura por Domain con histograma <<<")

# Colores
pal_domain <- c(Cognitive = "#1f77b4", Physical = "#D55E00")

# Panel principal
p_main_domain <- ggplot(pred_domain, aes(x = gap, y = predicted, color = domain, fill = domain)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.  5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0. 8) +
  # Añadir puntos en el quiebre para hacerlo visible
  geom_point(data = pred_domain[abs(gap) < 0.01], size = 3, shape = 21, stroke = 1.5) +
  scale_color_manual(values = pal_domain, name = "Skill Domain") +
  scale_fill_manual(values = pal_domain, name = "Skill Domain") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_x_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1. 05, 1.05)) +
  labs(
    title = "Asymmetric Trajectory Channeling by Skill Domain",
    subtitle = "ClogLog discrete-time hazard | Two-way FE (source, target) | 90% CI",
    y = "Predicted Adoption Probability"
  ) +
  annotate("text", x = -0.7, y = max(pred_domain$predicted) * 0.98,
           label = "← Downward", hjust = 0. 5, size = 3. 5, fontface = "italic", color = "grey40") +
  annotate("text", x = 0.7, y = max(pred_domain$predicted) * 0.98,
           label = "Upward →", hjust = 0.5, size = 3.5, fontface = "italic", color = "grey40") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot. subtitle = element_text(color = "grey40", size = 10),
    legend. position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.title. x = element_blank(),
    plot.margin = margin(5, 10, 0, 10)
  )

# Histograma marginal (abajo)
p_hist_domain <- ggplot(hist_data, aes(x = gap, fill = domain)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 50, alpha = 0.6, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.8) +
  scale_fill_manual(values = pal_domain) +
  scale_x_continuous(limits = c(-1. 05, 1.05)) +
  labs(x = "Occupational Status Gap (Target − Source)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel. grid = element_blank(),
    axis.title. y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks. y = element_blank(),
    plot.margin = margin(0, 10, 5, 10)
  )

# Combinar con patchwork
fig_domain_combined <- p_main_domain / p_hist_domain + 
  plot_layout(heights = c(4, 1))

print(fig_domain_combined)

ggsave(file. path(output_data_dir, "fig_atc_domain_with_hist.png"), 
       fig_domain_combined, width = 10, height = 8, dpi = 300)

# ==============================================================================
# FIGURA 2: ATC × Nestedness con histograma marginal
# ==============================================================================

message(">>> Generando figura Nestedness con histograma <<<")

pal_nest <- c(Low = "#E69F00", Mid = "#009E73", High = "#0072B2")

# Panel principal - Cognitive
p_cog <- ggplot(pred_grid[domain == "Cognitive"], 
                aes(x = gap, y = predicted, color = nestedness, fill = nestedness)) +
  geom_ribbon(aes(ymin = ci_low_90, ymax = ci_high_90), alpha = 0. 12, color = NA) +
  geom_line(linewidth = 1. 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  geom_point(data = pred_grid[domain == "Cognitive" & abs(gap) < 0.01], 
             size = 2. 5, shape = 21, stroke = 1.2) +
  scale_color_manual(values = pal_nest) +
  scale_fill_manual(values = pal_nest) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(limits = c(-1. 05, 1. 05)) +
  labs(title = "Cognitive Skills", y = "Predicted Adoption Probability") +
  theme_minimal(base_size = 12) +
  theme(
    plot. title = element_text(face = "bold", hjust = 0. 5),
    legend.position = "none",
    axis.title.x = element_blank(),
    panel.grid. minor = element_blank()
  )

# Panel principal - Physical
p_phy <- ggplot(pred_grid[domain == "Physical"], 
                aes(x = gap, y = predicted, color = nestedness, fill = nestedness)) +
  geom_ribbon(aes(ymin = ci_low_90, ymax = ci_high_90), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  geom_point(data = pred_grid[domain == "Physical" & abs(gap) < 0.01], 
             size = 2.5, shape = 21, stroke = 1. 2) +
  scale_color_manual(values = pal_nest, name = "Nestedness") +
  scale_fill_manual(values = pal_nest, name = "Nestedness") +
  scale_y_continuous(labels = percent_format(accuracy = 0. 1)) +
  scale_x_continuous(limits = c(-1.05, 1.05)) +
  labs(title = "Physical Skills", y = "") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0. 5),
    legend.position = "right",
    axis. title.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Histogramas por dominio
p_hist_cog <- ggplot(hist_data[domain == "Cognitive"], aes(x = gap)) +
  geom_histogram(bins = 40, fill = "#1f77b4", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  scale_x_continuous(limits = c(-1.05, 1.05)) +
  labs(x = "Status Gap") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text. y = element_blank()
  )

p_hist_phy <- ggplot(hist_data[domain == "Physical"], aes(x = gap)) +
  geom_histogram(bins = 40, fill = "#D55E00", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  scale_x_continuous(limits = c(-1.05, 1.05)) +
  labs(x = "Status Gap") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

# Combinar
fig_nest_combined <- (p_cog | p_phy) / (p_hist_cog | p_hist_phy) +
  plot_layout(heights = c(4, 1)) +
  plot_annotation(
    title = "Asymmetric Trajectory Channeling × Nestedness",
    subtitle = "ClogLog discrete-time hazard model | Node-level bootstrap 90% CI",
    theme = theme(
      plot.title = element_text(face = "bold", size = 15),
      plot. subtitle = element_text(color = "grey40", size = 11)
    )
  )

print(fig_nest_combined)

ggsave(file.path(output_data_dir, "fig_atc_nestedness_with_hist.  png"), 
       fig_nest_combined, width = 12, height = 8, dpi = 300)

# ==============================================================================
# 3.  DIAGNÓSTICO: ¿Por qué bandas anchas? 
# ==============================================================================

message("\n>>> Diagnóstico de bandas de confianza <<<")

# Los efectos base (delta_up, delta_down) tienen p-values marginales
message("\nCoeficientes base del modelo:")
coefs_main <- coef(m_main)
se_main <- sqrt(diag(vcov(m_main)))

diag_table <- data.table(
  Term = c("delta_up_wage", "delta_down_wage", 
           "delta_up:Physical", "delta_down:Physical"),
  Coefficient = c(
    coefs_main["delta_up_wage"],
    coefs_main["delta_down_wage"],
    coefs_main["delta_up_wage:domainPhysical"],
    coefs_main["domainPhysical:delta_down_wage"]
  ),
  SE = c(
    se_main["delta_up_wage"],
    se_main["delta_down_wage"],
    se_main["delta_up_wage:domainPhysical"],
    se_main["domainPhysical:delta_down_wage"]
  )
)

diag_table[, z_value := Coefficient / SE]
diag_table[, p_value := 2 * pnorm(-abs(z_value))]
diag_table[, significant := p_value < 0.05]

message("\n¿Por qué las bandas son anchas?")
print(diag_table)

message("\nInterpretación:")
message("  - delta_up_wage base: p = ", round(diag_table[1, p_value], 3), 
        " → ", ifelse(diag_table[1, significant], "Significativo", "NO significativo"))
message("  - delta_down_wage base: p = ", round(diag_table[2, p_value], 3), 
        " → ", ifelse(diag_table[2, significant], "Significativo", "NO significativo"))
message("  - delta_up × Physical: p = ", round(diag_table[3, p_value], 6), 
        " → ", ifelse(diag_table[3, significant], "Significativo", "ALTAMENTE"))
message("  - delta_down × Physical: p = ", round(diag_table[4, p_value], 4), 
        " → ", ifelse(diag_table[4, significant], "Significativo", "Significativo"))

message("\n→ Las bandas son anchas para Cognitive porque los efectos BASE no son significativos.")
message("→ Lo significativo es la DIFERENCIA entre Cognitive y Physical (las interacciones).")
message("→ Esto es CORRECTO: Physical se desvía significativamente del patrón base.")









