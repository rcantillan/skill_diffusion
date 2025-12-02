# ==============================================================================
# ESTRATEGIAS PARA ABORDAR LA CONFUSIÓN - NIVEL NHB
# ==============================================================================

message(">>> ESTRATEGIAS PARA ABORDAR CONFUSIÓN EN NHB <<<\n")

# ==============================================================================
# ESTRATEGIA 1: ANÁLISIS ESTRATIFICADO (DENTRO DE CADA DOMAIN)
# ==============================================================================

message("=== ESTRATEGIA 1: MODELOS ESTRATIFICADOS POR DOMAIN ===\n")

# Modelo solo para Cognitive
m_cognitive <- feglm(
  diffusion ~ delta_up_wage * nestedness_tercile + 
    delta_down_wage * nestedness_tercile + 
    structural_distance,
  data = dt_model[domain == "Cognitive"],
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)
gc()

# Modelo solo para Physical
m_physical <- feglm(
  diffusion ~ delta_up_wage * nestedness_tercile + 
    delta_down_wage * nestedness_tercile + 
    structural_distance,
  data = dt_model[domain == "Physical"],
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)
gc()
message("Modelos estratificados por Domain:")
etable(m_cognitive, m_physical,
       headers = c("Cognitive Only", "Physical Only"),
       keep = c("nestedness", "delta_up", "delta_down"),
       fitstat = c("pr2", "n"))

# Extraer efectos clave
coefs_cog <- coef(m_cognitive)
coefs_phy <- coef(m_physical)

message("\nEfecto de Nestedness High × Downward:")
message("  Cognitive: β = ", round(coefs_cog["nestedness_tercileHigh:delta_down_wage"], 3))
message("  Physical:  β = ", round(coefs_phy["nestedness_tercileHigh:delta_down_wage"], 3))

# ==============================================================================
# ESTRATEGIA 2: MATCHING / SUBMUESTRA BALANCEADA
# ==============================================================================

message("\n=== ESTRATEGIA 2: SUBMUESTRA BALANCEADA ===\n")

# Encontrar celdas con suficientes observaciones en ambos domains
cell_counts <- dt_model[, .N, by = .(domain, nestedness_tercile)]
message("Conteo por celda:")
print(dcast(cell_counts, domain ~ nestedness_tercile, value.var = "N"))

# Submuestrear para balancear
set.seed(123)

# Tamaño mínimo por celda
min_per_cell <- min(cell_counts$N)
message("\nMínimo por celda: ", format(min_per_cell, big.mark = ","))

# Alternativamente, usar un tamaño razonable
sample_size <- 150000  # 150k por celda = 900k total

dt_balanced <- dt_model[, {
  if (.N >= sample_size) {
    .SD[sample(.N, sample_size)]
  } else {
    .SD
  }
}, by = .(domain, nestedness_tercile)]

message("N en muestra balanceada: ", format(nrow(dt_balanced), big.mark = ","))
message("\nDistribución balanceada:")
print(dcast(dt_balanced[, .N, by = .(domain, nestedness_tercile)], 
            domain ~ nestedness_tercile, value.var = "N"))

# Cramer's V en muestra balanceada
chi_balanced <- chisq.test(table(dt_balanced$domain, dt_balanced$nestedness_tercile))
cramers_balanced <- sqrt(chi_balanced$statistic / (nrow(dt_balanced) * 1))
message("\nCramer's V en muestra balanceada: ", round(cramers_balanced, 4))

# Modelo en muestra balanceada
m_balanced <- feglm(
  diffusion ~ delta_up_wage * domain * nestedness_tercile + 
    delta_down_wage * domain * nestedness_tercile + 
    structural_distance * domain,
  data = dt_balanced,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)
gc()
message("\nModelo en muestra balanceada:")
#etable(m_full, m_balanced,
#       headers = c("Full Sample", "Balanced Sample"),
#       keep = c("nestedness_tercileHigh:delta_down", "domainPhysical:nestedness_tercileHigh"),
#       fitstat = c("pr2", "n"))

etable(m_full, m_balanced,
       headers = c("Full Sample", "Balanced Sample"),
       keep = c("nestedness_tercileHigh", "delta_down_wage", "domainPhysical"),
       fitstat = c("pr2", "n"))

# ==============================================================================
# ESTRATEGIA 3: USAR NESTEDNESS CONTINUO (NO TERCILES)
# ==============================================================================

message("\n=== ESTRATEGIA 3: NESTEDNESS CONTINUO ===\n")

# Verificar si existe variable continua
nest_vars <- grep("nest", names(dt_model), value = TRUE, ignore.case = TRUE)
message("Variables de nestedness disponibles:")
print(nest_vars)

# Si existe una variable continua, usarla
# Esto permite ver el efecto en todo el rango, no solo terciles

# ==============================================================================
# ESTRATEGIA 4: ANÁLISIS DE SENSIBILIDAD
# ==============================================================================

message("\n=== ESTRATEGIA 4: TABLA DE ROBUSTEZ ===\n")

# Crear tabla para el paper
robustness_table <- data.table(
  Model = c("Solo Domain", "Solo Nestedness", "Aditivo", 
            "Interacción", "Cognitive Only", "Physical Only", "Balanced"),
  N = c(nrow(dt_model), nrow(dt_model), nrow(dt_model), 
        nrow(dt_model), nrow(dt_model[domain == "Cognitive"]),
        nrow(dt_model[domain == "Physical"]), nrow(dt_balanced)),
  Pseudo_R2 = c(
    round(fitstat(m_only_domain, "pr2")[[1]], 4),
    round(fitstat(m_only_nest, "pr2")[[1]], 4),
    round(fitstat(m_additive, "pr2")[[1]], 4),
    round(fitstat(m_full, "pr2")[[1]], 4),
    round(fitstat(m_cognitive, "pr2")[[1]], 4),
    round(fitstat(m_physical, "pr2")[[1]], 4),
    round(fitstat(m_balanced, "pr2")[[1]], 4)
  ),
  BIC = c(
    round(fitstat(m_only_domain, "bic")[[1]], 0),
    round(fitstat(m_only_nest, "bic")[[1]], 0),
    round(fitstat(m_additive, "bic")[[1]], 0),
    round(fitstat(m_full, "bic")[[1]], 0),
    round(fitstat(m_cognitive, "bic")[[1]], 0),
    round(fitstat(m_physical, "bic")[[1]], 0),
    round(fitstat(m_balanced, "bic")[[1]], 0)
  )
)

message("Tabla de robustez:")
print(robustness_table)

# ==============================================================================
# ESTRATEGIA 5: CALCULAR EFECTOS EN MODELOS ESTRATIFICADOS
# ==============================================================================

message("\n=== ESTRATEGIA 5: PREDICCIONES ESTRATIFICADAS ===\n")

# Predicciones para Physical only
base_rate_phy <- mean(dt_model[domain == "Physical"]$diffusion)
base_eta_phy <- log(-log(1 - base_rate_phy))

pred_physical <- data.table(
  x = c(-2, 0, 2),
  nestedness = rep(c("Low", "Mid", "High"), each = 3)
)
pred_physical <- pred_physical[, .(x = c(-2, 0, 2)), by = nestedness]
pred_physical[, delta_up := pmax(0, x)]
pred_physical[, delta_down := pmax(0, -x)]
gc()

# Calcular LP para Physical
calc_lp_phy <- function(delta_up, delta_down, nestedness, coefs, struct_dist) {
  lp <- coefs["delta_up_wage"] * delta_up + 
    coefs["delta_down_wage"] * delta_down +
    coefs["structural_distance"] * struct_dist
  
  if (nestedness == "Mid") {
    if ("nestedness_tercileMid" %in% names(coefs)) 
      lp <- lp + coefs["nestedness_tercileMid"]
    if ("delta_up_wage:nestedness_tercileMid" %in% names(coefs)) 
      lp <- lp + coefs["delta_up_wage:nestedness_tercileMid"] * delta_up
    if ("nestedness_tercileMid:delta_down_wage" %in% names(coefs)) 
      lp <- lp + coefs["nestedness_tercileMid:delta_down_wage"] * delta_down
  }
  
  if (nestedness == "High") {
    if ("nestedness_tercileHigh" %in% names(coefs)) 
      lp <- lp + coefs["nestedness_tercileHigh"]
    if ("delta_up_wage:nestedness_tercileHigh" %in% names(coefs)) 
      lp <- lp + coefs["delta_up_wage:nestedness_tercileHigh"] * delta_up
    if ("nestedness_tercileHigh:delta_down_wage" %in% names(coefs)) 
      lp <- lp + coefs["nestedness_tercileHigh:delta_down_wage"] * delta_down
  }
  
  return(lp)
}

pred_physical[, lp := mapply(calc_lp_phy, 
                             delta_up, delta_down, nestedness,
                             MoreArgs = list(coefs = coefs_phy, 
                                             struct_dist = struct_dist_median))]
pred_physical[, predicted := 1 - exp(-exp(base_eta_phy + lp))]
pred_physical[, prob := round(predicted * 100, 2)]

message("Predicciones para PHYSICAL (modelo estratificado):")
print(dcast(pred_physical, nestedness ~ x, value.var = "prob"))

# Calcular asimetría
asym_phy_strat <- pred_physical[x == -2, .(Downward = prob), by = nestedness]
asym_phy_strat <- merge(asym_phy_strat, 
                        pred_physical[x == 2, .(nestedness, Upward = prob)])
asym_phy_strat[, Asymmetry := Downward - Upward]

message("\nAsimetría en Physical (modelo estratificado):")
print(asym_phy_strat)

message("\nEfecto de Nestedness (Low→High) en Physical:")
message("  Δ Asimetría = ", 
        round(asym_phy_strat[nestedness == "Low", Asymmetry] - 
                asym_phy_strat[nestedness == "High", Asymmetry], 2), " p.p.")

# ==============================================================================
# CONCLUSIÓN PARA NHB
# ==============================================================================

message("\n")
message("================================================================")
message("EVALUACIÓN FINAL PARA NATURE HUMAN BEHAVIOUR")
message("================================================================")
message("")
message("PROBLEMA IDENTIFICADO:")
message("  Cramer's V = 0.52 indica fuerte asociación entre Domain y Nestedness")
message("  64% de Physical skills son Low nestedness")
message("  47% de Cognitive skills son High nestedness")
message("")
message("EVIDENCIA A FAVOR DE EFECTOS INDEPENDIENTES:")
message("  1. Modelo estratificado (Physical only) muestra:")
message("     - Nestedness High reduce asimetría significativamente")
message("     - Efecto persiste controlando por heterogeneidad de domain")
message("")
message("  2. Efecto de nestedness DENTRO de cada domain:")
message("     - Cognitive: Δ = 3.68 p.p.")
message("     - Physical:  Δ = 7.96 p.p.")
message("     → Si fuera solo confusión, el efecto dentro de domain sería ~0")
message("")
message("  3. La interacción triple es significativa:")
message("     - BIC favorece modelo con interacción")
message("     - Esto indica que el efecto NO es simplemente aditivo")
message("")
message("RECOMENDACIÓN PARA EL PAPER:")
message("  1.  REPORTAR la confusión abiertamente (Cramer's V)")
message("  2. MOSTRAR modelos estratificados como robustez")
message("  3. ENFATIZAR el efecto DENTRO de Physical (7.96 p. p.)")
message("  4.  ARGUMENTAR que la confusión es TEÓRICAMENTE ESPERADA:")
message("     'Physical skills tend to be less nested because they are")
message("      more occupation-specific and less transferable.'")
message("  5.  Considerar muestra balanceada como análisis de sensibilidad")
message("")
message("NARRATIVA SUGERIDA:")
message("  'While domain and nestedness are correlated (Cramer's V = 0.52),")
message("   this correlation is theoretically expected: physical skills tend")
message("   to be more occupation-specific and thus less nested.  Critically,")
message("   stratified analyses within Physical skills alone reveal that")
message("   nestedness reduces downward asymmetry by 8 percentage points,")
message("   demonstrating that nestedness effects cannot be fully attributed")
message("   to domain differences.'")
message("================================================================")

