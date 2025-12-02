# ==============================================================================
# 03_CEM_MATCHING.R
# Coarsened Exact Matching para robustez
# ==============================================================================

message("\n")
message("================================================================")
message("03_CEM_MATCHING.R")
message("================================================================")

gc()

# ==============================================================================
# PREPARAR DATOS PARA MATCHING
# ==============================================================================

message("\n>>> Preparando datos para CEM <<<")

dt_match <- copy(dt_model)
dt_match[, treat := as.integer(domain == "Physical")]
dt_match[, nest_num := as.integer(nestedness_tercile)]

# ==============================================================================
# BALANCE PRE-MATCHING
# ==============================================================================

message("\n>>> Balance Pre-Matching <<<")

# Chi-squared y Cramer's V
chi_pre <- chisq.test(table(dt_match$domain, dt_match$nestedness_tercile))
cramers_v_pre <- sqrt(chi_pre$statistic / (nrow(dt_match) * 1))

message("Cramer's V (pre-matching): ", round(cramers_v_pre, 4))

# SMD para structural_distance
smd_struct_pre <- dt_match[, {
  mean_phy <- mean(structural_distance[domain == "Physical"], na.rm = TRUE)
  mean_cog <- mean(structural_distance[domain == "Cognitive"], na.rm = TRUE)
  sd_phy <- sd(structural_distance[domain == "Physical"], na.rm = TRUE)
  sd_cog <- sd(structural_distance[domain == "Cognitive"], na.rm = TRUE)
  pooled_sd <- sqrt((sd_phy^2 + sd_cog^2) / 2)
  (mean_phy - mean_cog) / pooled_sd
}]

message("SMD structural_distance (pre-matching): ", round(smd_struct_pre, 4))

# Distribución por celda
message("\nDistribución Domain × Nestedness (pre-matching):")
print(dcast(dt_match[, .N, by = .(domain, nestedness_tercile)], 
            domain ~ nestedness_tercile, value.var = "N"))
gc()

# ==============================================================================
# CREAR STRATA PARA CEM
# ==============================================================================

message("\n>>> Creando strata para CEM <<<")

# Discretizar structural_distance en terciles
dt_match[, struct_tercile := cut(
  structural_distance, 
  breaks = quantile(structural_distance, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("Low", "Mid", "High"),
  include.lowest = TRUE
)]

# Crear strata
dt_match[, match_strata := paste(nestedness_tercile, struct_tercile, sep = "_")]

# Contar por strata
strata_counts <- dt_match[, .N, by = .(match_strata, domain)]
strata_wide <- dcast(strata_counts, match_strata ~ domain, value.var = "N", fill = 0)
strata_wide[, min_n := pmin(Cognitive, Physical)]
strata_wide[, matched := min_n > 0]

message("Strata de matching:")
print(strata_wide[order(-min_n)])

total_matcheable <- sum(strata_wide$min_n) * 2
message("\nObservaciones matcheables: ", format(total_matcheable, big.mark = ","))
message("Proporción del total: ", round(total_matcheable / nrow(dt_match) * 100, 1), "%")

# ==============================================================================
# REALIZAR CEM
# ==============================================================================

message("\n>>> Realizando CEM <<<")

set.seed(42)

dt_cem <- dt_match[match_strata %in% strata_wide[matched == TRUE, match_strata]]

dt_cem_balanced <- dt_cem[, {
  n_cog <- sum(domain == "Cognitive")
  n_phy <- sum(domain == "Physical")
  n_match <- min(n_cog, n_phy)
  
  if (n_match > 0) {
    cog_idx <- sample(which(domain == "Cognitive"), n_match)
    phy_idx <- sample(which(domain == "Physical"), n_match)
    .SD[c(cog_idx, phy_idx)]
  } else {
    .SD[0]
  }
}, by = match_strata]

message("N después de CEM: ", format(nrow(dt_cem_balanced), big.mark = ","))

# ==============================================================================
# BALANCE POST-CEM
# ==============================================================================

message("\n>>> Balance Post-CEM <<<")

# Cramer's V post-CEM
chi_post <- chisq.test(table(dt_cem_balanced$domain, dt_cem_balanced$nestedness_tercile))
cramers_v_post <- sqrt(chi_post$statistic / (nrow(dt_cem_balanced) * 1))

message("Cramer's V (post-CEM): ", round(cramers_v_post, 4))

# SMD para structural_distance
smd_struct_post <- dt_cem_balanced[, {
  mean_phy <- mean(structural_distance[domain == "Physical"], na.rm = TRUE)
  mean_cog <- mean(structural_distance[domain == "Cognitive"], na.rm = TRUE)
  sd_phy <- sd(structural_distance[domain == "Physical"], na.rm = TRUE)
  sd_cog <- sd(structural_distance[domain == "Cognitive"], na.rm = TRUE)
  pooled_sd <- sqrt((sd_phy^2 + sd_cog^2) / 2)
  (mean_phy - mean_cog) / pooled_sd
}]

message("SMD structural_distance (post-CEM): ", round(smd_struct_post, 4))

# Distribución post-CEM
message("\nDistribución Domain × Nestedness (post-CEM):")
print(dcast(dt_cem_balanced[, .N, by = .(domain, nestedness_tercile)], 
            domain ~ nestedness_tercile, value.var = "N"))

# Mejora
message("\n>>> MEJORA EN BALANCE <<<")
message("Cramer's V: ", round(cramers_v_pre, 3), " → ", round(cramers_v_post, 3))
message("SMD struct_dist: ", round(smd_struct_pre, 3), " → ", round(smd_struct_post, 3))

# ==============================================================================
# MODELO EN MUESTRA CEM
# ==============================================================================

message("\n>>> Estimando modelo en muestra CEM <<<")
gc()
m_cem <- feglm(
  diffusion ~ delta_up_wage * domain * nestedness_tercile + 
    delta_down_wage * domain * nestedness_tercile + 
    structural_distance * domain,
  data = dt_cem_balanced,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)

message("Modelo CEM estimado.  Pseudo R² = ", 
        round(fitstat(m_cem, "pr2")[[1]], 4))

gc()

# ==============================================================================
# COMPARACIÓN FULL vs CEM
# ==============================================================================

message("\n>>> Comparación Full vs CEM <<<\n")

etable(m_full, m_cem,
       headers = c("Full Sample", "CEM Matched"),
       keep = c("nestedness_tercileHigh", "delta_down_wage", "domainPhysical"),
       fitstat = c("pr2", "n"))

# ==============================================================================
# GUARDAR RESULTADOS CEM
# ==============================================================================

cem_results <- list(
  dt_cem_balanced = dt_cem_balanced,
  m_cem = m_cem,
  diagnostics = list(
    cramers_v_pre = cramers_v_pre,
    cramers_v_post = cramers_v_post,
    smd_struct_pre = smd_struct_pre,
    smd_struct_post = smd_struct_post,
    n_matched = nrow(dt_cem_balanced),
    n_original = nrow(dt_model)
  )
)

saveRDS(cem_results, file. path(output_data_dir, "cem_results.rds"))

message("\n>>> 03_cem_matching.R completado <<<")
gc()