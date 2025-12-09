# ==============================================================================
# 03_COMPUTE_ASYMMETRY_TABLE.R
# Calcular tasas de difusión por dirección y dominio para la slide
# ==============================================================================

library(data.table)

# ==============================================================================
# DEFINIR DIRECCIONALIDAD
# ==============================================================================

# Crear indicadores de dirección basados en delta_up_wage y delta_down_wage
# Upward: cuando delta_up_wage > 0 (target tiene mayor salario que source)
# Downward: cuando delta_down_wage > 0 (target tiene menor salario que source)

dt_model[, direction := fcase(
  delta_up_wage > 0 & delta_down_wage == 0, "Upward",
  delta_down_wage > 0 & delta_up_wage == 0, "Downward",
  delta_up_wage == 0 & delta_down_wage == 0, "Lateral",
  default = "Mixed"
)]

# Verificar distribución
message("\n>>> Distribución de direcciones <<<")
print(dt_model[, .N, by = direction][order(-N)])

# ==============================================================================
# CALCULAR TASAS POR DOMINIO Y DIRECCIÓN
# ==============================================================================

# Tabla de tasas de adopción (diffusion rate)
asymmetry_table <- dt_model[direction %in% c("Upward", "Downward"), .(
  n_opportunities = .N,
  n_adoptions = sum(diffusion),
  adoption_rate = mean(diffusion) * 100
), by = .(domain, direction)]

# Pivotear para formato de tabla
asymmetry_wide <- dcast(asymmetry_table, 
                        direction ~ domain, 
                        value.var = "adoption_rate")

# Calcular gaps
asymmetry_wide[, `:=`(
  Gap_Cognitive = NULL,
  Gap_Physical = NULL
)]

# Mostrar resultados
message("\n>>> TASAS DE ADOPCIÓN POR DOMINIO Y DIRECCIÓN <<<\n")
print(asymmetry_wide)

# ==============================================================================
# FORMATO PARA LA SLIDE
# ==============================================================================

# Extraer valores específicos
rate_cog_up <- asymmetry_table[domain == "Cognitive" & direction == "Upward", adoption_rate]
rate_cog_down <- asymmetry_table[domain == "Cognitive" & direction == "Downward", adoption_rate]
rate_phys_up <- asymmetry_table[domain == "Physical" & direction == "Upward", adoption_rate]
rate_phys_down <- asymmetry_table[domain == "Physical" & direction == "Downward", adoption_rate]

# Calcular gaps
gap_cognitive <- rate_cog_up - rate_cog_down
gap_physical <- rate_phys_up - rate_phys_down

# Calcular ratio de fricción
friction_ratio <- rate_phys_down / rate_phys_up

message("\n")
message("================================================================")
message("VALORES PARA LA SLIDE 'The Asymmetry'")
message("================================================================")
message("")
message("                    | Cognitive  | Physical   |")
message("--------------------+------------+------------+")
message(sprintf("Upward rate         | %6.1f%%    | %6.1f%%    |", rate_cog_up, rate_phys_up))
message(sprintf("Downward rate       | %6.1f%%    | %6.1f%%    |", rate_cog_down, rate_phys_down))
message(sprintf("Gap (Up - Down)     | %+5.1f p.p.  | %+5.1f p.p. |", gap_cognitive, gap_physical))
message("")
message(sprintf("Physical skills face %. 1fx more friction going up than down", friction_ratio))
message("")

# ==============================================================================
# TABLA DETALLADA CON N
# ==============================================================================

message("\n>>> Tabla detallada con conteos <<<\n")
print(asymmetry_table[order(domain, direction)])

# ==============================================================================
# VERIFICACIÓN ADICIONAL: Por quintiles de wage
# ==============================================================================

message("\n>>> Verificación por quintiles de wage del target <<<\n")

# Si tienes una variable de quintil de wages en dt_model
if ("wage_quintile_target" %in% names(dt_model)) {
  by_quintile <- dt_model[, .(
    adoption_rate = mean(diffusion) * 100,
    n = .N
  ), by = .(domain, wage_quintile_target)]
  print(dcast(by_quintile, wage_quintile_target ~ domain, value.var = "adoption_rate"))
}

# ==============================================================================
# GUARDAR RESULTADOS
# ==============================================================================

asymmetry_results <- list(
  table_wide = asymmetry_wide,
  table_long = asymmetry_table,
  values = list(
    rate_cog_up = rate_cog_up,
    rate_cog_down = rate_cog_down,
    rate_phys_up = rate_phys_up,
    rate_phys_down = rate_phys_down,
    gap_cognitive = gap_cognitive,
    gap_physical = gap_physical,
    friction_ratio = friction_ratio
  )
)

# saveRDS(asymmetry_results, "asymmetry_table_results.rds")

message("\n>>> 03_compute_asymmetry_table.R completado <<<")