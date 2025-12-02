# ==============================================================================
# 02_ESTIMATE_MODELS.R
# Estimación de todos los modelos para análisis ATC
# ==============================================================================

message("\n")
message("================================================================")
message("02_ESTIMATE_MODELS.R")
message("================================================================")

# Cargar funciones (ajustar path según tu estructura)
# source("01_setup_functions.R")

gc()

# ==============================================================================
# VERIFICAR DATOS
# ==============================================================================

message("\n>>> Verificando datos <<<")
message("N observaciones: ", format(nrow(dt_model), big.mark = ","))
message("Variables: ", paste(names(dt_model)[1:12], collapse = ", "))

# Parámetros base
base_rate <- mean(dt_model$diffusion)
struct_dist_median <- median(dt_model$structural_distance)
base_eta <- log(-log(1 - base_rate))

message("Tasa base de adopción: ", round(base_rate * 100, 2), "%")
message("Distancia estructural mediana: ", round(struct_dist_median, 3))

# ==============================================================================
# MODELO 1: SIMPLE (Solo Domain)
# ==============================================================================

message("\n>>> Estimando Modelo 1: Simple (Domain) <<<")

m_simple <- feglm(
  diffusion ~ delta_up_wage * domain + delta_down_wage * domain + 
    structural_distance * domain,
  data = dt_model,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)

message("Modelo simple estimado.  Pseudo R² = ", 
        round(fitstat(m_simple, "pr2")[[1]], 4))

# ==============================================================================
# MODELO 2: COMPLETO (Domain × Nestedness)
# ==============================================================================

message("\n>>> Estimando Modelo 2: Completo (Domain × Nestedness) <<<")

m_full <- feglm(
  diffusion ~ delta_up_wage * domain * nestedness_tercile + 
    delta_down_wage * domain * nestedness_tercile + 
    structural_distance * domain,
  data = dt_model,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)

message("Modelo completo estimado. Pseudo R² = ", 
        round(fitstat(m_full, "pr2")[[1]], 4))
gc()

# ==============================================================================
# MODELO 3: SOLO NESTEDNESS
# ==============================================================================

message("\n>>> Estimando Modelo 3: Solo Nestedness <<<")

m_only_nest <- feglm(
  diffusion ~ delta_up_wage * nestedness_tercile + 
    delta_down_wage * nestedness_tercile + 
    structural_distance,
  data = dt_model,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)
gc()
message("Modelo nestedness estimado.  Pseudo R² = ", 
        round(fitstat(m_only_nest, "pr2")[[1]], 4))

# ==============================================================================
# MODELO 4: ADITIVO (sin interacción triple)
# ==============================================================================

message("\n>>> Estimando Modelo 4: Aditivo <<<")

m_additive <- feglm(
  diffusion ~ delta_up_wage * domain + delta_down_wage * domain + 
    delta_up_wage * nestedness_tercile + delta_down_wage * nestedness_tercile +
    structural_distance * domain,
  data = dt_model,
  family = binomial(link = "cloglog"),
  fixef = c("source", "target"),
  cluster = c("source", "target")
)
gc()
message("Modelo aditivo estimado.  Pseudo R² = ", 
        round(fitstat(m_additive, "pr2")[[1]], 4))

# ==============================================================================
# MODELOS ESTRATIFICADOS
# ==============================================================================

message("\n>>> Estimando Modelos Estratificados <<<")

# Cognitive only
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
message("Modelo Cognitive estimado. N = ", 
        format(nrow(dt_model[domain == "Cognitive"]), big.mark = ","),
        ", Pseudo R² = ", round(fitstat(m_cognitive, "pr2")[[1]], 4))

# Physical only
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
message("Modelo Physical estimado. N = ", 
        format(nrow(dt_model[domain == "Physical"]), big.mark = ","),
        ", Pseudo R² = ", round(fitstat(m_physical, "pr2")[[1]], 4))

# ==============================================================================
# EXTRAER COEFICIENTES
# ==============================================================================

coefs_simple <- coef(m_simple)
coefs_full <- coef(m_full)
coefs_cognitive <- coef(m_cognitive)
coefs_physical <- coef(m_physical)

vcov_simple <- vcov(m_simple)
vcov_full <- vcov(m_full)

# ==============================================================================
# RESUMEN DE MODELOS
# ==============================================================================

message("\n>>> RESUMEN DE MODELOS <<<\n")

etable(m_simple, m_only_nest, m_additive, m_full,
       headers = c("Domain Only", "Nestedness Only", "Additive", "Interaction"),
       fitstat = c("pr2", "bic", "n"))

message("\n>>> Modelos estratificados <<<\n")

etable(m_cognitive, m_physical,
       headers = c("Cognitive Only", "Physical Only"),
       keep = c("nestedness", "delta_up", "delta_down"),
       fitstat = c("pr2", "n"))

# ==============================================================================
# GUARDAR MODELOS
# ==============================================================================

models_list <- list(
  m_simple = m_simple,
  m_full = m_full,
  m_only_nest = m_only_nest,
  m_additive = m_additive,
  m_cognitive = m_cognitive,
  m_physical = m_physical,
  params = list(
    base_rate = base_rate,
    struct_dist_median = struct_dist_median,
    base_eta = base_eta
  )
)

#saveRDS(models_list, file.path(output_data_dir, "models_estimated.rds"))

message("\n>>> 02_estimate_models. R completado <<<")
gc()
