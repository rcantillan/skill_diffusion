# ==============================================================================
# 01_SETUP_FUNCTIONS. R
# Configuración inicial y funciones auxiliares para análisis ATC
# ==============================================================================

# Limpiar ambiente
#rm(list = ls())
gc()

# ==============================================================================
# LIBRERÍAS
# ==============================================================================

library(data.table)
library(fixest)
library(ggplot2)
library(scales)
library(patchwork)
library(MatchIt)
library(cobalt)

# ==============================================================================
# CONFIGURACIÓN
# ==============================================================================

# Directorio de salida (ajustar según tu estructura)
#output_data_dir <- "output/figures"
#output_tables_dir <- "output/tables"

# Crear directorios si no existen
#dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)
#dir.create(output_tables_dir, recursive = TRUE, showWarnings = FALSE)

# Semilla para reproducibilidad
set.seed(42)

# ==============================================================================
# PALETAS DE COLORES
# ==============================================================================

pal_domain <- c(Cognitive = "#1f77b4", Physical = "#D55E00")
pal_nest <- c(Low = "#E69F00", Mid = "#009E73", High = "#0072B2")

# ==============================================================================
# FUNCIÓN: calc_lp (Linear Predictor)
# ==============================================================================

calc_lp <- function(delta_up, delta_down, domain, nestedness, coefs, struct_dist) {
  
  lp <- 0
  
  # Términos base
  if ("delta_up_wage" %in% names(coefs)) {
    lp <- lp + coefs["delta_up_wage"] * delta_up
  }
  if ("delta_down_wage" %in% names(coefs)) {
    lp <- lp + coefs["delta_down_wage"] * delta_down
  }
  if ("structural_distance" %in% names(coefs)) {
    lp <- lp + coefs["structural_distance"] * struct_dist
  }
  
  # Domain Physical
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
  
  # Nestedness Mid
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
  
  # Nestedness High
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
# FUNCIÓN: calc_se (Standard Errors via Delta Method)
# ==============================================================================

calc_se <- function(delta_up, delta_down, domain, nestedness, vcov_m, struct_dist) {
  
  coef_names <- colnames(vcov_m)
  grad <- setNames(rep(0, length(coef_names)), coef_names)
  
  # Términos base
  if ("delta_up_wage" %in% coef_names) grad["delta_up_wage"] <- delta_up
  if ("delta_down_wage" %in% coef_names) grad["delta_down_wage"] <- delta_down
  if ("structural_distance" %in% coef_names) grad["structural_distance"] <- struct_dist
  
  # Domain Physical
  if (domain == "Physical") {
    if ("domainPhysical" %in% coef_names) grad["domainPhysical"] <- 1
    if ("delta_up_wage:domainPhysical" %in% coef_names) {
      grad["delta_up_wage:domainPhysical"] <- delta_up
    }
    if ("domainPhysical:delta_down_wage" %in% coef_names) {
      grad["domainPhysical:delta_down_wage"] <- delta_down
    }
    if ("domainPhysical:structural_distance" %in% coef_names) {
      grad["domainPhysical:structural_distance"] <- struct_dist
    }
  }
  
  # Nestedness Mid
  if (nestedness == "Mid") {
    if ("nestedness_tercileMid" %in% coef_names) grad["nestedness_tercileMid"] <- 1
    if ("delta_up_wage:nestedness_tercileMid" %in% coef_names) {
      grad["delta_up_wage:nestedness_tercileMid"] <- delta_up
    }
    if ("nestedness_tercileMid:delta_down_wage" %in% coef_names) {
      grad["nestedness_tercileMid:delta_down_wage"] <- delta_down
    }
    if (domain == "Physical") {
      if ("domainPhysical:nestedness_tercileMid" %in% coef_names) {
        grad["domainPhysical:nestedness_tercileMid"] <- 1
      }
      if ("delta_up_wage:domainPhysical:nestedness_tercileMid" %in% coef_names) {
        grad["delta_up_wage:domainPhysical:nestedness_tercileMid"] <- delta_up
      }
      if ("domainPhysical:nestedness_tercileMid:delta_down_wage" %in% coef_names) {
        grad["domainPhysical:nestedness_tercileMid:delta_down_wage"] <- delta_down
      }
    }
  }
  
  # Nestedness High
  if (nestedness == "High") {
    if ("nestedness_tercileHigh" %in% coef_names) grad["nestedness_tercileHigh"] <- 1
    if ("delta_up_wage:nestedness_tercileHigh" %in% coef_names) {
      grad["delta_up_wage:nestedness_tercileHigh"] <- delta_up
    }
    if ("nestedness_tercileHigh:delta_down_wage" %in% coef_names) {
      grad["nestedness_tercileHigh:delta_down_wage"] <- delta_down
    }
    if (domain == "Physical") {
      if ("domainPhysical:nestedness_tercileHigh" %in% coef_names) {
        grad["domainPhysical:nestedness_tercileHigh"] <- 1
      }
      if ("delta_up_wage:domainPhysical:nestedness_tercileHigh" %in% coef_names) {
        grad["delta_up_wage:domainPhysical:nestedness_tercileHigh"] <- delta_up
      }
      if ("domainPhysical:nestedness_tercileHigh:delta_down_wage" %in% coef_names) {
        grad["domainPhysical:nestedness_tercileHigh:delta_down_wage"] <- delta_down
      }
    }
  }
  
  se <- tryCatch({
    sqrt(as.numeric(t(grad) %*% vcov_m %*% grad))
  }, error = function(e) 0.1)
  
  return(se)
}

# ==============================================================================
# FUNCIÓN: calc_lp_stratified (para modelos estratificados)
# ==============================================================================

calc_lp_stratified <- function(delta_up, delta_down, nestedness, coefs, struct_dist) {
  
  lp <- coefs["delta_up_wage"] * delta_up + 
    coefs["delta_down_wage"] * delta_down +
    coefs["structural_distance"] * struct_dist
  
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
  }
  
  return(lp)
}

message(">>> 01_setup_functions.R cargado exitosamente <<<")
