# ==============================================================================
# 00_MASTER.R
# Script maestro para ejecutar todo el análisis
# ==============================================================================

message("\n")
message("================================================================")
message("ANÁLISIS ATC - NATURE HUMAN BEHAVIOUR")
message("================================================================")
message("Fecha: ", Sys.Date())
message("================================================================")

# ==============================================================================
# CONFIGURACIÓN INICIAL
# ==============================================================================

# Limpiar ambiente
rm(list = ls())
gc()

# Directorio de trabajo (ajustar según tu estructura)
# setwd("tu/directorio/de/trabajo")

# Directorios de salida
output_data_dir <- "output/figures"
output_tables_dir <- "output/tables"

dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_tables_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

message("\n>>> Cargando datos <<<")

# Aquí debes cargar tu dt_model
# dt_model <- readRDS("path/to/your/dt_model.rds")
# O la forma en que tengas tus datos

message("Datos cargados: ", format(nrow(dt_model), big.mark = ","), " observaciones")

# ==============================================================================
# EJECUTAR SCRIPTS
# ==============================================================================

message("\n>>> Ejecutando scripts <<<\n")

# 1. Setup y funciones
source("01_setup_functions.R")

# 2. Estimar modelos
source("02_estimate_models. R")

# 3. CEM Matching
source("03_cem_matching.R")

# 4. Figuras main text
source("04_figures_main.R")

# 5.  Supplementary materials
source("05_supplementary.R")

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

message("\n")
message("================================================================")
message("ANÁLISIS COMPLETADO")
message("================================================================")
message