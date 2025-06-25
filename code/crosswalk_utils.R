# ============================================================================
# UTILIDADES PARA OPTIMIZACIÓN DE CROSSWALKS
# Archivo: crosswalk_utils.R
# ============================================================================

library(data.table)
library(readxl)
library(dplyr)

# --- 1. FUNCIONES DE ESTANDARIZACIÓN DE CÓDIGOS ---

# Función mejorada para estandarizar códigos SOC
# Función simplificada para estandarizar códigos SOC
format_soc_code <- function(code, format_type = "standard") {
  # Manejar vectores elemento por elemento
  if (length(code) > 1) {
    result <- character(length(code))
    for (i in 1:length(code)) {
      result[i] <- format_soc_code(code[i], format_type)
    }
    return(result)
  }
  
  # Para un solo elemento
  if (is.na(code)) return(NA_character_)
  
  code_str <- as.character(code)
  if (code_str == "" || is.null(code_str)) return(NA_character_)
  
  # Limpiar código
  code_clean <- trimws(code_str)
  
  # Formatear según tipo
  if (format_type == "onet") {
    # Códigos O*NET-SOC
    if (grepl("^[0-9]{2}-[0-9]{4}", code_clean)) {
      return(substr(code_clean, 1, 7))
    } else if (grepl("^[0-9]{2}\\.[0-9]{4}", code_clean)) {
      return(gsub("\\.", "-", substr(code_clean, 1, 7)))
    } else if (grepl("^[0-9]{6}", code_clean)) {
      return(paste0(substr(code_clean, 1, 2), "-", substr(code_clean, 3, 6)))
    }
  } else if (format_type == "census") {
    # Códigos Census OCC
    code_digits <- gsub("[^0-9]", "", code_clean)
    if (nchar(code_digits) > 0 && nchar(code_digits) <= 4) {
      return(sprintf("%04d", as.integer(code_digits)))
    }
  } else {
    # Códigos SOC estándar
    if (grepl("^[0-9]{2}-[0-9]{4}", code_clean)) {
      return(substr(code_clean, 1, 7))
    } else if (grepl("^[0-9]{6}", code_clean)) {
      return(paste0(substr(code_clean, 1, 2), "-", substr(code_clean, 3, 6)))
    }
  }
  
  return(NA_character_)
}


# Función para cargar y preparar crosswalks con logging
load_crosswalk <- function(file_path, type, col_specs) {
  message(paste0("Cargando crosswalk: ", basename(file_path), " (", type, ")"))
  
  if (!file.exists(file_path)) {
    stop(paste0("Archivo no encontrado: ", file_path))
  }
  
  # Cargar según tipo de archivo
  if (grepl("\\.xlsx$|\\.xls$", file_path)) {
    # Determinar qué hoja usar
    sheet_names <- excel_sheets(file_path)
    sheet_to_use <- 1  # Default a primera hoja
    
    # Para crosswalks específicos, ajustar la hoja
    if (type == "occ2010_to_soc2010" && "Crosswalk" %in% sheet_names) {
      sheet_to_use <- "Crosswalk"
    } else if (type == "soc2010_to_soc2018" && length(sheet_names) > 0) {
      sheet_to_use <- sheet_names[1]  # Primera hoja para este crosswalk
    }
    
    tryCatch({
      raw_data <- read_excel(file_path, sheet = sheet_to_use, na = c("", "NA", "#N/A"))
      crosswalk_dt <- as.data.table(raw_data)
    }, error = function(e) {
      message(paste0("Error al cargar Excel: ", e$message))
      # Intentar con otra configuración si falla
      raw_data <- read_excel(file_path, sheet = sheet_to_use, na = c("", "NA", "#N/A"), skip = 1)
      crosswalk_dt <- as.data.table(raw_data)
    })
  } else if (grepl("\\.csv$", file_path)) {
    crosswalk_dt <- fread(file_path, na.strings = c("", "NA", "#N/A"))
  } else {
    stop(paste0("Formato de archivo no soportado: ", file_path))
  }
  
  # Verificar que las columnas necesarias existen
  required_cols <- names(col_specs)
  missing_cols <- setdiff(required_cols, names(crosswalk_dt))
  
  if (length(missing_cols) > 0) {
    # Intentar encontrar columnas por nombres alternativos
    for (col in missing_cols) {
      alt_names <- col_specs[[col]]$alt_names
      if (!is.null(alt_names)) {
        for (alt_name in alt_names) {
          if (alt_name %in% names(crosswalk_dt)) {
            message(paste0("Usando columna alternativa '", alt_name, "' para '", col, "'"))
            setnames(crosswalk_dt, alt_name, col)
            missing_cols <- setdiff(missing_cols, col)
            break
          }
        }
      }
    }
  }
  
  if (length(missing_cols) > 0) {
    stop(paste0("Columnas requeridas no encontradas: ", paste(missing_cols, collapse = ", ")))
  }
  
  # Seleccionar y renombrar columnas según especificaciones
  cols_to_select <- c()
  new_names <- c()
  
  for (col in names(col_specs)) {
    if (col %in% names(crosswalk_dt)) {
      cols_to_select <- c(cols_to_select, col)
      new_names <- c(new_names, col_specs[[col]]$new_name)
    }
  }
  
  result_dt <- crosswalk_dt[, ..cols_to_select]
  setnames(result_dt, cols_to_select, new_names)
  
  # Aplicar formato a códigos según tipo
  for (col in names(result_dt)) {
    format_type <- if (grepl("onet", col, ignore.case = TRUE)) "onet" else 
      if (grepl("census|occ[0-9]", col, ignore.case = TRUE)) "census" else "standard"
    
    # Solo aplicar a columnas que contienen códigos SOC/ONET
    if (grepl("code|soc|onet", col, ignore.case = TRUE)) {
      result_dt[, (col) := format_soc_code(.SD[[col]], format_type = format_type), .SDcols = col]
    }
  }
  
  # Eliminar filas con NA en columnas clave
  for (col in names(result_dt)) {
    result_dt <- result_dt[!is.na(get(col))]
  }
  
  # Eliminar duplicados
  result_dt <- unique(result_dt)
  
  message(paste0("Crosswalk preparado con ", nrow(result_dt), " filas únicas"))
  return(result_dt)
}

# Especificaciones de columnas para cada crosswalk
occ2010_to_soc2010_specs <- list(
  "2010 Census Code" = list(new_name = "OCC2010_Code", alt_names = c("Census Code", "OCC", "OCC Code", "OCC2010")),
  "SOC Code" = list(new_name = "SOC2010_Code", alt_names = c("2010 SOC Code", "SOC", "SOC 2010"))
)

soc2010_to_soc2018_specs <- list(
  "2010 SOC Code" = list(new_name = "SOC2010_Code", alt_names = c("SOC 2010", "2010_SOC_Code")),
  "2018 SOC Code" = list(new_name = "SOC2018_Code", alt_names = c("SOC 2018", "2018_SOC_Code"))
)

onet2010_to_onet2019_specs <- list(
  "O*NET-SOC 2010 Code" = list(new_name = "ONETSOC_2010_Code", alt_names = c("O*NET-SOC 2010", "ONET_SOC_2010")),
  "O*NET-SOC 2019 Code" = list(new_name = "ONETSOC_2019_Code", alt_names = c("O*NET-SOC 2019", "ONET_SOC_2019"))
)
