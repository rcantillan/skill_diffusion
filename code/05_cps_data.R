# --- 0. CONFIGURACIÓN INICIAL Y CARGA DE PAQUETES ---
# (Igual que antes)
library(data.table)
library(readxl)
library(dplyr)
library(ipumsr) # Asegúrate de que esté cargado si vas a cargar cps_data aquí

# --- =======================================================================
# --- PARTE 1: CARGA/SIMULACIÓN DE DATOS DE O*NET (AHORA diffusion_events)
# --- =======================================================================
# (Igual que antes - asegúrate de que diffusion_events esté cargada o simulada)
if (!exists("diffusion_events") || !is.data.table(diffusion_events) || nrow(diffusion_events) == 0) {
  warning("La tabla 'diffusion_events' no existe o está vacía. Creando una tabla SIMULADA. ¡USA TUS DATOS REALES!", immediate. = TRUE)
  diffusion_events <- data.table(
    source = c("11-1011.00", "11-1021.00", "13-1111.00", "11-1011.03", "11-2022.00"),
    target = c("11-1021.00", "13-1111.00", "15-1131.00", "11-1011.00", "11-9199.00"),
    skill_name = rep("Algun Skill Ejemplo", 5),
    diffusion = sample(0:1, 5, replace = TRUE)
    # Añade aquí otras columnas que esperas en diffusion_events, como structural_distance, etc.
    # Ejemplo: structural_distance = runif(5, 0, 1)
  )
  diffusion_events[, source := as.character(source)]
  diffusion_events[, target := as.character(target)]
}
setDT(diffusion_events) # Asegurar que es data.table

# --- =======================================================================
# --- PARTE 2: CARGA/SIMULACIÓN Y PROCESAMIENTO DE DATOS CPS PARA FLUJO DE TRABAJADORES
# --- =======================================================================

# 3. Define las rutas a tus archivos
# (Asegúrate de que los nombres de archivo coincidan exactamente con los que descargaste)
ruta_archivo_dat <- "/home/rober/Descargas/cps_00001.dat"
ruta_archivo_xml_ddi <- "/home/rober/Descargas/cps_00001.xml"

# 4. Verifica que AMBOS archivos existan en las rutas especificadas
if (!file.exists(ruta_archivo_dat)) {
  stop(paste("¡Archivo .dat NO encontrado en!", ruta_archivo_dat))
}
if (!file.exists(ruta_archivo_xml_ddi)) {
  stop(paste("¡Archivo .xml DDI NO encontrado en!", ruta_archivo_xml_ddi))
}

# 5. Lee los datos usando el archivo DDI y el archivo .dat
# La función read_ipums_micro usa la metadata del DDI para interpretar correctamente el .dat
# El argumento 'data_file' es opcional si el .dat tiene el mismo nombre base y está en el mismo directorio que el DDI.
# Pero es buena práctica especificarlo.
tryCatch({
  # La función devolverá un data.table
  cps_data <- read_ipums_micro(
    ddi = ruta_archivo_xml_ddi,
    data_file = ruta_archivo_dat,
    verbose = TRUE # verbose = TRUE te dará más información durante el proceso de carga
  )
  
  # 6. ¡Verifica tus datos!
  print("¡Datos leídos exitosamente con ipumsr!")
  
  # Muestra las primeras filas del data.table
  print("Primeras filas de los datos cargados (cps_data):")
  print(head(cps_data))
  
  # Muestra los nombres de las columnas (variables)
  print("Nombres de las columnas (variables):")
  print(colnames(cps_data))
  
  # Muestra las dimensiones del data.table (filas, columnas)
  print("Dimensiones del data.table (filas, columnas):")
  print(dim(cps_data))
  
  # Puedes ver un resumen de la estructura si quieres
  # print("Estructura del data.table:")
  # str(cps_data) # Puede ser muy largo si hay muchas variables
  
  # Un buen resumen si tienes muchas columnas:
  if (requireNamespace("dplyr", quietly = TRUE)) {
    library(dplyr)
    print("Un vistazo rápido a los datos (glimpse):")
    glimpse(cps_data)
  }
  
}, error = function(e) {
  message("Ocurrió un error al leer los datos con ipumsr:")
  message(e$message)
  message("Sugerencias:")
  message("- Verifica que las rutas a los archivos .dat y .xml sean absolutamente correctas.")
  message("- Asegúrate de que el archivo .xml DDI que descargaste corresponde exactamente a la extracción de datos del archivo .dat.")
  message("- Si el error persiste, el mensaje de error de R a veces da pistas sobre qué pudo haber salido mal (por ejemplo, problemas de memoria si el archivo es muy grande).")
})

glimpse(cps_data)

setDT(cps_data); gc()

# --- PARÁMETROS IMPORTANTES PARA EL PROCESAMIENTO DE CPS (¡REVISA Y AJUSTA!) ---
MIN_YEAR_DATA_CPS     <- 2015
MAX_YEAR_DATA_CPS     <- 2023 
TARGET_MONTH_CPS      <- 3   
PERSON_WEIGHT_VAR_CPS   <- "WTFINL" 
VALID_EMP_CODES_CPS     <- c(10, 12)
MIN_VALID_OCC_CPS       <- 1   
MAX_VALID_OCC_CPS       <- 9899 

if (!(PERSON_WEIGHT_VAR_CPS %in% names(cps_data))) {
  stop(paste0("La variable de peso especificada ('", PERSON_WEIGHT_VAR_CPS, "') no se encuentra en cps_data."))
}

# --- PASO 2.1: SUBSELECCIÓN INICIAL DE DATOS DE CPS ---
message(paste0("Paso 2.1: Filtrando datos CPS para años ", MIN_YEAR_DATA_CPS, "-", MAX_YEAR_DATA_CPS, " y mes ", TARGET_MONTH_CPS, "..."))
cps_subset_longitudinal <- cps_data[
  YEAR >= MIN_YEAR_DATA_CPS & YEAR <= MAX_YEAR_DATA_CPS & MONTH == TARGET_MONTH_CPS,
  c("YEAR", "CPSIDP", "EMPSTAT", "OCC2010", PERSON_WEIGHT_VAR_CPS),
  with = FALSE
]
setnames(cps_subset_longitudinal, old = PERSON_WEIGHT_VAR_CPS, new = "SELECTED_WEIGHT")

# --- *** NUEVA SECCIÓN DE LIMPIEZA DE OCC2010 ANTES DE CUALQUIER OTRA COSA *** ---
message("Limpiando y formateando OCC2010 en cps_subset_longitudinal...")
cps_subset_longitudinal[, OCC2010_clean := trimws(as.character(OCC2010))]
cps_subset_longitudinal[, OCC2010_clean := sub("\\.0+$", "", OCC2010_clean)] 
cps_subset_longitudinal[, OCC2010_clean := gsub("[^0-9]", "", OCC2010_clean)] 
cps_subset_longitudinal[grepl("^[0-9]{1,4}$", OCC2010_clean) & OCC2010_clean != "", 
                        OCC2010_clean := sprintf("%04s", as.integer(OCC2010_clean))]
cps_subset_longitudinal[grepl("^[0-9]{4}$", OCC2010_clean), OCC2010 := OCC2010_clean]
cps_subset_longitudinal[!grepl("^[0-9]{4}$", OCC2010), OCC2010 := "99999"] 

message(paste0("Dimensiones de cps_subset_longitudinal después de limpieza OCC2010: ", nrow(cps_subset_longitudinal), "x", ncol(cps_subset_longitudinal)))
if (nrow(cps_subset_longitudinal) == 0) stop("cps_subset_longitudinal está vacío. Verifica filtros.")

# --- PASO 2.2: ASEGURAR UNICIDAD POR PERSONA-AÑO ---
message("Paso 2.2: Asegurando unicidad por (CPSIDP, YEAR)...")
if (anyDuplicated(cps_subset_longitudinal, by = c("CPSIDP", "YEAR"))) {
  message("Duplicados encontrados en (CPSIDP, YEAR). Tomando la primera observación por grupo.")
  cps_subset_unique <- cps_subset_longitudinal[, .SD[1], by = .(CPSIDP, YEAR)]
} else {
  message("No se encontraron duplicados en (CPSIDP, YEAR).")
  cps_subset_unique <- cps_subset_longitudinal
}
message(paste0("Dimensiones de cps_subset_unique: ", nrow(cps_subset_unique), "x", ncol(cps_subset_unique)))

message("Paso 2.3: Preparando tablas para emparejamiento (origen y destino)...")
data_t <- cps_subset_unique[, .(CPSIDP, YEAR_t = YEAR, EMPSTAT_t = EMPSTAT, OCC2010_t = OCC2010, WEIGHT_t = SELECTED_WEIGHT)]
data_t_plus_1 <- cps_subset_unique[, .(CPSIDP, YEAR_t1 = YEAR, EMPSTAT_t1 = EMPSTAT, OCC2010_t1 = OCC2010)]
data_t_plus_1[, YEAR_t_for_link := YEAR_t1 - 1]

message("Paso 2.4: Realizando el merge longitudinal...")
transitions_raw <- merge(
  data_t, data_t_plus_1,
  by.x = c("CPSIDP", "YEAR_t"), by.y = c("CPSIDP", "YEAR_t_for_link"),
  all = FALSE, allow.cartesian = FALSE
)
message(paste0("Dimensiones de 'transitions_raw': ", nrow(transitions_raw), "x", ncol(transitions_raw)))
if (nrow(transitions_raw) == 0) stop("'transitions_raw' vacío. Revisa lógica de emparejamiento.")

message("Paso 2.5: Filtrando transiciones válidas...")
transitions_raw[, OCC2010_t_num := suppressWarnings(as.numeric(OCC2010_t))]
transitions_raw[, OCC2010_t1_num := suppressWarnings(as.numeric(OCC2010_t1))]

transitions_filtered <- transitions_raw[
  EMPSTAT_t %in% VALID_EMP_CODES_CPS & EMPSTAT_t1 %in% VALID_EMP_CODES_CPS &
    OCC2010_t_num >= MIN_VALID_OCC_CPS & OCC2010_t_num <= MAX_VALID_OCC_CPS &
    OCC2010_t1_num >= MIN_VALID_OCC_CPS & OCC2010_t1_num <= MAX_VALID_OCC_CPS &
    !is.na(OCC2010_t_num) & !is.na(OCC2010_t1_num)
]
transitions_filtered[, OCC_CHANGE := (OCC2010_t != OCC2010_t1)]
message(paste0("Dimensiones de 'transitions_filtered': ", nrow(transitions_filtered), "x", ncol(transitions_filtered)))
if (nrow(transitions_filtered) == 0) stop("'transitions_filtered' vacío. Revisa filtros EMPSTAT/OCC2010.")

transitions_filtered_weighted <- transitions_filtered[!is.na(WEIGHT_t) & WEIGHT_t > 0]
message(paste0("Dimensiones de 'transitions_filtered_weighted': ", nrow(transitions_filtered_weighted), "x", ncol(transitions_filtered_weighted)))

message("Paso 2.6A: Calculando flujo anual AGREGADO PONDERADO...")
worker_flows_total_weighted_agg <- transitions_filtered_weighted[
  OCC_CHANGE == TRUE,
  .(Agg_Total_Flow_Weighted = sum(WEIGHT_t, na.rm = TRUE)),
  by = .(SOC_Origen_OCC2010 = OCC2010_t, SOC_Destino_OCC2010 = OCC2010_t1)
][order(-Agg_Total_Flow_Weighted)]

message("Paso 2.6B: Calculando flujo anual AGREGADO NO PONDERADO...")
worker_flows_counts_unweighted_agg <- transitions_filtered[
  OCC_CHANGE == TRUE,
  .(Agg_Count_Flow_Unweighted = .N),
  by = .(SOC_Origen_OCC2010 = OCC2010_t, SOC_Destino_OCC2010 = OCC2010_t1)
][order(-Agg_Count_Flow_Unweighted)]

message("Paso 2.7: Calculando probabilidades de transición agregadas (ponderadas)...")
total_from_origin_agg_weighted <- transitions_filtered_weighted[
  ,
  .(Agg_Total_Origin_Pop_Weighted = sum(WEIGHT_t, na.rm = TRUE)),
  by = .(SOC_Origen_OCC2010 = OCC2010_t)
]
worker_transition_probabilities_agg <- merge(
  worker_flows_total_weighted_agg, total_from_origin_agg_weighted,
  by = "SOC_Origen_OCC2010", all.x = TRUE
)
worker_transition_probabilities_agg[, Prob_Transicion_Agg_Weighted := ifelse(Agg_Total_Origin_Pop_Weighted > 0,
                                                                             Agg_Total_Flow_Weighted / Agg_Total_Origin_Pop_Weighted, 0)]
worker_transition_probabilities_agg <- worker_transition_probabilities_agg[order(-Agg_Total_Flow_Weighted)]

rm(cps_subset_longitudinal, cps_subset_unique, data_t, data_t_plus_1, transitions_raw, 
   transitions_filtered, transitions_filtered_weighted, total_from_origin_agg_weighted)
gc()

# --- =======================================================================
# --- PARTE 3: CROSSWALKS Y FUSIÓN FINAL
# --- =======================================================================
message("Iniciando Parte 3: Aplicación de Crosswalks y Fusión Final.")

# --- Función de ayuda para formatear códigos SOC a "XX-XXXX" ---
format_soc_code_func <- function(code) {
  if (is.na(code) || tolower(trimws(code)) %in% c("none", "na", "") ) return(NA_character_)
  code_clean <- trimws(as.character(code))
  code_clean <- gsub("[^0-9-]", "", code_clean) 
  
  if (grepl("^[0-9]{2}-?[0-9]{4}$", code_clean)) {
    return(paste0(substr(gsub("-", "", code_clean), 1, 2), "-", substr(gsub("-", "", code_clean), 3, 6)))
  }
  if (grepl("^[0-9]{6}$", code_clean)) { 
    return(paste0(substr(code_clean, 1, 2), "-", substr(code_clean, 3, 6)))
  }
  return(NA_character_) 
}

# --- PASO 3A: ESTANDARIZAR CÓDIGOS O*NET EN 'diffusion_events' A O*NET-SOC 2019 Y LUEGO A SOC BASE (XX-XXXX) ---
message("Paso 3A: Estandarizando códigos O*NET en diffusion_events...")

ruta_crosswalk_onet10_onet19 <- "/home/rober/Descargas/2010_to_2019_Crosswalk.csv" 
col_onet10_en_onet_cw <- "O*NET-SOC 2010 Code" 
col_onet19_en_onet_cw <- "O*NET-SOC 2019 Code"

dt_onet2010_to_onet2019 <- NULL
if (file.exists(ruta_crosswalk_onet10_onet19)) {
  tryCatch({
    dt_onet2010_to_onet2019 <- fread(ruta_crosswalk_onet10_onet19, stringsAsFactors = FALSE, colClasses = "character")
    if (!all(c(col_onet10_en_onet_cw, col_onet19_en_onet_cw) %in% names(dt_onet2010_to_onet2019))) {
      stop(paste0("Columnas ('",col_onet10_en_onet_cw,"', '",col_onet19_en_onet_cw,"') no encontradas en crosswalk O*NET 2010 a 2019."))
    }
    dt_onet2010_to_onet2019 <- dt_onet2010_to_onet2019[, .(
      ONETSOC_2010_Code = get(col_onet10_en_onet_cw),
      ONETSOC_2019_Code = get(col_onet19_en_onet_cw)
    )]
    dt_onet2010_to_onet2019 <- unique(dt_onet2010_to_onet2019)
    message(paste0("Crosswalk O*NET-SOC 2010 a O*NET-SOC 2019 cargado. Mapeos únicos: ", nrow(dt_onet2010_to_onet2019)))
  }, error = function(e) {
    message(paste("Error cargando crosswalk O*NET 2010 a 2019:", e$message))
    dt_onet2010_to_onet2019 <- NULL
  })
} else {
  warning(paste("Crosswalk O*NET 2010-2019 NO encontrado en:", ruta_crosswalk_onet10_onet19, ". Los códigos 'source' podrían no estandarizarse."), immediate. = TRUE)
}

if (!exists("diffusion_events") || !is.data.table(diffusion_events)) {
  stop("La tabla 'diffusion_events' no existe. Cárgala o ejecútala primero.")
}

diffusion_events[, source := as.character(source)]
diffusion_events[, target := as.character(target)] 
diffusion_events[, source_original_onet_format := source] 
diffusion_events[, target_original_onet_format := target]

if (!is.null(dt_onet2010_to_onet2019) && nrow(dt_onet2010_to_onet2019) > 0) {
  diffusion_events <- merge(
    diffusion_events, dt_onet2010_to_onet2019,
    by.x = "source", 
    by.y = "ONETSOC_2010_Code",
    all.x = TRUE
  )
  diffusion_events[!is.na(ONETSOC_2019_Code), source := ONETSOC_2019_Code] 
  diffusion_events[, ONETSOC_2019_Code := NULL] 
  message("Columna 'source' en diffusion_events (O*NET 2015) actualizada a O*NET-SOC 2019 (donde fue posible).")
} else {
  message("No se aplicó el crosswalk O*NET 2010->2019 a 'diffusion_events$source'. Asumiendo que 'source' ya está en O*NET-SOC 2019 o es comparable.")
}

extract_base_soc_code_func <- function(onet_soc_code) {
  if (is.na(onet_soc_code)) return(NA_character_)
  match_val <- regexpr("^[0-9]{2}-[0-9]{4}", onet_soc_code)
  if (match_val != -1) {
    return(substr(onet_soc_code, match_val, match_val + attr(match_val, "match.length") - 1))
  }
  return(NA_character_) 
}
diffusion_events[, source_soc_std := sapply(source, extract_base_soc_code_func)]
diffusion_events[, target_soc_std := sapply(target, extract_base_soc_code_func)]

# Crear diffusion_events_clean para el merge, preservando la diffusion_events original hasta el final si se desea
# O, si diffusion_events debe ser la tabla que se modifica en cada paso, entonces no es necesario _clean
# Por ahora, mantendré la lógica de crear una versión limpia para el merge.
diffusion_events_clean <- diffusion_events[!is.na(source_soc_std) & !is.na(target_soc_std)]
message(paste0("Códigos O*NET en diffusion_events estandarizados a SOC base. Filas restantes en la versión limpia: ", nrow(diffusion_events_clean)))
if (nrow(diffusion_events_clean) > 0) {
  print(head(diffusion_events_clean[,.(source_original_onet_format, source_soc_std, target_original_onet_format, target_soc_std)], 3))
}


# --- PASO 3B: APLICAR CROSSWALKS PARA DATOS DE FLUJO CPS (OCC2010 -> SOC2010 -> SOC2018) ---
message("Paso 3B: Mapeando códigos OCC2010 de flujos CPS a SOC O*NET (SOC 2018)...")

ruta_crosswalk_occ2010_to_soc2010 <- "/home/rober/Descargas/2010-occ-codes-with-crosswalk-from-2002-2011.xls"
dt_occ2010_to_soc2010 <- NULL
if (file.exists(ruta_crosswalk_occ2010_to_soc2010)) {
  tryCatch({
    if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl"); library(readxl)
    filas_a_omitir_census <- 5 
    dt_raw_census <- as.data.table(read_excel(ruta_crosswalk_occ2010_to_soc2010, skip = filas_a_omitir_census, col_names = FALSE))
    
    col_idx_occ2010_census <- 3 
    col_idx_soc2010_census <- 4 
    
    dt_occ2010_to_soc2010 <- dt_raw_census[, .(
      OCC2010_Code_Raw = trimws(as.character(get(paste0("...", col_idx_occ2010_census)))),
      SOC2010_Code_For_Merge = trimws(as.character(get(paste0("...", col_idx_soc2010_census))))
    )]
    dt_occ2010_to_soc2010[, OCC2010_Code_Raw := sub("\\.0+$", "", OCC2010_Code_Raw)]
    dt_occ2010_to_soc2010[, OCC2010_Code_Raw := gsub("[^0-9]", "", OCC2010_Code_Raw)]
    dt_occ2010_to_soc2010 <- dt_occ2010_to_soc2010[nchar(OCC2010_Code_Raw) > 0 & nchar(OCC2010_Code_Raw) <= 4]
    dt_occ2010_to_soc2010[, OCC2010_Code_Raw := sprintf("%04s", OCC2010_Code_Raw)]
    
    dt_occ2010_to_soc2010[!is.na(SOC2010_Code_For_Merge), SOC2010_Code_For_Merge := sapply(SOC2010_Code_For_Merge, format_soc_code_func)]
    dt_occ2010_to_soc2010 <- unique(dt_occ2010_to_soc2010[!is.na(OCC2010_Code_Raw) & grepl("^[0-9]{4}$", OCC2010_Code_Raw) & !is.na(SOC2010_Code_For_Merge)])
    message(paste0("Crosswalk OCC2010 a SOC2010 cargado. Mapeos válidos: ", nrow(dt_occ2010_to_soc2010)))
  }, error = function(e){message(paste0("Error cargando crosswalk OCC2010->SOC2010: ", e$message)); dt_occ2010_to_soc2010 <- NULL})
} else {
  warning(paste("Crosswalk OCC2010->SOC2010 NO encontrado. Se usará SIMULACIÓN."), immediate. = TRUE)
}

ruta_crosswalk_soc2010_soc2018 <- "/home/rober/Descargas/soc_2010_to_2018_crosswalk.xlsx"
dt_intermediate_crosswalk_soc2010_to_soc2018 <- NULL
if (file.exists(ruta_crosswalk_soc2010_soc2018)) {
  tryCatch({
    if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl"); library(readxl)
    skip_rows_bls <- 7 
    dt_raw_bls <- as.data.table(read_excel(ruta_crosswalk_soc2010_soc2018, skip = skip_rows_bls, col_names = FALSE))
    
    col_idx_soc2010_bls <- 1 
    col_idx_soc2018_bls <- 3 
    
    dt_intermediate_crosswalk_soc2010_to_soc2018 <- dt_raw_bls[, .(
      SOC2010_Code = trimws(as.character(get(paste0("...", col_idx_soc2010_bls)))),
      SOC_ONET_Code = trimws(as.character(get(paste0("...", col_idx_soc2018_bls))))
    )]
    dt_intermediate_crosswalk_soc2010_to_soc2018[!is.na(SOC2010_Code), SOC2010_Code := sapply(SOC2010_Code, format_soc_code_func)]
    dt_intermediate_crosswalk_soc2010_to_soc2018[!is.na(SOC_ONET_Code), SOC_ONET_Code := sapply(SOC_ONET_Code, format_soc_code_func)]
    dt_intermediate_crosswalk_soc2010_to_soc2018 <- unique(dt_intermediate_crosswalk_soc2010_to_soc2018[!is.na(SOC2010_Code) & !is.na(SOC_ONET_Code)])
    message(paste0("Crosswalk SOC2010 a SOC2018 (para O*NET) cargado. Mapeos válidos: ", nrow(dt_intermediate_crosswalk_soc2010_to_soc2018)))
  }, error = function(e){message(paste("Error cargando crosswalk SOC2010->SOC2018:", e$message)); dt_intermediate_crosswalk_soc2010_to_soc2018 <- NULL})
} else {
  warning(paste("Crosswalk SOC2010->SOC2018 NO encontrado. Se usará SIMULACIÓN."), immediate. = TRUE)
}

final_cps_flow_data <- data.table() 
can_proceed_with_cps_mapping <- 
  !is.null(dt_occ2010_to_soc2010) && nrow(dt_occ2010_to_soc2010) > 0 &&
  !is.null(dt_intermediate_crosswalk_soc2010_to_soc2018) && nrow(dt_intermediate_crosswalk_soc2010_to_soc2018) > 0 &&
  exists("worker_transition_probabilities_agg") && nrow(worker_transition_probabilities_agg) > 0

if (can_proceed_with_cps_mapping) {
  message("Asegurando tipos para el primer merge (OCC2010 -> SOC2010)...")
  worker_transition_probabilities_agg[, SOC_Origen_OCC2010_char := sprintf("%04s", trimws(as.character(SOC_Origen_OCC2010)))]
  worker_transition_probabilities_agg[, SOC_Destino_OCC2010_char := sprintf("%04s", trimws(as.character(SOC_Destino_OCC2010)))]
  
  dt_occ2010_to_soc2010[, OCC2010_Code_Raw := as.character(OCC2010_Code_Raw)]
  dt_occ2010_to_soc2010[, SOC2010_Code_For_Merge := as.character(SOC2010_Code_For_Merge)]
  
  message("Mapeando flujos CPS: OCC2010 -> SOC2010...")
  flows_cps_soc2010_temp <- merge(
    worker_transition_probabilities_agg,
    dt_occ2010_to_soc2010, 
    by.x = "SOC_Origen_OCC2010_char", 
    by.y = "OCC2010_Code_Raw",
    all.x = TRUE,
    allow.cartesian = TRUE 
  )
  setnames(flows_cps_soc2010_temp, "SOC2010_Code_For_Merge", "SOC_Origen_2010", skip_absent = TRUE)
  
  flows_cps_soc2010_temp <- merge(
    flows_cps_soc2010_temp,
    dt_occ2010_to_soc2010, 
    by.x = "SOC_Destino_OCC2010_char", 
    by.y = "OCC2010_Code_Raw",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  setnames(flows_cps_soc2010_temp, "SOC2010_Code_For_Merge", "SOC_Destino_2010", skip_absent = TRUE)
  
  flows_valid_soc2010 <- flows_cps_soc2010_temp[!is.na(SOC_Origen_2010) & !is.na(SOC_Destino_2010)]
  message(paste0("Número de flujos después del mapeo OCC2010->SOC2010 (con SOC2010 válidos para ambos): ", nrow(flows_valid_soc2010)))
  
  if (nrow(flows_valid_soc2010) > 0) {
    message("Asegurando tipos para el segundo merge (SOC2010 -> SOC O*NET)...")
    flows_valid_soc2010[, SOC_Origen_2010 := as.character(SOC_Origen_2010)]
    flows_valid_soc2010[, SOC_Destino_2010 := as.character(SOC_Destino_2010)]
    dt_intermediate_crosswalk_soc2010_to_soc2018[, SOC2010_Code := as.character(SOC2010_Code)]
    dt_intermediate_crosswalk_soc2010_to_soc2018[, SOC_ONET_Code := as.character(SOC_ONET_Code)]   
    
    message("Mapeando flujos CPS: SOC2010 -> SOC O*NET (SOC2018)...")
    flows_cps_soc_onet_temp <- merge(
      flows_valid_soc2010,
      dt_intermediate_crosswalk_soc2010_to_soc2018, 
      by.x = "SOC_Origen_2010",   
      by.y = "SOC2010_Code",     
      all.x = TRUE,
      allow.cartesian = TRUE 
    )
    setnames(flows_cps_soc_onet_temp, "SOC_ONET_Code", "Source_SOC_ONET", skip_absent = TRUE)
    
    flows_cps_soc_onet_temp <- merge(
      flows_cps_soc_onet_temp,
      dt_intermediate_crosswalk_soc2010_to_soc2018,
      by.x = "SOC_Destino_2010", 
      by.y = "SOC2010_Code",     
      all.x = TRUE,
      allow.cartesian = TRUE
    )
    setnames(flows_cps_soc_onet_temp, "SOC_ONET_Code", "Target_SOC_ONET", skip_absent = TRUE)
    
    message("Mapeo a SOC O*NET (SOC2018) completado.")
    
    final_cps_flow_data <- flows_cps_soc_onet_temp[
      !is.na(Source_SOC_ONET) & !is.na(Target_SOC_ONET),
      .(
        Total_Flow_Weighted = sum(Agg_Total_Flow_Weighted, na.rm = TRUE),
        Total_Origin_Pop_Weighted = sum(Agg_Total_Origin_Pop_Weighted, na.rm = TRUE)
      ),
      by = .(Source_SOC_ONET, Target_SOC_ONET) 
    ]
    
    final_cps_flow_data[, Prob_Transicion_Weighted := ifelse(Total_Origin_Pop_Weighted > 0,
                                                             Total_Flow_Weighted / Total_Origin_Pop_Weighted,
                                                             0)]
    
    message(paste0("Pares de flujo CPS después del mapeo en dos pasos y agregación a SOC O*NET: ", nrow(final_cps_flow_data)))
    if (nrow(final_cps_flow_data) > 0) {
      print("Primeras 10 filas de datos de flujo CPS finales, mapeados y agregados a SOC O*NET (después de dos pasos):")
      print(head(final_cps_flow_data[order(-Total_Flow_Weighted)], 10))
    }
  } else {
    warning("No quedaron flujos válidos después del primer mapeo OCC2010 a SOC2010. 'final_cps_flow_data' estará vacía.", immediate. = TRUE)
  }
} else {
  warning("Uno o ambos crosswalks necesarios, o la tabla 'worker_transition_probabilities_agg', no están disponibles, están vacíos o no se cargaron correctamente. No se puede proceder con el mapeo de flujos CPS.", immediate. = TRUE)
}

# --- PASO 3C: UNIR DATOS DE FLUJO CPS (MAPEADOS) CON 'diffusion_events_clean' ---
message("Paso 3C: Uniendo datos de flujo CPS (mapeados) con 'diffusion_events_clean'...")
# La tabla final se llamará diffusion_events
# Si diffusion_events_clean no tiene filas, el merge resultará en 0 filas o error, así que manejamos eso.

if (exists("diffusion_events_clean") && is.data.table(diffusion_events_clean) && nrow(diffusion_events_clean) > 0 &&
    exists("final_cps_flow_data") && is.data.table(final_cps_flow_data) && nrow(final_cps_flow_data) > 0) {
  
  # Realizar el merge y asignar el resultado a diffusion_events
  diffusion_events <- merge(
    diffusion_events_clean, 
    final_cps_flow_data,
    by.x = c("source_soc_std", "target_soc_std"), 
    by.y = c("Source_SOC_ONET", "Target_SOC_ONET"), 
    all.x = TRUE 
  )
  
  cols_flujo_cps <- c("Total_Flow_Weighted", "Prob_Transicion_Weighted", "Total_Origin_Pop_Weighted")
  for (col_name in cols_flujo_cps) {
    if (col_name %in% names(diffusion_events)) {
      diffusion_events[is.na(get(col_name)), (col_name) := 0]
    } else { 
      diffusion_events[, (col_name) := 0]
    }
  }
  message("Merge final completado. La tabla 'diffusion_events' ha sido actualizada con datos de flujo CPS.")
  print("Primeras 3 filas de 'diffusion_events' después del merge final:")
  print(head(diffusion_events[,.(source_soc_std, target_soc_std, skill_name, diffusion, Total_Flow_Weighted, Prob_Transicion_Weighted)], 3))
  
  cols_to_summarize <- intersect(cols_flujo_cps, names(diffusion_events))
  if(length(cols_to_summarize) > 0){
    print("Resumen de columnas de flujo añadidas a 'diffusion_events':")
    print(summary(diffusion_events[, ..cols_to_summarize]))
  } else {
    print("No se encontraron columnas de flujo para resumir en 'diffusion_events'.")
  }
} else {
  warning("No se pudo realizar el merge final: 'diffusion_events_clean' o 'final_cps_flow_data' no disponibles, vacíos o con problemas. 'diffusion_events' podría no tener datos de flujo CPS o ser una copia de 'diffusion_events_clean'.", immediate. = TRUE)
  if(exists("diffusion_events_clean") && is.data.table(diffusion_events_clean)) { 
    # Si no hubo datos de flujo, diffusion_events será la versión limpia sin datos de flujo, pero con columnas placeholder
    if (!exists("diffusion_events") || nrow(diffusion_events) == 0) { # Si diffusion_events no fue actualizada
      diffusion_events <- copy(diffusion_events_clean) 
    }
    cols_flujo_cps_placeholder <- c("Total_Flow_Weighted", "Prob_Transicion_Weighted", "Total_Origin_Pop_Weighted")
    for(col_pl in cols_flujo_cps_placeholder){
      if(! (col_pl %in% names(diffusion_events)) ){
        diffusion_events[, (col_pl) := 0]
      } else {
        # Si la columna existe pero el merge falló y no se llenaron NAs con 0 antes
        diffusion_events[is.na(get(col_pl)), (col_pl) := 0]
      }
    }
    message("'diffusion_events' contiene los datos de O*NET procesados; se añadieron columnas de flujo con ceros debido a la falta de datos de flujo CPS o problemas en el merge.")
  } else if (!exists("diffusion_events_clean") && exists("diffusion_events") && is.data.table(diffusion_events)) {
    # Si diffusion_events_clean no se pudo crear, diffusion_events es la original. Añadir placeholders.
    cols_flujo_cps_placeholder <- c("Total_Flow_Weighted", "Prob_Transicion_Weighted", "Total_Origin_Pop_Weighted")
    for(col_pl in cols_flujo_cps_placeholder){
      if(! (col_pl %in% names(diffusion_events)) ){
        diffusion_events[, (col_pl) := 0]
      }
    }
    message("'diffusion_events_clean' no estaba disponible. 'diffusion_events' (original) tiene columnas de flujo con ceros añadidas.")
  }
}

# --- FIN DEL SCRIPT ---
message("Script finalizado. La tabla resultante es 'diffusion_events'.")


# --- CÓDIGO DE EVALUACIÓN DEL NUEVO PROCEDIMIENTO ---
# (Adaptado para usar 'diffusion_events' y 'diffusion_events_clean')

# --- PASO DE EVALUACIÓN 1: Revisar las Pérdidas Durante los Crosswalks de los Datos de Flujo CPS ---
message("--- Evaluación de Pérdidas en los Crosswalks de Flujos CPS (Después de Correcciones de Tipo) ---")

if (exists("worker_transition_probabilities_agg") && is.data.table(worker_transition_probabilities_agg) && nrow(worker_transition_probabilities_agg) > 0) {
  message(paste("Número total de pares de flujo OCC2010-OCC2010 únicos en 'worker_transition_probabilities_agg':", 
                nrow(worker_transition_probabilities_agg)))
  message(paste("Suma total de Agg_Total_Flow_Weighted en 'worker_transition_probabilities_agg':", 
                sum(worker_transition_probabilities_agg$Agg_Total_Flow_Weighted, na.rm = TRUE)))
  message(paste("Suma total de Agg_Count_Flow_Unweighted (si existe):", 
                if("Agg_Count_Flow_Unweighted" %in% names(worker_transition_probabilities_agg)) sum(worker_transition_probabilities_agg$Agg_Count_Flow_Unweighted, na.rm = TRUE) else "Columna no encontrada"))
} else {
  message("'worker_transition_probabilities_agg' no encontrada o vacía. No se puede evaluar la pérdida inicial.")
}

if (exists("worker_transition_probabilities_agg") && exists("dt_occ2010_to_soc2010") && 
    is.data.table(worker_transition_probabilities_agg) && is.data.table(dt_occ2010_to_soc2010) &&
    nrow(worker_transition_probabilities_agg) > 0 && nrow(dt_occ2010_to_soc2010) > 0) {
  
  codigos_occ_origen_flujo <- unique(worker_transition_probabilities_agg$SOC_Origen_OCC2010)
  codigos_occ_destino_flujo <- unique(worker_transition_probabilities_agg$SOC_Destino_OCC2010)
  codigos_occ_unicos_en_flujo <- unique(c(codigos_occ_origen_flujo, codigos_occ_destino_flujo))
  
  codigos_occ_en_crosswalk1 <- unique(dt_occ2010_to_soc2010$OCC2010_Code_Raw)
  
  mapeados_en_crosswalk1 <- intersect(codigos_occ_unicos_en_flujo, codigos_occ_en_crosswalk1)
  no_mapeados_en_crosswalk1 <- setdiff(codigos_occ_unicos_en_flujo, codigos_occ_en_crosswalk1)
  
  message(paste("De", length(codigos_occ_unicos_en_flujo), "códigos OCC2010 únicos en flujos, ", 
                length(mapeados_en_crosswalk1), "SÍ tienen entrada en dt_occ2010_to_soc2010."))
  message(paste(length(no_mapeados_en_crosswalk1), "códigos OCC2010 de flujos NO encontraron mapeo en dt_occ2010_to_soc2010."))
  if(length(no_mapeados_en_crosswalk1) > 0) {
    print("Primeros 10 OCC2010 de flujos no mapeados a SOC2010:")
    print(head(no_mapeados_en_crosswalk1, 10))
  }
}

if (exists("final_cps_flow_data") && is.data.table(final_cps_flow_data) && nrow(final_cps_flow_data) > 0) {
  message(paste("Número de pares de flujo SOC-O*NET únicos finales en 'final_cps_flow_data':", 
                nrow(final_cps_flow_data)))
  message(paste("Suma total de Total_Flow_Weighted en 'final_cps_flow_data':", 
                sum(final_cps_flow_data$Total_Flow_Weighted, na.rm = TRUE)))
  message(paste("Suma total de Total_Origin_Pop_Weighted en 'final_cps_flow_data':", 
                sum(final_cps_flow_data$Total_Origin_Pop_Weighted, na.rm = TRUE)))
} else {
  message("'final_cps_flow_data' no encontrada o vacía. No se puede evaluar el resultado final de los flujos CPS.")
}

message("--- Comparación de Formatos de Códigos SOC antes del Merge Final (Post-Correcciones) ---")
if (exists("diffusion_events_clean") && nrow(diffusion_events_clean) > 0 && "source_soc_std" %in% names(diffusion_events_clean)) {
  message("Primeros 5 códigos 'source_soc_std' únicos en 'diffusion_events_clean':")
  print(head(unique(diffusion_events_clean$source_soc_std)))
}
if (exists("final_cps_flow_data") && nrow(final_cps_flow_data) > 0 && "Source_SOC_ONET" %in% names(final_cps_flow_data)) {
  message("Primeros 5 códigos 'Source_SOC_ONET' únicos en 'final_cps_flow_data':")
  print(head(unique(final_cps_flow_data$Source_SOC_ONET)))
}

# --- PASO DE EVALUACIÓN 2: Analizar la Cobertura del Merge Final ---
message("--- Evaluación de la Cobertura del Merge Final (Post-Correcciones) ---")

if (exists("diffusion_events") && is.data.table(diffusion_events) && nrow(diffusion_events) > 0) {
  total_eventos_onet_final <- nrow(diffusion_events) # Ahora usamos diffusion_events
  
  eventos_con_flujo_en_tabla_final <- nrow(diffusion_events[Total_Flow_Weighted > 0])
  porcentaje_con_flujo_en_tabla_final <- (eventos_con_flujo_en_tabla_final / total_eventos_onet_final) * 100
  
  message(paste("Total de eventos de difusión en 'diffusion_events':", total_eventos_onet_final))
  message(paste("Número de estos eventos con Total_Flow_Weighted > 0:", eventos_con_flujo_en_tabla_final))
  message(paste("Porcentaje de eventos de difusión con datos de flujo > 0:", round(porcentaje_con_flujo_en_tabla_final, 2), "%"))
  
  if (eventos_con_flujo_en_tabla_final > 0 && "Total_Flow_Weighted" %in% names(diffusion_events)) {
    message("Análisis de las características de los eventos CON y SIN flujo (basado en Total_Flow_Weighted > 0):")
    diffusion_events[, matched_cps_flow_eval := (Total_Flow_Weighted > 0)]
    
    if ("structural_distance" %in% names(diffusion_events)) {
      print(diffusion_events[, 
                             .(mean_struct_dist = mean(structural_distance, na.rm = TRUE), N_obs = .N), 
                             by = .(matched_cps_flow_eval)])
    }
    
    if ("diffusion" %in% names(diffusion_events)) {
      print(diffusion_events[, 
                             .(N_obs =.N, Pct_Total_Categoria = .N / total_eventos_onet_final * 100), 
                             by = .(diffusion, matched_cps_flow_eval)][order(diffusion, matched_cps_flow_eval)])
      
      resumen_flujo_por_difusion_actualizado <- diffusion_events[, 
                                                                 .(
                                                                   N_eventos_total_cat = .N,
                                                                   N_eventos_con_flujo_obs_cat = sum(Total_Flow_Weighted > 0, na.rm = TRUE),
                                                                   Pct_eventos_con_flujo_obs_cat = sum(Total_Flow_Weighted > 0, na.rm = TRUE) / .N * 100,
                                                                   Media_Flujo_Ponderado_Total_cat = mean(Total_Flow_Weighted, na.rm = TRUE),
                                                                   Media_Flujo_Si_Hay_Flujo_cat = mean(Total_Flow_Weighted[Total_Flow_Weighted > 0], na.rm = TRUE),
                                                                   Mediana_Flujo_Si_Hay_Flujo_cat = median(Total_Flow_Weighted[Total_Flow_Weighted > 0], na.rm = TRUE),
                                                                   Media_Prob_Trans_Si_Hay_Flujo_cat = mean(Prob_Transicion_Weighted[Total_Flow_Weighted > 0], na.rm = TRUE)
                                                                 ), 
                                                                 by = .(diffusion)
      ]
      message("Resumen del flujo de trabajadores por estado de difusión (ACTUALIZADO):")
      print(resumen_flujo_por_difusion_actualizado)
    }
    diffusion_events[, matched_cps_flow_eval := NULL] 
  } else if (!("Total_Flow_Weighted" %in% names(diffusion_events))) {
    message("La columna 'Total_Flow_Weighted' no existe en 'diffusion_events' para la evaluación detallada.")
  }
} else {
  message("'diffusion_events' no encontrada o vacía para la evaluación de cobertura.")
}

# --- PASO DE EVALUACIÓN 3: Investigar los Códigos que No Hacen Match (si es necesario) ---
message("--- Investigando Códigos que No Hacen Match (Post-Correcciones) ---")
if (exists("diffusion_events_clean") && exists("final_cps_flow_data") && 
    is.data.table(diffusion_events_clean) && is.data.table(final_cps_flow_data) &&
    nrow(diffusion_events_clean) > 0 && nrow(final_cps_flow_data) > 0) {
  
  onet_sources <- unique(diffusion_events_clean$source_soc_std)
  onet_targets <- unique(diffusion_events_clean$target_soc_std)
  cps_sources <- unique(final_cps_flow_data$Source_SOC_ONET)
  cps_targets <- unique(final_cps_flow_data$Target_SOC_ONET)
  
  onet_sources_no_match_cps <- setdiff(onet_sources, cps_sources)
  message(paste("Número de códigos 'source_soc_std' de O*NET (limpios) que NO existen como 'Source_SOC_ONET' en flujos CPS:", length(onet_sources_no_match_cps)))
  if (length(onet_sources_no_match_cps) > 0) print(head(onet_sources_no_match_cps, 10))
  
  onet_targets_no_match_cps <- setdiff(onet_targets, cps_targets)
  message(paste("Número de códigos 'target_soc_std' de O*NET (limpios) que NO existen como 'Target_SOC_ONET' en flujos CPS:", length(onet_targets_no_match_cps)))
  if (length(onet_targets_no_match_cps) > 0) print(head(onet_targets_no_match_cps, 10))
  
} else {
  message("No se pueden investigar los códigos no coincidentes; 'diffusion_events_clean' o 'final_cps_flow_data' no disponibles o vacíos.")
}


glimpse(diffusion_events)




