# =======================================================================
# PARTE 1: CREACIÓN DE LA BASE DE DATOS DE EVENTOS DE DIFUSIÓN
# Versión: 12.0.8_MultiClasif_LouvainLeiden_2015_2023 (Cleaned) - Ligeramente ajustada para consistencia
# - Genera y guarda all_events_final_enriched.
# - Manages intermediate data.tables by removing them after use.
# =======================================================================
gc()
# --- 0. Cargar Librerías Necesarias ---
message("PARTE 1: PASO 0: Cargando librerías necesarias...")
packages_to_load_p1 <- c("data.table", "readxl", "progress", "here",
                         "Matrix", "lsa", "cluster", "moments", "igraph", "stringr", "dplyr") # dplyr para glimpse

for (pkg_p1 in packages_to_load_p1) {
  if (!requireNamespace(pkg_p1, quietly = TRUE)) {
    message(paste("Instalando paquete (Parte 1):", pkg_p1, "..."))
    tryCatch({ install.packages(pkg_p1, dependencies = TRUE) }, error = function(e){ warning(paste("Fallo al instalar (Parte 1):", pkg_p1, "-", e$message), call. = FALSE)})
  }
  if (!require(pkg_p1, character.only = TRUE)) {
    warning(paste("No se pudo cargar el paquete (Parte 1):", pkg_p1), call. = FALSE)
  }
}

# --- 1. Definir Rutas y Nombres de Archivo ---
message("\nPARTE 1: PASO 1: Definiendo rutas y nombres de archivo...")
onet_db_path <- "/home/rober/Descargas/db_29_2_text" # PLEASE VERIFY THIS PATH
crosswalk_data_path_p1 <- "/home/rober/Documentos/skill_diffusion/data/crosswalk/" # PLEASE VERIFY THIS PATH
data_external_path_p1 <- "/home/rober/Documentos/skill_diffusion/data/" # PLEASE VERIFY THIS PATH
output_data_dir_p1 <- "datos_eventos_v12_MultiClasif_Cleaned_Corrected" # Directorio actualizado
if (!dir.exists(output_data_dir_p1)) { dir.create(output_data_dir_p1, recursive = TRUE) }
message("La base de datos 'all_events_final_enriched' (Parte 1) se guardará en: ", normalizePath(output_data_dir_p1))

occupation_data_filename_onet <- "Occupation Data.txt"
job_zones_filename_onet <- "Job Zones.txt"
ete_data_filename_onet <- "Education, Training, and Experience.txt"
ete_categories_filename_onet <- "Education, Training, and Experience Categories.txt"
skills_filename_onet <- "Skills.txt"
abilities_filename_onet <- "Abilities.txt"
knowledge_filename_onet <- "Knowledge.txt"
work_activities_filename_onet <- "Work Activities.txt"

crosswalk_onet2019_to_soc2018_filename <- "2019_to_SOC_Crosswalk.xlsx"
crosswalk_onet2010_to_onet2019_filename <- "2010_to_2019_Crosswalk.csv"
crosswalk_soc2010_to_soc2018_filename <- "soc_2010_to_2018_crosswalk.xlsx"

bls_filename_2015 <- "national_M2015_dl.xlsx"
bls_filename_2024 <- "national_M2024_dl.xlsx" # Assuming this is for 2023 data based on usage

# --- 2.A Función Auxiliar para Cargar Archivos .txt de O*NET ---
load_onet_text_file <- function(filename, path = onet_db_path) {
  full_path <- file.path(path, filename); if (!file.exists(full_path)) { warning(paste("Archivo no encontrado (Parte 1):", full_path)); return(data.table()) }; dt <- tryCatch({ fread(full_path, sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE, showProgress = FALSE, na.strings=c("NA","n/a","","*","**","****")) }, error = function(e) { warning(paste("Error fread (Parte 1):", e$message, ". Intentando read.delim.")); tryCatch({ as.data.table(read.delim(full_path, sep="\t", header=TRUE, quote="", stringsAsFactors=FALSE, na.strings=c("NA","n/a","","*","**","****"))) }, error = function(e2) { warning(paste("Error read.delim (Parte 1):", e2$message)); return(data.table()) }) }); if (is.null(dt) || nrow(dt) == 0) { warning(paste("Fallo al cargar/archivo vacío (Parte 1):", filename)); return(data.table()) }; current_names <- names(dt); new_names <- current_names; name_map <- list( "O*NET-SOC Code"="O.NET.SOC.Code", "Element Id"="Element.ID", "Element ID"="Element.ID", "Element Name"="Element.Name", "Scale ID"="Scale.ID", "Scale Name"="Scale.Name", "Data Value"="Data.Value", "Standard Error"="Standard.Error", "Lower CI Bound"="Lower.CI.Bound", "Upper CI Bound"="Upper.CI.Bound", "Recommend Suppress"="Recommend.Suppress", "Not Relevant"="Not.Relevant", "Domain Source"="Domain.Source", "Job Zone"="Job.Zone", "Category Description"="Category.Description" ); changed_any_name <- FALSE; for (old_nm_map in names(name_map)) { new_nm_map <- name_map[[old_nm_map]]; if (old_nm_map %in% new_names) { idx_rn <- which(new_names == old_nm_map); if (length(idx_rn) > 0) { if (old_nm_map != new_nm_map && (!(new_nm_map %in% new_names) || sum(new_names == new_nm_map) == 0 || new_names[idx_rn[1]] != new_nm_map)) { new_names[idx_rn] <- new_nm_map; changed_any_name <- TRUE } } } }; if (changed_any_name) { unique_new_names <- make.unique(new_names, sep = "_"); if (!identical(names(dt), unique_new_names)) setnames(dt, old = current_names, new = unique_new_names, skip_absent = TRUE) }; return(dt)
}

# --- Función de limpieza de códigos SOC (usada en Parte 1 y Parte 2) ---
# Devuelve un código SOC de 6 dígitos como string, o NA.
clean_soc_code_universal <- function(soc_code_vector) {
  soc_code_vector <- as.character(soc_code_vector)
  # Remove hyphens
  soc_code_vector <- stringr::str_remove_all(soc_code_vector, "-")
  # Remove ".0" or ".00" from the end of the string (common in O*NET SOC codes like "11-1011.00")
  # Also handles cases like "412010.0" becoming "412010"
  soc_code_vector <- stringr::str_replace(soc_code_vector, "\\.00$", "")
  soc_code_vector <- stringr::str_replace(soc_code_vector, "\\.0$", "")
  
  soc_code_vector <- stringr::str_trim(soc_code_vector)
  
  # Extract only digits. This will also remove any remaining non-numeric characters.
  soc_code_vector_numeric_part <- gsub("[^0-9]", "", soc_code_vector)
  
  soc_code_vector_formatted <- sapply(soc_code_vector_numeric_part, function(code) {
    if (is.na(code) || nchar(code) == 0) {
      return(NA_character_)
    } else if (nchar(code) > 0 && nchar(code) < 6) { # Pad if shorter than 6
      return(stringr::str_pad(code, 6, side = "left", pad = "0"))
    } else if (nchar(code) == 6) { # Already 6 digits
      return(code)
    } else if (nchar(code) > 6) { # If longer than 6, attempt to take the first 6.
      # This might be too aggressive if the code is e.g. a more detailed SOC code,
      # but for harmonization to SOC6, this is a pragmatic choice.
      # Consider logging or specific handling if this truncation is problematic.
      # warning(paste("Código SOC con más de 6 dígitos numéricos:", code, ". Tomando los primeros 6."))
      return(substr(code, 1, 6)) 
    } else { # Other unexpected cases
      return(NA_character_)
    }
  })
  # Final validation: ensure it's 6 digits and numeric, otherwise NA
  soc_code_vector_formatted[!grepl("^[0-9]{6}$", soc_code_vector_formatted) & !is.na(soc_code_vector_formatted)] <- NA_character_
  return(soc_code_vector_formatted)
}


# --- 2.B Cargar y Preparar Crosswalks ---
message("\nPARTE 1: PASO 2.B: Cargando y preparando crosswalks para armonización a SOC 2018...")
clean_onet_soc_code <- function(onet_soc_code_vector) { # Esta es específica para O*NET que puede tener formato XXXXXX.XX
  onet_soc_code_vector <- as.character(onet_soc_code_vector)
  onet_soc_code_vector <- stringr::str_trim(onet_soc_code_vector)
  return(onet_soc_code_vector) # Dejarla como está, el merge con onet2019_to_soc2018_map se encargará
}

path_onet2019_to_soc2018 <- file.path(crosswalk_data_path_p1, crosswalk_onet2019_to_soc2018_filename)
onet2019_to_soc2018_map <- data.table()
if (file.exists(path_onet2019_to_soc2018)) {
  tryCatch({
    dt_raw <- read_excel(path_onet2019_to_soc2018, sheet = "O-NET-SOC 2019 Occupation Listi", skip = 2) # Verify sheet name "Listi"
    dt_loc <- as.data.table(dt_raw)
    onet_col <- "O*NET-SOC 2019 Code"; soc2018_col <- "2018 SOC Code"
    if (all(c(onet_col, soc2018_col) %in% names(dt_loc))) {
      onet2019_to_soc2018_map <- dt_loc[, .("O.NET.SOC.Code.2019" = clean_onet_soc_code(get(onet_col)), 
                                            "SOC_Code_Harmonized_2018" = clean_soc_code_universal(get(soc2018_col)))] 
      onet2019_to_soc2018_map <- unique(onet2019_to_soc2018_map[!is.na(O.NET.SOC.Code.2019) & !is.na(SOC_Code_Harmonized_2018)])
      message(sprintf("Cargado O*NET-SOC 2019 a SOC 2018 (Parte 1): %d mapeos.", nrow(onet2019_to_soc2018_map)))
    } else { warning(paste("Columnas ('",onet_col,"', '",soc2018_col,"') no encontradas en (Parte 1)", crosswalk_onet2019_to_soc2018_filename)) }
  }, error = function(e) { warning(paste("Error cargando (Parte 1)", crosswalk_onet2019_to_soc2018_filename, ":", e$message)) })
} else { warning(paste("CROSSWALK NO ENCONTRADO (Parte 1):", path_onet2019_to_soc2018)) }

path_onet2010_to_onet2019 <- file.path(crosswalk_data_path_p1, crosswalk_onet2010_to_onet2019_filename)
onet2010_to_onet2019_map <- data.table()
if (file.exists(path_onet2010_to_onet2019)) {
  tryCatch({
    onet2010_to_onet2019_map_raw <- fread(path_onet2010_to_onet2019)
    onet2010_col_name <- "O*NET-SOC 2010 Code"; onet2019_col_name <- "O*NET-SOC 2019 Code"
    if(all(c(onet2010_col_name, onet2019_col_name) %in% names(onet2010_to_onet2019_map_raw))){
      onet2010_to_onet2019_map <- onet2010_to_onet2019_map_raw[, .("O.NET.SOC.Code.2010" = clean_onet_soc_code(get(onet2010_col_name)),
                                                                   "O.NET.SOC.Code.2019" = clean_onet_soc_code(get(onet2019_col_name)))]
      onet2010_to_onet2019_map <- unique(onet2010_to_onet2019_map[!is.na(O.NET.SOC.Code.2010) & !is.na(O.NET.SOC.Code.2019)])
      message(sprintf("Cargado O*NET-SOC 2010 a O*NET-SOC 2019 (Parte 1): %d mapeos.", nrow(onet2010_to_onet2019_map)))
    } else { warning(paste("Columnas O*NET 2010/2019 no encontradas con nombres exactos en (Parte 1)", crosswalk_onet2010_to_onet2019_filename)) }
  }, error = function(e) { warning(paste("Error cargando (Parte 1)", crosswalk_onet2010_to_onet2019_filename, ":", e$message)) })
} else { warning(paste("CROSSWALK NO ENCONTRADO (Parte 1):", path_onet2010_to_onet2019)) }

path_soc2010_to_soc2018 <- file.path(crosswalk_data_path_p1, crosswalk_soc2010_to_soc2018_filename)
soc2010_to_soc2018_map <- data.table()
if (file.exists(path_soc2010_to_soc2018)) {
  tryCatch({
    dt_raw <- read_excel(path_soc2010_to_soc2018, sheet = "Sorted by 2010", skip = 8) 
    dt_loc <- as.data.table(dt_raw)
    soc2010_col <- "2010 SOC Code"; soc2018_col <- "2018 SOC Code"
    if (all(c(soc2010_col, soc2018_col) %in% names(dt_loc))) {
      soc2010_to_soc2018_map <- dt_loc[, .("SOC_Code_2010" = clean_soc_code_universal(get(soc2010_col)),
                                           "SOC_Code_Harmonized_2018" = clean_soc_code_universal(get(soc2018_col)))]
      soc2010_to_soc2018_map <- unique(soc2010_to_soc2018_map[!is.na(SOC_Code_2010) & !is.na(SOC_Code_Harmonized_2018)])
      message(sprintf("Cargado SOC 2010 a SOC 2018 (Parte 1): %d mapeos.", nrow(soc2010_to_soc2018_map)))
    } else { warning(paste("Columnas SOC 2010/2018 no encontradas con nombres exactos en (Parte 1)", crosswalk_soc2010_to_soc2018_filename)) }
  }, error = function(e) { warning(paste("Error cargando (Parte 1)", crosswalk_soc2010_to_soc2018_filename, ":", e$message)) })
} else { warning(paste("CROSSWALK NO ENCONTRADO (Parte 1):", path_soc2010_to_soc2018)) }

# --- 3. PREPARACIÓN DE DATOS GLOBALES ---
message("\nPARTE 1: --- SECCIÓN 3.A: Creando 'occupation_attributes_final' (armonizado) ---")
process_bls_data_for_final <- function(bls_fp, yr_lbl, soc_col_raw, grp_col, wage_col, soc_version_bls, map_soc2010_to_soc2018, map_onet2019_to_soc2018 = NULL) {
  cat("\n--- Procesando salarios BLS (Parte 1):", yr_lbl, " (formato SOC", soc_version_bls, ") ---\n")
  if (!file.exists(bls_fp)) { warning(paste("Archivo BLS no encontrado (Parte 1):", bls_fp)); return(data.table()) }
  bls_raw <- tryCatch(read_excel(bls_fp, sheet = 1), error = function(e) NULL)
  if (is.null(bls_raw) || nrow(bls_raw) == 0) { warning(paste("Fallo al cargar/archivo BLS vacío (Parte 1):", bls_fp)); return(data.table()) }
  bls_df <- as.data.table(bls_raw)
  required_bls_cols <- c(soc_col_raw, grp_col, wage_col)
  if (!all(required_bls_cols %in% names(bls_df))) { warning(paste("Columnas BLS requeridas no encontradas en (Parte 1)", bls_fp, ". Columnas disponibles:", paste(names(bls_df), collapse=", "))); return(data.table()) }
  dt_det <- bls_df[tolower(get(grp_col)) %in% c("detailed", "detail")]
  if (nrow(dt_det) == 0) { warning(paste("No hay ocupaciones 'detailed' en (Parte 1)", bls_fp)); return(data.table()) }
  bls_w_p <- dt_det[, .SD, .SDcols = c(soc_col_raw, wage_col)]
  temp_wage_col_name <- paste0("Median_Wage_BLS_Raw_", yr_lbl); setnames(bls_w_p, old = c(soc_col_raw, wage_col), new = c("SOC_Code_Original_BLS", temp_wage_col_name))
  bls_w_p[, temp_cleaned_wage := as.character(get(temp_wage_col_name))][temp_cleaned_wage %in% c("*", "**", "***", "****", "#", "N.A.", "n.a."), temp_cleaned_wage := NA_character_]
  bls_w_p[, temp_cleaned_wage := gsub("[^0-9\\.]", "", temp_cleaned_wage)][, (temp_wage_col_name) := as.numeric(temp_cleaned_wage)][, temp_cleaned_wage := NULL]
  bls_w_p <- bls_w_p[!is.na(get(temp_wage_col_name))]; bls_w_p[, SOC_Code_Original_BLS := clean_soc_code_universal(SOC_Code_Original_BLS)] 
  if (nrow(bls_w_p) == 0) { warning(paste("No hay datos de salarios válidos después de limpiar para (Parte 1)", yr_lbl)); return(data.table()) }
  bls_w_p_armonizado <- data.table()
  if (soc_version_bls == "2010") {
    if (!is.null(map_soc2010_to_soc2018) && nrow(map_soc2010_to_soc2018) > 0) {
      bls_w_p_armonizado <- merge(bls_w_p, map_soc2010_to_soc2018, by.x = "SOC_Code_Original_BLS", by.y = "SOC_Code_2010", all.x = FALSE, allow.cartesian = TRUE)
    } else { warning("Crosswalk SOC 2010 a SOC 2018 no disponible (Parte 1)."); return(data.table()) }
  } else if (soc_version_bls == "2018") {
    bls_w_p_armonizado <- copy(bls_w_p); bls_w_p_armonizado[, SOC_Code_Harmonized_2018 := SOC_Code_Original_BLS]
  } else { warning(paste("Versión SOC no soportada para datos BLS (Parte 1):", soc_version_bls)); return(data.table())}
  if (nrow(bls_w_p_armonizado) == 0) { warning(paste("No hay datos BLS después de armonizar para (Parte 1)", yr_lbl)); return(data.table()) }
  final_wage_col_name <- paste0("Median_Wage_", yr_lbl)
  bls_w_final <- bls_w_p_armonizado[, .(temp_wage = mean(get(temp_wage_col_name), na.rm = TRUE)), by = SOC_Code_Harmonized_2018]; setnames(bls_w_final, "temp_wage", final_wage_col_name)
  message(sprintf("Salarios BLS para %s procesados (Parte 1): %d entradas.", yr_lbl, nrow(bls_w_final))); return(bls_w_final)
}
occupation_data_onet_raw <- load_onet_text_file(occupation_data_filename_onet); job_zones_onet_raw <- load_onet_text_file(job_zones_filename_onet)
ete_data_raw <- load_onet_text_file(ete_data_filename_onet); ete_categories_raw <- load_onet_text_file(ete_categories_filename_onet)
if(nrow(occupation_data_onet_raw)==0 || nrow(job_zones_onet_raw)==0 || nrow(ete_data_raw)==0 || nrow(ete_categories_raw)==0) stop("Fallo crítico archivos O*NET principales (Parte 1).")

occupation_data_onet_raw[, O.NET.SOC.Code := clean_onet_soc_code(O.NET.SOC.Code)]
job_zones_onet_raw[, O.NET.SOC.Code := clean_onet_soc_code(O.NET.SOC.Code)]
ete_data_raw[, O.NET.SOC.Code := clean_onet_soc_code(O.NET.SOC.Code)]

if (nrow(onet2019_to_soc2018_map) == 0) stop("onet2019_to_soc2018_map está vacío (Parte 1). No se puede armonizar O*NET a SOC 2018.")
occupation_data_onet <- merge(occupation_data_onet_raw, onet2019_to_soc2018_map, by.x = "O.NET.SOC.Code", by.y = "O.NET.SOC.Code.2019", all.x = FALSE)
occupation_data_onet <- unique(occupation_data_onet, by = c("SOC_Code_Harmonized_2018", "O.NET.SOC.Code"))
job_zones_onet <- merge(job_zones_onet_raw, onet2019_to_soc2018_map, by.x = "O.NET.SOC.Code", by.y = "O.NET.SOC.Code.2019", all.x = FALSE)
job_zones_onet <- unique(job_zones_onet, by = c("SOC_Code_Harmonized_2018", "O.NET.SOC.Code", "Job.Zone"))
ete_data <- merge(ete_data_raw, onet2019_to_soc2018_map, by.x = "O.NET.SOC.Code", by.y = "O.NET.SOC.Code.2019", all.x = FALSE); ete_data <- unique(ete_data)
message(sprintf("Datos O*NET principales mapeados (Parte 1). Filas: Occs:%d, Zones:%d, ETE:%d", nrow(occupation_data_onet), nrow(job_zones_onet), nrow(ete_data)))
job_zones_simple <- data.table(); if (nrow(job_zones_onet) > 0 && all(c("SOC_Code_Harmonized_2018", "Job.Zone") %in% names(job_zones_onet))) job_zones_simple <- unique(job_zones_onet[, .(Job.Zone = as.integer(round(mean(as.numeric(Job.Zone), na.rm=TRUE)))), by = SOC_Code_Harmonized_2018])
education_scores_onet <- data.table(); required_ete_cols_harmonized <- c("SOC_Code_Harmonized_2018", "Element.ID", "Scale.ID", "Category", "Data.Value"); required_cat_cols <- c("Element.ID", "Scale.ID", "Category", "Category.Description")
if (nrow(ete_data) > 0 && all(c("O.NET.SOC.Code", required_ete_cols_harmonized) %in% names(ete_data)) && nrow(ete_categories_raw) > 0 && all(required_cat_cols %in% names(ete_categories_raw))) {
  rl_data <- ete_data[Element.ID == "2.D.1" & Scale.ID == "RL"]; rl_cat_loc <- ete_categories_raw[Element.ID == "2.D.1" & Scale.ID == "RL", .(Category, Category.Description)]; rl_cat_loc[, Category := as.integer(Category)]; setkey(rl_cat_loc, Category)
  edu_scale_map <- data.table(Category = 1:12, Edu_Numeric_Value = 1:12); setkey(edu_scale_map, Category); rl_data[, Category_Int := as.integer(Category)]; rl_data_sc <- merge(rl_data, edu_scale_map, by.x = "Category_Int", by.y = "Category", all.x = TRUE); rl_data_sc <- rl_data_sc[!is.na(Edu_Numeric_Value)]
  if (nrow(rl_data_sc) > 0) { education_scores_temp <- rl_data_sc[, .(Edu_Score_Weighted_Sum = sum(Data.Value * Edu_Numeric_Value, na.rm = TRUE), Data_Value_Sum = sum(Data.Value, na.rm=TRUE)), by = SOC_Code_Harmonized_2018]; education_scores_temp[, Edu_Score_Weighted := fifelse(Data_Value_Sum > 0, Edu_Score_Weighted_Sum / Data_Value_Sum, NA_real_)][, Total_Pct_Reported := Data_Value_Sum]; education_scores_onet <- education_scores_temp[Total_Pct_Reported > 50, .(SOC_Code_Harmonized_2018, Edu_Score_Weighted)] }
}
occupation_attributes_temp <- data.table(); if (nrow(occupation_data_onet) > 0) { setorder(occupation_data_onet, SOC_Code_Harmonized_2018, O.NET.SOC.Code); occupation_attributes_temp <- unique(occupation_data_onet[, .(SOC_Code_Harmonized_2018, Title, Description)], by = "SOC_Code_Harmonized_2018") } else stop("occupation_data_onet armonizado está vacío (Parte 1).")
if (nrow(job_zones_simple) > 0) occupation_attributes_temp <- merge(occupation_attributes_temp, job_zones_simple, by = "SOC_Code_Harmonized_2018", all.x = TRUE) else occupation_attributes_temp[, Job.Zone := NA_integer_]
if (nrow(education_scores_onet) > 0) occupation_attributes_temp <- merge(occupation_attributes_temp, education_scores_onet, by = "SOC_Code_Harmonized_2018", all.x = TRUE) else occupation_attributes_temp[, Edu_Score_Weighted := NA_real_]
bls_wages_2015_final <- process_bls_data_for_final(bls_fp = file.path(data_external_path_p1, bls_filename_2015), yr_lbl = "2015", soc_col_raw = "OCC_CODE", grp_col = "OCC_GROUP", wage_col = "A_MEDIAN", soc_version_bls = "2010", map_soc2010_to_soc2018 = soc2010_to_soc2018_map)
bls_wages_2023_final <- process_bls_data_for_final(bls_fp = file.path(data_external_path_p1, bls_filename_2024), yr_lbl = "2023", soc_col_raw = "OCC_CODE", grp_col = "O_GROUP", wage_col = "A_MEDIAN", soc_version_bls = "2018", map_soc2010_to_soc2018 = NULL)
occupation_attributes_final <- copy(occupation_attributes_temp)
if (nrow(bls_wages_2015_final) > 0) occupation_attributes_final <- merge(occupation_attributes_final, bls_wages_2015_final, by = "SOC_Code_Harmonized_2018", all.x = TRUE) else occupation_attributes_final[, Median_Wage_2015 := NA_real_]
if (nrow(bls_wages_2023_final) > 0) occupation_attributes_final <- merge(occupation_attributes_final, bls_wages_2023_final, by = "SOC_Code_Harmonized_2018", all.x = TRUE) else occupation_attributes_final[, Median_Wage_2023 := NA_real_]
message("'occupation_attributes_final' creada (Parte 1)."); if(nrow(occupation_attributes_final) > 0) fwrite(occupation_attributes_final, file.path(output_data_dir_p1, "occupation_attributes_final_harmonized.csv"))

rm(occupation_data_onet_raw, job_zones_onet_raw, ete_data_raw, ete_categories_raw)
rm(occupation_data_onet, job_zones_onet, ete_data, job_zones_simple, education_scores_onet, education_scores_temp)
rm(occupation_attributes_temp, bls_wages_2015_final, bls_wages_2023_final)
gc()
message("Objetos intermedios de PARTE 1: PASO 3.A limpiados de memoria.")

message("\nPARTE 1: --- SECCIÓN 3.B: Creando 'all_skills_data' (con armonización) ---")
skills_list_raw <- list(load_onet_text_file(skills_filename_onet), load_onet_text_file(abilities_filename_onet), load_onet_text_file(knowledge_filename_onet), load_onet_text_file(work_activities_filename_onet))
skills_list_valid_raw <- Filter(function(x) !is.null(x) && is.data.table(x) && nrow(x) > 0 && ncol(x) > 0, skills_list_raw)
all_skills_data <- data.table()
if(length(skills_list_valid_raw) > 0) {
  all_skills_data_raw <- rbindlist(skills_list_valid_raw, use.names = TRUE, fill = TRUE)
  all_skills_data_raw[, O.NET.SOC.Code := clean_onet_soc_code(O.NET.SOC.Code)] 
  if ("Date" %in% names(all_skills_data_raw)) { all_skills_data_raw[, year_text := as.character(Date)][, year := suppressWarnings(as.integer(stringr::str_extract(year_text, "\\d{4}$")))][, year_text := NULL]; all_skills_data_raw <- all_skills_data_raw[!is.na(year) & year > 1900] } else { all_skills_data_raw[, year := as.integer(NA)]; warning("Columna 'Date'/'year' no encontrada en all_skills_data_raw (Parte 1).") }
  if ("Data.Value" %in% names(all_skills_data_raw)) { all_skills_data_raw[, Data.Value := as.numeric(Data.Value)]; all_skills_data_raw <- all_skills_data_raw[!is.na(Data.Value)] } else warning("Columna 'Data.Value' no encontrada en all_skills_data_raw (Parte 1).")
  if (nrow(onet2019_to_soc2018_map) > 0) {
    all_skills_data <- merge(all_skills_data_raw, onet2019_to_soc2018_map, by.x = "O.NET.SOC.Code", by.y = "O.NET.SOC.Code.2019", all.x = FALSE, allow.cartesian = TRUE)
    all_skills_data <- all_skills_data[!is.na(SOC_Code_Harmonized_2018)]; message(sprintf("'all_skills_data' preparada (Parte 1): %d filas.", nrow(all_skills_data)))
  } else warning("'all_skills_data' no pudo ser armonizada porque onet2019_to_soc2018_map está vacío (Parte 1).")
  rm(all_skills_data_raw)
} else warning("'all_skills_data_raw' está vacía (ningún archivo de skills válido cargado) (Parte 1).")
rm(skills_list_raw, skills_list_valid_raw)
gc()
message("Objetos intermedios de PARTE 1: PASO 3.B limpiados de memoria.")
if(nrow(all_skills_data) > 0) fwrite(all_skills_data, file.path(output_data_dir_p1, "all_skills_data_harmonized.csv"))


# --- 4. FUNCIONES DE CLASIFICACIÓN DE SKILLS ---
classify_skills_by_type <- function(skill_data, year_to_analyze, n_clusters = 3) {
  message("Clasificando skills por clúster (Parte 1)..."); if (is.null(skill_data) || nrow(skill_data) == 0) stop("Datos skills vacíos para classify_skills_by_type (Parte 1)"); s_year <- skill_data; if (!is.null(year_to_analyze) && !is.na(year_to_analyze) && "year" %in% names(skill_data)) { temp_s_year <- skill_data[year == year_to_analyze]; if (nrow(temp_s_year) > 0) s_year <- temp_s_year }; s_level <- s_year; if ("Scale.ID" %in% names(s_year)) { temp_s_level <- s_year[Scale.ID == "LV"]; if (nrow(temp_s_level) > 0) s_level <- temp_s_level };
  s_dist_metrics <- s_level[, .( mean_level = mean(Data.Value, na.rm = TRUE), median_level = median(Data.Value, na.rm = TRUE), usage_count = .N, sd_level = sd(Data.Value, na.rm = TRUE) ), by = .(Element.Name)];
  s_hist_data_list <- list(); u_skills_list <- unique(s_level$Element.Name); pb_h <- NULL; if(requireNamespace("progress", quietly=TRUE) && length(u_skills_list)>10) pb_h <- progress::progress_bar$new(total=length(u_skills_list), format="Hist. [:bar] :percent"); for (sk_iter in u_skills_list) { if(!is.null(pb_h)) pb_h$tick(); sk_vals_iter <- s_level[Element.Name == sk_iter, Data.Value]; if (length(sk_vals_iter) < 5) next; hr_obj <- hist(sk_vals_iter, breaks = 8, plot = FALSE); nc_vals <- hr_obj$counts / sum(hr_obj$counts); skn_val <- moments::skewness(sk_vals_iter, na.rm = TRUE); hup_val <- sum(sk_vals_iter > 3, na.rm = TRUE) / length(sk_vals_iter); s_hist_data_list[[sk_iter]] <- c( nc_vals, skn_val, hup_val, median(sk_vals_iter, na.rm = TRUE), sd(sk_vals_iter, na.rm = TRUE) ) }; if (length(s_hist_data_list) == 0) stop("No se calcularon distribuciones para clustering en classify_skills_by_type (Parte 1)."); s_matrix_for_clust <- do.call(rbind, s_hist_data_list); rownames(s_matrix_for_clust) <- names(s_hist_data_list); s_matrix_for_clust[!is.finite(s_matrix_for_clust)] <- 0; sk_dist_obj <- dist(s_matrix_for_clust); sk_hcl_obj <- hclust(sk_dist_obj, method = "ward.D2"); sk_clus_assign <- cutree(sk_hcl_obj, k = n_clusters); cl_sum_table <- data.table( Cluster = 1:n_clusters, MedianLevel = sapply(1:n_clusters, function(i) { sk_in_cl_names <- names(sk_clus_assign[sk_clus_assign == i]); if (length(sk_in_cl_names) == 0) return(NA_real_); median(s_dist_metrics[Element.Name %in% sk_in_cl_names, median_level], na.rm = TRUE) }) ); cl_sum_table <- cl_sum_table[order(-MedianLevel)]; if (n_clusters == 3) { type_labels_vec <- c("general", "intermedia", "específica") } else { type_labels_vec <- paste0("Tipo", 1:n_clusters) }; cl_sum_table[, SkillType := type_labels_vec[1:nrow(cl_sum_table)]]; cl_map_obj <- cl_sum_table$SkillType; names(cl_map_obj) <- cl_sum_table$Cluster; sk_class_final <- data.table( Element.Name = names(sk_clus_assign), Cluster = sk_clus_assign, SkillType = cl_map_obj[as.character(sk_clus_assign)] ); sk_class_final <- merge(sk_class_final, s_dist_metrics, by = "Element.Name", all.x = TRUE); message("Clasif clúster completada (Parte 1)."); return(sk_class_final)
}
classify_skills_empirical <- function(skill_data, year_to_analyze) {
  message("Clasificando skills empíricamente (Parte 1)..."); if (is.null(skill_data) || nrow(skill_data) == 0) stop("Datos skills vacíos para classify_skills_empirical (Parte 1)");
  if (!"SOC_Code_Harmonized_2018" %in% names(skill_data)) stop("La columna 'SOC_Code_Harmonized_2018' es necesaria en classify_skills_empirical (Parte 1).")
  s_year <- skill_data; if (!is.null(year_to_analyze) && !is.na(year_to_analyze) && "year" %in% names(skill_data)) { temp_data <- skill_data[year == year_to_analyze]; if (nrow(temp_data) > 0) s_year <- temp_data };
  s_level <- s_year; if ("Scale.ID" %in% names(s_year)) { temp_level <- s_year[Scale.ID == "LV"]; if(nrow(temp_level)>0) s_level <- temp_level };
  sk_metrics <- s_level[, .( usage_count = uniqueN(SOC_Code_Harmonized_2018), mean_level = mean(Data.Value, na.rm = TRUE) ), by = .(Element.Name)];
  sk_metrics <- sk_metrics[!is.na(usage_count) & !is.na(mean_level)]; if(nrow(sk_metrics) == 0) stop("No se calcularon métricas empíricas en classify_skills_empirical (Parte 1).");
  med_usage <- median(sk_metrics$usage_count, na.rm = TRUE); med_level <- median(sk_metrics$mean_level, na.rm = TRUE); if(is.na(med_usage) || is.na(med_level)) stop("No se calcularon umbrales en classify_skills_empirical (Parte 1).");
  message(sprintf("Umbrales empíricos (Parte 1): Uso >= %.1f; Nivel >= %.2f", med_usage, med_level));
  sk_metrics[, generality := ifelse(usage_count >= med_usage, "Amplia", "Nicho")][, level_cat := ifelse(mean_level >= med_level, "Nivel_Alto", "Nivel_Bajo")][, SkillType := paste(generality, level_cat, sep = "_/_")];
  sk_class_emp <- sk_metrics[, .(Element.Name, usage_count, mean_level, SkillType)]; message("Clasif empírica completada (Parte 1)."); print(sk_class_emp[, .N, by=SkillType]); return(sk_class_emp)
}

classify_skills_network <- function(skill_data,
                                    year_to_analyze,
                                    comparison_year = NULL,
                                    complementarity_threshold = 1e-9,
                                    calculate_flow_status = TRUE,
                                    occupation_attributes_harmonized = NULL) {
  message("--- Iniciando Clasificación por Red Skill-Skill (Alabdulkareem et al. method + Leiden) (Parte 1) ---")
  if(!requireNamespace("igraph", quietly = TRUE)) {warning("Paquete 'igraph' necesario para classify_skills_network (Parte 1)."); return(NULL)}
  if (is.null(skill_data) || nrow(skill_data) == 0) {message("Datos skills vacíos para classify_skills_network (Parte 1)."); return(NULL)}
  required_cols_skill_data <- c("SOC_Code_Harmonized_2018", "year", "Element.Name", "Scale.ID", "Data.Value")
  if (!all(required_cols_skill_data %in% names(skill_data))) {
    missing_cols <- setdiff(required_cols_skill_data, names(skill_data))
    stop(paste("skill_data no tiene todas las columnas necesarias en classify_skills_network (Parte 1). Faltan:", paste(missing_cols, collapse=", ")))
  }
  if (calculate_flow_status && (is.null(occupation_attributes_harmonized) || !"SOC_Code_Harmonized_2018" %in% names(occupation_attributes_harmonized) || nrow(occupation_attributes_harmonized)==0 )) {
    warning("occupation_attributes_harmonized es NULL, vacío o no tiene 'SOC_Code_Harmonized_2018'. No se calculará estatus en classify_skills_network (Parte 1).")
    calculate_flow_status <- FALSE
  }
  years_to_process <- unique(c(year_to_analyze, if(!is.null(comparison_year)) comparison_year else NULL))
  years_available <- unique(skill_data$year)
  years_to_process <- intersect(years_to_process, years_available)
  if(length(years_to_process) == 0) { message("Ninguno de los años especificados tiene datos en classify_skills_network (Parte 1). Saliendo."); return(NULL) }
  message(sprintf("Años a procesar para clustering en classify_skills_network (Parte 1): %s", paste(years_to_process, collapse=", ")))
  results_list <- list()
  all_skills_map_dt <- unique(skill_data[, .(Element.Name)])
  setkey(all_skills_map_dt, Element.Name)
  for(current_year in years_to_process) {
    message(sprintf("\n=== Procesando Año %d en classify_skills_network (Parte 1) ===", current_year))
    dt_year <- skill_data[year == current_year]
    if(nrow(dt_year) == 0) { message(sprintf("No hay datos para año %d en classify_skills_network (Parte 1).", current_year)); next }
    message("Preparando datos y calculando RCA (usando Scale.ID == 'IM') (Parte 1)...")
    dt_year_rca_input <- dt_year[Scale.ID == "IM", .(SOC_Code_Harmonized_2018, Element.Name, Data.Value)]
    if(nrow(dt_year_rca_input) == 0) { message(sprintf("No hay datos con Scale.ID 'IM' para RCA en año %d en classify_skills_network (Parte 1).", current_year)); next }
    oc_s <- dt_year_rca_input[, .(os_sum_val = sum(Data.Value, na.rm = TRUE)), by = SOC_Code_Harmonized_2018]
    sk_s <- dt_year_rca_input[, .(ss_sum_val = sum(Data.Value, na.rm = TRUE)), by = Element.Name]
    tot_s_val <- sum(dt_year_rca_input$Data.Value, na.rm = TRUE)
    if (tot_s_val == 0) { message(sprintf("ERROR: Suma Data.Value cero para RCA en año %d en classify_skills_network (Parte 1). Saltando.", current_year)); next }
    dt_year_rca <- merge(dt_year_rca_input, oc_s, by = "SOC_Code_Harmonized_2018", all.x = TRUE)
    dt_year_rca <- merge(dt_year_rca, sk_s, by = "Element.Name", all.x = TRUE)
    dt_year_rca <- dt_year_rca[!is.na(os_sum_val) & !is.na(ss_sum_val) & os_sum_val > 1e-9 & ss_sum_val > 1e-9]
    if(nrow(dt_year_rca) == 0) { message(sprintf("ERROR: No quedan datos para RCA después de merges para año %d en classify_skills_network (Parte 1).", current_year)); next }
    dt_year_rca[, rca := (Data.Value / os_sum_val) / (ss_sum_val / tot_s_val)]
    dt_year_rca[is.infinite(rca) | is.nan(rca), rca := 0]
    dt_year_rca[, effective_use := rca > 1.0]
    skill_occ_effective <- dt_year_rca[effective_use == TRUE, .(SOC_Code_Harmonized_2018, Element.Name, rca)]
    skills_with_rca <- unique(skill_occ_effective$Element.Name)
    if(length(skills_with_rca) < 3) { message(sprintf("ERROR: Muy pocas skills con RCA > 1 para año %d (%d skills) en classify_skills_network (Parte 1). Saltando.", current_year, length(skills_with_rca))); next }
    message(sprintf("RCA calculado para %d skills con uso efectivo en año %d en classify_skills_network (Parte 1).", length(skills_with_rca), current_year))
    if(calculate_flow_status) {
      message("Calculando estatus de skills en classify_skills_network (Parte 1)...")
      skill_status_dt_year <- data.table(Element.Name = skills_with_rca)
      wage_col_name <- paste0("Median_Wage_", current_year)
      edu_col_name <- "Edu_Score_Weighted"
      if (wage_col_name %in% names(occupation_attributes_harmonized)) {
        occ_wage <- occupation_attributes_harmonized[!is.na(get(wage_col_name)), .(SOC_Code_Harmonized_2018, wage = get(wage_col_name))]
        if (nrow(occ_wage) > 0) {
          skill_occ_wage <- merge(skill_occ_effective, occ_wage, by = "SOC_Code_Harmonized_2018", allow.cartesian=TRUE)
          if (nrow(skill_occ_wage) > 0) {
            skill_wage_status <- skill_occ_wage[, .(wage_val = weighted.mean(wage, rca, na.rm = TRUE)), by = Element.Name]
            mean_w <- mean(skill_wage_status$wage_val, na.rm = TRUE); sd_w <- sd(skill_wage_status$wage_val, na.rm = TRUE)
            if (!is.na(sd_w) && sd_w > 0) { skill_wage_status[, wage_z := (wage_val - mean_w) / sd_w] } else { skill_wage_status[, wage_z := 0] }
            skill_status_dt_year <- merge(skill_status_dt_year, skill_wage_status[,.(Element.Name, wage_val, wage_z)], by = "Element.Name", all.x = TRUE)
          } else { message(sprintf("skill_occ_wage vacía para año %d en classify_skills_network (Parte 1).", current_year)) }
        } else { message(sprintf("occ_wage vacía para año %d en classify_skills_network (Parte 1).", current_year)) }
      } else { message(sprintf("Columna de salario %s no encontrada en occupation_attributes_harmonized para año %d (Parte 1).", wage_col_name, current_year)) }
      if (edu_col_name %in% names(occupation_attributes_harmonized)) {
        occ_edu <- occupation_attributes_harmonized[!is.na(get(edu_col_name)), .(SOC_Code_Harmonized_2018, edu = get(edu_col_name))]
        if (nrow(occ_edu) > 0) {
          skill_occ_edu <- merge(skill_occ_effective, occ_edu, by = "SOC_Code_Harmonized_2018", allow.cartesian=TRUE)
          if (nrow(skill_occ_edu) > 0) {
            skill_edu_status <- skill_occ_edu[, .(edu_val = weighted.mean(edu, rca, na.rm = TRUE)), by = Element.Name]
            mean_e <- mean(skill_edu_status$edu_val, na.rm = TRUE); sd_e <- sd(skill_edu_status$edu_val, na.rm = TRUE)
            if (!is.na(sd_e) && sd_e > 0) { skill_edu_status[, edu_z := (edu_val - mean_e) / sd_e] } else { skill_edu_status[, edu_z := 0] }
            skill_status_dt_year <- merge(skill_status_dt_year, skill_edu_status[,.(Element.Name, edu_val, edu_z)], by = "Element.Name", all.x = TRUE)
          } else { message(sprintf("skill_occ_edu vacía para año %d en classify_skills_network (Parte 1).", current_year)) }
        } else { message(sprintf("occ_edu vacía para año %d en classify_skills_network (Parte 1).", current_year)) }
      } else { message(sprintf("Columna de educación %s no encontrada en occupation_attributes_harmonized (Parte 1).", edu_col_name)) }
      has_wage_z <- "wage_z" %in% names(skill_status_dt_year); if (!has_wage_z) skill_status_dt_year[, wage_z := NA_real_]
      has_edu_z <- "edu_z" %in% names(skill_status_dt_year); if (!has_edu_z) skill_status_dt_year[, edu_z := NA_real_]
      if (has_wage_z && has_edu_z) {
        skill_status_dt_year[, composite_z := (fifelse(is.na(wage_z), 0, wage_z) + fifelse(is.na(edu_z), 0, edu_z)) / 2]
      } else if (has_wage_z) {
        skill_status_dt_year[, composite_z := fifelse(is.na(wage_z), 0, wage_z)]
      } else if (has_edu_z) {
        skill_status_dt_year[, composite_z := fifelse(is.na(edu_z), 0, edu_z)]
      } else { skill_status_dt_year[, composite_z := NA_real_] }
      if ("composite_z" %in% names(skill_status_dt_year) && !all(is.na(skill_status_dt_year$composite_z))) {
        skill_status_dt_year[, status_cat := cut(composite_z, breaks = c(-Inf, -0.5, 0.5, Inf), labels = c("LowStatus", "MediumStatus", "HighStatus"), include.lowest = TRUE, right = TRUE)]
        status_cols_to_rename <- setdiff(names(skill_status_dt_year), "Element.Name")
        setnames(skill_status_dt_year, status_cols_to_rename, paste0(status_cols_to_rename, "_", current_year))
        results_list[[paste0("status_", current_year)]] <- skill_status_dt_year
        message(sprintf("Estatus calculado para año %d en classify_skills_network (Parte 1).", current_year))
      } else { message(sprintf("No se pudo calcular 'composite_z' o 'status_cat' para %d en classify_skills_network (Parte 1).", current_year)) }
    }
    message("Construyendo red skill-skill y clusterizando en classify_skills_network (Parte 1)...")
    skill_occ_matrix_for_dcast <- skill_occ_effective[, .(Element.Name, SOC_Code_Harmonized_2018)]
    skill_occ_matrix <- dcast(skill_occ_matrix_for_dcast, Element.Name ~ SOC_Code_Harmonized_2018, fun.aggregate = function(x) if(length(x)>0) 1L else 0L, value.var = "SOC_Code_Harmonized_2018", fill = 0L)
    skill_names_year <- skill_occ_matrix[["Element.Name"]]
    skill_mat_year <- as.matrix(skill_occ_matrix[, -which(names(skill_occ_matrix) == "Element.Name"), with = FALSE])
    rownames(skill_mat_year) <- skill_names_year
    n_skills_current <- nrow(skill_mat_year)
    if(n_skills_current < 2) { message(sprintf("Menos de 2 skills en matriz para año %d. Saltando construcción de red (Parte 1).", current_year)); next }
    comp_matrix_current <- matrix(0, n_skills_current, n_skills_current, dimnames = list(skill_names_year, skill_names_year))
    skill_usage_current <- rowSums(skill_mat_year)
    pb_net_curr <- NULL
    if (n_skills_current > 10 && requireNamespace("progress", quietly = TRUE)) {
      pb_net_curr <- progress::progress_bar$new(total = n_skills_current, format = "Complementariedad [:bar] :percent ETA: :eta")
    }
    for(i in 1:n_skills_current) {
      if(!is.null(pb_net_curr)) pb_net_curr$tick()
      if(skill_usage_current[i] == 0) next
      for(j in (i+1):n_skills_current) {
        if(j > n_skills_current) break
        if(skill_usage_current[j] == 0) next
        both_count_current <- sum(skill_mat_year[i,] & skill_mat_year[j,])
        if(both_count_current > 0) {
          comp_val <- min(both_count_current / skill_usage_current[i], both_count_current / skill_usage_current[j])
          if (comp_val >= complementarity_threshold) {
            comp_matrix_current[i, j] <- comp_val
            comp_matrix_current[j, i] <- comp_val
          }
        }
      }
    }
    skill_graph_current <- igraph::graph_from_adjacency_matrix(comp_matrix_current, mode = "undirected", weighted = TRUE, diag = FALSE)
    message(sprintf("Grafo para clustering año %d (Parte 1): %d nodos, %d aristas (umbral complementariedad: %.1e)", current_year, igraph::vcount(skill_graph_current), igraph::ecount(skill_graph_current), complementarity_threshold))
    current_year_cluster_results <- data.table(Element.Name = skills_with_rca)
    if(igraph::vcount(skill_graph_current) < 2 || igraph::ecount(skill_graph_current) == 0) {
      message(sprintf("ADVERTENCIA: Grafo con <2 nodos o 0 aristas para clustering en año %d (Parte 1). No se realizará clustering.", current_year));
      current_year_cluster_results[, (paste0("LouvainCluster_", current_year)) := paste0("NoClusterGraph_", current_year)]
      current_year_cluster_results[, (paste0("LeidenCluster_", current_year)) := paste0("NoClusterGraph_", current_year)]
    } else {
      louvain_communities <- tryCatch({ igraph::cluster_louvain(skill_graph_current) }, error = function(e) { message(paste("Louvain falló para año", current_year,"(Parte 1):", e$message)); NULL })
      if(!is.null(louvain_communities) && length(igraph::V(skill_graph_current)$name) == length(louvain_communities$membership)) {
        dt_louvain_year <- data.table(Element.Name = igraph::V(skill_graph_current)$name, Louvain_ID = louvain_communities$membership)
        dt_louvain_year[, (paste0("LouvainCluster_", current_year)) := paste0("LouvainC_", Louvain_ID)][, Louvain_ID := NULL]
        current_year_cluster_results <- merge(current_year_cluster_results, dt_louvain_year, by="Element.Name", all.x=TRUE)
        message(sprintf("Louvain (Parte 1): %d comunidades encontradas para año %d.", length(unique(louvain_communities$membership)), current_year))
      } else {
        message(sprintf("Louvain clustering falló o inconsistente para año %d (Parte 1).", current_year))
        current_year_cluster_results[, (paste0("LouvainCluster_", current_year)) := paste0("LouvainFail_", current_year)]
      }
      leiden_communities <- tryCatch({ igraph::cluster_leiden(skill_graph_current, objective_function = "modularity", resolution_parameter = 1.0) }, error = function(e) { message(paste("Leiden falló para año", current_year,"(Parte 1):", e$message)); NULL })
      if(!is.null(leiden_communities) && length(igraph::V(skill_graph_current)$name) == length(leiden_communities$membership)) {
        dt_leiden_year <- data.table(Element.Name = igraph::V(skill_graph_current)$name, Leiden_ID = leiden_communities$membership)
        dt_leiden_year[, (paste0("LeidenCluster_", current_year)) := paste0("LeidenC_", Leiden_ID)][, Leiden_ID := NULL]
        current_year_cluster_results <- merge(current_year_cluster_results, dt_leiden_year, by="Element.Name", all.x=TRUE)
        message(sprintf("Leiden (Parte 1): %d comunidades encontradas para año %d.", length(unique(leiden_communities$membership)), current_year))
      } else {
        message(sprintf("Leiden clustering falló o inconsistente para año %d (Parte 1).", current_year))
        current_year_cluster_results[, (paste0("LeidenCluster_", current_year)) := paste0("LeidenFail_", current_year)]
      }
    }
    louvain_col_name <- paste0("LouvainCluster_", current_year)
    leiden_col_name <- paste0("LeidenCluster_", current_year)
    if(!louvain_col_name %in% names(current_year_cluster_results)) current_year_cluster_results[, (louvain_col_name) := paste0("NoClusterData_", current_year)]
    else current_year_cluster_results[is.na(get(louvain_col_name)), (louvain_col_name) := paste0("NoClusterData_", current_year)]
    if(!leiden_col_name %in% names(current_year_cluster_results)) current_year_cluster_results[, (leiden_col_name) := paste0("NoClusterData_", current_year)]
    else current_year_cluster_results[is.na(get(leiden_col_name)), (leiden_col_name) := paste0("NoClusterData_", current_year)]
    results_list[[paste0("cluster_results_", current_year)]] <- current_year_cluster_results
  }
  message("\n--- Combinando resultados de todos los años en classify_skills_network (Parte 1) ---")
  final_classification_dt <- all_skills_map_dt
  if(length(results_list) > 0) {
    for(res_table_name in names(results_list)){
      current_res_dt <- results_list[[res_table_name]]
      if("Element.Name" %in% names(current_res_dt) && nrow(current_res_dt) > 0){
        final_classification_dt <- merge(final_classification_dt, current_res_dt, by = "Element.Name", all.x = TRUE)
      } else { warning(paste("La tabla de resultados", res_table_name, "no tiene 'Element.Name' o está vacía. No se unirá en classify_skills_network (Parte 1).")) }
    }
  } else { message("No se generaron resultados de clustering o estatus en classify_skills_network (Parte 1).") }
  all_cluster_cols <- grep("Cluster_", names(final_classification_dt), value = TRUE)
  for (col in all_cluster_cols) {
    year_suffix_loop <- stringr::str_extract(col, "\\d{4}$")
    if (is.na(year_suffix_loop)) year_suffix_loop <- "UnknownYear" # Default if no year found
    if(col %in% names(final_classification_dt)){
      final_classification_dt[is.na(get(col)), (col) := paste0("NoCluster_", year_suffix_loop)]
    }
  }
  all_status_cat_cols <- grep("^status_cat_", names(final_classification_dt), value = TRUE)
  for (col in all_status_cat_cols) {
    year_suffix_loop <- stringr::str_extract(col, "\\d{4}$")
    if (is.na(year_suffix_loop)) year_suffix_loop <- "UnknownYear" # Default if no year found
    if(col %in% names(final_classification_dt)){
      final_classification_dt[is.na(get(col)), (col) := paste0("NoStatus_", year_suffix_loop)] 
    }
  }
  all_status_val_cols <- grep("(_z|_val)_\\d{4}$", names(final_classification_dt), value = TRUE) 
  for (col in all_status_val_cols) {
    if(col %in% names(final_classification_dt)){
      final_classification_dt[is.na(get(col)), (col) := 0] 
    }
  }
  message("--- Clasificación por Red (classify_skills_network) Completada (Parte 1) ---")
  message(sprintf("Tabla final de classify_skills_network (Parte 1) con %d skills y %d columnas.", nrow(final_classification_dt), ncol(final_classification_dt)))
  return(final_classification_dt)
}

# --- 5. CÁLCULO DE DISTANCIA ESTRUCTURAL ---
calculate_occupation_distances <- function(skill_vectors_input, distance_metric_selected = "jaccard_rca", binarization_threshold = 0, backbone_alpha = 0.05) {
  id_col_name <- "SOC_Code_Harmonized_2018"
  if (!id_col_name %in% names(skill_vectors_input)) {
    if ("O.NET.SOC.Code" %in% names(skill_vectors_input) && !("SOC_Code_Harmonized_2018" %in% names(skill_vectors_input))) {
      warning("calculate_occupation_distances: 'O.NET.SOC.Code' presente pero se esperaba 'SOC_Code_Harmonized_2018'. Verificar consistencia.")
      setnames(skill_vectors_input, "O.NET.SOC.Code", id_col_name)
    } else {
      stop(paste("calculate_occupation_distances (Parte 1): Se esperaba la columna '", id_col_name, "'."))
    }
  }
  
  if (is.null(skill_vectors_input) || nrow(skill_vectors_input) < 2 || !all(c(id_col_name, "Element.ID", "Data.Value") %in% names(skill_vectors_input))) {message("Datos de entrada insuficientes para calculate_occupation_distances (Parte 1)."); return(NULL)}
  skill_vectors_clean <- skill_vectors_input[!is.na(Data.Value) & !is.na(Element.ID) & !is.na(get(id_col_name))]
  skill_vectors_clean[, Data.Value := as.numeric(Data.Value)][!is.na(Data.Value)]
  if (nrow(skill_vectors_clean) < 2 || length(unique(skill_vectors_clean[[id_col_name]])) < 2) {message("Menos de 2 ocupaciones únicas o filas después de limpiar en calculate_occupation_distances (Parte 1)."); return(NULL)}
  
  formula_dcast <- as.formula(paste(id_col_name, "~ Element.ID"))
  occ_skill_matrix_dt <- tryCatch(dcast(skill_vectors_clean, formula_dcast, value.var = "Data.Value", fill = 0), error = function(e) {warning(paste("Error en dcast, calculate_occupation_distances (Parte 1):",e$message)); NULL})
  
  if (is.null(occ_skill_matrix_dt) || nrow(occ_skill_matrix_dt) <= 1 || ncol(occ_skill_matrix_dt) <= 1) {message("Matriz ocupación-skill inválida después de dcast en calculate_occupation_distances (Parte 1)."); return(NULL)}
  row_sums_check <- rowSums(occ_skill_matrix_dt[, .SD, .SDcols = -c(id_col_name)]); if (any(row_sums_check == 0)) { occ_skill_matrix_dt <- occ_skill_matrix_dt[row_sums_check > 0]; message(sprintf("%d ocupaciones eliminadas por tener suma de skills cero (Parte 1).", sum(row_sums_check == 0))) }
  if (nrow(occ_skill_matrix_dt) <= 1) {message("Menos de 2 ocupaciones después de filtrar por suma de skills cero (Parte 1)."); return(NULL)}
  occ_codes_in_matrix <- occ_skill_matrix_dt[[id_col_name]]
  skill_matrix_for_dist <- tryCatch(as.matrix(occ_skill_matrix_dt[, .SD, .SDcols = -c(id_col_name)]), error = function(e) {warning(paste("Error convirtiendo a matriz, calculate_occupation_distances (Parte 1):",e$message)); NULL})
  if (is.null(skill_matrix_for_dist) || nrow(skill_matrix_for_dist) < 2 || ncol(skill_matrix_for_dist) < 1) {message("Matriz de skills para distancia inválida (Parte 1)."); return(NULL)}
  skill_matrix_for_dist[!is.finite(skill_matrix_for_dist)] <- 0; rownames(skill_matrix_for_dist) <- occ_codes_in_matrix
  dist_matrix_output <- NULL; intended_dimnames <- list(occ_codes_in_matrix, occ_codes_in_matrix)
  tryCatch({
    if (distance_metric_selected == "cosine") {
      sim_mat_cos <- lsa::cosine(t(skill_matrix_for_dist)); sim_mat_cos[!is.finite(sim_mat_cos)] <- 0; dist_matrix_output <- 1 - sim_mat_cos
    } else if (distance_metric_selected == "jaccard") {
      skill_mat_bin <- skill_matrix_for_dist > binarization_threshold; if (nrow(skill_mat_bin) < 2) stop("Jaccard necesita >=2 ocupaciones (Parte 1)."); dist_obj_jaccard <- stats::dist(skill_mat_bin, method = "binary"); dist_matrix_output <- as.matrix(dist_obj_jaccard)
    } else if (distance_metric_selected == "jaccard_rca") {
      rca_matrix <- matrix(0, nrow(skill_matrix_for_dist), ncol(skill_matrix_for_dist)); skill_totals_rca <- colSums(skill_matrix_for_dist); occ_totals_rca <- rowSums(skill_matrix_for_dist); grand_total_rca <- sum(skill_matrix_for_dist)
      if(any(occ_totals_rca==0)) occ_totals_rca[occ_totals_rca==0] <- 1e-9; if(any(skill_totals_rca==0)) skill_totals_rca[skill_totals_rca==0] <- 1e-9; if(grand_total_rca==0) grand_total_rca <- 1e-9
      for (i in 1:nrow(skill_matrix_for_dist)) for (j in 1:ncol(skill_matrix_for_dist)) if (occ_totals_rca[i] > 0 && skill_totals_rca[j] > 0) rca_matrix[i, j] <- (skill_matrix_for_dist[i, j] / occ_totals_rca[i]) / (skill_totals_rca[j] / grand_total_rca)
      rca_matrix[!is.finite(rca_matrix)] <- 0; binary_rca <- rca_matrix > 1.0; rownames(binary_rca) <- occ_codes_in_matrix
      if(ncol(binary_rca) == 0 || nrow(binary_rca) < 2) {message("Matriz RCA binaria vacía o con pocas filas (Parte 1)."); dist_matrix_output <- matrix(0.5, length(occ_codes_in_matrix), length(occ_codes_in_matrix), dimnames = intended_dimnames)}
      else dist_matrix_output <- as.matrix(stats::dist(binary_rca, method = "binary"))
    } else {warning(paste("Métrica de distancia no soportada (Parte 1):", distance_metric_selected)); dist_matrix_output <- NULL}
  }, error = function(e) {warning(paste("Error calculando distancia (Parte 1):", e$message)); dist_matrix_output <<- NULL})
  if (is.null(dist_matrix_output)) {message("dist_matrix_output es NULL (Parte 1)."); return(NULL)}
  if (is.matrix(dist_matrix_output) && nrow(dist_matrix_output) > 0) {
    if(!identical(rownames(dist_matrix_output), occ_codes_in_matrix) || !identical(colnames(dist_matrix_output), occ_codes_in_matrix)) dimnames(dist_matrix_output) <- intended_dimnames
    if (!isSymmetric(dist_matrix_output, tol = 1e-8)) dist_matrix_output <- (dist_matrix_output + t(dist_matrix_output)) / 2
    diag(dist_matrix_output) <- 0; dist_matrix_output[dist_matrix_output < 0] <- 0; dist_matrix_output[dist_matrix_output > 1] <- 1
    if (any(!is.finite(dist_matrix_output))) { mean_dist_val <- mean(dist_matrix_output[is.finite(dist_matrix_output)], na.rm=TRUE); if(is.na(mean_dist_val) || !is.finite(mean_dist_val)) mean_dist_val <- 0.5; dist_matrix_output[!is.finite(dist_matrix_output)] <- mean_dist_val; diag(dist_matrix_output) <- 0 }
  } else {message("Matriz de distancia final inválida (Parte 1)."); return(NULL)}
  return(dist_matrix_output)
}

# --- 6. CREACIÓN DE RED DE DIFUSIÓN BINARIA ---
create_binary_diffusion_network <- function(skills_data_input_harmonized, occupation_attributes_global_harmonized, start_year_arg, end_year_arg, skill_classification_input = NULL, distance_metric_structural = "jaccard_rca", generate_negatives = TRUE, negative_sampling_ratio = 1.0) {
  message("Iniciando creación de red de difusión binaria (Parte 1)...")
  if (is.null(skills_data_input_harmonized) || !"SOC_Code_Harmonized_2018" %in% names(skills_data_input_harmonized) || nrow(skills_data_input_harmonized)==0) stop("skills_data_input_harmonized inválido o vacío en create_binary_diffusion_network (Parte 1).")
  if (is.null(occupation_attributes_global_harmonized) || !"SOC_Code_Harmonized_2018" %in% names(occupation_attributes_global_harmonized) || nrow(occupation_attributes_global_harmonized)==0) stop("occupation_attributes_global_harmonized inválido o vacío en create_binary_diffusion_network (Parte 1).")
  dt <- as.data.table(skills_data_input_harmonized)
  req_s_cols <- c("SOC_Code_Harmonized_2018", "Element.Name", "Scale.ID", "year", "Data.Value"); if (!all(req_s_cols %in% names(dt))) stop(paste("Faltan columnas en skills_data_input_harmonized para create_binary_diffusion_network (Parte 1):", paste(setdiff(req_s_cols, names(dt)), collapse=", ")))
  dt <- dt[year %in% c(start_year_arg, end_year_arg)][!is.na(SOC_Code_Harmonized_2018) & !is.na(Element.Name) & !is.na(Data.Value)][, Data.Value := as.numeric(Data.Value)][!is.na(Data.Value)]
  dt <- dt[Scale.ID == "IM"]
  if(nrow(dt) == 0) stop("No hay datos de skills con Scale.ID 'IM' para los años seleccionados en create_binary_diffusion_network (Parte 1).")
  dt_unique_vals <- dt[, .(Data.Value = mean(Data.Value, na.rm = TRUE)), by = .(SOC_Code_Harmonized_2018, Element.Name, year)]; if (nrow(dt_unique_vals) == 0) stop("No hay datos skills post-filtrado y promediado en create_binary_diffusion_network (Parte 1).")
  rca_list <- list()
  tryCatch({ for (yr_r in c(start_year_arg, end_year_arg)) {
    yr_d_r <- dt_unique_vals[year == yr_r]; if (nrow(yr_d_r) == 0) {message(sprintf("Sin datos para calcular RCA en año %d (Parte 1).", yr_r)); next}
    oc_s <- yr_d_r[, .(os_sum_val = sum(Data.Value, na.rm = TRUE)), by = SOC_Code_Harmonized_2018]; sk_s <- yr_d_r[, .(ss_sum_val = sum(Data.Value, na.rm = TRUE)), by = Element.Name]; tot_s_val <- sum(yr_d_r$Data.Value, na.rm = TRUE)
    if (tot_s_val == 0) {message(sprintf("Suma Data.Value cero para RCA en año %d (Parte 1). Saltando.", yr_r)); next}
    yr_d_r <- merge(merge(yr_d_r, oc_s, by = "SOC_Code_Harmonized_2018", all.x=TRUE), sk_s, by="Element.Name", all.x=TRUE)[!is.na(os_sum_val)&!is.na(ss_sum_val)&os_sum_val>1e-9&ss_sum_val>1e-9]
    if (nrow(yr_d_r) == 0) {message(sprintf("Sin filas post-merge para RCA en año %d (Parte 1).", yr_r)); next}
    yr_d_r[, rca := (Data.Value / os_sum_val) / (ss_sum_val / tot_s_val)][is.infinite(rca)|is.nan(rca), rca:=0][,effective_use := rca > 1.0]; rca_list[[as.character(yr_r)]] <- yr_d_r
  }}, error = function(e) stop(paste("Error calculando RCA en create_binary_diffusion_network (Parte 1):", e$message)))
  if (length(rca_list) < 2 || !as.character(start_year_arg)%in%names(rca_list) || !as.character(end_year_arg)%in%names(rca_list)) {message("No se calculó RCA para ambos años en create_binary_diffusion_network (Parte 1). No se pueden generar eventos."); return(data.table())}
  dt_rca_all <- rbindlist(rca_list, fill=TRUE); data_orig_rca <- dt_rca_all[year == start_year_arg]; data_adop_rca <- dt_rca_all[year == end_year_arg]
  if (nrow(data_orig_rca)==0 || nrow(data_adop_rca)==0) {message("Sin datos RCA en origen o adopción (Parte 1). No se pueden generar eventos."); return(data.table())}
  all_relevant_occs_harmonized <- unique(c(data_orig_rca$SOC_Code_Harmonized_2018, data_adop_rca$SOC_Code_Harmonized_2018))
  if (!is.null(skill_classification_input) && nrow(skill_classification_input) > 0) {
    if (!all(c("Element.Name", "SkillType") %in% names(skill_classification_input))) warning("skill_classification_input sin 'Element.Name' y 'SkillType' en create_binary_diffusion_network (Parte 1).")
    else { temp_class_map <- unique(skill_classification_input[, .(Element.Name, SkillType)]); if ("SkillType"%in%names(data_orig_rca)) data_orig_rca[,SkillType:=NULL]; if ("SkillType"%in%names(data_adop_rca)) data_adop_rca[,SkillType:=NULL]; data_orig_rca <- merge(data_orig_rca, temp_class_map, by="Element.Name", all.x=TRUE); data_adop_rca <- merge(data_adop_rca, temp_class_map, by="Element.Name", all.x=TRUE); message("Clasificación de skills desde input añadida en create_binary_diffusion_network (Parte 1).")}}
  if (!"SkillType"%in%names(data_orig_rca)) data_orig_rca[,SkillType:="no_clasificada"]; if (!"SkillType"%in%names(data_adop_rca)) data_adop_rca[,SkillType:="no_clasificada"]
  data_orig_rca[is.na(SkillType),SkillType:="no_clasificada"]; data_adop_rca[is.na(SkillType),SkillType:="no_clasificada"]
  all_skills_unique_names <- unique(c(data_orig_rca$Element.Name, data_adop_rca$Element.Name)); dist_s_matrix <- NULL
  profiles_end_year <- dt_unique_vals[year==end_year_arg & SOC_Code_Harmonized_2018%in%all_relevant_occs_harmonized & Data.Value>0, .(SOC_Code_Harmonized_2018, Element.ID=Element.Name, Data.Value)]
  occs_needing_fallback <- setdiff(all_relevant_occs_harmonized, unique(profiles_end_year$SOC_Code_Harmonized_2018))
  profiles_start_year_fallback <- if(length(occs_needing_fallback)>0) dt_unique_vals[year==start_year_arg & SOC_Code_Harmonized_2018%in%occs_needing_fallback & Data.Value>0, .(SOC_Code_Harmonized_2018, Element.ID=Element.Name, Data.Value)] else data.table()
  skill_vecs_dist <- unique(rbindlist(list(profiles_end_year, profiles_start_year_fallback), use.names=TRUE, fill=TRUE), by=c("SOC_Code_Harmonized_2018", "Element.ID"))[!is.na(SOC_Code_Harmonized_2018)&!is.na(Element.ID)]
  if (nrow(skill_vecs_dist)>1 && length(unique(skill_vecs_dist$SOC_Code_Harmonized_2018))>1) dist_s_matrix <- calculate_occupation_distances(skill_vecs_dist, distance_metric_structural) else warning("No se calculará distancia estructural (Parte 1): datos insuficientes en skill_vecs_dist.")
  pb <- NULL; if (requireNamespace("progress",quietly=TRUE) && length(all_skills_unique_names)>10) pb <- progress::progress_bar$new(total=length(all_skills_unique_names), format="Eventos [:bar] :percent")
  diffusion_events <- list()
  for (skill_name_iter in all_skills_unique_names) { tryCatch({ if(!is.null(pb)) pb$tick(); occs_orig_eff <- unique(data_orig_rca[Element.Name==skill_name_iter & effective_use==TRUE, SOC_Code_Harmonized_2018]); occs_adop_eff <- unique(data_adop_rca[Element.Name==skill_name_iter & effective_use==TRUE, SOC_Code_Harmonized_2018]); occs_new_adop <- setdiff(occs_adop_eff, occs_orig_eff); sk_type_val <- unique(data_orig_rca[Element.Name==skill_name_iter, SkillType]); sk_type <- if(length(sk_type_val)==0||is.na(sk_type_val[1])) "no_clasif_evento" else sk_type_val[1]
  if(length(occs_orig_eff)>0 && length(occs_new_adop)>0){ data_o_v<-data_orig_rca[Element.Name==skill_name_iter & SOC_Code_Harmonized_2018%in%occs_orig_eff,.(SOC_Code_Harmonized_2018,dvs=Data.Value)]; data_a_v<-data_adop_rca[Element.Name==skill_name_iter & SOC_Code_Harmonized_2018%in%occs_new_adop,.(SOC_Code_Harmonized_2018,dvt=Data.Value)]; ev_p<-CJ(source=occs_orig_eff,target=occs_new_adop)[source!=target]; if(nrow(ev_p)>0){ev_p[,`:=`(skill_name=skill_name_iter,skill_type=sk_type,diffusion=1L,year_emission=start_year_arg,year_adoption=end_year_arg)]; ev_p<-merge(merge(ev_p,data_o_v,by.x="source",by.y="SOC_Code_Harmonized_2018",all.x=TRUE),data_a_v,by.x="target",by.y="SOC_Code_Harmonized_2018",all.x=TRUE); setnames(ev_p,c("dvs","dvt"),c("data_value_source","data_value_target")); ev_p[is.na(data_value_source),data_value_source:=0][is.na(data_value_target),data_value_target:=0]; diffusion_events<-c(diffusion_events,list(ev_p))}}
  if(generate_negatives && length(occs_orig_eff)>0){ occs_pot_non_adop<-setdiff(all_relevant_occs_harmonized,occs_adop_eff); if(length(occs_pot_non_adop)>0){ev_n<-CJ(source=occs_orig_eff,target=occs_pot_non_adop)[source!=target]; if(nrow(ev_n)>0){ev_n[,`:=`(skill_name=skill_name_iter,skill_type=sk_type,diffusion=0L,year_emission=start_year_arg,year_adoption=end_year_arg)];data_o_v_n<-data_orig_rca[Element.Name==skill_name_iter & SOC_Code_Harmonized_2018%in%occs_orig_eff,.(SOC_Code_Harmonized_2018,dvs=Data.Value)];data_a_v_n<-data_adop_rca[Element.Name==skill_name_iter & SOC_Code_Harmonized_2018%in%occs_pot_non_adop,.(SOC_Code_Harmonized_2018,dvt=Data.Value)];ev_n<-merge(merge(ev_n,data_o_v_n,by.x="source",by.y="SOC_Code_Harmonized_2018",all.x=TRUE),data_a_v_n,by.x="target",by.y="SOC_Code_Harmonized_2018",all.x=TRUE);setnames(ev_n,c("dvs","dvt"),c("data_value_source","data_value_target"));ev_n[is.na(data_value_source),data_value_source:=0][is.na(data_value_target),data_value_target:=0];diffusion_events<-c(diffusion_events,list(ev_n))}}}}
  , error=function(e)warning(paste("Error procesando skill",skill_name_iter,"en create_binary_diffusion_network (Parte 1):",e$message)))}
  if(length(diffusion_events)==0){message("No se generaron eventos de difusión/no difusión (Parte 1).");return(data.table())}; all_events<-rbindlist(diffusion_events,fill=TRUE); if(nrow(all_events)==0){message("Tabla 'all_events' vacía después de rbindlist (Parte 1).");return(data.table())}
  message(sprintf("Eventos totales generados (positivos y negativos) (Parte 1): %s (+:%s, -:%s)", format(nrow(all_events),big.mark=","),format(sum(all_events$diffusion==1,na.rm=TRUE),big.mark=","),format(sum(all_events$diffusion==0,na.rm=TRUE),big.mark=",")))
  attrs_use<-copy(occupation_attributes_global_harmonized);edu_col<-"Edu_Score_Weighted";wage_s_col_name<-paste0("Median_Wage_",start_year_arg);wage_e_col_name<-paste0("Median_Wage_",end_year_arg)
  s_attrs<-if(nrow(attrs_use)>0)attrs_use[,.SD,.SDcols=intersect(c("SOC_Code_Harmonized_2018",edu_col,wage_s_col_name),names(attrs_use))]else data.table()
  if(edu_col%in%names(s_attrs))setnames(s_attrs,edu_col,"source_education",skip_absent=TRUE);if(wage_s_col_name%in%names(s_attrs))setnames(s_attrs,wage_s_col_name,"source_wage",skip_absent=TRUE)
  t_attrs<-if(nrow(attrs_use)>0)attrs_use[,.SD,.SDcols=intersect(c("SOC_Code_Harmonized_2018",edu_col,wage_e_col_name),names(attrs_use))]else data.table()
  if(edu_col%in%names(t_attrs))setnames(t_attrs,edu_col,"target_education",skip_absent=TRUE);if(wage_e_col_name%in%names(t_attrs))setnames(t_attrs,wage_e_col_name,"target_wage",skip_absent=TRUE)
  if(nrow(s_attrs)>0&&"SOC_Code_Harmonized_2018"%in%names(s_attrs)&&"source"%in%names(all_events))all_events<-merge(all_events,s_attrs,by.x="source",by.y="SOC_Code_Harmonized_2018",all.x=TRUE)else{if(!("source_education"%in%names(all_events)))all_events[,source_education:=NA_real_];if(!("source_wage"%in%names(all_events)))all_events[,source_wage:=NA_real_]}
  if(nrow(t_attrs)>0&&"SOC_Code_Harmonized_2018"%in%names(t_attrs)&&"target"%in%names(all_events))all_events<-merge(all_events,t_attrs,by.x="target",by.y="SOC_Code_Harmonized_2018",all.x=TRUE)else{if(!("target_education"%in%names(all_events)))all_events[,target_education:=NA_real_];if(!("target_wage"%in%names(all_events)))all_events[,target_wage:=NA_real_]}
  if(!is.null(dist_s_matrix)&&is.matrix(dist_s_matrix)&&nrow(dist_s_matrix)>0){med_dist<-median(dist_s_matrix[is.finite(dist_s_matrix)],na.rm=TRUE);if(is.na(med_dist)||!is.finite(med_dist))med_dist<-0.5;dist_dt<-as.data.table(dist_s_matrix,keep.rownames="s_H");dist_long<-melt(dist_dt,id.vars="s_H",variable.name="t_H",value.name="s_d_val",variable.factor=FALSE);setkeyv(dist_long,c("s_H","t_H"));all_events[,s_key:=as.character(source)][,t_key:=as.character(target)];setkeyv(all_events,c("s_key","t_key"));if("structural_distance"%in%names(all_events))all_events[,structural_distance:=NULL];all_events<-merge(all_events,dist_long,by.x=c("s_key","t_key"),by.y=c("s_H","t_H"),all.x=TRUE);all_events[is.na(s_d_val)|!is.finite(s_d_val),s_d_val:=med_dist];setnames(all_events,"s_d_val","structural_distance");all_events[,c("s_key","t_key"):=NULL]}else all_events[,structural_distance:=0.5]
  for(dci in c("education","wage")){s_c<-paste0("source_",dci);t_c<-paste0("target_",dci);d_abs<-paste0(dci,"_diff_abs");d_rel<-paste0(dci,"_diff_rel");if(all(c(s_c,t_c)%in%names(all_events))){if(is.numeric(all_events[[s_c]])&&is.numeric(all_events[[t_c]])){all_events[,(d_abs):=get(t_c)-get(s_c)];all_events[,(d_rel):=fifelse(abs(get(s_c))>1e-9,(get(t_c)/get(s_c))-1,NA_real_)]}else all_events[,c(d_abs,d_rel):=NA_real_]}else all_events[,c(d_abs,d_rel):=NA_real_]}
  cols_rem<-intersect(c("rca","effective_use","O.NET.SOC.Code","os_sum_val","ss_sum_val","Data.Value"),names(all_events));if(length(cols_rem)>0)all_events[,(cols_rem):=NULL]
  message("Creación base de eventos (create_binary_diffusion_network) completa (Parte 1).");setkey(all_events,NULL);if(nrow(all_events)>1e6)message("Dataset 'all_events' MUY GRANDE (Parte 1):",format(nrow(all_events),big.mark=","),"filas.");return(all_events)
}

# --- Bloque de Ejecución: Crear y Guardar la Base de Datos de Eventos ---
message("\nPARTE 1: --- Bloque de Ejecución: Creando Base de Datos 'all_events_final_enriched' (Múltiples Clasificaciones) ---")

if (!exists("onet2019_to_soc2018_map") || !exists("soc2010_to_soc2018_map") || nrow(onet2019_to_soc2018_map)==0) {
  stop("Crosswalks esenciales no cargados (Parte 1). Revisa PASO 2.B.")
}
if (!exists("all_skills_data") || !exists("occupation_attributes_final") || !is.data.table(all_skills_data) || nrow(all_skills_data)==0 || !is.data.table(occupation_attributes_final) || nrow(occupation_attributes_final)==0) {
  stop("Datos 'all_skills_data' u 'occupation_attributes_final' no disponibles (Parte 1). Ejecuta PASO 3.")
}

analysis_start_year <- 2015
analysis_end_year <- 2023

years_in_data <- unique(all_skills_data$year)
if (!analysis_start_year %in% years_in_data || !analysis_end_year %in% years_in_data) {
  stop(paste("Años", analysis_start_year, "o", analysis_end_year, "no están en 'all_skills_data' (Parte 1). Años disponibles:", paste(sort(years_in_data), collapse=", ")))
}

message("Ejecutando clasificación por red (classify_skills_network) UNA VEZ para obtener todas las combinaciones (Parte 1)...")
skill_classification_output_raw <- classify_skills_network(
  skill_data = all_skills_data,
  year_to_analyze = analysis_end_year,
  comparison_year = analysis_start_year,
  complementarity_threshold = 1e-9,
  calculate_flow_status = TRUE,
  occupation_attributes_harmonized = occupation_attributes_final
)

if(is.null(skill_classification_output_raw) || nrow(skill_classification_output_raw) == 0 ){
  stop("skill_classification_output_raw (con Louvain/Leiden) está vacía o es NULL (Parte 1). No se puede continuar.")
}

classif_full_filename <- file.path(output_data_dir_p1, "skill_classification_FULL_LouvainLeiden_AllYears.csv")
message("Guardando salida COMPLETA de clasificación (Louvain & Leiden para todos los años procesados) en (Parte 1): ", classif_full_filename)
fwrite(skill_classification_output_raw, classif_full_filename)
message("Tabla de clasificación guardada (Parte 1). Se procederá a usarla para generar los eventos.")

col_main_cluster_for_model <- paste0("LouvainCluster_", analysis_start_year)
skill_classification_for_main_model <- data.table()

if (col_main_cluster_for_model %in% names(skill_classification_output_raw) && "Element.Name" %in% names(skill_classification_output_raw) ) {
  message(sprintf("Preparando 'skill_classification_for_main_model' usando clústeres '%s' (Parte 1).", col_main_cluster_for_model))
  skill_classification_for_main_model <- skill_classification_output_raw[,
                                                                         .(Element.Name = Element.Name,
                                                                           SkillType = get(col_main_cluster_for_model))]
  skill_classification_for_main_model <- skill_classification_for_main_model[!is.na(SkillType) & !SkillType %like% "NoCluster" & !SkillType %like% "Fail"]
  message(sprintf("Se usarán %d skills con clasificación '%s' válida para la columna 'skill_type' principal (Parte 1).",
                  nrow(skill_classification_for_main_model), col_main_cluster_for_model))
  if(nrow(skill_classification_for_main_model)>0) {
    message("Resumen de SkillType principal a usar en create_binary_diffusion_network (Parte 1):")
    print(skill_classification_for_main_model[,.N, by=SkillType][order(-N)])
  } else {
    message("ADVERTENCIA: No hay skills con clasificación principal válida (Parte 1). 'skill_type' en all_events_final_base será 'no_clasificada'.")
    skill_classification_for_main_model <- data.table(Element.Name=character(0), SkillType=character(0))
  }
} else {
  warning(sprintf("Columna '%s' o 'Element.Name' no encontrada en skill_classification_output_raw (Parte 1). 'skill_type' será 'no_clasificada'.", col_main_cluster_for_model))
  skill_classification_for_main_model <- data.table(Element.Name=character(0), SkillType=character(0))
}

message("\n--- Creando la base de eventos de difusión (all_events_final_base) (Parte 1) ---")
all_events_final_base <- create_binary_diffusion_network(
  skills_data_input_harmonized = all_skills_data,
  occupation_attributes_global_harmonized = occupation_attributes_final,
  start_year_arg = analysis_start_year,
  end_year_arg = analysis_end_year,
  skill_classification_input = skill_classification_for_main_model,
  distance_metric_structural = "jaccard_rca",
  generate_negatives = TRUE
)

if (exists("skill_classification_for_main_model")) {
  rm(skill_classification_for_main_model)
  message("'skill_classification_for_main_model' eliminada de memoria (Parte 1).")
  gc()
}

if (is.null(all_events_final_base) || nrow(all_events_final_base) == 0) {
  stop("La generación de 'all_events_final_base' falló o resultó en una tabla vacía (Parte 1). No se puede continuar.")
}
message(sprintf("'all_events_final_base' creada con %d filas (Parte 1). Procediendo a enriquecerla.", nrow(all_events_final_base)))


message("\n--- Enriqueciendo la base de eventos con todas las clasificaciones de skills (Louvain/Leiden 2015/2023 y estatus) (Parte 1) ---")
cols_to_merge_from_raw <- c("Element.Name",
                            grep("^LouvainCluster_", names(skill_classification_output_raw), value = TRUE),
                            grep("^LeidenCluster_", names(skill_classification_output_raw), value = TRUE),
                            grep("^status_cat_", names(skill_classification_output_raw), value = TRUE),
                            grep("^composite_z_", names(skill_classification_output_raw), value = TRUE),
                            grep("^wage_val_", names(skill_classification_output_raw), value = TRUE),
                            grep("^edu_val_", names(skill_classification_output_raw), value = TRUE),
                            grep("^wage_z_", names(skill_classification_output_raw), value = TRUE),
                            grep("^edu_z_", names(skill_classification_output_raw), value = TRUE)
)
cols_to_merge_from_raw <- unique(cols_to_merge_from_raw)
skill_classifications_to_join <- skill_classification_output_raw[, ..cols_to_merge_from_raw]
message(sprintf("'skill_classifications_to_join' creada con %d columnas para el merge (Parte 1).", ncol(skill_classifications_to_join)))


if (exists("skill_classification_output_raw")) {
  rm(skill_classification_output_raw)
  message("'skill_classification_output_raw' eliminada de memoria (Parte 1) (ya fue guardada y procesada).")
  gc()
}

all_events_final_enriched <- data.table()

if ("skill_name" %in% names(all_events_final_base) && "Element.Name" %in% names(skill_classifications_to_join) && nrow(all_events_final_base) > 0) {
  all_events_final_enriched <- merge(
    all_events_final_base,
    skill_classifications_to_join,
    by.x = "skill_name",
    by.y = "Element.Name",
    all.x = TRUE
  )
  message(sprintf("Merge completado (Parte 1). 'all_events_final_enriched' ahora tiene %d filas y %d columnas.", nrow(all_events_final_enriched), ncol(all_events_final_enriched)))
  
  rm(all_events_final_base)
  rm(skill_classifications_to_join)
  message("'all_events_final_base' y 'skill_classifications_to_join' eliminadas de memoria (Parte 1).")
  gc()
  
} else {
  warning("No se pudieron unir todas las clasificaciones a la tabla de eventos (Parte 1). 'skill_name' o 'Element.Name' podrían faltar, o 'all_events_final_base' podría estar vacía. Se guardará 'all_events_final_base' sin enriquecimiento completo si existe.")
  if(exists("all_events_final_base") && nrow(all_events_final_base) > 0) {
    all_events_final_enriched <- all_events_final_base
  } else {
    all_events_final_enriched <- data.table()
  }
  if (exists("all_events_final_base")) rm(all_events_final_base)
  if (exists("skill_classifications_to_join")) rm(skill_classifications_to_join)
  gc()
}


if (!is.null(all_events_final_enriched) && nrow(all_events_final_enriched) > 0) {
  rds_filename_p1 <- file.path(output_data_dir_p1, "all_diffusion_events_ENRICHED_AllClassifications.rds")
  csv_filename_p1 <- file.path(output_data_dir_p1, "all_diffusion_events_ENRICHED_AllClassifications.csv")
  
  message("\nGuardando base de datos de eventos ENRIQUECIDA Y COMPLETA (Parte 1)...")
  message("  Contiene 'skill_type' (Louvain 2015 por defecto) y columnas separadas para otras clasificaciones y estatus.")
  message("  Archivo RDS (Parte 1): ", rds_filename_p1)
  saveRDS(all_events_final_enriched, rds_filename_p1)
  message("  Archivo CSV (Parte 1): ", csv_filename_p1)
  tryCatch({ fwrite(all_events_final_enriched, csv_filename_p1, na = "NA") },
           error = function(e){ warning(paste("Error guardando CSV (Parte 1):", e$message), call. = FALSE) })
  
  message("\n--- Base de datos 'all_events_final_enriched' creada y guardada (Parte 1). ---")
  if("diffusion" %in% names(all_events_final_enriched)) {
    message("Resumen de la columna 'diffusion' (Parte 1):")
    print(table(all_events_final_enriched$diffusion, useNA="ifany"))
  }
  if("skill_type" %in% names(all_events_final_enriched)) {
    message("Resumen de la columna 'skill_type' principal (ej. Louvain 2015) en 'all_events_final_enriched' (Parte 1):")
    print(table(all_events_final_enriched$skill_type, useNA="ifany"))
  }
  
  cols_to_summarize_p1 <- names(all_events_final_enriched)[grep("^(LouvainCluster_|LeidenCluster_|status_cat_)", names(all_events_final_enriched))]
  for(col_p1 in cols_to_summarize_p1){
    if(col_p1 %in% names(all_events_final_enriched)){
      message(sprintf("Resumen de la columna '%s' (Parte 1):", col_p1))
      print(table(all_events_final_enriched[[col_p1]], useNA = "ifany"))
    }
  }
  
} else {
  warning("Tabla final 'all_events_final_enriched' vacía o NULL (Parte 1). No se guardó nada.")
}

message("\n\n=== PROCESAMIENTO DE PARTE 1 COMPLETADO ===")

if(exists("all_events_final_enriched") && is.data.table(all_events_final_enriched) && nrow(all_events_final_enriched) > 0) {
  message("\n--- Glimpse de 'all_events_final_enriched' (primeras columnas) (Parte 1) ---")
  num_cols_to_glimpse_p1 <- min(10, ncol(all_events_final_enriched))
  if(requireNamespace("dplyr", quietly = TRUE)) {
    dplyr::glimpse(all_events_final_enriched[, 1:num_cols_to_glimpse_p1, with = FALSE])
  } else {
    str(all_events_final_enriched[, 1:num_cols_to_glimpse_p1, with = FALSE])
  }
  message(sprintf("Total columnas en 'all_events_final_enriched' (Parte 1): %d", ncol(all_events_final_enriched)))
} else {
  message("all_events_final_enriched no existe o está vacía para el glimpse/str (Parte 1).")
}

# Opcional: Limpiar datos base si no se necesitan más en la sesión
# rm(all_skills_data, occupation_attributes_final)
# rm(onet2019_to_soc2018_map, onet2010_to_onet2019_map, soc2010_to_soc2018_map)
# gc()
# message("Datos base y crosswalks limpiados de memoria (opcional) (Parte 1).")


# =======================================================================
# PARTE 2: PROCESAMIENTO DE DATOS CPS Y UNIÓN CON EVENTOS DE DIFUSIÓN
# Versión: 1.0.8_CPS_Integration_CrosswalkFix_Cartesian
# - Carga 'all_events_final_enriched' (memoria/RDS).
# - Corregida la carga del crosswalk OCC2010 a SOC2010 para usar índices.
# - Añadido allow.cartesian=TRUE a los merges con soc2010_to_soc2018_map_cps.
# - Lógica de procesamiento de CPS revisada: agrega flujos por OCC2010 primero, luego armoniza.
# - Mensajes de diagnóstico detallados para el procesamiento de CPS.
# - Guarda la tabla combinada final.
# =======================================================================
gc()
# --- 0. CONFIGURACIÓN INICIAL Y CARGA DE PAQUETES ---
message("\n\nPARTE 2: PASO 0: Cargando librerías necesarias...")
packages_to_load_p2 <- c("data.table", "readxl", "dplyr", "ipumsr", "stringr") 

for (pkg_p2 in packages_to_load_p2) {
  if (!requireNamespace(pkg_p2, quietly = TRUE)) {
    message(paste("Instalando paquete (Parte 2):", pkg_p2, "..."))
    tryCatch({ install.packages(pkg_p2, dependencies = TRUE) }, error = function(e){ warning(paste("Fallo al instalar (Parte 2):", pkg_p2, "-", e$message), call. = FALSE)})
  }
  if (!require(pkg_p2, character.only = TRUE)) {
    warning(paste("No se pudo cargar el paquete (Parte 2):", pkg_p2), call. = FALSE)
  }
}

# --- 1. DEFINIR RUTAS Y NOMBRES DE ARCHIVO ---
message("\nPARTE 2: PASO 1: Definiendo rutas y nombres de archivo...")
crosswalk_data_path_p2 <- "/home/rober/Documentos/skill_diffusion/data/crosswalk/" # PLEASE VERIFY THIS PATH
output_data_dir_cps_final <- "datos_eventos_con_flujo_cps_final_v108_Corrected" # Nueva carpeta para esta versión
if (!dir.exists(output_data_dir_cps_final)) { dir.create(output_data_dir_cps_final, recursive = TRUE) }
message("La base de datos final combinada (Parte 2) se guardará en: ", normalizePath(output_data_dir_cps_final))

ruta_archivo_dat_cps <- "/home/rober/Descargas/cps_00001.dat" # PLEASE VERIFY THIS PATH
ruta_archivo_xml_ddi_cps <- "/home/rober/Descargas/cps_00001.xml" # PLEASE VERIFY THIS PATH

crosswalk_occ2010_to_soc2010_filename_cps <- "2010-occ-codes-with-crosswalk-from-2002-2011.xls" 
crosswalk_soc2010_to_soc2018_filename_cps <- "soc_2010_to_2018_crosswalk.xlsx"

# --- CARGA DE all_events_final_enriched (DATOS DE DIFUSIÓN REALES) ---
message("\nPARTE 2: --- CARGANDO DATOS DE DIFUSIÓN 'all_events_final_enriched' ---")
path_to_all_events_enriched_rds <- file.path(output_data_dir_p1, "all_diffusion_events_ENRICHED_AllClassifications.rds")
data_loaded_successfully_p2 <- FALSE
all_events_final_enriched_obj_name_p2 <- "all_events_final_enriched" 

if (exists(all_events_final_enriched_obj_name_p2, envir = .GlobalEnv)) {
  message(paste0("Objeto '", all_events_final_enriched_obj_name_p2, "' encontrado en el entorno global (Parte 2)."))
  temp_obj_p2 <- get(all_events_final_enriched_obj_name_p2, envir = .GlobalEnv)
  if (is.data.table(temp_obj_p2) && nrow(temp_obj_p2) > 0) {
    all_events_for_cps_merge <- copy(temp_obj_p2) 
    if (!is.data.table(all_events_for_cps_merge)) setDT(all_events_for_cps_merge)
    message(paste0("'all_events_for_cps_merge' usado desde memoria (Parte 2): ", nrow(all_events_for_cps_merge), " filas."))
    data_loaded_successfully_p2 <- TRUE
  } else {
    message(paste0("'", all_events_final_enriched_obj_name_p2, "' en memoria no es un data.table válido o está vacío (Parte 2)."))
  }
} else {
  message(paste0("Objeto '", all_events_final_enriched_obj_name_p2, "' no encontrado en memoria (Parte 2)."))
}

if (!data_loaded_successfully_p2) {
  message("Intentando cargar 'all_events_final_enriched' desde archivo RDS (Parte 2)...")
  if (file.exists(path_to_all_events_enriched_rds)) {
    all_events_for_cps_merge <- readRDS(path_to_all_events_enriched_rds)
    setDT(all_events_for_cps_merge)
    if (nrow(all_events_for_cps_merge) > 0) {
      message(sprintf("'all_events_for_cps_merge' cargada desde archivo (Parte 2): %d filas.", nrow(all_events_for_cps_merge)))
      data_loaded_successfully_p2 <- TRUE
    } else {
      message("'all_events_for_cps_merge' cargada desde archivo pero está vacía (Parte 2).")
      data_loaded_successfully_p2 <- FALSE
    }
  } else {
    stop(paste("¡ERROR CRÍTICO (Parte 2)! 'all_events_final_enriched' no se encontró ni en memoria ni en la ruta:", path_to_all_events_enriched_rds))
  }
}

if (!data_loaded_successfully_p2 || nrow(all_events_for_cps_merge) == 0) {
  stop("'all_events_for_cps_merge' no pudo ser cargado o está vacío (Parte 2).")
}

message("Procesando 'all_events_for_cps_merge' cargada (Parte 2)...")
if (!"source" %in% names(all_events_for_cps_merge)) stop("Columna 'source' no encontrada en 'all_events_for_cps_merge' (Parte 2).")
if (!"target" %in% names(all_events_for_cps_merge)) stop("Columna 'target' no encontrada en 'all_events_for_cps_merge' (Parte 2).")

all_events_for_cps_merge[, source_clean_cps := clean_soc_code_universal(as.character(source))]
all_events_for_cps_merge[, target_clean_cps := clean_soc_code_universal(as.character(target))]

if (any(is.na(all_events_for_cps_merge$source_clean_cps))) {
  warning("Algunos 'source' en 'all_events_for_cps_merge' son NA después de limpiar (Parte 2).")
  print(head(all_events_for_cps_merge[is.na(source_clean_cps), .(original_source = source)]))
}
if (any(is.na(all_events_for_cps_merge$target_clean_cps))) {
  warning("Algunos 'target' en 'all_events_for_cps_merge' son NA después de limpiar (Parte 2).")
  print(head(all_events_for_cps_merge[is.na(target_clean_cps), .(original_target = target)]))
}
all_events_for_cps_merge <- all_events_for_cps_merge[!is.na(source_clean_cps) & !is.na(target_clean_cps)]
message(sprintf("Filas en 'all_events_for_cps_merge' después de limpiar source/target_clean_cps (Parte 2): %d", nrow(all_events_for_cps_merge)))
if (nrow(all_events_for_cps_merge) == 0) stop("No quedaron filas en 'all_events_for_cps_merge' después de limpiar códigos SOC (Parte 2).")

# --- 2. FUNCIONES AUXILIARES DE FORMATEO DE CÓDIGOS SOC (para visualización) ---
format_soc_code_to_std_display <- function(soc_code_vector_cleaned_6_digits) {
  soc_code_vector_cleaned_6_digits <- as.character(soc_code_vector_cleaned_6_digits)
  formatted_codes <- ifelse(!is.na(soc_code_vector_cleaned_6_digits) & nchar(soc_code_vector_cleaned_6_digits) == 6 & grepl("^[0-9]{6}$", soc_code_vector_cleaned_6_digits),
                            paste0(substr(soc_code_vector_cleaned_6_digits, 1, 2), "-", substr(soc_code_vector_cleaned_6_digits, 3, 6)),
                            NA_character_)
  return(formatted_codes)
}

# --- 3. CARGAR Y PREPARAR CROSSWALKS PARA DATOS CPS ---
message("\nPARTE 2: PASO 3: Cargando y preparando crosswalks para datos CPS...")
path_occ2010_to_soc2010_cps <- file.path(crosswalk_data_path_p2, crosswalk_occ2010_to_soc2010_filename_cps)
occ2010_to_soc2010_map_cps <- data.table()
if(file.exists(path_occ2010_to_soc2010_cps)){
  tryCatch({
    dt_raw_census <- as.data.table(read_excel(path_occ2010_to_soc2010_cps, 
                                              sheet = "2010OccCodeList", 
                                              skip = 5, 
                                              col_names = FALSE, 
                                              .name_repair = "minimal"))
    message("Datos crudos del crosswalk Census-SOC2010 cargados (Parte 2). Dimensiones: ", paste(dim(dt_raw_census), collapse="x"))
    
    if (nrow(dt_raw_census) > 0 && ncol(dt_raw_census) >= 4) { # Check if there are at least 4 columns
      # Assign generic names as the error "se intenta usar un nombre de varible de longitud cero"
      # suggests that .SD[[index]] might fail if names are not set or are problematic.
      current_col_names <- paste0("V", 1:ncol(dt_raw_census))
      setnames(dt_raw_census, current_col_names)
      message("Nombres de columna por defecto asignados al crosswalk Census-SOC2010: ", paste(names(dt_raw_census), collapse=", "))
      
      col_occ2010_name <- current_col_names[3] 
      col_soc2010_name <- current_col_names[4] 
      
      # Check if these columns now exist with the generic names
      if (all(c(col_occ2010_name, col_soc2010_name) %in% names(dt_raw_census))) {
        occ2010_to_soc2010_map_cps <- dt_raw_census[, .(
          OCC2010_Census_Code = stringr::str_pad(trimws(as.character(get(col_occ2010_name))), 4, side = "left", pad = "0"),
          SOC_Code_2010_cleaned_cps = clean_soc_code_universal(as.character(get(col_soc2010_name))) 
        )]
        occ2010_to_soc2010_map_cps <- unique(occ2010_to_soc2010_map_cps[!is.na(OCC2010_Census_Code) & grepl("^[0-9]{4}$", OCC2010_Census_Code) & !is.na(SOC_Code_2010_cleaned_cps)])
        message(sprintf("Crosswalk OCC2010->SOC2010 (Parte 2) procesado: %d mapeos válidos.", nrow(occ2010_to_soc2010_map_cps)))
        if (nrow(occ2010_to_soc2010_map_cps) == 0) {
          warning("Crosswalk OCC2010->SOC2010 vacío después de procesar (Parte 2).")
        } else {
          print(head(occ2010_to_soc2010_map_cps))
        }
      } else {
        warning(paste("Columnas por defecto (",col_occ2010_name,", ",col_soc2010_name,") no encontradas después de asignar nombres genéricos al crosswalk Census-SOC2010 (Parte 2). Columnas disponibles: ", paste(names(dt_raw_census), collapse=", "), sep=""))
      }
    } else {
      warning("Crosswalk Census-SOC2010 cargado pero vacío o con menos de 4 columnas (Parte 2).")
    }
  }, error = function(e){ 
    warning(paste("Error crítico cargando o procesando (Parte 2)",crosswalk_occ2010_to_soc2010_filename_cps,":", e$message)) 
    occ2010_to_soc2010_map_cps <- data.table() # Ensure it's empty on error
  })
} else { warning(paste("CROSSWALK NO ENCONTRADO (Parte 2):", path_occ2010_to_soc2010_cps)) }

path_soc2010_to_soc2018_cps <- file.path(crosswalk_data_path_p2, crosswalk_soc2010_to_soc2018_filename_cps)
soc2010_to_soc2018_map_cps <- data.table()
if (file.exists(path_soc2010_to_soc2018_cps)) {
  tryCatch({
    dt_raw_soc <- read_excel(path_soc2010_to_soc2018_cps, sheet = "Sorted by 2010", skip = 8)
    dt_loc_soc <- as.data.table(dt_raw_soc)
    soc2010_col_name <- "2010 SOC Code"; soc2018_col_name <- "2018 SOC Code"
    if (all(c(soc2010_col_name, soc2018_col_name) %in% names(dt_loc_soc))) {
      soc2010_to_soc2018_map_cps <- dt_loc_soc[, .(
        SOC_Code_2010_cleaned_cps = clean_soc_code_universal(get(soc2010_col_name)), 
        SOC_Code_2018_Harmonized_cleaned_cps = clean_soc_code_universal(get(soc2018_col_name)) 
      )]
      soc2010_to_soc2018_map_cps <- unique(soc2010_to_soc2018_map_cps[!is.na(SOC_Code_2010_cleaned_cps) & !is.na(SOC_Code_2018_Harmonized_cleaned_cps)])
      message(sprintf("Crosswalk SOC2010->SOC2018 (Parte 2): %d mapeos válidos.", nrow(soc2010_to_soc2018_map_cps)))
      if(nrow(soc2010_to_soc2018_map_cps) == 0) warning("Crosswalk SOC2010->SOC2018 vacío (Parte 2).")
      else print(head(soc2010_to_soc2018_map_cps))
    } else { warning(paste("Columnas SOC2010/2018 no encontradas en (Parte 2)", crosswalk_soc2010_to_soc2018_filename_cps)) }
  }, error = function(e) { warning(paste("Error cargando (Parte 2)", crosswalk_soc2010_to_soc2018_filename_cps, ":", e$message)) })
} else { warning(paste("CROSSWALK NO ENCONTRADO (Parte 2):", path_soc2010_to_soc2018_cps)) }


# --- 4. PROCESAMIENTO DE DATOS CPS PARA FLUJO DE TRABAJADORES ---
message("\nPARTE 2: PASO 4: Procesando datos CPS para flujos de trabajadores...")
cps_data_raw <- data.table()
if (file.exists(ruta_archivo_dat_cps) && file.exists(ruta_archivo_xml_ddi_cps)) {
  tryCatch({
    cps_data_raw <- read_ipums_micro(ddi = ruta_archivo_xml_ddi_cps, data_file = ruta_archivo_dat_cps, verbose = FALSE)
    setDT(cps_data_raw)
    message(sprintf("Datos CPS crudos cargados (Parte 2): %d filas.", nrow(cps_data_raw)))
  }, error = function(e) { message("Error cargando datos CPS (Parte 2): ", e$message) })
} else {
  warning("Archivos CPS no encontrados (Parte 2). No se procesarán flujos.")
}

worker_transition_probabilities_cps_harmonized <- data.table()

if (nrow(cps_data_raw) > 0 && nrow(occ2010_to_soc2010_map_cps) > 0 && nrow(soc2010_to_soc2018_map_cps) > 0) {
  MIN_YEAR_DATA_CPS    <- 2015; MAX_YEAR_DATA_CPS    <- 2023
  TARGET_MONTH_CPS     <- 3; PERSON_WEIGHT_VAR_CPS <- "WTFINL"
  VALID_EMP_CODES_CPS  <- c(10, 12) 
  
  required_cps_vars <- c("YEAR", "MONTH", "CPSIDP", "EMPSTAT", "OCC2010", PERSON_WEIGHT_VAR_CPS)
  if (!all(required_cps_vars %in% names(cps_data_raw))) {
    stop(paste0("Variables CPS requeridas no encontradas (Parte 2): ", paste(setdiff(required_cps_vars, names(cps_data_raw)), collapse=", ")))
  }
  
  cps_subset_longitudinal <- cps_data_raw[YEAR >= MIN_YEAR_DATA_CPS & YEAR <= (MAX_YEAR_DATA_CPS + 1) & MONTH == TARGET_MONTH_CPS, ..required_cps_vars]
  setnames(cps_subset_longitudinal, old = PERSON_WEIGHT_VAR_CPS, new = "SELECTED_WEIGHT")
  message(sprintf("CPS subset longitudinal (años %d-%d, mes %d) (Parte 2): %d filas.", MIN_YEAR_DATA_CPS, MAX_YEAR_DATA_CPS + 1, TARGET_MONTH_CPS, nrow(cps_subset_longitudinal)))
  
  cps_subset_longitudinal[, OCC2010_raw_char := trimws(as.character(OCC2010))]
  cps_subset_longitudinal[, OCC2010_Census_Code_cleaned := stringr::str_pad(gsub("[^0-9]", "", OCC2010_raw_char), 4, side = "left", pad = "0")]
  cps_subset_longitudinal[!grepl("^[0-9]{4}$", OCC2010_Census_Code_cleaned) | OCC2010_Census_Code_cleaned == "0000", OCC2010_Census_Code_cleaned := "9999_INVALID"] 
  message(sprintf("OCC2010_Census_Code_cleaned '9999_INVALID' asignados (Parte 2): %d. Filas con OCC2010 válido: %d",
                  nrow(cps_subset_longitudinal[OCC2010_Census_Code_cleaned == "9999_INVALID"]),
                  nrow(cps_subset_longitudinal[OCC2010_Census_Code_cleaned != "9999_INVALID"])))
  
  cps_subset_unique <- unique(cps_subset_longitudinal, by = c("CPSIDP", "YEAR"))
  message(sprintf("CPS subset único por persona-año (Parte 2): %d filas.", nrow(cps_subset_unique)))
  
  data_t <- cps_subset_unique[, .(CPSIDP, YEAR_t = YEAR, EMPSTAT_t = EMPSTAT, OCC2010_Census_t = OCC2010_Census_Code_cleaned, WEIGHT_t = SELECTED_WEIGHT)]
  data_t_plus_1 <- cps_subset_unique[, .(CPSIDP, YEAR_t1 = YEAR, EMPSTAT_t1 = EMPSTAT, OCC2010_Census_t1 = OCC2010_Census_Code_cleaned)]
  data_t_plus_1[, YEAR_t_for_link := YEAR_t1 - 1]
  
  transitions_raw_cps <- merge(data_t, data_t_plus_1, by.x = c("CPSIDP", "YEAR_t"), by.y = c("CPSIDP", "YEAR_t_for_link"), all = FALSE)
  message(sprintf("Transiciones crudas (t a t+1) generadas (Parte 2): %d filas.", nrow(transitions_raw_cps)))
  
  transitions_raw_cps <- transitions_raw_cps[YEAR_t >= MIN_YEAR_DATA_CPS & YEAR_t <= MAX_YEAR_DATA_CPS]
  message(sprintf("Transiciones crudas filtradas por año de inicio (%d-%d) (Parte 2): %d filas.", MIN_YEAR_DATA_CPS, MAX_YEAR_DATA_CPS, nrow(transitions_raw_cps)))
  
  if (nrow(transitions_raw_cps) > 0) {
    transitions_filtered_cps <- transitions_raw_cps[
      EMPSTAT_t %in% VALID_EMP_CODES_CPS & EMPSTAT_t1 %in% VALID_EMP_CODES_CPS &
        OCC2010_Census_t != "9999_INVALID" & OCC2010_Census_t1 != "9999_INVALID"
    ]
    message(sprintf("Transiciones filtradas por EMPSTAT y OCC2010 válido (Parte 2): %d filas.", nrow(transitions_filtered_cps)))
    
    transitions_filtered_weighted_cps <- transitions_filtered_cps[!is.na(WEIGHT_t) & WEIGHT_t > 0]
    message(sprintf("Transiciones filtradas por peso válido (>0) (Parte 2): %d filas.", nrow(transitions_filtered_weighted_cps)))
    
    if (nrow(transitions_filtered_weighted_cps) > 0) {
      worker_flows_cps_occ2010 <- transitions_filtered_weighted_cps[,
                                                                    .(Agg_Flow_OCC2010_Weighted = sum(WEIGHT_t, na.rm = TRUE)),
                                                                    by = .(OCC2010_Source_raw = OCC2010_Census_t, OCC2010_Target_raw = OCC2010_Census_t1) 
      ]
      message(sprintf("Flujos agregados por OCC2010 (Source-Target, sin filtro de cambio aún) (Parte 2): %d pares.", nrow(worker_flows_cps_occ2010)))
      
      total_from_origin_occ2010 <- transitions_filtered_weighted_cps[,
                                                                     .(Agg_Origin_Pop_OCC2010_Weighted = sum(WEIGHT_t, na.rm = TRUE)),
                                                                     by = .(OCC2010_Source_raw = OCC2010_Census_t) 
      ]
      message(sprintf("Población de origen agregada por OCC2010_Source_raw (Parte 2): %d ocupaciones.", nrow(total_from_origin_occ2010)))
      
      if (nrow(worker_flows_cps_occ2010) > 0) {
        message("\n--- Armonizando tabla de flujos OCC2010 agregados (Parte 2) ---")
        
        message(sprintf("Filas en worker_flows_cps_occ2010 ANTES de merge con occ2010_to_soc2010_map_cps (Source): %d", nrow(worker_flows_cps_occ2010)))
        flows_harm_step1a <- merge(worker_flows_cps_occ2010, occ2010_to_soc2010_map_cps,
                                   by.x = "OCC2010_Source_raw", by.y = "OCC2010_Census_Code", all.x = TRUE, allow.cartesian = TRUE) # Allow cartesian
        setnames(flows_harm_step1a, "SOC_Code_2010_cleaned_cps", "SOC2010_Source_Cleaned_cps", skip_absent = TRUE)
        message(sprintf("Filas DESPUÉS de merge (Source): %d", nrow(flows_harm_step1a)))
        message(sprintf("NAs en SOC2010_Source_Cleaned_cps post-merge: %d", sum(is.na(flows_harm_step1a$SOC2010_Source_Cleaned_cps))))
        if(sum(is.na(flows_harm_step1a$SOC2010_Source_Cleaned_cps)) > 0 && nrow(flows_harm_step1a[is.na(SOC2010_Source_Cleaned_cps)]) >0) print(head(flows_harm_step1a[is.na(SOC2010_Source_Cleaned_cps), .(OCC2010_Source_raw, Agg_Flow_OCC2010_Weighted)]))
        flows_harm_step1a <- flows_harm_step1a[!is.na(SOC2010_Source_Cleaned_cps)]
        message(sprintf("Filas DESPUÉS de filtrar NAs en SOC2010_Source_Cleaned_cps: %d", nrow(flows_harm_step1a)))
        
        message(sprintf("Filas en flows_harm_step1a ANTES de merge con occ2010_to_soc2010_map_cps (Target): %d", nrow(flows_harm_step1a)))
        flows_harm_step1b <- merge(flows_harm_step1a, occ2010_to_soc2010_map_cps,
                                   by.x = "OCC2010_Target_raw", by.y = "OCC2010_Census_Code", all.x = TRUE, allow.cartesian = TRUE) # Allow cartesian
        setnames(flows_harm_step1b, "SOC_Code_2010_cleaned_cps", "SOC2010_Target_Cleaned_cps", skip_absent = TRUE)
        message(sprintf("Filas DESPUÉS de merge (Target): %d", nrow(flows_harm_step1b)))
        message(sprintf("NAs en SOC2010_Target_Cleaned_cps post-merge: %d", sum(is.na(flows_harm_step1b$SOC2010_Target_Cleaned_cps))))
        if(sum(is.na(flows_harm_step1b$SOC2010_Target_Cleaned_cps)) > 0 && nrow(flows_harm_step1b[is.na(SOC2010_Target_Cleaned_cps)]) > 0) print(head(flows_harm_step1b[is.na(SOC2010_Target_Cleaned_cps), .(OCC2010_Target_raw, Agg_Flow_OCC2010_Weighted)]))
        flows_harm_step1b <- flows_harm_step1b[!is.na(SOC2010_Target_Cleaned_cps)]
        message(sprintf("Filas DESPUÉS de filtrar NAs en SOC2010_Target_Cleaned_cps: %d", nrow(flows_harm_step1b)))
        
        if (nrow(flows_harm_step1b) > 0) {
          message(sprintf("Filas en flows_harm_step1b ANTES de merge con soc2010_to_soc2018_map_cps (Source): %d", nrow(flows_harm_step1b)))
          flows_harm_step2a <- merge(flows_harm_step1b, soc2010_to_soc2018_map_cps,
                                     by.x = "SOC2010_Source_Cleaned_cps", by.y = "SOC_Code_2010_cleaned_cps", all.x = TRUE, allow.cartesian = TRUE) # Allow cartesian
          setnames(flows_harm_step2a, "SOC_Code_2018_Harmonized_cleaned_cps", "Source_SOC_2018_Final", skip_absent = TRUE) 
          message(sprintf("Filas DESPUÉS de merge (Source SOC2010->SOC2018): %d", nrow(flows_harm_step2a)))
          message(sprintf("NAs en Source_SOC_2018_Final post-merge: %d", sum(is.na(flows_harm_step2a$Source_SOC_2018_Final))))
          if(sum(is.na(flows_harm_step2a$Source_SOC_2018_Final)) > 0 && nrow(flows_harm_step2a[is.na(Source_SOC_2018_Final)]) > 0) print(head(flows_harm_step2a[is.na(Source_SOC_2018_Final), .(SOC2010_Source_Cleaned_cps, Agg_Flow_OCC2010_Weighted)]))
          flows_harm_step2a <- flows_harm_step2a[!is.na(Source_SOC_2018_Final)]
          message(sprintf("Filas DESPUÉS de filtrar NAs en Source_SOC_2018_Final: %d", nrow(flows_harm_step2a)))
          
          message(sprintf("Filas en flows_harm_step2a ANTES de merge con soc2010_to_soc2018_map_cps (Target): %d", nrow(flows_harm_step2a)))
          flows_harm_step2b <- merge(flows_harm_step2a, soc2010_to_soc2018_map_cps,
                                     by.x = "SOC2010_Target_Cleaned_cps", by.y = "SOC_Code_2010_cleaned_cps", all.x = TRUE, allow.cartesian = TRUE) # Allow cartesian
          setnames(flows_harm_step2b, "SOC_Code_2018_Harmonized_cleaned_cps", "Target_SOC_2018_Final", skip_absent = TRUE) 
          message(sprintf("Filas DESPUÉS de merge (Target SOC2010->SOC2018): %d", nrow(flows_harm_step2b)))
          message(sprintf("NAs en Target_SOC_2018_Final post-merge: %d", sum(is.na(flows_harm_step2b$Target_SOC_2018_Final))))
          if(sum(is.na(flows_harm_step2b$Target_SOC_2018_Final)) > 0 && nrow(flows_harm_step2b[is.na(Target_SOC_2018_Final)]) > 0) print(head(flows_harm_step2b[is.na(Target_SOC_2018_Final), .(SOC2010_Target_Cleaned_cps, Agg_Flow_OCC2010_Weighted)]))
          flows_fully_harmonized_temp <- flows_harm_step2b[!is.na(Target_SOC_2018_Final)]
          message(sprintf("Flujos temporalmente armonizados a SOC2018 (Source y Target) (Parte 2): %d filas.", nrow(flows_fully_harmonized_temp)))
          
          if (nrow(flows_fully_harmonized_temp) > 0) {
            worker_flows_cps_harmonized_agg <- flows_fully_harmonized_temp[,
                                                                           .(Agg_Total_Flow_Weighted_CPS = sum(Agg_Flow_OCC2010_Weighted, na.rm = TRUE)),
                                                                           by = .(Source_SOC_2018_Cleaned = Source_SOC_2018_Final, Target_SOC_2018_Cleaned = Target_SOC_2018_Final) 
            ]
            message(sprintf("Flujos finales re-agregados por SOC2018 (Parte 2): %d pares.", nrow(worker_flows_cps_harmonized_agg)))
            
            worker_flows_cps_harmonized_agg <- worker_flows_cps_harmonized_agg[Source_SOC_2018_Cleaned != Target_SOC_2018_Cleaned]
            message(sprintf("Flujos finales re-agregados por SOC2018 (Source != Target) (Parte 2): %d pares.", nrow(worker_flows_cps_harmonized_agg)))
            
            message("\n--- Armonizando población de origen (Parte 2) ---")
            total_from_origin_occ2010_mapped_soc2010 <- merge(total_from_origin_occ2010, occ2010_to_soc2010_map_cps,
                                                              by.x = "OCC2010_Source_raw", by.y = "OCC2010_Census_Code", all.x = TRUE, allow.cartesian = TRUE) # Allow cartesian
            total_from_origin_occ2010_mapped_soc2010 <- total_from_origin_occ2010_mapped_soc2010[!is.na(SOC_Code_2010_cleaned_cps)]
            message(sprintf("Población origen OCC2010->SOC2010: %d", nrow(total_from_origin_occ2010_mapped_soc2010)))
            
            total_from_origin_soc2018_harmonized <- merge(total_from_origin_occ2010_mapped_soc2010, soc2010_to_soc2018_map_cps,
                                                          by.x = "SOC_Code_2010_cleaned_cps", by.y = "SOC_Code_2010_cleaned_cps", all.x = TRUE, allow.cartesian = TRUE) # Allow cartesian
            total_from_origin_soc2018_harmonized <- total_from_origin_soc2018_harmonized[!is.na(SOC_Code_2018_Harmonized_cleaned_cps)]
            message(sprintf("Población origen SOC2010->SOC2018: %d", nrow(total_from_origin_soc2018_harmonized)))
            
            total_origin_pop_final_soc2018 <- total_from_origin_soc2018_harmonized[,
                                                                                   .(Agg_Total_Origin_Pop_Weighted_CPS = sum(Agg_Origin_Pop_OCC2010_Weighted, na.rm = TRUE)),
                                                                                   by = .(Source_SOC_2018_Cleaned = SOC_Code_2018_Harmonized_cleaned_cps) 
            ]
            message(sprintf("Población de origen armonizada y agregada por SOC2018 (Parte 2): %d ocupaciones.", nrow(total_origin_pop_final_soc2018)))
            
            if (nrow(worker_flows_cps_harmonized_agg) > 0 && nrow(total_origin_pop_final_soc2018) > 0) {
              worker_transition_probabilities_cps_harmonized <- merge(
                worker_flows_cps_harmonized_agg, total_origin_pop_final_soc2018,
                by = "Source_SOC_2018_Cleaned", all.x = TRUE
              )
              worker_transition_probabilities_cps_harmonized[is.na(Agg_Total_Origin_Pop_Weighted_CPS), Agg_Total_Origin_Pop_Weighted_CPS := 0]
              worker_transition_probabilities_cps_harmonized[, Prob_Transicion_Avg_CPS :=
                                                               fifelse(Agg_Total_Origin_Pop_Weighted_CPS > 0, Agg_Total_Flow_Weighted_CPS / Agg_Total_Origin_Pop_Weighted_CPS, 0)]
              
              message(sprintf("Tabla final de probabilidades de transición CPS (revisada) (Parte 2): %d filas.", nrow(worker_transition_probabilities_cps_harmonized)))
              if(nrow(worker_transition_probabilities_cps_harmonized) > 0) {
                fwrite(worker_transition_probabilities_cps_harmonized,
                       file.path(output_data_dir_cps_final, "worker_transition_prob_cps_2015_2023_harmonized_revised.csv"))
                print(head(worker_transition_probabilities_cps_harmonized[order(-Agg_Total_Flow_Weighted_CPS)], 5))
              } else { message("ADVERTENCIA FINAL (Parte 2): worker_transition_probabilities_cps_harmonized vacía.")}
            } else { message("ADVERTENCIA (Parte 2): Flujos armonizados agregados o población de origen armonizada vacíos.")}
          } else { message("ADVERTENCIA (Parte 2): No quedaron flujos después de la armonización completa a SOC2018 (temporal).")}
        } else { message("ADVERTENCIA (Parte 2): No quedaron flujos después de la armonización OCC2010->SOC2010 (Target).")}
      } else { message("ADVERTENCIA (Parte 2): No hay flujos OCC2010 agregados para armonizar.")}
    } else {message("ADVERTENCIA (Parte 2): No hay transiciones ponderadas válidas en datos CPS después de filtrar pesos.")}
  } else {message("ADVERTENCIA (Parte 2): No hay transiciones crudas en datos CPS después del emparejamiento inicial o filtro de años.")}
  
  rm(list=ls(pattern="^cps_subset_|^data_t$|^data_t_plus_1$|^transitions_raw_cps$|^transitions_filtered_cps$|^transitions_filtered_weighted_cps$"))
  rm(list=ls(pattern="^worker_flows_cps_occ2010$|^total_from_origin_occ2010$"))
  rm(list=ls(pattern="^flows_harm_step|^flows_fully_harmonized_temp$"))
  rm(list=ls(pattern="^total_from_origin_occ2010_mapped_soc2010$|^total_from_origin_soc2018_harmonized$|^total_origin_pop_final_soc2018$"))
  if(exists("worker_flows_cps_harmonized_agg")) rm(worker_flows_cps_harmonized_agg)
  gc()
  message("Objetos intermedios de CPS limpiados de memoria (Parte 2).")
  
} else {
  warning("Datos CPS crudos o crosswalks necesarios para CPS no están disponibles/válidos (Parte 2). No se calcularán flujos de trabajadores.")
}


# --- 5. UNIR DATOS DE FLUJO CPS A all_events_for_cps_merge ---
message("\nPARTE 2: PASO 5: Uniendo flujos de trabajadores CPS a 'all_events_for_cps_merge'...")

if (exists("all_events_for_cps_merge") && is.data.table(all_events_for_cps_merge) && nrow(all_events_for_cps_merge) > 0) {
  if (exists("worker_transition_probabilities_cps_harmonized") && nrow(worker_transition_probabilities_cps_harmonized) > 0) {
    
    worker_transition_probabilities_cps_harmonized <- worker_transition_probabilities_cps_harmonized[
      !is.na(Source_SOC_2018_Cleaned) & !is.na(Target_SOC_2018_Cleaned)
    ]
    message(sprintf("Filas en worker_transition_probabilities_cps_harmonized antes del merge final (Parte 2): %d", nrow(worker_transition_probabilities_cps_harmonized)))
    message(sprintf("Filas en all_events_for_cps_merge antes del merge final (Parte 2): %d", nrow(all_events_for_cps_merge)))
    
    message("Ejemplo de claves en all_events_for_cps_merge (source_clean_cps, target_clean_cps):")
    print(head(all_events_for_cps_merge[, .(source_clean_cps, target_clean_cps)]))
    message("Ejemplo de claves en worker_transition_probabilities_cps_harmonized (Source_SOC_2018_Cleaned, Target_SOC_2018_Cleaned):")
    print(head(worker_transition_probabilities_cps_harmonized[, .(Source_SOC_2018_Cleaned, Target_SOC_2018_Cleaned)]))
    
    
    all_events_final_with_cps_data <- merge( 
      all_events_for_cps_merge,
      worker_transition_probabilities_cps_harmonized[, .SD, .SDcols = c("Source_SOC_2018_Cleaned", "Target_SOC_2018_Cleaned",
                                                                        "Agg_Total_Flow_Weighted_CPS", "Prob_Transicion_Avg_CPS",
                                                                        "Agg_Total_Origin_Pop_Weighted_CPS")],
      by.x = c("source_clean_cps", "target_clean_cps"),
      by.y = c("Source_SOC_2018_Cleaned", "Target_SOC_2018_Cleaned"),
      all.x = TRUE
    )
    message(sprintf("Filas en all_events_final_with_cps_data DESPUÉS del merge final (Parte 2): %d", nrow(all_events_final_with_cps_data)))
    
    cols_flujo_cps_to_fill_na <- c("Agg_Total_Flow_Weighted_CPS", "Prob_Transicion_Avg_CPS", "Agg_Total_Origin_Pop_Weighted_CPS")
    
    message("Resumen de NAs en columnas de flujo CPS ANTES de convertirlos a 0 (Parte 2):")
    for (col_name_fill in cols_flujo_cps_to_fill_na) {
      if (col_name_fill %in% names(all_events_final_with_cps_data)) {
        message(sprintf("NAs en %s: %d de %d (%.2f%%)", col_name_fill,
                        sum(is.na(all_events_final_with_cps_data[[col_name_fill]])),
                        nrow(all_events_final_with_cps_data),
                        100 * sum(is.na(all_events_final_with_cps_data[[col_name_fill]])) / nrow(all_events_final_with_cps_data)
        ))
      } else {
        message(sprintf("Columna %s no encontrada para chequeo de NA (Parte 2).", col_name_fill))
      }
    }
    
    message("Convirtiendo NAs en columnas de flujo CPS a 0 (Parte 2). Si hay muchos NAs, puede indicar un problema en el merge.")
    for (col_name_fill_final in cols_flujo_cps_to_fill_na) {
      if (col_name_fill_final %in% names(all_events_final_with_cps_data)) {
        all_events_final_with_cps_data[is.na(get(col_name_fill_final)), (col_name_fill_final) := 0]
      } else {
        all_events_final_with_cps_data[, (col_name_fill_final) := 0.0] 
      }
    }
    message("Merge con flujos CPS completado (Parte 2). 'all_events_final_with_cps_data' actualizada.")
    
    all_events_final_with_cps_data[, source_display_final := format_soc_code_to_std_display(source_clean_cps)]
    all_events_final_with_cps_data[, target_display_final := format_soc_code_to_std_display(target_clean_cps)]
    
    all_events_final_with_cps_data[, source := source_display_final]
    all_events_final_with_cps_data[, target := target_display_final]
    all_events_final_with_cps_data[, c("source_display_final", "target_display_final") := NULL]
    
    if ("source_clean_cps" %in% names(all_events_final_with_cps_data)) all_events_final_with_cps_data[, source_clean_cps := NULL]
    if ("target_clean_cps" %in% names(all_events_final_with_cps_data)) all_events_final_with_cps_data[, target_clean_cps := NULL]
    
  } else {
    message("ADVERTENCIA (Parte 2): No hay datos de flujo CPS ('worker_transition_probabilities_cps_harmonized') para unir. Se añadirán columnas placeholder a 'all_events_final_with_cps_data'.")
    all_events_final_with_cps_data <- all_events_for_cps_merge 
    all_events_final_with_cps_data[, Agg_Total_Flow_Weighted_CPS := 0.0]
    all_events_final_with_cps_data[, Prob_Transicion_Avg_CPS := 0.0]
    all_events_final_with_cps_data[, Agg_Total_Origin_Pop_Weighted_CPS := 0.0]
    
    if ("source_clean_cps" %in% names(all_events_final_with_cps_data)) {
      all_events_final_with_cps_data[, source := format_soc_code_to_std_display(source_clean_cps)]
      all_events_final_with_cps_data[, source_clean_cps := NULL]
    } else if ("source" %in% names(all_events_final_with_cps_data)) { 
      all_events_final_with_cps_data[, source := format_soc_code_to_std_display(clean_soc_code_universal(source))]
    }
    if ("target_clean_cps" %in% names(all_events_final_with_cps_data)) {
      all_events_final_with_cps_data[, target := format_soc_code_to_std_display(target_clean_cps)]
      all_events_final_with_cps_data[, target_clean_cps := NULL]
    } else if ("target" %in% names(all_events_final_with_cps_data)) {
      all_events_final_with_cps_data[, target := format_soc_code_to_std_display(clean_soc_code_universal(target))]
    }
  }
  
  rds_final_path <- file.path(output_data_dir_cps_final, "all_events_final_with_CPS_transitions.rds")
  csv_final_path <- file.path(output_data_dir_cps_final, "all_events_final_with_CPS_transitions.csv")
  
  all_events_final_with_cps_data <- all_events_final_with_cps_data[!is.na(source) & !is.na(target)] 
  if (nrow(all_events_final_with_cps_data) == 0) {
    warning("La tabla final 'all_events_final_with_cps_data' está vacía después de asegurar que 'source' y 'target' no son NA (Parte 2). No se guardará nada.")
  } else {
    saveRDS(all_events_final_with_cps_data, rds_final_path)
    fwrite(all_events_final_with_cps_data, csv_final_path, na = "NA")
    message("Tabla 'all_events_final_with_cps_data' con flujos CPS guardada en (Parte 2): ", normalizePath(output_data_dir_cps_final))
    
    message("Primeras filas de 'all_events_final_with_cps_data' con flujos CPS (Parte 2):")
    print(head(all_events_final_with_cps_data))
    message("Resumen de columnas de flujo CPS en 'all_events_final_with_cps_data' (Parte 2):")
    cols_to_summarize_final_p2 <- c("Agg_Total_Flow_Weighted_CPS", "Prob_Transicion_Avg_CPS", "Agg_Total_Origin_Pop_Weighted_CPS")
    if(all(cols_to_summarize_final_p2 %in% names(all_events_final_with_cps_data))){
      print(summary(all_events_final_with_cps_data[, ..cols_to_summarize_final_p2]))
      message("Distribución de Agg_Total_Flow_Weighted_CPS > 0 (Parte 2):")
      print(table(all_events_final_with_cps_data$Agg_Total_Flow_Weighted_CPS > 0))
    }
  }
  
} else {
  warning("'all_events_for_cps_merge' (originalmente 'all_events_final_enriched') no existe o está vacía (Parte 2). No se puede proceder con la unión de flujos CPS.")
}

if(exists("all_events_for_cps_merge")) rm(all_events_for_cps_merge) 
if(exists("all_events_final_enriched")) rm(all_events_final_enriched) 
rm(occ2010_to_soc2010_map_cps, soc2010_to_soc2018_map_cps)
if(exists("worker_transition_probabilities_cps_harmonized")) rm(worker_transition_probabilities_cps_harmonized)
gc()
message("Crosswalks de CPS y tabla de probabilidades de transición limpiados de memoria (Parte 2).")

message("\n--- FIN DEL PROCESAMIENTO DE FLUJOS CPS Y MERGE (PARTE 2) ---")
