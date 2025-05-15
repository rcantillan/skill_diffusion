# =======================================================================
# PARTE 1: CREACIÓN DE LA BASE DE DATOS DE EVENTOS DE DIFUSIÓN
# Versión: 11.0.7_Base (Debug SkillNet + allow.cartesian en todos los merges)
# - Objetivo: Generar y guardar la tabla 'all_events'.
# - Incluye generación de TODOS los eventos negativos.
# - Excluye rca_source/target/weight de la tabla final.
# - Clasificación de skills por Red (con más debug), Empírica o Clúster.
# - Usa distancia Jaccard sobre RCA>1.
# - Añadido allow.cartesian=TRUE a todos los merges relevantes.
# =======================================================================

# --- 0. Cargar Librerías Necesarias ---
message("PASO 0: Cargando librerías necesarias...")
packages_to_load <- c("data.table", "readxl", "progress", "here", 
                      "Matrix", "lsa", "cluster", "moments", 
                      "igraph") 

for (pkg in packages_to_load) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Instalando paquete:", pkg, "..."))
    tryCatch({ install.packages(pkg, dependencies = TRUE) }, error = function(e){ warning(paste("Fallo al instalar:", pkg, "-", e$message), call. = FALSE)})
  }
  if (!require(pkg, character.only = TRUE)) {
    warning(paste("No se pudo cargar el paquete:", pkg), call. = FALSE)
  }
}

# --- 1. Definir Rutas y Nombres de Archivo ---
message("\nPASO 1: Definiendo rutas y nombres de archivo...")
# !!! ACTUALIZA ESTAS RUTAS SEGÚN TU ENTORNO !!!
onet_db_path <- "C:/Users/qramo/Downloads/db_29_2_text/db_29_2_text" 
data_external_path <- "C:/Users/qramo/Documents/Repositorios/skill_diffusion/data" 
output_data_dir <- "datos_eventos_generados_v7_debug_net" 
if (!dir.exists(output_data_dir)) { dir.create(output_data_dir, recursive = TRUE) }
message("La base de datos 'all_events' se guardará en: ", normalizePath(output_data_dir))

occupation_data_filename_onet <- "Occupation Data.txt"; job_zones_filename_onet <- "Job Zones.txt"
ete_data_filename_onet <- "Education, Training, and Experience.txt"; ete_categories_filename_onet <- "Education, Training, and Experience Categories.txt"
skills_filename_onet <- "Skills.txt"; abilities_filename_onet <- "Abilities.txt"; knowledge_filename_onet <- "Knowledge.txt"; work_activities_filename_onet <- "Work Activities.txt"
crosswalk_filename_user <- "2019_to_SOC_Crosswalk.xlsx"; bls_filename_2015 <- "national_M2015_dl.xlsx"; bls_filename_2024 <- "national_M2024_dl.xlsx" 

# --- 2. Función Auxiliar para Cargar Archivos .txt de O*NET ---
load_onet_text_file <- function(filename, path = onet_db_path) {
  full_path <- file.path(path, filename); if (!file.exists(full_path)) { warning(paste("Archivo no encontrado:", full_path)); return(data.table()) }; dt <- tryCatch({ fread(full_path, sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE, showProgress = FALSE, na.strings=c("NA","n/a","","*","**","****")) }, error = function(e) { warning(paste("Error fread:", e$message, ". Intentando read.delim.")); tryCatch({ as.data.table(read.delim(full_path, sep="\t", header=TRUE, quote="", stringsAsFactors=FALSE, na.strings=c("NA","n/a","","*","**","****"))) }, error = function(e2) { warning(paste("Error read.delim:", e2$message)); return(data.table()) }) }); if (is.null(dt) || nrow(dt) == 0) { warning(paste("Fallo al cargar/archivo vacío:", filename)); return(data.table()) }; current_names <- names(dt); new_names <- current_names; name_map <- list( "O*NET-SOC Code"="O.NET.SOC.Code", "Element Id"="Element.ID", "Element ID"="Element.ID", "Element Name"="Element.Name", "Scale ID"="Scale.ID", "Scale Name"="Scale.Name", "Data Value"="Data.Value", "Standard Error"="Standard.Error", "Lower CI Bound"="Lower.CI.Bound", "Upper CI Bound"="Upper.CI.Bound", "Recommend Suppress"="Recommend.Suppress", "Not Relevant"="Not.Relevant", "Domain Source"="Domain.Source", "Job Zone"="Job.Zone", "Category Description"="Category.Description" ); changed_any_name <- FALSE; for (old_nm_map in names(name_map)) { new_nm_map <- name_map[[old_nm_map]]; if (old_nm_map %in% new_names) { idx_rn <- which(new_names == old_nm_map); if (length(idx_rn) > 0) { if (old_nm_map != new_nm_map && (!(new_nm_map %in% new_names) || sum(new_names == new_nm_map) == 0 || new_names[idx_rn[1]] != new_nm_map)) { new_names[idx_rn] <- new_nm_map; changed_any_name <- TRUE } } } }; if (changed_any_name) { unique_new_names <- make.unique(new_names, sep = "_"); if (!identical(names(dt), unique_new_names)) setnames(dt, old = current_names, new = unique_new_names, skip_absent = TRUE) }; return(dt)
}

# --- 3. PREPARACIÓN DE DATOS GLOBALES ---
message("\n--- SECCIÓN 3.A: Creando 'occupation_attributes_final' ---")
process_bls_data_for_final <- function(bls_fp, yr_lbl, soc_col, grp_col, wage_col, wage_top_code_placeholder, cw_cl) { cat("\n--- Procesando salarios BLS:", yr_lbl, "---\n"); if (!file.exists(bls_fp)) { return(data.table()) }; bls_raw <- tryCatch(read_excel(bls_fp, sheet = 1), error = function(e) NULL); if (is.null(bls_raw) || nrow(bls_raw) == 0) { return(data.table()) }; bls_df <- as.data.table(bls_raw); required_bls_cols <- c(soc_col, grp_col, wage_col); if (!all(required_bls_cols %in% names(bls_df))) { return(data.table()) }; dt_det <- bls_df[get(grp_col) %in% c("detailed", "detail")]; if (nrow(dt_det) == 0) { return(data.table()) }; bls_w_p <- dt_det[, .SD, .SDcols = c(soc_col, wage_col)]; new_w_col <- paste0("Median_Wage_", yr_lbl); setnames(bls_w_p, old = c(soc_col, wage_col), new = c("SOC_Code_BLS_Format", new_w_col)); bls_w_p[, temp_cleaned_wage := as.character(get(new_w_col))]; bls_w_p[temp_cleaned_wage %in% c("*", "**", "***", "****", "#", "N.A.", "n.a."), temp_cleaned_wage := NA_character_]; bls_w_p[, temp_cleaned_wage := gsub("[^0-9\\.]", "", temp_cleaned_wage)]; bls_w_p[, (new_w_col) := as.numeric(temp_cleaned_wage)]; bls_w_p[, temp_cleaned_wage := NULL]; bls_w_p <- bls_w_p[!is.na(get(new_w_col))]; if (nrow(bls_w_p) == 0) { return(data.table()) }; bls_w_final_mg <- data.table(); if (!is.null(cw_cl) && nrow(cw_cl) > 0 && "SOC_Code_BLS_Format" %in% names(cw_cl) && "O.NET.SOC.Code" %in% names(cw_cl)) { bls_w_p[, SOC_Code_BLS_Format := as.character(SOC_Code_BLS_Format)]; cw_cl_copy <- copy(cw_cl); cw_cl_copy[, SOC_Code_BLS_Format := as.character(SOC_Code_BLS_Format)]; merged_cw <- merge(bls_w_p, cw_cl_copy, by = "SOC_Code_BLS_Format", all.x = FALSE, allow.cartesian = TRUE); merged_cw <- merged_cw[!is.na(O.NET.SOC.Code)]; if (nrow(merged_cw) > 0) { bls_w_final_mg <- unique(merged_cw[, .SD, .SDcols = c("O.NET.SOC.Code", new_w_col)], by = "O.NET.SOC.Code") } } else { cat("Advertencia: Crosswalk no disponible/inválido para", yr_lbl, "\n") }; return(bls_w_final_mg) }
occupation_data_onet <- load_onet_text_file(occupation_data_filename_onet); job_zones_onet <- load_onet_text_file(job_zones_filename_onet); ete_data <- load_onet_text_file(ete_data_filename_onet); ete_categories <- load_onet_text_file(ete_categories_filename_onet)
if(nrow(occupation_data_onet)==0 || nrow(job_zones_onet)==0 || nrow(ete_data)==0 || nrow(ete_categories)==0) { stop("Fallo crítico archivos O*NET.") }
job_zones_simple <- data.table(); if (all(c("O.NET.SOC.Code", "Job.Zone") %in% names(job_zones_onet))) { job_zones_simple <- unique(job_zones_onet[, .(`O.NET.SOC.Code`, `Job.Zone`)]) }
education_scores_onet <- data.table(); required_ete_cols <- c("O.NET.SOC.Code", "Element.ID", "Scale.ID", "Category", "Data.Value"); required_cat_cols <- c("Element.ID", "Scale.ID", "Category", "Category.Description")
if (all(required_ete_cols %in% names(ete_data)) && all(required_cat_cols %in% names(ete_categories))) { rl_data <- ete_data[Element.ID == "2.D.1" & Scale.ID == "RL"]; rl_cat_loc <- ete_categories[Element.ID == "2.D.1" & Scale.ID == "RL", .(Category, `Category.Description`)]; rl_cat_loc[, Category := as.integer(Category)]; setkey(rl_cat_loc, Category); edu_scale_map <- data.table(Category = 1:12, Edu_Numeric_Value = 1:12); setkey(edu_scale_map, Category); rl_data[, Category_Int := as.integer(Category)]; rl_data_sc <- merge(rl_data, edu_scale_map, by.x = "Category_Int", by.y = "Category", all.x = TRUE); rl_data_sc <- rl_data_sc[!is.na(Edu_Numeric_Value)]; if (nrow(rl_data_sc) > 0) { education_scores_onet <- rl_data_sc[, .(Edu_Score_Weighted = sum(Data.Value * Edu_Numeric_Value, na.rm = TRUE) / sum(Data.Value, na.rm = TRUE), Total_Pct_Reported = sum(Data.Value, na.rm = TRUE)), by = `O.NET.SOC.Code`]; education_scores_onet <- education_scores_onet[Total_Pct_Reported > 50]; } }
occupation_attributes_temp <- if (nrow(occupation_data_onet) > 0) { unique(occupation_data_onet[, .(O.NET.SOC.Code, Title, Description)], by = "O.NET.SOC.Code") } else { stop("occupation_data_onet vacío.") }
if (nrow(job_zones_simple) > 0) { occupation_attributes_temp <- merge(occupation_attributes_temp, job_zones_simple, by = "O.NET.SOC.Code", all.x = TRUE) } else { occupation_attributes_temp[, Job.Zone := NA_integer_] }
if (nrow(education_scores_onet) > 0) { occupation_attributes_temp <- merge(occupation_attributes_temp, education_scores_onet[, .(O.NET.SOC.Code, Edu_Score_Weighted)], by = "O.NET.SOC.Code", all.x = TRUE) } else { occupation_attributes_temp[, Edu_Score_Weighted := NA_real_] }
full_cw_path <- file.path(data_external_path, crosswalk_filename_user); cw_cleaned <- data.table(); if (file.exists(full_cw_path)) { tryCatch({ cw_dt_raw <- read_excel(full_cw_path, sheet = "O-NET-SOC 2019 Occupation Listi", skip = 2); cw_dt_loc <- as.data.table(cw_dt_raw); onet_cw_col <- "O*NET-SOC 2019 Code"; soc_cw_col <- "2018 SOC Code"; if (onet_cw_col %in% names(cw_dt_loc) && soc_cw_col %in% names(cw_dt_loc)) { cw_cleaned <- cw_dt_loc[, .SD, .SDcols = c(onet_cw_col, soc_cw_col)]; setnames(cw_cleaned, old = c(onet_cw_col, soc_cw_col), new = c("O.NET.SOC.Code", "SOC_Code_BLS_Format")); cw_cleaned <- cw_cleaned[!is.na(O.NET.SOC.Code) & !is.na(SOC_Code_BLS_Format)]; cw_cleaned[, O.NET.SOC.Code := as.character(O.NET.SOC.Code)]; cw_cleaned[, SOC_Code_BLS_Format := as.character(SOC_Code_BLS_Format)]; cw_cleaned <- unique(cw_cleaned); } else { stop(paste("Columnas no encontradas en crosswalk.")) } }, error = function(e) { cat("Error Crosswalk:", e$message, "\n") }) } else { warning(paste("Crosswalk no encontrado:", full_cw_path)); }
bls_wages_2015_final <- process_bls_data_for_final(file.path(data_external_path, bls_filename_2015), "2015", "OCC_CODE", "OCC_GROUP", "A_MEDIAN", "208000+", cw_cleaned)
bls_wages_2023_final <- process_bls_data_for_final(file.path(data_external_path, bls_filename_2024), "2023", "OCC_CODE", "O_GROUP", "A_MEDIAN", "249600+", cw_cleaned)
occupation_attributes_final <- copy(occupation_attributes_temp); if (nrow(bls_wages_2015_final) > 0) { occupation_attributes_final <- merge(occupation_attributes_final, bls_wages_2015_final, by = "O.NET.SOC.Code", all.x = TRUE) } else { occupation_attributes_final[, Median_Wage_2015 := NA_real_] }
if (nrow(bls_wages_2023_final) > 0) { occupation_attributes_final <- merge(occupation_attributes_final, bls_wages_2023_final, by = "O.NET.SOC.Code", all.x = TRUE) } else { occupation_attributes_final[, Median_Wage_2023 := NA_real_] }
message("'occupation_attributes_final' creada.")

message("\n--- SECCIÓN 3.B: Creando 'all_skills_data' ---")
skills_list <- list( load_onet_text_file(skills_filename_onet), load_onet_text_file(abilities_filename_onet), load_onet_text_file(knowledge_filename_onet), load_onet_text_file(work_activities_filename_onet) ); skills_list_valid <- Filter(function(x) !is.null(x) && is.data.table(x) && nrow(x) > 0 && ncol(x) > 0, skills_list); if(length(skills_list_valid) > 0) { all_skills_data <- rbindlist(skills_list_valid, use.names = TRUE, fill = TRUE); if ("Date" %in% names(all_skills_data)) { all_skills_data[, year := suppressWarnings(as.integer(sub(".*(\\d{4})$", "\\1", as.character(Date))))]; all_skills_data <- all_skills_data[!is.na(year) & year > 1900] } else { all_skills_data[, year := as.integer(NA)]; warning("Columna 'Date'/'year' no encontrada.") }; if ("Data.Value" %in% names(all_skills_data)) { all_skills_data[, Data.Value := as.numeric(Data.Value)]; all_skills_data <- all_skills_data[!is.na(Data.Value)] } else { warning("Columna 'Data.Value' no encontrada.") }; message("'all_skills_data' preparada.") } else { all_skills_data <- data.table(); warning("'all_skills_data' estará vacía.") }


# --- 4. FUNCIONES DE CLASIFICACIÓN DE SKILLS ---
# 4.A: Clasificación por Clustering
classify_skills_by_type <- function(skill_data, year_to_analyze, n_clusters = 3) {
  message("Clasificando skills por clúster..."); if (is.null(skill_data) || nrow(skill_data) == 0) stop("Datos skills vacíos"); s_year <- skill_data; if (!is.null(year_to_analyze) && !is.na(year_to_analyze) && "year" %in% names(skill_data)) { temp_s_year <- skill_data[year == year_to_analyze]; if (nrow(temp_s_year) > 0) s_year <- temp_s_year }; s_level <- s_year; if ("Scale.ID" %in% names(s_year)) { temp_s_level <- s_year[Scale.ID == "LV"]; if (nrow(temp_s_level) > 0) s_level <- temp_s_level }; s_dist_metrics <- s_level[, .( mean_level = mean(Data.Value, na.rm = TRUE), median_level = median(Data.Value, na.rm = TRUE), usage_count = .N, sd_level = sd(Data.Value, na.rm = TRUE) ), by = .(Element.Name)]; s_hist_data_list <- list(); u_skills_list <- unique(s_level$Element.Name); for (sk_iter in u_skills_list) { sk_vals_iter <- s_level[Element.Name == sk_iter, Data.Value]; if (length(sk_vals_iter) < 5) next; hr_obj <- hist(sk_vals_iter, breaks = 8, plot = FALSE); nc_vals <- hr_obj$counts / sum(hr_obj$counts); skn_val <- moments::skewness(sk_vals_iter, na.rm = TRUE); hup_val <- sum(sk_vals_iter > 3, na.rm = TRUE) / length(sk_vals_iter); s_hist_data_list[[sk_iter]] <- c( nc_vals, skn_val, hup_val, median(sk_vals_iter, na.rm = TRUE), sd(sk_vals_iter, na.rm = TRUE) ) }; if (length(s_hist_data_list) == 0) stop("No se calcularon distribuciones para clustering."); s_matrix_for_clust <- do.call(rbind, s_hist_data_list); rownames(s_matrix_for_clust) <- names(s_hist_data_list); s_matrix_for_clust[!is.finite(s_matrix_for_clust)] <- 0; sk_dist_obj <- dist(s_matrix_for_clust); sk_hcl_obj <- hclust(sk_dist_obj, method = "ward.D2"); sk_clus_assign <- cutree(sk_hcl_obj, k = n_clusters); cl_sum_table <- data.table( Cluster = 1:n_clusters, MedianLevel = sapply(1:n_clusters, function(i) { sk_in_cl_names <- names(sk_clus_assign[sk_clus_assign == i]); if (length(sk_in_cl_names) == 0) return(NA_real_); median(s_dist_metrics[Element.Name %in% sk_in_cl_names, median_level], na.rm = TRUE) }) ); cl_sum_table <- cl_sum_table[order(-MedianLevel)]; if (n_clusters == 3) { type_labels_vec <- c("general", "intermedia", "específica") } else { type_labels_vec <- paste0("Tipo", 1:n_clusters) }; cl_sum_table[, SkillType := type_labels_vec[1:nrow(cl_sum_table)]]; cl_map_obj <- cl_sum_table$SkillType; names(cl_map_obj) <- cl_sum_table$Cluster; sk_class_final <- data.table( Element.Name = names(sk_clus_assign), Cluster = sk_clus_assign, SkillType = cl_map_obj[as.character(sk_clus_assign)] ); sk_class_final <- merge(sk_class_final, s_dist_metrics, by = "Element.Name", all.x = TRUE); message("Clasif clúster completada."); return(sk_class_final)
}

# 4.B: Función Clasificación Empírica (Umbrales)
classify_skills_empirical <- function(skill_data, year_to_analyze) {
  message("Clasificando skills empíricamente..."); if (is.null(skill_data) || nrow(skill_data) == 0) stop("Datos skills vacíos"); s_year <- skill_data; if (!is.null(year_to_analyze) && !is.na(year_to_analyze) && "year" %in% names(skill_data)) { temp_data <- skill_data[year == year_to_analyze]; if (nrow(temp_data) > 0) s_year <- temp_data }; s_level <- s_year; if ("Scale.ID" %in% names(s_year)) { temp_level <- s_year[Scale.ID == "LV"]; if(nrow(temp_level)>0) s_level <- temp_level }; sk_metrics <- s_level[, .( usage_count = uniqueN(O.NET.SOC.Code), mean_level = mean(Data.Value, na.rm = TRUE) ), by = .(Element.Name)]; sk_metrics <- sk_metrics[!is.na(usage_count) & !is.na(mean_level)]; if(nrow(sk_metrics) == 0) stop("No se calcularon métricas empíricas."); med_usage <- median(sk_metrics$usage_count, na.rm = TRUE); med_level <- median(sk_metrics$mean_level, na.rm = TRUE); if(is.na(med_usage) || is.na(med_level)) stop("No se calcularon umbrales."); message(sprintf("Umbrales: Uso >= %.1f; Nivel >= %.2f", med_usage, med_level)); sk_metrics[, generality := ifelse(usage_count >= med_usage, "Amplia", "Nicho")]; sk_metrics[, level_cat := ifelse(mean_level >= med_level, "Nivel_Alto", "Nivel_Bajo")]; sk_metrics[, SkillType := paste(generality, level_cat, sep = "_/_")]; sk_class_emp <- sk_metrics[, .(Element.Name, usage_count, mean_level, SkillType)]; message("Clasif empírica completada."); print(sk_class_emp[, .N, by=SkillType]); return(sk_class_emp)
}

# 4.C: Función Clasificación por Red Skill-Skill (VERSIÓN CORREGIDA)
# Función mejorada basada en la metodología de Alabdulkareem et al. (2018)
# Versión corregida final con conteo adecuado de skills
# 4.C: Función Clasificación por Red Skill-Skill - VERSIÓN MEJORADA
# Función mejorada con cálculo de clusters para ambos años (source y target) y estatus de difusión
# Calcula estatus de skills basado en características de las ocupaciones donde son importantes
classify_skills_network <- function(skill_data, 
                                    year_to_analyze, 
                                    complementarity_threshold = 0.1,
                                    calculate_flow_status = TRUE,
                                    comparison_year = NULL,
                                    occupation_attributes = NULL) { 
  message("Clasificando habilidades por Red Skill-Skill (Método Alabdulkareem)...")
  if(!requireNamespace("igraph", quietly = TRUE)) {warning("Paquete 'igraph' necesario."); return(NULL)}
  if (is.null(skill_data) || nrow(skill_data) == 0) {message("Datos skills vacíos."); return(NULL)}
  
  # Guardar año principal y de comparación para mayor claridad
  primary_year <- year_to_analyze
  
  # Si se proporciona un año de comparación, verificamos que existe en los datos
  if(!is.null(comparison_year)) {
    if(!(comparison_year %in% unique(skill_data$year))) {
      message(sprintf("Advertencia: El año de comparación %d no existe en los datos. Se ignorará.", comparison_year))
      comparison_year <- NULL
    } else {
      message(sprintf("Calculando clusters para ambos años: %d (principal) y %d (comparación)",
                      primary_year, comparison_year))
    }
  }
  
  # Procesar skills para cada año
  skill_clusters_list <- list()
  years_to_process <- c(primary_year)
  if(!is.null(comparison_year)) {
    years_to_process <- c(years_to_process, comparison_year)
  }
  
  # Inicializar dataframe final
  all_skills_map_dt <- NULL
  
  # Procesar cada año
  for(current_year in years_to_process) {
    message(sprintf("\n=== Procesando año %d ===", current_year))
    
    # Filtrar datos por año
    dt_year_orig <- skill_data[year == current_year] 
    if(nrow(dt_year_orig) == 0) {
      message(paste("No hay datos para el año", current_year))
      next
    }
    
    # Preservar nombres originales sin crear artificialmente nombres únicos
    dt_year <- copy(dt_year_orig) 
    dt_year[, Element.Name.Original := Element.Name] 
    dt_year[, Element.Name.Clean := Element.Name]
    
    # Extraer mapeo de nombres (solo los realmente únicos)
    year_skills_map_dt <- unique(dt_year[, .(Element.Name.Clean, Element.Name.Original)])
    unique_skills_count <- nrow(year_skills_map_dt)
    message(sprintf("   DEBUG: Total de skills únicas en el año %d: %d", 
                    current_year, unique_skills_count))
    
    # Guardar para uso posterior
    if(is.null(all_skills_map_dt)) {
      all_skills_map_dt <- year_skills_map_dt
    } else {
      all_skills_map_dt <- unique(rbind(all_skills_map_dt, year_skills_map_dt))
    }
    
    # Filtrar por Scale.ID si es necesario 
    message("   Preparando datos para análisis RCA...")
    if ("Scale.ID" %in% names(dt_year)) {
      temp_lv <- dt_year[Scale.ID == "LV"]
      if(nrow(temp_lv) > 0) dt_year <- temp_lv
    }
    
    # Preparar datos para RCA
    dt_year_for_rca <- dt_year[, .(O.NET.SOC.Code, Element.Name, Data.Value)] 
    
    # Verificar conteos
    unique_element_names <- uniqueN(dt_year_for_rca$Element.Name)
    unique_onet_codes <- uniqueN(dt_year_for_rca$O.NET.SOC.Code)
    message(sprintf("   INFO: Preparados %d skills únicas en %d ocupaciones únicas para RCA", 
                    unique_element_names, unique_onet_codes))
    
    # Calcular RCA
    message("   Calculando Revealed Comparative Advantage (RCA)...")
    oc_s <- dt_year_for_rca[, .(os_sum_val = sum(Data.Value, na.rm = TRUE)), by = O.NET.SOC.Code]
    sk_s <- dt_year_for_rca[, .(ss_sum_val = sum(Data.Value, na.rm = TRUE)), by = Element.Name] 
    tot_s_val <- sum(dt_year_for_rca$Data.Value, na.rm = TRUE)
    
    if (tot_s_val == 0) {
      message("   ERROR: Suma Data.Value cero para RCA.")
      next
    }
    
    dt_year_for_rca <- merge(dt_year_for_rca, oc_s, by = "O.NET.SOC.Code", all.x = TRUE)
    dt_year_for_rca <- merge(dt_year_for_rca, sk_s, by = "Element.Name", all.x = TRUE) 
    dt_year_for_rca <- dt_year_for_rca[!is.na(os_sum_val) & !is.na(ss_sum_val) & 
                                         os_sum_val > 1e-9 & ss_sum_val > 1e-9]
    if(nrow(dt_year_for_rca) == 0) {
      message("   ERROR: No quedan datos para calcular RCA.")
      next
    }
    
    # Calcular RCA y determinar uso efectivo
    dt_year_for_rca[, rca := (Data.Value / os_sum_val) / (ss_sum_val / tot_s_val)]
    dt_year_for_rca[is.infinite(rca) | is.nan(rca), rca := 0]
    dt_year_for_rca[, effective_use := rca > 1.0]
    
    # Identificar skills con uso efectivo
    skill_occ_effective <- dt_year_for_rca[effective_use == TRUE, .(O.NET.SOC.Code, Element.Name, rca)]
    skills_with_rca_data <- unique(skill_occ_effective$Element.Name)
    unique_occs_with_rca <- unique(skill_occ_effective$O.NET.SOC.Code)
    
    message(sprintf("   INFO: %d skills únicas tienen uso efectivo (RCA>1) en %d ocupaciones únicas", 
                    length(skills_with_rca_data), 
                    length(unique_occs_with_rca)))
    
    if(length(skills_with_rca_data) < 3) {
      message("   ERROR: Muy pocas skills con uso efectivo RCA>1.")
      next
    }
    
    # CALCULAR ESTATUS DE LAS SKILLS (si se proporcionan datos de ocupaciones)
    skill_status_dt <- NULL
    if(calculate_flow_status && !is.null(occupation_attributes) && nrow(occupation_attributes) > 0) {
      message("   Calculando estatus de skills basado en ocupaciones...")
      
      # Verificar columnas necesarias en occupation_attributes
      required_cols <- c("O.NET.SOC.Code")
      wage_col <- paste0("Median_Wage_", current_year)
      edu_col <- "Edu_Score_Weighted"
      
      # Comprobar si tenemos datos de salario para este año
      has_wage_data <- wage_col %in% names(occupation_attributes)
      has_edu_data <- edu_col %in% names(occupation_attributes)
      
      if(!all(required_cols %in% names(occupation_attributes))) {
        message("   ADVERTENCIA: Faltan columnas requeridas en datos de ocupaciones. No se calculará estatus.")
      } else {
        # Crear tabla base de estatus
        skill_status_dt <- data.table(Element.Name = skills_with_rca_data)
        
        # Calcular estatus por salario
        if(has_wage_data) {
          # Juntar datos de ocupaciones con skills efectivas
          occ_wage_dt <- occupation_attributes[, c("O.NET.SOC.Code", wage_col), with = FALSE]
          setnames(occ_wage_dt, wage_col, "wage")
          
          # Quitar valores faltantes
          occ_wage_dt <- occ_wage_dt[!is.na(wage)]
          
          if(nrow(occ_wage_dt) > 0) {
            # Unir con datos de skills efectivas
            skill_occ_wage <- merge(skill_occ_effective, occ_wage_dt, by = "O.NET.SOC.Code", all.x = FALSE)
            
            # Calcular salario promedio ponderado por RCA para cada skill
            if(nrow(skill_occ_wage) > 0) {
              skill_wage_status <- skill_occ_wage[, .(
                wage_status = weighted.mean(wage, rca, na.rm = TRUE),
                wage_status_unweighted = mean(wage, na.rm = TRUE),
                wage_status_median = median(wage, na.rm = TRUE),
                n_occupations = .N
              ), by = Element.Name]
              
              # Normalizar estatus a z-scores
              mean_wage <- mean(skill_wage_status$wage_status, na.rm = TRUE)
              sd_wage <- sd(skill_wage_status$wage_status, na.rm = TRUE)
              
              if(sd_wage > 0) {
                skill_wage_status[, wage_status_z := (wage_status - mean_wage) / sd_wage]
              } else {
                skill_wage_status[, wage_status_z := 0]
              }
              
              # Unir con tabla de estatus
              skill_status_dt <- merge(skill_status_dt, skill_wage_status, by = "Element.Name", all.x = TRUE)
              message(sprintf("   Se calculó estatus salarial para %d skills", nrow(skill_wage_status)))
            }
          }
        }
        
        # Calcular estatus por educación
        if(has_edu_data) {
          # Juntar datos de ocupaciones con skills efectivas
          occ_edu_dt <- occupation_attributes[, c("O.NET.SOC.Code", edu_col), with = FALSE]
          setnames(occ_edu_dt, edu_col, "education")
          
          # Quitar valores faltantes
          occ_edu_dt <- occ_edu_dt[!is.na(education)]
          
          if(nrow(occ_edu_dt) > 0) {
            # Unir con datos de skills efectivas
            skill_occ_edu <- merge(skill_occ_effective, occ_edu_dt, by = "O.NET.SOC.Code", all.x = FALSE)
            
            # Calcular nivel educativo promedio ponderado por RCA para cada skill
            if(nrow(skill_occ_edu) > 0) {
              skill_edu_status <- skill_occ_edu[, .(
                edu_status = weighted.mean(education, rca, na.rm = TRUE),
                edu_status_unweighted = mean(education, na.rm = TRUE),
                edu_status_median = median(education, na.rm = TRUE)
              ), by = Element.Name]
              
              # Normalizar estatus a z-scores
              mean_edu <- mean(skill_edu_status$edu_status, na.rm = TRUE)
              sd_edu <- sd(skill_edu_status$edu_status, na.rm = TRUE)
              
              if(sd_edu > 0) {
                skill_edu_status[, edu_status_z := (edu_status - mean_edu) / sd_edu]
              } else {
                skill_edu_status[, edu_status_z := 0]
              }
              
              # Unir con tabla de estatus
              skill_status_dt <- merge(skill_status_dt, skill_edu_status, by = "Element.Name", all.x = TRUE)
              message(sprintf("   Se calculó estatus educativo para %d skills", nrow(skill_edu_status)))
            }
          }
        }
        
        # Calcular estatus compuesto
        if("wage_status_z" %in% names(skill_status_dt) && "edu_status_z" %in% names(skill_status_dt)) {
          skill_status_dt[, composite_status := (wage_status_z + edu_status_z) / 2]
          
          # Clasificar en categorías
          skill_status_dt[, status_category := cut(
            composite_status,
            breaks = c(-Inf, -0.5, 0.5, Inf),
            labels = c("LowStatus", "MediumStatus", "HighStatus")
          )]
          
          message("   Se calculó estatus compuesto (salario + educación) para skills")
        } else if("wage_status_z" %in% names(skill_status_dt)) {
          skill_status_dt[, composite_status := wage_status_z]
          
          # Clasificar en categorías
          skill_status_dt[, status_category := cut(
            composite_status,
            breaks = c(-Inf, -0.5, 0.5, Inf),
            labels = c("LowStatus", "MediumStatus", "HighStatus")
          )]
          
          message("   Se calculó estatus compuesto (solo salario) para skills")
        } else if("edu_status_z" %in% names(skill_status_dt)) {
          skill_status_dt[, composite_status := edu_status_z]
          
          # Clasificar en categorías
          skill_status_dt[, status_category := cut(
            composite_status,
            breaks = c(-Inf, -0.5, 0.5, Inf),
            labels = c("LowStatus", "MediumStatus", "HighStatus")
          )]
          
          message("   Se calculó estatus compuesto (solo educación) para skills")
        }
      }
    }
    
    # Crear matriz binaria skill-ocupación
    message("   Creando matriz binaria skill-ocupación...")
    skill_occ_matrix <- dcast(skill_occ_effective, 
                              Element.Name ~ O.NET.SOC.Code,
                              fun.aggregate = length,
                              value.var = "O.NET.SOC.Code", 
                              fill = 0)
    
    skill_occ_matrix[, c(names(skill_occ_matrix)[-1]) := lapply(.SD, function(x) as.integer(x > 0)), 
                     .SDcols = names(skill_occ_matrix)[-1]]
    
    # Verificación adicional de conteo
    message(sprintf("   VERIFICACIÓN: La matriz contiene %d skills únicas en las filas", 
                    nrow(skill_occ_matrix)))
    
    # Verificar que tenemos datos suficientes
    if(ncol(skill_occ_matrix) < 3 || nrow(skill_occ_matrix) < 3) {
      message("   ERROR: Datos insuficientes en matriz skill-ocupación.")
      next
    }
    
    # Calcular complementariedad de skills según Alabdulkareem et al.
    message("   Calculando complementariedad de skills (método Alabdulkareem)...")
    
    # Creamos matriz de complementariedad
    skill_names <- skill_occ_matrix[[1]]
    n_skills <- length(skill_names)
    complementarity_matrix <- matrix(0, nrow = n_skills, ncol = n_skills)
    rownames(complementarity_matrix) <- skill_names
    colnames(complementarity_matrix) <- skill_names
    
    # Para cada par de skills, calculamos su complementariedad
    pb <- NULL
    if(requireNamespace("progress", quietly = TRUE) && n_skills > 50) {
      message("   Inicializando barra de progreso para cálculo de complementariedad...")
      pb <- progress::progress_bar$new(
        format = "   Calculando complementariedad [:bar] :percent ETA: :eta",
        total = n_skills,
        clear = FALSE,
        width = 80
      )
    }
    
    occ_cols <- names(skill_occ_matrix)[-1]
    skill_occupation_data <- as.matrix(skill_occ_matrix[, ..occ_cols])
    
    # Optimización: pre-calcular el número de ocupaciones que usa cada skill
    skill_usage_counts <- rowSums(skill_occupation_data)
    
    message(sprintf("   Calculando complementariedad entre %d skills únicas...", n_skills))
    
    for(i in 1:n_skills) {
      if(!is.null(pb)) pb$tick()
      
      skill_i_occs <- skill_occupation_data[i, ]
      skill_i_count <- skill_usage_counts[i]
      
      if(skill_i_count == 0) next
      
      for(j in (i+1):n_skills) {
        if(j > n_skills) break
        
        skill_j_occs <- skill_occupation_data[j, ]
        skill_j_count <- skill_usage_counts[j]
        
        if(skill_j_count == 0) next
        
        # Número de ocupaciones que usan ambas skills
        both_count <- sum(skill_i_occs & skill_j_occs)
        
        if(both_count > 0) {
          # P(i|j) - probabilidad de encontrar skill i dado skill j
          p_i_given_j <- both_count / skill_j_count
          
          # P(j|i) - probabilidad de encontrar skill j dado skill i
          p_j_given_i <- both_count / skill_i_count
          
          # Complementariedad q = min(P(i|j), P(j|i))
          complementarity <- min(p_i_given_j, p_j_given_i)
          
          complementarity_matrix[i, j] <- complementarity
          complementarity_matrix[j, i] <- complementarity  # Matriz simétrica
        }
      }
    }
    
    # Verificar densidad de la matriz de complementariedad
    connections_pre_threshold <- sum(complementarity_matrix > 0)
    message(sprintf("   INFO: Matriz complementariedad tiene %d conexiones no-cero (%.1f%% del total posible)", 
                    connections_pre_threshold,
                    100 * connections_pre_threshold / (n_skills * n_skills)))
    
    # Aplicar umbral de complementariedad
    if(!is.null(complementarity_threshold) && complementarity_threshold > 0) {
      complementarity_matrix_thresholded <- complementarity_matrix
      complementarity_matrix_thresholded[complementarity_matrix < complementarity_threshold] <- 0
      
      connections_post_threshold <- sum(complementarity_matrix_thresholded > 0)
      retention_pct <- 100 * connections_post_threshold / max(1, connections_pre_threshold)
      
      message(sprintf("   INFO: Después de umbral %.3f quedan %d conexiones (%.1f%% retenido)", 
                      complementarity_threshold, 
                      connections_post_threshold,
                      retention_pct))
      
      # Si el umbral elimina demasiadas conexiones, lo ajustamos automáticamente
      if(connections_post_threshold < n_skills && connections_pre_threshold > n_skills) {
        message("   ADVERTENCIA: Umbral demasiado restrictivo, ajustando automáticamente...")
        
        # Encontrar un umbral apropiado que conserve suficientes conexiones
        potential_thresholds <- seq(0.01, complementarity_threshold, by = 0.01)
        if(min(potential_thresholds) > 0.005) {
          potential_thresholds <- c(0.001, 0.005, potential_thresholds)
        }
        
        for(test_threshold in sort(potential_thresholds, decreasing = TRUE)) {
          test_matrix <- complementarity_matrix
          test_matrix[test_matrix < test_threshold] <- 0
          test_connections <- sum(test_matrix > 0)
          
          # Un buen umbral debería conservar al menos 3 veces el número de nodos
          if(test_connections >= min(3 * n_skills, connections_pre_threshold * 0.05)) {
            complementarity_matrix_thresholded <- test_matrix
            complementarity_threshold <- test_threshold
            connections_post_threshold <- test_connections
            
            message(sprintf("   AJUSTE: Umbral reducido a %.4f, ahora hay %d conexiones (%.1f%% del total)",
                            complementarity_threshold,
                            connections_post_threshold,
                            100 * connections_post_threshold / (n_skills * n_skills)))
            break
          }
        }
        
        # Si ningún umbral funciona, usamos un enfoque basado en percentiles
        if(connections_post_threshold < n_skills) {
          message("   AJUSTE FINAL: Usando enfoque basado en percentiles para umbral...")
          non_zero_values <- complementarity_matrix[complementarity_matrix > 0]
          if(length(non_zero_values) > 0) {
            # Usar el percentil 10 como umbral
            percentile_threshold <- quantile(non_zero_values, 0.1)
            complementarity_matrix_thresholded <- complementarity_matrix
            complementarity_matrix_thresholded[complementarity_matrix < percentile_threshold] <- 0
            
            connections_post_threshold <- sum(complementarity_matrix_thresholded > 0)
            message(sprintf("   UMBRAL PERCENTIL: %.4f con %d conexiones (%.1f%% del total)",
                            percentile_threshold,
                            connections_post_threshold,
                            100 * connections_post_threshold / (n_skills * n_skills)))
          }
        }
      }
      
      complementarity_matrix <- complementarity_matrix_thresholded
    }
    
    # Verificar que tenemos suficientes conexiones para crear el grafo
    if(sum(complementarity_matrix > 0) == 0) {
      message("   ERROR: No quedan conexiones después de aplicar umbral")
      next
    }
    
    # Crear grafo de skills
    message("   Creando grafo de skills para clustering...")
    skill_graph <- igraph::graph_from_adjacency_matrix(
      complementarity_matrix, 
      mode = "undirected", 
      weighted = TRUE,
      diag = FALSE
    )
    
    message(sprintf("   INFO: Grafo para clustering: %d nodos, %d enlaces", 
                    igraph::vcount(skill_graph), 
                    igraph::ecount(skill_graph)))
    
    # Preparar clasificación final para este año
    year_classification <- copy(year_skills_map_dt)
    year_classification[, SkillType := "Network_NoRCAData"] 
    setnames(year_classification, "Element.Name.Clean", "Element.Name.Key") 
    setkey(year_classification, Element.Name.Key)
    
    # Aplicar algoritmo de detección de comunidades
    if(igraph::vcount(skill_graph) >= 3 && igraph::ecount(skill_graph) > 0) {
      message("   Aplicando algoritmo de detección de comunidades...")
      
      # Intentar diferentes algoritmos de clustering
      skill_communities <- tryCatch({
        message("   Probando algoritmo Louvain...")
        igraph::cluster_louvain(skill_graph)
      }, error = function(e) {
        message(paste("   Error en Louvain:", e$message))
        
        # Intentar con Fast Greedy si Louvain falla
        tryCatch({
          message("   Probando algoritmo Fast Greedy...")
          igraph::cluster_fast_greedy(skill_graph)
        }, error = function(e2) {
          message(paste("   Error en Fast Greedy:", e2$message))
          
          # Intentar con algoritmo de Leading Eigenvector
          tryCatch({
            message("   Probando algoritmo Leading Eigenvector...")
            igraph::cluster_leading_eigen(skill_graph)
          }, error = function(e3) {
            message(paste("   Error en Leading Eigenvector:", e3$message))
            
            # Último intento con Label Propagation (muy simple pero robusto)
            tryCatch({
              message("   Probando algoritmo Label Propagation...")
              igraph::cluster_label_prop(skill_graph)
            }, error = function(e4) {
              message(paste("   Error en todos los algoritmos de clustering:", e4$message))
              NULL
            })
          })
        })
      })
      
      # Procesar resultados del clustering
      if(!is.null(skill_communities) && 
         length(igraph::V(skill_graph)$name) == length(skill_communities$membership)) {
        
        num_communities <- max(skill_communities$membership)
        message(sprintf("   ÉXITO: Se encontraron %d comunidades", num_communities))
        
        # Crear tabla con asignaciones de comunidades
        clustered_skills_dt <- data.table(
          Element.Name.Key = as.character(igraph::V(skill_graph)$name),
          CommunityType = paste0("NetCluster_", skill_communities$membership)
        )
        setkey(clustered_skills_dt, Element.Name.Key)
        
        # Mostrar distribución de comunidades
        community_sizes <- table(skill_communities$membership)
        message("   Distribución de tamaños de comunidades:")
        print(community_sizes)
        
        # Actualizar clasificación final
        for(i in 1:nrow(clustered_skills_dt)) {
          key_to_match <- clustered_skills_dt[i, Element.Name.Key]
          comm_type <- clustered_skills_dt[i, CommunityType]
          year_classification[Element.Name.Key == key_to_match, SkillType := comm_type]
        }
        
        # Verificar actualización
        num_updated <- sum(year_classification$SkillType %like% "NetCluster_", na.rm = TRUE)
        message(sprintf("   INFO: %d/%d skills actualizadas con comunidades",
                        num_updated, nrow(clustered_skills_dt)))
        
        if(num_updated == 0 && nrow(clustered_skills_dt) > 0) {
          message("   ALERTA CRÍTICA: Ninguna skill actualizada con comunidades, verificando nombres...")
          
          # Intentar un método más directo de actualización
          year_classification[, SkillType := "Network_ErrorDeCluster"]  # Reset
          
          for(skill_name in skill_names) {
            comm_idx <- match(skill_name, clustered_skills_dt$Element.Name.Key)
            if(!is.na(comm_idx)) {
              comm_type <- clustered_skills_dt[comm_idx, CommunityType]
              year_classification[Element.Name.Key == skill_name, SkillType := comm_type]
            }
          }
          
          # Verificar nuevamente
          num_updated <- sum(year_classification$SkillType %like% "NetCluster_", na.rm = TRUE)
          message(sprintf("   RECUPERACIÓN: %d/%d skills actualizadas tras corrección",
                          num_updated, nrow(clustered_skills_dt)))
        }
      } else {
        message("   ERROR: Detección de comunidades falló. Marcando skills en grafo como 'ErrorDeCluster'")
        year_classification[Element.Name.Key %in% skill_names, 
                            SkillType := "Network_ErrorDeCluster"]
      }
    } else {
      message("   ERROR: Grafo de skills inválido para clustering")
      year_classification[Element.Name.Key %in% skill_names, 
                          SkillType := "Network_GrafoInvalido"]
    }
    
    # Asegurar que todas las skills tienen clasificación
    year_classification[is.na(SkillType), SkillType := "Network_ErrorInesperado"]
    
    # Mostrar resumen
    message(sprintf("Clasificación para el año %d completada:", current_year))
    type_counts <- year_classification[, .N, by = SkillType][order(-N)]
    print(type_counts)
    
    # Renombrar la columna SkillType para indicar el año
    year_suffix <- paste0("_", current_year)
    new_name <- paste0("SkillType", year_suffix)
    setnames(year_classification, "SkillType", new_name)
    
    # Guardar clasificación para este año
    skill_clusters_list[[as.character(current_year)]] <- year_classification
    
    # Si tenemos datos de estatus, los guardamos también
    if(!is.null(skill_status_dt) && nrow(skill_status_dt) > 0) {
      # Renombrar columnas para indicar el año
      status_cols <- setdiff(names(skill_status_dt), "Element.Name")
      for(col in status_cols) {
        new_col_name <- paste0(col, year_suffix)
        setnames(skill_status_dt, col, new_col_name)
      }
      
      # Guardar con el año en el nombre
      skill_clusters_list[[paste0("status_", current_year)]] <- skill_status_dt
    }
  }
  
  # Combinar resultados de todos los años
  final_classification <- all_skills_map_dt
  final_classification[, SkillType := "Network_NoData"]
  
  # Agregar clasificaciones por año
  for(year_key in names(skill_clusters_list)) {
    if(!grepl("^status_", year_key)) {  # Solo procesar clusters (no estatus)
      current_dt <- skill_clusters_list[[year_key]]
      if(!is.null(current_dt) && nrow(current_dt) > 0) {
        # Obtener nombre de la columna SkillType
        skilltype_col <- grep("^SkillType_", names(current_dt), value = TRUE)
        if(length(skilltype_col) > 0) {
          # Unir con resultado final
          final_classification <- merge(
            final_classification, 
            current_dt[, c("Element.Name.Original", skilltype_col), with = FALSE],
            by = "Element.Name.Original", 
            all.x = TRUE
          )
        }
      }
    }
  }
  
  # Para compatibilidad con el resto del código, asignar SkillType del año principal
  if(paste0("SkillType_", primary_year) %in% names(final_classification)) {
    final_classification[, SkillType := get(paste0("SkillType_", primary_year))]
  }
  
  # Agregar datos de estatus si están disponibles
  for(year_key in names(skill_clusters_list)) {
    if(grepl("^status_", year_key)) {  # Solo procesar estatus
      status_year <- as.integer(sub("^status_", "", year_key))
      current_dt <- skill_clusters_list[[year_key]]
      if(!is.null(current_dt) && nrow(current_dt) > 0) {
        # Unir con resultado final
        status_cols <- setdiff(names(current_dt), "Element.Name")
        if(length(status_cols) > 0) {
          final_classification <- merge(
            final_classification, 
            current_dt,
            by.x = "Element.Name.Original", 
            by.y = "Element.Name",
            all.x = TRUE
          )
        }
      }
    }
  }
  
  # Mostrar resumen final
  message("\n=== RESUMEN FINAL ===")
  message(sprintf("Clasificación de %d skills completada con datos para %d año(s).", 
                  nrow(final_classification), length(years_to_process)))
  message("Distribución de tipos de skills en el año principal:")
  main_type_counts <- final_classification[, .N, by = SkillType][order(-N)]
  print(main_type_counts)
  
  # Verificar si tenemos datos de estatus
  status_cols <- grep("_status_z$|composite_status$|status_category$", names(final_classification), value = TRUE)
  if(length(status_cols) > 0) {
    message("Se calcularon las siguientes medidas de estatus:")
    print(status_cols)
  }
  
  # Devolver clasificación final
  return(final_classification)
}


# --- 5. CÁLCULO DE DISTANCIA ESTRUCTURAL ---
# (Función calculate_occupation_distances sin cambios respecto a la última versión)
calculate_occupation_distances <- function(skill_vectors_input, distance_metric_selected = "jaccard_rca", binarization_threshold = 0, backbone_alpha = 0.05) {
  if (is.null(skill_vectors_input) || nrow(skill_vectors_input) < 2 || !all(c("O.NET.SOC.Code", "Element.ID", "Data.Value") %in% names(skill_vectors_input))) { return(NULL) }
  message(sprintf("Calculando distancias: %s.", distance_metric_selected))
  skill_vectors_clean <- skill_vectors_input[!is.na(Data.Value) & !is.na(Element.ID) & !is.na(O.NET.SOC.Code)]
  skill_vectors_clean[, Data.Value := as.numeric(Data.Value)]
  skill_vectors_clean <- skill_vectors_clean[!is.na(Data.Value)]
  if (nrow(skill_vectors_clean) < 2 || length(unique(skill_vectors_clean$O.NET.SOC.Code)) < 2) { return(NULL) }
  occ_codes_original <- sort(unique(skill_vectors_clean$O.NET.SOC.Code))
  occ_skill_matrix_dt <- NULL
  tryCatch({
    occ_skill_matrix_dt <- dcast(skill_vectors_clean[O.NET.SOC.Code %in% occ_codes_original], O.NET.SOC.Code ~ Element.ID, value.var = "Data.Value", fill = 0)
    if (is.null(occ_skill_matrix_dt) || nrow(occ_skill_matrix_dt) <= 1 || ncol(occ_skill_matrix_dt) <= 1) { return(NULL) }
    row_sums_check <- rowSums(occ_skill_matrix_dt[, .SD, .SDcols = -c("O.NET.SOC.Code")])
    if (any(row_sums_check == 0)) {
      occ_skill_matrix_dt <- occ_skill_matrix_dt[row_sums_check > 0]
    }
    if (nrow(occ_skill_matrix_dt) <= 1) { return(NULL) }
  }, error = function(e) {
    warning(paste("Error dcast:", e$message))
    return(NULL)
  })
  if (is.null(occ_skill_matrix_dt) || nrow(occ_skill_matrix_dt) <= 1) { return(NULL) }
  occ_codes_in_matrix <- occ_skill_matrix_dt[["O.NET.SOC.Code"]]
  skill_matrix_for_dist <- tryCatch({
    as.matrix(occ_skill_matrix_dt[, .SD, .SDcols = -c("O.NET.SOC.Code")])
  }, error = function(e) {
    warning(paste("Error matriz:", e$message))
    return(NULL)
  })
  if (is.null(skill_matrix_for_dist) || nrow(skill_matrix_for_dist) < 2 || ncol(skill_matrix_for_dist) < 1) { return(NULL) }
  if (any(!is.finite(skill_matrix_for_dist))) {
    skill_matrix_for_dist[!is.finite(skill_matrix_for_dist)] <- 0
  }
  rownames(skill_matrix_for_dist) <- occ_codes_in_matrix
  dist_matrix_output <- NULL
  intended_dimnames <- list(occ_codes_in_matrix, occ_codes_in_matrix)
  
  tryCatch({
    if (distance_metric_selected == "cosine") {
      row_s_check_cos <- rowSums(abs(skill_matrix_for_dist))
      valid_r_cos <- row_s_check_cos > 1e-9
      valid_occ_codes_cos <- occ_codes_in_matrix[valid_r_cos]
      if (sum(valid_r_cos) < 2) {
        dist_matrix_output <- matrix(1.0, length(occ_codes_in_matrix), length(occ_codes_in_matrix), dimnames = intended_dimnames)
        diag(dist_matrix_output) <- 0
      } else {
        sim_mat_cos <- lsa::cosine(t(as.matrix(skill_matrix_for_dist[valid_r_cos, , drop = FALSE])))
        sim_mat_cos[!is.finite(sim_mat_cos)] <- 0
        dist_mat_cos_p <- 1 - sim_mat_cos
        diag(dist_mat_cos_p) <- 0
        dist_mat_cos_p[dist_mat_cos_p < 0] <- 0
        dist_matrix_output <- matrix(1.0, length(occ_codes_in_matrix), length(occ_codes_in_matrix), dimnames = intended_dimnames)
        dist_matrix_output[valid_occ_codes_cos, valid_occ_codes_cos] <- dist_mat_cos_p
        diag(dist_matrix_output) <- 0
      }
    } else if (distance_metric_selected == "jaccard") {
      message("Calculando Jaccard sobre Data.Value > threshold...")
      skill_mat_bin <- skill_matrix_for_dist > binarization_threshold
      if (nrow(skill_mat_bin) < 2) stop(">= 2 ocupaciones para Jaccard.")
      dist_obj_jaccard <- stats::dist(as.matrix(skill_mat_bin), method = "binary")
      dist_matrix_output <- as.matrix(dist_obj_jaccard)
      if(is.null(rownames(dist_matrix_output))) dimnames(dist_matrix_output) <- intended_dimnames
    } else if (distance_metric_selected == "jaccard_rca") {
      message(sprintf("Calculando Jaccard sobre RCA>1 (Métrica: %s)...", distance_metric_selected))
      rca_matrix <- matrix(0, nrow = nrow(skill_matrix_for_dist), ncol = ncol(skill_matrix_for_dist))
      skill_totals_rca <- colSums(skill_matrix_for_dist)
      occ_totals_rca <- rowSums(skill_matrix_for_dist)
      grand_total_rca <- sum(skill_matrix_for_dist)
      if (any(occ_totals_rca == 0) || any(skill_totals_rca == 0) || grand_total_rca == 0) {
        occ_totals_rca[occ_totals_rca == 0] <- 1e-6
        skill_totals_rca[skill_totals_rca == 0] <- 1e-6
        if (grand_total_rca == 0) grand_total_rca <- 1e-6
      }
      for (i in 1:nrow(skill_matrix_for_dist)) {
        for (j in 1:ncol(skill_matrix_for_dist)) {
          if (occ_totals_rca[i] > 1e-7 && skill_totals_rca[j] > 1e-7 && grand_total_rca > 1e-7) {
            rca_matrix[i, j] <- (skill_matrix_for_dist[i, j] / occ_totals_rca[i]) / (skill_totals_rca[j] / grand_total_rca)
          }
        }
      }
      rca_matrix[!is.finite(rca_matrix)] <- 0
      binary_rca <- rca_matrix > 1.0
      rownames(binary_rca) <- occ_codes_in_matrix # occ_codes_in_matrix son los rownames de skill_matrix_for_dist
      
      # --- INICIO DEL BLOQUE DE CHEQUEO A1 ---
      message("--- CHEQUEO A1: Inspección de 'binary_rca' ---")
      message("Dimensiones de 'binary_rca': ", paste(dim(binary_rca), collapse="x"))
      message("Tabla de valores en 'binary_rca' (TRUEs son RCA>1):")
      print(table(as.vector(binary_rca)))
      message("Suma de RCA>1 por ocupación (primeras 5):")
      print(head(rowSums(binary_rca), 5))
      message("Número de skills con al menos una ocupación con RCA>1: ", sum(colSums(binary_rca) > 0))
      message("Número de ocupaciones con al menos una skill con RCA>1: ", sum(rowSums(binary_rca) > 0))
      message("Primeras 5 filas, 5 columnas de 'binary_rca':")
      print(binary_rca[1:min(5, nrow(binary_rca)), 1:min(5, ncol(binary_rca))])
      message("--- FIN DEL BLOQUE DE CHEQUEO A1 ---")
      
      if(ncol(binary_rca) == 0 || nrow(binary_rca) < 2) {
        warning("Matriz RCA binaria inválida. Placeholder dist=0.5.")
        dist_matrix_output <- matrix(0.5, length(occ_codes_in_matrix), length(occ_codes_in_matrix), dimnames = intended_dimnames)
        diag(dist_matrix_output) <- 0
      } else {
        dist_obj_jaccard_rca <- stats::dist(binary_rca, method = "binary")
        dist_matrix_output <- as.matrix(dist_obj_jaccard_rca)
        
        # --- INICIO DEL BLOQUE DE CHEQUEO A2 ---
        message("--- CHEQUEO A2: 'dist_matrix_output' ANTES de imputar no finitos (dentro de calculate_occupation_distances) ---")
        message("Resumen de 'dist_matrix_output' (cruda de stats::dist):")
        print(summary(as.vector(dist_matrix_output)))
        message(sprintf("Valores no finitos en 'dist_matrix_output': %d NaN, %d Inf",
                        sum(is.nan(dist_matrix_output)), sum(is.infinite(dist_matrix_output))))
        message("--- FIN DEL BLOQUE DE CHEQUEO A2 ---")
        
        if(is.null(rownames(dist_matrix_output)) || is.null(colnames(dist_matrix_output))){
          dimnames(dist_matrix_output) <- intended_dimnames
          warning("Dimnames perdidos en dist_matrix_output, reasignados.", call.=FALSE)
        }
      }
    } else {
      warning(paste("Métrica", distance_metric_selected, "no soportada."))
      dist_matrix_output <- NULL
    }
  }, error = function(e_dist_c) {
    warning(paste("Error calculando distancia:", e_dist_c$message))
    dist_matrix_output <<- NULL # Usar <<- si es necesario para asignar en el entorno correcto en error
  })
  
  if (is.null(dist_matrix_output)) { return(NULL) }
  
  if (is.matrix(dist_matrix_output) && nrow(dist_matrix_output) > 0) {
    current_dimnames <- dimnames(dist_matrix_output)
    if(is.null(current_dimnames) || is.null(current_dimnames[[1]]) || is.null(current_dimnames[[2]])) {
      if(!is.null(intended_dimnames) && length(intended_dimnames[[1]])==nrow(dist_matrix_output)) {
        dimnames(dist_matrix_output) <- intended_dimnames
        current_dimnames <- intended_dimnames
      } else {
        return(NULL)
      }
    }
    if (!isSymmetric(dist_matrix_output, tol = 1e-8)) {
      dist_matrix_output <- (dist_matrix_output + t(dist_matrix_output)) / 2
      dimnames(dist_matrix_output) <- current_dimnames
    }
    if (any(diag(dist_matrix_output) != 0)) {
      diag(dist_matrix_output) <- 0
    }
    if (any(dist_matrix_output < 0, na.rm = TRUE) || any(dist_matrix_output > 1, na.rm = TRUE)) {
      dist_matrix_output[dist_matrix_output < 0] <- 0
      dist_matrix_output[dist_matrix_output > 1] <- 1
    }
    if (any(!is.finite(dist_matrix_output))) {
      finite_vals <- dist_matrix_output[is.finite(dist_matrix_output)]
      mean_dist_val <- if(length(finite_vals) > 0) mean(finite_vals) else 0.5 # Este es el mean_dist_val original
      message(sprintf("IMPUTACIÓN INTERNA (calculate_occupation_distances): Reemplazando %d valores no finitos con mean_dist_val = %f",
                      sum(!is.finite(dist_matrix_output)), mean_dist_val))
      dist_matrix_output[!is.finite(dist_matrix_output)] <- mean_dist_val
      diag(dist_matrix_output) <- 0 # Reasegurar diagonal después de imputar
    }
    if(is.null(rownames(dist_matrix_output)) || is.null(colnames(dist_matrix_output))){
      if(!is.null(intended_dimnames) && length(intended_dimnames[[1]]) == nrow(dist_matrix_output)) {
        dimnames(dist_matrix_output) <- intended_dimnames
      } else {
        warning("Fallo asignar dimnames finales.")
        return(NULL)
      }
    }
  } else {
    return(NULL)
  }
  message("Matriz distancia creada/verificada (después de imputaciones internas). Dim: ", paste(dim(dist_matrix_output), collapse="x"));
  return(dist_matrix_output)
}


# --- 6. CREACIÓN DE RED DE DIFUSIÓN BINARIA ---
# (Función create_binary_diffusion_network completa y corregida, igual que la última versión que te di, con allow.cartesian=TRUE en merges)
create_binary_diffusion_network <- function(skills_data_input,
                                            occupation_attributes_global,
                                            start_year_arg,
                                            end_year_arg,
                                            skill_classification = NULL,
                                            distance_metric_structural = "jaccard_rca",
                                            generate_negatives = TRUE,
                                            negative_sampling_ratio = 1.0) {
  
  message("Iniciando creación de red de difusión binaria (GENERANDO *TODOS* LOS NEGATIVOS v2)...")
  
  # --- VALIDACIONES INICIALES ---
  if (is.null(occupation_attributes_global) || nrow(occupation_attributes_global) == 0) {
    stop("occupation_attributes_global vacío.")
  }
  dt <- as.data.table(skills_data_input)
  req_s_cols <- c("O.NET.SOC.Code", "Element.Name", "Scale.ID", "year", "Data.Value")
  if (!all(req_s_cols %in% names(dt))) {
    stop(paste("Faltan columnas:", paste(setdiff(req_s_cols, names(dt)), collapse = ",")))
  }
  
  # --- FILTRADO INICIAL DE 'dt' (skills_data_input) ---
  dt <- dt[year %in% c(start_year_arg, end_year_arg)]
  dt <- dt[!is.na(O.NET.SOC.Code) & !is.na(Element.Name) & !is.na(Data.Value)]
  dt[, Data.Value := as.numeric(Data.Value)]
  dt <- dt[!is.na(Data.Value)]
  if ("Scale.ID" %in% names(dt)) dt <- dt[Scale.ID %in% c("LV", "IM")]
  
  # --- CREACIÓN DE dt_unique_vals ---
  dt_unique_vals <- dt[, .(Data.Value = mean(Data.Value, na.rm = TRUE)), by = .(O.NET.SOC.Code, Element.Name, year)]
  if (nrow(dt_unique_vals) == 0) stop("No hay datos skills post-filtrado en dt_unique_vals.")
  setkey(dt_unique_vals, O.NET.SOC.Code, Element.Name, year)
  
  # --- CÁLCULO DE RCA ---
  message("Calculando RCA...")
  rca_list <- list()
  tryCatch({
    for (yr_r in c(start_year_arg, end_year_arg)) {
      yr_d_r <- dt_unique_vals[year == yr_r]
      if (nrow(yr_d_r) == 0) { message(sprintf("Advertencia: No hay datos en dt_unique_vals para el año %d para calcular RCA.", yr_r)); next }
      oc_s <- yr_d_r[, .(os_sum_val = sum(Data.Value, na.rm = TRUE)), by = O.NET.SOC.Code]; sk_s <- yr_d_r[, .(ss_sum_val = sum(Data.Value, na.rm = TRUE)), by = Element.Name]; tot_s_val <- sum(yr_d_r$Data.Value, na.rm = TRUE)
      if (tot_s_val == 0) { message(sprintf("Advertencia: Suma total de Data.Value es 0 para el año %d en cálculo de RCA.", yr_r)); next }
      yr_d_r <- merge(yr_d_r, oc_s, by = "O.NET.SOC.Code", all.x = TRUE); yr_d_r <- merge(yr_d_r, sk_s, by = "Element.Name", all.x = TRUE)
      yr_d_r <- yr_d_r[!is.na(os_sum_val) & !is.na(ss_sum_val) & os_sum_val > 1e-9 & ss_sum_val > 1e-9]
      if (nrow(yr_d_r) == 0) { message(sprintf("Advertencia: No hay filas después de merges y filtros de sumas en cálculo de RCA para el año %d.", yr_r)); next }
      yr_d_r[, rca := (Data.Value / os_sum_val) / (ss_sum_val / tot_s_val)]; yr_d_r[is.infinite(rca) | is.nan(rca), rca := 0]; yr_d_r[, effective_use := rca > 1.0]
      rca_list[[as.character(yr_r)]] <- yr_d_r
    }
  }, error = function(e) { stop(paste("Error calculando RCA:", e$message)) })
  if (length(rca_list) < 2 || !as.character(start_year_arg) %in% names(rca_list) || !as.character(end_year_arg) %in% names(rca_list)) {
    message("No se pudo calcular RCA para ambos años. Verifica los datos en dt_unique_vals para cada año."); return(data.table())
  }
  dt_rca_all <- rbindlist(rca_list, fill = TRUE)
  
  # --- DEFINICIÓN DE DATASETS POR AÑO Y OCUPACIONES RELEVANTES ---
  message(sprintf("\nIdentificando difusión: %d -> %d...", start_year_arg, end_year_arg))
  data_orig_rca <- dt_rca_all[year == start_year_arg]
  data_adop_rca <- dt_rca_all[year == end_year_arg]
  if (nrow(data_orig_rca) == 0 || nrow(data_adop_rca) == 0) { message("No hay datos RCA en data_orig_rca o data_adop_rca."); return(data.table()) }
  all_relevant_occs <- unique(c(data_orig_rca$O.NET.SOC.Code, data_adop_rca$O.NET.SOC.Code))
  message(sprintf("Total ocupaciones relevantes (all_relevant_occs) identificadas: %d", length(all_relevant_occs)))
  
  # --- CLASIFICACIÓN DE SKILLS ---
  # ... (Tu código de clasificación de skills aquí, sin cambios) ...
  if (!is.null(skill_classification) && nrow(skill_classification) > 0) {
    message("Preparando clasificación de skills...")
    skill_classification_to_merge <- copy(skill_classification)
    skilltype_col_to_use <- NULL
    if ("SkillType" %in% names(skill_classification_to_merge)) {
      skilltype_col_to_use <- "SkillType"
      message("Usando columna 'SkillType' para clasificación.")
    } else {
      year_specific_col <- paste0("SkillType_", end_year_arg)
      if (year_specific_col %in% names(skill_classification_to_merge)) {
        skilltype_col_to_use <- year_specific_col
        message(sprintf("Usando columna '%s' para clasificación.", year_specific_col))
        skill_classification_to_merge[, SkillType := get(year_specific_col)]
      } else {
        any_skilltype_col <- grep("^SkillType_", names(skill_classification_to_merge), value = TRUE)
        if (length(any_skilltype_col) > 0) {
          skilltype_col_to_use <- any_skilltype_col[1]
          message(sprintf("Usando columna '%s' para clasificación.", skilltype_col_to_use))
          skill_classification_to_merge[, SkillType := get(skilltype_col_to_use)]
        }
      }
    }
    if (!is.null(skilltype_col_to_use) && "SkillType" %in% names(skill_classification_to_merge)) {
      if ("Element.Name" %in% names(skill_classification_to_merge) ||
          "Element.Name.Original" %in% names(skill_classification_to_merge)) {
        element_name_col <- if ("Element.Name" %in% names(skill_classification_to_merge))
          "Element.Name" else "Element.Name.Original"
        skill_classification_to_merge[, (element_name_col) := as.character(get(element_name_col))]
        data_orig_rca[, Element.Name := as.character(Element.Name)]
        data_adop_rca[, Element.Name := as.character(Element.Name)]
        message("Añadiendo clasificación skills...")
        data_orig_rca <- merge( data_orig_rca, skill_classification_to_merge[, .(elem_name = get(element_name_col), SkillType)], by.x = "Element.Name", by.y = "elem_name", all.x = TRUE, allow.cartesian = TRUE )
        data_adop_rca <- merge( data_adop_rca, skill_classification_to_merge[, .(elem_name = get(element_name_col), SkillType)], by.x = "Element.Name", by.y = "elem_name", all.x = TRUE, allow.cartesian = TRUE )
        message(paste("Clasificación añadida."))
      } else { warning("No se encontró columna 'Element.Name' en clasificación de skills.") }
    } else { warning("No se encontró columna 'SkillType' o similar en clasificación de skills.") }
  } else { warning("Clasificación de skills no proporcionada o inválida.") }
  if (!"SkillType" %in% names(data_orig_rca)) data_orig_rca[, SkillType := "no_clasificada"]
  if (!"SkillType" %in% names(data_adop_rca)) data_adop_rca[, SkillType := "no_clasificada"]
  data_orig_rca[is.na(SkillType), SkillType := "no_clasificada"]; data_adop_rca[is.na(SkillType), SkillType := "no_clasificada"]
  
  # --- PREPARACIÓN PARA DISTANCIA ESTRUCTURAL CON FALLBACK ---
  all_skills_unique_names <- unique(c(data_orig_rca$Element.Name, data_adop_rca$Element.Name))
  message(sprintf("Procesando %d skills para eventos de difusión...", length(all_skills_unique_names)))
  dist_s_matrix <- NULL
  
  message(sprintf("Construyendo 'skill_vecs_dist' con fallback (Prioridad %d, Fallback %d) para %d 'all_relevant_occs'.",
                  end_year_arg, start_year_arg, length(all_relevant_occs)))
  
  # 1. Intentar obtener perfiles del end_year_arg (2023)
  profiles_end_year <- dt_unique_vals[
    year == end_year_arg & O.NET.SOC.Code %in% all_relevant_occs & Data.Value > 0,
    .(O.NET.SOC.Code, Element.ID = Element.Name, Data.Value, profile_source_year = as.integer(end_year_arg))
  ]
  occs_with_end_year_profile <- unique(profiles_end_year$O.NET.SOC.Code)
  message(sprintf("Se encontraron perfiles de %d (Data.Value > 0) para %d ocupaciones únicas.",
                  end_year_arg, length(occs_with_end_year_profile)))
  
  # 2. Identificar ocupaciones que necesitan fallback al perfil del start_year_arg (2015)
  occs_needing_fallback <- setdiff(all_relevant_occs, occs_with_end_year_profile)
  message(sprintf("%d de %d ocupaciones relevantes necesitan fallback a perfiles de %d.",
                  length(occs_needing_fallback), length(all_relevant_occs), start_year_arg))
  
  profiles_start_year_fallback <- data.table() # Inicializar tabla vacía
  if (length(occs_needing_fallback) > 0) {
    profiles_start_year_fallback <- dt_unique_vals[
      year == start_year_arg & O.NET.SOC.Code %in% occs_needing_fallback & Data.Value > 0,
      .(O.NET.SOC.Code, Element.ID = Element.Name, Data.Value, profile_source_year = as.integer(start_year_arg))
    ]
    occs_with_start_year_profile_fallback <- unique(profiles_start_year_fallback$O.NET.SOC.Code)
    message(sprintf("Se encontraron perfiles de fallback de %d (Data.Value > 0) para %d de estas ocupaciones.",
                    start_year_arg, length(occs_with_start_year_profile_fallback)))
  }
  
  # 3. Combinar los perfiles
  skill_vecs_dist <- rbindlist(list(profiles_end_year, profiles_start_year_fallback), use.names = TRUE, fill = TRUE)
  
  if ("profile_source_year" %in% names(skill_vecs_dist) && nrow(skill_vecs_dist) > 0) {
    message("Distribución de años de los perfiles en 'skill_vecs_dist' final:")
    print(table(skill_vecs_dist$profile_source_year, useNA = "ifany"))
    skill_vecs_dist[, profile_source_year := NULL] # Eliminar columna auxiliar
  }
  
  skill_vecs_dist <- skill_vecs_dist[!is.na(O.NET.SOC.Code) & !is.na(Element.ID)] # Data.Value ya filtrado >0 y no NA
  
  message(sprintf("Número de filas en 'skill_vecs_dist' final (combinado) antes de llamar a calculate_occupation_distances: %d", nrow(skill_vecs_dist)))
  num_occs_for_dist_matrix <- length(unique(skill_vecs_dist$O.NET.SOC.Code))
  message(sprintf("Número de ocupaciones únicas en 'skill_vecs_dist' final que SÍ entran al cálculo de distancias: %d", num_occs_for_dist_matrix))
  
  # --- CÁLCULO DE DISTANCIA ESTRUCTURAL ---
  if (nrow(skill_vecs_dist) > 1 && num_occs_for_dist_matrix > 1 &&
      exists("calculate_occupation_distances") && is.function(calculate_occupation_distances)) {
    message("Calculando distancia (llamando a calculate_occupation_distances)...")
    dist_s_matrix <- calculate_occupation_distances(
      skill_vecs_dist, # Esta ahora puede tener perfiles de años mixtos
      distance_metric_selected = distance_metric_structural
    )
    if (!is.null(dist_s_matrix) && is.matrix(dist_s_matrix)) {
      message("Matriz distancia calculada y recibida en create_binary_diffusion_network.")
      message(sprintf("Dimensiones de dist_s_matrix recibida: %d x %d", nrow(dist_s_matrix), ncol(dist_s_matrix)))
    } else {
      warning("Fallo cálculo matriz distancia o resultado nulo/no matriz.")
      dist_s_matrix <- NULL
    }
  } else {
    warning("No se calculará distancia estructural (condiciones no cumplidas, función no existe o 'skill_vecs_dist' insuficiente).")
    message(sprintf("Condiciones para calcular distancia: nrow(skill_vecs_dist) > 1 (%s), num_occs_for_dist_matrix > 1 (%s)",
                    nrow(skill_vecs_dist) > 1, num_occs_for_dist_matrix > 1 ))
  }
  
  # --- GENERACIÓN DE EVENTOS DE DIFUSIÓN ---
  # ... (Tu código de generación de eventos aquí, sin cambios) ...
  pb <- NULL; if (requireNamespace("progress", quietly = TRUE)) { pb <- progress::progress_bar$new(total = length(all_skills_unique_names), format = "Procesando skills [:bar] :percent ETA: :eta", clear = FALSE, show_after = 0) }
  diffusion_events <- list(); message("Generando eventos positivos y negativos para cada habilidad...")
  for (skill_name_iter in all_skills_unique_names) {
    tryCatch({
      if (!is.null(pb)) pb$tick()
      occs_orig_effective <- unique(data_orig_rca[Element.Name == skill_name_iter & effective_use == TRUE, O.NET.SOC.Code]); occs_adop_effective <- unique(data_adop_rca[Element.Name == skill_name_iter & effective_use == TRUE, O.NET.SOC.Code]); occs_new_adoption <- setdiff(occs_adop_effective, occs_orig_effective)
      current_skill_type <- unique(data_orig_rca[Element.Name == skill_name_iter, SkillType]); if(length(current_skill_type) == 0 || is.na(current_skill_type[1])) { current_skill_type = "no_clasificada" } else { current_skill_type = current_skill_type[1] }
      if (length(occs_orig_effective) > 0 && length(occs_new_adoption) > 0) {
        data_orig_skill_subset_vals <- data_orig_rca[Element.Name == skill_name_iter, .(O.NET.SOC.Code, data_value_source = Data.Value)]; data_adop_skill_subset_vals <- data_adop_rca[Element.Name == skill_name_iter, .(O.NET.SOC.Code, data_value_target = Data.Value)]; events_pos <- CJ(source = occs_orig_effective, target = occs_new_adoption); events_pos <- events_pos[source != target]
        if(nrow(events_pos) > 0) {
          events_pos[, `:=`(skill_name = skill_name_iter, skill_type = current_skill_type, diffusion = 1L, year_emission = start_year_arg, year_adoption = end_year_arg)]
          events_pos <- merge(events_pos, data_orig_skill_subset_vals, by.x = "source", by.y = "O.NET.SOC.Code", all.x = TRUE, allow.cartesian = TRUE); events_pos <- merge(events_pos, data_adop_skill_subset_vals, by.x = "target", by.y = "O.NET.SOC.Code", all.x = TRUE, allow.cartesian = TRUE)
          cols_to_na_fill_pos <- c("data_value_source", "data_value_target"); for(col_na_pos in cols_to_na_fill_pos){ if(!(col_na_pos %in% names(events_pos))) { events_pos[, (col_na_pos) := 0.0] } else { events_pos[is.na(get(col_na_pos)), (col_na_pos) := 0.0] }}
          diffusion_events <- c(diffusion_events, list(events_pos))
        }
      }
      if (generate_negatives && length(occs_orig_effective) > 0) {
        occs_potential_non_adoption_all <- setdiff(all_relevant_occs, occs_adop_effective)
        if(length(occs_potential_non_adoption_all) > 0) {
          events_neg_all <- CJ(source = occs_orig_effective, target = occs_potential_non_adoption_all); events_neg_all <- events_neg_all[source != target]
          if(nrow(events_neg_all) > 0) {
            events_neg_all[, `:=`(skill_name = skill_name_iter, skill_type = current_skill_type, diffusion = 0L, year_emission = start_year_arg, year_adoption = end_year_arg)]
            data_orig_skill_subset_vals_neg <- data_orig_rca[Element.Name == skill_name_iter, .(O.NET.SOC.Code, data_value_source = Data.Value)]; events_neg_all <- merge(events_neg_all, data_orig_skill_subset_vals_neg, by.x = "source", by.y = "O.NET.SOC.Code", all.x = TRUE, allow.cartesian = TRUE)
            data_adop_skill_subset_vals_neg <- data_adop_rca[Element.Name == skill_name_iter, .(O.NET.SOC.Code, data_value_target = Data.Value)]; events_neg_all <- merge(events_neg_all, data_adop_skill_subset_vals_neg, by.x = "target", by.y = "O.NET.SOC.Code", all.x = TRUE, allow.cartesian = TRUE)
            cols_to_na_fill_neg <- c("data_value_source", "data_value_target"); for(col_na_neg in cols_to_na_fill_neg){ if(!(col_na_neg %in% names(events_neg_all))) { events_neg_all[, (col_na_neg) := 0.0] } else { events_neg_all[is.na(get(col_na_neg)), (col_na_neg) := 0.0] }}
            diffusion_events <- c(diffusion_events, list(events_neg_all))
          }
        }
      }
    }, error = function(e) { warning(paste("Error skill '", skill_name_iter, "': ", e$message), call. = FALSE) })
  }
  if (!exists("diffusion_events", inherits = FALSE) || length(diffusion_events) == 0) { message("No se generaron eventos."); return(data.table()) }
  message("Combinando eventos..."); all_events <- tryCatch({ rbindlist(diffusion_events, fill = TRUE) }, error = function(e){ warning(paste("Error rbindlist:", e$message)); return(data.table()) })
  if(nrow(all_events) == 0) { message("Tabla 'all_events' vacía."); return(data.table()) }
  message(sprintf("Generados %s eventos totales (%s positivos, %s negativos)", format(nrow(all_events), big.mark=","), format(sum(all_events$diffusion == 1, na.rm = TRUE), big.mark=","), format(sum(all_events$diffusion == 0, na.rm = TRUE), big.mark=",")))
  if(nrow(all_events) > 1000000) { warning(paste("Dataset MUY GRANDE:", nrow(all_events)), call.=FALSE); gc() }
  
  # --- ENRIQUECER CON ATRIBUTOS DE OCUPACIONES (sin cambios) ---
  # ...
  message("Enriqueciendo..."); attrs_use <- copy(occupation_attributes_global); edu_col <- "Edu_Score_Weighted"; wage_col_s <- paste0("Median_Wage_", start_year_arg); wage_col_e <- paste0("Median_Wage_", end_year_arg)
  source_attrs_cols <- c("O.NET.SOC.Code", edu_col, wage_col_s); target_attrs_cols <- c("O.NET.SOC.Code", edu_col, wage_col_e)
  source_attrs <- if(nrow(attrs_use) > 0) attrs_use[, .SD, .SDcols = intersect(source_attrs_cols, names(attrs_use))] else data.table(); if (edu_col %in% names(source_attrs)) setnames(source_attrs, old = edu_col, new = "source_education", skip_absent = TRUE); if (wage_col_s %in% names(source_attrs)) setnames(source_attrs, old = wage_col_s, new = "source_wage", skip_absent = TRUE)
  target_attrs <- if(nrow(attrs_use) > 0) attrs_use[, .SD, .SDcols = intersect(target_attrs_cols, names(attrs_use))] else data.table(); if (edu_col %in% names(target_attrs)) setnames(target_attrs, old = edu_col, new = "target_education", skip_absent = TRUE); if (wage_col_e %in% names(target_attrs)) setnames(target_attrs, old = wage_col_e, new = "target_wage", skip_absent = TRUE)
  if(nrow(source_attrs) > 0 && "O.NET.SOC.Code" %in% names(source_attrs) && "source" %in% names(all_events)) { all_events <- merge(all_events, source_attrs, by.x = "source", by.y = "O.NET.SOC.Code", all.x = TRUE, allow.cartesian=TRUE) } else { if(!("source_education" %in% names(all_events))) all_events[, source_education := NA_real_]; if(!("source_wage" %in% names(all_events))) all_events[, source_wage := NA_real_] }
  if(nrow(target_attrs) > 0 && "O.NET.SOC.Code" %in% names(target_attrs) && "target" %in% names(all_events)) { all_events <- merge(all_events, target_attrs, by.x = "target", by.y = "O.NET.SOC.Code", all.x = TRUE, allow.cartesian=TRUE) } else { if(!("target_education" %in% names(all_events))) all_events[, target_education := NA_real_]; if(!("target_wage" %in% names(all_events))) all_events[, target_wage := NA_real_] }
  
  # --- AÑADIR DISTANCIA ESTRUCTURAL (CON CHEQUEO B1) ---
  # El bloque de CHEQUEO B1 que ya tienes es muy importante aquí y no necesita cambios.
  # Asegúrate de que la función calculate_occupation_distances también tiene sus chequeos A1 y A2 como antes.
  message("Añadiendo distancia (puede tardar)...")
  if (!is.null(dist_s_matrix) && is.matrix(dist_s_matrix) && nrow(dist_s_matrix) > 0 && ncol(dist_s_matrix) > 0 &&
      !is.null(rownames(dist_s_matrix)) && !is.null(colnames(dist_s_matrix))) {
    median_overall_dist_enrich <- median(dist_s_matrix[!is.na(dist_s_matrix) & is.finite(dist_s_matrix)], na.rm = TRUE)
    if (is.na(median_overall_dist_enrich) || !is.finite(median_overall_dist_enrich)) { median_overall_dist_enrich <- 0.5 }
    rn_dist <- rownames(dist_s_matrix); cn_dist <- colnames(dist_s_matrix)
    dist_dt_for_melt <- as.data.table(dist_s_matrix, keep.rownames = "s_dist_col")
    dist_dt_long_for_merge <- melt(dist_dt_for_melt, id.vars = "s_dist_col", measure.vars = cn_dist, variable.name = "t_dist_col", value.name = "structural_distance_val", variable.factor = FALSE)
    setkeyv(dist_dt_long_for_merge, c("s_dist_col", "t_dist_col"))
    all_events[, source_char_join_key := as.character(source)]; all_events[, target_char_join_key := as.character(target)]; setkeyv(all_events, c("source_char_join_key", "target_char_join_key"))
    if("structural_distance" %in% names(all_events)) all_events[, structural_distance := NULL]
    all_events <- merge(all_events, dist_dt_long_for_merge, by.x = c("source_char_join_key", "target_char_join_key"), by.y = c("s_dist_col", "t_dist_col"), all.x = TRUE, allow.cartesian = TRUE)
    
    # --- INICIO DEL BLOQUE DE CHEQUEO B1 ---
    message("--- CHEQUEO B1: Estado de 'structural_distance_val' ANTES de imputación final ---")
    message("Resumen de 'structural_distance_val' después del merge:"); print(summary(all_events$structural_distance_val))
    message(sprintf("Total NAs en 'structural_distance_val': %d de %d filas (%.2f%%)", sum(is.na(all_events$structural_distance_val)), nrow(all_events), (sum(is.na(all_events$structural_distance_val))/nrow(all_events))*100 ))
    message(sprintf("Total !is.finite en 'structural_distance_val' (excl. NAs): %d de %d filas", sum(!is.finite(all_events$structural_distance_val) & !is.na(all_events$structural_distance_val)), nrow(all_events)))
    message("Valor de 'median_overall_dist_enrich' que se usará para imputar: ", median_overall_dist_enrich)
    message("Resumen de la matriz de distancias original 'dist_s_matrix' (vectorizada):"); if (!is.null(dist_s_matrix)) print(summary(as.vector(dist_s_matrix))) else message("'dist_s_matrix' es NULL")
    message("Primeras filas y columnas de 'dist_s_matrix':"); if (!is.null(dist_s_matrix) && ncol(dist_s_matrix) > 0 && nrow(dist_s_matrix) >0) print(head(dist_s_matrix[1:min(nrow(dist_s_matrix),5),1:min(5, ncol(dist_s_matrix))], 5)) else message("'dist_s_matrix' es NULL, sin filas o sin columnas")
    if(!is.null(dist_s_matrix) && nrow(dist_s_matrix) > 0) {
      codes_in_dist_matrix_check <- rownames(dist_s_matrix); codes_in_events_source_check <- unique(all_events$source_char_join_key); codes_in_events_target_check <- unique(all_events$target_char_join_key) 
      message(sprintf("Primeros 5 códigos en rownames(dist_s_matrix): %s", paste(head(codes_in_dist_matrix_check,5), collapse=", ")))
      message(sprintf("Primeros 5 códigos en all_events$source_char_join_key (únicos): %s", paste(head(codes_in_events_source_check,5), collapse=", ")))
      source_in_matrix_check <- sum(codes_in_events_source_check %in% codes_in_dist_matrix_check); target_in_matrix_check <- sum(codes_in_events_target_check %in% codes_in_dist_matrix_check)
      message(sprintf("Coincidencia de códigos SOURCE: %d de %d códigos de 'source_char_join_key' están en rownames(dist_s_matrix).", source_in_matrix_check, length(codes_in_events_source_check)))
      message(sprintf("Coincidencia de códigos TARGET: %d de %d códigos de 'target_char_join_key' están en rownames(dist_s_matrix).", target_in_matrix_check, length(codes_in_events_target_check)))
      if(length(codes_in_events_source_check) > source_in_matrix_check) { message("Ejemplos de códigos SOURCE no encontrados en la matriz de distancia:"); print(head(setdiff(codes_in_events_source_check, codes_in_dist_matrix_check), 10)) }
      if(length(codes_in_events_target_check) > target_in_matrix_check) { message("Ejemplos de códigos TARGET no encontrados en la matriz de distancia:"); print(head(setdiff(codes_in_events_target_check, codes_in_dist_matrix_check), 10)) }
    } else { message ("'dist_s_matrix' es NULL o vacía, no se puede chequear coincidencia de códigos.") }
    message("--- FIN DEL BLOQUE DE CHEQUEO B1 ---")
    
    all_events[is.na(structural_distance_val) | !is.finite(structural_distance_val), structural_distance_val := median_overall_dist_enrich]
    setnames(all_events, "structural_distance_val", "structural_distance"); all_events[, c("source_char_join_key", "target_char_join_key") := NULL]
    message(sprintf("Distancia añadida. NAs (después de imputación): %d", sum(is.na(all_events$structural_distance))))
  } else {
    message("Matriz distancia ('dist_s_matrix') no disponible o inválida al momento del merge. 'structural_distance' será 0.5 por defecto.")
    all_events[, structural_distance := 0.5]
  }
  
  # --- RESTO DE LA FUNCIÓN (calcular diferencias, añadir estatus, etc.) ---
  # ... (Tu código para calcular diferencias, eliminar columnas, añadir estatus, sin cambios) ...
  message("Calculando diferencias...")
  for (diff_col_iter in c("education", "wage")) {
    source_col_name <- paste0("source_", diff_col_iter); target_col_name <- paste0("target_", diff_col_iter); diff_col_abs_name <- paste0(diff_col_iter, "_diff_abs"); diff_col_rel_name <- paste0(diff_col_iter, "_diff_rel")
    if (all(c(source_col_name, target_col_name) %in% names(all_events))) {
      if(is.numeric(all_events[[source_col_name]]) && is.numeric(all_events[[target_col_name]])) {
        all_events[, (diff_col_abs_name) := get(target_col_name) - get(source_col_name)]; all_events[, (diff_col_rel_name) := fifelse( abs(get(source_col_name)) > 1e-9, get(target_col_name) / get(source_col_name) - 1, NA_real_ )]
      } else { all_events[, c(diff_col_abs_name, diff_col_rel_name) := NA_real_] }
    } else { all_events[, c(diff_col_abs_name, diff_col_rel_name) := NA_real_] }
  }
  cols_to_remove <- intersect(c("rca_source", "rca_target", "weight", "rca", "effective_use"), names(all_events))
  if(length(cols_to_remove) > 0) { all_events[, (cols_to_remove) := NULL]; message(paste("Columnas redundantes eliminadas:", paste(cols_to_remove, collapse=", "))) }
  if (!is.null(skill_classification) && nrow(skill_classification) > 0) {
    status_cols <- grep("^status_category_|^composite_status_", names(skill_classification), value = TRUE)
    if (length(status_cols) > 0) {
      message("Añadiendo información de estatus de skills...")
      status_col_end_year <- paste0("status_category_", end_year_arg); chosen_status_col <- if (status_col_end_year %in% status_cols) status_col_end_year else status_cols[1]
      element_name_col_status <- if ("Element.Name" %in% names(skill_classification)) "Element.Name" else if ("Element.Name.Original" %in% names(skill_classification)) "Element.Name.Original" else NULL
      if (!is.null(element_name_col_status) && chosen_status_col %in% names(skill_classification)) {
        message(sprintf("Usando columna de estatus '%s' y columna de nombre '%s'", chosen_status_col, element_name_col_status))
        status_map <- unique(skill_classification[, .SD, .SDcols = c(element_name_col_status, chosen_status_col)]); setnames(status_map, old=c(element_name_col_status, chosen_status_col), new=c("skill_name_map_key", "skill_status_val"))
        if (nrow(status_map) > 0 && "skill_name" %in% names(all_events)) {
          all_events <- merge( all_events, status_map, by.x = "skill_name", by.y = "skill_name_map_key", all.x = TRUE ); setnames(all_events, "skill_status_val", "skill_status", skip_absent=TRUE) 
          if ("skill_status" %in% names(all_events)) { all_events[is.na(skill_status), skill_status := "UnknownStatus"]; message(sprintf("Información de estatus añadida de columna '%s'", chosen_status_col))
          } else { message("Columna 'skill_status_val' no se pudo renombrar a 'skill_status' (no se unió correctamente).") }
        }
      } else { message(sprintf("No se pudo añadir información de estatus. Columna de nombre: '%s', Columna de estatus: '%s'", ifelse(is.null(element_name_col_status), "NULL", element_name_col_status), chosen_status_col)) }
    }
  }
  
  message("Creación base eventos completa. Dim: ", paste(dim(all_events), collapse="x"))
  setkey(all_events, NULL)
  if(nrow(all_events) > 1000000){ message("\n!!! ADVERTENCIA: Dataset 'all_events' MUY GRANDE:", format(nrow(all_events), big.mark=","), "filas. !!!\n") }
  return(all_events)
}


# --- Bloque de Ejecución: Crear y Guardar la Base de Datos de Eventos ---
message("\n--- Bloque de Ejecución: Creando la Base de Datos 'all_events' ---")

data_ready_exec <- exists("all_skills_data", mode="list") && 
  exists("occupation_attributes_final", mode="list") && 
  is.data.table(all_skills_data) && 
  is.data.table(occupation_attributes_final) &&
  nrow(all_skills_data) > 0 && 
  nrow(occupation_attributes_final) > 0

if (data_ready_exec) {
  analysis_start_year <- 2015
  analysis_end_year <- 2023 
  years_available <- unique(all_skills_data$year)
  if (!is.null(years_available) && all(!is.na(years_available)) && 
      (!analysis_start_year %in% years_available || !analysis_end_year %in% years_available)) {
    warning(paste("Años", analysis_start_year, "-", analysis_end_year, "no tienen datos completos."), call. = FALSE)
  } else if (is.null(years_available) || any(is.na(years_available))) {
    warning("Columna 'year' en 'all_skills_data' es NA o no existe.", call.=FALSE)
  } else {
    chosen_skill_classification_type <- "network" 
    chosen_distance_metric <- "jaccard_rca" 
    message(paste("\n*** Iniciando creación de 'all_events' para", analysis_start_year, "-", analysis_end_year, "***"))
    message(paste("   Usando clasificación de Skills:", chosen_skill_classification_type))
    message(paste("   Usando métrica de Distancia:", chosen_distance_metric))
    
    skill_classification_output <- NULL
    skill_type_col_final_name <- "SkillType" 
    
    if (chosen_skill_classification_type == "cluster") {
      message("Ejecutando clasificación por clúster...")
      skill_classification_output <- classify_skills_by_type(all_skills_data, analysis_end_year, 3)
      if(!is.null(skill_classification_output)) 
        setnames(skill_classification_output, "SkillType", skill_type_col_final_name, skip_absent=TRUE)
    } else if (chosen_skill_classification_type == "empirical_threshold") {
      message("Ejecutando clasificación empírica...")
      skill_classification_output <- classify_skills_empirical(all_skills_data, analysis_end_year)
      if(!is.null(skill_classification_output)) 
        setnames(skill_classification_output, "SkillType", skill_type_col_final_name, skip_absent=TRUE)
    } else if (chosen_skill_classification_type == "network") {
      message("Ejecutando clasificación por red...")
      # Usar la función mejorada que ahora calcula estatus y soporta múltiples años
      skill_classification_output <- classify_skills_network(
        all_skills_data, 
        analysis_end_year, 
        complementarity_threshold = 0.1,
        calculate_flow_status = TRUE,           # Activar cálculo de estatus
        comparison_year = analysis_start_year,  # Calcular también para año inicial
        occupation_attributes = occupation_attributes_final  # Pasar datos de ocupaciones
      )
      
      if(!is.null(skill_classification_output)) 
        setnames(skill_classification_output, "SkillType", skill_type_col_final_name, skip_absent=TRUE)
    } else if (chosen_skill_classification_type == "predefined") {
      cat("Selecciona archivo CSV de clasificación predefinida.\n")
      Sys.sleep(1)
      predef_file_path <- try(file.choose(), silent = TRUE)
      if(!inherits(predef_file_path, "try-error") && length(predef_file_path) > 0 && file.exists(predef_file_path)){
        skill_classification_output <- tryCatch(fread(predef_file_path), error = function(e) NULL)
        if(!is.null(skill_classification_output) && 
           ("Element.Name" %in% names(skill_classification_output) && 
            "SkillType" %in% names(skill_classification_output))) {
          # Ok
        } else {
          skill_classification_output <- NULL
        }
      } else {
        skill_classification_output <- NULL
      }
      if(is.null(skill_classification_output)) warning("No se cargó clasificación predefinida.")
    } else {
      message("No se realizará clasificación de skills.")
    }
    
    # Verificar y guardar la clasificación de skills
    if(!is.null(skill_classification_output) && skill_type_col_final_name %in% names(skill_classification_output)){
      classif_filename <- file.path(output_data_dir, paste0("skill_classification_", chosen_skill_classification_type, ".csv"))
      message("Guardando clasificación en: ", classif_filename)
      fwrite(skill_classification_output, classif_filename)
      
      # Guardar también la clasificación con datos de estatus si están disponibles
      status_cols <- grep("_status_|status_category", names(skill_classification_output), value = TRUE)
      if(length(status_cols) > 0) {
        status_classif_filename <- file.path(output_data_dir, 
                                             paste0("skill_classification_", chosen_skill_classification_type, "_with_status.csv"))
        message("Guardando clasificación con datos de estatus en: ", status_classif_filename)
        fwrite(skill_classification_output, status_classif_filename)
        
        # Mostrar resumen de estatus
        if("status_category_2023" %in% names(skill_classification_output)) {
          message("Distribución de categorías de estatus (2023):")
          print(table(skill_classification_output$status_category_2023, useNA="ifany"))
        }
      }
      
      # Si hay datos para ambos años, guardar una tabla de comparación
      if(all(c("SkillType_2015", "SkillType_2023") %in% names(skill_classification_output))) {
        message("Generando tabla de cambios en clusters entre 2015 y 2023...")
        cluster_changes <- skill_classification_output[, .(
          Element.Name = Element.Name.Original,
          Cluster_2015 = SkillType_2015,
          Cluster_2023 = SkillType_2023,
          Cambio = ifelse(SkillType_2015 == SkillType_2023, "Sin cambio", "Cambió de cluster")
        )]
        
        changes_filename <- file.path(output_data_dir, "skill_cluster_changes_2015_2023.csv")
        message("Guardando tabla de cambios en: ", changes_filename)
        fwrite(cluster_changes, changes_filename)
        
        # Mostrar resumen de cambios
        message("Resumen de cambios en clusters:")
        print(table(cluster_changes$Cambio, useNA="ifany"))
      }
    }
    
    # Ejecutar la función create_binary_diffusion_network con la clasificación
    all_events_final <- create_binary_diffusion_network(
      skills_data_input = all_skills_data, 
      occupation_attributes_global = occupation_attributes_final, 
      start_year_arg = analysis_start_year, 
      end_year_arg = analysis_end_year, 
      skill_classification = skill_classification_output, 
      distance_metric_structural = chosen_distance_metric, 
      generate_negatives = TRUE 
    )
    
    if (!is.null(all_events_final) && nrow(all_events_final) > 0) {
      rds_filename <- file.path(output_data_dir, "all_diffusion_events_COMPLETE.rds")
      csv_filename <- file.path(output_data_dir, "all_diffusion_events_COMPLETE.csv")
      
      message("\nGuardando base de datos de eventos completa...")
      message("   Archivo RDS: ", rds_filename)
      saveRDS(all_events_final, rds_filename)
      
      message("   Archivo CSV: ", csv_filename, " (puede tardar y ser grande)")
      tryCatch({
        fwrite(all_events_final, csv_filename)
      }, error = function(e){
        warning(paste("Error guardando CSV:", e$message), call. = FALSE)
      })
      
      message("\n--- Base de datos 'all_events' creada y guardada. ---")
      message("Resumen de difusión en 'all_events_final':")
      print(table(all_events_final$diffusion, useNA="ifany"))
      
      if("skill_type" %in% names(all_events_final)){
        message("Resumen de skill_type en 'all_events_final':")
        print(table(all_events_final$skill_type, useNA="ifany"))
      }
      
      # Si existen datos de estatus, crear resumen adicional
      if("status_category_2023" %in% names(skill_classification_output)) {
        # Añadir estatus a la tabla de eventos si no está ya
        if(!"skill_status" %in% names(all_events_final) && 
           "skill_name" %in% names(all_events_final)) {
          
          # Crear dataframe de mapeo de skill a estatus
          status_map <- skill_classification_output[, .(
            Element.Name.Original, 
            skill_status = status_category_2023
          )]
          
          # Unir con eventos
          all_events_status <- merge(
            all_events_final,
            status_map,
            by.x = "skill_name",
            by.y = "Element.Name.Original",
            all.x = TRUE
          )
          
          # Si se añadieron datos de estatus, guardar esta versión
          if("skill_status" %in% names(all_events_status)) {
            message("\nGuardando base de datos de eventos con estatus...")
            
            status_rds_filename <- file.path(output_data_dir, "all_diffusion_events_with_status.rds")
            message("   Archivo RDS: ", status_rds_filename)
            saveRDS(all_events_status, status_rds_filename)
            
            # Mostrar resumen de difusión por estatus
            message("Resumen de difusión por estatus:")
            status_diffusion_table <- table(all_events_status$diffusion, all_events_status$skill_status)
            print(status_diffusion_table)
          }
        }
      }
    } else {
      warning("Tabla final 'all_events' vacía o NULL.")
    }
  } 
} else {
  warning("Datos necesarios no disponibles o vacíos. Ejecuta Pasos 3.A y 3.B primero.", call. = FALSE)
}

library(tidyverse)
glimpse(all_events_final)
