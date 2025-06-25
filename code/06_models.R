
# --- 0. CARGAR PAQUETES NECESARIOS ---
# install.packages(c("data.table", "dplyr", "pROC", "ggplot2", "ggeffects", "car", "ggrepel"))
library(data.table)
library(dplyr)
library(pROC)
library(ggplot2)
library(ggeffects)
library(car) # Para VIF
library(ggrepel) # Para etiquetas de texto que no se solapen

# --- 1. PREPARACIÓN DE DATOS ---
# Asumiendo que 'diffusion_events' es tu data.table ya cargada.
# Usaremos 'diffusion_events' como si fuera 'all_events_final' de tu código de presentación.
if (!exists("diffusion_events") || !is.data.table(diffusion_events) || nrow(diffusion_events) == 0) {
  stop("La tabla 'diffusion_events' no existe, está vacía o no es un data.table. Por favor, cárgala primero.")
}
setDT(diffusion_events)

message("Paso 1: Preparación de Datos Inicial...")

# ----- 1.1 Imputación de NAs en Salarios (replicando tu lógica) -----
if ("target_wage" %in% names(diffusion_events) && "source_wage" %in% names(diffusion_events)) {
  median_target_wage <- median(diffusion_events$target_wage, na.rm = TRUE)
  if (is.na(median_target_wage)) median_target_wage <- 0 # Fallback si todos son NA
  diffusion_events[, target_wage_original_was_na := is.na(target_wage)]
  diffusion_events[is.na(target_wage), target_wage := median_target_wage]
  diffusion_events[, target_wage_imputed_indicator := factor(target_wage_original_was_na,
                                                             levels = c(FALSE, TRUE),
                                                             labels = c("Observed", "Imputed"))]
  
  median_source_wage <- median(diffusion_events$source_wage, na.rm = TRUE)
  if (is.na(median_source_wage)) median_source_wage <- 0 # Fallback
  diffusion_events[, source_wage_original_was_na := is.na(source_wage)]
  diffusion_events[is.na(source_wage), source_wage := median_source_wage]
  diffusion_events[, source_wage_imputed_indicator := factor(source_wage_original_was_na,
                                                             levels = c(FALSE, TRUE),
                                                             labels = c("Observed", "Imputed"))]
  message("Imputación de NAs en salarios completada.")
} else {
  warning("Columnas 'target_wage' o 'source_wage' no encontradas. No se realizó imputación de salarios.")
  # Crear indicadores dummy si no existen, para que la fórmula no falle después
  if (!"target_wage_imputed_indicator" %in% names(diffusion_events)) diffusion_events[, target_wage_imputed_indicator := factor("NotApplicable")]
  if (!"source_wage_imputed_indicator" %in% names(diffusion_events)) diffusion_events[, source_wage_imputed_indicator := factor("NotApplicable")]
}


# ----- 1.2 Calcular Variables de Diferencia DIRECTA (con signo) y sus Cuadrados -----
message("Creando variables de diferencia directa y sus cuadrados...")
if ("target_education" %in% names(diffusion_events) && "source_education" %in% names(diffusion_events)) {
  diffusion_events[, education_diff := target_education - source_education]
  diffusion_events[, education_diff_sq := education_diff^2]
} else { warning("Variables de educación para 'education_diff' no encontradas.")}

if ("target_wage" %in% names(diffusion_events) && "source_wage" %in% names(diffusion_events)) {
  diffusion_events[, wage_diff := target_wage - source_wage]
  diffusion_events[, wage_diff_sq := wage_diff^2]
} else { warning("Variables de salario para 'wage_diff' no encontradas.")}

if ("structural_distance" %in% names(diffusion_events) && !"structural_distance_sq" %in% names(diffusion_events)) {
  diffusion_events[, structural_distance_sq := structural_distance^2]
} else if (!"structural_distance" %in% names(diffusion_events)) { warning("'structural_distance' no encontrada.")}

gc()
# ----- 1.3 Estandarización de Predictores Continuos -----
message("Estandarizando predictores continuos...")
# Lista de variables originales (NO _sq) que se van a estandarizar
# Los términos _sq se crearán y luego se estandarizarán sus versiones _std también si existen.
cols_to_standardize_base <- c("structural_distance", "education_diff", "wage_diff",
                              "source_wage", "source_education", "data_value_source",
                              "Prob_Transicion_Weighted", "source_job_zone") 
# Añade aquí otras variables continuas base que uses

# También estandarizaremos los términos cuadráticos correspondientes
cols_to_standardize_sq <- c("structural_distance_sq", "education_diff_sq", "wage_diff_sq")

cols_to_standardize_final <- unique(c(cols_to_standardize_base, cols_to_standardize_sq))

for (col in intersect(cols_to_standardize_final, names(diffusion_events))) {
  if (is.numeric(diffusion_events[[col]])) {
    mean_val <- mean(diffusion_events[[col]], na.rm = TRUE)
    sd_val <- sd(diffusion_events[[col]], na.rm = TRUE)
    if (!is.na(sd_val) && sd_val > 0 && is.finite(mean_val)) {
      diffusion_events[, (paste0(col, "_std")) := (get(col) - mean_val) / sd_val]
    } else {
      diffusion_events[, (paste0(col, "_std")) := 0] # Si sd es 0, NA o media no finita
      warning(paste("SD para la columna '", col, "' fue cero, NA o media no finita. Estandarizada a 0."))
    }
  }
}
message("Estandarización completada.")

# ----- 1.4 Asegurar Factores para el Modelo -----
diffusion_events[, diffusion_numeric := as.integer(diffusion)] # Para glm
if (!"diffusion_factor" %in% names(diffusion_events) || !is.factor(diffusion_events$diffusion_factor)) {
  diffusion_events[, diffusion_factor := factor(ifelse(diffusion == 1, "Yes", "No"), levels = c("No", "Yes"))]
}
if ("skill_type" %in% names(diffusion_events) && !is.factor(diffusion_events$skill_type)) {
  diffusion_events[, skill_type := as.factor(skill_type)]
} else if (!"skill_type" %in% names(diffusion_events)) {stop("Columna 'skill_type' es necesaria.")}

if ("skill_status" %in% names(diffusion_events) && !is.factor(diffusion_events$skill_status)) {
  diffusion_events[, skill_status := as.factor(skill_status)]
} else if (!"skill_status" %in% names(diffusion_events)) { # Si no existe, crearla como NA o un valor por defecto para que la fórmula no falle
  diffusion_events[, skill_status := factor(NA)]
  warning("Columna 'skill_status' no encontrada, creada como factor con NAs.")
}


# ----- 1.5 División de Datos -----
set.seed(123) # Para reproducibilidad
train_indices <- sample(1:nrow(diffusion_events), floor(0.7 * nrow(diffusion_events)))
train_data <- diffusion_events[train_indices, ]
test_data <- diffusion_events[-train_indices, ]
message(paste("Datos divididos: Entrenamiento =", nrow(train_data), "filas, Prueba =", nrow(test_data), "filas."))

# --- 2. MODELADO POR CLUSTER (skill_type) ---
message("\n--- PASO 2: Modelado por Cluster (skill_type) ---")

skill_type_levels <- levels(train_data$skill_type)
if (is.null(skill_type_levels)) stop ("skill_type no es un factor o no tiene niveles.")
message(paste("Niveles de skill_type a modelar:", paste(skill_type_levels, collapse=", ")))

list_of_cluster_models_final <- list()
list_of_cluster_aucs_final <- list()

# Definir los predictores ESTANDARIZADOS para la fórmula base
# Usaremos las diferencias directas (con signo) y sus cuadrados, todos estandarizados.
# Y el cuadrático para structural_distance_std.
base_predictors_std_cluster <- c(
  "Prob_Transicion_Weighted_std", # Si lo estandarizaste
  "skill_status",                 # Factor, no se estandariza
  "data_value_source_std",        # Si lo estandarizaste
  "source_job_zone_std",          # Si lo estandarizaste
  "structural_distance_std", "structural_distance_sq_std",
  "education_diff_std", "education_diff_sq_std",
  "wage_diff_std", "wage_diff_sq_std",
  "target_wage_imputed_indicator", # Factor
  "source_wage_imputed_indicator"  # Factor
)
# Filtrar por los que realmente existen tras la estandarización
base_predictors_std_cluster <- intersect(base_predictors_std_cluster, names(train_data))
message(paste("Predictores base (estandarizados) para modelos por cluster:", paste(base_predictors_std_cluster, collapse=", ")))


for (current_cluster in skill_type_levels) {
  message(paste("\n--- Ajustando modelo final para skill_type:", current_cluster, "---"))
  train_subset <- train_data[skill_type == current_cluster, ]
  test_subset <- test_data[skill_type == current_cluster, ]
  
  message(paste("Filas en train_subset para", current_cluster, ":", nrow(train_subset)))
  
  min_obs_train <- max(200, length(base_predictors_std_cluster) * 15) # Mínimo más robusto
  min_obs_test <- 50   
  
  if (nrow(train_subset) < min_obs_train || length(unique(train_subset$diffusion_numeric)) < 2) {
    message(paste("Datos de entrenamiento insuficientes para", current_cluster, ". Saltando."))
    list_of_cluster_models_final[[current_cluster]] <- NULL
    list_of_cluster_aucs_final[[current_cluster]] <- NA
    next
  }
  
  current_predictors_dynamic_final <- c(base_predictors_std_cluster)
  factor_vars_in_formula_final <- current_predictors_dynamic_final[sapply(current_predictors_dynamic_final, function(p) is.factor(train_subset[[p]]))]
  
  potential_model_vars_final <- c("diffusion_numeric", current_predictors_dynamic_final)
  train_subset_for_na_check_final <- train_subset[, ..potential_model_vars_final, drop = FALSE]
  train_subset_after_na_omit_final <- na.omit(train_subset_for_na_check_final)
  
  if(nrow(train_subset_after_na_omit_final) < min_obs_train || length(unique(train_subset_after_na_omit_final$diffusion_numeric)) < 2) {
    message(paste("Después de na.omit, datos insuficientes para", current_cluster, ". Saltando."))
    list_of_cluster_models_final[[current_cluster]] <- NULL
    list_of_cluster_aucs_final[[current_cluster]] <- NA
    next
  }
  
  predictors_to_remove_due_to_levels_final <- c()
  for (factor_var in factor_vars_in_formula_final) {
    if (factor_var %in% names(train_subset_after_na_omit_final)) {
      actual_levels_count <- length(levels(droplevels(train_subset_after_na_omit_final[[factor_var]])))
      if (actual_levels_count < 2) {
        message(paste("Factor '", factor_var, "' tiene < 2 niveles para", current_cluster, ". Se removerá."))
        predictors_to_remove_due_to_levels_final <- c(predictors_to_remove_due_to_levels_final, factor_var)
      }
    } else {
      message(paste("Factor '", factor_var, "' eliminado por na.omit para", current_cluster, ". Se removerá."))
      predictors_to_remove_due_to_levels_final <- c(predictors_to_remove_due_to_levels_final, factor_var)
    }
  }
  current_predictors_dynamic_final <- setdiff(current_predictors_dynamic_final, predictors_to_remove_due_to_levels_final)
  
  if(length(current_predictors_dynamic_final) == 0){
    message(paste("No quedaron predictores válidos para", current_cluster, ". Saltando."))
    list_of_cluster_models_final[[current_cluster]] <- NULL
    list_of_cluster_aucs_final[[current_cluster]] <- NA
    next
  }
  
  formula_dynamic_str_final <- paste("diffusion_numeric ~", paste(current_predictors_dynamic_final, collapse = " + "))
  final_formula_for_this_cluster_final <- as.formula(formula_dynamic_str_final)
  message(paste("Usando fórmula final para", current_cluster, ":\n", formula_dynamic_str_final))
  
  tryCatch({
    model_for_cluster_final <- glm(final_formula_for_this_cluster_final,
                                   data = train_subset,
                                   family = binomial(link = "logit"),
                                   na.action = na.omit)
    
    list_of_cluster_models_final[[current_cluster]] <- model_for_cluster_final
    message(paste("Resumen del modelo final para skill_type:", current_cluster))
    print(summary(model_for_cluster_final))
    
    if (nrow(test_subset) >= min_obs_test && length(unique(test_subset$diffusion_numeric)) == 2) {
      pred_probs_cluster_final <- tryCatch({
        predict(model_for_cluster_final, newdata = test_subset, type = "response")
      }, error = function(e_pred){ message(paste("Error predict() final:", e_pred$message)); return(NULL) })
      
      if (!is.null(pred_probs_cluster_final)) {
        valid_indices_for_roc_final <- !is.na(pred_probs_cluster_final) & !is.na(test_subset$diffusion_numeric)
        if(sum(valid_indices_for_roc_final) > min_obs_test/2 && length(unique(test_subset$diffusion_numeric[valid_indices_for_roc_final])) == 2) {
          roc_cluster_final <- roc(response = test_subset$diffusion_numeric[valid_indices_for_roc_final],
                                   predictor = pred_probs_cluster_final[valid_indices_for_roc_final],
                                   levels = c(0, 1), direction = "<") 
          auc_value_final <- auc(roc_cluster_final)
          message(paste("AUC (final) para skill_type", current_cluster, ":", round(auc_value_final, 4)))
          list_of_cluster_aucs_final[[current_cluster]] <- auc_value_final
        } else { list_of_cluster_aucs_final[[current_cluster]] <- NA }
      } else { list_of_cluster_aucs_final[[current_cluster]] <- NA }
    } else { list_of_cluster_aucs_final[[current_cluster]] <- NA }
  }, error = function(e_glm) {
    message(paste("Error fatal glm final para", current_cluster, ":", e_glm$message))
    list_of_cluster_models_final[[current_cluster]] <- NULL
    list_of_cluster_aucs_final[[current_cluster]] <- NA
  })
}

message("\n--- Resumen de AUCs por skill_type (Modelos Finales) ---")
aucs_df_final <- data.frame(skill_type = names(list_of_cluster_aucs_final), AUC_final = unlist(list_of_cluster_aucs_final))
if(nrow(aucs_df_final) > 0) {
  print(aucs_df_final[order(aucs_df_final$AUC_final, decreasing = TRUE, na.last = TRUE), ])
} else { message("No se calcularon AUCs (final) para ningún cluster.")}


# --- 3. VISUALIZACIÓN MEJORADA DE EFECTOS POR CLUSTER ---
message("\n--- PASO 3: Visualización Mejorada de Efectos por Cluster ---")

# Tema base para ggplot con fuentes más grandes y mejor estética
theme_custom_large <- function(base_size = 16, base_family = "sans") { # Puedes cambiar "sans" a "serif" u otra
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(size = rel(1.4), face = "bold", hjust = 0.5, margin = margin(b = 15)),
      plot.subtitle = element_text(size = rel(1.1), hjust = 0.5, margin = margin(b = 15)),
      axis.title = element_text(size = rel(1.2), face = "bold"),
      axis.text = element_text(size = rel(1.0)),
      legend.title = element_text(size = rel(1.1), face = "bold"),
      legend.text = element_text(size = rel(1.0)),
      strip.text = element_text(size = rel(1.1), face = "bold"),
      plot.caption = element_text(size = rel(0.8), hjust = 1, color = "grey40"),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_line(colour = "grey92"),
      plot.background = element_rect(fill = "white", colour = NA) # Fondo blanco para el plot
    )
}

# Función para generar datos de predicción y plotear (MODIFICADA Y MEJORADA)
plot_model_effect_final <- function(model, 
                                    term_original_name, # Ej: "education_diff" (NO estandarizado)
                                    data_for_ranges_and_medians, # Ej: train_subset
                                    x_label, 
                                    plot_title_base, 
                                    color = "dodgerblue4",
                                    num_points = 100) {
  
  term_std_name <- paste0(term_original_name, "_std") # Modelo usa la versión estandarizada
  
  if (!term_std_name %in% names(coef(model))) {
    message(paste("El término estandarizado", term_std_name, "no está en el modelo. No se puede plotear para", plot_title_base))
    return(NULL)
  }
  
  # Rango de la variable original para el eje X del plot
  min_val_orig <- min(data_for_ranges_and_medians[[term_original_name]], na.rm = TRUE)
  max_val_orig <- max(data_for_ranges_and_medians[[term_original_name]], na.rm = TRUE)
  
  if(!is.finite(min_val_orig) || !is.finite(max_val_orig) || min_val_orig >= max_val_orig) {
    message(paste("Rango original inválido para", term_original_name, "en", plot_title_base,". No se puede plotear."))
    return(NULL)
  }
  pred_x_original_scale <- seq(min_val_orig, max_val_orig, length.out = num_points)
  
  # Usar ggpredict: pasamos el término ESTANDARIZADO que usa el modelo.
  # ggpredict usará data_for_ranges_and_medians para las medias/modas de otras variables.
  # Es crucial que data_for_ranges_and_medians tenga TODAS las variables que el modelo necesita,
  # incluyendo las versiones _std y las originales si se usan para reescalar.
  
  # Para que ggpredict funcione con la variación de 'term_std_name' y muestre 'term_original_name'
  # en el eje x, tenemos que ser un poco creativos o reescalar después.
  # ggpredict usa los datos del modelo si no se especifica `data` en ggpredict.
  # Si especificamos `data`, debe tener la variable estandarizada.
  
  # Opción 1: Dejar que ggpredict use los datos del modelo y reescalar el eje X
  pred_effect <- NULL
  tryCatch({
    # Predice sobre el rango del término ESTANDARIZADO
    pred_effect <- ggpredict(model, terms = paste0(term_std_name, " [all]"))
  }, error = function(e) {
    message(paste("Error con ggpredict para", term_std_name, "en", plot_title_base, ":", e$message))
  })
  if (is.null(pred_effect)) return(NULL)
  
  # Re-escalar el eje 'x' de pred_effect (que está en escala estandarizada) a la escala original
  mean_orig <- mean(data_for_ranges_and_medians[[term_original_name]], na.rm = TRUE)
  sd_orig <- sd(data_for_ranges_and_medians[[term_original_name]], na.rm = TRUE)
  
  if (is.na(sd_orig) || sd_orig == 0 || !is.finite(mean_orig)) {
    message(paste("SD o Media de", term_original_name, "es 0/NA/No finita. Plot se mostrará en escala estandarizada para el eje X."))
    pred_effect$x_plot_scale <- pred_effect$x # Usar la escala estandarizada
    final_x_label <- paste(x_label, "(Estandarizado)")
  } else {
    pred_effect$x_plot_scale <- (pred_effect$x * sd_orig) + mean_orig # Re-escalar
    final_x_label <- x_label
  }
  
  # Calcular el vértice en la escala original para anotación (si aplica)
  vertex_original_scale <- NA
  term_sq_std_name <- paste0(term_original_name, "_sq_std") # ej: education_diff_sq_std
  
  if (term_std_name %in% names(coef(model)) && term_sq_std_name %in% names(coef(model))) {
    b_std <- coef(model)[term_std_name]
    a_std <- coef(model)[term_sq_std_name]
    if (!is.na(a_std) && a_std != 0 && !is.na(b_std)) {
      vertex_std <- -b_std / (2 * a_std)
      # Re-escalar vértice a escala original
      if (!is.na(sd_orig) && sd_orig != 0 && is.finite(mean_orig)){
        vertex_original_scale <- (vertex_std * sd_orig) + mean_orig
      }
    }
  }
  
  plot_subtitle <- ifelse(!is.na(vertex_original_scale),
                          paste("Vértice aprox. en x =", format(vertex_original_scale, digits = 2, nsmall=2)),
                          "")
  
  p <- ggplot(pred_effect, aes(x = x_plot_scale, y = predicted)) +
    geom_line(color = color, linewidth = 1.5) + # Línea más gruesa
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, fill = color) + # Sombreado más sutil
    labs(title = plot_title_base, subtitle = plot_subtitle, x = final_x_label, y = "Probabilidad Predicha de Difusión") +
    coord_cartesian(ylim = c(0, NA)) + # Asegura que el eje Y empieza en 0
    theme_custom_large() # Aplicar tema con fuentes grandes
  
  # Línea vertical en x=0 y anotaciones solo para variables de diferencia con signo
  if (term_original_name %in% c("education_diff", "wage_diff")) {
    p <- p + geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8)
    
    # Ajustar posición de anotaciones
    y_pos_annotation <- max(pred_effect$predicted, na.rm = TRUE) * 0.9
    if(is.na(y_pos_annotation) || !is.finite(y_pos_annotation)) y_pos_annotation <- 0.8 # Fallback
    x_range_plot <- range(pred_effect$x_plot_scale, na.rm=TRUE)
    
    p <- p + annotate("text", x = x_range_plot[1] + 0.1 * diff(x_range_plot), y = y_pos_annotation, 
                      label = "Source > Target", hjust = 0, size = 5, color = "grey30", fontface="italic") +
      annotate("text", x = x_range_plot[2] - 0.1 * diff(x_range_plot), y = y_pos_annotation, 
               label = "Target > Source", hjust = 1, size = 5, color = "grey30", fontface="italic")
  }
  
  # Añadir línea de vértice si se calculó
  if(!is.na(vertex_original_scale)){
    p <- p + geom_vline(xintercept = vertex_original_scale, linetype = "dotted", color = "red", linewidth = 1)
  }
  
  return(p)
}

# Generar plots para cada cluster
for (current_cluster_name_plot in names(list_of_cluster_models_final)) {
  current_model_for_plot <- list_of_cluster_models_final[[current_cluster_name_plot]]
  current_train_subset_for_plot <- train_data[skill_type == current_cluster_name_plot, ]
  
  if (!is.null(current_model_for_plot) && inherits(current_model_for_plot, "glm")) {
    message(paste("\n--- Generando gráficos para el modelo final de:", current_cluster_name_plot, "---"))
    
    # Plot para Structural Distance
    if ("structural_distance_std" %in% names(coef(current_model_for_plot))) {
      plot_s <- plot_model_effect_final(current_model_for_plot, "structural_distance", current_train_subset_for_plot,
                                        "Structural Distance (Original Scale)", 
                                        paste(current_cluster_name_plot, ": Difusión vs. Distancia Estructural"),
                                        color = "navyblue")
      if(!is.null(plot_s)) print(plot_s)
    }
    
    # Plot para Education Difference
    if ("education_diff_std" %in% names(coef(current_model_for_plot)) && "education_diff" %in% names(current_train_subset_for_plot)) {
      plot_e <- plot_model_effect_final(current_model_for_plot, "education_diff", current_train_subset_for_plot,
                                        "Education Difference (Target - Source, Original Scale)", 
                                        paste(current_cluster_name_plot, ": Difusión vs. Diferencia Educativa"),
                                        color = "forestgreen")
      if(!is.null(plot_e)) print(plot_e)
    }
    
    # Plot para Wage Difference
    if ("wage_diff_std" %in% names(coef(current_model_for_plot)) && "wage_diff" %in% names(current_train_subset_for_plot)) {
      plot_w <- plot_model_effect_final(current_model_for_plot, "wage_diff", current_train_subset_for_plot,
                                        "Wage Difference (Target - Source, Original Scale)", 
                                        paste(current_cluster_name_plot, ": Difusión vs. Diferencia Salarial"),
                                        color = "sienna")
      if(!is.null(plot_w)) print(plot_w)
    }
  }
}

# --- 4. PLOTEO DE EFECTOS PARA EDUCATION_DIFF Y WAGE_DIFF ---


















