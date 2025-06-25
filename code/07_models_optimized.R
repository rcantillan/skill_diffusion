
# --- 0. CARGAR PAQUETES NECESARIOS ---
# Asegúrate de tenerlos instalados: install.packages(c("data.table", "dplyr", "ggplot2", "ggeffects", "scales", "car", "ggrepel", "tidytext"))
library(data.table)
library(dplyr)
library(ggplot2)
library(ggeffects) # Para calcular efectos marginales/predicciones fácilmente
library(scales)    # Para formatear ejes en plots (ej. percent_format)
library(car)       # Para VIF (si lo necesitas más adelante)
library(ggrepel)   # Para etiquetas de texto que no se solapen (opcional)
library(tidytext)  # Para reorder_within en plots de skills (opcional)
library(pROC)      # Para AUC, si decides añadir evaluación de modelos por cluster

# --- ASUMPCIÓN INICIAL ---
# Asumimos que ya tienes cargado tu data.table principal y se llama 'diffusion_events'
# Y que contiene las columnas base como 'diffusion', 'target_wage', 'source_wage',
# 'target_education', 'source_education', 'skill_type', 'skill_status', 'structural_distance', etc.

if (!exists("diffusion_events") || !is.data.table(diffusion_events) || nrow(diffusion_events) == 0) {
  stop("La tabla 'diffusion_events' no existe, está vacía o no es un data.table. Por favor, cárgala y prepárala primero.")
}
setDT(diffusion_events)

## ============================================================================
## 1. DATA PREPARATION AND VARIABLE CREATION (Adaptado para 'diffusion_events')
## ============================================================================
message("--- PASO 1: Preparación de Datos y Creación de Variables ---")

# ------------- 1.1 NA Handling en Salarios -------------
if ("target_wage" %in% names(diffusion_events) && "source_wage" %in% names(diffusion_events)) {
  median_target_wage <- median(diffusion_events$target_wage, na.rm = TRUE)
  if(is.na(median_target_wage)) median_target_wage <- 0 # Fallback si todos son NA
  diffusion_events[, target_wage_original_was_na := is.na(target_wage)]
  diffusion_events[is.na(target_wage), target_wage := median_target_wage]
  diffusion_events[, target_wage_imputed_indicator := factor(target_wage_original_was_na,
                                                             levels = c(FALSE, TRUE),
                                                             labels = c("Observed", "Imputed"))]
  
  median_source_wage <- median(diffusion_events$source_wage, na.rm = TRUE)
  if(is.na(median_source_wage)) median_source_wage <- 0 # Fallback
  diffusion_events[, source_wage_original_was_na := is.na(source_wage)]
  diffusion_events[is.na(source_wage), source_wage := median_source_wage]
  diffusion_events[, source_wage_imputed_indicator := factor(source_wage_original_was_na,
                                                             levels = c(FALSE, TRUE),
                                                             labels = c("Observed", "Imputed"))]
  message("Manejo de NAs en salarios completado.")
} else {
  warning("Columnas 'target_wage' o 'source_wage' no encontradas. No se realizó imputación de salarios.")
  if (!"target_wage_imputed_indicator" %in% names(diffusion_events)) diffusion_events[, target_wage_imputed_indicator := factor("NotApplicable", levels=c("Observed", "Imputed", "NotApplicable"))]
  if (!"source_wage_imputed_indicator" %in% names(diffusion_events)) diffusion_events[, source_wage_imputed_indicator := factor("NotApplicable", levels=c("Observed", "Imputed", "NotApplicable"))]
}

# ------------- 1.2 Calcular Variables Derivadas (Diferencias Directas y Cuadrados) -------------
# Diferencias directas (Target - Source)
if ("target_education" %in% names(diffusion_events) && "source_education" %in% names(diffusion_events)) {
  diffusion_events[, education_diff := target_education - source_education]
  diffusion_events[, education_diff_sq := education_diff^2]
} else { message("Advertencia: No se pudieron crear 'education_diff' y 'education_diff_sq'.") }

if ("target_wage" %in% names(diffusion_events) && "source_wage" %in% names(diffusion_events)) {
  diffusion_events[, wage_diff := target_wage - source_wage]
  diffusion_events[, wage_diff_sq := wage_diff^2]
} else { message("Advertencia: No se pudieron crear 'wage_diff' y 'wage_diff_sq'.") }

# Cuadrado de structural_distance
if ("structural_distance" %in% names(diffusion_events)) {
  if (!"structural_distance_sq" %in% names(diffusion_events)) {
    diffusion_events[, structural_distance_sq := structural_distance^2]
  }
} else { message("Advertencia: 'structural_distance' no encontrada.")}

# Variable dependiente y factores clave
diffusion_events[, diffusion_numeric := as.integer(diffusion)]
if ("skill_type" %in% names(diffusion_events)) {
  diffusion_events[, skill_type := as.factor(skill_type)]
} else { stop("La columna 'skill_type' es necesaria y no se encuentra.")}
if ("skill_status" %in% names(diffusion_events)) {
  diffusion_events[, skill_status := as.factor(skill_status)]
} else { 
  diffusion_events[, skill_status := factor("Unknown")] # Fallback si no existe
  warning("La columna 'skill_status' no se encontró. Se creó una con valor 'Unknown'.")
}
message("Variables de diferencia y factores preparados.")

# ------------- 1.3 Estandarización de Variables Predictoras Continuas -------------
message("Estandarizando predictores continuos...")
# Lista de variables base originales y sus cuadrados a estandarizar
# Asegúrate de que todas estas variables base existan ANTES de intentar acceder a sus '_sq'
cols_to_standardize <- c()
base_vars_for_std <- c("structural_distance", "education_diff", "wage_diff", 
                       "source_wage", "source_education", "data_value_source",
                       "Prob_Transicion_Weighted", "source_job_zone") # Añade otras que uses

for(base_var in base_vars_for_std){
  if(base_var %in% names(diffusion_events)) cols_to_standardize <- c(cols_to_standardize, base_var)
  sq_var <- paste0(base_var, "_sq")
  if(sq_var %in% names(diffusion_events)) cols_to_standardize <- c(cols_to_standardize, sq_var)
}
cols_to_standardize <- unique(cols_to_standardize) # Quitar duplicados si los hubiera

for (col in cols_to_standardize) {
  if (is.numeric(diffusion_events[[col]])) {
    mean_val <- mean(diffusion_events[[col]], na.rm = TRUE)
    sd_val <- sd(diffusion_events[[col]], na.rm = TRUE)
    if (!is.na(sd_val) && sd_val > 0 && is.finite(mean_val)) {
      diffusion_events[, (paste0(col, "_std")) := (get(col) - mean_val) / sd_val]
    } else {
      diffusion_events[, (paste0(col, "_std")) := 0]
      message(paste("Advertencia: Columna", col, "tenía sd=0, NA o media no finita. Estandarizada a 0:", paste0(col, "_std")))
    }
  }
}
message("Estandarización completada.")

# ------------- 1.4 División de Datos (Opcional aquí si ya se hizo, pero bueno para encapsular) -------------
# Si ya tienes train_data y test_data definidos y preparados con estas nuevas variables, puedes omitir este paso.
# Si no, este es un buen lugar para hacerlo.
if (!exists("train_data") || !exists("test_data")) {
  message("Dividiendo datos en entrenamiento y prueba...")
  set.seed(123)
  train_indices <- sample(1:nrow(diffusion_events), floor(0.7 * nrow(diffusion_events)))
  train_data <- diffusion_events[train_indices, ]
  test_data <- diffusion_events[-train_indices, ]
  message(paste("Datos divididos: Entrenamiento =", nrow(train_data), "filas, Prueba =", nrow(test_data), "filas."))
} else {
  message("Usando train_data y test_data preexistentes.")
}


## ============================================================================
## 2. MODELADO POR CLUSTER (skill_type)
## ============================================================================
message("\n--- PASO 2: Modelado por Cluster (skill_type) ---")

skill_type_levels <- levels(train_data$skill_type)
if (is.null(skill_type_levels)) stop ("skill_type no es un factor o no tiene niveles en train_data.")
message(paste("Niveles de skill_type a modelar:", paste(skill_type_levels, collapse=", ")))

list_of_cluster_models_final <- list()
list_of_cluster_aucs_final <- list()

# Predictores ESTANDARIZADOS para la fórmula base de los modelos por cluster
# Esta es la fórmula que se acerca más a la de tu presentación (Modelo 3) + otros predictores robustos
# y asume que todas estas variables _std existen después del Paso 1.3
predictors_std_cluster_formula <- c(
  "structural_distance_std", "structural_distance_sq_std",
  "education_diff_std", "education_diff_sq_std",
  "wage_diff_std", "wage_diff_sq_std",
  "Prob_Transicion_Weighted_std", # Asegúrate que este existe si lo quieres estandarizado
  "data_value_source_std",
  "source_job_zone_std",          # Asegúrate que este existe si lo quieres estandarizado
  "source_education_std",
  "source_wage_std",
  "skill_status",                 # Factor
  "target_wage_imputed_indicator",# Factor
  "source_wage_imputed_indicator" # Factor
)
# Verificar que los predictores seleccionados realmente existen en train_data
actual_predictors_for_formula <- intersect(predictors_std_cluster_formula, names(train_data))
message(paste("Predictores finales (estandarizados) para la fórmula por cluster:", paste(actual_predictors_for_formula, collapse=", ")))


for (current_cluster in skill_type_levels) {
  message(paste("\n--- Ajustando modelo final para skill_type:", current_cluster, "---"))
  train_subset <- train_data[skill_type == current_cluster, ]
  test_subset <- test_data[skill_type == current_cluster, ]
  
  message(paste("Filas en train_subset para", current_cluster, ":", nrow(train_subset)))
  
  min_obs_train <- max(200, length(actual_predictors_for_formula) * 20) # Umbral más robusto
  min_obs_test <- 50   
  
  if (nrow(train_subset) < min_obs_train || length(unique(train_subset$diffusion_numeric)) < 2) {
    message(paste("Datos de entrenamiento insuficientes para", current_cluster, ". Saltando."))
    list_of_cluster_models_final[[current_cluster]] <- NULL
    list_of_cluster_aucs_final[[current_cluster]] <- NA
    next
  }
  
  # Ajuste dinámico de fórmula para factores con < 2 niveles
  current_predictors_dynamic <- c(actual_predictors_for_formula)
  factor_vars_in_formula <- current_predictors_dynamic[sapply(current_predictors_dynamic, function(p) is.factor(train_subset[[p]]))]
  
  # Verificar niveles DESPUÉS de un na.omit simulado sobre las variables del modelo actual
  potential_model_vars <- c("diffusion_numeric", current_predictors_dynamic)
  train_subset_complete_cases <- na.omit(train_subset[, ..potential_model_vars, drop = FALSE])
  
  if(nrow(train_subset_complete_cases) < min_obs_train || length(unique(train_subset_complete_cases$diffusion_numeric)) < 2) {
    message(paste("Después de na.omit, datos insuficientes para", current_cluster, ". Saltando."))
    list_of_cluster_models_final[[current_cluster]] <- NULL
    list_of_cluster_aucs_final[[current_cluster]] <- NA
    next
  }
  
  predictors_to_remove_from_current_formula <- c()
  for (factor_var in factor_vars_in_formula) {
    if (factor_var %in% names(train_subset_complete_cases)) { # Asegurar que la columna existe
      if (length(levels(droplevels(train_subset_complete_cases[[factor_var]]))) < 2) {
        message(paste("Factor '", factor_var, "' tiene < 2 niveles para", current_cluster, ". Se removerá."))
        predictors_to_remove_from_current_formula <- c(predictors_to_remove_from_current_formula, factor_var)
      }
    } else { # Si el factor no está en train_subset_complete_cases, también hay que quitarlo
      message(paste("Factor '", factor_var, "' no presente en datos completos para", current_cluster, ". Se removerá."))
      predictors_to_remove_from_current_formula <- c(predictors_to_remove_from_current_formula, factor_var)
    }
  }
  current_predictors_for_glm <- setdiff(current_predictors_dynamic, predictors_to_remove_from_current_formula)
  
  if(length(current_predictors_for_glm) == 0){
    message(paste("No quedaron predictores válidos para", current_cluster, ". Saltando."))
    list_of_cluster_models_final[[current_cluster]] <- NULL
    list_of_cluster_aucs_final[[current_cluster]] <- NA
    next
  }
  
  formula_this_cluster_str <- paste("diffusion_numeric ~", paste(current_predictors_for_glm, collapse = " + "))
  formula_this_cluster <- as.formula(formula_this_cluster_str)
  message(paste("Usando fórmula final para", current_cluster, ":\n", formula_this_cluster_str))
  
  tryCatch({
    model_cluster <- glm(formula_this_cluster,
                         data = train_subset, # glm aplicará na.omit sobre las variables de ESTA fórmula
                         family = binomial(link = "logit"),
                         na.action = na.omit) # na.omit aquí es importante
    
    list_of_cluster_models_final[[current_cluster]] <- model_cluster
    message(paste("Resumen del modelo final para skill_type:", current_cluster))
    print(summary(model_cluster))
    
    # Evaluación
    if (nrow(test_subset) >= min_obs_test && length(unique(test_subset$diffusion_numeric)) == 2) {
      pred_probs <- tryCatch({
        predict(model_cluster, newdata = test_subset, type = "response")
      }, error = function(e_pred){ message(paste("Error predict() para",current_cluster,":", e_pred$message)); return(NULL) })
      
      if (!is.null(pred_probs)) {
        valid_idx <- !is.na(pred_probs) & !is.na(test_subset$diffusion_numeric)
        if(sum(valid_idx) > min_obs_test/2 && length(unique(test_subset$diffusion_numeric[valid_idx])) == 2) {
          roc_obj <- roc(response = test_subset$diffusion_numeric[valid_idx],
                         predictor = pred_probs[valid_idx],
                         levels = c(0, 1), direction = "<") 
          auc_val <- auc(roc_obj)
          message(paste("AUC (final) para skill_type", current_cluster, ":", round(auc_val, 4)))
          list_of_cluster_aucs_final[[current_cluster]] <- auc_val
        } else { list_of_cluster_aucs_final[[current_cluster]] <- NA }
      } else { list_of_cluster_aucs_final[[current_cluster]] <- NA }
    } else { list_of_cluster_aucs_final[[current_cluster]] <- NA }
  }, error = function(e_glm) {
    message(paste("Error fatal glm final para", current_cluster, ":", e_glm$message))
    list_of_cluster_models_final[[current_cluster]] <- NULL
    list_of_cluster_aucs_final[[current_cluster]] <- NA
  })
}

# Resumen de AUCs
message("\n--- Resumen de AUCs por skill_type (Modelos Finales con preparación estilo presentación) ---")
aucs_df_final <- data.frame(skill_type = names(list_of_cluster_aucs_final), AUC_final = unlist(list_of_cluster_aucs_final))
if(nrow(aucs_df_final) > 0) {
  print(aucs_df_final[order(aucs_df_final$AUC_final, decreasing = TRUE, na.last = TRUE), ])
} else { message("No se calcularon AUCs (final) para ningún cluster.")}

## ============================================================================
## 3. VISUALIZATION FUNCTIONS AND PLOTS (Mejoradas)
## ============================================================================
message("\n--- PASO 3: Visualización Mejorada de Efectos por Cluster ---")

# Tema base para ggplot con fuentes más grandes y mejor estética
theme_custom_final <- function(base_size = 17, base_family = "sans") { # Aumentado base_size
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(size = rel(1.3), face = "bold", hjust = 0.5, margin = margin(b = 15)), # Ligeramente más pequeño que antes, pero relativo a base_size
      plot.subtitle = element_text(size = rel(1.1), hjust = 0.5, margin = margin(b = 15)),
      axis.title = element_text(size = rel(1.15), face = "bold"), # Un poco más grande
      axis.text = element_text(size = rel(1.0)),
      legend.title = element_text(size = rel(1.05), face = "bold"),
      legend.text = element_text(size = rel(0.95)),
      strip.text = element_text(size = rel(1.1), face = "bold"),
      plot.caption = element_text(size = rel(0.8), hjust = 1, color = "grey40"),
      panel.grid.major = element_line(colour = "grey88"), # Grilla un poco más clara
      panel.grid.minor = element_line(colour = "grey94"),
      plot.background = element_rect(fill = "white", colour = NA) 
    )
}

# Función para generar datos de predicción y plotear
plot_model_effect_final <- function(model, 
                                    term_original_name, # Ej: "education_diff" (NO estandarizado)
                                    data_for_ranges_and_medians, # Ej: train_subset correspondiente al modelo
                                    x_label, 
                                    plot_title_base, 
                                    color = "dodgerblue4",
                                    num_points = 100) {
  
  term_std_name <- paste0(term_original_name, "_std") # Modelo usa la versión estandarizada
  
  if (!term_std_name %in% names(coef(model))) {
    # Intentar con el nombre original si el estandarizado no está (por si acaso la estandarización falló para esa var)
    if (term_original_name %in% names(coef(model))) {
      term_std_name <- term_original_name # Usar el original si el _std no es un coef
      message(paste("Usando término no estandarizado", term_original_name, "para plot de", plot_title_base))
    } else {
      message(paste("Término", term_std_name, "ni", term_original_name, "están en el modelo. No se puede plotear para", plot_title_base))
      return(NULL)
    }
  }
  
  min_val_orig <- min(data_for_ranges_and_medians[[term_original_name]], na.rm = TRUE)
  max_val_orig <- max(data_for_ranges_and_medians[[term_original_name]], na.rm = TRUE)
  
  if(!is.finite(min_val_orig) || !is.finite(max_val_orig) || (max_val_orig - min_val_orig) < 1e-6 ) { # Añadida comprobación de rango > 0
    message(paste("Rango original inválido o muy pequeño para", term_original_name, "en", plot_title_base,". No se puede plotear."))
    return(NULL)
  }
  
  pred_effect <- NULL
  tryCatch({
    # ggpredict usará los datos del modelo para fijar otros predictores en sus medias/modas
    # Es importante que el modelo se haya ajustado con na.action=na.omit o con datos completos
    pred_effect <- ggpredict(model, terms = paste0(term_std_name, " [all]")) 
  }, error = function(e) {
    message(paste("Error con ggpredict para", term_std_name, "en", plot_title_base, ":", e$message))
  })
  if (is.null(pred_effect) || nrow(pred_effect) == 0) {
    message(paste("ggpredict no devolvió datos para", term_std_name, "en", plot_title_base))
    return(NULL)
  }
  
  # Re-escalar el eje 'x' de pred_effect (que está en escala estandarizada) a la escala original
  mean_orig <- mean(data_for_ranges_and_medians[[term_original_name]], na.rm = TRUE)
  sd_orig <- sd(data_for_ranges_and_medians[[term_original_name]], na.rm = TRUE)
  
  final_x_label <- x_label
  if (term_std_name == term_original_name || is.na(sd_orig) || sd_orig == 0 || !is.finite(mean_orig)) { # Si se usó el término original o no se puede re-escalar
    pred_effect$x_plot_scale <- pred_effect$x 
    if(term_std_name == term_original_name) message(paste("Plot para", term_original_name, "se muestra en su escala original (no se encontró/usó _std)."))
    else final_x_label <- paste(x_label, "(Estandarizado)")
  } else {
    pred_effect$x_plot_scale <- (pred_effect$x * sd_orig) + mean_orig # Re-escalar
  }
  
  vertex_original_scale <- NA
  term_sq_std_name <- gsub("_std$", "_sq_std", term_std_name) # ej: education_diff_std -> education_diff_sq_std
  # o structural_distance_std -> structural_distance_sq_std
  
  if (term_std_name %in% names(coef(model)) && term_sq_std_name %in% names(coef(model))) {
    b_std <- coef(model)[term_std_name]
    a_std <- coef(model)[term_sq_std_name]
    if (!is.na(a_std) && a_std != 0 && !is.na(b_std)) {
      vertex_std <- -b_std / (2 * a_std)
      if (term_std_name != term_original_name && !is.na(sd_orig) && sd_orig != 0 && is.finite(mean_orig)){ # Si se puede re-escalar
        vertex_original_scale <- (vertex_std * sd_orig) + mean_orig
      } else { # Mostrar vértice en escala estandarizada si no se pudo re-escalar
        vertex_original_scale <- vertex_std
      }
    }
  }
  
  plot_subtitle <- ifelse(!is.na(vertex_original_scale),
                          paste("Vértice aprox. en x =", format(vertex_original_scale, digits = 3, nsmall=2), 
                                ifelse(term_std_name != term_original_name && (!is.na(sd_orig) && sd_orig != 0 && is.finite(mean_orig)), "(escala original)", "(escala estd.)")),
                          "")
  
  p <- ggplot(pred_effect, aes(x = x_plot_scale, y = predicted)) +
    geom_line(color = color, linewidth = 1.3) + 
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = color) + 
    labs(title = plot_title_base, subtitle = plot_subtitle, x = final_x_label, y = "Probabilidad Predicha de Difusión") +
    coord_cartesian(ylim = c(0, NA)) + 
    theme_custom_final() 
  
  if (term_original_name %in% c("education_diff", "wage_diff")) {
    p <- p + geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7)
    y_pos_annotation <- max(pred_effect$predicted, na.rm = TRUE) * 0.92
    if(is.na(y_pos_annotation) || !is.finite(y_pos_annotation)) y_pos_annotation <- 0.8 
    x_range_plot <- range(pred_effect$x_plot_scale, na.rm=TRUE)
    if(all(is.finite(x_range_plot))) {
      p <- p + 
        annotate("text", x = x_range_plot[1] + 0.05 * diff(x_range_plot), y = y_pos_annotation, 
                 label = "Source > Target", hjust = 0, size = 4.5, color = "grey30", fontface="italic") + # Ajustado tamaño y posición
        annotate("text", x = x_range_plot[2] - 0.05 * diff(x_range_plot), y = y_pos_annotation, 
                 label = "Target > Source", hjust = 1, size = 4.5, color = "grey30", fontface="italic")
    }
  }
  
  if(!is.na(vertex_original_scale)){
    p <- p + geom_vline(xintercept = vertex_original_scale, linetype = "dotted", color = "red3", linewidth = 1)
  }
  
  return(p)
}

# Generar plots para cada cluster con los modelos finales
for (current_cluster_name_plot in names(list_of_cluster_models_final)) {
  current_model_for_plot <- list_of_cluster_models_final[[current_cluster_name_plot]]
  # Usar train_subset para obtener rangos y medianas/modas consistentes con los datos de entrenamiento del modelo
  current_train_subset_for_plot <- train_data[skill_type == current_cluster_name_plot, ]
  
  if (!is.null(current_model_for_plot) && inherits(current_model_for_plot, "glm")) {
    message(paste("\n--- Generando gráficos para el modelo final de:", current_cluster_name_plot, "---"))
    
    # Plot para Structural Distance
    if ("structural_distance_std" %in% names(coef(current_model_for_plot))) {
      plot_s <- plot_model_effect_final(current_model_for_plot, "structural_distance", current_train_subset_for_plot,
                                        "Distancia Estructural (Escala Original)", 
                                        paste(current_cluster_name_plot, ": Difusión vs. Distancia Estructural"),
                                        color = "navyblue")
      if(!is.null(plot_s)) print(plot_s)
    }
    
    # Plot para Education Difference
    if ("education_diff_std" %in% names(coef(current_model_for_plot)) && "education_diff" %in% names(current_train_subset_for_plot)) {
      plot_e <- plot_model_effect_final(current_model_for_plot, "education_diff", current_train_subset_for_plot,
                                        "Diferencia Educativa (Target - Source, Escala Original)", 
                                        paste(current_cluster_name_plot, ": Difusión vs. Diferencia Educativa"),
                                        color = "forestgreen")
      if(!is.null(plot_e)) print(plot_e)
    }
    
    # Plot para Wage Difference
    if ("wage_diff_std" %in% names(coef(current_model_for_plot)) && "wage_diff" %in% names(current_train_subset_for_plot)) {
      plot_w <- plot_model_effect_final(current_model_for_plot, "wage_diff", current_train_subset_for_plot,
                                        "Diferencia Salarial (Target - Source, Escala Original)", 
                                        paste(current_cluster_name_plot, ": Difusión vs. Diferencia Salarial"),
                                        color = "sienna")
      if(!is.null(plot_w)) print(plot_w)
    }
  } else {
    message(paste("No hay modelo ajustado para", current_cluster_name_plot, "o no es un objeto glm. No se generan plots."))
  }
}

message("--- Análisis por cluster y generación de gráficos completados. ---")


# Asumiendo que 'diffusion_events' es tu data.table ya cargado
# y que tiene las columnas 'skill_type' y 'skill_name'
if (!exists("diffusion_events") || !is.data.table(diffusion_events) || nrow(diffusion_events) == 0) {
  stop("La tabla 'diffusion_events' no existe, está vacía o no es un data.table.")
}
if (!"skill_type" %in% names(diffusion_events) || !"skill_name" %in% names(diffusion_events)) {
  stop("Las columnas 'skill_type' y/o 'skill_name' no se encuentran en 'diffusion_events'.")
}

# Asegurarse de que skill_type es un factor (ayuda si quieres iterar por niveles)
if (!is.factor(diffusion_events$skill_type)) {
  diffusion_events[, skill_type := as.factor(skill_type)]
}

# Opción 1: Crear una lista donde cada elemento es un cluster y contiene los nombres de las habilidades
message("--- Opción 1: Lista de habilidades por cluster ---")
skills_por_cluster_lista <- list()
skill_type_levels <- levels(diffusion_events$skill_type)

for (cluster_level in skill_type_levels) {
  # Obtener los nombres de habilidades únicos para el cluster actual
  habilidades_unicas_en_cluster <- unique(diffusion_events[skill_type == cluster_level, skill_name])
  skills_por_cluster_lista[[cluster_level]] <- sort(habilidades_unicas_en_cluster) # Opcional: ordenar alfabéticamente
  
  # Imprimir algunos ejemplos
  message(paste("\nEjemplos de habilidades en", cluster_level, "(Total únicas:", length(habilidades_unicas_en_cluster), "):"))
  print(head(skills_por_cluster_lista[[cluster_level]], 20)) # Mostrar hasta 20 ejemplos
}

# Ahora 'skills_por_cluster_lista' contiene la información.
# Por ejemplo, para ver todas las habilidades de NetCluster_1:
# print(skills_por_cluster_lista[["NetCluster_1"]])


# Opción 2: Crear un data.table que mapee cada habilidad a su cluster (si una habilidad solo puede estar en un cluster)
# Si una habilidad podría, teóricamente, aparecer con diferentes skill_type en diferentes filas (aunque no es lo usual),
# esta tabla mostraría todas esas asignaciones. Pero usualmente, una skill tiene UN tipo de cluster.
message("\n--- Opción 2: Data.table con habilidades únicas y su(s) cluster(s) asignado(s) ---")
# Agrupar por skill_name y luego listar los skill_type únicos asociados (en caso de que una skill pudiera tener más de uno)
# Pero es más probable que cada skill_name tenga un único skill_type asignado globalmente.
# Por lo tanto, una forma más directa si la asignación es única:
mapeo_skill_a_cluster <- unique(diffusion_events[, .(skill_name, skill_type)])
setkey(mapeo_skill_a_cluster, skill_type, skill_name) # Ordenar para mejor visualización

message("Mapeo de habilidades a clusters (primeras filas):")
print(head(mapeo_skill_a_cluster, 20))

# Para ver todas las skills de un cluster específico usando esta tabla:
# print(mapeo_skill_a_cluster[skill_type == "NetCluster_1"])


# Opción 3: Si tienes una tabla de clasificación de skills separada
# (como la que se mencionó en tu código de presentación: 'skill_classification_table')
# Esa tabla sería la fuente canónica. El código que usaste en la Sección 7.2 de tu
# script de presentación es exactamente para esto:

# output_data_dir <- "datos_eventos_generados_v7_debug_net" # Ajusta tu path
# classification_file_path <- file.path(output_data_dir, "skill_classification_network_with_status.csv")
#
# if (file.exists(classification_file_path)) {
#   skill_classification_table <- fread(classification_file_path)
#   setDT(skill_classification_table)
#   message(paste("Tabla de clasificación de habilidades cargada desde:", classification_file_path))
#   
#   # Asumiendo que las columnas se llaman 'SkillName' y 'SkillType'
#   # (o 'Element.Name.Original' y 'SkillType_YYYY' que se renombran)
#   
#   if (!"SkillName" %in% names(skill_classification_table) && "Element.Name.Original" %in% names(skill_classification_table)){
#       setnames(skill_classification_table, "Element.Name.Original", "SkillName")
#   }
#   # Ajustar el nombre de SkillType si es específico del año, ej. SkillType_2023
#   # if (!"SkillType" %in% names(skill_classification_table) && "SkillType_2023" %in% names(skill_classification_table)){
#   #     setnames(skill_classification_table, "SkillType_2023", "SkillType")
#   # }
#
#   if ("SkillType" %in% names(skill_classification_table) && "SkillName" %in% names(skill_classification_table)) {
#     message("\n--- Habilidades por Cluster (desde tabla de clasificación externa) ---")
#     
#     for (cluster_level in levels(as.factor(skill_classification_table$SkillType))) {
#       habilidades_en_cluster_externo <- skill_classification_table[SkillType == cluster_level, SkillName]
#       message(paste("\nEjemplos de habilidades en", cluster_level, "(Total:", length(habilidades_en_cluster_externo), "):"))
#       print(head(sort(habilidades_en_cluster_externo), 50))
#     }
#   } else {
#     message("La tabla de clasificación de skills no tiene las columnas 'SkillType' o 'SkillName' esperadas.")
#   }
# } else {
#   message(paste("Archivo de clasificación de skills no encontrado en:", classification_file_path))
#   message("Usando datos de 'diffusion_events' para listar skills por cluster (Opción 1 y 2).")
# }



