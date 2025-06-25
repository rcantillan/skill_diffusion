# =======================================================================
# PARTE 2: MODELOS DE REGRESIÓN LOGÍSTICA CON CLASIFICACIÓN LEIDEN2015 (MODIFICADA A 2 CLUSTERS) Y CONTROLES CPS
# Versión: 2.5.4_mod_plot (Ajustes en el gráfico combinado)
# =======================================================================

# --- 0. CONFIGURACIÓN Y CARGA DE LIBRERÍAS ---
message("--- Loading necessary libraries for PART 2 (Models with Modified Leiden2015 and CPS controls) ---")
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(broom)
  library(purrr)
  library(dplyr)
  library(RColorBrewer)
  library(ggpubr) # Para annotate_figure
  library(tidyr)
  library(stringr)
})

# --- CONFIGURACIÓN DE RUTAS ---
message("### IMPORTANT INSTRUCTIONS FOR INPUT DATA PATH ###")
message("# This script expects an .rds file containing BOTH the 'LeidenCluster2015' classification")
message("# AND the CPS flow variables (from Part 2). This loaded data will be referred to as 'all_events_final_with_cps_data'.")
message("# Ensure 'path_to_final_prepared_data_dir' points to the directory containing this file.")
message("###################################################################")

# EXAMPLE OF HOW TO DEFINE THE PATH MANUALLY (UNCOMMENT AND EDIT THE FOLLOWING TWO LINES):
final_prepared_data_dir_defined_manually <- TRUE 
path_to_final_prepared_data_dir <- "/home/rober/Documentos/skill_diffusion/datos_eventos_v12_MultiClasif_Cleaned_Corrected" 

if (!exists("final_prepared_data_dir_defined_manually") || !isTRUE(final_prepared_data_dir_defined_manually)) {
  message("\nThe variable 'final_prepared_data_dir_defined_manually' is not TRUE or does not exist.")
  message("You will be prompted to enter the path manually.")
  path_to_final_prepared_data_dir <- readline(prompt="Enter the FULL path to the directory with the final RDS file (with LeidenCluster2015 skill_type and CPS data): ")
  
  if (length(path_to_final_prepared_data_dir) == 0 || !is.character(path_to_final_prepared_data_dir) || nchar(path_to_final_prepared_data_dir) < 2 || grepl("if\\s*\\(", path_to_final_prepared_data_dir) || grepl("stop\\(", path_to_final_prepared_data_dir)) {
    stop("The entered path ('", path_to_final_prepared_data_dir ,"') seems invalid or empty. Please define 'path_to_final_prepared_data_dir' and 'final_prepared_data_dir_defined_manually <- TRUE' at the beginning of the script.")
  }
} else {
  if (!exists("path_to_final_prepared_data_dir") || !is.character(path_to_final_prepared_data_dir) || length(path_to_final_prepared_data_dir) != 1 || nchar(path_to_final_prepared_data_dir) == 0) {
    stop("'final_prepared_data_dir_defined_manually' is TRUE, but 'path_to_final_prepared_data_dir' is not defined as a valid character string.")
  }
  message("Using predefined path_to_final_prepared_data_dir: ", path_to_final_prepared_data_dir)
}

if (!dir.exists(path_to_final_prepared_data_dir)) {
  stop(paste("The specified input directory:", path_to_final_prepared_data_dir, "does not exist. Please verify the path."))
}
message(paste("Input directory for models:", normalizePath(path_to_final_prepared_data_dir)))

leiden_base_col_name <- "LeidenCluster_2015" 
classification_column_to_use <- leiden_base_col_name 

output_dir_modelos <- file.path(path_to_final_prepared_data_dir, paste0("Resultados_Modelos_", leiden_base_col_name, "_2Clusters_CPS_v2.5.4_mod"))
if (!dir.exists(output_dir_modelos)) {
  dir.create(output_dir_modelos, recursive = TRUE)
  message(paste("Output directory for Models created at:", normalizePath(output_dir_modelos)))
} else {
  message(paste("Output directory for Models already exists at:", normalizePath(output_dir_modelos)))
}


# --- Load Data ---
events_filename <- paste0("all_events_FOR_MODELS_", leiden_base_col_name, ".rds") 
path_to_events_file <- file.path(path_to_final_prepared_data_dir, events_filename)

if (!file.exists(path_to_events_file)) {
  stop(paste("The events file:", path_to_events_file, "was not found. Ensure it exists at the specified path and contains both the '", leiden_base_col_name, "' classification and the CPS variables."))
}
message(paste("\nLoading data from:", path_to_events_file))
all_events_final_with_cps_data <- readRDS(path_to_events_file)
setDT(all_events_final_with_cps_data)

message(sprintf("Data loaded into 'all_events_final_with_cps_data': %d rows and %d columns.", nrow(all_events_final_with_cps_data), ncol(all_events_final_with_cps_data)))

if (leiden_base_col_name %in% names(all_events_final_with_cps_data)) {
  message(paste("Verifying '", leiden_base_col_name, "' column BEFORE MODIFICATION:"))
  print(all_events_final_with_cps_data[, .N, by = .(cluster_col = get(leiden_base_col_name))][order(cluster_col)])
} else {
  stop(paste("The '", leiden_base_col_name, "' column (expected for cluster classification) was not found in the loaded 'all_events_final_with_cps_data'."))
}

# <<< MODIFICACIÓN INICIO: REASIGNAR CLUSTER Y MANTENER SOLO DOS GRUPOS >>>
message("\n--- Applying custom Leiden cluster modifications (Base: ", leiden_base_col_name, ") to 'all_events_final_with_cps_data' ---")
message("Target: Reassign cluster 'NoCluster_2015' to 'LeidenC_1', then keep only clusters 'LeidenC_1' and 'LeidenC_2'.")

if (leiden_base_col_name %in% names(all_events_final_with_cps_data) && nrow(all_events_final_with_cps_data) > 0) {
  all_events_final_with_cps_data[, (leiden_base_col_name) := as.character(get(leiden_base_col_name))]
  
  cluster_to_reassign <- "NoCluster_2015"
  target_cluster_for_reassignment <- "LeidenC_1"
  final_desired_clusters <- c("LeidenC_1", "LeidenC_2")
  
  message(sprintf("Original distribution of '%s' (character type) before any modification:", leiden_base_col_name))
  print(all_events_final_with_cps_data[, .N, by = .(cluster_col = get(leiden_base_col_name))][order(cluster_col)])
  
  if (cluster_to_reassign %in% unique(all_events_final_with_cps_data[[leiden_base_col_name]])) {
    all_events_final_with_cps_data[get(leiden_base_col_name) == cluster_to_reassign, (leiden_base_col_name) := target_cluster_for_reassignment]
    message(sprintf("Reassigned all instances of cluster '%s' in column '%s' to '%s'.", cluster_to_reassign, leiden_base_col_name, target_cluster_for_reassignment))
  } else {
    message(sprintf("Cluster '%s' not found in column '%s' for reassignment. No changes made for this specific reassignment step.", cluster_to_reassign, leiden_base_col_name))
  }
  
  message(sprintf("Distribution of '%s' after potential reassignment:", leiden_base_col_name))
  print(all_events_final_with_cps_data[, .N, by = .(cluster_col = get(leiden_base_col_name))][order(cluster_col)])
  
  original_row_count_before_filter <- nrow(all_events_final_with_cps_data)
  all_events_final_with_cps_data <- all_events_final_with_cps_data[get(leiden_base_col_name) %in% final_desired_clusters]
  message(sprintf("Filtered data to keep only clusters: %s in column '%s'. Rows changed from %d to %d.",
                  paste(final_desired_clusters, collapse=", "), leiden_base_col_name, original_row_count_before_filter, nrow(all_events_final_with_cps_data)))
  
  if (nrow(all_events_final_with_cps_data) > 0) {
    message("Final distribution of '", leiden_base_col_name, "' after all modifications (reassignment and filtering):")
    print(all_events_final_with_cps_data[, .N, by = .(cluster_col = get(leiden_base_col_name))][order(cluster_col)])
    
    current_unique_clusters <- unique(all_events_final_with_cps_data[[leiden_base_col_name]])
    if (length(current_unique_clusters) == length(final_desired_clusters) && all(sort(current_unique_clusters) == sort(final_desired_clusters))) {
      message("Successfully modified and filtered clusters in '", leiden_base_col_name, "'. Proceeding with: ", paste(sort(current_unique_clusters), collapse=", "))
    } else {
      warning(sprintf("WARNING: The unique clusters remaining in '%s' are: [%s]. Expected: [%s] after modifications. Please verify.", 
                      leiden_base_col_name, 
                      paste(sort(current_unique_clusters), collapse=", "), 
                      paste(sort(final_desired_clusters), collapse=", ")))
    }
  } else {
    warning(paste0("CRITICAL WARNING: No data remaining in 'all_events_final_with_cps_data' after reassigning and filtering clusters in column '", leiden_base_col_name, 
                   "'. Please check your cluster labels ('", cluster_to_reassign, "', '", target_cluster_for_reassignment, "', and '", 
                   paste(final_desired_clusters, collapse="','"), "') and ensure they are present in your data and that the logic is correct. Modeling will likely fail or produce no results."))
  }
} else {
  warning(paste("WARNING: Column '", leiden_base_col_name, "' not found in 'all_events_final_with_cps_data' or data is empty. Skipping custom cluster modification steps."))
}
# <<< MODIFICACIÓN FIN >>>


# --- Global Theme for ggplot2 ---
theme_paper_modelos <- function(base_size = 15) { 
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "sans", color = "grey20"),
      plot.title = element_text(size = rel(1.6), face = "bold", hjust = 0.5, margin = margin(b = 15)), 
      plot.subtitle = element_text(size = rel(1.3), color = "grey40", hjust = 0.5, margin = margin(b = 15)), 
      plot.caption = element_text(size = rel(0.9), color = "grey50", hjust = 1, margin = margin(t = 12)), 
      axis.title = element_text(size = rel(1.25), face = "bold", color = "grey30"), 
      axis.text = element_text(size = rel(1.1), color = "grey40"), 
      axis.line = element_line(color = "grey80", linewidth = 0.6),
      panel.grid.major = element_line(color = "grey93", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = rel(1.15), face = "bold"), 
      legend.text = element_text(size = rel(1.05)), 
      legend.background = element_rect(fill = "white", color = "grey90"),
      legend.key = element_rect(fill = "white", color = NA),
      strip.text = element_text(size = rel(1.25), face = "bold", color = "grey20"), 
      strip.background = element_rect(fill = "grey97", color = "grey80")
    )
}
theme_set(theme_paper_modelos())


# --- 1. DATA PREPARATION FOR MODELS ---
message("\n--- 1. Preparing data for models (using modified '", leiden_base_col_name, "' from 'all_events_final_with_cps_data') ---")

data_for_models <- copy(all_events_final_with_cps_data) 
message("Copied 'all_events_final_with_cps_data' (which includes Leiden cluster modifications) to 'data_for_models'.")

if (leiden_base_col_name %in% names(data_for_models)) {
  setnames(data_for_models, old = leiden_base_col_name, new = "skill_group_for_model", skip_absent = FALSE)
  message("Renamed column '", leiden_base_col_name, "' (now modified and filtered) to 'skill_group_for_model' in 'data_for_models'.")
} else {
  stop(paste0("CRITICAL ERROR: Column '", leiden_base_col_name, 
              "' not found in 'data_for_models' after copying from 'all_events_final_with_cps_data'. This column is required for modeling. ",
              "This might happen if 'all_events_final_with_cps_data' became empty or the column was unexpectedly dropped."))
}


required_dist_vars <- c("structural_distance", "education_diff_abs", "wage_diff_abs")
if(!all(required_dist_vars %in% names(data_for_models))) {
  missing_vars_error <- setdiff(required_dist_vars, names(data_for_models))
  stop(paste("Missing base distance variables:", paste(missing_vars_error, collapse=", ")))
}
data_for_models[, structural_distance_sq := structural_distance^2]
data_for_models[, education_diff_abs_sq := education_diff_abs^2]
data_for_models[, wage_diff_abs_sq := wage_diff_abs^2]

model_main_predictors <- c("structural_distance", "structural_distance_sq",
                           "education_diff_abs", "education_diff_abs_sq",
                           "wage_diff_abs", "wage_diff_abs_sq")

control_predictors_cps <- c()

if ("Prob_Transicion_Avg_CPS" %in% names(data_for_models)) { 
  message("Variable 'Prob_Transicion_Avg_CPS' found. Creating squared term.")
  data_for_models[, Prob_Transicion_Avg_CPS_sq := Prob_Transicion_Avg_CPS^2]
  control_predictors_cps <- c(control_predictors_cps, "Prob_Transicion_Avg_CPS", "Prob_Transicion_Avg_CPS_sq")
} else {
  warning("Variable 'Prob_Transicion_Avg_CPS' NOT found.")
}

if ("Agg_Total_Flow_Weighted_CPS" %in% names(data_for_models)) {
  message("Variable 'Agg_Total_Flow_Weighted_CPS' found. Including as predictor.")
  control_predictors_cps <- c(control_predictors_cps, "Agg_Total_Flow_Weighted_CPS")
} else {
  warning("Variable 'Agg_Total_Flow_Weighted_CPS' NOT found.")
}

if ("Agg_Total_Origin_Pop_Weighted_CPS" %in% names(data_for_models)) {
  message("Variable 'Agg_Total_Origin_Pop_Weighted_CPS' found. Including as predictor.")
  control_predictors_cps <- c(control_predictors_cps, "Agg_Total_Origin_Pop_Weighted_CPS")
} else {
  warning("Variable 'Agg_Total_Origin_Pop_Weighted_CPS' NOT found.")
}

if (length(control_predictors_cps) == 0) {
  warning("No CPS control variables were found or added. Models will run WITHOUT CPS flow controls.")
} else {
  message("CPS controls to include: ", paste(control_predictors_cps, collapse = ", "))
}


all_model_predictors <- c(model_main_predictors, control_predictors_cps)
all_model_predictors <- unique(all_model_predictors) 
message("Final predictors for the model: ", paste(all_model_predictors, collapse=", "))

cols_for_na_check <- c("diffusion", all_model_predictors, "skill_group_for_model")
cols_for_na_check_existing <- intersect(cols_for_na_check, names(data_for_models))

data_for_models_original_rows <- nrow(data_for_models)
data_for_models <- na.omit(data_for_models, cols = cols_for_na_check_existing)
message(sprintf("Original rows in data_for_models (after cluster modification & renaming): %d. Rows after na.omit on predictors and group: %d (removed: %d)",
                data_for_models_original_rows, nrow(data_for_models), data_for_models_original_rows - nrow(data_for_models)))

if (nrow(data_for_models) == 0) stop("No data remaining after applying na.omit. This might be due to previous cluster filtering or NA values in predictors.")

data_for_models[, skill_group_for_model := as.character(skill_group_for_model)]
unique_skill_groups <- sort(unique(data_for_models$skill_group_for_model)) 
message(sprintf("\nModeling for %d skill groups: %s", length(unique_skill_groups), paste(unique_skill_groups, collapse=", ")))
if (length(unique_skill_groups) > 0) {
  print(data_for_models[,.N, by=skill_group_for_model][order(skill_group_for_model)])
} else {
  message("No skill groups left to model.")
}


# --- 2. LOGISTIC REGRESSION MODEL FITTING ---
message("\n--- 2. Fitting logistic regression models ---")
fitted_models_list <- list()
model_formula_str <- paste("diffusion ~", paste(all_model_predictors, collapse = " + "))
model_formula <- as.formula(model_formula_str)
message(paste("Using model formula:", model_formula_str))

if (length(unique_skill_groups) > 0) { 
  for (current_skill_group in unique_skill_groups) {
    message(sprintf("\n=== Modeling for Skill Group: %s ===", current_skill_group))
    subset_data <- data_for_models[skill_group_for_model == current_skill_group]
    min_obs_required <- length(all_model_predictors) + 50 
    if (nrow(subset_data) < min_obs_required) {
      message(sprintf("Insufficient observations (%d) for '%s'. At least %d required. Skipping.", nrow(subset_data), current_skill_group, min_obs_required)); next
    }
    if (length(unique(subset_data$diffusion)) < 2) {
      message(sprintf("No variance in 'diffusion' for '%s'. Skipping.", current_skill_group)); next
    }
    model_fit <- tryCatch({ glm(model_formula, data = subset_data, family = binomial(link = "logit")) },
                          error = function(e) { message(sprintf("Error fitting model for '%s': %s", current_skill_group, e$message)); return(NULL) })
    if (!is.null(model_fit)) {
      summary_model <- summary(model_fit); logLik_fit <- tryCatch(logLik(model_fit), error = function(e) NA_real_); mcfadden_r2 <- NA_real_
      if (!is.na(logLik_fit)) {
        null_model <- tryCatch(glm(diffusion ~ 1, data = subset_data, family = binomial(link = "logit")), error = function(e) NULL)
        if (!is.null(null_model)) {
          logLik_null <- tryCatch(logLik(null_model), error = function(e) NA_real_)
          if(!is.na(logLik_null) && logLik_null != 0 && logLik_fit <= logLik_null) mcfadden_r2 <- 1 - (logLik_fit / logLik_null) 
          else message(sprintf("Warning: Could not calculate McFadden R² for '%s' (logLik_fit=%s, logLik_null=%s)", current_skill_group, logLik_fit, logLik_null))
        }}
      fitted_models_list[[current_skill_group]] <- list(model = model_fit, summary_coeffs = summary_model$coefficients, aic = AIC(model_fit), bic = BIC(model_fit), mcfadden_r2 = as.numeric(mcfadden_r2), n_obs = nrow(subset_data), n_events = sum(subset_data$diffusion == 1), diffusion_rate = mean(subset_data$diffusion), formula_str = model_formula_str, predictors_in_model = all_model_predictors, skill_group_name = current_skill_group)
      message(sprintf("Model for '%s' fitted. N=%s, Events=%s (%.1f%%), AIC: %.2f, McFadden R²: %.4f", current_skill_group, scales::comma(nrow(subset_data)), scales::comma(sum(subset_data$diffusion == 1)), 100*mean(subset_data$diffusion), AIC(model_fit), as.numeric(mcfadden_r2)))
    }
  }
} else {
  message("No unique skill groups available for model fitting. Check data preprocessing and cluster modification steps.")
}
saveRDS(fitted_models_list, file.path(output_dir_modelos, paste0("fitted_logistic_models_", leiden_base_col_name, "_2Clusters_CPS.rds")))
message(paste("\nFitted models saved to:", file.path(output_dir_modelos, paste0("fitted_logistic_models_", leiden_base_col_name, "_2Clusters_CPS.rds"))))

gc()
# --- 3. GENERATING PREDICTIONS FOR MARGINAL EFFECTS ---
message("\n--- 3. Generating predictions for marginal effects ---")
marginal_effects_data_list <- list()
# <<< MODIFICACIÓN: Quitar 'Prob_Transicion_Avg_CPS' de la lista base si no se quiere plottear >>>
# Se deja aquí para generar los datos, pero se quitará del plot combinado más adelante.
vars_for_effects_plots <- c("structural_distance", "education_diff_abs", "wage_diff_abs")
if ("Prob_Transicion_Avg_CPS" %in% all_model_predictors) vars_for_effects_plots <- c(vars_for_effects_plots, "Prob_Transicion_Avg_CPS")
if ("Agg_Total_Flow_Weighted_CPS" %in% all_model_predictors) vars_for_effects_plots <- c(vars_for_effects_plots, "Agg_Total_Flow_Weighted_CPS")
if ("Agg_Total_Origin_Pop_Weighted_CPS" %in% all_model_predictors) vars_for_effects_plots <- c(vars_for_effects_plots, "Agg_Total_Origin_Pop_Weighted_CPS")
vars_for_effects_plots <- unique(vars_for_effects_plots)


for (current_skill_group_iter in names(fitted_models_list)) {
  model_output <- fitted_models_list[[current_skill_group_iter]]
  if (is.null(model_output) || is.null(model_output$model)) { message(sprintf("No valid model for '%s' to generate predictions.", current_skill_group_iter)); next }
  message(sprintf("🔄 Predictions for group: '%s'", current_skill_group_iter))
  current_model_obj <- model_output$model
  subset_data_for_preds <- data_for_models[skill_group_for_model == current_skill_group_iter] 
  if(nrow(subset_data_for_preds) == 0) {
    message(sprintf("No data found for group '%s' in data_for_models for predictions. Skipping.", current_skill_group_iter))
    next
  }
  predictors_in_this_model <- model_output$predictors_in_model
  
  reference_grid <- sapply(intersect(predictors_in_this_model, names(subset_data_for_preds)), 
                           function(p) median(subset_data_for_preds[[p]], na.rm=T), simplify=F)
  reference_grid <- lapply(reference_grid, function(x) if(is.na(x)) 0 else x) 
  
  for (var_interest in vars_for_effects_plots) {
    if (!var_interest %in% predictors_in_this_model) { 
      next 
    }
    message(sprintf("    • For variable '%s'...", var_interest))
    var_values_for_seq <- subset_data_for_preds[[var_interest]]; var_range <- quantile(var_values_for_seq, c(0.025,0.975),na.rm=T,type=7)
    if(is.na(var_range[1])||is.na(var_range[2])|| (abs(diff(var_range)) < 1e-6 && var_range[1] != 0) ){ 
      median_v<-median(var_values_for_seq,na.rm=T)
      if(is.na(median_v)){message(sprintf("❌ Cannot generate sequence for %s in %s (median is NA)",var_interest,current_skill_group_iter));next}
      sd_v <- sd(var_values_for_seq, na.rm=T)
      if(is.na(sd_v) || sd_v < 1e-6) sd_v <- abs(median_v * 0.1) + 1e-5 
      var_range[1]<-median_v - sd_v 
      var_range[2]<-median_v + sd_v
      if(is.na(var_range[1]) || is.na(var_range[2]) || (abs(diff(var_range)) < 1e-6 && var_range[1] != 0) ) { 
        var_range[1] <- median_v * 0.9 - (if(median_v==0) 0.1 else 0)
        var_range[2] <- median_v * 1.1 + (if(median_v==0) 0.1 else 0)
      }
      if(abs(var_range[1]-var_range[2])<1e-6 && var_range[1] == 0) var_range[2] <- var_range[1]+1 
      else if(abs(var_range[1]-var_range[2])<1e-6) var_range[2] <- var_range[1] + abs(var_range[1]*0.1) + 1e-6 
      
      message(sprintf("⚠️ Adjusted range for %s:[%.3f,%.3f]",var_interest,var_range[1],var_range[2]))
    }
    var_seq <- seq(var_range[1], var_range[2], length.out=100)
    
    newdata_for_pred <- map_dfr(var_seq, function(val){
      point_data <- as.data.table(reference_grid)
      point_data[[var_interest]] <- val
      sq_col <- paste0(var_interest,"_sq")
      if(sq_col %in% predictors_in_this_model) point_data[[sq_col]] <- val^2
      return(point_data)
    })
    
    final_model_predictors_cols <- names(coef(current_model_obj))[-1] 
    final_model_predictors_cols_present <- intersect(final_model_predictors_cols, names(newdata_for_pred))
    
    missing_cols_in_newdata <- setdiff(final_model_predictors_cols, final_model_predictors_cols_present)
    if(length(missing_cols_in_newdata) > 0){
      message(sprintf("Warning: Missing columns in newdata for prediction for group '%s', variable '%s': %s. These predictors will effectively be zero or their median if not varied. This might be OK if they are not the variable of interest or its square.", 
                      current_skill_group_iter, var_interest, paste(missing_cols_in_newdata, collapse=", ")))
      for(mc in missing_cols_in_newdata){
        if(mc %in% names(reference_grid)){
          newdata_for_pred[[mc]] <- reference_grid[[mc]]
        } else {
          message(sprintf("Critical Warning: Predictor '%s' missing from reference_grid for group '%s'. Prediction might be unreliable.", mc, current_skill_group_iter))
          newdata_for_pred[[mc]] <- 0 
        }
      }
    }
    newdata_for_pred_final <- newdata_for_pred[, ..final_model_predictors_cols] 
    
    
    preds_link <- predict(current_model_obj, newdata=newdata_for_pred_final, type="link", se.fit=T)
    marginal_effects_data_list[[length(marginal_effects_data_list)+1]] <- data.table(
      skill_group_for_model=current_skill_group_iter,
      variable_of_interest=var_interest,
      value_of_variable=var_seq,
      predicted_probability=plogis(preds_link$fit),
      lower_ci=plogis(preds_link$fit-1.96*preds_link$se.fit),
      upper_ci=plogis(preds_link$fit+1.96*preds_link$se.fit),
      classification_column_used = classification_column_to_use 
    )
  }
}
if (length(marginal_effects_data_list)>0) { 
  all_marginal_effects_data <- rbindlist(marginal_effects_data_list)
  fwrite(all_marginal_effects_data, file.path(output_dir_modelos, paste0("predicted_marginal_effects_", leiden_base_col_name, "_2Clusters_CPS.csv")))
  message(paste("\n💾 Marginal effects data saved.")) 
} else { 
  all_marginal_effects_data <- data.table() 
  message("⚠️ No marginal effects data generated.") 
}

# --- 5. TABLA RESUMEN DE COEFICIENTES (MOVIDA ANTES DEL PLOT COMBINADO) ---
message("\n--- 5. Creating coefficient summary table (", leiden_base_col_name, " 2 Clusters with CPS) ---") 
coef_summary_df_formatted <- data.table() # Inicializar
if (length(fitted_models_list) > 0) {
  message("📋 Generating coefficient table...") 
  
  coef_summary_df <- map_dfr(names(fitted_models_list), function(s_grp) { model_entry<-fitted_models_list[[s_grp]]; if(!is.null(model_entry$model)){tidied<-broom::tidy(model_entry$model,conf.int=T); tidied$skill_group_for_model<-s_grp; tidied$n_obs<-model_entry$n_obs; tidied$n_events<-model_entry$n_events; tidied$diffusion_rate<-model_entry$diffusion_rate; tidied$mcfadden_r2<-model_entry$mcfadden_r2; tidied$aic<-model_entry$aic; tidied$formula<-model_entry$formula_str; tidied$classification_method<-paste0(classification_column_to_use, "_2GrpsMod"); return(tidied)}; return(NULL) }) 
  if (nrow(coef_summary_df) > 0) {
    coef_summary_df_formatted <- coef_summary_df %>% 
      mutate(odds_ratio=exp(estimate),
             sig_stars=case_when(p.value<0.001~"***",p.value<0.01~"**",p.value<0.05~"*",p.value<0.10~".",TRUE~""), 
             estimate_plus_ci=sprintf("%.3f%s [%.3f, %.3f]",estimate,sig_stars,conf.low,conf.high)) %>% 
      select(skill_group_for_model,term,estimate,std.error,statistic,p.value,sig_stars,odds_ratio,conf.low,conf.high,estimate_plus_ci,n_obs,mcfadden_r2,aic,formula,classification_method) %>% 
      arrange(skill_group_for_model,term)
    fwrite(coef_summary_df_formatted, file.path(output_dir_modelos, paste0("table_coefficients_",leiden_base_col_name,"_2Clusters_CPS_full_ENG.csv"))) 
    message(paste("💾 Coefficient table saved.")) 
    tryCatch({ 
      coef_pivot<-coef_summary_df_formatted %>% 
        filter(term!="(Intercept)") %>% 
        select(term,skill_group_for_model,estimate_plus_ci) %>% 
        tidyr::pivot_wider(names_from=skill_group_for_model,values_from=estimate_plus_ci,values_fill="-") %>% 
        arrange(match(term,all_model_predictors)) 
      message("\n📊 Pivoted Coefficient Summary:"); print(coef_pivot) 
      fwrite(coef_pivot, file.path(output_dir_modelos, paste0("table_coefficients_",leiden_base_col_name,"_2Clusters_CPS_pivoted_ENG.csv"))) 
    }, error=function(e)message("⚠️ Error in pivoted table: ",e$message)) 
  } else message("⚠️ No coefficients extracted.") 
} else message("⚠️ No models fitted (or no models in fitted_models_list).") 

gc()

# --- 4. CREATING ENHANCED PLOTS (AHORA DESPUÉS DE LA TABLA DE COEF) ---
message("\n--- 4. Creating marginal effects plots ---")
if (nrow(all_marginal_effects_data) > 0) {
  num_skill_groups_plot <- uniqueN(all_marginal_effects_data$skill_group_for_model) 
  plot_colors <- if(num_skill_groups_plot==2) c("#E31A1C", "#1F78B4") else if(num_skill_groups_plot <=8 && num_skill_groups_plot >0) brewer.pal(max(3,num_skill_groups_plot),"Set1") else if (num_skill_groups_plot > 0) colorRampPalette(brewer.pal(8,"Set1"))(num_skill_groups_plot) else "black"
  if(num_skill_groups_plot > 0 && length(plot_colors) < num_skill_groups_plot) plot_colors <- rep(plot_colors, length.out = num_skill_groups_plot)
  
  current_groups_in_plot_data <- sort(unique(all_marginal_effects_data$skill_group_for_model))
  if(num_skill_groups_plot > 0 && length(current_groups_in_plot_data) == length(plot_colors)) {
    names(plot_colors) <- current_groups_in_plot_data
  } else if (num_skill_groups_plot > 0) { 
    plot_colors <- setNames(rep(plot_colors, length.out = length(current_groups_in_plot_data)), current_groups_in_plot_data)
  }
  
  
  create_adv_marginal_plot <- function(d,var,xlab,title_main,pal, class_col_name_plot){ 
    ds<-d[variable_of_interest==var];if(nrow(ds)==0)return(ggplot()+labs(title=paste("No data for",var))) 
    n_grp<-uniqueN(ds$skill_group_for_model)
    groups_in_ds <- sort(unique(ds$skill_group_for_model))
    current_pal <- pal[names(pal) %in% groups_in_ds]
    if(length(current_pal) == 0 && length(groups_in_ds) > 0) { 
      current_pal <- setNames(pal[1:length(groups_in_ds)], groups_in_ds)
    }
    
    p <- ggplot(ds,aes(x=value_of_variable,y=predicted_probability,color=skill_group_for_model,fill=skill_group_for_model,group=skill_group_for_model)) +
      geom_line(linewidth=1.3,alpha=0.9) + geom_ribbon(aes(ymin=lower_ci,ymax=upper_ci),alpha=0.15,linetype="dashed",show.legend=F) +
      scale_y_continuous(labels=scales::percent_format(accuracy=1),limits=c(0,NA),expand=expansion(mult=c(0,0.05))) +
      scale_color_manual(values=current_pal,name="Skill Group:") + scale_fill_manual(values=current_pal,name="Skill Group:", guide="none") + 
      labs(title=title_main,subtitle=paste("Base Classification (modified):",class_col_name_plot),x=xlab,y="Predicted Diffusion Probability",caption="Other variables at median. 95% CI.") 
    if(n_grp >=2 && n_grp <=4) p <- p + facet_wrap(~skill_group_for_model,scales="free_y",ncol=min(2,n_grp)) + theme(legend.position="none")
    else if(n_grp > 4) message(sprintf("Info: Plot for '%s' has %d groups. No faceting applied by default.",var, n_grp)) 
    return(p)
  }
  # Se generan los plots individuales como antes (opcional, pero se deja por si son útiles)
  plot_struc <- create_adv_marginal_plot(all_marginal_effects_data,"structural_distance","Structural Distance","Effect of Structural Distance",plot_colors, classification_column_to_use); print(plot_struc); ggsave(file.path(output_dir_modelos,paste0("plot_",leiden_base_col_name,"_2Clusters_CPS_effect_structural_ENG.png")),plot_struc,w=12,h=max(6, 4*num_skill_groups_plot/2),dpi=300,bg="white") 
  plot_educ <- create_adv_marginal_plot(all_marginal_effects_data,"education_diff_abs","Absolute Educational Distance","Effect of Educational Distance",plot_colors, classification_column_to_use); print(plot_educ); ggsave(file.path(output_dir_modelos,paste0("plot_",leiden_base_col_name,"_2Clusters_CPS_effect_education_ENG.png")),plot_educ,w=12,h=max(6, 4*num_skill_groups_plot/2),dpi=300,bg="white")
  plot_wage <- create_adv_marginal_plot(all_marginal_effects_data,"wage_diff_abs","Absolute Wage Distance","Effect of Wage Distance",plot_colors, classification_column_to_use); print(plot_wage); ggsave(file.path(output_dir_modelos,paste0("plot_",leiden_base_col_name,"_2Clusters_CPS_effect_wage_ENG.png")),plot_wage,w=12,h=max(6, 4*num_skill_groups_plot/2),dpi=300,bg="white")
  
  # --- MODIFICACIÓN: Plot combinado ajustado ---
  
  # Seleccionar solo las variables de distancia para el plot combinado
  main_dist_vars_for_combined_plot <- c("structural_distance", "education_diff_abs", "wage_diff_abs")
  
  plot_data_comb <- all_marginal_effects_data[variable_of_interest %in% main_dist_vars_for_combined_plot]
  
  if(nrow(plot_data_comb)>0){
    # Crear etiquetas para facets con "Distance"
    combined_plot_labels <- c(
      "structural_distance" = "Structural Distance",
      "education_diff_abs" = "Educational Distance", # <<< MODIFICACIÓN: Etiqueta
      "wage_diff_abs" = "Wage Distance"             # <<< MODIFICACIÓN: Etiqueta
    )
    vars_actually_in_plot_data_comb <- intersect(main_dist_vars_for_combined_plot, unique(plot_data_comb$variable_of_interest))
    
    plot_data_comb[, var_label_facet := factor(variable_of_interest, 
                                               levels=intersect(vars_actually_in_plot_data_comb, names(combined_plot_labels)), 
                                               labels=combined_plot_labels[intersect(vars_actually_in_plot_data_comb, names(combined_plot_labels))])] 
    
    # Preparar datos de significancia si hay coeficientes
    significance_data <- data.table() # Inicializar
    if (nrow(coef_summary_df_formatted) > 0) {
      significance_data <- as.data.table(coef_summary_df_formatted) %>%
        .[term %in% c("structural_distance", "structural_distance_sq", 
                      "education_diff_abs", "education_diff_abs_sq", 
                      "wage_diff_abs", "wage_diff_abs_sq")] %>%
        .[, .(variable_of_interest = str_remove(term, "_sq$"),
              term_type = ifelse(str_detect(term, "_sq$"), "Quad", "Lin"),
              sig_stars), by = .(skill_group_for_model, term)] %>%
        dcast(skill_group_for_model + variable_of_interest ~ term_type, value.var = "sig_stars", fill = "") %>%
        .[, sig_label := paste0("Sig: L", Lin, " Q", Quad)] %>%
        .[, .(skill_group_for_model, variable_of_interest, sig_label)] %>%
        unique()
      
      positions <- plot_data_comb[, .(x_pos = max(value_of_variable), 
                                      y_pos = min(lower_ci)), 
                                  by = .(skill_group_for_model, variable_of_interest)]
      
      significance_data <- merge(significance_data, positions, by = c("skill_group_for_model", "variable_of_interest"), all.x = TRUE)
      
      # Merge con plot_data_comb para obtener var_label_facet
      significance_data <- merge(significance_data, unique(plot_data_comb[, .(skill_group_for_model, variable_of_interest, var_label_facet)]),
                                 by = c("skill_group_for_model", "variable_of_interest"), all.x = TRUE)
      
    } else {
      message("⚠️ No hay datos de coeficientes, no se añadirán etiquetas de significancia.")
    }
    
    groups_in_combined_plot <- sort(unique(plot_data_comb$skill_group_for_model))
    combined_plot_pal <- plot_colors[names(plot_colors) %in% groups_in_combined_plot]
    if(length(combined_plot_pal) == 0 && length(groups_in_combined_plot) > 0) { 
      combined_plot_pal <- setNames(plot_colors[1:length(groups_in_combined_plot)], groups_in_combined_plot)
    }
    
    # 1. Ejecuta tu código de nuevo para estar seguros
    plot_data_comb <- plot_data_comb %>%
      mutate(skill_group_for_model = case_when(
        skill_group_for_model == "LeidenC_1" ~ "Socio-cognitive",
        skill_group_for_model == "LeidenC_2" ~ "Socio-technical",
        TRUE ~ NA_character_
      ))
    
     combined_plot_pal <- c(
      "Socio-cognitive" = "#E69F00", # Naranja
      "Socio-technical" = "#56B4E9"  # Azul cielo
    )
    
    p_comb <- ggplot(plot_data_comb,aes(x=value_of_variable,y=predicted_probability,color=skill_group_for_model,fill=skill_group_for_model,group=skill_group_for_model)) +
      geom_line(linewidth=1.1) + 
      # <<< MODIFICACIÓN: Banda de error (CI 95%) más visible >>>
      geom_ribbon(aes(ymin=lower_ci,ymax=upper_ci),alpha=0.5, linetype=3, show.legend=F) +
      facet_grid(skill_group_for_model ~ var_label_facet, scales="free_x", switch="y") + 
      scale_y_continuous(labels=scales::percent_format(accuracy=1)) + 
      scale_color_manual(values=combined_plot_pal,guide="none") + 
      scale_fill_manual(values=combined_plot_pal,guide="none") +
      # <<< MODIFICACIÓN: Título y caption actualizados >>>
      labs(title="Predicted values of distances by skill group", 
           #subtitle=paste("Base Classification (modified):",classification_column_to_use), 
           x="Predictor Value",y="Predicted Diffusion Probability", 
           caption="Other variables at median. Shaded area represents 95% Confidence Interval (±1.96 * SE).") + 
      theme(axis.text.x=element_text(angle=35,hjust=1,size=rel(0.9)), 
            strip.text.y=element_text(angle=0, size=rel(0.95)), 
            strip.text.x = element_text(size=rel(0.95))) 
    
    # <<< MODIFICACIÓN: Añadir etiquetas de significancia si existen >>>
    if(nrow(significance_data) > 0 && "var_label_facet" %in% names(significance_data)) {
      p_comb <- p_comb + 
        geom_text(data = significance_data, 
                  aes(x = x_pos, y = y_pos, label = sig_label), 
                  hjust = 1.05, vjust = -0.4, size = 3, color = "grey30", inherit.aes = FALSE,
                  check_overlap = TRUE)
    }
    
    print(p_comb); ggsave(file.path(output_dir_modelos,paste0("plot_",leiden_base_col_name,"_2Clusters_CPS_combined_distances_ENG.png")),p_comb,w=12,h=max(8,2.5+2.8*num_skill_groups_plot),dpi=300,bg="white",limitsize=F) 
  }
} else message("⚠️ No marginal effects data to plot.")

