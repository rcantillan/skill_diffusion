# =======================================================================
# PART 2.5: DESCRIPTIVE STATISTICS AND ADVANCED VISUALIZATIONS
# Version: 3.1.8 (Enhanced Diagnostics in B.3, English Labels, Clearer Fonts)
# =======================================================================

# --- 0. SETUP AND LIBRARY LOADING ---
message("--- PART 2.5: Advanced Descriptive Analysis (Leiden 2015 Corrected) ---")
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(viridis) 
  library(ggraph)
  library(igraph)
  library(scales)
  library(ggridges)
  library(patchwork) 
  library(ggpubr) 
  library(ggrepel)
  library(knitr)
  library(kableExtra)
  library(RColorBrewer)
  library(stringr) 
  library(purrr) 
  library(ggalluvial)
})

# --- PATH CONFIGURATION ---
message("### IMPORTANT INSTRUCTIONS FOR DATA PATH ###")
message("# Please ensure the 'path_to_parte1_outputs' variable is correctly defined.")
message("# OPTION 1 (RECOMMENDED): Define the following two lines BEFORE running this script:")
message("# path_to_parte1_outputs_defined_manually <- TRUE")
message("# path_to_parte1_outputs <- \"/actual/path/to/your/part1_outputs_directory\"")
message("# Replace with your actual path, e.g., \"/home/rober/Documentos/skill_diffusion/datos_eventos_v12_MultiClasif_Cleaned_Corrected\"")
message("########################################################")

path_to_parte1_outputs_defined_manually <- TRUE 
path_to_parte1_outputs <- "/home/rober/Documentos/skill_diffusion/datos_eventos_v12_MultiClasif_Cleaned_Corrected" 

if (!exists("path_to_parte1_outputs_defined_manually") || !isTRUE(path_to_parte1_outputs_defined_manually)) {
  message("\nThe variable 'path_to_parte1_outputs_defined_manually' is not TRUE or does not exist.")
  message("You will be prompted to enter the path manually.")
  path_to_parte1_outputs <- readline(prompt="Enter the FULL path to PART 1 results (where .csv and .rds files are): ")
  
  if (length(path_to_parte1_outputs) == 0 || !is.character(path_to_parte1_outputs) || nchar(path_to_parte1_outputs) < 2 || grepl("if\\s*\\(", path_to_parte1_outputs) || grepl("stop\\(", path_to_parte1_outputs)) {
    stop("The entered path ('", path_to_parte1_outputs ,"') seems invalid or empty, or might be a line of code. Please define 'path_to_parte1_outputs' and 'path_to_parte1_outputs_defined_manually <- TRUE' at the beginning of the script with the correct path.")
  }
} else {
  if (!exists("path_to_parte1_outputs") || !is.character(path_to_parte1_outputs) || length(path_to_parte1_outputs) != 1 || nchar(path_to_parte1_outputs) == 0) {
    stop("'path_to_parte1_outputs_defined_manually' is TRUE, but 'path_to_parte1_outputs' is not defined as a valid character string. Please define it at the beginning of the script.")
  }
  message("Using predefined path_to_parte1_outputs: ", path_to_parte1_outputs)
}

if (!dir.exists(path_to_parte1_outputs)) {
  stop(paste("The PART 1 output directory specified does not exist:", path_to_parte1_outputs, 
             "\nPlease verify the path and ensure PART 1 outputs are there."))
} else {
  message("PART 1 output directory found: ", normalizePath(path_to_parte1_outputs))
}

output_dir_descriptives <- file.path(path_to_parte1_outputs, "Descriptivos_Leiden2015_Corregido_v3.1.8") # Updated version
if (!dir.exists(output_dir_descriptives)) {
  dir.create(output_dir_descriptives, recursive = TRUE)
}
message("Outputs from this script will be saved in: ", normalizePath(output_dir_descriptives))

# --- VISUAL THEME (ENGLISH LABELS, LARGER FONTS) ---
theme_paper <- function(base_size = 15) { 
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
theme_set(theme_paper())

# --- HELPER FUNCTION FOR FACET_WRAP ---
determine_facet_ncols <- function(num_facetas) {
  if (num_facetas <= 1) return(1)
  if (num_facetas == 2) return(2)
  if (num_facetas == 3) return(3)
  if (num_facetas == 4) return(2) 
  if (num_facetas <= 6) return(3) 
  if (num_facetas <= 9) return(3) 
  return(min(4, ceiling(sqrt(num_facetas)))) 
}


# --- DATA LOADING ---
message("\n--- Loading Data ---")
skill_class_dt_path <- file.path(path_to_parte1_outputs, "skill_classification_FULL_LouvainLeiden_AllYears.csv")
all_events_dt_original_path <- file.path(path_to_parte1_outputs, "all_events_FOR_MODELS_LeidenCluster_2015_Corrected.rds") 

onet_skill_values_dt_path <- file.path(path_to_parte1_outputs, "all_skills_data_harmonized.csv")

if(!file.exists(skill_class_dt_path)) stop("File skill_classification_FULL_LouvainLeiden_AllYears.csv not found in: ", path_to_parte1_outputs)
if(!file.exists(all_events_dt_original_path)) stop("File all_events_FOR_MODELS_LeidenCluster_2015_Corrected.rds not found in: ", path_to_parte1_outputs, ". Please ensure a previous script (Part 1 or Part 2.5) generated this file with the corrected 'skill_type'.")

skill_class_dt <- fread(skill_class_dt_path)
all_events_dt_for_diffusion_analysis <- readRDS(all_events_dt_original_path) 
setDT(all_events_dt_for_diffusion_analysis) 

onet_skill_values_dt <- NULL
if (file.exists(onet_skill_values_dt_path)) {
  onet_skill_values_dt <- fread(onet_skill_values_dt_path)
} else { message("WARNING: File all_skills_data_harmonized.csv not found.") }
gc()

# =============================================================================
# SECTION A: ANALYSIS OF CORRECTED LEIDEN 2015 CLUSTERS
# =============================================================================
message("\n--- A. ANALYSIS OF CORRECTED LEIDEN 2015 CLUSTERS ---")
corrected_leiden_col <- "LeidenCluster_2015_Corrected" 
year_for_leiden_metrics <- 2015 
leiden_cognitive_cluster_name <- "LeidenC_1" 
leiden_physical_cluster_name <- "LeidenC_2"  

cluster_col_original_leiden <- "LeidenCluster_2015"
if (!cluster_col_original_leiden %in% names(skill_class_dt)) {
  stop(paste0("Source column for correction '", cluster_col_original_leiden, "' not found in skill_class_dt. Columns available: ", paste(names(skill_class_dt), collapse=", ")))
}
if (!corrected_leiden_col %in% names(skill_class_dt)) {
  message(paste0("Creating '", corrected_leiden_col, "' from '", cluster_col_original_leiden, "'."))
  skill_class_dt[, (corrected_leiden_col) := as.character(get(cluster_col_original_leiden))]
} else {
  message(paste0("Column '", corrected_leiden_col, "' already exists in skill_class_dt."))
}


# A.1. Distribution of Skills by Corrected Cluster
if (corrected_leiden_col %in% names(skill_class_dt)) {
  cluster_summary_corrected_A1 <- skill_class_dt[, .N, by = .(Cluster = get(corrected_leiden_col))] 
  setnames(cluster_summary_corrected_A1, "Cluster", "Cluster_Label") 
  cluster_summary_corrected_A1[Cluster_Label == leiden_cognitive_cluster_name, Display_Name := "Sociocognitive Skills (LeidenC_1)"]
  cluster_summary_corrected_A1[Cluster_Label == leiden_physical_cluster_name, Display_Name := "Physical/Sensorymotor Skills (LeidenC_2)"]
  cluster_summary_corrected_A1[is.na(Display_Name), Display_Name := Cluster_Label] 
  
  setorder(cluster_summary_corrected_A1, -N)
  message(paste("\nA.1. Skill Distribution by Corrected Cluster (", corrected_leiden_col, "):"))
  
  print(kable(cluster_summary_corrected_A1[,.(Display_Name, N)], 
              caption = paste("Skill Distribution by Corrected Cluster (", corrected_leiden_col, ")", sep=""),
              col.names = c("Skill Cluster Type", "Number of Skills")) %>% 
          kable_styling(bootstrap_options = "striped", full_width = FALSE))
} else {
  message(paste("WARNING: Column", corrected_leiden_col, "not found in skill_class_dt. Skipping A.1."))
}

# A.2. Skill Polarization Plot
message("\n--- A.2. Skill Polarization Plot ---")
wage_metric_col <- paste0("wage_val_", year_for_leiden_metrics)
edu_metric_col <- paste0("edu_val_", year_for_leiden_metrics)

message("DEBUG: Columns available in skill_class_dt for Polarization Plot: ", paste(names(skill_class_dt), collapse=", "))
required_cols_polarization <- c(corrected_leiden_col, wage_metric_col, edu_metric_col, "Element.Name")
missing_cols_for_polarization <- setdiff(required_cols_polarization, names(skill_class_dt))

if (length(missing_cols_for_polarization) == 0) {
  
  plot_data_polarization <- skill_class_dt[!is.na(get(wage_metric_col)) & !is.na(get(edu_metric_col)) & 
                                             !is.na(get(corrected_leiden_col)) & (get(corrected_leiden_col) %in% c(leiden_cognitive_cluster_name, leiden_physical_cluster_name)), 
                                           .(Element.Name, 
                                             Skill_Cluster_Raw = get(corrected_leiden_col), 
                                             Education_Score = get(edu_metric_col), 
                                             Salary_USD_Thousands = get(wage_metric_col) / 1000)] 
  
  plot_data_polarization[Skill_Cluster_Raw == leiden_cognitive_cluster_name, Skill_Type_Label := "Sociocognitive Skills"]
  plot_data_polarization[Skill_Cluster_Raw == leiden_physical_cluster_name, Skill_Type_Label := "Physical/Sensorymotor Skills"]
  plot_data_polarization <- plot_data_polarization[!is.na(Skill_Type_Label)] 
  
  skills_to_label_cognitive <- c("Critical Thinking", "Science", "Systems Analysis", "Complex Problem Solving", "Programming", "Judgment and Decision Making", "Active Learning", "Mathematics")
  skills_to_label_physical <- c("Spatial Orientation", "Control Precision", "Static Strength", "Arm-Hand Steadiness", "Manual Dexterity", "Stamina", "Rate Control", "Finger Dexterity", "Gross Body Coordination")
  
  skills_for_plot_labels <- plot_data_polarization[Element.Name %in% c(skills_to_label_cognitive, skills_to_label_physical)]
  
  if (nrow(plot_data_polarization) > 10) { 
    p_skill_polarization <- ggplot(plot_data_polarization, aes(x = Education_Score, y = Salary_USD_Thousands, color = Skill_Type_Label)) +
      geom_point(alpha = 0.6, size = 3.5) + 
      stat_ellipse(aes(group = Skill_Type_Label, fill = Skill_Type_Label), geom = "polygon", alpha = 0.15, type = "t", level = 0.90, show.legend = FALSE) +
      geom_text_repel(data = skills_for_plot_labels, aes(label = Element.Name), 
                      size = 4.5, box.padding = 0.7, point.padding = 0.5, 
                      max.overlaps = 25, segment.color = 'grey40', segment.alpha = 0.7,
                      min.segment.length = 0.1, force = 20, force_pull = 0.15) + 
      scale_color_manual(values = c("Sociocognitive Skills" = "#0072B2", "Physical/Sensorymotor Skills" = "#D55E00"), name = "Skill Cluster Type:") +
      scale_fill_manual(values = c("Sociocognitive Skills" = "#0072B2", "Physical/Sensorymotor Skills" = "#D55E00")) +
      labs(title = "Skill Polarization in the Labor Market",
           subtitle = paste("Based on", corrected_leiden_col, "and O*NET Attributes (",year_for_leiden_metrics,")", sep=""),
           x = "Educational Requirement Score (O*NET)",
           y = paste("Associated Median Salary (Thousand USD, ", year_for_leiden_metrics, ")", sep=""),
           caption = "Ellipses represent 90% confidence intervals for each cluster.") +
      theme_paper(base_size = 16) + 
      theme(legend.position = "bottom",
            legend.title = element_text(size=rel(1.05)),
            legend.text = element_text(size=rel(1.0)),
            axis.title = element_text(size = rel(1.2)), 
            axis.text = element_text(size = rel(1.1)))  
    
    print(p_skill_polarization)
    ggsave(file.path(output_dir_descriptives, "A2_skill_polarization_plot_CORREGIDO_ENG.png"), p_skill_polarization, width = 14, height = 11, dpi = 300, bg = "white")
  } else {
    message("Not enough data points after filtering for the Skill Polarization Plot (N=", nrow(plot_data_polarization), ").")
  }
} else {
  message(paste("One or more required columns for Skill Polarization Plot not found in skill_class_dt. Missing:", paste(missing_cols_for_polarization, collapse=", "), ". Skipping A.2."))
}


# --- ENSURE all_events_dt_for_diffusion_analysis IS PREPARED ---
if ("skill_type" %in% names(all_events_dt_for_diffusion_analysis)) {
  all_events_dt_for_diffusion_analysis[, skill_type_plot_label := fcase(
    skill_type == leiden_cognitive_cluster_name, "Sociocognitive",
    skill_type == leiden_physical_cluster_name, "Physical/Sensorymotor",
    skill_type == paste0("SkillNoEnClasif_",year_for_leiden_metrics), "Unclassified in Events",
    skill_type == paste0("RemainedUnclustered_", year_for_leiden_metrics), "Remained Unclustered",
    skill_type == paste0("LeidenC_Unknown_", year_for_leiden_metrics), "Unknown Leiden Cluster",
    default = as.character(skill_type) 
  )]
  defined_plot_labels <- c("Sociocognitive", "Physical/Sensorymotor", "Unclassified in Events", "Remained Unclustered", "Unknown Leiden Cluster")
  other_labels <- setdiff(unique(all_events_dt_for_diffusion_analysis$skill_type_plot_label), defined_plot_labels)
  all_possible_labels <- c(defined_plot_labels, other_labels)
  all_events_dt_for_diffusion_analysis[, skill_type_plot_label := factor(skill_type_plot_label, levels = all_possible_labels)]
  
  message("Distribution of 'skill_type_plot_label' for plotting:")
  print(all_events_dt_for_diffusion_analysis[,.N, by=skill_type_plot_label][order(-N)])
} else {
  warning("'skill_type' column not found in all_events_dt_for_diffusion_analysis. Plots might not have descriptive labels.")
  all_events_dt_for_diffusion_analysis[, skill_type_plot_label := factor("Unknown")] 
}


# =============================================================================
# SECTION B: DIFFUSION DYNAMICS (DEEP DIVE) - USES CORRECTED DATA
# =============================================================================
message("\n\n--- B. ANALYSIS OF DIFFUSION DYNAMICS (using Corrected 'skill_type') ---")

# B.1. Diffusion Rates with Confidence Intervals
message("\n--- B.1. Diffusion Rates (with Corrected skill_type) ---")
if ("skill_type_plot_label" %in% names(all_events_dt_for_diffusion_analysis)) {
  diffusion_summary <- all_events_dt_for_diffusion_analysis[, .(
    diffusion_rate = mean(diffusion),
    se = sd(diffusion) / sqrt(.N),
    n_events = .N,
    n_positive_diffusion = sum(diffusion == 1)
  ), by = .(skill_type = skill_type_plot_label)] 
  
  diffusion_summary[, `:=`(
    ci_lower = pmax(0, diffusion_rate - 1.96 * se), 
    ci_upper = pmin(1, diffusion_rate + 1.96 * se)  
  )]
  setorder(diffusion_summary, -diffusion_rate)
  
  message("\nDiffusion Rates by Skill Type (Corrected):")
  print(kable(diffusion_summary, digits=3, col.names = c("Skill Type", "Diffusion Rate", "Std. Error", "N Events", "N Positive", "CI Lower", "CI Upper"),
              caption = "Diffusion Rates by Corrected Skill Type") %>% 
          kable_styling(bootstrap_options = "striped", full_width = F))
  fwrite(diffusion_summary, file.path(output_dir_descriptives, "B1_diffusion_rates_corrected.csv"))
  
  if(nrow(diffusion_summary[n_events > 100]) > 0) { 
    p_diffusion_rates <- ggplot(diffusion_summary[n_events > 100],
                                aes(x = reorder(skill_type, -diffusion_rate),
                                    y = diffusion_rate, fill=skill_type)) +
      geom_col(width = 0.7, show.legend = FALSE, alpha=0.8) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                    width = 0.2, linewidth = 0.8, color="gray40") + 
      geom_text(aes(label = sprintf("%.1f%%\n(N=%s)", diffusion_rate * 100, scales::comma(n_events))),
                vjust = -0.5, size = 4, fontface = "plain", color="black") + 
      scale_y_continuous(labels = percent_format(accuracy=1),
                         limits = c(0, max(diffusion_summary$ci_upper, na.rm=TRUE) * 1.15), 
                         expand = expansion(mult = c(0, 0.05))) +
      scale_fill_manual(values = RColorBrewer::brewer.pal(max(3,uniqueN(diffusion_summary$skill_type)), "Set2"), 
                        name = "Skill Type:") + 
      labs(
        title = "Diffusion Rates by Skill Type", 
        subtitle = paste("Base Classification:", corrected_leiden_col, "| 95% Confidence Intervals"), 
        x = "Skill Type", 
        y = "Estimated Diffusion Rate" 
      ) + 
      theme_paper(base_size=16) + 
      theme(axis.text.x = element_text(angle = 35, hjust = 1, size=rel(0.95)), 
            axis.title.x = element_text(size=rel(1.1)),
            axis.title.y = element_text(size=rel(1.1)))
    
    print(p_diffusion_rates)
    ggsave(file.path(output_dir_descriptives, "B1_diffusion_rates_ci_CORREGIDO_ENG.png"),
           p_diffusion_rates, width = max(11, 3*nrow(diffusion_summary[n_events > 100])), height = 9, dpi = 300, bg = "white") 
  } else {
    message("Not enough data in corrected skill_type groups to plot diffusion rates.")
  }
} else {
  message("Column 'skill_type_plot_label' not found. Skipping B.1.")
}

# B.2. Distance Analysis - Violin Plots (Corrected)
message("\n--- B.2. Distance Distribution (with Corrected skill_type) ---")
dist_vars <- c("structural_distance", "education_diff_abs", "wage_diff_abs")
dist_labels_eng <- c("Structural\nDistance", "Educational\nDistance", "Wage\nDistance")

if (all(dist_vars %in% names(all_events_dt_for_diffusion_analysis))) {
  dist_data_long_corr <- all_events_dt_for_diffusion_analysis[, c("diffusion", dist_vars), with = FALSE]
  dist_data_long_corr <- melt(dist_data_long_corr, id.vars = "diffusion",
                              measure.vars = dist_vars,
                              variable.name = "distance_type",
                              value.name = "distance_value")
  dist_data_long_corr[, distance_label := factor(distance_type, levels = dist_vars, labels = dist_labels_eng)]
  dist_data_long_corr[, diffusion_label := factor(diffusion, levels = c(0, 1), labels = c("No Diffusion", "Diffusion"))]
  
  dist_stats_corr <- dist_data_long_corr[!is.na(distance_value) & is.finite(distance_value), .(
    mean = mean(distance_value, na.rm = TRUE),
    median = median(distance_value, na.rm = TRUE)
  ), by = .(distance_label, diffusion_label)]
  
  if(nrow(dist_data_long_corr[!is.na(distance_value)]) > 0){
    p_violin_dist_corr <- ggplot(dist_data_long_corr[!is.na(distance_value) & is.finite(distance_value)],
                                 aes(x = diffusion_label, y = distance_value, fill = diffusion_label)) +
      geom_violin(alpha = 0.7, scale = "width", trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + 
      geom_point(data = dist_stats_corr, aes(y = mean), shape = 23, size = 3.5, fill = "white", color = "black", position = position_nudge(x=0.05)) + 
      facet_wrap(~distance_label, scales = "free_y", nrow = 1) +
      scale_fill_manual(values = c("No Diffusion" = "#E41A1C", "Diffusion" = "#377EB8"), name="Diffusion Status:") + 
      labs(title = "Distance Distribution by Diffusion Status", 
           subtitle = "Violins with quartiles. Diamonds indicate means.", 
           y = "Distance Value", x = NULL) + 
      theme_paper(base_size = 16) + 
      theme(legend.position = "top", 
            strip.text = element_text(face = "bold", size = rel(1.2)), 
            axis.text.x = element_text(size=rel(1.1)),
            axis.title.y = element_text(size=rel(1.15)))
    
    print(p_violin_dist_corr)
    ggsave(file.path(output_dir_descriptives, "B2_distance_violin_plots_CORREGIDO_ENG.png"),
           p_violin_dist_corr, width = 16, height = 8.5, dpi = 300, bg = "white") 
  } else { message("No valid distance data for violin plots.")}
} else { message("One or more distance variables not found. Skipping B.2.")}


# B.3. Diffusion Curves by Quantiles (Smooth Style) - REFACTORED TO SINGLE FACETED PLOT
message("\n--- B.3. Diffusion Curves by Distance Quantiles (Corrected skill_type, Smooth Style) ---")
all_quantile_rates_list <- list()

for(dist_var_corr_idx in seq_along(dist_vars)) {
  dist_var_corr <- dist_vars[dist_var_corr_idx]
  dist_label_eng_current <- dist_labels_eng[dist_var_corr_idx] 
  
  if(dist_var_corr %in% names(all_events_dt_for_diffusion_analysis) && is.numeric(all_events_dt_for_diffusion_analysis[[dist_var_corr]])) {
    data_clean_corr <- all_events_dt_for_diffusion_analysis[!is.na(get(dist_var_corr)) & is.finite(get(dist_var_corr))]
    
    message(sprintf("Processing B.3 for %s: nrow(data_clean_corr) = %d", dist_var_corr, nrow(data_clean_corr)))
    if (nrow(data_clean_corr) > 0) {
      message(sprintf("Unique skill_type_plot_label in data_clean_corr for %s:", dist_var_corr))
      print(table(data_clean_corr$skill_type_plot_label, useNA = "ifany"))
    }
    
    if (nrow(data_clean_corr) < 1000 || uniqueN(na.omit(data_clean_corr$skill_type_plot_label)) < 1 ) { 
      message(paste("Insufficient data (N=",nrow(data_clean_corr),") or too few unique, non-NA skill types (N_unique=",uniqueN(na.omit(data_clean_corr$skill_type_plot_label)),") to plot quantiles for ", dist_var_corr, sep="")); next
    }
    
    num_quantiles <- 10 
    quantile_breaks <- tryCatch({
      unique(quantile(data_clean_corr[[dist_var_corr]], probs = seq(0, 1, by = 1/num_quantiles), na.rm = TRUE, type=7))
    }, error = function(e) { NULL })
    
    if (is.null(quantile_breaks) || length(quantile_breaks) < 2) {
      min_val <- min(data_clean_corr[[dist_var_corr]],na.rm=T); max_val <- max(data_clean_corr[[dist_var_corr]],na.rm=T)
      quantile_breaks <- unique(c(min_val - abs(min_val)*0.02 - 1e-6, max_val + abs(max_val)*0.02 + 1e-6 )) 
      if (length(quantile_breaks) < 2) { message(paste("Still not enough breaks for", dist_var_corr, ". Skipping.")); next }
    }
    
    data_clean_corr[, quantile_num := cut(get(dist_var_corr), breaks = quantile_breaks, include.lowest = TRUE, labels = FALSE)]
    
    quantile_rates_corr_temp <- data_clean_corr[!is.na(quantile_num) & !is.na(skill_type_plot_label), .( 
      tasa = mean(diffusion, na.rm=T),
      n = .N,
      median_dist_in_quantile = median(get(dist_var_corr), na.rm=T) 
    ), by = .(quantile_num, skill_type = skill_type_plot_label)] 
    
    message(sprintf("For %s, before N>30 filter, quantile_rates_corr_temp has %d rows.", dist_var_corr, nrow(quantile_rates_corr_temp)))
    quantile_rates_corr_temp <- quantile_rates_corr_temp[n > 30] 
    message(sprintf("For %s, AFTER N>30 filter, quantile_rates_corr_temp has %d rows.", dist_var_corr, nrow(quantile_rates_corr_temp)))
    setorder(quantile_rates_corr_temp, skill_type, quantile_num)
    
    if(nrow(quantile_rates_corr_temp) > 0){
      quantile_rates_corr_temp[, distance_metric_label := dist_label_eng_current] 
      all_quantile_rates_list[[dist_var_corr]] <- quantile_rates_corr_temp
    } else {message(paste("Not enough data after filtering by N>30 for", dist_var_corr))}
  } else {
    message(paste("Variable", dist_var_corr, "not found or not numeric. Skipping for B.3."))
  }
}

if(length(all_quantile_rates_list) > 0){
  all_quantile_rates_combined <- rbindlist(all_quantile_rates_list)
  all_quantile_rates_combined[, distance_metric_label := factor(distance_metric_label, levels = dist_labels_eng)] 
  
  present_skill_types <- levels(droplevels(all_quantile_rates_combined$skill_type)) 
  
  full_palette_labels <- c("Sociocognitive" = "#1B9E77", 
                           "Physical/Sensorymotor" = "#D95F02", 
                           "Unclassified in Events" = "#7570B3", 
                           "Remained Unclustered" = "#E7298A", 
                           "Unknown Cluster" = "grey50",
                           "LeidenC_1" = "#1B9E77", # Fallback if raw names persist
                           "LeidenC_2" = "#D95F02"  # Fallback if raw names persist
  ) 
  
  palette_quantiles_all <- full_palette_labels[names(full_palette_labels) %in% present_skill_types]
  
  missing_from_palette <- setdiff(present_skill_types, names(palette_quantiles_all))
  if(length(missing_from_palette) > 0){
    default_colors_missing <- colorRampPalette(RColorBrewer::brewer.pal(max(3,min(8,length(missing_from_palette))), "Set3"))(length(missing_from_palette))
    names(default_colors_missing) <- missing_from_palette
    palette_quantiles_all <- c(palette_quantiles_all, default_colors_missing)
  }
  
  all_quantile_rates_combined[, skill_type := factor(skill_type, levels = names(palette_quantiles_all))]
  
  
  p_quantile_curves_faceted <- ggplot(all_quantile_rates_combined, 
                                      aes(x = quantile_num, y = tasa, 
                                          color = skill_type, fill = skill_type, group = skill_type)) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linewidth = 1.3, show.legend = FALSE) + 
    geom_point(aes(size=n), alpha = 0.8) + 
    facet_wrap(~distance_metric_label, scales = "free_y", ncol = 1) + 
    scale_x_continuous(breaks = 1:num_quantiles, labels = paste0("D", 1:num_quantiles)) + 
    scale_y_continuous(labels = percent_format(accuracy=1), limits=c(0,NA), expand=expansion(mult=c(0,0.1))) +
    scale_color_manual(values = palette_quantiles_all, name = "Skill Cluster Type:", drop = FALSE) + 
    scale_fill_manual(values = palette_quantiles_all, guide="none", drop = FALSE) + 
    scale_size_continuous(name="N Events in Decile-Group:", range = c(3.5, 9), breaks=round(quantile(all_quantile_rates_combined$n, c(0.25,0.5,0.75), na.rm=T),0), guide = "none") + 
    labs(title = "Diffusion Rates by Distance Deciles and Skill Cluster", 
         #subtitle = paste("LOESS curves by skill cluster type. Points scaled by N events."), 
         x = "Deciles of Distance Variable", 
         y = "Estimated Diffusion Rate") + 
    theme_paper(base_size=16) + 
    theme(legend.position = "top", 
          axis.text.x=element_text(angle=0, hjust=0.5, size=rel(1.0)), 
          axis.title = element_text(size=rel(1.15)), 
          strip.text = element_text(size = rel(1.15)),
          legend.text = element_text(size=rel(0.95)), 
          legend.title = element_text(size=rel(1.0))) 
  
  print(p_quantile_curves_faceted)
  ggsave(file.path(output_dir_descriptives, "B3_quantile_diffusion_curves_SMOOTH_FACETED_ENG.png"), 
         plot = p_quantile_curves_faceted, width = 12, height = 5 + 4.5 * length(dist_vars), 
         dpi = 300, bg="white", limitsize = FALSE)
} else {
  message("No data generated for combined quantile diffusion plots.")
}


# B.4. Heatmap of Diffusion Flows and Socioeconomic Mobility (Corrected)
message("\n--- B.4. Heatmaps of Flows and Mobility (with Corrected skill_type) ---")

# Option 1: Diffusion Matrix between Skill Types (Corrected)
if ("skill_name" %in% names(all_events_dt_for_diffusion_analysis) && "skill_type_plot_label" %in% names(all_events_dt_for_diffusion_analysis)) { 
  message("\n--- B.4.1. Heatmap of Diffusion Flows between Skill Types ---")
  
  occ_skill_mapping_source <- all_events_dt_for_diffusion_analysis[, .(
    dominant_skill_type = if(.N > 0 && any(!is.na(skill_type_plot_label))) names(sort(table(na.omit(skill_type_plot_label)), decreasing = TRUE)[1]) else NA_character_
  ), by = source]
  occ_skill_mapping_target <- all_events_dt_for_diffusion_analysis[, .(
    dominant_skill_type = if(.N > 0 && any(!is.na(skill_type_plot_label))) names(sort(table(na.omit(skill_type_plot_label)), decreasing = TRUE)[1]) else NA_character_
  ), by = target]
  
  events_enhanced_corr <- merge(all_events_dt_for_diffusion_analysis, occ_skill_mapping_source, by = "source", all.x = TRUE)
  setnames(events_enhanced_corr, "dominant_skill_type", "source_dominant_skill_type")
  events_enhanced_corr <- merge(events_enhanced_corr, occ_skill_mapping_target, by = "target", all.x = TRUE)
  setnames(events_enhanced_corr, "dominant_skill_type", "target_dominant_skill_type")
  
  flow_matrix_corr <- events_enhanced_corr[!is.na(skill_type_plot_label) & !is.na(target_dominant_skill_type), .(
    diffusion_rate = mean(diffusion) * 100, 
    n_events = .N,
    net_diffusion_flows = sum(diffusion) 
  ), by = .(Origin_Skill_Type = skill_type_plot_label, Destination_Occ_Dominant_Skill_Type = target_dominant_skill_type)] # Use skill_type_plot_label
  
  flow_matrix_corr <- flow_matrix_corr[n_events >= 50] 
  
  if (nrow(flow_matrix_corr) > 0) {
    origen_order_corr <- flow_matrix_corr[, .(total_flow = sum(net_diffusion_flows)), by = Origin_Skill_Type][order(-total_flow), Origin_Skill_Type]
    destino_order_corr <- flow_matrix_corr[, .(total_flow = sum(net_diffusion_flows)), by = Destination_Occ_Dominant_Skill_Type][order(-total_flow), Destination_Occ_Dominant_Skill_Type]
    
    flow_matrix_corr[, Origin_Skill_Type := factor(Origin_Skill_Type, levels = origen_order_corr)]
    flow_matrix_corr[, Destination_Occ_Dominant_Skill_Type := factor(Destination_Occ_Dominant_Skill_Type, levels = destino_order_corr)]
    
    flow_matrix_corr[, label_heatmap := ifelse(n_events >= 50, sprintf("%.0f%%\n(N=%s)", diffusion_rate, scales::comma(n_events)), "")] 
    
    p_heatmap_corr <- ggplot(flow_matrix_corr, aes(x = Destination_Occ_Dominant_Skill_Type, y = Origin_Skill_Type, fill = diffusion_rate)) +
      geom_tile(color = "grey70", linewidth = 0.5) + 
      geom_text(aes(label = label_heatmap), size = 3.8, color = "black") + 
      scale_fill_gradient2(low = "#2C7BB6", mid = "#FFFFBF", high = "#D7191C", 
                           midpoint = median(flow_matrix_corr$diffusion_rate[flow_matrix_corr$diffusion_rate > 0 & flow_matrix_corr$n_events >=50], na.rm=T), 
                           limits = c(0, max(flow_matrix_corr$diffusion_rate, na.rm=T)), name = "Diffusion Rate\n(%)",
                           na.value = "grey80", labels = scales::percent_format(scale=1, accuracy=1)) +
      labs(title = "Diffusion Flow Matrix between Skill Types", 
           subtitle = "Skills diffused from their origin type to occupations dominated by destination type", 
           x = "Dominant Skill Type in Destination Occupation", y = "Skill Type (Origin)", 
           caption = paste("Only flows with N >= 50 events. Base classification:", corrected_leiden_col)) +
      theme_paper(base_size=14) + 
      theme(axis.text.x = element_text(angle = 40, hjust = 1, size=rel(1.0)), 
            axis.text.y = element_text(size=rel(1.0)), 
            panel.grid = element_blank(), 
            plot.title = element_text(size=rel(1.4))) + 
      coord_fixed()
    
    print(p_heatmap_corr)
    ggsave(file.path(output_dir_descriptives, "B4_flow_matrix_CORREGIDO_ENG.png"), p_heatmap_corr, width = 12, height = 9, dpi = 300, bg = "white") 
    fwrite(flow_matrix_corr, file.path(output_dir_descriptives, "B4_flow_matrix_data_CORREGIDO.csv"))
    
  } else { message("Not enough data for the diffusion flow heatmap.")}
}


# B.4.2. Socioeconomic Mobility Heatmap (Wages)
if (all(c("source_wage", "target_wage", "skill_type_plot_label") %in% names(all_events_dt_for_diffusion_analysis))) { 
  message("\n--- B.4.2. Socioeconomic Mobility Heatmap (Wages) ---")
  
  temp_events_for_ses_wage <- copy(all_events_dt_for_diffusion_analysis)
  
  valid_source_wages <- temp_events_for_ses_wage[!is.na(source_wage) & is.finite(source_wage), source_wage]
  valid_target_wages <- temp_events_for_ses_wage[!is.na(target_wage) & is.finite(target_wage), target_wage]
  
  if (length(valid_source_wages) > 100 && length(valid_target_wages) > 100 &&
      uniqueN(valid_source_wages) >= 4 && uniqueN(valid_target_wages) >= 4) { 
    
    source_wage_breaks <- unique(quantile(valid_source_wages, probs = seq(0, 1, 0.25), na.rm = TRUE, type=7))
    target_wage_breaks <- unique(quantile(valid_target_wages, probs = seq(0, 1, 0.25), na.rm = TRUE, type=7))
    
    if(length(source_wage_breaks) < 2) source_wage_breaks <- range(valid_source_wages, na.rm=T) + c(-1e-6, 1e-6) 
    if(length(target_wage_breaks) < 2) target_wage_breaks <- range(valid_target_wages, na.rm=T) + c(-1e-6, 1e-6)
    
    source_q_labels <- paste0("Q", 1:(length(source_wage_breaks)-1), " (Orig. Wage)") 
    target_q_labels <- paste0("Q", 1:(length(target_wage_breaks)-1), " (Dest. Wage)") 
    
    temp_events_for_ses_wage[!is.na(source_wage) & is.finite(source_wage), source_ses_wage_q := cut(source_wage,
                                                                                                    breaks = source_wage_breaks,
                                                                                                    labels = source_q_labels,
                                                                                                    include.lowest = TRUE, right = TRUE)]
    temp_events_for_ses_wage[!is.na(target_wage) & is.finite(target_wage), target_ses_wage_q := cut(target_wage,
                                                                                                    breaks = target_wage_breaks,
                                                                                                    labels = target_q_labels,
                                                                                                    include.lowest = TRUE, right = TRUE)]
    
    ses_matrix_wage_corr <- temp_events_for_ses_wage[!is.na(source_ses_wage_q) & !is.na(target_ses_wage_q), .(
      diffusion_rate_ses = mean(diffusion, na.rm=T) * 100, 
      n_ses = .N
    ), by = .(skill_type = skill_type_plot_label, source_ses_wage_q, target_ses_wage_q)] 
    
    ses_matrix_wage_corr <- ses_matrix_wage_corr[n_ses >= 30] 
    
    if(nrow(ses_matrix_wage_corr) > 0 && uniqueN(ses_matrix_wage_corr$skill_type) > 0) {
      n_cols_facet_ses <- determine_facet_ncols(uniqueN(ses_matrix_wage_corr$skill_type))
      
      p_ses_heatmap_wage_corr <- ggplot(ses_matrix_wage_corr, aes(x = target_ses_wage_q, y = source_ses_wage_q, fill = diffusion_rate_ses)) +
        geom_tile(color = "grey70", linewidth = 0.3) +
        geom_text(aes(label = sprintf("%.0f%%\n(N=%s)", diffusion_rate_ses, scales::comma(n_ses))), size = 3.2, color="black") + 
        facet_wrap(~skill_type, ncol = n_cols_facet_ses) + 
        scale_fill_viridis_c(name = "Diffusion Rate\n(%)", option = "plasma", limits = c(0, NA), na.value="grey70", labels = scales::percent_format(scale=1)) + 
        labs(title = "Socioeconomic Mobility in Skill Diffusion (Wages)", 
             #subtitle = paste("Base Classification:", corrected_leiden_col), 
             x = "Wage Quartile of Destination Occupation", y = "Wage Quartile of Source Occupation", 
             caption = "Only cells with N >= 30 events.") +
        theme_paper(base_size=14) + 
        theme(axis.text.x = element_text(angle = 40, hjust = 1, size=rel(0.9)),
              axis.text.y = element_text(size=rel(0.9)),
              strip.text = element_text(size=rel(1.05)), 
              panel.spacing = unit(1.3, "lines"),
              panel.grid = element_blank()) +
        coord_fixed() 
      
      print(p_ses_heatmap_wage_corr)
      height_ses_heatmap_wage <- 5 + 4 * ceiling(uniqueN(ses_matrix_wage_corr$skill_type) / n_cols_facet_ses) 
      ggsave(file.path(output_dir_descriptives, "B4_ses_mobility_heatmap_wages_CORREGIDO_ENG.png"),
             p_ses_heatmap_wage_corr, width = 14, height = height_ses_heatmap_wage, dpi = 300, bg = "white", limitsize=FALSE) 
      fwrite(ses_matrix_wage_corr, file.path(output_dir_descriptives, "B4_ses_mobility_data_wages_CORREGIDO.csv"))
      
    } else { message("Not enough data for the socioeconomic mobility (wages) heatmap after filtering.")}
  } else { message("Not enough valid wage data to create quantiles for the mobility (wages) heatmap.")}
} else { message("Wage columns 'source_wage' or 'target_wage' not found. Skipping socioeconomic mobility (wages) heatmap.")}

# B.4.3. Socioeconomic Mobility Heatmap (Education) - Ensure English labels
if (all(c("source_education", "target_education", "skill_type_plot_label") %in% names(all_events_dt_for_diffusion_analysis))) { 
  message("\n--- B.4.3. Socioeconomic Mobility Heatmap (Education) ---")
  
  temp_events_for_edu_ses <- copy(all_events_dt_for_diffusion_analysis)
  
  valid_source_edu <- temp_events_for_edu_ses[!is.na(source_education) & is.finite(source_education), source_education]
  valid_target_edu <- temp_events_for_edu_ses[!is.na(target_education) & is.finite(target_education), target_education]
  
  if (length(valid_source_edu) > 100 && length(valid_target_edu) > 100 &&
      uniqueN(valid_source_edu) >= 4 && uniqueN(valid_target_edu) >= 4) { 
    
    source_edu_breaks <- unique(quantile(valid_source_edu, probs = seq(0, 1, 0.25), na.rm = TRUE, type=7))
    target_edu_breaks <- unique(quantile(valid_target_edu, probs = seq(0, 1, 0.25), na.rm = TRUE, type=7))
    
    if(length(source_edu_breaks) < 2) source_edu_breaks <- range(valid_source_edu, na.rm=T) + c(-1e-6, 1e-6)
    if(length(target_edu_breaks) < 2) target_edu_breaks <- range(valid_target_edu, na.rm=T) + c(-1e-6, 1e-6)
    
    source_q_edu_labels <- paste0("Q", 1:(length(source_edu_breaks)-1), " (Orig. Educ)") 
    target_q_edu_labels <- paste0("Q", 1:(length(target_edu_breaks)-1), " (Dest. Educ)") 
    
    temp_events_for_edu_ses[!is.na(source_education) & is.finite(source_education), source_ses_edu_q := cut(source_education,
                                                                                                            breaks = source_edu_breaks,
                                                                                                            labels = source_q_edu_labels,
                                                                                                            include.lowest = TRUE, right = TRUE)]
    temp_events_for_edu_ses[!is.na(target_education) & is.finite(target_education), target_ses_edu_q := cut(target_education,
                                                                                                            breaks = target_edu_breaks,
                                                                                                            labels = target_q_edu_labels,
                                                                                                            include.lowest = TRUE, right = TRUE)]
    
    ses_matrix_edu_corr <- temp_events_for_edu_ses[!is.na(source_ses_edu_q) & !is.na(target_ses_edu_q), .(
      diffusion_rate_ses = mean(diffusion, na.rm=T) * 100, 
      n_ses = .N
    ), by = .(skill_type = skill_type_plot_label, source_ses_edu_q, target_ses_edu_q)] 
    
    ses_matrix_edu_corr <- ses_matrix_edu_corr[n_ses >= 30] 
    
    if(nrow(ses_matrix_edu_corr) > 0 && uniqueN(ses_matrix_edu_corr$skill_type) > 0) {
      n_cols_facet_ses_edu <- determine_facet_ncols(uniqueN(ses_matrix_edu_corr$skill_type))
      
      p_ses_heatmap_edu_corr <- ggplot(ses_matrix_edu_corr, aes(x = target_ses_edu_q, y = source_ses_edu_q, fill = diffusion_rate_ses)) +
        geom_tile(color = "grey70", linewidth = 0.3) +
        geom_text(aes(label = sprintf("%.0f%%\n(N=%s)", diffusion_rate_ses, scales::comma(n_ses))), size = 3.2, color="black") + 
        facet_wrap(~skill_type, ncol = n_cols_facet_ses_edu) + 
        scale_fill_viridis_c(name = "Diffusion Rate\n(%)", option = "viridis", limits = c(0, NA), na.value="grey70", labels = scales::percent_format(scale=1)) + 
        labs(title = "Socioeconomic Mobility in Skill Diffusion (Education)", 
             #subtitle = paste("Base Classification:", corrected_leiden_col), 
             x = "Education Quartile of Destination Occupation", y = "Education Quartile of Source Occupation", 
             caption = "Only cells with N >= 30 events.") +
        theme_paper(base_size=14) + 
        theme(axis.text.x = element_text(angle = 40, hjust = 1, size=rel(0.9)),
              axis.text.y = element_text(size=rel(0.9)),
              strip.text = element_text(size=rel(1.05)), 
              panel.spacing = unit(1.3, "lines"),
              panel.grid = element_blank()) +
        coord_fixed() 
      
      print(p_ses_heatmap_edu_corr)
      height_ses_heatmap_edu <- 5 + 4 * ceiling(uniqueN(ses_matrix_edu_corr$skill_type) / n_cols_facet_ses_edu) 
      ggsave(file.path(output_dir_descriptives, "B4_ses_mobility_heatmap_education_CORREGIDO_ENG.png"),
             p_ses_heatmap_edu_corr, width = 14, height = height_ses_heatmap_edu, dpi = 300, bg = "white", limitsize=FALSE) 
      fwrite(ses_matrix_edu_corr, file.path(output_dir_descriptives, "B4_ses_mobility_data_education_CORREGIDO.csv"))
    } else { message("Not enough data for the socioeconomic mobility (education) heatmap after filtering.")}
  } else { message("Not enough valid education data to create quantiles for the mobility (education) heatmap.")}
} else { message("Education columns 'source_education' or 'target_education' not found. Skipping socioeconomic mobility (education) heatmap.")}


# SECTION C: TEMPORAL ANALYSIS AND PATTERNS (Corrected)
message("\n--- C. TEMPORAL ANALYSIS AND PATTERNS (with Corrected skill_type) ---")
if ("year_emission" %in% names(all_events_dt_for_diffusion_analysis) && "year_adoption" %in% names(all_events_dt_for_diffusion_analysis)) {
  temporal_data_corr <- all_events_dt_for_diffusion_analysis[, .(
    time_diff = year_adoption - year_emission,
    diffusion,
    skill_type = skill_type_plot_label 
  )]
  temporal_summary_corr <- temporal_data_corr[time_diff > 0, .(
    diffusion_rate = mean(diffusion, na.rm=T), 
    n_events = .N 
  ), by = .(time_diff, skill_type)]
  
  if(nrow(temporal_summary_corr[n_events > 50]) > 0){ 
    
    current_skill_types_temporal <- levels(temporal_summary_corr[n_events > 50, skill_type]) 
    num_current_skill_types_temporal <- length(current_skill_types_temporal)
    palette_temporal <- if (num_current_skill_types_temporal <= 8 && num_current_skill_types_temporal > 0) {
      brewer.pal(max(3, num_current_skill_types_temporal), "Set2") 
    } else if (num_current_skill_types_temporal > 0) {
      colorRampPalette(brewer.pal(8, "Set2"))(num_current_skill_types_temporal)
    } else { c("black") }
    if(length(current_skill_types_temporal) > 0 && length(palette_temporal) >= num_current_skill_types_temporal) {
      names(palette_temporal) <- current_skill_types_temporal
    } else if (length(current_skill_types_temporal) > 0) {
      palette_temporal <- rep(palette_temporal, length.out = num_current_skill_types_temporal)
      names(palette_temporal) <- current_skill_types_temporal
    }
    
    p_temporal_corr <- ggplot(temporal_summary_corr[n_events > 50], aes(x = factor(time_diff), y = diffusion_rate, color = skill_type, group = skill_type)) +
      geom_line(linewidth = 1.3, alpha=0.85) + 
      geom_point(aes(size=n_events), alpha=0.7) +
      scale_y_continuous(labels = percent_format(accuracy=1), limits=c(0,NA), expand=expansion(mult=c(0,0.1))) +
      scale_color_manual(values = palette_temporal, name = "Skill Cluster Type:") + 
      scale_size_continuous(name="N Events:", range = c(2.5, 7), breaks=round(quantile(temporal_summary_corr[n_events>50,n_events], c(0.25,0.5,0.75), na.rm=T),0)) +
      labs(title = "Temporal Patterns of Diffusion", 
           subtitle = "Diffusion rate by time difference between skill emission and adoption", 
           x = "Years Between Skill Emission and Adoption", y = "Estimated Diffusion Rate") + 
      theme_paper(base_size=15) + 
      theme(legend.position = "top", 
            axis.title = element_text(size=rel(1.1)),
            axis.text = element_text(size=rel(1.0)))
    
    print(p_temporal_corr)
    ggsave(file.path(output_dir_descriptives, "C1_temporal_patterns_CORREGIDO_ENG.png"), p_temporal_corr, width = 12, height = 8, dpi = 300, bg = "white") 
    fwrite(temporal_summary_corr, file.path(output_dir_descriptives, "C1_temporal_summary_data_CORREGIDO.csv"))
  } else { message("Not enough data for the temporal patterns plot.")}
} else { message("Columns 'year_emission' or 'year_adoption' not found. Skipping temporal analysis.")}





# =============================================================================
# SECTION C: TEMPORAL ANALYSIS AND PATTERNS (Corrected)
# =============================================================================
message("\n--- C. TEMPORAL ANALYSIS AND PATTERNS (with Corrected skill_type) ---")
if ("year_emission" %in% names(all_events_dt_for_diffusion_analysis) && "year_adoption" %in% names(all_events_dt_for_diffusion_analysis)) {
  temporal_data_corr <- all_events_dt_for_diffusion_analysis[, .(
    time_diff = year_adoption - year_emission,
    diffusion,
    skill_type = skill_type_factor 
  )]
  temporal_summary_corr <- temporal_data_corr[time_diff > 0, .(
    diffusion_rate = mean(diffusion, na.rm=T), 
    n_events = .N 
  ), by = .(time_diff, skill_type)]
  
  if(nrow(temporal_summary_corr[n_events > 50]) > 0){ 
    
    current_skill_types_temporal <- levels(temporal_summary_corr[n_events > 50, skill_type]) 
    num_current_skill_types_temporal <- length(current_skill_types_temporal)
    palette_temporal <- if (num_current_skill_types_temporal <= 8 && num_current_skill_types_temporal > 0) {
      brewer.pal(max(3, num_current_skill_types_temporal), "Set2") 
    } else if (num_current_skill_types_temporal > 0) {
      colorRampPalette(brewer.pal(8, "Set2"))(num_current_skill_types_temporal)
    } else { c("black") }
    if(length(current_skill_types_temporal) > 0 && length(palette_temporal) >= num_current_skill_types_temporal) {
      names(palette_temporal) <- current_skill_types_temporal
    } else if (length(current_skill_types_temporal) > 0) {
      palette_temporal <- rep(palette_temporal, length.out = num_current_skill_types_temporal)
      names(palette_temporal) <- current_skill_types_temporal
    }
    
    p_temporal_corr <- ggplot(temporal_summary_corr[n_events > 50], aes(x = factor(time_diff), y = diffusion_rate, color = skill_type, group = skill_type)) +
      geom_line(linewidth = 1.3, alpha=0.85) + 
      geom_point(aes(size=n_events), alpha=0.7) +
      scale_y_continuous(labels = percent_format(accuracy=1), limits=c(0,NA), expand=expansion(mult=c(0,0.1))) +
      scale_color_manual(values = palette_temporal, name = "Skill Type (Corrected):") +
      scale_size_continuous(name="N Events:", range = c(2.5, 7), breaks=round(quantile(temporal_summary_corr[n_events>50,n_events], c(0.25,0.5,0.75), na.rm=T),0)) +
      labs(title = "Temporal Patterns of Diffusion (Corrected)",
           subtitle = "Diffusion rate by time difference between skill emission and adoption",
           x = "Years Between Skill Emission and Adoption", y = "Estimated Diffusion Rate") +
      theme_paper(base_size=14) + 
      theme(legend.position = "top", 
            axis.title = element_text(size=rel(1.05)),
            axis.text = element_text(size=rel(0.95)))
    
    print(p_temporal_corr)
    ggsave(file.path(output_dir_descriptives, "C1_temporal_patterns_CORREGIDO_ENG.png"), p_temporal_corr, width = 11, height = 7.5, dpi = 300, bg = "white") 
    fwrite(temporal_summary_corr, file.path(output_dir_descriptives, "C1_temporal_summary_data_CORREGIDO.csv"))
  } else { message("Not enough data for the temporal patterns plot with the corrected classification.")}
} else { message("Columns 'year_emission' or 'year_adoption' not found. Skipping temporal analysis.")}



# D.2. Correlations between distances
if (length(dist_vars) > 1 && all(dist_vars %in% names(all_events_dt_for_diffusion_analysis))) {
  cor_data_corr <- all_events_dt_for_diffusion_analysis[, ..dist_vars] 
  if(nrow(na.omit(cor_data_corr)) > 10 && ncol(cor_data_corr) > 1) {
    cor_matrix_corr <- cor(cor_data_corr, use = "pairwise.complete.obs")
    message("\nD.2. Correlation Matrix between Distance Variables:")
    print(kable(round(cor_matrix_corr,2), caption="Correlation Matrix between Distance Variables",
                col.names = gsub("_", " ", gsub("abs", "(Abs)", gsub("diff", "Diff.", dist_vars))) ) %>% # Nicer names
            kable_styling(bootstrap_options = "condensed", full_width = FALSE))
    
    cor_long_corr <- melt(as.data.table(cor_matrix_corr, keep.rownames="Var1"), id.vars="Var1", variable.name="Var2", value.name="value")
    # Use dist_labels_eng for factor levels for correct ordering and naming in plot
    cor_long_corr$Var1 <- factor(cor_long_corr$Var1, levels = dist_vars, labels = dist_labels_eng) 
    cor_long_corr$Var2 <- factor(cor_long_corr$Var2, levels = dist_vars, labels = dist_labels_eng)
    
    
    p_cor_corr <- ggplot(cor_long_corr, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", value)), size = 4.5, fontface = "bold", color="grey10") + 
      scale_fill_gradient2(low = "#0571B0", mid = "white", high = "#CA0020", midpoint = 0, limits = c(-1, 1), name = "Correlation\nCoefficient") +
      coord_fixed() + 
      labs(title = "Correlation Matrix between Distance Variables", x = NULL, y = NULL) +
      theme_paper(base_size=13) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=rel(1.0)), 
            axis.text.y = element_text(size=rel(1.0)), 
            legend.position="right",
            panel.grid = element_blank(), axis.line = element_blank()) 
    print(p_cor_corr)
    ggsave(file.path(output_dir_descriptives, "D2_correlation_matrix_CORREGIDO_ENG.png"), p_cor_corr, width = 9, height = 8, dpi = 300, bg = "white") 
    fwrite(as.data.table(cor_matrix_corr, keep.rownames="Variable"), file.path(output_dir_descriptives, "D2_correlation_matrix_data_CORREGIDO.csv"))
  } else { message("Not enough data to calculate the correlation matrix.")}
} else { message("One or more distance variables not found for correlation matrix.")}


# =============================================================================
# FINAL SUMMARY
# =============================================================================
message("\n\n=== ADVANCED DESCRIPTIVE ANALYSIS (LEIDEN 2015 CORRECTED) COMPLETED ===")
message(paste("Results saved in:", normalizePath(output_dir_descriptives)))
if (exists("corrected_events_rds_path")) {
  message(sprintf("\nKey RDS file for PART 2 (Models) or subsequent analyses: %s", normalizePath(corrected_events_rds_path)))
}

gc()
glimpse(all_events_final_with_cps_data)
