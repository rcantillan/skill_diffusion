know###############################################################################
# Required libraries
###############################################################################
library(data.table)
library(tidyverse)
library(progress)
library(ggplot2)
library(patchwork)
library(viridis)


###############################################################################
# Data loading
###############################################################################

list.files("C:/Users/qramo/Downloads/db_29_2_text/db_29_2_text")

# Cargar los archivos principales
skills <- read.delim("C:/Users/qramo/Downloads/db_29_2_text/db_29_2_text/Skills.txt", sep="\t")
abilities <- read.delim("C:/Users/qramo/Downloads/db_29_2_text/db_29_2_text/Abilities.txt", sep="\t")
knowledge <- read.delim("C:/Users/qramo/Downloads/db_29_2_text/db_29_2_text/Knowledge.txt", sep="\t")
work_activities <- read.delim("C:/Users/qramo/Downloads/db_29_2_text/db_29_2_text/Work Activities.txt", sep="\t")
work_styles <- read.delim("C:/Users/qramo/Downloads/db_29_2_text/db_29_2_text/Work Styles.txt", sep="\t")

# También necesitarás los datos de ocupaciones
occupation_data <- read.delim("C:/Users/qramo/Downloads/db_29_2_text/db_29_2_text/Occupation Data.txt", sep="\t")

# Review
glimpse(skills)
glimpse(abilities)
glimpse(knowledge)
glimpse(work_activities)
glimpse(work_styles)
glimpse(occupation_data)

###############################################################################
# Function to create skill diffusion dataset
###############################################################################
create_skill_diffusion_network <- function(skills_data, occupation_data = NULL, batch_size = 100) {
  message("Preparing skills data...")
  
  # Convert to data.table
  dt <- as.data.table(skills_data)
  
  # Filter only level (LV) data
  dt <- dt[Scale.ID == "LV"]
  
  # Extract year from Date column
  message("Extracting year from Date column...")
  dt[, year := as.numeric(NA)]
  
  # Try different date formats
  if (any(grepl("/", dt$Date)) || any(grepl("-", dt$Date))) {
    message("Detected date format with separators...")
    dt[, year := as.numeric(format(as.Date(dt$Date, format = "%m/%d/%Y", optional = TRUE), "%Y"))]
  }
  
  # If still NAs, try extracting 4 consecutive digits
  if (all(is.na(dt$year))) {
    message("Trying to extract 4 consecutive digits as years...")
    dt[, year := as.numeric(stringr::str_extract(dt$Date, "\\d{4}"))]
  }
  
  # If still NAs, try extracting first 4 characters
  if (all(is.na(dt$year))) {
    message("Trying to extract first 4 characters as years...")
    dt[, year := as.numeric(substr(dt$Date, 1, 4))]
  }
  
  # Check if valid years were extracted
  valid_years <- dt[!is.na(year), unique(year)]
  message(sprintf("Extracted years: %s", paste(valid_years, collapse = ", ")))
  
  if (length(valid_years) == 0) {
    stop("Could not extract valid years from the Date column.")
  }
  
  # Filter rows with NA in key columns
  dt <- dt[
    !is.na(O.NET.SOC.Code) & !is.na(Element.Name) & 
      !is.na(year) & !is.na(Data.Value)
  ]
  
  # Check if there's enough data
  if (nrow(dt) == 0) {
    stop("Not enough data after filtering NA values.")
  }
  
  # Define primary key
  setkey(dt, O.NET.SOC.Code, Element.Name, year)
  
  # Calculate RCA for each occupation-skill-year combination
  message("Calculating RCA...")
  
  # For each year, calculate RCA
  years <- sort(unique(dt$year))
  
  # Check if there are at least 2 years for diffusion analysis
  if (length(years) < 2) {
    stop(sprintf("At least 2 years needed for diffusion analysis. Years found: %s", 
                 paste(years, collapse = ", ")))
  }
  
  # List to store RCA results
  rca_results <- vector("list", length(years))
  
  for (i in seq_along(years)) {
    year_data <- dt[year == years[i]]
    
    # Calculate necessary sums for RCA
    occupation_sums <- year_data[, .(occ_sum = sum(Data.Value)), by = O.NET.SOC.Code]
    skill_sums <- year_data[, .(skill_sum = sum(Data.Value)), by = Element.Name]
    total_sum <- sum(year_data$Data.Value)
    
    # Calculate RCA
    year_data <- merge(year_data, occupation_sums, by = "O.NET.SOC.Code")
    year_data <- merge(year_data, skill_sums, by = "Element.Name")
    
    year_data[, rca := (Data.Value / occ_sum) / (skill_sum / total_sum)]
    
    # Identify effectively used skills (RCA > 1)
    year_data[, effective_use := rca > 1]
    
    rca_results[[i]] <- year_data
  }
  
  # Combine all RCA results
  dt_rca <- rbindlist(rca_results)
  
  message("\nIdentifying skills with diffusion between occupations...")
  
  # Progress bar for years
  pb_years <- progress_bar$new(
    format = "Years [:bar] :percent | :current/:total | ETA: :eta",
    total = length(years) - 1
  )
  
  # Find skills with diffusion (in any year)
  skills_diffusion <- rbindlist(lapply(seq_len(length(years)-1), function(i) {
    year_origin <- years[i]
    year_adoption <- years[i+1]
    
    # Filter dt_rca for those 2 years and only effectively used skills
    result <- dt_rca[year %in% c(year_origin, year_adoption) & effective_use == TRUE, {
      occs_origin <- unique(O.NET.SOC.Code[year == year_origin])
      occs_destination <- unique(O.NET.SOC.Code[year == year_adoption])
      has_diffusion <- length(occs_origin) > 0 &&
        !all(occs_destination %in% occs_origin)
      .(has_diffusion = has_diffusion)
    }, by = Element.Name][has_diffusion == TRUE, .(Element.Name)]
    
    pb_years$tick()
    result
  }))
  
  # Final list of diffusing skills
  diffusion_skills <- unique(skills_diffusion$Element.Name)
  message(sprintf("\nTotal skills showing diffusion: %d", length(diffusion_skills)))
  
  if (length(diffusion_skills) == 0) {
    message("No skills with diffusion.")
    return(NULL)
  }
  
  # Split skills list into batches
  batches <- split(diffusion_skills, ceiling(seq_along(diffusion_skills)/batch_size))
  n_batches <- length(batches)
  
  message(sprintf("\nProcessing %d skills in %d batches", length(diffusion_skills), n_batches))
  
  pb_batches <- progress_bar$new(
    format = "Batches [:bar] :percent | Batch :current/:total | ETA: :eta",
    total = n_batches
  )
  
  # Results list
  all_results <- vector("list", n_batches)
  
  for (i in seq_len(n_batches)) {
    # Filter dt_rca for batch skills and only effective_use = TRUE
    dt_batch <- dt_rca[Element.Name %in% batches[[i]] & effective_use == TRUE]
    
    # Identify first global appearance of each skill
    first_appearances <- dt_batch[
      , .(
        first_year = min(year),
        first_occ = O.NET.SOC.Code[which.min(year)],
        first_value = Data.Value[which.min(year)]
      ),
      by = Element.Name
    ]
    
    # Progress bar for years
    pb_batch_years <- progress_bar$new(
      format = "  Batch years [:bar] :percent | :current/:total",
      total = length(years) - 1,
      clear = FALSE,
      width = 60
    )
    
    batch_results_year <- vector("list", length(years) - 1)
    
    for (idx_year in seq_len(length(years) - 1)) {
      year_prev <- years[idx_year]      # Emission year
      year_curr <- years[idx_year + 1]  # Adoption year
      
      data_prev <- dt_batch[year == year_prev]
      data_curr <- dt_batch[year == year_curr]
      
      if (nrow(data_prev) == 0 || nrow(data_curr) == 0) {
        pb_batch_years$tick()
        next
      }
      
      # For each skill in data_prev
      diffusion_by_year <- rbindlist(lapply(unique(data_prev$Element.Name), function(skill) {
        
        datos_prev <- data_prev[Element.Name == skill,
                                .(O.NET.SOC.Code, rca_source = rca, data_value_source = Data.Value)]
        
        datos_curr <- data_curr[Element.Name == skill,
                                .(O.NET.SOC.Code, rca_target = rca, data_value_target = Data.Value)]
        
        occs_prev <- unique(datos_prev$O.NET.SOC.Code)
        occs_curr <- unique(datos_curr$O.NET.SOC.Code)
        
        # Occupations that adopted the skill in the current year
        occs_nuevas <- setdiff(occs_curr, occs_prev)
        if (length(occs_nuevas) == 0) return(NULL)
        
        # Create connections between occupations that had the skill and those that adopted it
        edges <- data.table::CJ(source = occs_prev, target = occs_nuevas, 
                                allow.cartesian=TRUE)
        
        # Save both emission and adoption years
        edges[, `:=`(
          Element.Name = skill, 
          year_emission = year_prev,
          year_adoption = year_curr
        )]
        
        # Merge with rca_source and data_value_source
        edges <- merge(edges, datos_prev, 
                       by.x = "source", by.y = "O.NET.SOC.Code")
        # Merge with rca_target and data_value_target
        edges <- merge(edges, datos_curr, 
                       by.x = "target", by.y = "O.NET.SOC.Code")
        
        # Connection weight based on RCA
        edges[, weight := sqrt(rca_source * rca_target)]
        
        edges
      }))
      
      pb_batch_years$tick()
      batch_results_year[[idx_year]] <- diffusion_by_year
    }
    
    # Combine batch results
    batch_results <- rbindlist(batch_results_year, use.names = TRUE, fill = TRUE)
    if (is.null(batch_results) || nrow(batch_results) == 0) {
      pb_batches$tick()
      all_results[[i]] <- NULL
      next
    }
    
    # Merge with first appearances
    combined <- merge(batch_results, first_appearances, 
                      by = "Element.Name", all.x = TRUE)
    combined <- combined[year_adoption >= first_year]
    
    # Calculate diffusion metrics
    local_res <- combined[
      , .(
        # Number of diffusion connections
        diffusion_count = .N,
        
        # Weighted sum of diffusions
        diffusion_weighted_sum = sum(weight, na.rm = TRUE),
        
        # Weighted mean of diffusions
        diffusion_weighted_mean = mean(weight, na.rm = TRUE),
        
        # Number of diffusions from original occupation
        origin_diffusion_count = sum(first_occ == source),
        
        # List of diffused skills
        skills_diffused = paste(unique(Element.Name), collapse = "|"),
        
        # Save emission and adoption years
        year_emission = first(year_emission),
        year_adoption = first(year_adoption),
        
        # Average RCA and importance values
        avg_rca_source = mean(rca_source, na.rm = TRUE),
        avg_rca_target = mean(rca_target, na.rm = TRUE),
        avg_data_value_source = mean(data_value_source, na.rm = TRUE),
        avg_data_value_target = mean(data_value_target, na.rm = TRUE),
        
        # Maximum RCA values
        max_rca_source = max(rca_source, na.rm = TRUE),
        max_rca_target = max(rca_target, na.rm = TRUE)
      ),
      by = .(source, target)
    ]
    
    all_results[[i]] <- local_res
    pb_batches$tick()
    
    rm(dt_batch, batch_results, batch_results_year, local_res)
    gc()
  }
  
  # Unify all results
  message("\nCombining final results...")
  final_results <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
  
  if (is.null(final_results) || nrow(final_results) == 0) {
    message("No results generated in diffusion.")
    return(NULL)
  }
  
  # Aggregate results by occupation pairs
  final_results <- final_results[
    , .(
      diffusion_count = sum(diffusion_count, na.rm = TRUE),
      diffusion_weighted_sum = sum(diffusion_weighted_sum, na.rm = TRUE),
      diffusion_weighted_mean = sum(diffusion_weighted_sum, na.rm = TRUE) / sum(diffusion_count, na.rm = TRUE),
      origin_diffusion_count = sum(origin_diffusion_count, na.rm = TRUE),
      skills_diffused = paste(unique(unlist(strsplit(skills_diffused, "\\|"))), collapse = "|"),
      year_emission = min(year_emission),  # First emission year
      year_adoption = max(year_adoption),  # Last adoption year
      avg_rca_source = mean(avg_rca_source, na.rm = TRUE),
      avg_rca_target = mean(avg_rca_target, na.rm = TRUE),
      avg_data_value_source = mean(avg_data_value_source, na.rm = TRUE),
      avg_data_value_target = mean(avg_data_value_target, na.rm = TRUE),
      max_rca_source = max(max_rca_source, na.rm = TRUE),
      max_rca_target = max(max_rca_target, na.rm = TRUE)
    ),
    by = .(source, target)
  ]
  
  # Replace possible NA with 0
  num_cols <- c(
    "diffusion_count", "diffusion_weighted_sum", "diffusion_weighted_mean", "origin_diffusion_count",
    "avg_rca_source", "avg_rca_target", "avg_data_value_source", "avg_data_value_target",
    "max_rca_source", "max_rca_target"
  )
  for (col_ in num_cols) {
    final_results[is.na(get(col_)), (col_) := 0]
  }
  
  # Add node and dyad attributes
  message("\nEnriching dataset with additional attributes...")
  
  # Extract unique occupations
  unique_occs <- unique(c(final_results$source, final_results$target))
  
  # Create dataframe of occupation attributes by year
  occupation_attributes_by_year <- data.table()
  
  # Process occupation data
  if (!is.null(occupation_data)) {
    # Convert to data.table if not already
    occ_data <- as.data.table(occupation_data)
    
    # Check if occupation data has year information
    if ("year" %in% names(occ_data)) {
      message("Using occupation data with temporal information...")
      
      # Check required columns
      req_cols <- c("O.NET.SOC.Code", "year", "education_level", "median_wage")
      missing_cols <- setdiff(req_cols, names(occ_data))
      
      if (length(missing_cols) > 0) {
        message(sprintf("Missing columns in occupation data: %s", 
                        paste(missing_cols, collapse = ", ")))
        
        # Add missing columns with simulated values
        if ("education_level" %in% missing_cols) {
          occ_data[, education_level := sample(1:5, .N, replace = TRUE)]
        }
        if ("median_wage" %in% missing_cols) {
          occ_data[, median_wage := runif(.N, 30000, 120000)]
        }
      }
      
      # Filter only relevant years
      all_years <- unique(c(final_results$year_emission, final_results$year_adoption))
      occupation_attributes_by_year <- occ_data[year %in% all_years]
      
    } else {
      message("Occupation data doesn't have temporal information. Simulating historical changes...")
      
      # Check required columns
      if (!"education_level" %in% names(occ_data)) {
        occ_data[, education_level := sample(1:5, .N, replace = TRUE)]
      }
      if (!"median_wage" %in% names(occ_data)) {
        occ_data[, median_wage := runif(.N, 30000, 120000)]
      }
      
      # Simulate data for each relevant year
      all_years <- unique(c(final_results$year_emission, final_results$year_adoption))
      
      for (y in all_years) {
        # Simulate changes in salaries (e.g., 2% reduction per year back from most recent year)
        year_factor <- 1 - 0.02 * (max(all_years) - y)
        
        year_attributes <- copy(occ_data)
        year_attributes[, `:=`(
          year = y,
          median_wage = median_wage * year_factor
          # Note: education_level generally doesn't change much over time
        )]
        
        occupation_attributes_by_year <- rbindlist(list(
          occupation_attributes_by_year,
          year_attributes[, .(O.NET.SOC.Code, year, education_level, median_wage)]
        ))
      }
    }
  } else {
    message("No occupation data provided. Creating simulated data...")
    
    # Create simulated data for each occupation and year
    all_years <- unique(c(final_results$year_emission, final_results$year_adoption))
    
    set.seed(123)  # For reproducibility
    
    # Base occupation database (constant values)
    base_occs <- data.table(
      O.NET.SOC.Code = unique_occs,
      base_education = sample(1:5, length(unique_occs), replace = TRUE),
      base_wage = runif(length(unique_occs), 30000, 120000)
    )
    
    # For each year, create a version with variations
    for (y in all_years) {
      # Adjustment factor by year (lower salaries in the past)
      year_factor <- 1 - 0.02 * (max(all_years) - y)
      
      year_attributes <- copy(base_occs)
      year_attributes[, `:=`(
        year = y,
        education_level = base_education,  # Education remains constant
        median_wage = base_wage * year_factor  # Salaries vary with time
      )]
      
      occupation_attributes_by_year <- rbindlist(list(
        occupation_attributes_by_year,
        year_attributes[, .(O.NET.SOC.Code, year, education_level, median_wage)]
      ))
    }
  }
  
  # Check that we have data for all necessary years
  missing_years <- setdiff(
    unique(c(final_results$year_emission, final_results$year_adoption)),
    unique(occupation_attributes_by_year$year)
  )
  
  if (length(missing_years) > 0) {
    message(sprintf("Missing occupation data for years: %s. Using approximations...", 
                    paste(missing_years, collapse = ", ")))
    
    # For each missing year, use the closest available year
    for (missing_year in missing_years) {
      available_years <- unique(occupation_attributes_by_year$year)
      closest_year <- available_years[which.min(abs(available_years - missing_year))]
      
      message(sprintf("  Using data from year %d to approximate %d", closest_year, missing_year))
      
      year_approx <- copy(occupation_attributes_by_year[year == closest_year])
      year_approx[, year := missing_year]
      
      # Adjust salaries based on year difference
      year_factor <- 1 - 0.02 * (missing_year - closest_year)
      year_approx[, median_wage := median_wage * year_factor]
      
      occupation_attributes_by_year <- rbindlist(list(
        occupation_attributes_by_year,
        year_approx
      ))
    }
  }
  
  # Add attributes to source occupations (using emission year)
  message("Adding source occupation attributes (emission year)...")
  final_results <- merge(
    final_results,
    occupation_attributes_by_year[, .(O.NET.SOC.Code, year, education_level, median_wage)],
    by.x = c("source", "year_emission"),
    by.y = c("O.NET.SOC.Code", "year"),
    all.x = TRUE
  )
  setnames(final_results, 
           c("education_level", "median_wage"), 
           c("source_education", "source_wage"))
  
  # Add attributes to target occupations (using adoption year)
  message("Adding target occupation attributes (adoption year)...")
  final_results <- merge(
    final_results,
    occupation_attributes_by_year[, .(O.NET.SOC.Code, year, education_level, median_wage)],
    by.x = c("target", "year_adoption"),
    by.y = c("O.NET.SOC.Code", "year"),
    all.x = TRUE
  )
  setnames(final_results, 
           c("education_level", "median_wage"), 
           c("target_education", "target_wage"))
  
  # Calculate dyad attributes
  message("Calculating distances between occupations...")
  final_results[, `:=`(
    # Educational distance (with sign - positive means upward diffusion)
    education_distance_signed = target_education - source_education,
    # Educational distance (absolute)
    education_distance = abs(target_education - source_education),
    
    # Wage distance (with sign - positive means upward diffusion)
    wage_distance_signed = target_wage - source_wage,
    # Wage distance (absolute)
    wage_distance = abs(target_wage - source_wage),
    
    # Diffusion direction (upward/downward in terms of salary)
    diffusion_direction = case_when(
      source_wage < target_wage ~ "upward",
      source_wage > target_wage ~ "downward",
      TRUE ~ "lateral"
    ),
    
    # Educational direction
    education_direction = case_when(
      source_education < target_education ~ "upward",
      source_education > target_education ~ "downward",
      TRUE ~ "lateral"
    ),
    
    # Diffusion time (years between emission and adoption)
    diffusion_time = year_adoption - year_emission
  )]
  
  # Handle possible NA in distances
  final_results[is.na(education_distance), education_distance := 0]
  final_results[is.na(wage_distance), wage_distance := 0]
  final_results[is.na(education_distance_signed), education_distance_signed := 0]
  final_results[is.na(wage_distance_signed), wage_distance_signed := 0]
  
  # Summary
  message("\nFinal dataset summary:")
  print(final_results[
    , .(
      total_edges = .N,
      mean_diffusion = mean(diffusion_count),
      total_skills_diffused = length(unique(unlist(strsplit(skills_diffused, "\\|")))),
      upward_diffusion_pct = 100 * sum(diffusion_direction == "upward", na.rm = TRUE) / .N,
      downward_diffusion_pct = 100 * sum(diffusion_direction == "downward", na.rm = TRUE) / .N,
      lateral_diffusion_pct = 100 * sum(diffusion_direction == "lateral", na.rm = TRUE) / .N,
      upward_education_pct = 100 * sum(education_direction == "upward", na.rm = TRUE) / .N,
      downward_education_pct = 100 * sum(education_direction == "downward", na.rm = TRUE) / .N,
      lateral_education_pct = 100 * sum(education_direction == "lateral", na.rm = TRUE) / .N,
      avg_education_distance = mean(education_distance, na.rm = TRUE),
      avg_wage_distance = mean(wage_distance, na.rm = TRUE),
      avg_education_distance_signed = mean(education_distance_signed, na.rm = TRUE),
      avg_wage_distance_signed = mean(wage_distance_signed, na.rm = TRUE),
      avg_diffusion_time = mean(diffusion_time, na.rm = TRUE),
      avg_rca_source = mean(avg_rca_source, na.rm = TRUE),
      avg_rca_target = mean(avg_rca_target, na.rm = TRUE)
    )
  ])
  
  return(final_results)
}

###############################################################################
# Function to create and analyze regression model
###############################################################################
analyze_diffusion_model <- function(diffusion_data) {
  # Ensure data is properly formatted
  dt <- as.data.table(diffusion_data)
  
  # Round distance values for better visualization
  dt[, `:=`(
    education_distance_signed = round(education_distance_signed, 0),
    wage_distance_signed = round(wage_distance_signed, 0)
  )]
  
  # Create a poisson regression model with quadratic terms
  message("Fitting regression model...")
  model <- glm(diffusion_count ~ education_distance_signed +
                 wage_distance_signed +
                 I(education_distance_signed^2) +
                 I(wage_distance_signed^2) +
                 factor(year_adoption),
               data = dt,
               family = poisson(link = "log"))
  
  # Print model summary
  message("\nRegression model summary:")
  print(summary(model))
  
  # Extract coefficients
  coefs <- coef(model)
  
  # Calculate critical points
  critical_edu <- -coefs["education_distance_signed"] / (2 * coefs["I(education_distance_signed^2)"])
  critical_wage <- -coefs["wage_distance_signed"] / (2 * coefs["I(wage_distance_signed^2)"])
  
  message(sprintf("\nCritical points: Educational distance = %.2f, Wage distance = %.2f",
                  critical_edu, critical_wage/1000))
  
  return(list(
    model = model,
    critical_points = list(
      education = critical_edu,
      wage = critical_wage
    )
  ))
}

###############################################################################
# Function to visualize regression effects - FIXED to avoid factor level error
###############################################################################
visualize_model_effects <- function(model, data) {
  # Extract coefficients
  coefs <- coef(model)
  
  # Coefficients for educational distance
  coef_education <- coefs["education_distance_signed"]
  coef_education_sq <- coefs["I(education_distance_signed^2)"]
  
  # Coefficients for wage distance
  coef_wage <- coefs["wage_distance_signed"]
  coef_wage_sq <- coefs["I(wage_distance_signed^2)"]
  
  # Calculate critical points
  critical_edu <- -coef_education / (2 * coef_education_sq)
  critical_wage <- -coef_wage / (2 * coef_wage_sq)
  
  # Get range of values for variables
  edu_range <- seq(min(data$education_distance_signed, na.rm = TRUE),
                   max(data$education_distance_signed, na.rm = TRUE),
                   length.out = 100)
  
  wage_range <- seq(min(data$wage_distance_signed, na.rm = TRUE),
                    max(data$wage_distance_signed, na.rm = TRUE),
                    length.out = 100)
  
  # Create plots for each effect
  
  # Get actual years from the data (not calculated values like median)
  # Extract the years that were actually used in the model
  actual_years <- sort(unique(data$year_adoption))
  
  # Select a subset of years (e.g., every 5 years or at most 5 years total)
  years_step <- max(1, ceiling(length(actual_years) / 5))
  plot_years <- actual_years[seq(1, length(actual_years), by = years_step)]
  
  message(paste("Creating visualization for years:", paste(plot_years, collapse=", ")))
  
  # 1. Effect of educational distance by year
  message("Creating educational distance effect plots by year...")
  edu_plots <- list()
  
  for (yr in plot_years) {
    # Create prediction data for this year
    edu_df <- data.frame(
      education_distance_signed = edu_range,
      wage_distance_signed = 0,  # Hold at 0 to isolate effect
      year_adoption = yr  # Using actual year from data
    )
    
    # Predict values
    edu_df$predicted <- predict(model, newdata = edu_df, type = "response")
    
    # Create plot
    p <- ggplot(edu_df, aes(x = education_distance_signed, y = predicted)) +
      geom_line(size = 1.2, color = "#3366CC") +
      geom_vline(xintercept = critical_edu, linetype = "dashed", color = "red") +
      labs(title = paste("Effect of Educational Distance in", yr),
           subtitle = paste("Critical point at x =", round(critical_edu, 2)),
           x = "Educational Distance (signed)",
           y = "Predicted Diffusion (count)") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    edu_plots[[as.character(yr)]] <- p
  }
  
  # 2. Effect of wage distance by year
  message("Creating wage distance effect plots by year...")
  wage_plots <- list()
  
  for (yr in plot_years) {
    # Create prediction data for this year
    wage_df <- data.frame(
      education_distance_signed = 0,  # Hold at 0 to isolate effect
      wage_distance_signed = wage_range,
      year_adoption = yr  # Using actual year from data
    )
    
    # Predict values
    wage_df$predicted <- predict(model, newdata = wage_df, type = "response")
    
    # Create plot
    p <- ggplot(wage_df, aes(x = wage_distance_signed/1000, y = predicted)) +
      geom_line(size = 1.2, color = "#CC3366") +
      geom_vline(xintercept = critical_wage/1000, linetype = "dashed", color = "blue") +
      labs(title = paste("Effect of Wage Distance in", yr),
           subtitle = paste("Critical point at x =", round(critical_wage/1000, 2), "k"),
           x = "Wage Distance (thousands, signed)",
           y = "Predicted Diffusion (count)") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    wage_plots[[as.character(yr)]] <- p
  }
  
  # 3. Combined effect using the most recent year (to avoid factor level issues)
  message("Creating combined effect plot...")
  recent_year <- max(actual_years)
  
  # Create grid for heatmap
  grid_size <- 50
  grid_data <- expand.grid(
    education_distance_signed = seq(min(data$education_distance_signed, na.rm = TRUE),
                                    max(data$education_distance_signed, na.rm = TRUE),
                                    length.out = grid_size),
    wage_distance_signed = seq(min(data$wage_distance_signed, na.rm = TRUE),
                               max(data$wage_distance_signed, na.rm = TRUE),
                               length.out = grid_size),
    year_adoption = recent_year  # Use most recent year to avoid factor level issues
  )
  
  # Predict values
  grid_data$predicted <- predict(model, newdata = grid_data, type = "response")
  
  # Create heatmap
  combined_plot <- ggplot(grid_data, aes(x = education_distance_signed, 
                                         y = wage_distance_signed/1000, 
                                         fill = predicted)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", name = "Predicted\nDiffusion") +
    geom_point(x = critical_edu, y = critical_wage/1000, color = "white", size = 3, shape = 4) +
    annotate("text", x = critical_edu, y = critical_wage/1000 + 10, 
             label = "Optimal point", color = "white", size = 4) +
    labs(title = paste("Combined Effect of Distances in", recent_year),
         subtitle = "Impact on skill diffusion",
         x = "Educational Distance (signed)",
         y = "Wage Distance (thousands, signed)") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      panel.grid = element_blank()
    )
  
  # 4. Create a patchwork of all year plots (education effect)
  # Combine all educational distance plots
  edu_combined <- wrap_plots(edu_plots, ncol = 2) +
    plot_annotation(
      title = 'Educational Distance Effect by Year',
      subtitle = 'How educational differences affect skill diffusion over time',
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
    )
  
  # Combine all wage distance plots
  wage_combined <- wrap_plots(wage_plots, ncol = 2) +
    plot_annotation(
      title = 'Wage Distance Effect by Year',
      subtitle = 'How wage differences affect skill diffusion over time',
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
    )
  
  # Save all plots to files
  results_dir <- "model_visualizations"
  dir.create(results_dir, showWarnings = FALSE)
  
  # Save individual plots by year
  for (yr in names(edu_plots)) {
    ggsave(file.path(results_dir, paste0("edu_effect_", yr, ".png")), 
           edu_plots[[yr]], width = 8, height = 6)
    ggsave(file.path(results_dir, paste0("wage_effect_", yr, ".png")), 
           wage_plots[[yr]], width = 8, height = 6)
  }
  
  # Save combined plot
  ggsave(file.path(results_dir, "combined_effect.png"), 
         combined_plot, width = 10, height = 8)
  
  # Save combined year plots
  ggsave(file.path(results_dir, "edu_effects_by_year.png"), 
         edu_combined, width = 16, height = 12)
  ggsave(file.path(results_dir, "wage_effects_by_year.png"), 
         wage_combined, width = 16, height = 12)
  
  message(sprintf("Saved visualization plots to %s", results_dir))
  
  return(list(
    edu_plots = edu_plots,
    wage_plots = wage_plots,
    combined_plot = combined_plot,
    edu_combined = edu_combined,
    wage_combined = wage_combined
  ))
}

###############################################################################
# Function to run the full analysis
###############################################################################
run_diffusion_analysis <- function(skills_data, occupation_data = NULL) {
  message("Starting skill diffusion analysis...")
  
  # Step 1: Create diffusion network
  diffusion_data <- create_skill_diffusion_network(skills_data, occupation_data)
  
  if (is.null(diffusion_data) || nrow(diffusion_data) == 0) {
    stop("Failed to create diffusion dataset. Check your data.")
  }
  
  # Save the diffusion dataset
  results_dir <- "results"
  dir.create(results_dir, showWarnings = FALSE)
  fwrite(diffusion_data, file.path(results_dir, "diffusion_network.csv"))
  
  # Step 2: Analyze diffusion model
  model_results <- analyze_diffusion_model(diffusion_data)
  
  # Step 3: Visualize effects
  plot_results <- visualize_model_effects(model_results$model, diffusion_data)
  
  message("Analysis complete. Results saved to 'results' and 'model_visualizations' directories.")
  
  return(list(
    diffusion_data = diffusion_data,
    model = model_results$model,
    critical_points = model_results$critical_points,
    plots = plot_results
  ))
}

###############################################################################
# Example of how to use the code
###############################################################################

# # Combine all skills data into one data frame/data.table
 all_skills_data <- rbindlist(list(
   skills, abilities, knowledge, work_activities, work_styles
 ), use.names = TRUE, fill = TRUE)
 
# # Run the analysis
 results <- run_diffusion_analysis(all_skills_data)
 diffusion_data <- results$diffusion_data
# # Alternatively, load and analyze a previously saved diffusion network
# diffusion_data <- fread("results/diffusion_network.csv")
 model_results <- analyze_diffusion_model(diffusion_data)
 plot_results <- visualize_model_effects(model_results$model, diffusion_data)
 
 
 
 
 