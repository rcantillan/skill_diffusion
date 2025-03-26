# ===============================================================================================
# ANÁLISIS DE REDES DE HABILIDADES BASADO EN VENTAJA COMPARATIVA REVELADA (RCA)
# ===============================================================================================
# Este script prepara conjuntos de datos de habilidades ocupacionales para análisis de redes,
# organizando los datos en períodos de 5 años y calculando métricas de RCA para identificar
# patrones significativos de coocurrencia de habilidades.
# ===============================================================================================

library(data.table)
library(dplyr)
library(tidyr)
library(igraph)   # Para análisis de redes posterior

# ===============================================================================================
# SECCIÓN 1: PREPARACIÓN INICIAL DE DATOS
# ===============================================================================================

# Combinar todos los datos de habilidades de diferentes fuentes
# INPUT:  Datasets individuales de habilidades, capacidades, conocimientos, etc.
# OUTPUT: Dataset unificado con todos los tipos de habilidades
all_skills_data <- rbindlist(list(
  skills, abilities, knowledge, work_activities, work_styles
), use.names = TRUE, fill = TRUE)

# Exploración temporal de los datos
# Ver fechas únicas disponibles para entender la cobertura temporal
unique_dates <- unique(all_skills_data$Date)
print(unique_dates)

# Contar observaciones por fecha para entender la distribución de datos
date_distribution <- table(all_skills_data$Date)
print(date_distribution)

# Convertir fechas de formato texto a objetos de fecha para facilitar el filtrado
all_skills_data$year_month <- as.Date(paste0("01/", all_skills_data$Date), format="%d/%m/%Y")

# Extraer solo el año para análisis por períodos
all_skills_data$year <- format(all_skills_data$year_month, "%Y")

# Verificar la distribución por año
table(all_skills_data$year)

# ===============================================================================================
# SECCIÓN 2: FUNCIONES PRINCIPALES
# ===============================================================================================

#' Prepara los datos divididos en períodos de 5 años
#'
#' Esta función toma el conjunto de datos completo de habilidades y:
#' 1. Lo divide en períodos de 5 años
#' 2. Selecciona los datos más recientes para cada combinación ocupación-habilidad en cada período
#' 3. Calcula estadísticas descriptivas para cada período
#'
#' @param all_skills_data Dataframe con todos los datos de habilidades
#' @return Lista con los datos procesados para cada período
prepare_data_by_period <- function(all_skills_data) {
  # Definición de períodos de 5 años
  periods <- list(
    "2005-2009" = 2005:2009,
    "2010-2014" = 2010:2014,
    "2015-2019" = 2015:2019,
    "2020-2024" = 2020:2024
  )
  
  # Estructura para almacenar resultados procesados
  period_data <- list()
  
  # Para cada período, procesar los datos
  for(period_name in names(periods)) {
    years <- periods[[period_name]]
    
    # Filtrar datos para el período actual
    current_data <- all_skills_data %>%
      filter(year %in% as.character(years))
    
    # Verificar cantidad de datos disponibles
    n_observations <- nrow(current_data)
    n_occupations <- length(unique(current_data$O.NET.SOC.Code))
    n_skills <- length(unique(current_data$Element.ID))
    
    cat(sprintf("Período %s: %d observaciones, %d ocupaciones, %d habilidades\n", 
                period_name, n_observations, n_occupations, n_skills))
    
    # Usar los datos más recientes para cada ocupación-habilidad dentro del período
    # Esto evita duplicados y asegura usar la información más actualizada
    processed_data <- current_data %>%
      arrange(O.NET.SOC.Code, Element.ID, Scale.ID, desc(year), desc(year_month)) %>%
      group_by(O.NET.SOC.Code, Element.ID, Scale.ID) %>%
      slice(1) %>%
      ungroup()
    
    # Enfocarse en calificaciones de importancia (IM scale) que son las más relevantes
    # para construir redes basadas en la importancia de las habilidades
    importance_data <- processed_data %>%
      filter(Scale.ID == "IM") %>%
      select(O.NET.SOC.Code, Element.ID, Element.Name, Data.Value)
    
    # Calcular estadísticas por ocupación
    occupation_stats <- importance_data %>%
      group_by(O.NET.SOC.Code) %>%
      summarize(
        n_skills = n(),                                 # Número de habilidades por ocupación
        mean_importance = mean(Data.Value, na.rm = TRUE), # Importancia media de habilidades
        .groups = 'drop'
      )
    
    # Calcular estadísticas por habilidad
    skill_stats <- importance_data %>%
      group_by(Element.ID, Element.Name) %>%
      summarize(
        n_occupations = n(),                             # Número de ocupaciones que usan la habilidad
        mean_importance = mean(Data.Value, na.rm = TRUE), # Importancia media a través de ocupaciones
        .groups = 'drop'
      )
    
    # Guardar datos procesados y estadísticas en la estructura de resultados
    period_data[[period_name]] <- list(
      raw_data = current_data,            # Datos crudos del período
      processed_data = processed_data,    # Datos procesados (sin duplicados)
      importance_data = importance_data,  # Solo datos de importancia
      occupation_stats = occupation_stats, # Estadísticas por ocupación
      skill_stats = skill_stats           # Estadísticas por habilidad
    )
  }
  
  return(period_data)
}

#' Calcula la Ventaja Comparativa Revelada (RCA) para las habilidades en cada período
#'
#' La RCA mide si una habilidad es más importante para una ocupación específica
#' en comparación con la importancia media de esa habilidad en todas las ocupaciones.
#' RCA > 1 indica especialización o uso efectivo de la habilidad.
#'
#' @param period_data Lista con datos procesados por período
#' @return Lista con datos de RCA y estadísticas para cada período
calculate_period_rca <- function(period_data) {
  period_rca <- list()
  
  for(period_name in names(period_data)) {
    cat(sprintf("Calculando RCA para período %s\n", period_name))
    
    # Obtener datos de importancia
    importance_data <- period_data[[period_name]]$importance_data
    
    # Calcular media de importancia por ocupación
    occupation_means <- importance_data %>%
      group_by(O.NET.SOC.Code) %>%
      summarize(occupation_mean = mean(Data.Value, na.rm = TRUE))
    
    # Calcular media de importancia por habilidad
    skill_means <- importance_data %>%
      group_by(Element.ID) %>%
      summarize(skill_mean = mean(Data.Value, na.rm = TRUE))
    
    # Calcular media general de importancia en todo el conjunto de datos
    overall_mean <- mean(importance_data$Data.Value, na.rm = TRUE)
    
    # Unir medias a los datos originales
    rca_data <- importance_data %>%
      left_join(occupation_means, by = "O.NET.SOC.Code") %>%
      left_join(skill_means, by = "Element.ID") %>%
      mutate(
        # Calcular RCA usando la fórmula clásica:
        # RCA = (Importancia de habilidad en ocupación / Media de importancia en ocupación) /
        #       (Media de importancia de habilidad en todas las ocupaciones / Media general)
        rca = (Data.Value / occupation_mean) / (skill_mean / overall_mean),
        # Indicador para uso efectivo (RCA > 1)
        effective_use = rca > 1
      )
    
    # Estadísticas sobre RCA
    rca_stats <- rca_data %>%
      summarize(
        n_total = n(),                              # Total de combinaciones ocupación-habilidad
        n_effective = sum(effective_use),           # Número con RCA > 1
        pct_effective = mean(effective_use) * 100,  # Porcentaje con RCA > 1
        mean_rca = mean(rca, na.rm = TRUE),         # RCA media
        median_rca = median(rca, na.rm = TRUE)      # RCA mediana
      )
    
    # Guardar datos de RCA y estadísticas
    period_rca[[period_name]] <- list(
      rca_data = rca_data,     # Datos con valores RCA calculados
      rca_stats = rca_stats    # Estadísticas de RCA
    )
  }
  
  return(period_rca)
}

#' Prepara los datos para la construcción de redes de habilidades
#'
#' Construye pares de habilidades que co-ocurren en las mismas ocupaciones con RCA > 1.
#' Estos pares forman la base para construir la red de habilidades.
#'
#' @param period_rca Lista con datos de RCA por período
#' @return Lista con datos de pares de habilidades para cada período
prepare_network_data <- function(period_rca) {
  network_data <- list()
  
  for(period_name in names(period_rca)) {
    cat(sprintf("Preparando datos de red para período %s\n", period_name))
    
    # Obtener datos de RCA
    rca_data <- period_rca[[period_name]]$rca_data
    
    # Filtrar solo habilidades efectivamente utilizadas (RCA > 1)
    # Esto enfoca el análisis en las habilidades que son especializadas para cada ocupación
    effective_skills <- rca_data %>%
      filter(effective_use == TRUE) %>%
      select(O.NET.SOC.Code, Element.ID, Element.Name, rca)
    
    # Crear pares de habilidades que co-ocurren en ocupaciones
    skill_pairs <- data.frame()
    
    # Para cada ocupación, identificar pares de habilidades con uso efectivo
    for(occ in unique(effective_skills$O.NET.SOC.Code)) {
      # Obtener habilidades para esta ocupación
      occ_skills <- effective_skills %>% 
        filter(O.NET.SOC.Code == occ)
      
      # Solo continuar si hay más de una habilidad (necesitamos al menos 2 para formar pares)
      if(nrow(occ_skills) > 1) {
        # Crear todos los pares posibles de habilidades
        pairs <- expand.grid(
          skill1 = occ_skills$Element.ID,
          skill2 = occ_skills$Element.ID,
          stringsAsFactors = FALSE
        ) %>%
          filter(skill1 < skill2) # Evitar auto-bucles y duplicados (mantenemos solo un orden)
        
        # Obtener nombres para mejor interpretación y visualización
        pairs$name1 <- occ_skills$Element.Name[match(pairs$skill1, occ_skills$Element.ID)]
        pairs$name2 <- occ_skills$Element.Name[match(pairs$skill2, occ_skills$Element.ID)]
        
        # Obtener valores RCA para ponderación
        pairs$rca1 <- occ_skills$rca[match(pairs$skill1, occ_skills$Element.ID)]
        pairs$rca2 <- occ_skills$rca[match(pairs$skill2, occ_skills$Element.ID)]
        
        # Calcular peso de la conexión como media geométrica de RCAs
        # Esta métrica captura la intensidad de la relación entre las dos habilidades
        pairs$weight <- sqrt(pairs$rca1 * pairs$rca2)
        
        # Registrar la ocupación de origen para análisis posteriores
        pairs$occupation <- occ
        
        # Acumular todos los pares encontrados
        skill_pairs <- rbind(skill_pairs, pairs)
      }
    }
    
    # Agregar pesos por pares de habilidades
    if(nrow(skill_pairs) > 0) {
      # Agregación: sumar pesos y contar ocurrencias para cada par único de habilidades
      aggregated_pairs <- skill_pairs %>%
        group_by(skill1, skill2, name1, name2) %>%
        summarize(
          weight = sum(weight),                 # Suma de pesos (intensidad acumulada)
          count = n(),                          # Número de ocupaciones donde co-ocurren
          occupation_count = n_distinct(occupation), # Número de ocupaciones únicas
          .groups = 'drop'
        )
      
      # Estadísticas sobre los pares de habilidades
      pair_stats <- data.frame(
        total_pairs = nrow(aggregated_pairs),   # Total de pares únicos
        unique_skills = length(unique(c(aggregated_pairs$skill1, aggregated_pairs$skill2))), # Habilidades únicas
        min_weight = min(aggregated_pairs$weight),     # Peso mínimo
        max_weight = max(aggregated_pairs$weight),     # Peso máximo
        median_weight = median(aggregated_pairs$weight), # Peso mediano
        mean_count = mean(aggregated_pairs$count)      # Promedio de ocupaciones por par
      )
      
      # Guardar datos de pares y estadísticas
      network_data[[period_name]] <- list(
        skill_pairs = aggregated_pairs, # Pares agregados para construir la red
        pair_stats = pair_stats         # Estadísticas sobre los pares
      )
      
      cat(sprintf("  Generados %d pares de habilidades con %d habilidades únicas\n", 
                  pair_stats$total_pairs, pair_stats$unique_skills))
    } else {
      cat("  No se generaron pares de habilidades para este período\n")
      network_data[[period_name]] <- NULL
    }
  }
  
  return(network_data)
}

#' Función principal que ejecuta todo el proceso de preparación de datos
#'
#' Coordina la ejecución de todas las funciones anteriores y guarda los resultados
#' en archivos CSV y RDS para análisis posteriores.
#'
#' @param all_skills_data Dataframe con todos los datos de habilidades
#' @param output_dir Directorio donde se guardarán los resultados
#' @return Lista con todos los datos procesados
prepare_all_data <- function(all_skills_data, output_dir = "data_output") {
  # Crear directorio para resultados
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 1. Preparar datos por período
  cat("Preparando datos por período de 5 años...\n")
  period_data <- prepare_data_by_period(all_skills_data)
  
  # 2. Calcular RCA para cada período
  cat("\nCalculando valores RCA...\n")
  period_rca <- calculate_period_rca(period_data)
  
  # 3. Preparar datos para redes
  cat("\nPreparando datos para construcción de redes...\n")
  network_data <- prepare_network_data(period_rca)
  
  # 4. Guardar datos procesados en archivos CSV
  cat("\nGuardando datos procesados...\n")
  
  for(period_name in names(network_data)) {
    # Crear directorio específico para cada período
    period_dir <- file.path(output_dir, period_name)
    dir.create(period_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Guardar datos de RCA
    write.csv(
      period_rca[[period_name]]$rca_data,
      file.path(period_dir, "skill_rca.csv"),
      row.names = FALSE
    )
    
    # Guardar datos de pares para construcción de redes
    write.csv(
      network_data[[period_name]]$skill_pairs,
      file.path(period_dir, "skill_pairs.csv"),
      row.names = FALSE
    )
    
    # Guardar estadísticas resumen del período
    stats <- data.frame(
      period = period_name,
      n_occupations = nrow(period_data[[period_name]]$occupation_stats),
      n_skills = nrow(period_data[[period_name]]$skill_stats),
      n_skill_pairs = network_data[[period_name]]$pair_stats$total_pairs,
      unique_skills_in_network = network_data[[period_name]]$pair_stats$unique_skills,
      pct_effective_skills = period_rca[[period_name]]$rca_stats$pct_effective,
      mean_rca = period_rca[[period_name]]$rca_stats$mean_rca
    )
    
    write.csv(
      stats,
      file.path(period_dir, "summary_stats.csv"),
      row.names = FALSE
    )
  }
  
  # 5. Guardar datos completos para análisis posterior
  cat("Guardando datos completos para análisis...\n")
  
  # Usar try-catch para manejar posibles errores al guardar datos complejos
  tryCatch({
    saveRDS(
      list(
        period_data = period_data,
        period_rca = period_rca,
        network_data = network_data
      ),
      file.path(output_dir, "complete_skill_data.rds")
    )
  }, error = function(e) {
    cat("Advertencia: No se pudieron guardar los datos completos. Error:", e$message, "\n")
    cat("Guardando componentes individuales...\n")
    
    # Intentar guardar componentes por separado si el guardado completo falla
    saveRDS(period_data, file.path(output_dir, "period_data.rds"))
    saveRDS(period_rca, file.path(output_dir, "period_rca.rds"))
    saveRDS(network_data, file.path(output_dir, "network_data.rds"))
  })
  
  cat("Preparación de datos completada. Resultados guardados en:", output_dir, "\n")
  
  # Devolver todos los datos procesados para uso posterior
  return(list(
    period_data = period_data,
    period_rca = period_rca,
    network_data = network_data
  ))
}

# ===============================================================================================
# SECCIÓN 3: EJECUCIÓN DEL ANÁLISIS
# ===============================================================================================

# Ejecutar todo el proceso de preparación de datos
# INPUT:  Dataset unificado de habilidades
# OUTPUT: Datos procesados guardados en archivos CSV y objetos R
results <- prepare_all_data(all_skills_data, "skill_network_data_5year")

# ===============================================================================================
# NOTAS ADICIONALES:
# ===============================================================================================
# 1. Los archivos generados pueden usarse para:
#    - Construir redes de habilidades con igraph o tidygraph
#    - Realizar análisis de blockmodeling con stochastic block models
#    - Identificar comunidades de habilidades relacionadas
#    - Visualizar la evolución de las redes de habilidades a lo largo del tiempo
#
# 2. Para construir una red básica con igraph, se puede usar:
#    library(igraph)
#    period_name <- "2020-2024"  # O cualquier otro período
#    skill_pairs <- read.csv(file.path("skill_network_data_5year", period_name, "skill_pairs.csv"))
#    g <- graph_from_data_frame(skill_pairs[, c("skill1", "skill2", "weight")], directed = FALSE)
#    plot(g, edge.width = E(g)$weight/max(E(g)$weight) * 5)
#
# 3. Para análisis de stochastic block models, integrar con graph-tool de Python mediante reticulate
# ===============================================================================================