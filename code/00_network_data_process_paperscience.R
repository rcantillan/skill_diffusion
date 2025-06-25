# ===============================================================================================
# ANÁLISIS DE REDES DE HABILIDADES BASADO EN VENTAJA COMPARATIVA REVELADA (RCA)
# SIGUIENDO LA METODOLOGÍA DE ALABDULKAREEM ET AL. (2018)
# ===============================================================================================
# Este script implementa la metodología exacta del paper "Unpacking the polarization of workplace skills" 
# para construir una red de habilidades donde los pesos representan la complementariedad entre habilidades.
# ===============================================================================================
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(igraph)   # Para análisis de redes posterior

# Data loading 
# Verificar los archivos disponibles en la carpeta data
list.files(here("data"))

# Cargar los archivos principales usando here para rutas relativas
skills <- read.delim(here("data", "Skills.txt"), sep="\t")
abilities <- read.delim(here("data", "Abilities.txt"), sep="\t")
knowledge <- read.delim(here("data", "Knowledge.txt"), sep="\t")
work_activities <- read.delim(here("data", "Work Activities.txt"), sep="\t")
work_styles <- read.delim(here("data", "Work Styles.txt"), sep="\t")
occupation_data <- read.delim(here("data", "Occupation Data.txt"), sep="\t")

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
    
    # Calcular RCA según la fórmula del paper:
    # rca(j,s) = (onet(j,s)/∑s' onet(j,s')) / (∑j' onet(j',s)/∑j',s' onet(j',s'))
    
    # 1. Calcular la suma de importancia para cada ocupación
    occupation_sums <- importance_data %>%
      group_by(O.NET.SOC.Code) %>%
      summarize(occupation_sum = sum(Data.Value, na.rm = TRUE))
    
    # 2. Calcular la suma de importancia para cada habilidad
    skill_sums <- importance_data %>%
      group_by(Element.ID) %>%
      summarize(skill_sum = sum(Data.Value, na.rm = TRUE))
    
    # 3. Calcular la suma total de importancia en todo el conjunto de datos
    total_sum <- sum(importance_data$Data.Value, na.rm = TRUE)
    
    # 4. Unir todo para calcular RCA
    rca_data <- importance_data %>%
      left_join(occupation_sums, by = "O.NET.SOC.Code") %>%
      left_join(skill_sums, by = "Element.ID") %>%
      mutate(
        # Calcular RCA utilizando la fórmula exacta del paper
        rca = (Data.Value / occupation_sum) / (skill_sum / total_sum),
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

#' Prepara los datos para la construcción de redes de habilidades siguiendo exactamente
#' la metodología del paper de Alabdulkareem et al. (2018)
#'
#' Calcula la complementariedad entre pares de habilidades como:
#' q(s,s') = ∑j∈J e(j,s)⋅e(j,s') / max(∑j∈J e(j,s), ∑j∈J e(j,s'))
#' donde e(j,s) = 1 si la habilidad s es efectivamente usada por la ocupación j (RCA > 1)
#'
#' @param period_rca Lista con datos de RCA por período
#' @return Lista con datos de pares de habilidades para cada período
prepare_network_data <- function(period_rca) {
  network_data <- list()
  
  for(period_name in names(period_rca)) {
    cat(sprintf("Preparando datos de red para período %s\n", period_name))
    
    # Obtener datos de RCA
    rca_data <- period_rca[[period_name]]$rca_data
    
    # Crear matriz de uso efectivo (e(j,s) = 1 si RCA > 1)
    # Primero, crear una versión más manejable de los datos con solo ocupación, habilidad y uso efectivo
    effective_use_data <- rca_data %>%
      select(O.NET.SOC.Code, Element.ID, Element.Name, effective_use) %>%
      filter(effective_use == TRUE)
    
    # Obtener listas únicas de ocupaciones y habilidades con uso efectivo
    occupations <- unique(effective_use_data$O.NET.SOC.Code)
    skills <- unique(effective_use_data$Element.ID)
    skill_names <- setNames(effective_use_data$Element.Name, effective_use_data$Element.ID)
    skill_names <- skill_names[!duplicated(names(skill_names))]
    
    # Calcular la complementariedad entre todos los pares de habilidades
    # usando la fórmula exacta del paper
    
    # Preparar dataframe para almacenar resultados
    skill_pairs <- data.frame()
    
    # Para cada par de habilidades, calcular complementariedad
    skill_counts <- table(effective_use_data$Element.ID)  # Contar ocupaciones por habilidad
    
    # Generar todos los posibles pares de habilidades (sin repetir)
    skill_combinations <- t(combn(skills, 2))
    colnames(skill_combinations) <- c("skill1", "skill2")
    skill_combinations <- as.data.frame(skill_combinations, stringsAsFactors = FALSE)
    
    # Para cada par, calcular complementariedad
    for(i in 1:nrow(skill_combinations)) {
      s1 <- skill_combinations$skill1[i]
      s2 <- skill_combinations$skill2[i]
      
      # Encontrar ocupaciones que usan efectivamente ambas habilidades
      occs_s1 <- effective_use_data$O.NET.SOC.Code[effective_use_data$Element.ID == s1]
      occs_s2 <- effective_use_data$O.NET.SOC.Code[effective_use_data$Element.ID == s2]
      
      # Ocupaciones que usan ambas habilidades (intersección)
      common_occs <- intersect(occs_s1, occs_s2)
      
      # Calcular complementariedad según la fórmula del paper
      numerator <- length(common_occs)
      denominator <- max(length(occs_s1), length(occs_s2))
      
      if(denominator > 0) {  # Evitar división por cero
        complementarity <- numerator / denominator
        
        # Solo incluir pares con complementariedad > 0
        if(complementarity > 0) {
          pair <- data.frame(
            skill1 = s1,
            skill2 = s2,
            name1 = skill_names[s1],
            name2 = skill_names[s2],
            weight = complementarity,  # Este es q(s,s') en el paper
            count = numerator,         # Número de ocupaciones donde co-ocurren
            max_count = denominator    # Máximo de ocupaciones con cualquiera de las habilidades
          )
          skill_pairs <- rbind(skill_pairs, pair)
        }
      }
    }
    
    # Estadísticas sobre los pares de habilidades
    if(nrow(skill_pairs) > 0) {
      pair_stats <- data.frame(
        total_pairs = nrow(skill_pairs),   # Total de pares únicos
        unique_skills = length(unique(c(skill_pairs$skill1, skill_pairs$skill2))), # Habilidades únicas
        min_weight = min(skill_pairs$weight),     # Peso mínimo
        max_weight = max(skill_pairs$weight),     # Peso máximo
        median_weight = median(skill_pairs$weight), # Peso mediano
        mean_weight = mean(skill_pairs$weight)    # Peso medio
      )
      
      # Guardar datos de pares y estadísticas
      network_data[[period_name]] <- list(
        skill_pairs = skill_pairs, # Pares con complementariedad como peso
        pair_stats = pair_stats    # Estadísticas sobre los pares
      )
      
      cat(sprintf("  Generados %d pares de habilidades con %d habilidades únicas\n", 
                  pair_stats$total_pairs, pair_stats$unique_skills))
      cat(sprintf("  Complementariedad media: %.4f, mediana: %.4f, rango: [%.4f, %.4f]\n",
                  pair_stats$mean_weight, pair_stats$median_weight, 
                  pair_stats$min_weight, pair_stats$max_weight))
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
  
  # 3. Preparar datos para redes siguiendo la metodología exacta del paper
  cat("\nPreparando datos para construcción de redes (metodología de Alabdulkareem et al.)...\n")
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
      mean_rca = period_rca[[period_name]]$rca_stats$mean_rca,
      mean_complementarity = network_data[[period_name]]$pair_stats$mean_weight,
      median_complementarity = network_data[[period_name]]$pair_stats$median_weight
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

#' Crear una red de habilidades filtrada y detectar comunidades
#'
#' @param skill_pairs Dataframe con pares de habilidades y sus pesos
#' @param min_weight Umbral mínimo de complementariedad para incluir una conexión (default: 0.6 como en el paper)
#' @return Lista con el objeto igraph y las comunidades detectadas
create_skillscape_network <- function(skill_pairs, min_weight = 0.6) {
  # Filtrar conexiones con peso mínimo
  filtered_pairs <- skill_pairs %>% 
    filter(weight >= min_weight)
  
  # Crear objeto de red igraph
  g <- graph_from_data_frame(
    filtered_pairs[, c("skill1", "skill2", "weight")], 
    directed = FALSE,
    vertices = unique(c(skill_pairs$skill1, skill_pairs$skill2))
  )
  
  # Añadir nombres de habilidades como atributos de nodos
  V(g)$name_desc <- c(
    setNames(skill_pairs$name1[match(V(g)$name, skill_pairs$skill1)], V(g)$name),
    setNames(skill_pairs$name2[match(V(g)$name, skill_pairs$skill2)], V(g)$name)
  )[V(g)$name]
  
  # Detectar comunidades usando el algoritmo Louvain (como en el paper)
  communities <- cluster_louvain(g)
  
  # Asignar comunidades como atributo de nodos
  V(g)$community <- communities$membership
  
  return(list(
    network = g,
    communities = communities
  ))
}

# ===============================================================================================
# SECCIÓN 3: EJECUCIÓN DEL ANÁLISIS
# ===============================================================================================

# Ejecutar todo el proceso de preparación de datos
# INPUT:  Dataset unificado de habilidades
# OUTPUT: Datos procesados guardados en archivos CSV y objetos R
results <- prepare_all_data(all_skills_data, "skillscape_alabdulkareem_method")

# NOTA: Para crear la red y detectar comunidades como en el paper:
# Ejemplo para el período más reciente:
most_recent_period <- names(results$network_data)[length(results$network_data)]
skill_pairs <- results$network_data[[most_recent_period]]$skill_pairs
skillscape <- create_skillscape_network(skill_pairs, min_weight = 0.6)

# Visualizar la red con colores por comunidad
# plot(skillscape$network, 
#     vertex.color = skillscape$communities$membership,
#     vertex.size = 5,
#     vertex.label = NA,
#     edge.width = E(skillscape$network)$weight * 2,
#     layout = layout_with_fr)

# ===============================================================================================
# NOTAS SOBRE LA METODOLOGÍA DEL PAPER VS. CÓDIGO ORIGINAL
# ===============================================================================================
# 1. Cambios principales respecto al código original:
#    - Se modificó la forma de calcular los pesos entre habilidades, usando directamente
#      la fórmula de complementariedad del paper: 
#      q(s,s') = ∑j e(j,s)⋅e(j,s') / max(∑j e(j,s), ∑j e(j,s'))
#    - Se eliminó el cálculo basado en la media geométrica de RCAs
#    - El peso ahora representa la probabilidad condicional mínima de que dos habilidades
#      sean efectivamente usadas juntas
#
# 2. Ventajas de la metodología del paper:
#    - Los pesos tienen una interpretación probabilística clara
#    - El enfoque captura directamente la co-ocurrencia de habilidades a través de ocupaciones
#    - Facilita la detección de comunidades mediante algoritmos estándar como Louvain
#
# 3. Aplicaciones posteriores:
#    - Análisis de polarización: verificar si emergen dos comunidades principales (sociocognitiva y sensorial-física)
#    - Estudiar la correlación entre comunidades de habilidades y salarios de ocupaciones
#    - Examinar cómo cambia la estructura de la red a lo largo del tiempo
# ===============================================================================================