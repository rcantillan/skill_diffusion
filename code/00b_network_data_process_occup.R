# ===============================================================================================
# ANÁLISIS DE REDES DE OCUPACIONES BASADO EN HABILIDADES COMPARTIDAS
# ADAPTACIÓN DE LA METODOLOGÍA DE ALABDULKAREEM ET AL. (2018)
# ===============================================================================================
# Este script implementa una adaptación de la metodología del paper "Unpacking the polarization of workplace skills" 
# para construir una red de ocupaciones donde los pesos representan la complementariedad entre ocupaciones
# basada en las habilidades que comparten.
# ===============================================================================================
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(igraph)   # Para análisis de redes

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

#' Prepara los datos para la construcción de redes de ocupaciones basada en habilidades compartidas
#'
#' Calcula la complementariedad entre pares de ocupaciones como:
#' q(j,j') = ∑s∈S e(j,s)⋅e(j',s) / max(∑s∈S e(j,s), ∑s∈S e(j',s))
#' donde e(j,s) = 1 si la habilidad s es efectivamente usada por la ocupación j (RCA > 1)
#'
#' @param period_rca Lista con datos de RCA por período
#' @return Lista con datos de pares de ocupaciones para cada período
prepare_occupation_network_data <- function(period_rca) {
  network_data <- list()
  
  for(period_name in names(period_rca)) {
    cat(sprintf("Preparando datos de red de ocupaciones para período %s\n", period_name))
    
    # Obtener datos de RCA
    rca_data <- period_rca[[period_name]]$rca_data
    
    # Crear matriz de uso efectivo (e(j,s) = 1 si RCA > 1)
    # Esta versión contiene solo ocupación, habilidad y uso efectivo
    effective_use_data <- rca_data %>%
      select(O.NET.SOC.Code, Element.ID, Element.Name, effective_use) %>%
      filter(effective_use == TRUE)
    
    # Obtener listas únicas de ocupaciones y habilidades con uso efectivo
    occupations <- unique(effective_use_data$O.NET.SOC.Code)
    skills <- unique(effective_use_data$Element.ID)
    
    # Obtener títulos de ocupaciones si están disponibles
    if("occupation_data" %in% ls() && "Title" %in% colnames(occupation_data)) {
      occupation_titles <- setNames(occupation_data$Title, occupation_data$O.NET.SOC.Code)
    } else {
      # Si no hay títulos disponibles, usar los códigos como nombres
      occupation_titles <- setNames(occupations, occupations)
    }
    
    # Calcular la complementariedad entre todos los pares de ocupaciones
    # usando la fórmula adaptada del paper
    
    # Preparar dataframe para almacenar resultados
    occupation_pairs <- data.frame()
    
    # Para cada par de ocupaciones, calcular complementariedad
    # Generar todos los posibles pares de ocupaciones (sin repetir)
    cat("  Generando combinaciones de ocupaciones...\n")
    
    # Este proceso puede ser intensivo para grandes números de ocupaciones
    # Podemos usar una implementación por lotes si es necesario
    n_occupations <- length(occupations)
    
    # Si hay muchas ocupaciones, dividir en lotes para evitar problemas de memoria
    batch_size <- 1000  # Ajustar según la capacidad del sistema
    
    if(n_occupations > 200) {
      cat(sprintf("  Procesando %d ocupaciones en lotes...\n", n_occupations))
      
      # Número total de pares a generar
      total_pairs <- n_occupations * (n_occupations - 1) / 2
      cat(sprintf("  Se generarán aproximadamente %d pares de ocupaciones\n", total_pairs))
      
      # Procesar por lotes para mayor eficiencia
      counter <- 0
      progress_step <- max(1, floor(total_pairs / 20))  # Reportar progreso cada ~5%
      
      for(i in 1:(n_occupations-1)) {
        j1 <- occupations[i]
        
        # Habilidades efectivamente usadas por la primera ocupación
        skills_j1 <- effective_use_data$Element.ID[effective_use_data$O.NET.SOC.Code == j1]
        n_skills_j1 <- length(skills_j1)
        
        for(j in (i+1):n_occupations) {
          j2 <- occupations[j]
          
          # Habilidades efectivamente usadas por la segunda ocupación
          skills_j2 <- effective_use_data$Element.ID[effective_use_data$O.NET.SOC.Code == j2]
          n_skills_j2 <- length(skills_j2)
          
          # Habilidades compartidas (intersección)
          common_skills <- intersect(skills_j1, skills_j2)
          n_common_skills <- length(common_skills)
          
          # Calcular complementariedad según la fórmula adaptada
          numerator <- n_common_skills
          denominator <- max(n_skills_j1, n_skills_j2)
          
          if(denominator > 0) {  # Evitar división por cero
            complementarity <- numerator / denominator
            
            # Solo incluir pares con complementariedad > 0
            if(complementarity > 0) {
              pair <- data.frame(
                occupation1 = j1,
                occupation2 = j2,
                name1 = occupation_titles[j1],
                name2 = occupation_titles[j2],
                weight = complementarity,  # Este es q(j,j') adaptado del paper
                count = numerator,         # Número de habilidades compartidas
                max_count = denominator,   # Máximo de habilidades efectivamente usadas
                stringsAsFactors = FALSE
              )
              occupation_pairs <- rbind(occupation_pairs, pair)
            }
          }
          
          # Actualizar contador y mostrar progreso
          counter <- counter + 1
          if(counter %% progress_step == 0) {
            cat(sprintf("    Progreso: %.1f%% (%d de %d pares procesados)\n", 
                        100 * counter / total_pairs, counter, total_pairs))
          }
        }
      }
    } else {
      # Para conjuntos pequeños, generar todos los pares de una vez
      occ_combinations <- t(combn(occupations, 2))
      colnames(occ_combinations) <- c("occupation1", "occupation2")
      occ_combinations <- as.data.frame(occ_combinations, stringsAsFactors = FALSE)
      
      # Para cada par, calcular complementariedad
      for(i in 1:nrow(occ_combinations)) {
        j1 <- occ_combinations$occupation1[i]
        j2 <- occ_combinations$occupation2[i]
        
        # Habilidades efectivamente usadas por cada ocupación
        skills_j1 <- effective_use_data$Element.ID[effective_use_data$O.NET.SOC.Code == j1]
        skills_j2 <- effective_use_data$Element.ID[effective_use_data$O.NET.SOC.Code == j2]
        
        # Habilidades compartidas (intersección)
        common_skills <- intersect(skills_j1, skills_j2)
        
        # Calcular complementariedad según la fórmula adaptada
        numerator <- length(common_skills)
        denominator <- max(length(skills_j1), length(skills_j2))
        
        if(denominator > 0) {  # Evitar división por cero
          complementarity <- numerator / denominator
          
          # Solo incluir pares con complementariedad > 0
          if(complementarity > 0) {
            pair <- data.frame(
              occupation1 = j1,
              occupation2 = j2,
              name1 = occupation_titles[j1],
              name2 = occupation_titles[j2],
              weight = complementarity,  # Este es q(j,j') adaptado del paper
              count = numerator,         # Número de habilidades compartidas
              max_count = denominator,   # Máximo de habilidades efectivamente usadas
              stringsAsFactors = FALSE
            )
            occupation_pairs <- rbind(occupation_pairs, pair)
          }
        }
      }
    }
    
    # Estadísticas sobre los pares de ocupaciones
    if(nrow(occupation_pairs) > 0) {
      pair_stats <- data.frame(
        total_pairs = nrow(occupation_pairs),   # Total de pares únicos
        unique_occupations = length(unique(c(occupation_pairs$occupation1, occupation_pairs$occupation2))), # Ocupaciones únicas
        min_weight = min(occupation_pairs$weight),     # Peso mínimo
        max_weight = max(occupation_pairs$weight),     # Peso máximo
        median_weight = median(occupation_pairs$weight), # Peso mediano
        mean_weight = mean(occupation_pairs$weight)    # Peso medio
      )
      
      # Guardar datos de pares y estadísticas
      network_data[[period_name]] <- list(
        occupation_pairs = occupation_pairs, # Pares de ocupaciones con complementariedad como peso
        pair_stats = pair_stats    # Estadísticas sobre los pares
      )
      
      cat(sprintf("  Generados %d pares de ocupaciones con %d ocupaciones únicas\n", 
                  pair_stats$total_pairs, pair_stats$unique_occupations))
      cat(sprintf("  Complementariedad media: %.4f, mediana: %.4f, rango: [%.4f, %.4f]\n",
                  pair_stats$mean_weight, pair_stats$median_weight, 
                  pair_stats$min_weight, pair_stats$max_weight))
    } else {
      cat("  No se generaron pares de ocupaciones para este período\n")
      network_data[[period_name]] <- NULL
    }
  }
  
  return(network_data)
}

#' Función principal que ejecuta todo el proceso de preparación de datos para la red de ocupaciones
#'
#' Coordina la ejecución de todas las funciones anteriores y guarda los resultados
#' en archivos CSV y RDS para análisis posteriores.
#'
#' @param all_skills_data Dataframe con todos los datos de habilidades
#' @param output_dir Directorio donde se guardarán los resultados
#' @return Lista con todos los datos procesados
prepare_occupation_network <- function(all_skills_data, output_dir = "occupation_network_data") {
  # Crear directorio para resultados
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 1. Preparar datos por período
  cat("Preparando datos por período de 5 años...\n")
  period_data <- prepare_data_by_period(all_skills_data)
  
  # 2. Calcular RCA para cada período
  cat("\nCalculando valores RCA...\n")
  period_rca <- calculate_period_rca(period_data)
  
  # 3. Preparar datos para redes de ocupaciones basadas en habilidades compartidas
  cat("\nPreparando datos para construcción de redes de ocupaciones...\n")
  occupation_network_data <- prepare_occupation_network_data(period_rca)
  
  # 4. Guardar datos procesados en archivos CSV
  cat("\nGuardando datos procesados...\n")
  
  for(period_name in names(occupation_network_data)) {
    # Crear directorio específico para cada período
    period_dir <- file.path(output_dir, period_name)
    dir.create(period_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Guardar datos de pares de ocupaciones para construcción de redes
    write.table(
      occupation_network_data[[period_name]]$occupation_pairs,
      file.path(period_dir, "occupation_pairs.csv"),
      row.names = FALSE,
      sep = ";",              # Usar punto y coma como separador
      dec = ".",              # Usar punto como separador decimal
      quote = TRUE
    )
    
    # Guardar estadísticas resumen del período
    stats <- data.frame(
      period = period_name,
      n_occupations = occupation_network_data[[period_name]]$pair_stats$unique_occupations,
      n_occupation_pairs = occupation_network_data[[period_name]]$pair_stats$total_pairs,
      mean_complementarity = occupation_network_data[[period_name]]$pair_stats$mean_weight,
      median_complementarity = occupation_network_data[[period_name]]$pair_stats$median_weight,
      min_complementarity = occupation_network_data[[period_name]]$pair_stats$min_weight,
      max_complementarity = occupation_network_data[[period_name]]$pair_stats$max_weight
    )
    
    write.table(
      stats,
      file.path(period_dir, "summary_stats.csv"),
      row.names = FALSE,
      sep = ";",
      dec = "."
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
        occupation_network_data = occupation_network_data
      ),
      file.path(output_dir, "complete_occupation_network_data.rds")
    )
  }, error = function(e) {
    cat("Advertencia: No se pudieron guardar los datos completos. Error:", e$message, "\n")
    cat("Guardando componentes individuales...\n")
    
    # Intentar guardar componentes por separado si el guardado completo falla
    saveRDS(period_data, file.path(output_dir, "period_data.rds"))
    saveRDS(period_rca, file.path(output_dir, "period_rca.rds"))
    saveRDS(occupation_network_data, file.path(output_dir, "occupation_network_data.rds"))
  })
  
  cat("Preparación de datos completada. Resultados guardados en:", output_dir, "\n")
  
  # Devolver todos los datos procesados para uso posterior
  return(list(
    period_data = period_data,
    period_rca = period_rca,
    occupation_network_data = occupation_network_data
  ))
}

#' Crear una red de ocupaciones filtrada y detectar comunidades
#'
#' @param occupation_pairs Dataframe con pares de ocupaciones y sus pesos
#' @param min_weight Umbral mínimo de complementariedad para incluir una conexión (default: 0.6 como en el paper)
#' @return Lista con el objeto igraph y las comunidades detectadas
create_occupation_network <- function(occupation_pairs, min_weight = 0.6) {
  # Filtrar conexiones con peso mínimo
  filtered_pairs <- occupation_pairs %>% 
    filter(weight >= min_weight)
  
  # Crear objeto de red igraph
  g <- graph_from_data_frame(
    filtered_pairs[, c("occupation1", "occupation2", "weight")], 
    directed = FALSE,
    vertices = unique(c(occupation_pairs$occupation1, occupation_pairs$occupation2))
  )
  
  # Añadir nombres de ocupaciones como atributos de nodos
  V(g)$name_desc <- c(
    setNames(occupation_pairs$name1[match(V(g)$name, occupation_pairs$occupation1)], V(g)$name),
    setNames(occupation_pairs$name2[match(V(g)$name, occupation_pairs$occupation2)], V(g)$name)
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

#' Visualiza la red de ocupaciones con colores por comunidad
#'
#' @param network Objeto de red igraph
#' @param output_file Nombre del archivo para guardar la visualización
#' @param layout Algoritmo de layout a utilizar (default: fruchterman_reingold)
#' @param vertex_size Tamaño de los nodos
#' @param show_labels Si se muestran las etiquetas de los nodos
visualize_occupation_network <- function(network, 
                                         output_file = NULL, 
                                         layout = layout_with_fr,
                                         vertex_size = 5,
                                         show_labels = FALSE) {
  
  # Definir colores únicos para cada comunidad
  n_communities <- length(unique(V(network)$community))
  community_colors <- rainbow(n_communities)
  
  # Configurar la visualización
  plot_params <- list(
    network,
    vertex.color = community_colors[V(network)$community],
    vertex.size = vertex_size,
    vertex.label = if(show_labels) V(network)$name_desc else NA,
    vertex.label.cex = 0.6,
    vertex.label.color = "black",
    edge.width = E(network)$weight * 2,
    edge.color = "gray80",
    layout = layout
  )
  
  # Guardar en archivo o mostrar en pantalla
  if(!is.null(output_file)) {
    # Determinar el formato basado en la extensión del archivo
    if(grepl("\\.pdf$", output_file)) {
      pdf(output_file, width = 12, height = 10)
      do.call(plot, plot_params)
      dev.off()
    } else if(grepl("\\.png$", output_file)) {
      png(output_file, width = 1200, height = 1000, res = 100)
      do.call(plot, plot_params)
      dev.off()
    } else if(grepl("\\.svg$", output_file)) {
      svg(output_file, width = 12, height = 10)
      do.call(plot, plot_params)
      dev.off()
    } else {
      # Formato por defecto si no se reconoce la extensión
      pdf(output_file, width = 12, height = 10)
      do.call(plot, plot_params)
      dev.off()
    }
    cat(sprintf("Visualización guardada en: %s\n", output_file))
  } else {
    # Mostrar en pantalla
    do.call(plot, plot_params)
  }
}

#' Analiza las comunidades de ocupaciones y sus características
#'
#' @param network_obj Lista con el objeto igraph y las comunidades detectadas
#' @param occupation_data Dataframe con información adicional de ocupaciones (opcional)
#' @return Dataframe con estadísticas de las comunidades
analyze_occupation_communities <- function(network_obj, occupation_data = NULL) {
  g <- network_obj$network
  communities <- network_obj$communities
  
  # Estadísticas básicas
  n_communities <- length(communities)
  sizes <- sizes(communities)
  
  # Crear dataframe para resultados
  results <- data.frame(
    community_id = 1:n_communities,
    size = sizes,
    modularity = modularity(communities),
    stringsAsFactors = FALSE
  )
  
  # Obtener ocupaciones representativas de cada comunidad
  community_members <- list()
  for(i in 1:n_communities) {
    members <- names(V(g)[communities$membership == i])
    community_members[[i]] <- members
    
    # Añadir miembros al dataframe de resultados
    if(length(members) > 0) {
      results$top_members[i] <- paste(head(members, 5), collapse = ", ")
    } else {
      results$top_members[i] <- ""
    }
  }
  
  # Si hay datos adicionales de ocupación, calcular estadísticas adicionales
  if(!is.null(occupation_data) && "O.NET.SOC.Code" %in% colnames(occupation_data)) {
    # Aquí se podrían añadir análisis adicionales, como salarios promedio por comunidad,
    # niveles educativos predominantes, etc., si esos datos están disponibles
    cat("Datos adicionales de ocupación disponibles para análisis avanzado\n")
  }
  
  return(list(
    stats = results,
    members = community_members
  ))
}

# ===============================================================================================
# SECCIÓN 3: EJECUCIÓN DEL ANÁLISIS
# ===============================================================================================

# Ejecutar todo el proceso de preparación de datos
# INPUT:  Dataset unificado de habilidades
# OUTPUT: Datos procesados guardados en archivos CSV y objetos R
results <- prepare_occupation_network(all_skills_data, "occupation_network_alabdulkareem_method")

# NOTA: Para crear la red y detectar comunidades:
# Ejemplo para el período más reciente:
most_recent_period <- names(results$occupation_network_data)[length(results$occupation_network_data)]
occupation_pairs <- results$occupation_network_data[[most_recent_period]]$occupation_pairs
occupation_network <- create_occupation_network(occupation_pairs, min_weight = 0.6)

# Visualizar la red con colores por comunidad
# visualize_occupation_network(
#   occupation_network$network, 
#   output_file = "occupation_network.pdf",
#   show_labels = FALSE
# )

# Analizar las comunidades de ocupaciones
community_analysis <- analyze_occupation_communities(occupation_network)
print(head(community_analysis$stats))

# ===============================================================================================
# NOTAS SOBRE LA METODOLOGÍA
# ===============================================================================================
# 1. Adaptación principal respecto al análisis original:
#    - Se modificó la fórmula de complementariedad para trabajar con ocupaciones:
#      q(j,j') = ∑s∈S e(j,s)⋅e(j',s) / max(∑s∈S e(j,s), ∑s∈S e(j',s))
#    - Esta fórmula representa la proporción de habilidades efectivamente utilizadas 
#      que son compartidas entre dos ocupaciones
#
# 2. Ventajas de este enfoque:
#    - Revela clusters de ocupaciones que comparten conjuntos similares de habilidades
#    - Identifica caminos potenciales de movilidad laboral entre ocupaciones
#    - Muestra la "distancia de habilidades" entre diferentes trabajos
#    - Identifica ocupaciones puente entre diferentes sectores
#
# 3. Aplicaciones posteriores:
#    - Análisis de clusters ocupacionales: identificar grupos de ocupaciones relacionadas
#    - Movilidad laboral: identificar rutas óptimas para transiciones de carrera
#    - Evaluación de la vulnerabilidad a la automatización por clusters
#    - Planificación educativa y de capacitación basada en habilidades transferibles
# ===============================================================================================

# ===============================================================================================
# FUNCIÓN PARA LEER CORRECTAMENTE LOS ARCHIVOS EXPORTADOS
# ===============================================================================================
#' Lee un archivo CSV generado con este script
#'
#' @param file_path Ruta al archivo CSV
#' @return Dataframe con los datos leídos correctamente
read_exported_csv <- function(file_path) {
  # Asegurarse de leer con los mismos parámetros usados para guardar
  data <- read.table(
    file_path,
    header = TRUE,
    sep = ";",
    dec = ".",
    stringsAsFactors = FALSE,
    quote = "\""
  )
  return(data)
}