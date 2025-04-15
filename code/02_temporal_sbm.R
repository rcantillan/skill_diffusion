# Análisis Temporal de Estructura Modular de Redes de Habilidades
# Este script procesa redes de habilidades para múltiples períodos y analiza
# su estructura modular usando el modelo de bloques estocásticos anidado.

library(reticulate)
library(tidyverse)
library(tidygraph)
library(igraph)


# Configurar el entorno de Python
reticulate::use_python("/home/rober/miniconda3/envs/graph-tool-env/bin/python", required = TRUE)

# Verificar las versiones
reticulate::py_run_string("
import numpy
import scipy
print('Versión de NumPy:', numpy.__version__)
print('Versión de SciPy:', scipy.__version__)
")


#----------------------------------------
# FUNCIONES AUXILIARES
#----------------------------------------

#' Prepara la red de habilidades a partir de datos crudos
#' @param skill_pairs Dataframe con pares de habilidades
#' @return Un objeto tidygraph que representa la red de habilidades
prepare_skill_network <- function(skill_pairs) {
  # Extraer habilidades únicas y sus etiquetas
  skills_data <- skill_pairs %>%
    select(skill1, name1) %>%
    rename(skill_id = skill1, skill_name = name1) %>%
    bind_rows(
      skill_pairs %>%
        select(skill2, name2) %>%
        rename(skill_id = skill2, skill_name = name2)
    ) %>%
    distinct() %>%
    arrange(skill_id)
  
  # Formatear aristas
  edges_df <- skill_pairs %>%
    select(from = skill1, to = skill2, weight) %>%
    distinct()
  
  # Crear dataframe de nodos
  nodes_df <- tibble(
    name = unique(skills_data$skill_id),
    label = sapply(unique(skills_data$skill_id), function(id) {
      # Encontrar el nombre de la habilidad correspondiente al ID
      name_idx <- which(skills_data$skill_id == id)[1]
      if(!is.na(name_idx)) {
        return(skills_data$skill_name[name_idx])
      }
      return(as.character(id))  # Fallback al ID como string
    })
  )
  
  # Crear objeto tidygraph
  skill_network <- tbl_graph(
    nodes = nodes_df, 
    edges = edges_df,
    directed = FALSE  # Red skill-skill es no dirigida
  )
  
  return(skill_network)
}

#' Prepara datos para graph-tool
#' @param network Red en formato tidygraph
#' @return Lista con datos formateados para graph-tool
prepare_for_gt <- function(network) {
  # Extraer datos de nodos
  nodes_df <- network %>% 
    activate(nodes) %>% 
    as_tibble() %>%
    mutate(id = row_number())  # Añadir IDs numéricos para graph-tool
  
  # Crear un mapeo de nombres de nodos a IDs numéricos
  node_id_map <- nodes_df %>%
    select(name, id) %>%
    deframe()
  
  # Extraer datos de aristas y mapear nombres a IDs
  edges_df <- network %>% 
    activate(edges) %>% 
    as_tibble() %>%
    mutate(
      from_id = node_id_map[from],
      to_id = node_id_map[to]
    )
  
  # Crear matriz de aristas
  edge_matrix <- as.matrix(edges_df[, c("from_id", "to_id", "weight")])
  
  # Devolver datos en formato necesario para graph-tool
  list(
    nodes = nodes_df,
    edges = edge_matrix,
    n_nodes = nrow(nodes_df),
    node_names = nodes_df$name,
    node_labels = nodes_df$label,
    node_id_map = node_id_map
  )
}

#' Procesa resultados del análisis SBM
#' @param py_result Resultados del análisis SBM en Python
#' @param gt_data Datos preparados para graph-tool
#' @return Lista con resultados procesados
process_sbm_results <- function(py_result, gt_data) {
  # Verificar si tenemos resultados válidos
  if (!py_result$success || is.null(py_result$result_json)) {
    stop("No hay resultados válidos para procesar")
  }
  
  # Extraer resultados JSON
  result <- jsonlite::fromJSON(py_result$result_json)
  
  # Inicializar lista para resultados
  sbm_data <- list()
  
  # Extraer información básica
  sbm_data$n_groups <- result$n_groups
  sbm_data$description_length <- result$description_length
  sbm_data$best_model <- result$best_model
  sbm_data$best_model_type <- result$best_model_type
  
  # Extraer comparación de modelos
  if (!is.null(result$model_comparison)) {
    sbm_data$model_comparison <- as.data.frame(result$model_comparison)
  }
  
  # Extraer información jerárquica
  if (!is.null(result$hierarchy_info)) {
    sbm_data$hierarchy_info <- as.data.frame(result$hierarchy_info)
  }
  
  # Extraer asignación de nodos a grupos
  if (!is.null(result$node_groups) && !is.null(gt_data)) {
    # Crear data frame con información de nodos
    node_groups <- data.frame(
      node_id = as.integer(names(result$node_groups)),
      group = as.integer(unlist(result$node_groups)),
      stringsAsFactors = FALSE
    )
    
    # Añadir nombres de nodos
    node_groups$name <- gt_data$nodes$name[node_groups$node_id + 1]
    node_groups$label <- gt_data$nodes$label[node_groups$node_id + 1]
    
    # Añadir meta-grupos si están disponibles
    if (!is.null(result$node_to_metagroup)) {
      metagroup_vector <- vector("integer", length(node_groups$node_id))
      
      for (i in seq_along(node_groups$node_id)) {
        node_id_str <- as.character(node_groups$node_id[i])
        if (node_id_str %in% names(result$node_to_metagroup)) {
          metagroup_vector[i] <- as.integer(result$node_to_metagroup[[node_id_str]])
        } else {
          metagroup_vector[i] <- -1
        }
      }
      
      node_groups$metagroup <- metagroup_vector
    }
    
    sbm_data$node_groups <- node_groups
  }
  
  # Extraer matriz de bloques
  if (!is.null(result$block_matrix)) {
    sbm_data$block_matrix <- matrix(
      unlist(result$block_matrix),
      nrow = result$n_groups,
      byrow = TRUE
    )
  }
  
  # NUEVO: Extraer mapeo jerárquico completo
  if (!is.null(result$node_hierarchy) && !is.null(gt_data)) {
    # Convertir el mapeo jerárquico a un dataframe
    node_hierarchy_list <- list()
    
    # Primero, determinar todas las columnas posibles
    all_level_cols <- unique(unlist(lapply(result$node_hierarchy, names)))
    
    for (node_id in names(result$node_hierarchy)) {
      node_data <- result$node_hierarchy[[node_id]]
      
      # Crear una fila para este nodo con todas las columnas posibles
      node_row <- data.frame(
        node_id = as.integer(node_id),
        stringsAsFactors = FALSE
      )
      
      # Inicializar todas las columnas con NA
      for (col in all_level_cols) {
        node_row[[col]] <- NA_integer_
      }
      
      # Añadir grupos para cada nivel disponible para este nodo
      for (level_name in names(node_data)) {
        node_row[[level_name]] <- as.integer(node_data[[level_name]])
      }
      
      # Añadir a la lista
      node_hierarchy_list[[length(node_hierarchy_list) + 1]] <- node_row
    }
    
    # Combinar en un dataframe
    if (length(node_hierarchy_list) > 0) {
      # Usar bind_rows en lugar de rbind para manejar diferentes columnas
      node_hierarchy_df <- dplyr::bind_rows(node_hierarchy_list)
      
      # Añadir nombres de nodos
      node_hierarchy_df$name <- gt_data$nodes$name[node_hierarchy_df$node_id + 1]
      node_hierarchy_df$label <- gt_data$nodes$label[node_hierarchy_df$node_id + 1]
      
      # Guardar en resultados
      sbm_data$node_hierarchy <- node_hierarchy_df
    }
  }
  
  # NUEVO: Extraer mapeos entre niveles
  if (!is.null(result$hierarchy_mappings)) {
    sbm_data$hierarchy_mappings <- result$hierarchy_mappings
  }
  
  return(sbm_data)
}

#----------------------------------------
# ANÁLISIS TEMPORAL: PROCESAMIENTO DE MÚLTIPLES PERÍODOS
#----------------------------------------

#' Ejecuta análisis SBM para múltiples períodos
#' @param results Lista con datos de red por período
#' @return Lista con resultados SBM por período
#' Ejecuta análisis SBM para múltiples períodos
#' @param results Lista con datos de red por período
#' @return Lista con resultados SBM por período
run_temporal_analysis <- function(results) {
  # Períodos disponibles
  periods <- names(results$network_data)
  
  # Lista para almacenar resultados por período
  sbm_results_by_period <- list()
  
  # Procesar cada período
  for(period in periods) {
    cat("\n\n========================================================\n")
    cat(paste0("PROCESANDO PERÍODO: ", period, "\n"))
    cat("========================================================\n\n")
    
    # Extraer datos de red de habilidades del período actual
    skill_pairs <- results[["network_data"]][[period]]$skill_pairs
    
    # Convertir pesos con coma a punto decimal si es necesario
    if(is.character(skill_pairs$weight)) {
      skill_pairs$weight <- as.numeric(gsub(",", ".", skill_pairs$weight))
    }
    
    # Crear red de habilidades
    skill_network <- prepare_skill_network(skill_pairs)
    
    # Imprimir resumen de la red
    n_nodes <- skill_network %>% activate(nodes) %>% as_tibble() %>% nrow()
    n_edges <- skill_network %>% activate(edges) %>% as_tibble() %>% nrow()
    
    cat("Resumen de la Red de Habilidades:\n")
    cat("Número de nodos:", n_nodes, "\n")
    cat("Número de aristas:", n_edges, "\n")
    
    # Preparar datos para graph-tool
    gt_data <- prepare_for_gt(skill_network)
    
    # Guardar datos en archivos temporales para Python
    temp_dir <- tempdir()
    
    # Crear un directorio específico para este período para evitar conflictos
    period_dir <- file.path(temp_dir, paste0("period_", gsub("-", "_", period)))
    dir.create(period_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Guardar nodos como CSV
    nodes_file <- file.path(period_dir, "nodes.csv")
    write.csv(gt_data$nodes, nodes_file, row.names = FALSE)
    
    # Guardar aristas como CSV
    edges_file <- file.path(period_dir, "edges.csv")
    write.csv(gt_data$edges, edges_file, row.names = FALSE)
    
    # Crear script Python para análisis SBM
    python_code <- paste0('
import graph_tool as gt
from graph_tool.inference import minimize_nested_blockmodel_dl, mcmc_equilibrate
import numpy as np
import json
import os
import traceback

try:
    # Datos del período
    period = "', period, '"
    n_nodes = ', n_nodes, '
    
    # Cargar datos de aristas desde archivo
    edges_file = "', edges_file, '"
    
    print(f"Procesando red de habilidades para el período {period}")
    print(f"Número de nodos: {n_nodes}")
    print(f"Archivo de aristas: {edges_file}")
    
    # Verificar que el archivo existe
    if not os.path.exists(edges_file):
        raise FileNotFoundError(f"No se encuentra el archivo de aristas: {edges_file}")
    
    # Leer archivo CSV de aristas
    edges_data = []
    with open(edges_file, "r") as f:
        # Saltar la primera línea (encabezados)
        next(f)
        for line in f:
            parts = line.strip().split(",")
            if len(parts) >= 3:
                try:
                    source = int(float(parts[0]))
                    target = int(float(parts[1]))
                    weight = float(parts[2])
                    edges_data.append((source, target, weight))
                except (ValueError, IndexError) as e:
                    print(f"Error al procesar línea: {line.strip()}, Error: {e}")
    
    print(f"Leídas {len(edges_data)} aristas del archivo")
    
    # Crear grafo no dirigido
    g = gt.Graph(directed=False)
    
    # Añadir vértices
    for i in range(n_nodes):
        g.add_vertex()
    
    # Crear propiedad de peso
    weights = g.new_edge_property("double")
    g.edge_properties["weight"] = weights
    
    # Añadir aristas
    edges_added = 0
    for source, target, weight in edges_data:
        try:
            source = source - 1  # Convertir a índice 0
            target = target - 1
            
            if 0 <= source < n_nodes and 0 <= target < n_nodes:
                e = g.add_edge(source, target)
                weights[e] = weight
                edges_added += 1
            else:
                print(f"Advertencia: Arista tiene índices inválidos: {source+1} -> {target+1}")
        except Exception as edge_error:
            print(f"Error procesando arista ({source+1}, {target+1}): {edge_error}")
    
    print(f"Añadidas exitosamente {edges_added} de {len(edges_data)} aristas")
    
    # Transformación logarítmica de pesos para modelo log-normal
    log_weights = g.new_edge_property("double")
    
    # Verificar si hay aristas con pesos positivos
    positive_weights = [weights[e] for e in g.edges() if weights[e] > 0]
    
    if len(positive_weights) > 0:
        min_weight = np.min(positive_weights)
        for e in g.edges():
            if weights[e] > 0:
                log_weights[e] = np.log(weights[e])
            else:
                log_weights[e] = np.log(min_weight / 10)
    else:
        # Si no hay pesos positivos, usar un valor predeterminado
        print("ADVERTENCIA: No hay aristas con pesos positivos. Usando valor predeterminado.")
        for e in g.edges():
            log_weights[e] = 0.0
    
    g.edge_properties["log_weight"] = log_weights
    
    # Calcular número máximo de grupos (limitado para evitar sobreajuste)
    # Usar una heurística más conservadora: log(n) * 2
    max_groups = int(np.log(n_nodes) * 2)
    min_groups = 2  # Al menos 2 grupos
    max_groups = max(min_groups, max_groups)
    print(f"Limitando el número de grupos entre {min_groups} y {max_groups}")
    
    # Definir modelos a probar
    models = []
    descriptions = []
    entropies = []
    model_types = []
    
    # Función para ejecutar MCMC con enfriamiento simulado
    def run_mcmc_with_annealing(state, n_sweeps=100):
        # Comenzar con temperatura alta (beta bajo) para explorar ampliamente
        beta_range = np.logspace(-1, 5, n_sweeps)
        print(f"Ejecutando {n_sweeps} iteraciones MCMC con enfriamiento simulado")
        for i, beta in enumerate(beta_range):
            if i % 10 == 0:
                print(f"  Iteración {i}, beta={beta:.4f}")
            state.multiflip_mcmc_sweep(beta=beta, niter=10)
        return state
    
    # Modelo 1: SBM anidado con distribución de peso Exponential
    print("Ajustando Modelo 1: SBM anidado con distribución de peso Exponential")
    state1 = minimize_nested_blockmodel_dl(
        g, 
        state_args=dict(
            recs=[weights], 
            rec_types=["real-exponential"], 
            deg_corr=True,
            B_min=min_groups,
            B_max=max_groups
        ),
        multilevel_mcmc_args=dict(
            beta=0.5,  # Temperatura más alta (beta más bajo) para explorar más
            niter=1000
        )
    )
    # Mejorar con MCMC y enfriamiento simulado
    state1 = run_mcmc_with_annealing(state1)
    models.append(state1)
    descriptions.append("Nested SBM with Exponential and Degree Correction")
    entropies.append(state1.entropy())
    model_types.append("nested")
    
    # Modelo 2: SBM anidado con distribución de peso Log-Normal
    print("Ajustando Modelo 2: SBM anidado con distribución de peso Log-Normal")
    state2 = minimize_nested_blockmodel_dl(
        g, 
        state_args=dict(
            recs=[log_weights], 
            rec_types=["real-normal"], 
            deg_corr=True,
            B_min=min_groups,
            B_max=max_groups
        ),
        multilevel_mcmc_args=dict(
            beta=0.5,
            niter=1000
        )
    )
    # Mejorar con MCMC y enfriamiento simulado
    state2 = run_mcmc_with_annealing(state2)
    models.append(state2)
    descriptions.append("Nested SBM with Log-Normal and Degree Correction")
    entropies.append(state2.entropy())
    model_types.append("nested")
    
    # Modelo 3: SBM anidado sin corrección de grado, pesos Exponential
    print("Ajustando Modelo 3: SBM anidado con pesos Exponential, sin corrección de grado")
    state3 = minimize_nested_blockmodel_dl(
        g, 
        state_args=dict(
            recs=[weights], 
            rec_types=["real-exponential"], 
            deg_corr=False,
            B_min=min_groups,
            B_max=max_groups
        ),
        multilevel_mcmc_args=dict(
            beta=0.5,
            niter=1000
        )
    )
    # Mejorar con MCMC y enfriamiento simulado
    state3 = run_mcmc_with_annealing(state3)
    models.append(state3)
    descriptions.append("Nested SBM with Exponential, no Degree Correction")
    entropies.append(state3.entropy())
    model_types.append("nested")
    
    # Modelo 4: SBM anidado sin corrección de grado, pesos Log-Normal
    print("Ajustando Modelo 4: SBM anidado con pesos Log-Normal, sin corrección de grado")
    state4 = minimize_nested_blockmodel_dl(
        g, 
        state_args=dict(
            recs=[log_weights], 
            rec_types=["real-normal"], 
            deg_corr=False,
            B_min=min_groups,
            B_max=max_groups
        ),
        multilevel_mcmc_args=dict(
            beta=0.5,
            niter=1000
        )
    )
    # Mejorar con MCMC y enfriamiento simulado
    state4 = run_mcmc_with_annealing(state4)
    models.append(state4)
    descriptions.append("Nested SBM with Log-Normal, no Degree Correction")
    entropies.append(state4.entropy())
    model_types.append("nested")
    
    # Encontrar el mejor modelo basado en entropía
    best_idx = np.argmin(entropies)
    best_state = models[best_idx]
    best_description = descriptions[best_idx]
    
    print(f"Comparación de modelos:")
    for i, (desc, entropy) in enumerate(zip(descriptions, entropies)):
        print(f"Modelo {i+1}: {desc}, Entropía: {entropy}")
    print(f"El mejor modelo es: {best_description} con entropía: {entropies[best_idx]}")
    
    # Extraer información del mejor modelo
    state = best_state
    
    # Equilibrar cadena de Markov con temperatura alta para evitar mínimos locales
    print("Equilibrando cadena de Markov con temperatura alta")
    mcmc_equilibrate(state, wait=100, mcmc_args=dict(niter=10, beta=0.5))
    
    # Recolectar muestras de la distribución posterior
    bs = []
    def collect_partitions(s):
        bs.append(s.get_bs())
    
    # Recolectar particiones con enfriamiento simulado
    print("Recolectando particiones con enfriamiento simulado")
    beta_range = np.logspace(-1, 5, 50)
    for beta in beta_range:
        mcmc_equilibrate(state, force_niter=10, mcmc_args=dict(niter=10, beta=beta),
                         callback=collect_partitions)
    
    # Extraer información de bloques del mejor modelo
    blocks = state.levels[0].get_blocks()
    
    # Convertir bloques a una lista de enteros
    blocks_list = [int(blocks[i]) for i in range(g.num_vertices())]
    n_groups = int(max(blocks_list) + 1)
    
    print(f"Número final de grupos: {n_groups}")
    
    # Obtener información jerárquica
    hierarchy_info = []
    for level_idx, level_state in enumerate(state.get_levels()):
        if level_state.get_N() == 1:
            break
        
        hierarchy_info.append({
            "level": level_idx,
            "n_nodes": int(level_state.get_N()),
            "n_groups": int(level_state.get_nonempty_B())
        })
    
    # Extraer la estructura jerárquica completa
    hierarchy_mappings = []
    for level_idx in range(len(state.get_levels()) - 1):  # Para cada nivel excepto el último
        if level_idx >= len(state.get_levels()) - 1:
            break

        current_level = state.levels[level_idx]
        next_level = state.levels[level_idx + 1]

        # Obtener bloques para este nivel y el siguiente
        current_blocks = current_level.get_blocks()
        next_blocks = next_level.get_blocks()

        # Crear mapeo para este nivel
        level_mapping = {}
        for i in range(current_level.get_N()):
            if i < next_level.get_N():
                level_mapping[str(i)] = int(next_blocks[i])

        # Guardar mapeo para este nivel
        hierarchy_mappings.append({
            "from_level": level_idx,
            "to_level": level_idx + 1,
            "mapping": level_mapping
        })
    
    # Crear mapeo completo de nodos a todos los niveles
    node_hierarchy = {}
    for i in range(g.num_vertices()):
        node_hierarchy[str(i)] = {}

        # Nivel 0
        level0_group = int(blocks[i])
        node_hierarchy[str(i)]["level0"] = level0_group

        # Niveles superiores
        current_group = level0_group
        for level_idx, level_mapping in enumerate(hierarchy_mappings):
            next_level = f"level{level_idx + 1}"
            if str(current_group) in level_mapping["mapping"]:
                current_group = level_mapping["mapping"][str(current_group)]
                node_hierarchy[str(i)][next_level] = current_group
            else:
                # Si no hay mapeo, usar un valor especial
                node_hierarchy[str(i)][next_level] = -1
                break  # No podemos continuar a niveles superiores
    
    # Extraer mapeo de estructura jerárquica original
    # Obtener el mapeo de grupos del nivel 0 a meta-grupos del nivel 1
    group_to_metagroup = {}
    if len(state.get_levels()) > 1:
        level1_blocks = state.levels[1].get_blocks()
        for i in range(state.levels[1].get_N()):
            if i < state.levels[1].get_N():
                group_to_metagroup[str(i)] = int(level1_blocks[i])
    
    # Mapear cada nodo a su meta-grupo
    node_to_metagroup = {}
    for i in range(g.num_vertices()):
        node_block = int(blocks[i])  # Grupo en nivel 0
        if str(node_block) in group_to_metagroup:
            meta_block = group_to_metagroup[str(node_block)]  # Meta-grupo en nivel 1
            node_to_metagroup[str(i)] = meta_block
        else:
            # Fallback si el mapeo está incompleto
            node_to_metagroup[str(i)] = -1
    
    # Calcular matriz de bloques
    block_matrix = np.zeros((n_groups, n_groups), dtype=float)
    for e in g.edges():
        s, t = int(e.source()), int(e.target())
        s_block, t_block = int(blocks[s]), int(blocks[t])
        if s_block < n_groups and t_block < n_groups:
            block_matrix[s_block, t_block] += weights[e]
            # Para redes no dirigidas, añadir en ambas direcciones (si no es un bucle)
            if s_block != t_block:
                block_matrix[t_block, s_block] += weights[e]
    
    # Obtener mapeo de nodo a grupo
    node_groups = {}
    for i in range(g.num_vertices()):
        node_groups[str(i)] = int(blocks[i])
    
    # Calcular estadísticas de grupo
    group_sizes = {}
    for i in range(n_groups):
        group_sizes[str(i)] = len([n for n in range(g.num_vertices()) if blocks[n] == i])
    
    # Almacenar resultados
    result = {
        "n_groups": int(n_groups),
        "node_groups": node_groups,
        "block_matrix": block_matrix.tolist(),
        "description_length": float(state.entropy()),
        "model_comparison": [
            {"name": desc, "entropy": float(ent), "type": typ} 
            for desc, ent, typ in zip(descriptions, entropies, model_types)
        ],
        "best_model": best_description,
        "best_model_type": model_types[best_idx],
        "hierarchy_info": hierarchy_info,
        "group_sizes": group_sizes,
        "group_to_metagroup": group_to_metagroup,
        "node_to_metagroup": node_to_metagroup,
        "hierarchy_mappings": hierarchy_mappings,
        "node_hierarchy": node_hierarchy
    }
    
    # Guardar resultados en archivo
    result_file = "', file.path(period_dir, "result.json"), '"
    with open(result_file, "w") as f:
        json.dump(result, f)
    
    # Indicar éxito
    print("Análisis completado con éxito")
    success = True
    result_json = json.dumps(result)

except Exception as e:
    import traceback
    error_details = {
        "error": str(e),
        "traceback": traceback.format_exc()
    }
    print(f"Error en ejecución Python para período {period}: {e}")
    print(traceback.format_exc())
    
    # Guardar error en archivo
    error_file = "', file.path(period_dir, "error.json"), '"
    with open(error_file, "w") as f:
        json.dump(error_details, f)
    
    success = False
    result_json = json.dumps(error_details)
')
    
    # Ejecutar el código Python
    py_result <- reticulate::py_run_string(python_code)
    
    # Verificar si se generó el archivo de resultados
    result_file <- file.path(period_dir, "result.json")
    if (file.exists(result_file)) {
      # Leer resultados del archivo
      result_json <- readLines(result_file, warn = FALSE)
      result_json <- paste(result_json, collapse = "")
      
      # Crear objeto de resultado
      py_result <- list(
        success = TRUE,
        result_json = result_json
      )
      
      # Procesar resultados
      sbm_results <- process_sbm_results(py_result, gt_data)
      
      # Añadir metadatos del período
      sbm_results$period <- period
      
      # Guardar resultados para este período
      sbm_results_by_period[[period]] <- sbm_results
      
      # Mostrar solo información básica del modelo
      cat("\n====== RESULTADOS SBM PARA EL PERÍODO", period, "======\n")
      cat("Mejor modelo:", sbm_results$best_model, "\n")
      cat("Número de grupos:", sbm_results$n_groups, "\n")
      cat("Longitud de descripción:", round(sbm_results$description_length), "\n")
      
      # Mostrar información jerárquica si está disponible
      if (!is.null(sbm_results$hierarchy_info)) {
        cat("\nEstructura jerárquica:\n")
        print(sbm_results$hierarchy_info)
      }
    } else {
      # Verificar si hay archivo de error
      error_file <- file.path(period_dir, "error.json")
      if (file.exists(error_file)) {
        error_json <- readLines(error_file, warn = FALSE)
        error_json <- paste(error_json, collapse = "")
        error_info <- jsonlite::fromJSON(error_json)
        
        cat("ERROR EN ANÁLISIS SBM PARA EL PERÍODO", period, ":\n")
        cat(error_info$error, "\n\n")
        cat(error_info$traceback, "\n\n")
      } else {
        cat("ERROR EN ANÁLISIS SBM PARA EL PERÍODO", period, ": No se generó archivo de resultados\n")
        
        # Mostrar salida de Python para depuración
        if (!is.null(py_result$stdout)) {
          cat("Salida de Python:\n")
          cat(py_result$stdout, "\n\n")
        }
        
        if (!is.null(py_result$stderr)) {
          cat("Errores de Python:\n")
          cat(py_result$stderr, "\n\n")
        }
      }
    }
    
    # Limpiar el entorno de Python para evitar conflictos entre períodos
    reticulate::py_run_string("
import gc
gc.collect()
")
  }
  
  # Devolver solo los resultados SBM por período
  return(sbm_results_by_period)
}

#----------------------------------------
# FUNCIONES DE ANÁLISIS DE RESULTADOS
#----------------------------------------

#' Analiza y visualiza la estructura jerárquica completa del SBM
#' @param sbm_result Resultado del análisis SBM para un período
#' @param max_level Nivel máximo a analizar (por defecto, todos)
#' @return Lista con análisis detallado de la estructura jerárquica
analyze_hierarchy <- function(sbm_result, max_level = NULL) {
  # Verificar si tenemos información jerárquica básica
  if (is.null(sbm_result$hierarchy_info)) {
    cat("No hay información jerárquica básica disponible en los resultados\n")
    return(NULL)
  }
  
  cat("Analizando estructura jerárquica para el período:", sbm_result$period, "\n\n")
  
  # Mostrar información jerárquica básica
  cat("Información jerárquica básica:\n")
  print(sbm_result$hierarchy_info)
  cat("\n")
  
  # Verificar si tenemos node_hierarchy
  if (!is.null(sbm_result$node_hierarchy)) {
    # Extraer datos de jerarquía
    hierarchy_data <- sbm_result$node_hierarchy
    
    # Identificar niveles disponibles
    level_cols <- names(hierarchy_data)[grepl("level", names(hierarchy_data))]
    
    # Limitar a max_level si se especifica
    if (!is.null(max_level)) {
      level_cols <- level_cols[as.numeric(gsub("level", "", level_cols)) <= max_level]
    }
    
    cat("Niveles disponibles en node_hierarchy:", paste(level_cols, collapse=", "), "\n\n")
    
    # Analizar cada nivel
    level_analysis <- list()
    
    for (level in level_cols) {
      level_num <- as.numeric(gsub("level", "", level))
      
      # Contar nodos por grupo en este nivel
      group_counts <- table(hierarchy_data[[level]])
      
      # Crear dataframe con conteos
      level_df <- data.frame(
        level = level_num,
        group = as.integer(names(group_counts)),
        count = as.integer(group_counts)
      )
      
      # Ordenar por tamaño de grupo
      level_df <- level_df[order(-level_df$count), ]
      
      # Guardar análisis de este nivel
      level_analysis[[level]] <- level_df
      
      # Mostrar resumen
      cat("Nivel", level_num, "- Grupos:", nrow(level_df), "\n")
      print(level_df)
      cat("\n")
    }
  } else {
    cat("No hay información detallada de node_hierarchy disponible\n\n")
    level_analysis <- NULL
  }
  
  # Analizar mapeos entre niveles
  if (!is.null(sbm_result$hierarchy_mappings)) {
    cat("\nMapeos entre niveles:\n")
    
    # Verificar si hierarchy_mappings es una lista
    if (is.list(sbm_result$hierarchy_mappings)) {
      for (i in seq_along(sbm_result$hierarchy_mappings)) {
        mapping <- sbm_result$hierarchy_mappings[[i]]
        
        # Verificar si mapping es una lista con los elementos esperados
        if (is.list(mapping) && all(c("from_level", "to_level", "mapping") %in% names(mapping))) {
          from_level <- mapping$from_level
          to_level <- mapping$to_level
          
          cat("Nivel", from_level, "->", "Nivel", to_level, ":\n")
          
          # Convertir mapeo a dataframe
          if (length(mapping$mapping) > 0) {
            mapping_df <- data.frame(
              from_group = as.integer(names(mapping$mapping)),
              to_group = as.integer(unlist(mapping$mapping))
            )
            
            # Ordenar por grupo de origen
            mapping_df <- mapping_df[order(mapping_df$from_group), ]
            
            print(mapping_df)
          } else {
            cat("  No hay mapeos disponibles\n")
          }
        } else {
          cat("  Mapeo", i, "tiene formato inesperado\n")
        }
        cat("\n")
      }
    } else {
      cat("  El formato de hierarchy_mappings no es una lista como se esperaba\n")
    }
  } else {
    cat("\nNo hay información de mapeos entre niveles disponible\n")
  }
  
  # Analizar grupos en el nivel 0 (básico)
  if (!is.null(sbm_result$node_groups)) {
    cat("\nAnálisis de grupos en el nivel 0:\n")
    
    # Contar nodos por grupo
    group_counts <- table(sbm_result$node_groups$group)
    
    # Mostrar distribución
    cat("Distribución de nodos por grupo:\n")
    print(group_counts)
    
    # Mostrar algunos ejemplos de nodos por grupo
    cat("\nEjemplos de habilidades por grupo:\n")
    
    for (g in names(group_counts)) {
      g_int <- as.integer(g)
      nodes_in_group <- sbm_result$node_groups[sbm_result$node_groups$group == g_int, ]
      
      if (nrow(nodes_in_group) > 0) {
        cat("Grupo", g, "-", nrow(nodes_in_group), "habilidades\n")
        
        # Mostrar hasta 5 ejemplos
        examples <- nodes_in_group$label[1:min(5, nrow(nodes_in_group))]
        cat("  Ejemplos:", paste(examples, collapse=", "), 
            ifelse(nrow(nodes_in_group) > 5, "...", ""), "\n\n")
      }
    }
  } else {
    cat("\nNo hay información de grupos en el nivel 0 disponible\n")
  }
  
  # Preparar datos para visualización
  viz_data <- list(
    period = sbm_result$period,
    hierarchy_info = sbm_result$hierarchy_info,
    levels = level_analysis,
    mappings = sbm_result$hierarchy_mappings,
    node_groups = sbm_result$node_groups
  )
  
  # Devolver análisis completo
  return(viz_data)
}

#' Extrae habilidades por grupo en cada nivel jerárquico
#' @param sbm_result Resultado del análisis SBM para un período
#' @param max_level Nivel máximo a analizar
#' @return Lista con habilidades agrupadas por nivel y grupo
extract_skills_by_hierarchy <- function(sbm_result, max_level = NULL) {
  # Verificar si tenemos información jerárquica básica
  if (is.null(sbm_result$hierarchy_info)) {
    cat("No hay información jerárquica básica disponible en los resultados\n")
    return(NULL)
  }
  
  # Determinar el número máximo de niveles
  max_available_level <- max(sbm_result$hierarchy_info$level)
  
  if (is.null(max_level) || max_level > max_available_level) {
    max_level <- max_available_level
  }
  
  cat("Extrayendo habilidades por grupo para el período:", sbm_result$period, "\n")
  cat("Analizando hasta el nivel:", max_level, "\n\n")
  
  # Lista para almacenar habilidades por nivel y grupo
  skills_by_level_group <- list()
  
  # Nivel 0 - Siempre disponible a través de node_groups
  if (!is.null(sbm_result$node_groups)) {
    level0_groups <- list()
    
    # Identificar grupos únicos en nivel 0
    unique_groups <- sort(unique(sbm_result$node_groups$group))
    
    # Para cada grupo, extraer habilidades
    for (group in unique_groups) {
      # Filtrar nodos en este grupo
      nodes_in_group <- sbm_result$node_groups[sbm_result$node_groups$group == group, ]
      
      # Extraer etiquetas de habilidades
      skills <- nodes_in_group$label
      
      # Guardar en la lista
      level0_groups[[as.character(group)]] <- skills
    }
    
    # Guardar grupos de nivel 0
    skills_by_level_group[["level0"]] <- level0_groups
    
    # Mostrar resumen del nivel 0
    cat("Nivel 0:\n")
    for (group in names(level0_groups)) {
      skills <- level0_groups[[group]]
      cat("  Grupo", group, "-", length(skills), "habilidades\n")
      
      # Mostrar algunas habilidades de ejemplo (máximo 5)
      if (length(skills) > 0) {
        sample_skills <- skills[1:min(5, length(skills))]
        cat("    Ejemplos:", paste(sample_skills, collapse=", "), 
            ifelse(length(skills) > 5, "...", ""), "\n")
      }
    }
    cat("\n")
  } else {
    cat("No hay información de grupos en el nivel 0 disponible\n\n")
  }
  
  # Niveles superiores - Requieren mapeos jerárquicos
  if (!is.null(sbm_result$hierarchy_mappings) && is.list(sbm_result$hierarchy_mappings)) {
    # Para cada nivel hasta max_level
    for (level_num in 1:max_level) {
      level_name <- paste0("level", level_num)
      
      # Buscar el mapeo del nivel anterior a este nivel
      mapping_idx <- NULL
      for (i in seq_along(sbm_result$hierarchy_mappings)) {
        mapping <- sbm_result$hierarchy_mappings[[i]]
        if (is.list(mapping) && 
            all(c("from_level", "to_level", "mapping") %in% names(mapping)) &&
            mapping$from_level == level_num - 1 && 
            mapping$to_level == level_num) {
          mapping_idx <- i
          break
        }
      }
      
      if (!is.null(mapping_idx)) {
        mapping <- sbm_result$hierarchy_mappings[[mapping_idx]]
        
        # Identificar grupos en este nivel
        if (length(mapping$mapping) > 0) {
          to_groups <- sort(unique(unlist(mapping$mapping)))
          
          # Crear lista para este nivel
          level_groups <- list()
          
          # Para cada grupo en este nivel
          for (to_group in to_groups) {
            # Encontrar grupos del nivel anterior que mapean a este grupo
            from_groups <- as.integer(names(mapping$mapping)[unlist(mapping$mapping) == to_group])
            
            # Si estamos en nivel 1, los from_groups son grupos del nivel 0
            if (level_num == 1) {
              # Recolectar todas las habilidades de los grupos del nivel 0
              all_skills <- c()
              for (from_group in from_groups) {
                if (as.character(from_group) %in% names(skills_by_level_group[["level0"]])) {
                  all_skills <- c(all_skills, skills_by_level_group[["level0"]][[as.character(from_group)]])
                }
              }
              
              # Guardar en la lista
              level_groups[[as.character(to_group)]] <- all_skills
            } else {
              # Para niveles superiores, necesitamos mapear a través de múltiples niveles
              # Esto es más complejo y requiere seguir la cadena de mapeos
              # Por simplicidad, solo mostramos los grupos que mapean a este grupo
              level_groups[[as.character(to_group)]] <- paste("Grupos del nivel", level_num - 1, ":", 
                                                              paste(from_groups, collapse=", "))
            }
          }
          
          # Guardar grupos de este nivel
          skills_by_level_group[[level_name]] <- level_groups
          
          # Mostrar resumen de este nivel
          cat("Nivel", level_num, ":\n")
          for (group in names(level_groups)) {
            skills <- level_groups[[group]]
            if (is.character(skills) && length(skills) == 1 && grepl("^Grupos del nivel", skills[1])) {
              # Es un mapeo de grupos, no habilidades directas
              cat("  Grupo", group, "-", skills, "\n")
            } else {
              # Son habilidades
              cat("  Grupo", group, "-", length(skills), "habilidades\n")
              
              # Mostrar algunas habilidades de ejemplo (máximo 5)
              if (length(skills) > 0) {
                sample_skills <- skills[1:min(5, length(skills))]
                cat("    Ejemplos:", paste(sample_skills, collapse=", "), 
                    ifelse(length(skills) > 5, "...", ""), "\n")
              }
            }
          }
          cat("\n")
        } else {
          cat("Nivel", level_num, ": No hay mapeos disponibles\n\n")
        }
      } else {
        cat("Nivel", level_num, ": No se encontró mapeo desde el nivel", level_num - 1, "\n\n")
      }
    }
  } else {
    cat("No hay información de mapeos jerárquicos disponible para niveles superiores\n")
  }
  
  # Devolver lista completa
  return(skills_by_level_group)
}

#' Visualiza la estructura jerárquica del SBM
#' @param sbm_result Resultado del análisis SBM para un período
#' @param max_level Nivel máximo a visualizar
#' @param output_dir Directorio para guardar visualizaciones
#' @return Lista con rutas a visualizaciones generadas
visualize_hierarchy <- function(sbm_result, max_level = NULL, output_dir = "visualizations") {
  # Verificar si tenemos información jerárquica básica
  if (is.null(sbm_result$hierarchy_info)) {
    cat("No hay información jerárquica básica disponible para visualizar\n")
    return(NULL)
  }
  
  # Crear directorio de salida si no existe
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Determinar el número máximo de niveles
  max_available_level <- max(sbm_result$hierarchy_info$level)
  
  if (is.null(max_level) || max_level > max_available_level) {
    max_level <- max_available_level
  }
  
  cat("Generando visualizaciones para el período:", sbm_result$period, "\n")
  cat("Visualizando hasta el nivel:", max_level, "\n\n")
  
  # Lista para almacenar rutas de visualizaciones
  viz_paths <- list()
  
  # Generar visualización de la estructura jerárquica básica
  cat("Generando visualización de la estructura jerárquica básica...\n")
  
  # Crear dataframe para visualización
  hierarchy_df <- sbm_result$hierarchy_info
  
  # Generar gráfico de barras de grupos por nivel
  p1 <- ggplot(hierarchy_df, aes(x = level, y = n_groups)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = n_groups), vjust = -0.5) +
    labs(
      title = paste("Estructura Jerárquica -", sbm_result$period),
      subtitle = "Número de grupos por nivel",
      x = "Nivel",
      y = "Número de grupos"
    ) +
    theme_minimal()
  
  # Guardar gráfico
  hierarchy_path <- file.path(output_dir, paste0("hierarchy_structure_", sbm_result$period, ".png"))
  ggsave(hierarchy_path, p1, width = 8, height = 6)
  viz_paths$hierarchy_structure <- hierarchy_path
  
  # Visualizar distribución de nodos por grupo en nivel 0
  if (!is.null(sbm_result$node_groups)) {
    cat("Generando visualización de grupos en el nivel 0...\n")
    
    # Contar nodos por grupo
    group_counts <- as.data.frame(table(sbm_result$node_groups$group))
    names(group_counts) <- c("group", "count")
    group_counts$group <- as.integer(as.character(group_counts$group))
    
    # Ordenar por tamaño de grupo
    group_counts <- group_counts[order(-group_counts$count), ]
    
    # Limitar a los 20 grupos más grandes para visualización
    if (nrow(group_counts) > 20) {
      top_groups <- group_counts[1:20, ]
      top_groups$group <- factor(top_groups$group, levels = top_groups$group)
    } else {
      top_groups <- group_counts
      top_groups$group <- factor(top_groups$group, levels = top_groups$group)
    }
    
    # Generar gráfico de barras
    p2 <- ggplot(top_groups, aes(x = group, y = count)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      geom_text(aes(label = count), vjust = -0.5) +
      labs(
        title = paste("Distribución de Habilidades por Grupo -", sbm_result$period),
        subtitle = paste("Nivel 0 -", ifelse(nrow(group_counts) > 20, "Top 20 grupos", "Todos los grupos")),
        x = "Grupo",
        y = "Número de habilidades"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Guardar gráfico
    groups_path <- file.path(output_dir, paste0("group_distribution_", sbm_result$period, ".png"))
    ggsave(groups_path, p2, width = 10, height = 6)
    viz_paths$group_distribution <- groups_path
    
    # Visualizar red de grupos (matriz de bloques)
    if (!is.null(sbm_result$block_matrix)) {
      cat("Generando visualización de la matriz de bloques...\n")
      
      # Convertir matriz de bloques a dataframe para ggplot
      block_df <- as.data.frame(as.table(sbm_result$block_matrix))
      names(block_df) <- c("row", "col", "weight")
      
      # Filtrar solo conexiones significativas
      block_df <- block_df[block_df$weight > 0, ]
      
      # Limitar a los grupos más grandes si hay muchos
      if (length(unique(c(block_df$row, block_df$col))) > 20) {
        top_group_ids <- as.integer(as.character(top_groups$group))
        block_df <- block_df[block_df$row %in% top_group_ids & block_df$col %in% top_group_ids, ]
      }
      
      # Generar heatmap
      p3 <- ggplot(block_df, aes(x = col, y = row, fill = weight)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "red") +
        labs(
          title = paste("Matriz de Bloques -", sbm_result$period),
          subtitle = "Intensidad de conexiones entre grupos (Nivel 0)",
          x = "Grupo destino",
          y = "Grupo origen",
          fill = "Peso"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank()
        )
      
      # Guardar heatmap
      blocks_path <- file.path(output_dir, paste0("block_matrix_", sbm_result$period, ".png"))
      ggsave(blocks_path, p3, width = 10, height = 8)
      viz_paths$block_matrix <- blocks_path
    }
  }
  
  # Generar informe HTML con todas las visualizaciones
  cat("Generando informe HTML con todas las visualizaciones...\n")
  
  # Crear contenido HTML
  html_content <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n",
    "<head>\n",
    "  <title>Análisis Jerárquico SBM - ", sbm_result$period, "</title>\n",
    "  <style>\n",
    "    body { font-family: Arial, sans-serif; margin: 20px; }\n",
    "    h1, h2 { color: #333366; }\n",
    "    .section { margin-bottom: 30px; }\n",
    "    .figure { margin: 20px 0; text-align: center; }\n",
    "    .figure img { max-width: 100%; border: 1px solid #ddd; }\n",
    "    table { border-collapse: collapse; width: 100%; }\n",
    "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }\n",
    "    th { background-color: #f2f2f2; }\n",
    "    tr:nth-child(even) { background-color: #f9f9f9; }\n",
    "  </style>\n",
    "</head>\n",
    "<body>\n",
    "  <h1>Análisis Jerárquico SBM - Período: ", sbm_result$period, "</h1>\n",
    "  \n",
    "  <div class='section'>\n",
    "    <h2>Información General</h2>\n",
    "    <p>Mejor modelo: ", sbm_result$best_model, "</p>\n",
    "    <p>Número de grupos en nivel 0: ", sbm_result$n_groups, "</p>\n",
    "    <p>Longitud de descripción: ", round(sbm_result$description_length, 2), "</p>\n",
    "  </div>\n",
    "  \n",
    "  <div class='section'>\n",
    "    <h2>Estructura Jerárquica</h2>\n",
    "    <table>\n",
    "      <tr><th>Nivel</th><th>Nodos</th><th>Grupos</th></tr>\n"
  )
  
  # Añadir filas de la tabla de estructura jerárquica
  for (i in 1:nrow(sbm_result$hierarchy_info)) {
    row <- sbm_result$hierarchy_info[i, ]
    html_content <- paste0(
      html_content,
      "      <tr><td>", row$level, "</td><td>", row$n_nodes, "</td><td>", row$n_groups, "</td></tr>\n"
    )
  }
  
  html_content <- paste0(
    html_content,
    "    </table>\n",
    "    \n",
    "    <div class='figure'>\n",
    "      <img src='", basename(viz_paths$hierarchy_structure), "' alt='Estructura Jerárquica'>\n",
    "      <p>Figura 1: Estructura Jerárquica - Número de grupos por nivel</p>\n",
    "    </div>\n",
    "  </div>\n"
  )
  
  # Añadir sección de distribución de grupos si está disponible
  if (!is.null(viz_paths$group_distribution)) {
    html_content <- paste0(
      html_content,
      "  <div class='section'>\n",
      "    <h2>Distribución de Habilidades por Grupo (Nivel 0)</h2>\n",
      "    <div class='figure'>\n",
      "      <img src='", basename(viz_paths$group_distribution), "' alt='Distribución de Grupos'>\n",
      "      <p>Figura 2: Distribución de habilidades por grupo en el nivel 0</p>\n",
      "    </div>\n",
      "  </div>\n"
    )
  }
  
  # Añadir sección de matriz de bloques si está disponible
  if (!is.null(viz_paths$block_matrix)) {
    html_content <- paste0(
      html_content,
      "  <div class='section'>\n",
      "    <h2>Matriz de Bloques (Nivel 0)</h2>\n",
      "    <div class='figure'>\n",
      "      <img src='", basename(viz_paths$block_matrix), "' alt='Matriz de Bloques'>\n",
      "      <p>Figura 3: Matriz de bloques mostrando la intensidad de conexiones entre grupos</p>\n",
      "    </div>\n",
      "  </div>\n"
    )
  }
  
  # Cerrar documento HTML
  html_content <- paste0(
    html_content,
    "</body>\n",
    "</html>"
  )
  
  # Guardar informe HTML
  html_path <- file.path(output_dir, paste0("sbm_report_", sbm_result$period, ".html"))
  writeLines(html_content, html_path)
  viz_paths$html_report <- html_path
  
  cat("Visualizaciones generadas y guardadas en:", output_dir, "\n")
  cat("Informe HTML:", html_path, "\n")
  
  # Devolver rutas de visualizaciones
  return(viz_paths)
}

# Para ejecutar el análisis completo:
results_temporales <- run_temporal_analysis(results)
glimpse(results)
# Para analizar la estructura jerárquica de un período específico:
# period <- "2005-2009"  # O cualquier otro período disponible
# hierarchy_analysis <- analyze_hierarchy(results_temporales[[period]])

# Para extraer habilidades por grupo en cada nivel:
# skills_hierarchy <- extract_skills_by_hierarchy(results_temporales[[period]])

# Para generar visualizaciones:
# viz_paths <- visualize_hierarchy(results_temporales[[period]], output_dir = "visualizations")




#' Analiza y visualiza la estructura jerárquica completa del SBM
#' @param sbm_result Resultado del análisis SBM para un período
#' @param max_level Nivel máximo a analizar (por defecto, todos)
#' @return Lista con análisis detallado de la estructura jerárquica
analyze_hierarchy <- function(sbm_result, max_level = NULL) {
  # Verificar si tenemos información jerárquica
  if (is.null(sbm_result$node_hierarchy)) {
    stop("No hay información jerárquica disponible en los resultados")
  }
  
  # Extraer datos de jerarquía
  hierarchy_data <- sbm_result$node_hierarchy
  
  # Identificar niveles disponibles
  level_cols <- names(hierarchy_data)[grepl("level", names(hierarchy_data))]
  
  # Limitar a max_level si se especifica
  if (!is.null(max_level)) {
    level_cols <- level_cols[as.numeric(gsub("level", "", level_cols)) <= max_level]
  }
  
  cat("Analizando estructura jerárquica para el período:", sbm_result$period, "\n")
  cat("Niveles disponibles:", paste(level_cols, collapse=", "), "\n\n")
  
  # Analizar cada nivel
  level_analysis <- list()
  
  for (level in level_cols) {
    level_num <- as.numeric(gsub("level", "", level))
    
    # Contar nodos por grupo en este nivel
    group_counts <- table(hierarchy_data[[level]])
    
    # Crear dataframe con conteos
    level_df <- data.frame(
      level = level_num,
      group = as.integer(names(group_counts)),
      count = as.integer(group_counts)
    )
    
    # Ordenar por tamaño de grupo
    level_df <- level_df[order(-level_df$count), ]
    
    # Guardar análisis de este nivel
    level_analysis[[level]] <- level_df
    
    # Mostrar resumen
    cat("Nivel", level_num, "- Grupos:", nrow(level_df), "\n")
    print(level_df)
    cat("\n")
  }
  
  # Analizar mapeos entre niveles
  if (!is.null(sbm_result$hierarchy_mappings)) {
    cat("\nMapeos entre niveles:\n")
    
    for (i in seq_along(sbm_result$hierarchy_mappings)) {
      mapping <- sbm_result$hierarchy_mappings[[i]]
      from_level <- mapping$from_level
      to_level <- mapping$to_level
      
      cat("Nivel", from_level, "->", "Nivel", to_level, ":\n")
      
      # Convertir mapeo a dataframe
      mapping_df <- data.frame(
        from_group = as.integer(names(mapping$mapping)),
        to_group = as.integer(unlist(mapping$mapping))
      )
      
      # Ordenar por grupo de origen
      mapping_df <- mapping_df[order(mapping_df$from_group), ]
      
      print(mapping_df)
      cat("\n")
    }
  }
  
  # Crear visualización de la estructura jerárquica
  cat("\nCreando visualización de la estructura jerárquica...\n")
  
  # Preparar datos para visualización
  viz_data <- list(
    period = sbm_result$period,
    levels = level_analysis,
    mappings = sbm_result$hierarchy_mappings,
    node_hierarchy = hierarchy_data
  )
  
  # Devolver análisis completo
  return(viz_data)
}

# Ejemplo de uso:
period <- "2005-2009"  # O cualquier otro período disponible
hierarchy_analysis <- analyze_hierarchy(results_temporales[[period]])




#' Visualiza la estructura jerárquica del SBM
#' @param hierarchy_analysis Resultado del análisis de jerarquía
#' @param max_level Nivel máximo a visualizar
#' @return Gráfico de la estructura jerárquica
visualize_hierarchy <- function(hierarchy_analysis, max_level = 3) {
  # Cargar bibliotecas necesarias
  library(ggplot2)
  library(igraph)
  library(ggraph)
  
  # Extraer datos
  period <- hierarchy_analysis$period
  levels <- hierarchy_analysis$levels
  mappings <- hierarchy_analysis$mappings
  
  # Limitar a max_level
  level_names <- names(levels)
  level_names <- level_names[as.numeric(gsub("level", "", level_names)) <= max_level]
  
  # Crear nodos para el grafo jerárquico
  nodes <- data.frame()
  
  for (level_name in level_names) {
    level_num <- as.numeric(gsub("level", "", level_name))
    level_data <- levels[[level_name]]
    
    # Añadir nodos para este nivel
    level_nodes <- data.frame(
      id = paste0("L", level_num, "_G", level_data$group),
      label = paste0("L", level_num, "_G", level_data$group),
      level = level_num,
      group = level_data$group,
      size = level_data$count
    )
    
    nodes <- rbind(nodes, level_nodes)
  }
  
  # Crear aristas entre niveles
  edges <- data.frame()
  
  for (i in seq_along(mappings)) {
    mapping <- mappings[[i]]
    from_level <- mapping$from_level
    to_level <- mapping$to_level
    
    if (from_level > max_level || to_level > max_level) {
      next
    }
    
    # Convertir mapeo a dataframe de aristas
    for (from_group in names(mapping$mapping)) {
      to_group <- mapping$mapping[[from_group]]
      
      edge <- data.frame(
        from = paste0("L", from_level, "_G", from_group),
        to = paste0("L", to_level, "_G", to_group)
      )
      
      edges <- rbind(edges, edge)
    }
  }
  
  # Crear grafo
  g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)
  
  # Asignar atributos
  V(g)$level <- nodes$level
  V(g)$size <- nodes$size
  
  # Crear layout por niveles
  layout <- create_layout(g, layout = "dendrogram", circular = FALSE)
  
  # Visualizar
  p <- ggraph(layout) + 
    geom_edge_link(arrow = arrow(length = unit(2, 'mm')), 
                   end_cap = circle(3, 'mm'),
                   start_cap = circle(3, 'mm'),
                   alpha = 0.7) + 
    geom_node_point(aes(size = size, color = as.factor(level)), alpha = 0.8) + 
    geom_node_text(aes(label = label), repel = TRUE, size = 3) +
    scale_size_continuous(range = c(3, 10)) +
    labs(title = paste("Estructura Jerárquica SBM -", period),
         subtitle = "Tamaño del nodo proporcional al número de habilidades",
         color = "Nivel") +
    theme_graph()
  
  # Mostrar gráfico
  print(p)
  
  # Devolver gráfico
  return(p)
}

# Ejemplo de uso:
hierarchy_viz <- visualize_hierarchy(hierarchy_analysis)


#' Extrae habilidades por grupo en cada nivel jerárquico
#' @param sbm_result Resultado del análisis SBM para un período
#' @param max_level Nivel máximo a analizar
#' @return Lista con habilidades agrupadas por nivel y grupo
extract_skills_by_hierarchy <- function(sbm_result, max_level = NULL) {
  # Verificar si tenemos información jerárquica
  if (is.null(sbm_result$node_hierarchy)) {
    stop("No hay información jerárquica disponible en los resultados")
  }
  
  # Extraer datos de jerarquía
  hierarchy_data <- sbm_result$node_hierarchy
  
  # Identificar niveles disponibles
  level_cols <- names(hierarchy_data)[grepl("level", names(hierarchy_data))]
  
  # Limitar a max_level si se especifica
  if (!is.null(max_level)) {
    level_cols <- level_cols[as.numeric(gsub("level", "", level_cols)) <= max_level]
  }
  
  # Lista para almacenar habilidades por nivel y grupo
  skills_by_level_group <- list()
  
  for (level in level_cols) {
    level_num <- as.numeric(gsub("level", "", level))
    
    # Crear lista para este nivel
    skills_by_level_group[[level]] <- list()
    
    # Identificar grupos únicos en este nivel
    unique_groups <- sort(unique(hierarchy_data[[level]]))
    
    # Para cada grupo, extraer habilidades
    for (group in unique_groups) {
      # Filtrar nodos en este grupo
      nodes_in_group <- hierarchy_data[hierarchy_data[[level]] == group, ]
      
      # Extraer etiquetas de habilidades
      skills <- nodes_in_group$label
      
      # Guardar en la lista
      skills_by_level_group[[level]][[as.character(group)]] <- skills
    }
  }
  
  # Crear un resumen
  cat("Resumen de habilidades por nivel y grupo para el período:", sbm_result$period, "\n\n")
  
  for (level in names(skills_by_level_group)) {
    level_num <- as.numeric(gsub("level", "", level))
    cat("Nivel", level_num, ":\n")
    
    for (group in names(skills_by_level_group[[level]])) {
      skills <- skills_by_level_group[[level]][[group]]
      cat("  Grupo", group, "-", length(skills), "habilidades\n")
      
      # Mostrar algunas habilidades de ejemplo (máximo 5)
      if (length(skills) > 0) {
        sample_skills <- skills[1:min(5, length(skills))]
        cat("    Ejemplos:", paste(sample_skills, collapse=", "), 
            ifelse(length(skills) > 5, "...", ""), "\n")
      }
    }
    cat("\n")
  }
  
  # Devolver lista completa
  return(skills_by_level_group)
}

# Ejemplo de uso:
period <- "2005-2009"  # O cualquier otro período disponible
skills_hierarchy <- extract_skills_by_hierarchy(results_temporales[[period]])




