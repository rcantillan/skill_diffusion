library(reticulate)
library(tidyverse)
library(jsonlite)

#' Análisis de Redes de Habilidades mediante Stochastic Block Models
#' 
#' Esta función analiza redes de habilidades utilizando diferentes variantes
#' del Stochastic Block Model (SBM) y compara su rendimiento.
#' 
#' @param results Lista de resultados que contiene los datos de la red
#' @param period Período a analizar (default: "2005-2009")
#' @return Lista con los resultados del análisis
analyze_skill_network_sbm <- function(results, period = "2005-2009") {
  
  # Extraer datos de la red de habilidades
  skill_pairs <- results$network_data[[period]]$skill_pairs
  # Extraer estadísticas de pares si están disponibles
  pair_stats <- results$network_data[[period]]$pair_stats
  
  # Imprime información sobre los datos
  cat("\n==================================================================\n")
  cat(sprintf("ANÁLISIS SBM PARA RED DE HABILIDADES: %s\n", period))
  cat("==================================================================\n\n")
  
  cat("INFORMACIÓN DEL DATASET:\n")
  cat("----------------------\n")
  cat(sprintf("- Período: %s\n", period))
  cat(sprintf("- Pares de habilidades: %d\n", nrow(skill_pairs)))
  
  # Extraer estadísticas únicas de habilidades
  unique_skills <- unique(c(skill_pairs$skill1, skill_pairs$skill2))
  
  cat(sprintf("- Habilidades únicas: %d\n", length(unique_skills)))
  cat(sprintf("- Rango de pesos: %.2f a %.2f\n", min(skill_pairs$weight), max(skill_pairs$weight)))
  cat(sprintf("- Peso mediano: %.2f\n\n", median(skill_pairs$weight)))
  
  # Preparar datos para graph-tool
  gt_data <- prepare_skill_network(skill_pairs)
  
  # Ejecutar análisis con Python/graph-tool
  py_run_string(paste0('
import graph_tool as gt
from graph_tool.inference import minimize_blockmodel_dl, minimize_nested_blockmodel_dl
from graph_tool.inference import mcmc_equilibrate, BlockState, NestedBlockState
import numpy as np
import json
import sys

try:
    # Crear grafo no dirigido para red de habilidades
    g = gt.Graph(directed=False)
    
    # Obtener datos de R
    edge_data = r.gt_data["edges"]
    n_nodes = r.gt_data["n_nodes"]
    node_names = r.gt_data["node_names"]
    
    # Añadir vértices
    for i in range(n_nodes):
        g.add_vertex()
    
    # Propiedad para nombres de nodos
    vname = g.new_vertex_property("string")
    for i in range(n_nodes):
        vname[g.vertex(i)] = node_names[i]
    g.vertex_properties["name"] = vname
    
    # Crear propiedad de peso para aristas
    weights = g.new_edge_property("double")
    g.edge_properties["weight"] = weights
    
    # Añadir aristas con programación defensiva
    edges_added = 0
    for i in range(edge_data.shape[0]):
        try:
            # Conversión explícita y verificación de límites
            source = int(edge_data[i, 0]) - 1  # Convertir a índice base 0
            target = int(edge_data[i, 1]) - 1
            weight = float(edge_data[i, 2])
            
            # Validar índices
            if 0 <= source < n_nodes and 0 <= target < n_nodes and source != target:
                e = g.add_edge(source, target)
                weights[e] = weight
                edges_added += 1
        except Exception as edge_error:
            print(f"Error procesando arista {i}: {edge_error}")
    
    print(f"Añadidas {edges_added} de {edge_data.shape[0]} aristas")
    
    # Log-transformación para modelo log-normal
    log_weights = g.new_edge_property("double")
    min_weight = np.min([weights[e] for e in g.edges() if weights[e] > 0])
    for e in g.edges():
        if weights[e] > 0:
            log_weights[e] = np.log(weights[e])
        else:
            log_weights[e] = np.log(min_weight / 10)  # Valor pequeño para pesos cero
    
    g.edge_properties["log_weight"] = log_weights
    
    # Definir y ejecutar diferentes modelos SBM
    models = []
    descriptions = []
    entropies = []
    model_types = []
    
    # Modelo 1: SBM básico (sin corrección de grado, sin pesos)
    print("Ajustando Modelo 1: SBM básico (sin corrección de grado, sin pesos)")
    state1 = minimize_blockmodel_dl(g, deg_corr=False)
    # Mejorar con MCMC
    for i in range(100):
        state1.multiflip_mcmc_sweep(niter=10, beta=np.inf)
    models.append(state1)
    descriptions.append("SBM básico (sin corrección de grado, sin pesos)")
    entropies.append(state1.entropy())
    model_types.append("flat")
    
    # Modelo 2: SBM con corrección de grado (sin pesos)
    print("Ajustando Modelo 2: SBM con corrección de grado (sin pesos)")
    state2 = minimize_blockmodel_dl(g, deg_corr=True)
    # Mejorar con MCMC
    for i in range(100):
        state2.multiflip_mcmc_sweep(niter=10, beta=np.inf)
    models.append(state2)
    descriptions.append("SBM con corrección de grado (sin pesos)")
    entropies.append(state2.entropy())
    model_types.append("flat")
    
    # Modelo 3: SBM con corrección de grado y pesos exponenciales
    print("Ajustando Modelo 3: SBM con corrección de grado y pesos exponenciales")
    state3 = minimize_blockmodel_dl(g, deg_corr=True, state_args=dict(
        recs=[weights], rec_types=["real-exponential"]
    ))
    # Mejorar con MCMC
    for i in range(100):
        state3.multiflip_mcmc_sweep(niter=10, beta=np.inf)
    models.append(state3)
    descriptions.append("SBM con corrección de grado y pesos exponenciales")
    entropies.append(state3.entropy())
    model_types.append("flat")
    
    # Modelo 4: SBM con corrección de grado y pesos log-normales
    print("Ajustando Modelo 4: SBM con corrección de grado y pesos log-normales")
    state4 = minimize_blockmodel_dl(g, deg_corr=True, state_args=dict(
        recs=[log_weights], rec_types=["real-normal"]
    ))
    # Mejorar con MCMC
    for i in range(100):
        state4.multiflip_mcmc_sweep(niter=10, beta=np.inf)
    models.append(state4)
    descriptions.append("SBM con corrección de grado y pesos log-normales")
    entropies.append(state4.entropy())
    model_types.append("flat")
    
    # Modelo 5: SBM jerárquico (nested) sin corrección de grado
    print("Ajustando Modelo 5: SBM jerárquico sin corrección de grado")
    state5 = minimize_nested_blockmodel_dl(g, deg_corr=False)
    # Mejorar con MCMC
    for i in range(100):
        state5.multiflip_mcmc_sweep(niter=10, beta=np.inf)
    models.append(state5)
    descriptions.append("SBM jerárquico sin corrección de grado")
    entropies.append(state5.entropy())
    model_types.append("nested")
    
    # Modelo 6: SBM jerárquico (nested) con corrección de grado
    print("Ajustando Modelo 6: SBM jerárquico con corrección de grado")
    state6 = minimize_nested_blockmodel_dl(g, deg_corr=True)
    # Mejorar con MCMC
    for i in range(100):
        state6.multiflip_mcmc_sweep(niter=10, beta=np.inf)
    models.append(state6)
    descriptions.append("SBM jerárquico con corrección de grado")
    entropies.append(state6.entropy())
    model_types.append("nested")
    
    # Modelo 7: SBM jerárquico con corrección de grado y pesos exponenciales
    print("Ajustando Modelo 7: SBM jerárquico con corrección de grado y pesos exponenciales")
    state7 = minimize_nested_blockmodel_dl(g, state_args=dict(
        recs=[weights], rec_types=["real-exponential"], deg_corr=True
    ))
    # Mejorar con MCMC
    for i in range(100):
        state7.multiflip_mcmc_sweep(niter=10, beta=np.inf)
    models.append(state7)
    descriptions.append("SBM jerárquico con corrección de grado y pesos exponenciales")
    entropies.append(state7.entropy())
    model_types.append("nested")
    
    # Modelo 8: SBM jerárquico con corrección de grado y pesos log-normales
    print("Ajustando Modelo 8: SBM jerárquico con corrección de grado y pesos log-normales")
    state8 = minimize_nested_blockmodel_dl(g, state_args=dict(
        recs=[log_weights], rec_types=["real-normal"], deg_corr=True
    ))
    # Mejorar con MCMC
    for i in range(100):
        state8.multiflip_mcmc_sweep(niter=10, beta=np.inf)
    models.append(state8)
    descriptions.append("SBM jerárquico con corrección de grado y pesos log-normales")
    entropies.append(state8.entropy())
    model_types.append("nested")
    
    # Encontrar el mejor modelo basado en la entropía
    best_idx = np.argmin(entropies)
    best_state = models[best_idx]
    best_description = descriptions[best_idx]
    best_model_type = model_types[best_idx]
    
    print("Comparación de modelos:")
    for i, (desc, entropy) in enumerate(zip(descriptions, entropies)):
        print(f"Modelo {i+1}: {desc}, Entropía: {entropy:.2f}")
    print(f"El mejor modelo es: {best_description} con entropía: {entropies[best_idx]:.2f}")
    
    # Obtener información del mejor modelo
    if best_model_type == "nested":
        # Para modelos jerárquicos
        blocks = best_state.levels[0].get_blocks()
        blocks_list = [int(blocks[i]) for i in range(g.num_vertices())]
        n_groups = int(max(blocks_list) + 1)
        
        # Información jerárquica
        hierarchy_info = []
        for level_idx, level_state in enumerate(best_state.get_levels()):
            if level_state.get_N() == 1:
                break
            
            hierarchy_info.append({
                "level": int(level_idx),
                "n_nodes": int(level_state.get_N()),
                "n_groups": int(level_state.get_nonempty_B())
            })
    else:
        # Para modelos planos
        blocks = best_state.get_blocks()
        blocks_list = [int(blocks[i]) for i in range(g.num_vertices())]
        n_groups = int(max(blocks_list) + 1)
        
        # No hay jerarquía en modelos planos
        hierarchy_info = [{
            "level": 0,
            "n_nodes": int(g.num_vertices()),
            "n_groups": int(n_groups)
        }]
    
    # Calcular matriz de bloques
    block_matrix = np.zeros((n_groups, n_groups), dtype=float)
    for e in g.edges():
        s, t = int(e.source()), int(e.target())
        s_block, t_block = int(blocks[s]), int(blocks[t])
        if s_block < n_groups and t_block < n_groups:  # Verificar límites
            block_matrix[s_block, t_block] += weights[e]
            # Para grafo no dirigido, añadir también la dirección opuesta
            block_matrix[t_block, s_block] += weights[e]
    
    # Almacenar resultados
    result = {
        "n_groups": int(n_groups),
        "blocks": blocks_list,
        "node_names": [str(node_names[i]) for i in range(len(node_names))],
        "block_matrix": block_matrix.tolist(),
        "description_length": float(best_state.entropy()),
        "model_comparison": [
            {"name": descriptions[i], "entropy": float(entropies[i]), "type": model_types[i]} 
            for i in range(len(descriptions))
        ],
        "best_model": best_description,
        "best_model_type": best_model_type,
        "hierarchy_info": hierarchy_info
    }
    
    # Convertir a JSON
    result_json = json.dumps(result)
    success = True

except Exception as e:
    import traceback
    error_details = {
        "error": str(e),
        "traceback": traceback.format_exc()
    }
    result_json = json.dumps(error_details)
    success = False
    print(f"Error en la ejecución de Python: {e}")
    print(traceback.format_exc())
  '))
  
  # Verificar si el análisis fue exitoso
  if (py$success) {
    # Extraer resultados
    result <- fromJSON(py$result_json)
    
    # Crear mapeo de nodos a grupos
    node_groups <- tibble(
      node_id = 0:(length(result$blocks) - 1),
      group = result$blocks,
      name = result$node_names
    )
    
    # Resumir grupos
    group_summary <- node_groups %>%
      group_by(group) %>%
      summarise(
        n = n(),
        members = paste(name, collapse = ", ")
      ) %>%
      arrange(desc(n))
    
    # Añadir estos elementos al resultado
    result$node_groups <- node_groups
    result$group_summary <- group_summary
    
    # Imprimir resultados del análisis
    print_sbm_results(result)
    
    return(result)
  } else {
    # Imprimir información de error
    cat("ERROR EN EL ANÁLISIS SBM:\n")
    cat("---------------------\n")
    error_info <- fromJSON(py$result_json)
    cat(error_info$error, "\n\n")
    cat("DETALLES DEL ERROR:\n")
    cat(error_info$traceback)
    
    return(NULL)
  }
}

#' Preparar datos de la red de habilidades para graph-tool
#' 
#' @param skill_pairs DataFrame con pares de habilidades
#' @return Lista con datos formateados para graph-tool
prepare_skill_network <- function(skill_pairs) {
  # Extraer habilidades únicas
  unique_skills <- unique(c(skill_pairs$skill1, skill_pairs$skill2))
  
  # Crear mapeo entre ID de habilidad y posición en la lista
  skill_to_idx <- setNames(seq_along(unique_skills) - 1, unique_skills)
  
  # Obtener nombres de habilidades
  skill_names <- character(length(unique_skills))
  for (i in seq_along(unique_skills)) {
    skill_id <- unique_skills[i]
    # Buscar el nombre en skill1/name1
    name_idx <- which(skill_pairs$skill1 == skill_id)
    if (length(name_idx) > 0) {
      skill_names[i] <- skill_pairs$name1[name_idx[1]]
    } else {
      # Buscar el nombre en skill2/name2
      name_idx <- which(skill_pairs$skill2 == skill_id)
      if (length(name_idx) > 0) {
        skill_names[i] <- skill_pairs$name2[name_idx[1]]
      } else {
        skill_names[i] <- skill_id  # Usar ID si no se encuentra nombre
      }
    }
  }
  
  # Crear matriz de aristas
  edges <- matrix(0, nrow = nrow(skill_pairs), ncol = 3)
  for (i in 1:nrow(skill_pairs)) {
    source <- skill_to_idx[skill_pairs$skill1[i]] + 1  # Ajustar a índice base 1 para R
    target <- skill_to_idx[skill_pairs$skill2[i]] + 1
    weight <- skill_pairs$weight[i]
    
    edges[i, ] <- c(source, target, weight)
  }
  
  # Devolver datos validados
  list(
    nodes = data.frame(
      id = 1:length(unique_skills),
      skill_id = unique_skills,
      name = skill_names
    ),
    edges = edges,
    n_nodes = length(unique_skills),
    node_names = skill_names
  )
}

#' Imprimir resultados del análisis SBM
#' 
#' @param result Resultados del análisis SBM
print_sbm_results <- function(result) {
  # Comparación de modelos
  cat("\nCOMPARACIÓN DE MODELOS:\n")
  cat("--------------------\n")
  
  model_comparison <- tibble(
    Modelo = sapply(result$model_comparison, `[[`, "name"),
    Tipo = sapply(result$model_comparison, `[[`, "type"),
    Entropía = sapply(result$model_comparison, function(x) round(x[["entropy"]], 2))
  ) %>% 
    arrange(Entropía)
  
  print(model_comparison)
  cat("\nMejor modelo seleccionado:", result$best_model, "\n\n")
  
  # Resumen del mejor modelo
  cat("RESUMEN DEL MEJOR MODELO:\n")
  cat("----------------------\n")
  cat(paste("Número de grupos detectados:", result$n_groups, "\n"))
  cat(paste("Longitud de descripción (entropía):", round(result$description_length, 2), "\n\n"))
  
  # Estructura jerárquica (si aplica)
  cat("ESTRUCTURA JERÁRQUICA:\n")
  cat("-------------------\n")
  print(as_tibble(result$hierarchy_info))
  cat("\n")
  
  # Resumen de grupos
  cat("COMUNIDADES DE HABILIDADES DETECTADAS:\n")
  cat("----------------------------------\n")
  print(result$group_summary %>% select(group, n))
  cat("\n")
  
  # Mostrar matriz de bloques si no es demasiado grande
  cat("MATRIZ DE INTERACCIÓN ENTRE BLOQUES (suma de pesos):\n")
  cat("-----------------------------------------------\n")
  block_matrix <- matrix(unlist(result$block_matrix), 
                         nrow = result$n_groups, 
                         byrow = TRUE)
  
  if (result$n_groups <= 10) {
    print(round(block_matrix, 1))
  } else {
    cat("Matriz demasiado grande para mostrar completa. Mostrando principales interacciones:\n\n")
    # Encontrar celdas con valores más altos
    flat_matrix <- as.vector(block_matrix)
    threshold <- sort(flat_matrix, decreasing = TRUE)[min(20, length(flat_matrix))]
    
    # Crear tabla de interacciones principales
    interactions <- tibble()
    for (i in 1:nrow(block_matrix)) {
      for (j in 1:ncol(block_matrix)) {
        if (block_matrix[i, j] >= threshold) {
          interactions <- bind_rows(interactions, 
                                    tibble(
                                      `Grupo Origen` = i-1,
                                      `Grupo Destino` = j-1,
                                      `Peso Total` = round(block_matrix[i, j], 1)
                                    ))
        }
      }
    }
    print(interactions %>% arrange(desc(`Peso Total`)))
  }
}

# Ejemplo de uso:
# result <- analyze_skill_network_sbm(periodo_data, "2005-2009")