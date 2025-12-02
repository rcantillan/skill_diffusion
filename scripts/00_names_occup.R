# ==============================================================================
# OBTENER NOMBRES DE OCUPACIONES DE O*NET Y CREAR FIGURA
# ==============================================================================

library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

# ==============================================================================
# OPCIÓN 1: CARGAR TÍTULOS DESDE O*NET 2024
# ==============================================================================

message(">>> Cargando títulos de ocupaciones desde O*NET <<<")

# Ruta a O*NET 2024 (la misma que usas en tu script)
path_2024 <- "/home/rober/Descargas/db_29_2_text"

# Buscar archivo de ocupaciones
occ_file <- file.path(path_2024, "Occupation Data.txt")
if (!file.exists(occ_file)) {
  # Buscar alternativas
  occ_files <- list.files(path_2024, pattern = "Occupation", full.names = TRUE)
  if (length(occ_files) > 0) occ_file <- occ_files[1]
}

# Leer archivo de ocupaciones
if (file.exists(occ_file)) {
  onet_occs <- fread(occ_file, sep = "\t", quote = "", na.strings = c("NA", "n/a", ""))
  setnames(onet_occs, old = names(onet_occs), new = make.names(names(onet_occs)))
  
  # Limpiar código SOC
  clean_id <- function(x) {
    s <- as.character(x)
    s <- gsub("-", "", s)
    s <- gsub("\\.00$", "", s)
    s <- gsub("\\.", "", s)
    return(stringr::str_trim(s))
  }
  
  onet_occs[, soc_code := clean_id(O.NET.SOC.Code)]
  onet_occs <- onet_occs[, .(soc_code, occ_title = Title)]
  
  message("Ocupaciones cargadas: ", nrow(onet_occs))
  print(head(onet_occs))
} else {
  stop("No se encontró archivo de ocupaciones en: ", path_2024)
}

# ==============================================================================
# CREAR TABLA DE OCUPACIONES POR QUINTIL
# ==============================================================================

message(">>> Creando tabla de ocupaciones por quintil <<<")

# Obtener ocupaciones únicas con sus salarios desde dt_model
occ_wages <- dt_model %>%
  as.data.frame() %>%
  filter(! is.na(s_wage)) %>%
  distinct(source, s_wage) %>%
  rename(soc_code = source) %>%
  mutate(soc_code = as.character(soc_code))

# Hacer merge con títulos de O*NET
occ_wages <- occ_wages %>%
  left_join(onet_occs, by = "soc_code")

# Verificar merge
message("Ocupaciones con título: ", sum(! is.na(occ_wages$occ_title)), " de ", nrow(occ_wages))

# Asignar quintiles
occ_with_quintiles <- occ_wages %>%
  filter(!is.na(occ_title)) %>%
  mutate(
    wage_quintile = suppressWarnings(
      ggplot2::cut_number(s_wage, n = 5, labels = paste0("Q", 1:5))
    )
  ) %>%
  group_by(wage_quintile) %>%
  arrange(s_wage) %>%
  mutate(
    rank_in_quintile = row_number(),
    n_in_quintile = n()
  ) %>%
  ungroup()

# Seleccionar ejemplos representativos (4 por quintil)
occ_examples <- occ_with_quintiles %>%
  group_by(wage_quintile) %>%
  arrange(desc(s_wage)) %>%
  slice_head(n = 4) %>%
  mutate(
    occ_short = str_trunc(occ_title, 35),
    wage_formatted = scales::dollar(s_wage, accuracy = 1)
  ) %>%
  select(wage_quintile, occ_short, wage_formatted, s_wage) %>%
  ungroup()

# Imprimir tabla
message("\n=== EJEMPLOS DE OCUPACIONES POR QUINTIL ===\n")
for (q in paste0("Q", 1:5)) {
  message(paste0("\n", q, ":"))
  occ_q <- occ_examples %>% filter(wage_quintile == q)
  for (i in 1:nrow(occ_q)) {
    message(paste0("  • ", occ_q$occ_short[i], " (", occ_q$wage_formatted[i], ")"))
  }
}

# ==============================================================================
# CREAR FIGURA
# ==============================================================================

message(">>> Generando figura de ocupaciones por quintil <<<")

occ_for_plot <- occ_examples %>%
  group_by(wage_quintile) %>%
  summarise(
    occupations = paste(occ_short, collapse = "\n"),
    .groups = "drop"
  )

p_occ_list <- ggplot(occ_for_plot, aes(x = wage_quintile, y = 1)) +
  geom_tile(aes(fill = wage_quintile), alpha = 0.2, color = "grey80", linewidth = 1) +
  geom_text(aes(label = occupations), size = 4, lineheight = 1.4, vjust = 0.5) +
  scale_fill_manual(values = c(
    Q1 = "#B2182B", Q2 = "#EF8A62", Q3 = "#999999", Q4 = "#67A9CF", Q5 = "#2166AC"
  ), guide = "none") +
  scale_y_continuous(limits = c(0.2, 1.8)) +
  labs(
    title = "Representative Occupations by Wage Quintile",
    subtitle = "Q1 = lowest wages → Q5 = highest wages",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "grey40"),
    axis.text.x = element_text(size = 18, face = "bold", margin = margin(t = 15)),
    plot.margin = margin(30, 30, 30, 30)
  )

print(p_occ_list)

ggsave(file.path(output_data_dir, "occupation_quintile_examples.png"),
       p_occ_list, width = 16, height = 8, dpi = 300)

# ==============================================================================
# TABLA ALTERNATIVA: CON RANGOS DE SALARIO
# ==============================================================================

# Calcular rangos de salario por quintil
quintile_ranges <- occ_with_quintiles %>%
  group_by(wage_quintile) %>%
  summarise(
    min_wage = min(s_wage),
    max_wage = max(s_wage),
    n_occs = n(),
    .groups = "drop"
  ) %>%
  mutate(
    range_label = paste0(scales::dollar(min_wage, accuracy = 1), " - ", 
                         scales::dollar(max_wage, accuracy = 1))
  )

message("\n=== RANGOS DE SALARIO POR QUINTIL ===\n")
print(quintile_ranges)

# Figura con rangos
occ_for_plot2 <- occ_examples %>%
  group_by(wage_quintile) %>%
  summarise(
    occupations = paste(occ_short, collapse = "\n"),
    .groups = "drop"
  ) %>%
  left_join(quintile_ranges, by = "wage_quintile")

p_occ_with_range <- ggplot(occ_for_plot2, aes(x = wage_quintile, y = 1)) +
  geom_tile(aes(fill = wage_quintile), alpha = 0.15, color = "grey70", linewidth = 1.2) +
  # Rango de salario arriba
  geom_text(aes(y = 1.55, label = range_label), size = 3.5, fontface = "italic", color = "grey40") +
  # Ocupaciones en el centro
  geom_text(aes(label = occupations), size = 4, lineheight = 1.3, vjust = 0.5) +
  scale_fill_manual(values = c(
    Q1 = "#B2182B", Q2 = "#EF8A62", Q3 = "#999999", Q4 = "#67A9CF", Q5 = "#2166AC"
  ), guide = "none") +
  scale_y_continuous(limits = c(0.3, 1.7)) +
  labs(
    title = "Representative Occupations by Wage Quintile",
    subtitle = "Q1 = lowest wages → Q5 = highest wages | Salary ranges shown above",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey40"),
    axis.text.x = element_text(size = 18, face = "bold", margin = margin(t = 15)),
    plot.margin = margin(30, 30, 30, 30)
  )

print(p_occ_with_range)

ggsave(file.path(output_data_dir, "occupation_quintile_with_ranges.png"),
       p_occ_with_range, width = 18, height = 9, dpi = 300)

message("\n>>> Figuras guardadas <<<")



# ==============================================================================
# OPCIÓN 1: QUINTILES COMO NODOS CON OCUPACIONES DEBAJO (ESTILO FLOW NETWORK)
# ==============================================================================

library(ggplot2)
library(dplyr)
library(stringr)

# Preparar datos (asumiendo que ya tienes occ_examples del código anterior)
occ_for_viz <- occ_examples %>%
  group_by(wage_quintile) %>%
  summarise(
    occupations = paste(str_trunc(occ_short, 30), collapse = "\n"),
    .groups = "drop"
  ) %>%
  left_join(quintile_ranges, by = "wage_quintile") %>%
  mutate(
    x = as.numeric(gsub("Q", "", wage_quintile)),
    y = 0
  )

# Colores consistentes con flow network
pal_quintile <- c(Q1 = "#B2182B", Q2 = "#EF8A62", Q3 = "#888888", Q4 = "#67A9CF", Q5 = "#2166AC")

p_quintile_nodes <- ggplot(occ_for_viz, aes(x = x, y = y)) +
  # Línea horizontal conectando quintiles
  geom_segment(aes(x = 1, xend = 5, y = 0, yend = 0), 
               color = "grey70", linewidth = 2, lineend = "round") +
  # Nodos (círculos blancos con borde)
  geom_point(size = 22, color = "black", fill = "white", shape = 21, stroke = 2) +
  # Labels de quintil dentro del nodo
  geom_text(aes(label = wage_quintile), size = 8, fontface = "bold") +
  # Rango de salario ARRIBA del nodo
  geom_text(aes(y = 0.35, label = range_label), size = 3.5, color = "grey40", fontface = "italic") +
  # Ocupaciones DEBAJO del nodo
  geom_text(aes(y = -0.5, label = occupations), size = 3.2, lineheight = 1.1, vjust = 1) +
  # Escala y límites
  scale_x_continuous(limits = c(0.3, 5.7), breaks = NULL) +
  scale_y_continuous(limits = c(-1.5, 0.6), breaks = NULL) +
  # Anotaciones de extremos
  annotate("text", x = 1, y = 0.55, label = "Low Wage", size = 4, fontface = "bold", color = "#B2182B") +
  annotate("text", x = 5, y = 0.55, label = "High Wage", size = 4, fontface = "bold", color = "#2166AC") +
  # Flechas indicando dirección
  annotate("segment", x = 1.5, xend = 4.5, y = 0.55, yend = 0.55,
           arrow = arrow(length = unit(3, "mm"), type = "closed"),
           color = "grey50", linewidth = 0.8) +
  labs(
    title = "Wage Quintiles: Representative Occupations",
    subtitle = "Nodes represent wage quintiles (same as flow network) | Example occupations shown below"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey40"),
    plot.margin = margin(20, 40, 20, 40)
  )

print(p_quintile_nodes)

ggsave(file. path(output_data_dir, "quintile_nodes_occupations.png"),
       p_quintile_nodes, width = 18, height = 10, dpi = 300)


# ==============================================================================
# OPCIÓN 2: NODOS CON COLOR GRADIENTE (MÁS VISUAL)
# ==============================================================================

p_quintile_gradient <- ggplot(occ_for_viz, aes(x = x, y = y)) +
  # Línea horizontal con gradiente implícito
  geom_segment(aes(x = 1, xend = 5, y = 0, yend = 0), 
               color = "grey80", linewidth = 3) +
  # Nodos con color de quintil (borde coloreado)
  geom_point(aes(color = wage_quintile), size = 24, fill = "white", shape = 21, stroke = 3) +
  scale_color_manual(values = pal_quintile, guide = "none") +
  # Labels de quintil
  geom_text(aes(label = wage_quintile), size = 9, fontface = "bold") +
  # Rango de salario arriba
  geom_label(aes(y = 0.4, label = range_label), size = 3.5, 
             fill = "white", label.size = 0, color = "grey30") +
  # Ocupaciones debajo
  geom_text(aes(y = -0.55, label = occupations), size = 3.3, lineheight = 1.15, vjust = 1) +
  scale_x_continuous(limits = c(0.2, 5.8), breaks = NULL) +
  scale_y_continuous(limits = c(-1. 6, 0.7), breaks = NULL) +
  labs(
    title = "Wage Quintiles: Representative Occupations",
    subtitle = "Each node represents a wage quintile | Color intensity indicates wage level"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot. title = element_text(face = "bold", size = 24, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5, color = "grey40"),
    plot.margin = margin(25, 50, 25, 50)
  )

print(p_quintile_gradient)

ggsave(file.path(output_data_dir, "quintile_nodes_gradient.png"),
       p_quintile_gradient, width = 20, height = 11, dpi = 300)


# ==============================================================================
# OPCIÓN 3: INTEGRADO CON EL FLOW NETWORK (COMBINADO)
# ==============================================================================

# Esta opción pone los ejemplos de ocupaciones DEBAJO del flow network

# Primero crear el flow network (reusar el código anterior)
# ...  (código de p_combined_rate)

# Luego crear panel de ocupaciones
p_occ_panel <- ggplot(occ_for_viz, aes(x = x, y = 0)) +
  geom_text(aes(label = occupations), size = 3, lineheight = 1.1) +
  geom_text(aes(y = 0.8, label = wage_quintile), size = 6, fontface = "bold") +
  geom_text(aes(y = 0.55, label = range_label), size = 2. 8, color = "grey50") +
  scale_x_continuous(limits = c(0. 5, 5.5)) +
  scale_y_continuous(limits = c(-0.8, 1)) +
  theme_void() +
  theme(plot.margin = margin(5, 20, 5, 20))

# Combinar con patchwork
# p_final <- p_combined_rate / p_occ_panel + plot_layout(heights = c(4, 1))


# ==============================================================================
# OPCIÓN 4: TABLA HORIZONTAL SIMPLE (ALTERNATIVA LIMPIA)
# ==============================================================================

# Más ocupaciones, formato más compacto
occ_expanded <- occ_with_quintiles %>%
  group_by(wage_quintile) %>%
  arrange(desc(s_wage)) %>%
  slice_head(n = 6) %>%
  mutate(occ_short = str_trunc(occ_title, 28)) %>%
  ungroup()

occ_for_table <- occ_expanded %>%
  group_by(wage_quintile) %>%
  summarise(
    occupations = paste(occ_short, collapse = "\n"),
    . groups = "drop"
  ) %>%
  left_join(quintile_ranges, by = "wage_quintile") %>%
  mutate(x = as.numeric(gsub("Q", "", wage_quintile)))

p_table_clean <- ggplot(occ_for_table, aes(x = x, y = 0)) +
  # Header con quintil y rango
  geom_tile(aes(y = 1. 2, fill = wage_quintile), height = 0.4, width = 0.9, alpha = 0.3) +
  geom_text(aes(y = 1. 2, label = paste0(wage_quintile, "\n", range_label)), 
            size = 4, fontface = "bold", lineheight = 0.9) +
  # Ocupaciones
  geom_text(aes(label = occupations), size = 3. 2, lineheight = 1.2, vjust = 1) +
  scale_fill_manual(values = pal_quintile, guide = "none") +
  scale_x_continuous(limits = c(0.3, 5.7)) +
  scale_y_continuous(limits = c(-1. 2, 1. 5)) +
  labs(
    title = "Wage Quintile Reference: Example Occupations",
    subtitle = "Use this guide to interpret skill flow patterns between quintiles"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "grey40"),
    plot.margin = margin(20, 30, 20, 30)
  )

print(p_table_clean)

ggsave(file.path(output_data_dir, "quintile_reference_table.png"),
       p_table_clean, width = 18, height = 8, dpi = 300)

message(">>> Figuras de referencia de quintiles guardadas <<<")


# ==============================================================================
# OPCIÓN A: ESTILO "ESCALERA" VERTICAL CON OCUPACIONES A LOS LADOS
# ==============================================================================

library(ggplot2)
library(dplyr)
library(stringr)

# Preparar datos - menos ocupaciones, nombres más cortos
occ_viz <- occ_with_quintiles %>%
  group_by(wage_quintile) %>%
  arrange(desc(s_wage)) %>%
  slice_head(n = 3) %>%  # Solo 3 por quintil
  mutate(occ_short = str_trunc(occ_title, 25)) %>%  # Nombres más cortos
  ungroup()

occ_summary <- occ_viz %>%
  group_by(wage_quintile) %>%
  summarise(
    occupations = paste(occ_short, collapse = "\n"),
    .groups = "drop"
  ) %>%
  left_join(quintile_ranges, by = "wage_quintile") %>%
  mutate(
    y = as.numeric(gsub("Q", "", wage_quintile)),
    x = 0
  )

pal_quintile <- c(Q1 = "#B2182B", Q2 = "#EF8A62", Q3 = "#888888", Q4 = "#67A9CF", Q5 = "#2166AC")

p_ladder <- ggplot(occ_summary, aes(x = x, y = y)) +
  # Línea vertical conectando
  geom_segment(aes(x = 0, xend = 0, y = 0.7, yend = 5.3), 
               color = "grey70", linewidth = 3) +
  # Nodos
  geom_point(aes(fill = wage_quintile), size = 20, shape = 21, stroke = 2, color = "black") +
  geom_text(aes(label = wage_quintile), size = 7, fontface = "bold") +
  scale_fill_manual(values = pal_quintile, guide = "none") +
  # Ocupaciones a la DERECHA
  geom_text(aes(x = 0.15, label = occupations), hjust = 0, size = 4.5, lineheight = 1.2) +
  # Rango salarial a la IZQUIERDA
  geom_text(aes(x = -0.1, label = range_label), hjust = 1, size = 4, color = "grey40", fontface = "italic") +
  # Límites y escalas
  scale_x_continuous(limits = c(-0.5, 1.2)) +
  scale_y_continuous(limits = c(0.3, 5.7)) +
  # Anotaciones
  annotate("text", x = 0, y = 5.6, label = "HIGH WAGE", fontface = "bold", size = 5, color = "#2166AC") +
  annotate("text", x = 0, y = 0.5, label = "LOW WAGE", fontface = "bold", size = 5, color = "#B2182B") +
  labs(
    title = "Wage Quintiles: Representative Occupations",
    subtitle = "Each node = one wage quintile | Example occupations shown to the right"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "grey40"),
    plot.margin = margin(30, 30, 30, 30)
  )

print(p_ladder)

ggsave(file. path(output_data_dir, "quintile_ladder.png"),
       p_ladder, width = 14, height = 12, dpi = 300)


# ==============================================================================
# OPCIÓN B: FORMATO HORIZONTAL CON CAJAS GRANDES Y TEXTO LEGIBLE
# ==============================================================================

# Usando geom_tile con texto más grande
p_boxes <- ggplot(occ_summary, aes(x = as.numeric(gsub("Q", "", wage_quintile)), y = 0)) +
  # Cajas de fondo
  geom_tile(aes(fill = wage_quintile), width = 0.9, height = 1.8, alpha = 0.3, color = "grey50") +
  # Quintil label arriba
  geom_text(aes(y = 0.7, label = wage_quintile), size = 10, fontface = "bold") +
  # Rango salarial
  geom_text(aes(y = 0.4, label = range_label), size = 4, color = "grey30") +
  # Ocupaciones - UNA POR LÍNEA, más grandes
  geom_text(aes(y = -0.1, label = occupations), size = 4.2, lineheight = 1.3, vjust = 1) +
  scale_fill_manual(values = pal_quintile, guide = "none") +
  scale_x_continuous(limits = c(0.3, 5.7), breaks = NULL) +
  scale_y_continuous(limits = c(-1.2, 1), breaks = NULL) +
  labs(
    title = "Wage Quintiles: Representative Occupations",
    subtitle = "Q1 = lowest wages → Q5 = highest wages"
  ) +
  theme_void(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "grey40"),
    plot.margin = margin(30, 20, 30, 20)
  )

print(p_boxes)

ggsave(file.path(output_data_dir, "quintile_boxes.png"),
       p_boxes, width = 22, height = 10, dpi = 300)


# ==============================================================================
# OPCIÓN C: INTEGRADO DIRECTAMENTE CON EL FLOW NETWORK
# ==============================================================================

# Esta es probablemente la MEJOR opción: poner las ocupaciones
# directamente debajo de cada nodo en el flow network

# Crear datos para anotaciones debajo de cada nodo
occ_annotations <- occ_viz %>%
  group_by(wage_quintile) %>%
  summarise(
    label = paste(str_trunc(occ_short, 22), collapse = "\n"),
    .groups = "drop"
  ) %>%
  mutate(x = as.numeric(gsub("Q", "", wage_quintile)))

# Modificar el flow network para incluir ocupaciones
p_flow_with_occs <- ggraph(g_cog, layout = "linear") +
  geom_edge_arc(
    aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
    strength = 0.3,
    arrow = arrow(angle = 10, length = unit(7, "mm"), type = "closed"),
    end_cap = circle(12, "mm"),
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(1, 10), guide = "none") +
  scale_edge_alpha(range = c(0.5, 1), guide = "none") +
  scale_edge_colour_manual(values = pal_direction) +
  # Nodos más grandes
  geom_node_point(size = 22, color = "black", fill = "white", shape = 21, stroke = 2) +
  geom_node_text(aes(label = name), vjust = 0.4, size = 8, fontface = "bold") +
  # OCUPACIONES DEBAJO DE CADA NODO
  geom_text(data = occ_annotations, aes(x = x, y = -0.8, label = label),
            size = 3.5, lineheight = 1.1, inherit.aes = FALSE) +
  coord_cartesian(clip = "off", ylim = c(-2, 1.5)) +
  labs(
    title = "Cognitive Skills: Flow Network with Example Occupations",
    subtitle = "Upward adoption rate: 20. 5% | Downward: 14.9%"
  ) +
  theme_void(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "#2166AC", face = "bold"),
    plot.margin = margin(20, 40, 60, 40)
  )

print(p_flow_with_occs)

ggsave(file.path(output_data_dir, "flow_with_occupations.png"),
       p_flow_with_occs, width = 16, height = 12, dpi = 300)


# ==============================================================================
# OPCIÓN D: TABLA SIMPLE PERO ELEGANTE (PARA APPENDIX)
# ==============================================================================

library(gt)  # Si tienes gt instalado

# Si no tienes gt, usar kableExtra o crear imagen manual
occ_table_data <- occ_viz %>%
  select(wage_quintile, occ_title, s_wage) %>%
  mutate(wage_formatted = scales::dollar(s_wage)) %>%
  select(-s_wage)

# Versión ggplot como tabla
occ_wide <- occ_viz %>%
  group_by(wage_quintile) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  select(wage_quintile, row, occ_short) %>%
  tidyr::pivot_wider(names_from = wage_quintile, values_from = occ_short)

p_table <- ggplot() +
  # Header
  annotate("rect", xmin = 0.5, xmax = 5.5, ymin = 3.5, ymax = 4.2, fill = "grey20") +
  annotate("text", x = 1:5, y = 3.85, 
           label = c("Q1\n$19k-$30k", "Q2\n$30k-$38k", "Q3\n$38k-$50k", "Q4\n$51k-$68k", "Q5\n$68k-$175k"),
           color = "white", size = 5, fontface = "bold", lineheight = 0.9) +
  # Rows
  annotate("text", x = 1, y = 3:1, label = occ_wide$Q1, size = 4, hjust = 0.5) +
  annotate("text", x = 2, y = 3:1, label = occ_wide$Q2, size = 4, hjust = 0.5) +
  annotate("text", x = 3, y = 3:1, label = occ_wide$Q3, size = 4, hjust = 0.5) +
  annotate("text", x = 4, y = 3:1, label = occ_wide$Q4, size = 4, hjust = 0.5) +
  annotate("text", x = 5, y = 3:1, label = occ_wide$Q5, size = 4, hjust = 0.5) +
  # Grid lines
  annotate("segment", x = 0.5, xend = 5.5, y = c(0.5, 1.5, 2.5, 3.5), yend = c(0.5, 1.5, 2.5, 3.5), color = "grey80") +
  annotate("segment", x = seq(0.5, 5.5, 1), xend = seq(0.5, 5.5, 1), y = 0.5, yend = 4.2, color = "grey80") +
  scale_x_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(title = "Representative Occupations by Wage Quintile") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, margin = margin(b = 20))
  )

print(p_table)

ggsave(file.path(output_data_dir, "quintile_table_clean.png"),
       p_table, width = 16, height = 8, dpi = 300)

message(">>> Todas las opciones de visualización guardadas <<<")