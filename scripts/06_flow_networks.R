# ==============================================================================
# FLOW NETWORKS COMPLETOS - BASADOS EN TASAS DE ADOPCIÓN
# ==============================================================================

library(ggraph)
library(igraph)
library(dplyr)
library(tidyr)
library(tibble)
library(patchwork)
library(stringr)
library(ggplot2)

# ==============================================================================
# CONFIGURACIÓN GLOBAL
# ==============================================================================

verts <- tibble(name = paste0("Q", 1:5), x = 1:5, y = 0)
pal_direction <- c(Down = "#B2182B", Up = "#2166AC")

# Flecha triangular definida
arrow_sharp <- arrow(
  angle = 10,
  length = unit(7, "mm"),
  type = "closed"
)

gc()

# ==============================================================================
# CALCULAR TASAS POR EDGE
# ==============================================================================

rate_by_edge <- dt_model %>%
  as.data.frame() %>%
  filter(! is.na(s_wage), !is.na(t_wage)) %>%
  mutate(
    wage_q_src = suppressWarnings(ggplot2::cut_number(s_wage, n = 5, labels = paste0("Q", 1:5))),
    wage_q_dest = suppressWarnings(ggplot2::cut_number(t_wage, n = 5, labels = paste0("Q", 1:5))),
    q_src = as.integer(str_remove(wage_q_src, "Q")),
    q_dst = as.integer(str_remove(wage_q_dest, "Q")),
    direction = case_when(
      q_dst > q_src ~ "Up",
      q_dst < q_src ~ "Down",
      TRUE ~ "Lateral"
    )
  ) %>%
  filter(direction != "Lateral") %>%
  group_by(domain, wage_q_src, wage_q_dest, direction) %>%
  summarise(
    n_opp = n(),
    n_adopt = sum(diffusion == 1),
    rate = mean(diffusion == 1),
    .groups = "drop"
  )

# Normalizar tasas
rate_by_edge <- rate_by_edge %>%
  group_by(domain) %>%
  mutate(rate_norm = rate / max(rate)) %>%
  ungroup()

# ==============================================================================
# FIGURA 1: FLOW NETWORKS AGREGADOS (COGNITIVE VS PHYSICAL)
# ==============================================================================

message(">>> Generando Flow Networks Agregados <<<")

# COGNITIVE
flows_cog_rate <- rate_by_edge %>%
  filter(domain == "Cognitive") %>%
  arrange(desc(rate)) %>%
  slice_head(n = 8)

g_cog <- graph_from_data_frame(
  d = transmute(flows_cog_rate, from = wage_q_src, to = wage_q_dest, 
                weight = rate_norm, direction = direction),
  vertices = verts, directed = TRUE
)

p_cog_rate <- ggraph(g_cog, layout = "linear") +
  geom_edge_arc(
    aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
    strength = 0.3,
    arrow = arrow_sharp,
    end_cap = circle(9, "mm"),
    linejoin = "mitre",
    lineend = "butt",
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(1, 10), guide = "none") +
  scale_edge_alpha(range = c(0.5, 1), guide = "none") +
  scale_edge_colour_manual(values = pal_direction, name = "Direction") +
  geom_node_point(size = 18, color = "black", fill = "white", shape = 21, stroke = 1.5) +
  geom_node_text(aes(label = name), vjust = 0.4, size = 7, color = "black", fontface = "bold") +
  coord_cartesian(clip = "off") +
  labs(
    title = "A. Cognitive Skills",
    subtitle = "Upward adoption rate: 20. 5% | Downward: 14.9%",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#2166AC", face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

# PHYSICAL
flows_phy_rate <- rate_by_edge %>%
  filter(domain == "Physical") %>%
  arrange(desc(rate)) %>%
  slice_head(n = 8)

g_phy <- graph_from_data_frame(
  d = transmute(flows_phy_rate, from = wage_q_src, to = wage_q_dest, 
                weight = rate_norm, direction = direction),
  vertices = verts, directed = TRUE
)

p_phy_rate <- ggraph(g_phy, layout = "linear") +
  geom_edge_arc(
    aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
    strength = 0.3,
    arrow = arrow_sharp,
    end_cap = circle(9, "mm"),
    linejoin = "mitre",
    lineend = "butt",
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(1, 10), guide = "none") +
  scale_edge_alpha(range = c(0.5, 1), guide = "none") +
  scale_edge_colour_manual(values = pal_direction, name = "Direction") +
  geom_node_point(size = 18, color = "black", fill = "white", shape = 21, stroke = 1.5) +
  geom_node_text(aes(label = name), vjust = 0.4, size = 7, color = "black", fontface = "bold") +
  coord_cartesian(clip = "off") +
  labs(
    title = "B. Physical Skills",
    subtitle = "Downward adoption rate: 19.8% | Upward: 10.4%",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#B2182B", face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

# Leyenda
p_legend <- ggplot() +
  annotate("segment", x = 0.5, xend = 1.2, y = 0.5, yend = 0.5,
           color = "#2166AC", linewidth = 4,
           arrow = arrow(angle = 10, length = unit(7, "mm"), type = "closed")) +
  annotate("text", x = 1.4, y = 0.5, label = "Upward", 
           color = "#2166AC", size = 8, fontface = "bold", hjust = 0) +
  annotate("segment", x = 3, xend = 3.7, y = 0.5, yend = 0.5,
           color = "#B2182B", linewidth = 4,
           arrow = arrow(angle = 10, length = unit(7, "mm"), type = "closed")) +
  annotate("text", x = 3.9, y = 0.5, label = "Downward", 
           color = "#B2182B", size = 8, fontface = "bold", hjust = 0) +
  xlim(0, 5.5) +
  ylim(0, 1) +
  theme_void()

# Combinar
p_combined_rate <- (p_cog_rate | p_phy_rate) / p_legend +
  plot_layout(heights = c(10, 1)) +
  plot_annotation(
    #title = "Asymmetric Trajectory Channeling: Adoption Rates by Direction",
    subtitle = "Edge thickness proportional to adoption rate | Arrows indicate diffusion direction",
    caption = "Nodes = wage quintiles (Q1 low → Q5 high) | Physical skills face 2x upward friction",
    theme = theme(
      plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
      plot.subtitle = element_text(size = 20, hjust = 0.5, color = "grey30"),
      plot.caption = element_text(size = 16, hjust = 0.5, color = "grey50")
    )
  )

print(p_combined_rate)

#ggsave(file.path(output_data_dir, "flow_net_aggregated. png"),
#       p_combined_rate, width = 18, height = 10, dpi = 300)

# ==============================================================================
# FIGURA 2: SMALL MULTIPLES POR SKILL (BASADO EN TASAS)
# ==============================================================================

message(">>> Generando Small Multiples por Skill <<<")

# Calcular tasas por skill
rate_by_skill <- dt_model %>%
  as.data.frame() %>%
  filter(!is.na(s_wage), ! is.na(t_wage), ! is.na(skill_name)) %>%
  mutate(
    wage_q_src = suppressWarnings(ggplot2::cut_number(s_wage, n = 5, labels = paste0("Q", 1:5))),
    wage_q_dest = suppressWarnings(ggplot2::cut_number(t_wage, n = 5, labels = paste0("Q", 1:5))),
    q_src = as.integer(str_remove(wage_q_src, "Q")),
    q_dst = as.integer(str_remove(wage_q_dest, "Q")),
    direction = case_when(
      q_dst > q_src ~ "Up",
      q_dst < q_src ~ "Down",
      TRUE ~ "Lateral"
    )
  ) %>%
  filter(direction != "Lateral") %>%
  group_by(domain, skill_name, wage_q_src, wage_q_dest, direction) %>%
  summarise(
    n_opp = n(),
    n_adopt = sum(diffusion == 1),
    rate = mean(diffusion == 1),
    .groups = "drop"
  )

# Normalizar por skill
rate_by_skill <- rate_by_skill %>%
  group_by(domain, skill_name) %>%
  mutate(rate_norm = rate / max(rate, na.rm = TRUE)) %>%
  ungroup()

# Top skills por número de oportunidades
topK_skill <- 6
minN_opp <- 1000

top_skills <- rate_by_skill %>%
  group_by(domain, skill_name) %>%
  summarise(total_opp = sum(n_opp), .groups = "drop") %>%
  filter(total_opp >= minN_opp) %>%
  group_by(domain) %>%
  arrange(desc(total_opp)) %>%
  slice_head(n = topK_skill) %>%
  ungroup()

# Flecha para small multiples (más pequeña)
arrow_small <- arrow(angle = 12, length = unit(4, "mm"), type = "closed")

# Función para crear panel de un skill
create_skill_panel <- function(skill_data, skill_nm, show_legend = FALSE) {
  
  ed <- skill_data %>%
    arrange(desc(rate)) %>%
    slice_head(n = 6)
  
  if (nrow(ed) == 0) return(NULL)
  
  g <- graph_from_data_frame(
    transmute(ed, from = wage_q_src, to = wage_q_dest, weight = rate_norm, direction),
    verts, directed = TRUE
  )
  
  ggraph(g, layout = "linear") +
    geom_edge_arc(
      aes(edge_alpha = weight, edge_width = weight, edge_colour = direction),
      strength = 0.5,
      arrow = arrow_small,
      end_cap = circle(6, "mm"),
      linejoin = "mitre",
      show.legend = show_legend
    ) +
    scale_edge_width(range = c(0.5, 5), guide = "none") +
    scale_edge_alpha(range = c(0.4, 1), guide = "none") +
    scale_edge_colour_manual(values = pal_direction, name = "Direction") +
    geom_node_point(size = 10, color = "black", fill = "white", shape = 21, stroke = 1) +
    geom_node_text(aes(label = name), vjust = 0.4, size = 4, color = "black", fontface = "bold") +
    coord_cartesian(clip = "off") +
    labs(title = str_wrap(skill_nm, width = 25), x = NULL, y = NULL) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# COGNITIVE SKILLS
skills_cog <- top_skills %>% filter(domain == "Cognitive") %>% pull(skill_name)

plist_cog <- lapply(skills_cog, function(sk) {
  skill_data <- rate_by_skill %>% filter(domain == "Cognitive", skill_name == sk)
  create_skill_panel(skill_data, sk)
})
plist_cog <- plist_cog[!sapply(plist_cog, is.null)]

p_cog_skills <- wrap_plots(plist_cog, ncol = 3) +
  plot_annotation(
    title = "Cognitive Skills: Top Skills — Adoption Rate Flows",
    subtitle = "Edge thickness proportional to adoption rate | Arrows show diffusion direction",
    caption = "Nodes: wage quintiles (Q1 low … Q5 high)"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 12)
  )

print(p_cog_skills)

#ggsave(file. path(output_data_dir, "flow_skills_cognitive.png"),
#       p_cog_skills, width = 14, height = 10, dpi = 300)

# PHYSICAL SKILLS
skills_phy <- top_skills %>% filter(domain == "Physical") %>% pull(skill_name)

plist_phy <- lapply(skills_phy, function(sk) {
  skill_data <- rate_by_skill %>% filter(domain == "Physical", skill_name == sk)
  create_skill_panel(skill_data, sk)
})
plist_phy <- plist_phy[! sapply(plist_phy, is.null)]

p_phy_skills <- wrap_plots(plist_phy, ncol = 3) +
  plot_annotation(
    title = "Physical Skills: Top Skills — Adoption Rate Flows",
    subtitle = "Edge thickness proportional to adoption rate | Arrows show diffusion direction",
    caption = "Nodes: wage quintiles (Q1 low … Q5 high)"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    plot.caption = element_text(size = 12)
  )

print(p_phy_skills)

#ggsave(file.path(output_data_dir, "flow_skills_physical.png"),
#       p_phy_skills, width = 14, height = 10, dpi = 300)


# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

message("\n")
message("================================================================")
message("FIGURAS GENERADAS")
message("================================================================")
message("")
message("1.  flow_net_aggregated.png - Networks agregados Cognitive vs Physical")
message("2. flow_skills_cognitive.png - Small multiples top Cognitive skills")
message("3. flow_skills_physical.png - Small multiples top Physical skills")
message("4. occupation_examples_quintiles. png - Ejemplos de ocupaciones")
message("5.  occupation_quintile_examples.png - Lista limpia por quintil")
message("6. flow_composition.png - Composición de oportunidades")
message("")
message("================================================================")
message("ESTADÍSTICAS CLAVE:")
message("================================================================")
message("")
message("TASAS DE ADOPCIÓN:")
message("  Cognitive - Upward:   20.5%")
message("  Cognitive - Downward: 14.9%")
message("  → Diferencia: -5.6 p.p.  (facilita subida)")
message("")
message("  Physical - Upward:    10.4%")
message("  Physical - Downward:  19.8%")
message("  → Diferencia: +9.4 p.p. (penaliza subida)")
message("")
message("RATIO:")
message("  Physical Down/Up: 1.9x (casi el doble)")
message("================================================================")

gc()