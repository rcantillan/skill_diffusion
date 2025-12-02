# ==============================================================================
# MASTER SCRIPT: SKILL DIFFUSION DATA CONSTRUCTION (V12 FINAL)
# Logic: Strict Flow (Risk Set 2015->2024) + Baseline Status Gaps (t0)
# Author: Roberto Cantillan | Date: Nov 30, 2025
# ==============================================================================

# --- 0. SETUP & LIBRERÍAS ---
gc()
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, readxl, progress, Matrix, lsa, cluster, igraph, stringr, dplyr, here)

# --- 1. DEFINICIÓN DE RUTAS (AJUSTAR) ---
path_2015           <- "/home/rober/Descargas/db_15_1"       # Baseline (t0)
path_2024           <- "/home/rober/Descargas/db_29_2_text" # Outcome (t1)
crosswalk_data_path <- "/home/rober/Documentos/skill_diffusion/data/crosswalk/"
data_external_path  <- "/home/rober/Documentos/skill_diffusion/data/"
output_data_dir     <- "datos_eventos_v12_FINAL"

if (!dir.exists(output_data_dir)) dir.create(output_data_dir, recursive = TRUE)

# Archivos auxiliares
file_cw_soc10_19 <- "2010_to_2019_Crosswalk.csv"
file_bls_15      <- "national_M2015_dl.xlsx"

# ==============================================================================
# 2. FUNCIONES DE LIMPIEZA Y CARGA
# ==============================================================================

clean_id <- function(x) {
  s <- as.character(x)
  s <- gsub("-", "", s); s <- gsub("\\.00$", "", s); s <- gsub("\\.", "", s)
  return(stringr::str_trim(s))
}

load_onet <- function(folder, filename) {
  f <- file.path(folder, filename)
  if (!file.exists(f)) { # Búsqueda flexible
    pat <- gsub(".txt", "", filename)
    f_alt <- list.files(folder, pattern = pat, full.names = TRUE)
    if(length(f_alt) > 0) f <- f_alt[1] else return(NULL)
  }
  
  dt <- fread(f, sep = "\t", quote = "", na.strings = c("NA", "n/a", "", "*"), showProgress=FALSE)
  setnames(dt, old=names(dt), new=make.names(names(dt))) # Scale ID -> Scale.ID
  
  if ("O.NET.SOC.Code" %in% names(dt)) dt[, soc_code := clean_id(O.NET.SOC.Code)]
  
  val_col <- grep("Data.?Value", names(dt), value=TRUE)[1]
  if(!is.na(val_col)) dt[, value := as.numeric(get(val_col))]
  
  return(dt)
}

calc_rca <- function(dt) {
  # Agregación por si hay duplicados tras el crosswalk
  agg <- dt[, .(v = mean(value, na.rm=TRUE)), by=.(soc_code, Element.Name)]
  
  occ_sum <- agg[, .(ot = sum(v)), by=soc_code]
  ski_sum <- agg[, .(st = sum(v)), by=Element.Name]
  gt      <- sum(agg$v)
  
  agg <- merge(agg, occ_sum, by="soc_code")
  agg <- merge(agg, ski_sum, by="Element.Name")
  agg[, rca := (v/ot)/(st/gt)]
  return(agg[, .(soc_code, skill = Element.Name, rca)])
}

# ==============================================================================
# 3. PROCESAMIENTO BASELINE (2015 - t0)
# ==============================================================================
message("\n>>> 1. PROCESANDO 2015 (Baseline)...")

files <- c("Skills.txt", "Abilities.txt", "Knowledge.txt", "Work Activities.txt")
dt_15 <- rbindlist(lapply(files, function(x) load_onet(path_2015, x)), fill=TRUE)
dt_15 <- dt_15[Scale.ID == "IM" & !is.na(value)]

# --- Crosswalk SOC 2010 -> 2019 ---
cw_file <- file.path(crosswalk_data_path, file_cw_soc10_19)
if(file.exists(cw_file)) {
  cw <- fread(cw_file)
  cw_clean <- unique(cw[, .(
    soc10 = clean_id(`O*NET-SOC 2010 Code`), 
    soc19 = clean_id(`O*NET-SOC 2019 Code`)
  )])
  # Merge permitiendo expansión (1 old -> N new)
  dt_15 <- merge(dt_15, cw_clean, by.x="soc_code", by.y="soc10", all.x=FALSE, allow.cartesian=TRUE)
  dt_15[, soc_code := soc19]
} else { warning("⚠️ Crosswalk 2010-2019 no encontrado.") }

rca_15 <- calc_rca(dt_15)
setnames(rca_15, "rca", "rca_t0")
message("Ocupaciones 2015 (Mapped): ", uniqueN(rca_15$soc_code))
rm(dt_15, cw, cw_clean); gc()

# ==============================================================================
# 4. PROCESAMIENTO OUTCOME (2024 - t1)
# ==============================================================================
message("\n>>> 2. PROCESANDO 2024 (Outcome)...")

dt_24 <- rbindlist(lapply(files, function(x) load_onet(path_2024, x)), fill=TRUE)
dt_24 <- dt_24[Scale.ID == "IM" & !is.na(value)]
rca_24 <- calc_rca(dt_24)
setnames(rca_24, "rca", "rca_t1")
rm(dt_24); gc()

# ==============================================================================
# 5. CLASIFICACIÓN & DISTANCIA (ESTRUCTURA t0)
# ==============================================================================
message("\n>>> 3. CALCULANDO ESTRUCTURA DE RED (2015)...")

mat_dt <- rca_15[rca_t0 > 1]
skill_classes <- data.table(skill = unique(rca_15$skill), domain = "Unknown")
dist_dt <- NULL

if(nrow(mat_dt) > 0) {
  # --- A. Clusters de Skills (Louvain 2015) ---
  mat_wide <- dcast(mat_dt, skill ~ soc_code, value.var="rca_t0", fun.aggregate=length)
  mat <- as.matrix(mat_wide[, -1]); rownames(mat) <- mat_wide$skill
  
  sim <- lsa::cosine(t(mat)); sim[is.nan(sim)] <- 0; diag(sim) <- 0
  g <- graph_from_adjacency_matrix(sim, mode="undirected", weighted=TRUE)
  g <- delete_edges(g, E(g)[weight < 0.1]) 
  lou <- cluster_louvain(g)
  
  skill_classes <- data.table(skill = lou$names, cluster = paste0("C", lou$membership))
  # Asignar dominio (Heurística simple, refinar si tienes labels)
  # Asumimos que el cluster más grande o central es cognitivo/social. 
  # Aquí C1 = Cognitive como placeholder.
  skill_classes[, domain := fifelse(cluster == "C1", "Cognitive", "Physical")]
  
  # --- B. Distancia Estructural (Jaccard 2015) ---
  message("   Calculando Jaccard Matrix (Ocupación x Ocupación)...")
  occ_wide <- dcast(mat_dt, soc_code ~ skill, fun.aggregate=length, value.var="rca_t0")
  occ_ids <- occ_wide$soc_code
  occ_mat <- as.matrix(occ_wide[, -1]); occ_mat <- (occ_mat > 0) * 1
  
  M <- Matrix::Matrix(occ_mat, sparse=TRUE)
  intersect <- tcrossprod(M)
  rs <- rowSums(M); union <- outer(rs, rs, "+") - as.matrix(intersect)
  jaccard <- as.matrix(intersect) / union; jaccard[is.na(jaccard)] <- 0
  
  # Convertir a tabla larga eficiente
  rownames(jaccard) <- occ_ids; colnames(jaccard) <- occ_ids
  dist_dt <- as.data.table(as.table(jaccard))
  setnames(dist_dt, c("source", "target", "sim"))
  dist_dt <- dist_dt[source != target & sim > 0] # Solo guardar > 0 para ahorrar RAM
  dist_dt[, structural_distance := 1 - sim]
  dist_dt[, sim := NULL]
  
  rm(M, intersect, union, jaccard, mat, sim, g, mat_dt, occ_mat); gc()
}

# ==============================================================================
# 6. CONSTRUCCIÓN DEL PANEL (RISK SET & FLOW)
# ==============================================================================
message("\n>>> 4. CONSTRUYENDO DIADAS (LÓGICA DE FLUJO)...")

# Unir T0 y T1
panel <- merge(rca_15, rca_24, by=c("soc_code", "skill"), all=TRUE)
panel[is.na(rca_t0), rca_t0 := 0]
panel[is.na(rca_t1), rca_t1 := 0]

# --- DEFINICIÓN ESTRICTA ---
# Source (t0): Expertos en 2015
sources_t0 <- panel[rca_t0 > 1.0, .(source = soc_code, skill)]

# Risk Set (t0): Quienes NO la tenían en 2015 (Potenciales adoptantes)
targets_risk <- panel[rca_t0 <= 1.0, .(target = soc_code, skill, rca_t1)]
targets_risk[, diffusion := fifelse(rca_t1 > 1.0, 1L, 0L)]

message("   Risk Set Total (Pares Target-Skill): ", format(nrow(targets_risk), big.mark=","))

# --- GENERACIÓN DE DIADAS ---
# Iteramos por skill para cruzar Sources x RiskSet
all_dyads_list <- list()
skills_vec <- unique(sources_t0$skill)
pb <- progress_bar$new(total = length(skills_vec))

for(sk in skills_vec) {
  pb$tick()
  srcs <- sources_t0[skill == sk]$source
  tgts <- targets_risk[skill == sk]
  
  if(length(srcs) == 0 || nrow(tgts) == 0) next
  
  # Sampling de targets (Balanceo opcional para memoria)
  # Si > 2000 negativos, sampleamos. Mantenemos todos los positivos.
  pos <- tgts[diffusion == 1]
  neg <- tgts[diffusion == 0]
  if(nrow(neg) > 2000) neg <- neg[sample(.N, 2000)]
  tgts_final <- rbind(pos, neg)
  
  # Expansión
  pairs <- as.data.table(expand.grid(source = srcs, target = tgts_final$target, stringsAsFactors = FALSE))
  pairs <- pairs[source != target]
  
  # Pegar outcome
  pairs <- merge(pairs, tgts_final[, .(target, diffusion)], by="target")
  pairs[, skill_name := sk]
  
  all_dyads_list[[sk]] <- pairs
}

all_events <- rbindlist(all_dyads_list)
rm(all_dyads_list, panel, sources_t0, targets_risk); gc()
message("\nBase de Eventos: ", format(nrow(all_events), big.mark=","))

# ==============================================================================
# 7. ENRIQUECIMIENTO Y STATUS GAPS (t0 - t0) - CORREGIDO
# ==============================================================================
message("\n>>> 5. ENRIQUECIENDO (SALARIOS 2015 & GAPS)...")

# Cargar Salarios 2015 (BLS)
get_wages <- function(file) {
  f <- file.path(data_external_path, file)
  if(!file.exists(f)) return(NULL)
  d <- as.data.table(read_excel(f, sheet=1))
  
  # Detección dinámica de columnas
  cc <- grep("OCC_CODE", names(d), value=TRUE)[1]
  cw <- grep("A_MEDIAN", names(d), value=TRUE)[1]
  
  if(is.na(cc) || is.na(cw)) return(NULL)
  
  # Limpieza y conversión
  d[, w := suppressWarnings(as.numeric(as.character(get(cw))))]
  # CORRECCIÓN AQUÍ: Usamos 'clean_id' que es como se definió al inicio
  d[, c := clean_id(get(cc))] 
  
  return(d[!is.na(w), .(wage = mean(w)), by=c])
}

w15 <- get_wages(file_bls_15)
if(is.null(w15)) stop("No se pudo cargar salarios 2015. Revisa la ruta o el nombre del archivo.")

# --- LÓGICA CLAVE: GAP = TARGET(t0) - SOURCE(t0) ---
# Source Wage (2015)
s_wage <- copy(w15)
setnames(s_wage, c("c", "wage"), c("source", "s_wage"))
all_events <- merge(all_events, s_wage, by="source", all.x=TRUE)

# Target Wage (2015) -> Estatus PRE-adopción
t_wage <- copy(w15)
setnames(t_wage, c("c", "wage"), c("target", "t_wage"))
all_events <- merge(all_events, t_wage, by="target", all.x=TRUE)

# Calcular ATC Gaps
all_events[, wage_diff_rel := (t_wage - s_wage) / s_wage]

# Pegar Skill Domain
all_events <- merge(all_events, skill_classes[, .(skill, domain)], 
                    by.x="skill_name", by.y="skill", all.x=TRUE)

# Pegar Distancia Estructural
if(!is.null(dist_dt)) {
  message("   Pegando distancias...")
  setkey(all_events, source, target)
  setkey(dist_dt, source, target)
  all_events <- merge(all_events, dist_dt, all.x=TRUE)
}
all_events[is.na(structural_distance), structural_distance := 1]

message("Enriquecimiento completado.")

# ==============================================================================
# 8. DIAGNÓSTICO FINAL Y GUARDADO
# ==============================================================================
message("\n>>> 6. DIAGNÓSTICO FINAL <<<")

# Filtrar completos
final_dt <- all_events[!is.na(wage_diff_rel) & !is.na(domain)]
final_dt[, year_adoption := 2023] # Metadata para el modelo

# Métricas
n_rows <- nrow(final_dt)
rate <- mean(final_dt$diffusion)
n_src <- uniqueN(final_dt$source)
n_tgt <- uniqueN(final_dt$target)

message("Observaciones Finales: ", format(n_rows, big.mark=","))
message("Tasa de Difusión (Flow): ", round(rate*100, 2), "%")
message("Ocupaciones Fuente: ", n_src)
message("Ocupaciones Destino: ", n_tgt)

# Chequeo de Balance de Gaps
gap_stats <- final_dt[, .(
  Upward_Pct = mean(wage_diff_rel > 0),
  Downward_Pct = mean(wage_diff_rel < 0)
)]
print(gap_stats)

# Guardar
save_path <- file.path(output_data_dir, "all_events_final_enriched_REAL.rds")
saveRDS(final_dt, save_path)
message("\n>>> GUARDADO EXITOSO: ", save_path)

# Limpieza Final de Memoria
rm(list=setdiff(ls(), "final_dt")); gc()