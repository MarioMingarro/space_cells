library(terra)
library(sf)
library(dplyr)
library(tools)
library(tictoc)
library(foreach)
library(doParallel)
library(stringr)

# ... (tus rutas igual)
dir_rasters_clima <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/Representativeness"
path_shp_aps      <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/national_parks_PI84.shp"
path_raster_np    <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ECNP.tif"
path_raster_er    <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ECR.tif"
nombre_columna_id <- "NAME" 

# --- FUNCIÓN DE LIMPIEZA ESTRICTA ---
clean_name <- function(x) {
  x <- tolower(x)
  x <- iconv(x, to="ASCII//TRANSLIT")
  x <- str_replace_all(x, "[[:punct:]]", "") # Quita puntos, comas, guiones bajos, etc.
  x <- str_replace_all(x, " ", "") # Quita espacios
  return(x)
}

# ==============================================================================
# PARTE 1: Calcular RCRI e ISC
# ==============================================================================
r_n2000 <- rast(path_raster_np)
r_er    <- rast(path_raster_er)
aps_vect <- vect(path_shp_aps) 

r_er[r_er == 0] <- 1
r_er_n    <- r_er / max(values(r_er), na.rm=T)
r_n2000_n <- r_n2000 / max(values(r_n2000), na.rm=T)
r_n2000_n[r_n2000_n == 0] <- NA

dat_n2000_ext <- terra::extract(r_n2000_n, aps_vect, ID = TRUE, na.rm = TRUE)
dat_er_ext    <- terra::extract(r_er_n, aps_vect, ID = TRUE, na.rm = TRUE)
colnames(dat_n2000_ext)[2] <- "val_np"
colnames(dat_er_ext)[2]    <- "val_er"

dat_combined <- left_join(dat_n2000_ext, dat_er_ext, by = "ID", relationship = "many-to-many")

# --- APLICAR LIMPIEZA AQUÍ ---
raw_names <- as.vector(unlist(aps_vect[[nombre_columna_id]]))
clean_names <- clean_name(raw_names)
map_id_real <- data.frame(ID = 1:length(aps_vect), ID_JOIN = clean_names)

dat_final <- left_join(dat_combined, map_id_real, by = "ID")

resultado_isc <- dat_final %>%
  group_by(ID_JOIN) %>% 
  summarise(
    med_np = median(val_np, na.rm = TRUE),
    med_er = median(val_er, na.rm = TRUE),
    q10 = quantile(val_np, 0.10, na.rm = TRUE),
    q90 = quantile(val_np, 0.90, na.rm = TRUE),
  ) %>%
  mutate(
    RCRI = log10(med_np / med_er),
    ISC = q10 * q90
  )

# ==============================================================================
# PARTE 2: Calcular Redundancia Externa
# ==============================================================================
aps_sf <- st_as_sf(aps_vect) 

# --- APLICAR LIMPIEZA AQUÍ ---
aps_sf[[nombre_columna_id]] <- clean_name(aps_sf[[nombre_columna_id]])

r_ejemplo <- rast(list.files(dir_rasters_clima, pattern = "\\.tif$", full.names = TRUE)[1])
if (st_crs(aps_sf) != st_crs(crs(r_ejemplo))) {
  aps_sf <- st_transform(aps_sf, st_crs(crs(r_ejemplo)))
}
rm(r_ejemplo)

num_cores <- 2
cl <- makeCluster(num_cores)
registerDoParallel(cl)

clusterExport(cl, c("aps_sf", "nombre_columna_id", "dir_rasters_clima"))
clusterEvalQ(cl, library(terra))
clusterEvalQ(cl, library(sf))
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(tools))
clusterEvalQ(cl, library(stringr)) # IMPORTANTE

archivos_clima <- list.files(dir_rasters_clima, pattern = "\\.tif$", full.names = TRUE)

tic()
lista_redundancia <- foreach(archivo_i = archivos_clima, 
                             .packages = c("terra", "sf", "dplyr", "tools", "stringr"), 
                             .combine = 'rbind') %dopar% {
                               
                               tryCatch({
                                 r_clima_i <- terra::rast(archivo_i)
                                 nombre_base <- tools::file_path_sans_ext(basename(archivo_i))
                                 
                                 # --- APLICAR LIMPIEZA AQUÍ TAMBIÉN ---
                                 id_ap_i <- clean_name(nombre_base) 
                                 
                                 otras_aps_sf <- aps_sf[aps_sf[[nombre_columna_id]] != id_ap_i, ]
                                 
                                 if(nrow(otras_aps_sf) == 0) return(NULL)
                                 
                                 valores_extraidos <- terra::extract(r_clima_i, vect(otras_aps_sf), fun = sum, na.rm = TRUE)
                                 
                                 col_sum <- colnames(valores_extraidos)[2] 
                                 num_parques_comparten <- sum(valores_extraidos[[col_sum]] > 0, na.rm = TRUE)
                                 total_redundancia_valor <- sum(valores_extraidos[[col_sum]], na.rm = TRUE)
                                 
                                 data.frame(
                                   ID_JOIN = id_ap_i,
                                   Redundancia_Valor_Total = total_redundancia_valor,
                                   Numero_Parques_Comparten = num_parques_comparten
                                 )
                               }, error = function(e) {
                                 return(data.frame(ID_JOIN = basename(archivo_i), 
                                                   Redundancia_Valor_Total = NA, 
                                                   Numero_Parques_Comparten = NA, 
                                                   Error = e$message))
                               })
                             }
toc()
stopCluster(cl)

# ==============================================================================
# PARTE 3: Unir
# ==============================================================================
resultados_finales <- left_join(resultado_isc, lista_redundancia, by = "ID_JOIN")

# Estandarizar vector principal antes del merge
aps_vect[[nombre_columna_id]] <- clean_name(aps_vect[[nombre_columna_id]])
aps_final <- merge(aps_vect, resultados_finales, by.x = nombre_columna_id, by.y = "ID_JOIN")

output_shp <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/RESULTADOS_UNIFICADOS.shp"
writeVector(aps_final, output_shp, overwrite = TRUE)
cat("Proceso finalizado. Archivo guardado en:", output_shp, "\n")