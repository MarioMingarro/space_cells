library(terra)
library(sf)
library(dplyr)
library(foreach)
library(doParallel)
library(stringr) # Para extraer IDs de los nombres

# --- Configuración de rutas ---
dir_rasters_clima <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/Representativeness"
path_shp_aps      <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/national_parks_PI84.shp"
path_raster_ap    <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ECNP.tif" # Corregido nombre de variable
path_raster_er    <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ECR.tif" 
nombre_columna_id <- "WDPAID"

# 1. CARGA Y LIMPIEZA INICIAL
aps_vect <- vect(path_shp_aps)
archivos_clima <- list.files(dir_rasters_clima, pattern = "\\.tif$", full.names = TRUE)

# CREAR RASTER DE REFERENCIA Y RASTER DE IDs (Para optimizar el bucle)
# Usamos el primer raster de clima como molde para que coincidan dimensiones y resolución
template <- rast(archivos_clima[1])

# Este raster tendrá el valor del ID del parque en los pixeles correspondientes
aps_id_raster <- rasterize(aps_vect, template, field = nombre_columna_id)

# ==============================================================================
# PARTE A: Dimensión "EMISOR" y Creación de la Red (Aporta)
# ==============================================================================
num_cores <- 2
cl <- makeCluster(num_cores)
registerDoParallel(cl)

aps_id_packed <- terra::wrap(aps_id_raster)

red_clima <- foreach(
  archivo_i = archivos_clima, 
  .packages = c("terra", "stringr", "dplyr"), 
  .combine = 'rbind',
  .export = "aps_id_packed" # Exportamos la versión empaquetada
) %dopar% {
  
  # 2. DENTRO del bucle: Desempaqueta para poder usarlo
  aps_id_local <- terra::unwrap(aps_id_packed)
  
  tryCatch({
    r_clima_i <- terra::rast(archivo_i)
    id_emisor <- as.numeric(stringr::str_extract(basename(archivo_i), "\\d+"))
    
    # 3. Operación Zonal con el objeto local desempaquetado
    solapes <- terra::zonal(r_clima_i, aps_id_local, fun = "sum", na.rm = TRUE)
    
    if(is.null(solapes) || nrow(solapes) == 0) {
      return(data.frame(ID_EMISOR = id_emisor, ID_RECEPTOR = NA, Pixeles = 0))
    }
    
    colnames(solapes) <- c("ID_RECEPTOR", "Pixeles_Analogos")
    
    # Lógica de limpieza
    res <- solapes %>% 
      dplyr::filter(Pixeles_Analogos > 0) %>% 
      dplyr::mutate(ID_EMISOR = id_emisor) %>%
      dplyr::select(ID_EMISOR, ID_RECEPTOR, Pixeles = Pixeles_Analogos)
    
    return(res)
    
  }, error = function(e) {
    # Captura el error específico para no detener todo el proceso
    return(data.frame(ID_EMISOR = NA, ID_RECEPTOR = NA, Pixeles = 0, Error = as.character(e)))
  })
}

stopCluster(cl)

# A partir de la red, calculamos con rigor las métricas (Masa y Conectividades reales)
# Excluimos la autorrepresentación (cuando EMISOR == RECEPTOR) para la conectividad
metricas_red_emisor <- red_clima %>%
  group_by(ID_EMISOR) %>%
  summarise(
    Masa_Salida_Total = sum(Pixeles, na.rm = TRUE),
    # ¿A cuántos OTROS parques toca?
    Conectividad_Salida = n_distinct(ID_RECEPTOR[ID_RECEPTOR != ID_EMISOR & !is.na(ID_RECEPTOR)])
  )

# ¡AQUÍ ESTÁ TU CONECTIVIDAD DE ENTRADA REAL!
metricas_red_receptor <- red_clima %>%
  filter(!is.na(ID_RECEPTOR), ID_RECEPTOR != ID_EMISOR) %>%
  group_by(ID_RECEPTOR) %>%
  summarise(
    # ¿Cuántos OTROS parques me tocan a mí?
    Conectividad_Entrada = n_distinct(ID_EMISOR)
  )


# ==============================================================================
# PARTE B y C: Dimensión "RECEPTOR" y "EXPRESIÓN" (Distribución interna)
# ==============================================================================
r_ap <- rast(path_raster_ap)
r_er <- rast(path_raster_er)

# Normalización
r_ap_n <- r_ap / terra::minmax(r_ap)[2] # max() a veces falla en rasters de memoria, minmax es más seguro
r_er_n <- r_er / 238


#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################

names(r_ap) <- "freq"
names(r_ap_n) <- "val_np"
names(r_er_n) <- "val_er"

# Extraemos todo. Usamos exact=TRUE si quieres exactitud fraccional en bordes (opcional pero lento)
ext_ap   <- terra::extract(r_ap, aps_vect, ID = TRUE)
ext_ap_n <- terra::extract(r_ap_n, aps_vect, ID = TRUE)
ext_er_n <- terra::extract(r_er_n, aps_vect, ID = TRUE)

# Unimos las 3 extracciones en un solo dataframe ANTES de calcular las métricas
df_extracciones_unidas <- ext_ap %>%
  left_join(ext_ap_n, by = "ID") %>%
  left_join(ext_er_n, by = "ID")

# Cálculo de métricas limpio y vectorizado
receptor_expresion <- df_extracciones_unidas %>%
  group_by(ID) %>%
  summarise(
    Cobertura_Respaldo = sum(freq >= 1, na.rm = TRUE) / n() * 100,
    p10_np = quantile(val_np, 0.10, na.rm = TRUE),
    p90_np = quantile(val_np, 0.90, na.rm = TRUE),
    med_np = median(val_np, na.rm = TRUE),
    med_er = median(val_er, na.rm = TRUE)
  ) %>%
  mutate(
    RCRI = log10(med_np / med_er),
    ISC  = p10_np * p90_np
  )

# ==============================================================================
# PARTE D: UNIFICACIÓN
# ==============================================================================
map_ids <- data.frame(ID = 1:nrow(aps_vect), ID_JOIN = as.vector(aps_vect[[nombre_columna_id]]))

final_df <- receptor_expresion %>%
  left_join(map_ids, by = "ID") %>%
  # Unimos lo que da (emisor)
  left_join(metricas_red_emisor, by = c("ID_JOIN" = "ID_EMISOR")) %>%
  # Unimos lo que recibe (receptor)
  left_join(metricas_red_receptor, by = c("ID_JOIN" = "ID_RECEPTOR")) %>%
  # Rellenamos NAs en conectividad con 0 (si un parque no da ni recibe nada)
  mutate(
    Conectividad_Salida = tidyr::replace_na(Conectividad_Salida, 0),
    Conectividad_Entrada = tidyr::replace_na(Conectividad_Entrada, 0)
  )

aps_final <- merge(aps_vect, final_df, by.x = nombre_columna_id, by.y = "ID_JOIN")
writeVector(aps_final, "Resultados_Clima_Completos.shp", overwrite=TRUE)