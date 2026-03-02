library(terra)
library(sf)
library(dplyr)
library(foreach)
library(doParallel)
library(stringr)

dir_rasters_clima <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/Representativeness"
path_shp_aps      <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/national_parks_PI84.shp"
path_raster_ap    <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ECNP.tif"
path_raster_er    <- "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ECR.tif" 
nombre_columna_id <- "WDPAID"

aps_vect <- vect(path_shp_aps)
archivos_clima <- list.files(dir_rasters_clima, pattern = "\\.tif$", full.names = TRUE)

template <- rast(archivos_clima[1])

aps_id_raster <- rasterize(aps_vect, template, field = nombre_columna_id)

num_cores <- 2
cl <- makeCluster(num_cores)
registerDoParallel(cl)

aps_id_packed <- terra::wrap(aps_id_raster)

red_clima <- foreach(
  archivo_i = archivos_clima, 
  .packages = c("terra", "stringr", "dplyr"), 
  .combine = 'rbind',
  .export = "aps_id_packed"
) %dopar% {
  
  aps_id_local <- terra::unwrap(aps_id_packed)
  
  tryCatch({
    r_clima_i <- terra::rast(archivo_i)
    id_emisor <- as.numeric(stringr::str_extract(basename(archivo_i), "\\d+"))
    solapes <- terra::zonal(r_clima_i, aps_id_local, fun = "sum", na.rm = TRUE)
    if(is.null(solapes) || nrow(solapes) == 0) {
      return(data.frame(id_emisor = id_emisor, id_receptor = NA, Pixeles = 0))
    }
    colnames(solapes) <- c("id_receptor", "Pixeles_Analogos")
    res <- solapes %>% 
      dplyr::filter(Pixeles_Analogos > 0) %>% 
      dplyr::mutate(id_emisor = id_emisor) %>%
      dplyr::select(id_emisor, id_receptor, Pixeles = Pixeles_Analogos)
    return(res)
  }, error = function(e) {
    return(data.frame(id_emisor = NA, id_receptor = NA, Pixeles = 0, Error = as.character(e)))
  })
}

stopCluster(cl)

area_total_aps <- terra::freq(aps_id_raster) %>%
  as.data.frame() %>%
  select(id_receptor = value, Area_TPix = count)

metricas_red_emisor <- red_clima %>%
  filter(!is.na(id_receptor), id_receptor != id_emisor) %>%
  group_by(id_emisor) %>%
  summarise(
    Sum_A = sum(Pixeles, na.rm = TRUE),
    Con_A = n_distinct(id_receptor)
  ) %>%
  left_join(area_total_aps, by = c("id_emisor" = "id_receptor")) %>%
  mutate(
    Por_A_Area = (Sum_A / Area_TPix) * 100
  )

metricas_red_receptor <- red_clima %>%
  filter(!is.na(id_receptor), id_receptor != id_emisor) %>%
  group_by(id_receptor) %>%
  summarise(
    Sum_R = sum(Pixeles, na.rm = TRUE),
    Con_R = n_distinct(id_emisor)
  ) %>%
  left_join(area_total_aps, by = "id_receptor") %>%
  mutate(Por_R_Area = (Sum_R / Area_TPix) * 100
  )


aps_vect <- terra::project(aps_vect, "EPSG:3035")

r_ap <- rast(path_raster_ap)
r_er <- rast(path_raster_er)

# Normalización
r_ap_n <- r_ap / terra::minmax(r_ap)[2] # max() a veces falla en rasters de memoria, minmax es más seguro
r_er_n <- r_er / terra::minmax(r_er)[2]

r_stack <- c(r_ap, r_ap_n, r_er_n)

names(r_stack) <- c("val_ap_original", "val_ap_norm", "val_er_norm")

df_extracciones_unidas <- terra::extract(r_stack, aps_vect, ID = TRUE)

receptor_expresion <- df_extracciones_unidas %>%
  group_by(ID) %>%
  summarise(
    p10_np = quantile(val_ap_norm, 0.10, na.rm = TRUE),
    p90_np = quantile(val_ap_norm, 0.90, na.rm = TRUE),
    med_np = median(val_ap_norm, na.rm = TRUE),
    med_er = median(val_er_norm, na.rm = TRUE)
  ) %>%
  mutate(
    RCRI = log10(med_np / (med_er)), 
    ISC  = p10_np * p90_np
  )

ids_reales <- as.numeric(aps_vect[[nombre_columna_id]][,1])
map_ids <- data.frame(ID = 1:nrow(aps_vect), ID_JOIN = ids_reales)

final_df <- receptor_expresion %>%
  left_join(map_ids, by = "ID") %>%
  left_join(metricas_red_emisor, by = c("ID_JOIN" = "id_emisor")) %>%
  left_join(metricas_red_receptor, by = c("ID_JOIN" = "id_receptor")) %>%
  mutate(
    across(c(Sum_A, Con_A, Con_A), ~tidyr::replace_na(., 0))
  )

aps_final <- terra::merge(aps_vect, final_df, by.x = nombre_columna_id, by.y = "ID_JOIN")
writeVector(aps_final, "Resultados_PNAC_aporta_recibe.shp", overwrite=TRUE)
