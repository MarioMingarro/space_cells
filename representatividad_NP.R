library(terra)
library(dplyr)

r_n2000 <- rast("C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ECNP.tif") # numero de APs que tienen el clima del pixel. Redundancia climatica
r_er    <- rast("C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ECR.tif") # Numero de celdas 10x10 que tienen el clima del pixel. Redundancia climatica

study_area <- vect("C:/A_TRABAJO/CELLS_CLIMAREP/Iberia_10km_AE.shp")


r_er[r_er == 0] <- 1


r_er_n    <- r_er / sum(values(r_er), na.rm=T)
r_n2000_n <- r_n2000 / sum(values(r_n2000), na.rm=T)
r_n2000_n[r_n2000_n == 0] <- NA

r_rareza <- 1/r_er # Rareza climatica
r_rareza_n <- r_rareza / sum(values(r_rareza), na.rm=T)

r_iccr <- (r_n2000_n / r_er_n) 
names(r_iccr) <- "ICCR"
plot(r_iccr)

r_iccr_inv <- (r_n2000_n / r_er_n) 
names(r_iccr) <- "ICCR"
plot(r_iccr)



r_iccr_nuevo <- (r_n2000 / r_er) * (1 / r_er)

r_log_iccr <- log10(r_iccr)
names(r_log_iccr) <- "logICCR"
plot(r_log_iccr)

writeRaster(
  r_n2000_n,
  "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/r_n2000_n.tif",
  overwrite = TRUE
)

writeRaster(
  r_er,
  "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/r_rareza.tif",
  overwrite = TRUE
)

writeRaster(
  r_iccr,
  "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/r_iccr_rareza.tif",
  overwrite = TRUE
)

writeRaster(
  r_log_iccr,
  "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/logICCR_rareza.tif",
  overwrite = TRUE
)

writeVector(
  aps,
  "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ISC_Final.shp",
  overwrite = TRUE
)

message("Cálculo finalizado. log(ICCR) e ISC integrados correctamente.")





aps     <- vect("C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/national_parks_PI2.shp") 
# 4. Extracción y cálculo del ISC
# Usamos bind = TRUE para mantener las columnas del shapefile (aps)
stats_raw <- terra::extract(r_log_iccr, aps, na.rm = TRUE)

# 2. Convertir el SpatVector 'aps' a dataframe para obtener los atributos
aps_df <- as.data.frame(aps)

# 3. Crear una columna de ID en aps_df para unir (es el número de fila)
aps_df$ID <- 1:nrow(aps_df)

# 4. Unir los píxeles extraídos con los atributos de los parques
stats_full <- left_join(stats_raw, aps_df, by = "ID")

# 5. Ahora sí podemos agrupar por WDPAID
resultado_isc <- stats_full %>%
  group_by(WDPAID) %>%
  summarise(
    # Asumo que la columna en tu raster se llama logICCR
    ISC = quantile(logICCR, 0.10, na.rm = TRUE) * quantile(logICCR, 0.90, na.rm = TRUE)
  )

aps <- merge(aps, resultado_isc, by = "WDPAID")
