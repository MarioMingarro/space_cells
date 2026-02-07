library(terra)
library(dplyr)

# 1. Cargar y alinear datos
r_n2000 <- rast("C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ECNP.tif")
r_er    <- rast("C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/ECR.tif")

study_area <- vect("C:/A_TRABAJO/CELLS_CLIMAREP/Iberia_10km_AE.shp")

# 2. Cálculo del ICCR (ratio de pesos relativos)
sums <- global(c(r_n2000, r_er), "sum", na.rm = TRUE)

r_n2000_n <- (r_n2000 / sums$sum[1]) 
r_er_n <- (r_er / sums$sum[2])
r_iccr <- r_n2000_n / r_er_n
names(r_iccr) <- "ICCR"

# 3. Transformación logarítmica (log10)
# ICCR = 0 -> NA (no representable en escala log)
r_log_iccr <- log10(r_iccr)
names(r_log_iccr) <- "logICCR"


writeRaster(
  r_n2000_n,
  "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/r_n2000_n.tif",
  overwrite = TRUE
)

writeRaster(
  r_er_n,
  "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/r_er_n.tif",
  overwrite = TRUE
)


writeRaster(
  r_iccr,
  "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/r_iccr.tif",
  overwrite = TRUE
)

writeRaster(
  r_log_iccr,
  "C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/logICCR.tif",
  overwrite = TRUE
)

writeVector(
  aps,
  "C:/A_TRABAJO/N2000_CLIMAREP/RESULTS_PAPER/N2000_ISC_Final.shp",
  overwrite = TRUE
)

message("Cálculo finalizado. log(ICCR) e ISC integrados correctamente.")





aps     <- vect("C:/A_TRABAJO/CELLS_CLIMAREP/NATIONAL_PARKS/national_parks_PI2.shp") 
# 4. Extracción y cálculo del ISC
# Usamos bind = TRUE para mantener las columnas del shapefile (aps)
stats <- terra::extract(r_log_iccr, aps, na.rm = TRUE, bind = TRUE)

# Convertimos a dataframe/tibble para usar dplyr
stats_df <- as.data.frame(stats)

# Ahora ya podemos agrupar por WDPAID
resultado_isc <- stats_df %>%                
  group_by(WDPAID) %>%
  summarise(
    # Asumo que la columna en tu raster se llama logICCR
    ISC = quantile(logICCR, 0.10, na.rm = TRUE) * quantile(logICCR, 0.90, na.rm = TRUE)
  )



# 4. Extracción y cálculo del ISC
stats <- terra::extract(r_log_iccr, aps, na.rm = TRUE) %>%
  group_by(WDPAID) %>%
  summarise(
    ISC = quantile(logICCR, 0.10) * quantile(logICCR, 0.90)
  )

# 5. Integración y guardado
aps$ISC <- stats$ISC