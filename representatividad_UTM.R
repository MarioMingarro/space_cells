library(terra)
library(sf)
library(tictoc)
library(dplyr)
library(ClimaRep)
library(foreach)
library(doParallel)


dir_result <- "C:/A_TRABAJO/CELLS_CLIMAREP"

dir_present_climate_data <- "C:/A_TRABAJO/DATA/CHELSA/PRESENT/"

## Climate variables exclusion ----
present_climate_variables <- terra::rast(list.files(dir_present_climate_data, "\\.tif$", full.names = T))
exclude_vars <- c("bio8", "bio9", "bio18", "bio19")
exclude_pattern <- paste0("bio(", paste(gsub("bio", "", exclude_vars), collapse = "|"), ")")
present_climate_variables <- subset(present_climate_variables, grep(exclude_pattern, names(present_climate_variables), invert = TRUE, value = TRUE))
names(present_climate_variables) <- c("CHELSA_bio1","CHELSA_bio10","CHELSA_bio11","CHELSA_bio12","CHELSA_bio13","CHELSA_bio14",
                                      "CHELSA_bio15","CHELSA_bio16","CHELSA_bio17","CHELSA_bio2",
                                      "CHELSA_bio3","CHELSA_bio4","CHELSA_bio5","CHELSA_bio6","CHELSA_bio7")

reference_system <- terra::crs(present_climate_variables)
study_area <- read_sf("C:/A_TRABAJO/CELLS_CLIMAREP/Iberia_10km_AE.shp")
polygon <- read_sf("C:/A_TRABAJO/CELLS_CLIMAREP/Iberia_100km_grid_final.shp")
polygon <- st_transform(polygon, crs(reference_system))
study_area <- st_transform(study_area, crs(reference_system))
present_climate_variables <-  terra::mask(crop(present_climate_variables, study_area), study_area)


## ClimaRep::vif_filter() ----
tic()
vif_result <- ClimaRep::vif_filter(present_climate_variables, th = 5)
toc()
vif_result$summary
present_climate_variables_filtered <- vif_result$filtered_raster
present_filtered_path <- file.path(dir_result, "present_climate_filtered.tif")
terra::writeRaster(present_climate_variables_filtered, present_filtered_path, overwrite = TRUE)


present_climate_variables_filtered <- terra::rast(present_filtered_path)

num_cores <- 2
cl <- makeCluster(num_cores)
registerDoParallel(cl)


tic("Ejecución paralela de mh_rep")
results <- foreach(
  i = 1:nrow(polygon),
  .packages = c("sf", "terra", "ClimaRep"),
  .combine = 'c',
  .errorhandling = 'stop'
) %dopar% {
  present_clim <- terra::rast(present_filtered_path)
  single_polygon <- polygon[i, ]
  ClimaRep::mh_rep(
    polygon = single_polygon,
    col_name = "CellCode",
    climate_variables = present_clim,
    study_area = study_area,
    th = 1,
    dir_output = dir_result,
    save_raw = FALSE
  )
}
toc()


stopCluster(cl)

tic()
ClimaRep::rep_overlay(folder_path = file.path("C:/A_TRABAJO/CELLS_CLIMAREP/100x100", "Representativeness"),
                      output_dir = "C:/A_TRABAJO/CELLS_CLIMAREP/100x100/100km_overlay_100" )
toc()
