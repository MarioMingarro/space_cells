library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan)


r_100_scaled <- rast("C:/A_TRABAJO/CELLS_CLIMAREP/100x100/100km_overlay_100/representatividad_100x100.tif")
r_climate_rep <- rast("C:/A_TRABAJO/CELLS_CLIMAREP/10x10/10km_overlay_100/representatividad_10x10.tif")
r_dem <- rast("C:/A_TRABAJO/N2000_CLIMAREP/DATA/DEM/wc2.1_30s_elev_EU_LAEA.tif")


target_crs <- crs(r_100_scaled)
target_ext <- ext(r_100_scaled)

r_dem_proj <- project(r_dem, target_crs)
r_dem_crop <- crop(r_dem_proj, target_ext)
r_climate_rep <- project(r_climate_rep, target_crs)

r_dem_resample <- resample(r_dem_crop, r_100_scaled)
r_climate_rep_resample <- resample(r_climate_rep, r_100_scaled)



r_final <- c(r_100_scaled, r_climate_rep_resample, r_dem_resample)
names(r_final) <- c("N2000_Rep", "Climate_Rep", "Elevation")

df <- as.data.frame(r_final, xy = TRUE, na.rm = TRUE)


df <- df %>%
  mutate(
    Elevation_Bin = floor(Elevation / 10) * 10,
    Elevation_Bin = ifelse(Elevation < 0, 0, Elevation_Bin)
  )

df_mean_rep <- df %>%
  group_by(Elevation_Bin) %>%
  summarise(
    N2000_Rep_Accumulation = sum(N2000_Rep, na.rm = TRUE),
    Climate_Rep_Accumulation = sum(Climate_Rep, na.rm = TRUE),
    Cell_Count = n()
  ) %>%
  ungroup() %>%
  filter(Cell_Count > 0)


df_plot <- df_mean_rep %>%
  mutate(
    N2000_Scaled = (N2000_Rep_Accumulation / max(N2000_Rep_Accumulation, na.rm = TRUE)) * 100,
    Climate_Scaled = (Climate_Rep_Accumulation / max(Climate_Rep_Accumulation, na.rm = TRUE)) * 100,
    Cells_Scaled = (Cell_Count / max(Cell_Count, na.rm = TRUE)) * 100
  )


residual_test <- chisq.test(
  x = df_plot$N2000_Rep_Accumulation,
  p = df_plot$Climate_Rep_Accumulation / sum(df_plot$Climate_Rep_Accumulation),
  rescale.p = TRUE
)

df_plot$Residual_Signed <- residuals(residual_test)

max_abs_residual <- max(abs(df_plot$Residual_Signed), na.rm = TRUE)
df_plot$Residual_Plot_Height <- (abs(df_plot$Residual_Signed) / max_abs_residual) * 20 * sign(df_plot$Residual_Signed)


max_abs_residual_rounded <- ceiling(max_abs_residual / 50) * 50


df_long <- df_plot %>%
  select(Elevation_Bin, N2000_Scaled, Climate_Scaled, Cells_Scaled) %>%
  pivot_longer(
    cols = c(N2000_Scaled, Climate_Scaled, Cells_Scaled),
    names_to = "Metric",
    values_to = "Value_Scaled"
  )


ggplot() +
  geom_col(
    data = df_plot,
    aes(
      x = Elevation_Bin,
      y = Residual_Plot_Height,
      fill = Residual_Signed
    ),
    width = 10,
    color = NA,
    alpha = 0.8
  ) +
  scale_fill_gradient2(
    low = "red4",
    mid = "white",
    high = "green4",
    midpoint = 0,
    limits = c(-max_abs_residual_rounded, max_abs_residual_rounded),
    name = expression(paste("Pearson residuals (", chi^2, ")"))
  ) +
  geom_line(
    data = df_long,
    aes(x = Elevation_Bin, y = Value_Scaled, color = Metric),
    linewidth = 1,
    alpha = 0.4
  ) +
  geom_point(
    data = df_long,
    aes(x = Elevation_Bin, y = Value_Scaled, color = Metric),
    size = 1,
    alpha = 0.4
  ) +
  scale_color_manual(
    values = c(
      "N2000_Scaled" = "magenta4",
      "Climate_Scaled" = "blue3",
      "Cells_Scaled" = "gray20"
    ),
    labels = c(
      "Climate_Scaled" = "10x10 climate availability",
      "Cells_Scaled" = "Geographic availability",
      "N2000_Scaled" = "100x100 climate availability"
    ),
    name = "Metrics"
  ) +
  labs(
    title = " ",
    x = "Elevation (m)",
    y = "Scaled value (%)"
  ) +
  scale_x_continuous(
    name = "Elevation (m)",
    breaks = seq(min(df_plot$Elevation_Bin), max(df_plot$Elevation_Bin), by = 500)) +
  scale_y_continuous(
    name = "Scaled value (%)",
    limits = c(-20, 100),
    breaks = seq(-20, 100, by = 10)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5),
         color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))



