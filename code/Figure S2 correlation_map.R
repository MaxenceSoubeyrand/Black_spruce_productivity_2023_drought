#Figure S2

rm(list = ls())

library(tidyverse)
theme_set(theme_bw())
library(viridis)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggpubr)
library(trend)
library(segmented)
library(modifiedmk)
library(ppcor)


canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")

res <- readRDS("results/res_mean_by_clim_norm.rds")

res <- res %>%
  filter(`sp_(dom)` == "Black_spruce", Forest_type == "BS") %>%
  dplyr::select(clim_id, ClimateYear, NPP=NPP_annual, VPD=vpd_MJJA, Temperature=tMoy_MJJA, Precipitation=Rain_MJJA) 

grid <- readRDS("results/grid.rds")

st_crs(grid) <- 4326

res_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(res)

# Functionn for the correlation in one pixel
calc_semipartial <- function(df) {
  
  result <- pcor(data.frame(df %>% dplyr::select(NPP, VPD, Temperature, Precipitation) %>% st_drop_geometry()))
  
  data.frame(clim=c("VPD", "Temperature", "Precipitation"), 
             estimate=result$estimate[1,2:4],
             p_val=result$p.value[1,2:4], row.names = NULL,
             clim_id=unique(df$clim_id))
}

cor <- res_grid %>%
  group_by(clim_id) %>%
  group_split() %>%
  map_dfr(calc_semipartial)

res_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(cor) %>% 
  mutate(p_adj = p.adjust(p_val, method = "fdr")) %>% 
  filter(p_adj<0.05)

res_grid$clim <- factor(
  res_grid$clim,
  levels = c("VPD", "Temperature", "Precipitation"),
  labels = c("VPD (kPa)", "Temperature (°C)", "Precipitation (mm)"),
)

max_val <- max(abs(res_grid$estimate), na.rm = TRUE)
hist(res_grid$estimate)

plot <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = res_grid, 
          aes(color = estimate, fill = estimate)) +
  
  scale_fill_gradientn(
    colours = c("red", "white", "white", "white", "blue"),
    values = scales::rescale(c(-1, -0.25, 0, 0.25, 1)),
    limits = c(-1, 1),
    name = "Partial correlation"
  )+
  scale_colour_gradientn(
    colours = c("red", "white", "white", "white", "blue"),
    values = scales::rescale(c(-1, -0.25, 0, 0.25, 1)),
    limits = c(-1, 1),
    name = "Partial correlation"
  )+

  facet_wrap(~clim, nrow = 3) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12), 
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 12)
  )

#Save the figure
pdf(file = "figures/partial_correlation.pdf", 
    width=7, height=6)
plot
dev.off()

png(filename = "figures/partial_correlation.png", 
    width=7, height=6, units = "in", res=500)
plot
dev.off()