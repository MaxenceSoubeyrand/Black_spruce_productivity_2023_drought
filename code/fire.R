#Script for the figure 5

rm(list = ls())

library(tidyverse)
theme_set(theme_bw())
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggpubr)

canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")

#Opening the grid
grid <- readRDS("results/grid.rds")
st_crs(grid) <- 4326

#Opening the results
res <- readRDS("results/res_mean_by_clim_norm.rds") %>% 
  filter(`sp_(dom)`=="Black_spruce",
         Forest_type=="BS")

res <- res %>% 
  select(clim_id, ClimateYear, NPP_sd)

#Combine grid and the results
NPP_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(res) %>% 
  filter(ClimateYear=="2023")

data <- res %>% 
  select(clim_id) %>% 
  unique()

#Opening the fire, data available here: https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/NBAC_2023_20250506.zip
fire <- st_read("nbac_2023_20240530.shp") %>% 
  select(geometry) %>% 
  st_simplify(dTolerance = 6000)

#Map of the NPP and the fire
general_map <- ggplot() +
  geom_sf(data=canada) +
  geom_sf(data=US) +
  geom_sf(data=NPP_grid, aes(fill=NPP_sd, color=NPP_sd))+
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", 
                         name = "NPP anomalies in sd") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       name = "NPP anomalies in sd") +
  geom_sf(data=fire, alpha=1, color=NA, fill='black') +
  geom_rect(aes(xmin = -130, xmax = -100, ymin = 53, ymax = 62),
            color = "chartreuse4", fill = NA, linewidth = 1.2) +
  geom_rect(aes(xmin = -80, xmax = -70, ymin = 45, ymax = 58),
            color = "goldenrod3", fill = NA, linewidth = 1.2) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(legend.position = "none")+
  theme(axis.title = element_text(size = 14),   
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

general_map

#Zoom at west Canada
bc_al <- ggplot() +
  geom_sf(data=canada) +
  geom_sf(data=US) +
  geom_sf(data=NPP_grid, aes(fill=NPP_sd, color=NPP_sd))+
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", 
                         name = "NPP anomalies in sd") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       name = "NPP anomalies in sd")+
  geom_sf(data=fire, alpha=0.5, color=NA, fill='black') +
  coord_sf(xlim = c(-130, -100), ylim = c(53, 62)) +
  theme(
    panel.border = element_rect(color = "chartreuse4", fill = NA, linewidth = 2)
  )+
  theme(axis.title = element_text(size = 14),   
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#Zoom in Quebec
qc <- ggplot() +
  geom_sf(data=canada) +
  geom_sf(data=US) +
  geom_sf(data=NPP_grid, aes(fill=NPP_sd, color=NPP_sd))+
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", 
                         name = "NPP anomalies in sd") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       name = "NPP anomalies in sd")+
  geom_sf(data=fire, alpha=0.5, color=NA, fill='black') +
  coord_sf(xlim = c(-80, -70), ylim = c(45, 58)) +
  theme(
    panel.border = element_rect(color = "goldenrod3", fill = NA, linewidth = 2),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  theme(legend.position = "none")+
  theme(axis.title = element_text(size = 14),   
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12))


gen_bc <- ggarrange(ggarrange(general_map, labels=c("A")),ggarrange(bc_al, labels=c("B")),
                  common.legend = TRUE,  
                  legend = "bottom", 
                  ncol=1, nrow=2,
                  labels=c("A", "B"))

#Map of fire with zooms
maps <- ggarrange(gen_bc, ggarrange(qc, labels=c("C")), 
                  ncol=2, nrow=1,
                  widths = c(1,0.4))


#Violin plots
#Prepare for extraction of NPP in and outside the fire
data_points <- NPP_grid %>% 
  select(clim_id)

fire <- st_transform(fire, crs=st_crs(data_points))

clim_id_fire <- st_join(fire ,data_points)

#Intersection between fire and NPP
intersect_fire <- st_intersection(filter(data_points, clim_id%in%clim_id_fire$clim_id), fire)

# Calcul of burned area in each cell
intersect_fire <- intersect_fire %>%
  mutate(burned_area = st_area(.))

#NPP with cell burned or not
NPP_fire <- NPP_grid %>%
  mutate(area = st_area(.)) %>% 
  st_drop_geometry() %>% 
  filter(clim_id%in%clim_id_fire$clim_id) %>% 
  left_join(intersect_fire) %>% 
  mutate(prop_area_burned=burned_area/area) %>% 
  filter(as.numeric(prop_area_burned)>0.3) #If more than 30% of area burned then cell burned


NPP_fire2 <- NPP_grid %>% 
  mutate(fire=clim_id %in% NPP_fire$clim_id,
         fire=ifelse(fire,">30% area burned", "<30% area burned"))

mean(filter(NPP_fire2, fire=="<30% area burned")$NPP_sd) -
mean(filter(NPP_fire2, fire==">30% area burned")$NPP_sd)

#test
t_test_result <- t.test(NPP_sd ~ fire, data = NPP_fire2)
t_test_result

#the figure
fire_diff_plot <- ggplot(NPP_fire2, aes(y = NPP_sd, x = fire)) +
  geom_violin() +
  stat_summary(fun = median, geom = "point", color = "red", size = 4) +  
  stat_summary(
    fun.data = function(y) data.frame(y = min(y) - 1, label = paste0("n =", length(y))),  
    geom = "text",
    size = 5
  ) +
  stat_compare_means(
    method = "wilcox.test",
    label.x = 1.25, 
    label.y = 3.1,
    size = 5
  ) +
  ylab("NPP anomalies in sd") +
  theme(axis.title.x=element_blank(),
    axis.title = element_text(size = 14),   
        axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))


empty <- ggplot() + theme_void()

#Combined plot
maps_boxplot <- ggarrange(maps, ggarrange(empty, ggarrange(fire_diff_plot, labels = c("D")), empty, widths = c(0.4,1,0.4), nrow=1), 
                  ncol=1, nrow=2,
                  heights = c(1,0.5))

#Save the plot

png(filename = "figures/fire_maps.png", 
    width=10, height=8.3, units = "in", res=500)
maps_boxplot
dev.off()

pdf(file = "figures/fire_maps.pdf", 
    width=10, height=8.3)
maps_boxplot
dev.off()
