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

grid <- readRDS("results/grid.rds")
st_crs(grid) <- 4326

res <- readRDS("results/res_mean_by_clim_norm.rds") %>% 
  filter(`sp_(dom)`=="Black_spruce",
         Forest_type=="BS")

res <- res %>% 
  dplyr::select(clim_id, ClimateYear, NPP_sd)

NPP_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(res) %>% 
  filter(ClimateYear%in% c("2020", "2021", "2022", "2023"))

data <- res %>% 
  dplyr::select(clim_id) %>% 
  unique()

###
fire <- st_read("results/nbac_2023_20240530.shp") %>% 
  dplyr::select(geometry) %>% 
  st_simplify(dTolerance = 6000)

general_map <- ggplot() +
  geom_sf(data=canada) +
  geom_sf(data=US) +
  geom_sf(data=filter(NPP_grid, ClimateYear=="2023"), aes(fill=NPP_sd, color=NPP_sd))+
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", 
                         name = expression(NPP[pot] ~ "anomaly in sd")) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       name = expression(NPP[pot] ~ "anomaly in sd")) +
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

#Zoom in bc
bc_al <- ggplot() +
  geom_sf(data=canada) +
  geom_sf(data=US) +
  geom_sf(data=NPP_grid, aes(fill=NPP_sd, color=NPP_sd))+
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       name = expression(NPP[pot] ~ "anomaly in sd"))+
  scale_color_gradient2(low = "red", mid = "white", high = "blue", 
                       name = expression(NPP[pot] ~ "anomaly in sd"))+
  geom_sf(data=fire, alpha=0.5, color=NA, fill='black') +
  coord_sf(xlim = c(-130, -100), ylim = c(53, 62)) +
  theme(
    panel.border = element_rect(color = "chartreuse4", fill = NA, linewidth = 2)
  )+
  theme(axis.title = element_text(size = 14),   
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),  # Taille du titre de la légende
        legend.text = element_text(size = 12))

qc <- ggplot() +
  geom_sf(data=canada) +
  geom_sf(data=US) +
  geom_sf(data=NPP_grid, aes(fill=NPP_sd, color=NPP_sd))+
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", 
                         name = expression(NPP[pot] ~ "anomaly in sd")) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       name = expression(NPP[pot] ~ "anomaly in sd"))+
  geom_sf(data=fire, alpha=0.5, color=NA, fill='black') +
  coord_sf(xlim = c(-80, -70), ylim = c(45, 58)) +
  theme(
    panel.border = element_rect(color = "goldenrod3", fill = NA, linewidth = 2),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  theme(legend.position = "none")+
  theme(axis.title = element_text(size = 14),   
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),  # Taille du titre de la légende
        legend.text = element_text(size = 12))


gen_bc <- ggarrange(ggarrange(general_map, labels=c("A")),ggarrange(bc_al, labels=c("B")),
                  common.legend = TRUE,  
                  legend = "bottom", 
                  ncol=1, nrow=2,
                  labels=c("A", "B"))

maps <- ggarrange(gen_bc, ggarrange(qc, labels=c("C")), 
                  ncol=2, nrow=1,
                  widths = c(1,0.4))



#Extract NPP in fire
data_points <- NPP_grid %>% 
  dplyr::select(clim_id)

fire <- st_transform(fire, crs=st_crs(data_points))

clim_id_fire <- st_join(fire ,data_points)


intersect_fire <- st_intersection(filter(data_points, clim_id%in%clim_id_fire$clim_id), fire)

# Area of burned area in each clim cell
intersect_fire <- intersect_fire %>%
  mutate(burned_area = st_area(.))

NPP_fire <- NPP_grid %>%
  mutate(area = st_area(.)) %>% 
  st_drop_geometry() %>% 
  filter(clim_id%in%clim_id_fire$clim_id) %>% 
  left_join(intersect_fire) %>% 
  mutate(prop_area_burned=burned_area/area) %>% 
  filter(as.numeric(prop_area_burned)>0.3)


NPP_fire2 <- NPP_grid %>% 
  mutate(fire=clim_id %in% NPP_fire$clim_id,
         fire=ifelse(fire,">30% area burned", "<30% area burned"))


counts <- NPP_fire2 %>%
  filter(ClimateYear == "2023") %>%
  group_by(fire) %>%
  summarise(n = n())

# Violin plots
fire_diff_plot <- 
  ggplot(
    filter(NPP_fire2, ClimateYear=="2023"),
    aes(
      x = fire,
      y = NPP_sd,
      fill = fire
    )
  ) +
  geom_violin( 
    color = "black",      
    fill = "white",
    position = position_dodge(width = 0.8),
    trim = FALSE,
    alpha = 0.9
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    position = position_dodge(width = 0.8),
    size = 3,
    color = "red"
  ) +
  geom_text(
    data = counts,
    aes(x = fire, y = min(NPP_fire2$NPP_sd) - 1.2, label = paste0("n=", n)),
    inherit.aes = FALSE,
    size = 4
  ) +
  stat_compare_means(
    aes(group = fire),
    method = "wilcox.test",
    label = "p.format",
    group.by = "ClimateYear",
    label.y = 3.5,
    label.x=1.45,
    size = 4
  ) +
  ylab(expression(NPP[pot] ~ "anomaly in sd")) +
  xlab("Fire category") +   
  theme(
    legend.position = "none", 
    axis.title = element_text(size = 14),   
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )

empty <- ggplot() + theme_void()

maps_boxplot <- ggarrange(maps, ggarrange(empty,ggarrange(fire_diff_plot, labels = c("D")), empty, widths=c(0.4,1,0.4), ncol=3, nrow=1), 
                  ncol=1, nrow=2,
                  heights = c(1,0.5))

png(filename = "/figures/fire_maps.png", 
    width=10, height=8.3, units = "in", res=500)
maps_boxplot
dev.off()

pdf(file = "/figures/fire_maps.pdf", 
    width=10, height=8.3)
maps_boxplot
dev.off()

