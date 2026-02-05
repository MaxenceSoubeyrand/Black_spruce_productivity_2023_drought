#Figure 4

rm(list=ls())

library(tidyverse)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggpubr)


canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")


#opening data
res <- readRDS("results/res_mean_by_clim_norm.rds") %>% 
  filter(`sp_(dom)`=="Black_spruce",
         Forest_type=="BS")

anomalie <- res %>%
  group_by(clim_id) %>%
  summarize(mean_ppt = mean(Rain_MJJA[ClimateYear >= 1950 & ClimateYear <= 2022], na.rm = TRUE),
            mean_temp = mean(tMoy_MJJA[ClimateYear >= 1950 & ClimateYear <= 2022], na.rm = TRUE),
            mean_VPD = mean(vpd_MJJA[ClimateYear >= 1950 & ClimateYear <= 2022], na.rm = TRUE)) %>%
  left_join(res, by = "clim_id") %>%  # Join all years
  mutate(anomaly_ppt = Rain_MJJA - mean_ppt,
         anomaly_temp = tMoy_MJJA - mean_temp,
         anomaly_VPD = vpd_MJJA - mean_VPD) %>%
  dplyr::select(clim_id, ClimateYear, anomaly_ppt, anomaly_temp, anomaly_VPD) %>% 
  group_by(clim_id, ClimateYear) %>% 
  summarize(ppt_anomaly=mean(anomaly_ppt),
            temp_anomaly=mean(anomaly_temp),
            VPD_anomaly=mean(anomaly_VPD)) %>% 
  pivot_longer(
    cols = ends_with("_anomaly"),     
    names_to = "variable",         
    values_to = "Anomaly"           
  )


#Put the results in the grid
grid <- readRDS("results/grid.rds")

st_crs(grid) <- 4326

anomalie_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(anomalie)

prec_an <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = filter(anomalie_grid, variable=="ppt_anomaly", ClimateYear=="2023"), 
          aes(color = Anomaly, fill=Anomaly)) +
  scale_colour_gradient2(low = "red", mid = "white", high = "blue", 
                         name = expression(atop("Precipitation\nanomalies (mm)"))) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       name = expression(atop("Precipitation\nanomalies (mm)"))) +
  labs(x = NULL, y = NULL) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14) 
  )

temp_an <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = filter(anomalie_grid, variable=="temp_anomaly", ClimateYear=="2023"), 
          aes(color = Anomaly, fill=Anomaly)) +
  scale_colour_gradient2(low = "blue", mid = "white", high = "red", 
                         name = expression(atop("Temperature\nanomalies (°C)"))) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       name = expression(atop("Temperature\nanomalies (°C)"))) +
  labs(x = NULL, y = NULL) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14) 
  )

vpd_an <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = filter(anomalie_grid, variable=="VPD_anomaly", ClimateYear=="2023"), 
          aes(color = Anomaly, fill=Anomaly)) +
  scale_colour_gradient2(low = "blue", mid = "white", high = "red", 
                         name = expression(atop("VPD anomalies\n(kPa)"))) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       name = expression(atop("VPD anomalies\n(kPa)"))) +
  labs(x = NULL, y = NULL) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14) 
  )

rank_clim <- res %>% 
  dplyr::select(clim_id, ClimateYear, vpd_MJJA, tMoy_MJJA, Rain_MJJA) %>% 
  group_by(clim_id) %>% 
  mutate(VPD_rank=rank(desc(vpd_MJJA)),
         temp_rank=rank(desc(tMoy_MJJA)),
         ppt_rank=rank(desc(Rain_MJJA))) %>% 
  pivot_longer(
    cols = ends_with("_rank"),         
    names_to = "variable",         
    values_to = "Rank"        
  ) %>% 
  mutate(variable=str_remove_all(variable, "_rank")) %>% 
  dplyr::select(-vpd_MJJA,  -tMoy_MJJA,  -Rain_MJJA)

rank_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(rank_clim)

prec_rank <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = filter(rank_grid, variable == "ppt", ClimateYear == "2023"), 
          aes(fill = Rank,
              color = Rank)) +
  scale_colour_gradient(low = "blue", high = "red", 
                        name = "Precipitation\nrank") +
  scale_fill_gradient(low = "blue", high = "red", 
                      name = "Precipitation\nrank") +
  labs(x = NULL, y = NULL)+
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14) 
  )

temp_rank <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = filter(rank_grid, variable == "temp", ClimateYear == 2023), 
          aes(fill = Rank,
              color = Rank)) +
  scale_colour_gradient(low = "red", high = "blue", 
                        name = "Temperature\nrank") +
  scale_fill_gradient(low = "red", high = "blue", 
                      name = "Temperature\nrank") +
  labs(x = NULL, y = NULL)+
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14) 
  )

vpd_rank <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = filter(rank_grid, variable == "VPD", ClimateYear == 2023), 
          aes(fill = Rank,
              color = Rank)) +
  scale_colour_gradient(low = "red", high = "blue", 
                        name = "VPD\nrank") +
  scale_fill_gradient(low = "red", high = "blue", 
                      name = "VPD\nrank") +
  labs(x = NULL, y = NULL)+
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14) 
  )

#For the NPP
res <- readRDS("results/res_mean_by_clim_norm.rds")

str(res)

res <- res %>% 
  filter(`sp_(dom)`=="Black_spruce",
         Forest_type=="BS") %>% 
  group_by(clim_id) %>% 
  mutate(NPP_rank=rank(desc(NPP_sd))) %>% 
  ungroup() %>% 
  filter(ClimateYear=="2023") %>% 
  dplyr::select(clim_id, NPP_rank, NPP_sd)

NPP_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(res) 

NPP_an <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = NPP_grid, 
          aes(fill = NPP_sd,
              color = NPP_sd)) +
  scale_color_gradient2(low = "red", mid = "white", high = "blue", 
                       name = expression(atop(NPP[pot] ~ "anomalies", "in SD"))) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       name = expression(atop(NPP[pot] ~ "anomalies", "in SD"))) +
  labs(x = NULL, y = NULL)+
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14) 
  )

NPP_rank <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = NPP_grid, 
          aes(fill = NPP_rank,
              color = NPP_rank)) +
  scale_colour_gradient(low = "blue", high = "red", 
                        name = expression(NPP[pot] ~ "rank")) +
  scale_fill_gradient(low = "blue", high = "red", 
                      name = expression(NPP[pot] ~ "rank")) +
  labs(x = NULL, y = NULL)+
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14) 
  )


library(ggpubr)

plots <- ggarrange(NPP_an, NPP_rank, temp_an, temp_rank, 
                   prec_an, prec_rank, vpd_an, vpd_rank,
                   ncol=2, nrow=4, 
                   labels = c("A", "B", "C", "D", "E", "F", "G", "H"))

#Save the figure
pdf(file = "figures/anomalies_rank_2023.pdf", 
    width=11, height=8)
plots
dev.off()

png(filename = "figures/anomalies_rank_2023.png", 
    width=11, height=8, units = "in", res=500)
plots
dev.off()

