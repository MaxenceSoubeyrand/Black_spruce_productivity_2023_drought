#Script for the figure 3

rm(list=ls())

library(tidyverse)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggpubr)


canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")

#Opening the simulation results
res <- readRDS("results/res_mean_by_clim_norm.rds") %>% 
  filter(`sp_(dom)`=="Black_spruce",
         Forest_type=="BS")

#Compute precipitation, temperature and vpd 2023 anomalies with the 1950-2024 reference period
anomalie <- res %>%
  group_by(clim_id) %>%
  summarize(mean_ppt = mean(Rain_MJJA[ClimateYear >= 1950 & ClimateYear <= 2024], na.rm = TRUE),
            mean_temp = mean(tMoy_MJJA[ClimateYear >= 1950 & ClimateYear <= 2024], na.rm = TRUE),
            mean_VPD = mean(vpd_MJJA[ClimateYear >= 1950 & ClimateYear <= 2024], na.rm = TRUE)) %>%
  left_join(res, by = "clim_id") %>%
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

anomalie2 <-anomalie %>% 
  group_by(ClimateYear, variable) %>% 
  summarize(Anomaly=mean(Anomaly)) %>% 
  mutate(Year=ifelse(ClimateYear=="2023", "2023", "1950-2024"))


#The figure
#Temperature
temp <- ggplot() +
  geom_boxplot(data = filter(anomalie2, variable == "temp_anomaly"), aes(y = Anomaly, x = variable)) +
  geom_point(data = filter(anomalie2, variable == "temp_anomaly"), 
             aes(y = Anomaly, x = variable, color = Year),
             position = position_jitter(width = 0.2, height = 0),
             size=3) +
  scale_color_manual(values = c("blue", "red")) +
  ylim(min = -max(abs(filter(anomalie2, variable == "temp_anomaly")$Anomaly)), 
       max = max(abs(filter(anomalie2, variable == "temp_anomaly")$Anomaly))) +
  xlab(NULL)+
  ylab("Temperature anomaly (in °C)")+
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_text(size = 14),   
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12))
  
#Precipitation
ppt <- ggplot() +
  geom_boxplot(data = filter(anomalie2, variable == "ppt_anomaly"), aes(y = Anomaly, x = variable)) +
  geom_point(data = filter(anomalie2, variable == "ppt_anomaly"), 
             aes(y = Anomaly, x = variable, color = Year),
             position = position_jitter(width = 0.2, height = 0),
             size=3) +
  scale_color_manual(values = c("blue", "red")) +
  ylim(min = -max(abs(filter(anomalie2, variable == "ppt_anomaly")$Anomaly)), 
       max = max(abs(filter(anomalie2, variable == "ppt_anomaly")$Anomaly))) +
  xlab(NULL)+
  ylab("Precipitation anomaly (in mm)")+
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_text(size = 14),   
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12))


#VPD
vpd <- ggplot() +
  geom_boxplot(data = filter(anomalie2, variable == "VPD_anomaly"), aes(y = Anomaly, x = variable)) +
  geom_point(data = filter(anomalie2, variable == "VPD_anomaly"), 
             aes(y = Anomaly, x = variable, color = Year), 
             position = position_jitter(width = 0.2, height = 0),
             size=3) +
  scale_color_manual(values = c("blue", "red")) +
  ylim(min = -max(abs(filter(anomalie2, variable == "VPD_anomaly")$Anomaly)), 
       max = max(abs(filter(anomalie2, variable == "VPD_anomaly")$Anomaly))) +
  xlab(NULL)+
  ylab("VPD anomaly (in kPa)")+
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_text(size = 14),   
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12))

#NPP anomalies
res <- res %>% 
  filter(`sp_(dom)`=="Black_spruce",
         Forest_type=="BS") %>% 
  ungroup() %>% 
  dplyr::select(clim_id, ClimateYear, NPP_sd)

grid <- readRDS("results/grid.rds")

st_crs(grid) <- 4326

#Put it the grid to extract values
NPP_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(res) %>% 
  st_drop_geometry() %>% 
  group_by(ClimateYear) %>% 
  summarize(NPP_sd=mean(NPP_sd)) %>% 
  mutate(Year=ifelse(ClimateYear=="2023", "2023", "1950-2024")) %>% 
  mutate(x="x")

#NPP figure
NPP <- ggplot() +
  geom_boxplot(data = NPP_grid, aes(y = NPP_sd, x=x)) +
  geom_point(data = NPP_grid, 
             aes(x=x, y = NPP_sd, color = Year),  # Augmenter la taille des points de 2023
             position = position_jitter(width = 0.2, height = 0),
             size=3) +
  scale_color_manual(values = c("blue", "red")) +
  # geom_point(data = filter(anomalie2, variable == "temp_anomaly", ClimateYear=="2023"), 
  #            aes(y = Anomaly, x = variable), color="red", size=12) +
  ylim(min = -max(abs(NPP_grid$NPP_sd)), 
       max = max(abs(NPP_grid$NPP_sd))) +
  xlab(NULL)+
  ylab("NPP anomaly in sd")+
  theme_minimal() +
  theme(axis.text.x = element_blank(),
    axis.title = element_text(size = 14),   
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),  # Taille du titre de la légende
    legend.text = element_text(size = 12))

#Combined the NPP boxplot and the climate plots
combined_plots <- ggarrange(NPP, temp, ppt, vpd, 
          ncol=4, nrow=1, 
          common.legend = T, legend="bottom",
          labels=c("A", "B", "C", "D"))

#Save the file
png(filename = "figures/boxplot_anomalies.png", 
    width=9, height=5, units = "in", res=500)
combined_plots
dev.off()

pdf(file = "figures/boxplot_anomalies.pdf", 
    width=9, height=5)
combined_plots
dev.off()

ggplot(anomalie2, aes(x=as.numeric(ClimateYear), y=Anomaly)) + 
  geom_line() +
  facet_wrap(~variable, scale="free")
