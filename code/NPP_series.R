#Script for the figure 2 and 6

rm(list = ls())

library(tidyverse)
theme_set(theme_bw())
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggpubr)
library(boot)
library(segmented)

#Opening simulation results
res <- readRDS("results/res_mean_by_clim_norm.rds")

#Keep black spruce in black spruce forest
res <- res %>% 
  filter(`sp_(dom)`=="Black_spruce",
         Forest_type=="BS")

#Temporal series
NPP_year <- res %>%  
  mutate(ClimateYear = as.numeric(ClimateYear)) %>%
  group_by(ClimateYear) %>% 
  summarize(NPP=median(NPP_sd), #calculate the median of each year
            NPP_q0.05=quantile(NPP_sd, 0.05), #with 0.05 and 0.95 quantiles
            NPP_q0.95=quantile(NPP_sd, 0.95))

#Adding the trending on the mean temporal series
model <- lm(NPP ~ ClimateYear, data = NPP_year)

# Adjust a segmented model to find the breakpoint
seg_model <- segmented(model, seg.Z = ~ClimateYear, psi = list(ClimateYear = 2000)) # 2000 is an a priori prediction 

breakpoint_year <- seg_model$psi[2]  #Breakpoint

p_value <- summary(seg_model)$coefficients[2, 4] #P-value on the breakpoint

#The figure
serie_NPP_figure <- ggplot(NPP_year, aes(x=ClimateYear, y=NPP)) +
  geom_ribbon(aes(ymin=NPP_q0.05, ymax=NPP_q0.95), alpha=0.5) +
  geom_line(color="red", linewidth=1) +
  geom_smooth(data = NPP_year %>% filter(ClimateYear <= breakpoint_year+1), 
              method = "lm", se = FALSE, color = "blue") +  #Segment before breakpoint
  geom_smooth(data = NPP_year %>% filter(ClimateYear > breakpoint_year), 
              method = "lm", se = FALSE, color = "blue") +  #Segment after breakpoint
  geom_vline(xintercept = breakpoint_year, linetype = "dashed", color = "black", linewidth = 1) +  #Dashed line at the breakpoint
  geom_hline(yintercept=0, linewidth=1, linetype="dashed", color="grey40") +
  labs(x="Year",
       y=expression("NPP anomalies in sd")) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

#Save the figure
pdf(file = "figures/NPP_temp.pdf",
    width=10, height=4)
serie_NPP_figure
dev.off()

png(filename = "figures/NPP_temp.png",
    width=10, height=4, units = "in", res=500)
serie_NPP_figure
dev.off()

#The mean NPP map 

res <- readRDS("results/res_mean_by_clim_norm.rds")

res <- res %>% 
  filter(`sp_(dom)`=="Black_spruce",
         Forest_type=="BS")

canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")

#Open the grid we create for the simulation pixel
grid <- readRDS("C:/Users/msoubeyr/OneDrive - NRCan RNCan/Documents/NPP_drought/github/results/grid.rds")

st_crs(grid) <- 4326

#Mean NPP in each pixel
NPP_mean <- res %>% 
  group_by(clim_id) %>% 
  summarize(NPP=median(NPP_annual)) 

#Combine with the grid
NPP_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(NPP_mean) 

#Create the figure
NPP_map <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data=NPP_grid, 
             aes(fill=NPP,
                 color=NPP)) +
  scale_fill_gradient(name="Median NPP\n1950-2024",low = "blue", high = "red") +
  scale_color_gradient(name="Median NPP\n1950-2024",low = "blue", high = "red") +
  labs(x="Longitude", y="Latitude") +  
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14) ,
    legend.title = element_text(size=14),
    legend.text = element_text(size=12)
  ) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55))

#Save the figure
pdf(file = "figures/NPP_spat.pdf",
    width=10, height=4)
NPP_map
dev.off()

png(filename = "figures/NPP_spat.png",
    width=10, height=4, units = "in", res=500)
NPP_map
dev.off()
