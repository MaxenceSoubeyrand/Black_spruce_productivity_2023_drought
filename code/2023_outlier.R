#Script for the figure S1

rm(list=ls())

library(tidyverse)
theme_set(theme_bw())
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggpubr)

canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")

#Opening the results
res <- readRDS("results/res_mean_by_clim.rds")

str(res)

res <- res %>% 
  filter(`sp_(dom)`=="Black_spruce",
         Forest_type=="BS") %>% 
  dplyr::select(clim_id, ClimateYear, NPP=NPP_annual, vpd=vpd_MJJA, temp=tMoy_MJJA, prec=Rain_MJJA)

#function to test the NPP 2023 in one pixel
test_npp <- function(data) {
  data_before_2023 <- filter(data, ClimateYear != 2023)$NPP
  npp_2023 <- filter(data, ClimateYear == 2023)$NPP
  
  
  test_low <- wilcox.test(data_before_2023, mu = npp_2023, alternative = "greater")$p.value
  test_high <- wilcox.test(data_before_2023, mu = npp_2023, alternative = "less")$p.value
  
  
  # Classification des résultats
  if (test_low < 0.05) {
    result <- "Higher NPP"
  } else if (test_high < 0.05) {
    result <- "Lower NPP"
  } else {
    result <- "No significant change"
  }
  
  return(result)
}

#Apply function on each function
outlier <- res %>%
  group_by(clim_id) %>%
  summarise(npp_category = test_npp(cur_data_all()))

#Join the test results with the grid
grid <- readRDS("results/grid.rds")

st_crs(grid) <- 4326

outlier_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(outlier)

st_crs(outlier_grid) <- st_crs(canada)

#The figure
outlier_plot <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = outlier_grid, aes(color=npp_category, fill=npp_category)) +
  scale_fill_manual("2023 NPP\nsignificant departure\nfrom normal", 
                       values = c("Higher NPP" = "blue", 
                                  "Lower NPP" = "red",
                                  "No significant change" = "black"))+
  scale_colour_manual("2023 NPP\nsignificant departure\nfrom normal", 
                        values = c("Higher NPP" = "blue",
                                   "Lower NPP" = "red",
                                   "No significant change" = "black"))+
  labs(x = NULL, y = NULL) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +  
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14) ,
    legend.title = element_text(size=14),
    legend.text = element_text(size=12)
  )

#Save them
pdf(file = "figures/2023_outlier.pdf",
    width=10, height=4)
outlier_plot
dev.off()

png(filename = "figures/2023_outlier.png",
    width=10, height=4, units = "in", res=500)
outlier_plot
dev.off()

