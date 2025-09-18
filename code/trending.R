#Script for the figure 7

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

canada <- ne_countries(scale = "medium", country = "Canada", returnclass = "sf")
US <- ne_countries(scale = "medium", country = "United States of America", returnclass = "sf")

#Opening the results
res <- readRDS("C:/Users/msoubeyr/OneDrive - NRCan RNCan/Documents/NPP_drought/github/results/res_mean_by_clim_norm.rds")

length(unique(res$Id))

filter(res, clim_id == "19068")

str(res)

res <- res %>%
  filter(`sp_(dom)` == "Black_spruce", Forest_type == "BS") %>%
  dplyr::select(clim_id, ClimateYear, NPP=NPP_sd) %>%
  group_by(clim_id) %>%
  filter(any(NPP != 0)) %>%
  ungroup()

trend_npp <- res %>%
  mutate(ClimateYear=as.numeric(ClimateYear)) %>% 
  group_by(clim_id) %>%
  summarize(
    slope_NPP = sens.slope(NPP, ClimateYear)$estimates,  #Theil-Sen slope
    pval_NPP = sens.slope(NPP, ClimateYear)$p.value
    
  )

grid <- readRDS("results/grid.rds")

st_crs(grid) <- 4326

alpha_threshold <- 0.1

trend_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(trend_npp) %>% 
  mutate(slope_NPP=case_when(pval_NPP >= alpha_threshold ~ NA,
                              .default=slope_NPP)) 

sum(is.na(trend_grid$slope_NPP))/length(trend_grid$slope_NPP)*100

sum(trend_grid$slope_NPP>0, na.rm = T)/length(trend_grid$slope_NPP)*100

#Plot of whole trend (from 1950-2024)
trend_plot <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = trend_grid, 
          aes(color = slope_NPP, fill=slope_NPP)) +
  scale_colour_gradient2(low = "red", high = "blue", mid="white",
                         name = "NPP trend\nfrom 1950 to 2024") +
  scale_fill_gradient2(low = "red", high = "blue", mid="white",
                       name = "NPP trend\nfrom 1950 to 2024") +
  labs(x = NULL, y = NULL) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55))+
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12))


get_breakpoint <- function(data) {
  model <- lm(NPP ~ ClimateYear, data = data)
  
  seg_model <- tryCatch(segmented(model, seg.Z = ~ClimateYear), 
                        error = function(e) NA)
  
  # extract estimate breakpoint
  breakpoint <- seg_model$psi[, "Est."]
  
  # Davies test 
  pvalue <- tryCatch(davies.test(model, seg.Z = ~ClimateYear)$p.value, error = function(e) NA)
  
  # Extract slopes standard errors from the segmented model
  coef_summary <- summary(seg_model)$coefficients
  
  slope_before <- coef_summary["ClimateYear", "Estimate"]
  se_before <- coef_summary["ClimateYear", "Std. Error"]
  
  slope_after <- slope_before + coef_summary["U1.ClimateYear", "Estimate"]
  se_after <- sqrt(se_before^2 + coef_summary["U1.ClimateYear", "Std. Error"]^2)  #Standard error
  
  trend_before <- ifelse(slope_before > 0, "up", "down")
  trend_after <- ifelse(slope_after > 0, "up", "down")
  
  #P-values directly extracted from the segmented model
  pval_before <- coef_summary["ClimateYear", "Pr(>|t|)"]
  pval_after <- coef_summary["U1.ClimateYear", "Pr(>|t|)"]
  
  # Slope change test
  slope_change <- coef_summary["U1.ClimateYear", "Estimate"]
  pval_change <- coef_summary["U1.ClimateYear", "Pr(>|t|)"]
  
  return(tibble(breakpoint = breakpoint, pvalue = pvalue,
                trend_before = trend_before, slope_before = slope_before, pval_before = pval_before,
                trend_after = trend_after, slope_after = slope_after, pval_after = pval_after,
                slope_change = slope_change, pval_change = pval_change))
}



data= res %>%
  mutate(ClimateYear = as.numeric(ClimateYear)) %>%
  filter(clim_id==11886)

get_breakpoint(data)

#The following commands take 10 minutes to run
#I run it one times and save the results
# breakpoint_npp <- res %>%
#   mutate(ClimateYear = as.numeric(ClimateYear)) %>%
#   group_by(clim_id) %>%
#   nest() %>%
#   mutate(results = map(data, get_breakpoint)) %>%
#   unnest(results)

#saveRDS(breakpoint_npp, "U:/YBoulanger/Maxence/prep_input/breakpoint_npp.rds")
#Load the results from the previous command
breakpoint_npp <- readRDS("C:/Users/msoubeyr/OneDrive - NRCan RNCan/Documents/NPP_drought/github/results/breakpoint_npp.rds")

breakpoint_grid <- st_sf(geometry = grid)  %>%
  mutate(clim_id = as.character(row_number()))  %>%
  filter(clim_id %in% unique(res$clim_id)) %>% 
  left_join(breakpoint_npp) %>% 
  na.omit()

trend_after_df <- breakpoint_grid %>% 
  mutate(trend_after=case_when(pval_after < alpha_threshold ~ trend_after,
                               pvalue >= alpha_threshold & pval_after >= alpha_threshold ~ "no breakpoint",
                               pval_after >= alpha_threshold ~ "no trend"))

table(trend_after_df$trend_after)/sum(table(trend_after_df$trend_after)) 
table(trend_after_df$trend_after)/(sum(table(trend_after_df$trend_after))-5719)


trend_after_df$trend_after <- factor(trend_after_df$trend_after, levels = c("up", "down", "no trend", "no breakpoint"))

#The plot
#trend after breakpoint
trend_after <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = trend_after_df, 
          aes(color = trend_after, fill = trend_after)) +
  scale_color_manual("Trend after\nbreakpoint",
                     values = c("down" = "red",  # Remplace par tes couleurs
                                "up" = "blue",
                                "no trend" = "grey20",
                                "no breakpoint" = "grey60")) +
  scale_fill_manual("Trend after\nbreakpoint",
                    values = c("down" = "red",
                               "up" = "blue",
                               "no trend" = "grey20",
                               "no breakpoint" = "grey60")) +
  labs(x = NULL, y = NULL) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12))

trend_before_df <- breakpoint_grid %>% 
  mutate(trend_before=case_when(pvalue >= alpha_threshold~ "no breakpoint",
                               pval_before < alpha_threshold ~ trend_before,
                               pvalue >= alpha_threshold & pval_before >= alpha_threshold ~ "no breakpoint",
                               pval_before >= alpha_threshold ~ "no trend"))

table(trend_before_df$trend_before)


trend_before_df$trend_before <- factor(trend_before_df$trend_before, levels = c("up", "down", "no trend", "no breakpoint"))

#trend before breakpoint
trend_before <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = trend_before_df, 
          aes(color = trend_before, fill=trend_before)) +
  scale_color_manual("Trend before\nbreakpoint", 
                     values = c("down" = "red",  # Remplace par tes couleurs
                                "up" = "blue",
                                "no trend" = "grey20",
                                "no breakpoint" = "grey60")) +
  scale_fill_manual("Trend before\nbreakpoint",
                    values = c("down" = "red",
                               "up" = "blue",
                               "no trend" = "grey20",
                               "no breakpoint" = "grey60")) +
  labs(x = NULL, y = NULL) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55))+
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 12),  # Taille du titre de la légende
    legend.text = element_text(size = 12)) 

bp_year_df <- breakpoint_grid  %>% 
  mutate(breakpoint=case_when(pvalue >= 0.05 ~ NA,
                              .default=breakpoint)) 
  
#plot of breakpoint year
bp_year <- ggplot() +
  geom_sf(data = canada, color = "black") +
  geom_sf(data = US, color = "black") +
  geom_sf(data = bp_year_df, 
          aes(color = breakpoint, fill=breakpoint)) +
  # scale_colour_gradient(low = "red", high = "blue", 
  #                        name = "breakpoint year") +
  # scale_fill_gradient(low = "red",  high = "blue", 
  #                      name = "breakpoint year") +
  labs(x = NULL, y = NULL) +
  coord_sf(ylim = c(44, 63),
           xlim = c(-140, -55)) +
  scale_colour_viridis_c("Breakpoint year\u00A0\u00A0\u00A0\u00A0") +
  scale_fill_viridis_c("Breakpoint year\u00A0\u00A0\u00A0\u00A0") +
  theme(
    axis.title = element_text(size = 16),   
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 12),  # Taille du titre de la légende
    legend.text = element_text(size = 12)) 

trend_plots <- ggarrange(trend_plot, bp_year, trend_before, trend_after,
          nrow=4, ncol=1, 
          labels=c("A", "B", "C", "D"), 
          font.label = list(size = 18))

pdf(file = "C:/Users/msoubeyr/OneDrive - NRCan RNCan/Documents/NPP_drought/github/figures/trend.pdf", 
    width=5.5, height=8)
trend_plots
dev.off()

png(filename = "C:/Users/msoubeyr/OneDrive - NRCan RNCan/Documents/NPP_drought/github/figures/trend.png", 
    width=5.5*1.3, height=8*1.3, units = "in", res=500)
trend_plots
dev.off()

#Among the cells that continue to increase, do the slopes decrease or increase? 

head(breakpoint_grid)
a=breakpoint_grid %>% 
  filter(slope_before >0, slope_after>0, pvalue<alpha_threshold)

sum(a$slope_before<a$slope_after)/length(a$slope_after)