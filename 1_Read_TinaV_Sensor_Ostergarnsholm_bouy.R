#### Load required packages ####

library(tidyverse)
library(here)

#### read data from sensor package ####

df <- read_csv(here::here("Data/_merged_data_files", "BloomSail_Ostergarnsholm_Sensor_track.csv"))


#### subset surface data from sensor package and calculate mean ####


# surface.ts <-
#   df %>% 
#   filter(label=="bo", cast=="down", Dep.S >2, Dep.S <6) %>%
#   group_by(start.date) %>% 
#   summarise(date = mean(date),
#             pCO2.max = max(pCO2),
#             pCO2.min = min(pCO2),
#             pCO2.mean = mean(pCO2),
#             Tem.S.max = max(Tem.S),
#             Tem.S.min = min(Tem.S),
#             Tem.S.mean = mean(Tem.S),
#             Sal.S.max = max(Sal.S),
#             Sal.S.min = min(Sal.S),
#             Sal.S.mean = mean(Sal.S)) %>% 
#   ungroup()

#### plot profiles ####

df %>% 
  filter(label=="bo", cast=="down") %>%
ggplot(aes(pCO2, Dep.S, col=as.factor(start.date)))+
  geom_path()+
  scale_color_paletteer_d("ggthemes", tableau_cyclic, name="Date")+
  scale_y_reverse()+
  labs(x="pCO2 [µatm]", y="Depth [m]", title = "Raw pCO2 profiles at Ostergarnsholm bouy")+
  theme_bw()

ggsave("OGB_pCO2_profiles.jpg")  

df %>% 
  filter(label=="bo", cast=="down") %>%
ggplot(aes(Tem.S, Dep.S, col=as.factor(start.date)))+
  geom_path()+
  scale_color_paletteer_d("ggthemes", tableau_cyclic, name="Date")+
  scale_y_reverse()+
  labs(x="Temp [°C]", y="Depth [m]", title = "Temperature profiles at Ostergarnsholm bouy")+
  theme_bw()

ggsave("OGB_Tem_profiles.jpg")  


df %>% 
  filter(label=="bo", cast=="down", Dep.S > 2) %>%
ggplot(aes(Sal.S, Dep.S, col=as.factor(start.date)))+
  geom_path()+
  scale_color_paletteer_d("ggthemes", tableau_cyclic, name="Date")+
  scale_y_reverse()+
  labs(x="Salinity", y="Depth [m]", title = "Salinity profiles at Ostergarnsholm bouy")+
  theme_bw()

ggsave("OGB_Sal_profiles.jpg")  


#### plot surface timeseries data at bouy ####

surface.ts %>%   
ggplot()+
  geom_errorbar(aes(date, ymax=pCO2.max, ymin=pCO2.min))+
  geom_path(aes(date, pCO2.mean))+
  geom_point(aes(date, pCO2.mean))+
  theme_bw()+
  labs(x="Date", y="pCO2 [µatm]", title = "Min, mean, and max pCO2 at bouy in 2-6 m water depth")

ggsave("OGB_surface_pCO2_timeseries.jpg")  

surface.ts %>%   
ggplot()+
  geom_errorbar(aes(date, ymax=Tem.S.max, ymin=Tem.S.min))+
  geom_path(aes(date, Tem.S.mean))+
  geom_point(aes(date, Tem.S.mean))+
  theme_bw()+
  labs(x="Date", y="Temp [°C]", title = "Min, mean, and max Temperature at bouy in 2-6 m water depth")

ggsave("OGB_surface_Tem_timeseries.jpg")  

surface.ts %>%   
ggplot()+
  geom_errorbar(aes(date, ymax=Sal.S.max, ymin=Sal.S.min))+
  geom_path(aes(date, Sal.S.mean))+
  geom_point(aes(date, Sal.S.mean))+
  theme_bw()+
  labs(x="Date", y="Salinity", title = "Min, mean, and max Salinity at bouy in 2-6 m water depth")

ggsave("OGB_surface_Sal_timeseries.jpg")  


#### plot surface maps around Ostergarnsholm ####

map <- read.csv("C:/Mueller_Jens_Data/Sea_charts/EMODnet/D6_2018.xyz/Bathymetry_Gotland_east.csv")

ggplot(map, aes(lon, lat, fill=elev))+
  geom_raster()+
  scale_fill_gradient(low = "grey20", high = "grey80", na.value = "black", name="Depth [m]")+
  coord_quickmap(expand = 0)+
  labs(x="Longitude [°E]", y="Latitude [°N]")+
  theme_bw()


all %>%
  filter(label %in% c("in", "ou")) %>% 
ggplot()+
  geom_raster(data=map, aes(lon, lat, fill=elev))+
  scale_fill_gradient(low = "grey20", high = "grey80", na.value = "black", name="Depth [m]")+
  coord_quickmap(expand = 0, ylim = c(57.35,57.55), xlim = c(18.85,19.35))+
  labs(x="Longitude [°E]", y="Latitude [°N]")+
  geom_point(aes(lon, lat, col=pCO2))+
  scale_color_paletteer_c("oompaBase", "jetColors", name="pCO2 [µatm]")+
  facet_wrap(~transect.ID)+
  theme_bw()
  
ggsave("OGB_surface_pCO2_map.jpg", width = 10, height = 7)  


  



