#### Load required packages ####

library(tidyverse)
library(here)

#### read data from sensor package ####

df <- read_csv(here::here("Data/_merged_data_files", "BloomSail_Ostergarnsholm_Sensor_track_HydroC.csv")) %>% 
  mutate(ID = as.factor(ID))

profiles <- df %>% 
  filter(station=="bo", cast=="down") %>%
  select(ID, cast, dep,
         Salinity = sal,
         "Temperature (deg C)" = tem,
         "pCO2 (uatm)" = pCO2,
         "O2 (sat.)"=O2) %>% 
  gather("parameter", "value", 4:7)

surface_ts_bouy_down <-
  df %>%
  filter(station=="bo", cast=="down", dep >2, dep <6) %>%
  select(ID, date_time,
         sal, tem, pCO2, O2) %>% 
  gather("parameter", "value", 3:6) %>% 
  group_by(ID, parameter) %>%
  summarise_all(list("mean","min","max", "sd"), na.rm=TRUE) %>%
  ungroup() %>% 
  select(-c(date_time_min, date_time_max, date_time_sd)) %>%
  rename(date_time=date_time_mean) 




#### plot profiles ####

profiles %>% 
  filter(cast=="down") %>%
  ggplot(aes(value, dep, col=ID))+
  geom_path()+
  scale_color_viridis_d()+
  scale_y_reverse()+
  facet_wrap(~parameter, scales = "free_x")+
  labs(x="", y="Depth [m]", title = "Downcast profiles at Ostergarnsholm bouy")+
  theme_bw()

ggsave(here::here("Plots/TinaV/Sensor/Ostergarnsholm","OGB_profiles_down.jpg"),
       width = 8, height = 7)  

df %>% 
  filter(station=="bo") %>%
  ggplot(aes(pCO2, dep, col=cast))+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=2, ymax=6, alpha=0.3)+
  geom_hline(yintercept = 4)+
  geom_path()+
  scale_color_brewer(palette = "Set1")+
  scale_y_reverse()+
  facet_wrap(~ID)+
  labs(x="pCO2 [µatm]", y="Depth [m]",
    title = "Post-processed pCO2 profiles at Ostergarnsholm bouy | no repsponse time correction")+
  theme_bw()

ggsave(here::here("Plots/TinaV/Sensor/Ostergarnsholm","OGB_pCO2_profiles_down_up.jpg"),
       width = 10, height = 10)  



#### plot surface timeseries data at bouy ####

surface_ts_bouy_down %>%   
ggplot(aes(x=date_time))+
  geom_errorbar(aes(ymax=value_max, ymin=value_min, col="Min-Max"))+
  geom_errorbar(aes(ymax=value_mean+value_sd, ymin=value_mean-value_sd, col="SD"))+
  geom_path(aes(y=value_mean))+
  geom_point(aes(y=value_mean))+
  theme_bw()+
  scale_color_brewer(palette = "Set1", name="Errorbars")+
  labs(x="Date", y="Value", title = "Surface water timeseries (2-6 m, downcast only) at Ostergarnsholm bouy")+
  facet_wrap(~parameter, scales = "free_y")

ggsave(here::here("Plots/TinaV/Sensor/Ostergarnsholm","OGB_surface_timeseries.jpg"),
       width = 10, height = 7)  


#### plot surface maps around Ostergarnsholm ####

map <- read_csv(here::here("Data/Maps", "Bathymetry_Gotland_east.csv"))

ggplot(map, aes(lon, lat, fill=elev))+
  geom_raster()+
  scale_fill_gradient(low = "grey20", high = "grey80", na.value = "black", name="Depth [m]")+
  coord_quickmap(expand = 0)+
  labs(x="Longitude [°E]", y="Latitude [°N]")+
  theme_bw()


df %>%
  filter(station %in% c("in", "ou")) %>% 
  ggplot()+
  geom_raster(data=map, aes(lon, lat, fill=elev))+
  scale_fill_gradient(low = "grey20", high = "grey80", na.value = "black", name="Depth [m]")+
  coord_quickmap(expand = 0, ylim = c(57.35,57.55), xlim = c(18.85,19.35))+
  labs(x="Longitude [°E]", y="Latitude [°N]")+
  geom_point(aes(lon, lat, col=pCO2))+
  scale_color_viridis_c()+
  facet_wrap(~ID)+
  theme_bw()
  
ggsave(here::here("Plots/TinaV/Sensor/Ostergarnsholm","OGB_surface_pCO2_maps.jpg"),
       width = 10, height = 7)  




#### plot pCO2 timeseries by transect ####

  
df %>% 
  ggplot()+
  geom_point(aes(date_time, pCO2, col=station))+
  scale_color_brewer(palette = "Set1")+
  labs(x="Date",y="pCO2 (µatm)",
       title="pCO2 measurements Ostergarnsholm | post drift correction")+
  facet_wrap(~ID, scales = "free_x")

ggsave(here::here("Plots/TinaV/Sensor/Ostergarnsholm", 
                  "OGB_pCO2_timeseries_by_transect.jpg"),
       width = 12, height = 9)



