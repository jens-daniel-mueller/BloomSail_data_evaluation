#### Load required packages ####

library(tidyverse)
library(here)

#### Read summarized sensor data file ####

Sensor <- read_csv(here("Data/_merged_data_files", "BloomSail_Sensor_HydroC_CT.csv"))

Sensor <- Sensor %>% 
  mutate(pCO2 = as.numeric(pCO2),
         CT = as.numeric(CT),
         pCO2 = if_else(Zero==1, NaN, pCO2),
         pCO2 = if_else(Flush==1, NaN, pCO2))


#### Plot Surface water timeseries ####

Sensor_mean <- Sensor %>% 
  filter(Zero %in% c(0,NA),
         Flush %in% c(0,NA)) %>% 
  mutate(dep_int = cut(dep, seq(0,30,5))) %>% 
  group_by(ID, dep_int, type, cast) %>% 
  summarise_all(mean, na.rm=TRUE) %>% 
  ungroup()

# Sensor_profile_surface <- Sensor %>% 
#   filter(dep < 4, dep > 2, cast == "down", type == "P")
# 
# Sensor_transect <- Sensor %>% 
#   filter(dep < 4, dep > 0.5, type == "T")

Sensor_mean_long <-
  Sensor_mean %>% 
  select(date_time, ID, dep=dep_int, cast, type,
         Salinity = sal,
         "Temperature (deg C)" = tem,
         "pCO2 (uatm)" = pCO2,
         "CT (umol/kg)" = CT,
         "pH (not calibrate)" = pH,
         "O2 (% sat.)"=O2,
          Chl) %>% 
  gather("parameter", "value", 6:12)

# 
# Sensor_transect_long <-
#   Sensor_transect %>% 
#   select(date_time, ID, station,
#          Salinity = sal,
#          "Temperature (deg C)" = tem,
#          "pCO2 (uatm)" = pCO2,
#          "CT (umol/kg)" = CT,
#          "pH (not calibrate)" = pH,
#          "O2 (% sat.)"=O2,
#          Chl) %>% 
#   gather("parameter", "value", 4:10)
# 




# ggplot()+
#   geom_point(data=Sensor_transect_long, aes(date_time, value, col="Transect (0.5-4m)"), shape=21)+
#   geom_point(data=Sensor_profile_surface_long, aes(date_time, value, col="Profiles (2-4m)"), shape=21)+
#   geom_path(data=Sensor_profile_surface_long_mean, aes(date_time, value, col="Profiles (2-4m)"))+
#   scale_color_brewer(palette = "Set1", name = "Recording")+
#   labs(x="Date")+
#   facet_grid(parameter~., scales = "free_y")+
#   theme_bw()

theme_set(theme_bw())

Sensor_mean_long %>% 
  filter(cast=="down", type=="P", !is.na(dep)) %>% 
  ggplot(aes(date_time, value, col=dep))+
  geom_point()+
  geom_path()+
  labs(x="Date")+
  facet_grid(parameter~., scales = "free_y")

ggsave(here("/Plots/TinaV/Sensor", "Sensor_timeseries_all_parameters.jpg"), width = 6, height = 12)

