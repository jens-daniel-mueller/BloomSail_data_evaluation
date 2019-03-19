#### Load required packages ####

library(tidyverse)
library(here)

#### Read summarized sensor data file ####

Sensor <- read_csv(here("Data/_summarized_data_files", "Tina_V_Sensor_Profiles_Transects.csv"))

Sensor <- Sensor %>% 
  mutate(pCO2 = as.numeric(pCO2),
         pCO2 = if_else(pCO2<60, NaN, pCO2))


#### Plot Surface water timeseries ####

Sensor_profile_surface <- Sensor %>% 
  filter(dep < 4, dep > 2, cast == "down", type == "P")

Sensor_transect <- Sensor %>% 
  filter(dep < 4, dep > 0.5, type == "T")

Sensor_profile_surface_long <-
  Sensor_profile_surface %>% 
  select(date_time, ID, station,
         Salinity = sal,
         "Temperature (deg C)" = tem,
         "pCO2 (uatm)" = pCO2,
         "pH (not calibrate)" = pH,
         "O2 (% sat.)"=O2,
          Chl) %>% 
  gather("parameter", "value", 4:9)

Sensor_profile_surface_long_mean <-
  Sensor_profile_surface_long %>% 
  group_by(ID, parameter) %>% 
  summarise_all(mean, na.rm=TRUE) %>% 
  ungroup()

Sensor_transect_long <-
  Sensor_transect %>% 
  select(date_time, ID, station,
         Salinity = sal,
         "Temperature (deg C)" = tem,
         "pCO2 (uatm)" = pCO2,
         "pH (not calibrate)" = pH,
         "O2 (% sat.)"=O2,
         Chl) %>% 
  gather("parameter", "value", 4:9)





ggplot()+
  geom_point(data=Sensor_transect_long, aes(date_time, value, col="Transect (0.5-4m)"), shape=21)+
  geom_point(data=Sensor_profile_surface_long, aes(date_time, value, col="Profiles (2-4m)"), shape=21)+
  geom_path(data=Sensor_profile_surface_long_mean, aes(date_time, value, col="Profiles (2-4m)"))+
  scale_color_brewer(palette = "Set1", name = "Recording")+
  labs(x="Date")+
  facet_grid(parameter~., scales = "free_y")+
  theme_bw()

ggsave(here("/Plots/TinaV/Sensor", "Sensor_surface_timeseries_all_parameters.jpg"), width = 6, height = 10)

