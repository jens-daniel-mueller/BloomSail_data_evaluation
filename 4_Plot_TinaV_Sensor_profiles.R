#### Load required packages ####

library(tidyverse)
library(here)

#### Read summarized sensor data file ####

Sensor <- read_csv(here("Data/_summarized_data_files", "Tina_V_Sensor_Profiles_Transects.csv"))

Sensor <- Sensor %>% 
  mutate(pCO2 = as.numeric(pCO2))


#### Plot vertical profiles ####

Sensor_profile <- Sensor %>% 
  filter(type == "P")

Sensor_profile_long <-
  Sensor_profile %>% 
  select(date_time, ID, station, cast, dep,
         Salinity = sal,
         "Temperature (deg C)" = tem,
         "pCO2 (uatm)" = pCO2,
         "pH (not calibrate)" = pH,
         "O2 (% sat.)"=O2,
          Chl) %>% 
  gather("parameter", "value", 6:11)

# Sensor_profile_long_mean <-
#   Sensor_profile_surface_long %>% 
#   group_by(ID, parameter) %>% 
#   summarise_all(mean, na.rm=TRUE) %>% 
#   ungroup()


#### Plot vertical Profiles ####



ggplot()+
  geom_path(data = Sensor[type=="P" & cast == "down"], aes(Sal.S, Dep.S, col=transect.ID))+
  scale_y_reverse()+
  facet_wrap(~label)+
  scale_x_continuous(limits = c(6.5,7.5), breaks = seq(0,35,0.5))+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  labs(x="Salinity", y="Depth [m]")

ggsave(here("/Plots/TinaV/Sensor", "Sensor_surface_timeseries_all_parameters.jpg"), width = 6, height = 10)


