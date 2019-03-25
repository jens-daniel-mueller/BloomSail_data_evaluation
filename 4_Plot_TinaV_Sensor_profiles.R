#### Load required packages ####

library(tidyverse)
library(here)

#### Read summarized sensor data file ####

Sensor <- read_csv(here("Data/_merged_data_files", "BloomSail_Sensor_HydroC_CT.csv"))

Sensor <- Sensor %>% 
  mutate(pCO2 = as.numeric(pCO2),
         CT = as.numeric(CT),
         ID = as.factor(ID),
         Chl = if_else(dep<1.5, NaN, Chl)) %>% 
  filter(!(station %in% c("PX1", "PX2")))

Sensor_all <- Sensor

Sensor <- Sensor %>% 
  filter(cast == "down")

#### Plot vertical profiles ####

Sensor_profile <- Sensor %>% 
  filter(type == "P")

Sensor_profile_long <-
  Sensor_profile %>% 
  select(date_time, ID, station, cast, dep,
         Salinity = sal,
         "Temperature (deg C)" = tem,
         "pCO2 (uatm)" = pCO2,
         "CT (umol/kg)" = CT,
         "pH (not calibrate)" = pH,
         "O2 (sat.)"=O2,
          Chl) %>% 
  gather("parameter", "value", 6:12)


Sensor_profile_long_mean <-
  Sensor_profile_long %>%
  mutate(dep_int = as.numeric(cut(dep, seq(0.5,40,1), labels = seq(1,39,1)))) %>%
  group_by(ID, parameter, dep_int) %>%
  summarise_all(mean, na.rm=TRUE) %>%
  ungroup()


#### Plot vertical Profiles ####


Sensor_profile_long_mean %>% 
ggplot()+
  geom_path(aes(value, dep_int, col=ID, group=interaction(ID, station)))+
  scale_y_reverse()+
  facet_wrap(~parameter, scales = "free_x", ncol = 2)+
  scale_color_viridis_d(name="Date")+
  labs(y="Depth [m]")+
  theme_bw()

ggsave(here("/Plots/TinaV/Sensor", "Sensor_profiles_by_parameters_mean.jpg"), width = 10, height = 14, dpi=300)

Sensor_profile_long %>% 
ggplot()+
  geom_path(aes(value, dep, col=ID, group=interaction(ID, station)))+
  scale_y_reverse()+
  facet_wrap(~parameter, scales = "free_x", ncol = 2)+
  scale_color_viridis_d(name="Date")+
  labs(y="Depth [m]")+
  theme_bw()

ggsave(here("/Plots/TinaV/Sensor", "Sensor_profiles_by_parameters.jpg"), width = 10, height = 14, dpi=300)


Sensor_profile_long %>% 
ggplot()+
  geom_path(aes(value, dep, col=ID))+
  scale_y_reverse()+
  facet_grid(station~parameter, scales = "free_x")+
  scale_color_viridis_d(name="Date")+
  labs(y="Depth [m]")+
  theme_bw()

ggsave(here("/Plots/TinaV/Sensor", "Sensor_profiles_by_parameters_station.jpg"), width = 11, height = 15, dpi=300)


Sensor_profile_long %>% 
ggplot()+
  geom_path(aes(value, dep, col=station))+
  scale_y_reverse()+
  facet_grid(ID~parameter, scales = "free_x")+
  #scale_color_viridis_d(name="Station")+
  labs(y="Depth [m]")+
  theme_bw()

ggsave(here("/Plots/TinaV/Sensor", "Sensor_profiles_by_parameters_Date.jpg"), width = 11, height = 15, dpi=300)


#### Plot all vertical profiles individually ####


rm(i_ID, i_parameter, i_station)

for(i_ID in unique(Sensor_profile_long$ID)){
  for(i_station in unique(Sensor_profile_long$station)){
    for(i_parameter in unique(Sensor_profile_long$parameter)){

Sensor_profile_long %>% 
  filter(ID == i_ID,
         station == i_station,
         parameter == i_parameter) %>% 
  ggplot(aes(value, dep, col=cast))+
  geom_path()+
  scale_y_reverse()+
  scale_color_brewer(palette = "Set1")+
  labs(y="Depth [m]", x=i_parameter, title = str_c("Date: ",i_ID," | Station: ",i_station))+
  theme_bw()

print(str_c(i_ID,"_",i_station,"_",i_parameter,".jpg"))
ggsave(here("/Plots/TinaV/Sensor/all_profiles", str_c(i_ID,"_",i_station,"_",i_parameter,".jpg")),
       width = 5, height = 5)

    }
  }
}


