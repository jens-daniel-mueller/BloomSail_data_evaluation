#### Load required packages ####

library(tidyverse)
library(lubridate)
library(here)

#### Read summarized sensor data file ####

Sensor <- read_csv(here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC_CT_O2.csv"))

Sensor <- Sensor %>% 
  mutate(pCO2 = as.numeric(pCO2),
         CT = as.numeric(CT),
         pCO2 = if_else(Zero==1, NaN, pCO2),
         pCO2 = if_else(Flush==1, NaN, pCO2))


#### Plot Surface water timeseries ####

Sensor_mean <- Sensor %>% 
  filter(Zero %in% c(0,NA),
         Flush %in% c(0,NA)) %>% 
  select(-c(Flush, Zero)) %>% 
  mutate(dep_int = cut(dep, seq(0,30,5))) %>% 
  group_by(ID, dep_int, type, cast) %>% 
  summarise_all(list("mean"), na.rm=TRUE) %>% 
  ungroup()


Sensor_mean_long <-
  Sensor_mean %>% 
  select(date_time, ID, dep=dep_int, cast, type,
         Salinity = sal,
         "Temperature (deg C)" = tem,
         "pCO2 (uatm)" = pCO2,
         "CT* (umol/kg)" = CT,
         "pH (not calibrate)" = pH,
         "O2 (% sat.)"=O2,
         "O2 (umol/l)"=O2_conc,
          Chl) %>% 
  gather("parameter", "value", 6:13)


theme_set(theme_bw())

Sensor_mean_long %>% 
  filter(!is.na(dep), type=="P") %>% 
  ggplot(aes(date_time, value, col=dep, shape=cast, linetype=cast))+
  geom_rect(aes(xmin=ymd_h("2018-07-05T12"), xmax=ymd_h("2018-07-24T01"),
                ymin=-Inf, ymax=Inf), alpha=0.8, col="grey80", fill="grey80")+
  geom_point()+
  geom_path()+
  labs(x="Date")+
  scale_color_viridis_d()+
  facet_wrap(~parameter, scales = "free_y", ncol = 2)

ggsave(here::here("/Plots/TinaV/Sensor", "Sensor_timeseries_all_parameters.jpg"), width = 10, height = 12)

