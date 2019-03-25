#### Load required libraries ####

library(tidyverse)
library(here)
library(seacarb)

#### Load summarized Sensor and HydroC Data ####

Sensor <- read_csv(here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC.csv")) %>% 
  mutate(pCO2 = as.numeric(pCO2))


####calcualte CT* based on measured pCO2, S, and T, as well as the Alkalinity
#### * in CT indicates that absolute values are somewhat vague, but changes in CT are represented well
#### underlying pCO2 data still require Response time (RT) correction

Sensor %>% 
  filter(is.na(Flush))




Sensor_CT <- Sensor %>%
  filter(!is.na(pCO2),
         !is.na(sal),
         !is.na(tem),
         !is.na(Flush),
         !is.na(Zero),
         Zero != 1,
         Flush !=1,
         pCO2 > 60) %>%
  mutate(CT = carb(24, var1=pCO2, var2=1720*1e-6,
                   S=sal, T=tem, P=dep/10, k1k2="m10", kf="dg", ks="d",
                   pHscale="T", gas="insitu")[,16]*1e6)




Sensor_CT %>% 
  filter(dep<5, dep>2) %>% 
  ggplot(aes(date_time, CT, col=type)) +
  geom_point()


df <- full_join(Sensor, Sensor_CT)

df %>% 
  filter(dep<5, dep>2, station=="P07") %>% 
  ggplot(aes(O2, pH, col=ID))+
  geom_point()+
  geom_path()+
  scale_color_viridis_c()


write_csv(df, here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC_CT.csv"))

