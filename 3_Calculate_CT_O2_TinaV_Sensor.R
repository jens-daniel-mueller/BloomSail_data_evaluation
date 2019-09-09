#### Load required libraries ####

library(tidyverse)
library(seacarb)
library(marelac)

#### Load summarized Sensor and HydroC Data ####

Sensor <- read_csv(here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC.csv"),
                   col_types = list("pCO2_int" = col_double())) %>% 
  mutate(pCO2 = as.numeric(pCO2_int))


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


####calcualte O2 Concentration based on measured O2 saturation ####

source("O2stoO2c.R")

Sensor_O2 <- Sensor %>%
  mutate(O2 = as.numeric(O2)) %>% 
  filter(!is.na(tem),
         !is.na(sal),
         !is.na(O2)) %>% 
  mutate(#O2_conc_marelac = gas_satconc(S=sal, t=tem, species = "O2")*O2/100,
         O2_conc = O2stoO2c(O2sat = O2, T=tem, S=sal, P=dep/10, p_atm = 1013.5))

df <- full_join(df, Sensor_O2)


df %>% 
  filter(dep<5, dep>2) %>% 
  ggplot(aes(date_time, O2_conc, col=type)) +
  geom_point()

df %>% 
  filter(dep<5, dep>2) %>% 
  ggplot(aes(O2, O2_conc, col=tem)) +
  geom_point()



write_csv(df, here::here("Data/_merged_data_files", "BloomSail_Sensor_HydroC_CT_O2.csv"))

