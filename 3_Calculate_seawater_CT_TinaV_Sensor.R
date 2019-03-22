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


#Sensor[transect.ID == "180616"]$pCO2 <- NA

# Sensor$CT <- 0
# Sensor$CT <- NA

Sensor <- Sensor %>%
  filter(!is.na(pCO2),
         !is.na(sal),
         !is.na(tem),
         !is.na(dep),
         pCO2 >30) %>%
  mutate(CT = carb(24, var1=pCO2, var2=1720*1e-6,
                   S=sal, T=tem, P=dep/10, k1k2="m10", kf="dg", ks="d",
                   pHscale="T", gas="insitu")[,16]*1e6)

Sensor %>% 
  filter(dep<5, dep>2) %>% 
  ggplot(aes(date_time, CT, col=Flush)) +
  geom_point()
