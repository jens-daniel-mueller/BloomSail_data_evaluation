#### load required packages ####

library(tidyverse)
library(lubridate)
library(here)


#### read data set from relative path ####
df <- read_csv(here("Data/_summarized_data_files", "TinaV_bottle_nutrients.csv"))

#### Produce plots ####

df.long <-
  df %>% 
  gather("parameter", "value", 4:8)


df.long %>% 
  filter(station %in% c("P07", "P10"),
         parameter %in% c("NO23", "PO4")) %>% 
  ggplot(aes(ymd(ID), value, fill=dep))+
  geom_point(shape=21)+
  scale_fill_viridis_c(name = "Depth (m)")+
  labs(x="Date", y="Concentration (µM)")+
  facet_grid(parameter~station, scales = "free", labeller = label_both)+
  theme_bw()

ggsave(here("Plots/TinaV/Bottle/Nutrients", "BloomSail_Nuts_P07-P10.jpg"))


