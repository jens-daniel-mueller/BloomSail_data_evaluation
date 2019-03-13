#### load required packages ####

library(tidyverse)
library(lubridate)
library(here)


#### read data set from relative path ####
df <- read_csv(here("Data/TinaV/Bottle/DOC", "BloomSail_DOC_Jenny_Jeschek_IOW.csv"))

df <-
df %>% 
  select(-`PG-NOM_ID`)


#### Produce plot to check data quality and correctness ####

df.long <-
  df %>% 
  gather("parameter", "value", 5:6)


df.long %>% 
  # filter(station %in% c("P07", "P10"),
  #        parameter %in% c("NO23", "PO4")) %>% 
  ggplot(aes(ymd(ID), value, fill=dep))+
  geom_point(shape=21)+
  scale_fill_viridis_c(name = "Depth (m)")+
  labs(x="Date", y="Concentration (µM)")+
  facet_grid(parameter~station, scales = "free", labeller = label_both)+
  theme_bw()

#### Write summary data file ####

write_csv(df, here("Data/_summarized_data_files", "TinaV_bottle_DOC-DN.csv"))

