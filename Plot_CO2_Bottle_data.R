library(ggplot2)
library(tidyverse)
library(data.table)
library(lubridate)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Merged_data_files")
Sensor <- data.table(read.csv("BloomSail_Sensor_Track_data.csv"))

Sensor$date <- ymd_hms(Sensor$date)
Sensor$start.date <- ymd_hms(Sensor$start.date)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Bottle/CO2")
Bottle <- data.table(read.csv("BloomSail_bottle_CO2_all.csv"))

# Bottle$date <- ymd(Bottle$transect.ID)
# 
# Bottle$CT <- as.numeric(as.character(Bottle$CT))
# 
# Bottle %>% 
#   filter(label %in% c("P07", "P10")) %>% 
#   ggplot()+
#   geom_path(aes(date, CT, col=as.factor(Dep)))+
#   facet_wrap(~label)


stations <-
  Sensor %>% 
  filter(label %in% unique(Bottle$label)) %>% 
  group_by(transect.ID, label) %>% 
  summarise(date = mean(date),
            lat = mean(lat),
            lon = mean(lon)) %>% 
  ungroup()


df <- merge(Bottle, stations, all=TRUE)
df$CT <- as.numeric(as.character(df$CT))
df <- na.omit(df)

df %>% 
  filter(label %in% c("P07", "P10")) %>% 
  ggplot()+
    geom_path(aes(date, CT, col=as.factor(Dep)))+
    facet_wrap(~label)
  





