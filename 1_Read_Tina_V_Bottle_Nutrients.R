#### load required packages ####

library(tidyverse)
library(lubridate)
library(here)


#### read data set from relative path ####
df <- read_csv(here("Data/Bottle/Gases", "BloomSail_bottle_tracegases_all.csv"))




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
  





