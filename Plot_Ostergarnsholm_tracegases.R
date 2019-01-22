#### load required packages ####

library(tidyverse)
library(lubridate)
library(here)


#### read data set from relative path ####
tracegases <- read_csv(here("Data/Bottle/Gases", "BloomSail_bottle_tracegases_all.csv"))


#### calculate date column and gather sampled parameters ####

tracegases <- 
  tracegases %>% 
  mutate(date.time = ymd_hm(paste(date, time, sep = "T")))

tracegases.long <-
  tracegases %>% 
  gather("parameter", "value", 5:7)

#### plot results ####

tracegases.long %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(date.time, value))+
  geom_line()+
  geom_point(aes(col=label))+
  scale_color_brewer(palette = "Set1")+
  facet_grid(parameter~., scales = "free_y")+
  theme_bw()

ggsave(here("Plots/Tracegases",
            "Ostergarnsholm_tracegases_Summer2018.jpg"), height = 120, width = 160, units = "mm")


