#### load required packages ####

library(tidyverse)
library(lubridate)
library(here)


#### read data set from relative path ####
df <- read_csv(here("Data/_summarized_data_files", "TinaV_bottle_DOC-DN.csv"))

df.int <-
df %>% 
  select(-Comment) %>% 
  filter(station %in% c("P07", "P10")) %>% 
  mutate(sal=as.numeric(sal),
         dep.int = cut(dep, seq(0,50,10), right = TRUE, include.lowest = TRUE)) %>% 
  group_by(ID, station, dep.int) %>% 
  summarise_all("mean", na.rm=TRUE) %>% 
  ungroup()

#### Produce plots ####

df.long <-
  df.int %>% 
  gather("parameter", "value", 6:7)


df.long %>% 
  ggplot(aes(ymd(ID), value, fill=dep.int, col=dep.int))+
  geom_point(shape=21)+
  geom_path()+
  scale_fill_viridis_d(name = "Depth (m)")+
  scale_color_viridis_d(name = "Depth (m)")+
  labs(x="Date", y="Concentration (µM)")+
  facet_grid(parameter~station, scales = "free", labeller = label_both)+
  theme_bw()

ggsave(here("Plots/TinaV/Bottle/DOC_DN", "BloomSail_DOC-DN_P07-P10.jpg"))



df %>% 
  filter(station %in% c("P07","P10")) %>% 
  ggplot(aes(ymd(ID), DOC, fill=dep))+
  geom_point(shape=21)+
  geom_point(data=df[!is.na(df$Comment) &
                       df$station %in% c("P07","P10"),], aes(ymd(ID), DOC), shape=21, col="red")+
  scale_fill_viridis_c(name = "Depth (m)")+
  labs(x="Date", y="Concentration (µM)")+
  facet_wrap(~station, scales = "free", labeller = label_both)+
  theme_bw()

ggsave(here("Plots/TinaV/Bottle/DOC_DN", "BloomSail_DOC-DN_P07-P10.jpg"))


