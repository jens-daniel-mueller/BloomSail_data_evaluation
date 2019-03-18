library(tidyverse)
library(here)


Bottle <- read_csv(here("Data/TinaV/Bottle/Tracegases", "BloomSail_bottle_CO2_all.csv"),
                   col_types = list("c","c","n","n","n","n","n"))

Bottle <- Bottle %>% 
  select(ID=transect.ID,
         station=label,
         dep=Dep,
         sal=Sal,
         CT, AT,
         pH_Mosley = pH.Mosley)

Bottle %>% 
  ggplot()+
  geom_path(aes(pH_Mosley,dep))+
  geom_point(aes(pH_Mosley,dep))+
  scale_y_reverse()+
  facet_wrap(~interaction(station,ID))+
  scale_color_brewer(palette = "Spectral")+
  labs(x="pH [spec, Mueller, 25C]", y="Depth [m]")

Bottle %>% 
  filter(station %in% c("P07", "P10")) %>% 
  ggplot()+
  geom_path(aes(AT,dep, col=ID))+
  geom_point(aes(AT,dep, col=ID))+
  scale_y_reverse()+
  facet_wrap(~station)

Bottle %>% 
  filter(station %in% c("P07", "P10")) %>% 
  ggplot()+
  geom_path(aes(CT,dep, col=ID))+
  geom_point(aes(CT,dep, col=ID))+
  scale_y_reverse()+
  facet_wrap(~station)

Bottle %>% 
  filter(station %in% c("P07", "P10")) %>% 
  ggplot()+
  geom_path(aes(sal,dep, col=ID))+
  geom_point(aes(sal,dep, col=ID))+
  scale_y_reverse()+
  facet_wrap(~station)


Bottle %>% 
  filter(station %in% c("P07", "P10"), dep<10) %>% 
  ggplot()+
  geom_path(aes(ymd(ID),CT, col=as.factor(dep)))+
  facet_wrap(~station)




write_csv(Bottle, here("Data/_summarized_data_files", "TinaV_bottle_CO2_lab.csv"))
