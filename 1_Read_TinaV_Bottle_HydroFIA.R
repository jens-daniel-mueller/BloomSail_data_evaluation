library(data.table)
library(lubridate)
library(tidyverse)
library(here)


#### Read and transform HydroFIA pH dataset ####


HF <- read_csv(here("Data/TinaV/Bottle/pH_HydroFIA", "2019-02-26-14-05-14-pH-recalculated.csv")) %>% 
  mutate(ID = str_sub(sampleName, 1,6),
         station = str_sub(sampleName, 8,10),
         dep = as.numeric(str_sub(sampleName, 12,13))) %>% 
  select(date_time_meas=timeStamp, ID, station, dep,
         salinity, tem_meas=temperature, pHT=ph_mueller, pHT_error=ph_mueller_error) %>% 
  filter(!is.na(dep))


#### Check data quality ####

HF %>% 
  filter(station  %in% c("P07", "P10")) %>% 
  ggplot(aes(date_time_meas, pHT))+
  geom_path()+
  geom_point()+
  scale_y_continuous(breaks = seq(5,10,0.005))+
  facet_wrap(~interaction(ID,station,dep), scales = "free")

HF <- HF %>% 
  group_by(ID,station,dep) %>% 
  slice(tail(row_number(), 3)) %>%  
  ungroup()


HF %>% 
  filter(station  %in% c("P07", "P10")) %>% 
  ggplot(aes(pHT, dep, col=ID))+
  geom_path()+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(~station)+
  scale_color_brewer(palette = "Spectral")+
  labs(x="pH [spec, Mueller, 25C]", y="Depth [m]")


# HF %>% 
#   filter(station  %in% c("P07", "P10")) %>%   
#   ggplot(aes(date.time, pHT, col=as.factor(dep)))+
#   geom_path()+
#   geom_point()+
#   facet_wrap(~station)



write_csv(HF, here("Data/_summarized_data_files", "TinaV_bottle_pH_HydroFIA.csv"))
