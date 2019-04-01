#### Load required packages ####

library(tidyverse)
library(lubridate)
library(here)
library(plotly)


#### Read data file as downloaded from HydroC ####

setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/TinaV/Sensor/HydroC-pCO2")
files <- list.files(pattern = "*.txt")

raw <- files %>%
  map(read.delim, sep=";", dec=",", skip=4, col.names = seq(1,23,1),
      colClasses = c(rep("character",2), rep("numeric", 21))) %>%
  reduce(rbind) %>%
  select(seq(1,22,1))

rm(files)

names(raw) <- c("Date",	"Time",	"Weekday",	"P_pump",
           "p_NDIR",	"p_in",	"I_total",	"U_supply",
          "Zero",	"Flush", "external_pump",	"Runtime",
          "Signal_raw",	"Signal_ref",	"T_sensor",
          "Signal_proc",	"Conc_estimate",	"pCO2_corr",
          "xCO2_corr",	"T_control",	"T_gas",	"%rH_gas")

raw <- raw %>%
  mutate(date_time = ymd_hms(paste(Date, Time)))


# #### Check logging frequency and pump switch from Seabird XX? (Power ~1W) to 5T (Power ~8W) ####
# 
# test <- raw %>%
#   filter(date_time > ymd("2018-07-01"),
#          date_time < ymd("2018-07-30"))
# 
# #Pump Switch
# test %>% 
#   filter(date_time > ymd("2018-07-16"),
#          date_time < ymd("2018-07-18")) %>% 
#   ggplot(aes(date_time, P_pump))+
#   geom_point()
# 
# #Logging frequency
# 
# test <- test %>%
#   mutate(frequency = c(0,diff(date_time)))
#   
# test %>%
#   filter(date_time > ymd_hm("2018-07-16 T 12:00"),
#          date_time < ymd_hm("2018-07-19 T 15:00")) %>%
#   ggplot(aes(date_time, frequency, shape=as.factor(Zero), col=as.factor(Flush)))+
#   geom_point()+
#   ylim(0,20)


#### Read Contros corrected data file ####

corr <-
  read_csv2(here("Data/TinaV/Sensor/HydroC-pCO2/corrected_Contros",
                 "parameter&pCO2s(method 43).txt"),
            col_names = c("date_time", "Zero", "Flush", "p_NDIR",
                          "p_in", "T_control", "T_gas", "%rH_gas",
                          "Signal_raw", "Signal_ref", "T_sensor",
                          "pCO2_corr", "Runtime", "nr.ave"))
                      
corr <- corr %>% 
  mutate(date_time = dmy_hms(date_time))



# #### merge raw and corrected data frames ####
# 
# df = full_join(raw, corr, by="date_time") %>% 
#   arrange(date_time) %>% 
#   slice(seq(1,n(),12))
# 
# 
# #### plots to check succesful read-in and data-quality ####
# 
# theme_set(theme_bw())
# 
# df %>% 
#   filter(Zero.x == 1) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr.x, col="raw"))+
#   geom_point(aes(date_time, pCO2_corr.y, col="corr"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   ylim(-5,30)+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="Zeroing pre and post drift correction | y-scale limits -5 -> 30 µatm")
# 
# ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_Zeroing.jpg"),
#        width = 10, height = 6)
# 
# df %>% 
#   filter(Zero.x == 0, Flush.x == 0,
#          Zero.y == 0, Flush.y == 0,
#          date_time>ymd_h("2018-07-24T01"),
#          date_time<ymd_h("2018-07-24T06")) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr.x, col="raw"))+
#   geom_point(aes(date_time, pCO2_corr.y, col="corr"))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="pCO2 (µatm)",
#        title="pCO2 measurements pre and post drift correction")
# 
# ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_example_pCO2.jpg"),
#        width = 10, height = 6)
# 
# df %>% 
#   filter(Zero.x == 0, Flush.x == 0,
#          Zero.y == 0, Flush.y == 0,
#          date_time>ymd_h("2018-07-24T01"),
#          date_time<ymd_h("2018-07-24T06")) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr.x - pCO2_corr.y))+
#   scale_color_brewer(palette = "Set1", name="Dataset")+
#   labs(x="Date",y="Delta pCO2 (µatm)",
#        title="Offset pCO2 measurements pre and post drift correction")
# 
# ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_example_pCO2_offset.jpg"),
#        width = 10, height = 6)
# 
# df %>% 
#   filter(Zero.x == 0, Flush.x == 0,
#          Zero.y == 0, Flush.y == 0) %>% 
#   ggplot()+
#   geom_point(aes(date_time, pCO2_corr.x - pCO2_corr.y, col=pCO2_corr.x))+
#   scale_color_viridis_c(name="pCO2_corr (uatm)")+
#   labs(x="Date",y="Delta pCO2 (µatm)",
#        title="Offset pCO2 measurements pre and post drift correction")
# 
# ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "Timeseries_all_pCO2_offset.jpg"),
#        width = 10, height = 6)
# 
# 
# df %>% 
#   filter(Zero.x == 0, Flush.x == 0,
#          Zero.y == 0, Flush.y == 0) %>% 
#   ggplot()+
#   geom_point(aes(pCO2_corr.x, pCO2_corr.x - pCO2_corr.y, col=date_time),
#              alpha=0.5)+
#   labs(x="pCO2 (µatm)",y="Delta pCO2 (µatm)",
#        title="Offset pCO2 measurements pre and post drift correction")
# 
# ggsave(here("Plots/TinaV/Sensor/HydroC_diagnostics", "pCO2_offset.jpg"),
#        width = 10, height = 6)

#### Determine Pump switch ####



#### Safe Contros corrected data file ####

write_csv(corr, here("Data/_summarized_data_files",
                     "Tina_V_Sensor_HydroC.csv"))



