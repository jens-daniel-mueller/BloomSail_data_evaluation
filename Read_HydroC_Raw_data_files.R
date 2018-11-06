library(tidyverse)
library(data.table)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/HydroC-pCO2")
files <- list.files(pattern = "*.txt")

df <- files %>%
  map(read.delim, sep=";", skip=4, col.names = seq(1,23,1)) %>%
  reduce(rbind) %>% 
  select(seq(1,22,1))
  
names(df) <- c("Date",	"Time",	"Weekday",	"P_pump",
           "p_NDIR",	"p_in",	"I_total",	"U_supply",
          "Zero",	"Flush", "external_pump",	"Runtime",
          "Signal_raw",	"Signal_ref",	"T_sensor",
          "Signal_proc",	"Conc_estimate",	"pCO2_corr",
          "xCO2_corr",	"T_control",	"T_gas",	"%rH_gas")

df <- data.table(df)

ggplot(df[Zero==1], aes(Runtime, pCO2_corr))+
  geom_point()
