library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/HydroC-pCO2")
files <- list.files(pattern = "*.txt")

df <- files %>%
  map(read.delim, sep=";", dec=",", skip=4, col.names = seq(1,23,1),
      colClasses = c(rep("character",2), rep("numeric", 21))) %>%
  reduce(rbind) %>% 
  select(seq(1,22,1))

rm(files)
  
names(df) <- c("Date",	"Time",	"Weekday",	"P_pump",
           "p_NDIR",	"p_in",	"I_total",	"U_supply",
          "Zero",	"Flush", "external_pump",	"Runtime",
          "Signal_raw",	"Signal_ref",	"T_sensor",
          "Signal_proc",	"Conc_estimate",	"pCO2_corr",
          "xCO2_corr",	"T_control",	"T_gas",	"%rH_gas")

df <- data.table(df)
df$date <- ymd_hms(paste(df$Date, df$Time))

zero <- data.table(
  df[Zero==1] %>%
  arrange(date) %>%   
  mutate(Diff = Runtime - lag(Runtime)))

zero[Diff > 11]


ggplot(zero, aes(date, Diff))+
  geom_point()


ggplot(df[Zero==1], aes(date, pCO2_corr))+
  geom_point()+
  ylim(0,70)






