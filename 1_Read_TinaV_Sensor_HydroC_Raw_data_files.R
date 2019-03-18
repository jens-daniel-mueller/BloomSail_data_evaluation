library(tidyverse)
library(data.table)
library(lubridate)


#### Read data file as downloaded from HydroC ####

setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/TinaV/Sensor/HydroC-pCO2")
files <- list.files(pattern = "*.txt")

df <- files %>%
  map(read.delim, sep=";", dec=",", skip=4, col.names = seq(1,23,1),
      colClasses = c(rep("character",2), rep("numeric", 21))) %>%
  reduce(rbind) %>% 
  select(seq(1,22,1)) %>% 
  setDT()

rm(files)
  
names(df) <- c("Date",	"Time",	"Weekday",	"P_pump",
           "p_NDIR",	"p_in",	"I_total",	"U_supply",
          "Zero",	"Flush", "external_pump",	"Runtime",
          "Signal_raw",	"Signal_ref",	"T_sensor",
          "Signal_proc",	"Conc_estimate",	"pCO2_corr",
          "xCO2_corr",	"T_control",	"T_gas",	"%rH_gas")

df$date.time <- ymd_hms(paste(df$Date, df$Time))

# #### Read Contros corrected data file ####
# 
# setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/TinaV/Sensor/HydroC-pCO2/corrected_Contros")
# 
# df_corr <-
#   data.table(read.delim("parameter&pCO2s(method 43).txt", sep=";", dec=",", header = FALSE))#,
#                         colClasses = c(rep("character",2), rep("numeric", 21))))
# 
# rm(files)
# 
# names(df) <- c("Date",	"Time",	"Weekday",	"P_pump",
#            "p_NDIR",	"p_in",	"I_total",	"U_supply",
#           "Zero",	"Flush", "external_pump",	"Runtime",
#           "Signal_raw",	"Signal_ref",	"T_sensor",
#           "Signal_proc",	"Conc_estimate",	"pCO2_corr",
#           "xCO2_corr",	"T_control",	"T_gas",	"%rH_gas")
# 
# df$date.time <- ymd_hms(paste(df$Date, df$Time))


#### plots to check succesful read-in and data-quality ####

df %>% 
  filter(Zero == 0, Flush == 0) %>% 
  ggplot(aes(date.time, pCO2_corr))+
  geom_point()

df %>% 
  filter(Zero == 0, Flush == 0) %>% 
  ggplot(aes(date.time, p_NDIR))+
  geom_point()

df %>% 
  filter(Zero == 0, Flush == 0) %>% 
  ggplot(aes(date.time, P_pump))+
  geom_point()


setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/Summarized_data_files")
write.csv(df, "Tina_V_Sensor_HydroC.csv", row.names = FALSE)



