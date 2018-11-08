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
  select(seq(1,22,1)) %>% 
  setDT()

rm(files)
  
names(df) <- c("Date",	"Time",	"Weekday",	"P_pump",
           "p_NDIR",	"p_in",	"I_total",	"U_supply",
          "Zero",	"Flush", "external_pump",	"Runtime",
          "Signal_raw",	"Signal_ref",	"T_sensor",
          "Signal_proc",	"Conc_estimate",	"pCO2_corr",
          "xCO2_corr",	"T_control",	"T_gas",	"%rH_gas")

df$date <- ymd_hms(paste(df$Date, df$Time))

# zero <- data.table(
#   df[Zero==1] %>%
#   arrange(date) %>%   
#   mutate(Diff = Runtime - lag(Runtime)))
# 
# zero[Diff > 11]
# 
# 
# ggplot(zero, aes(date, Diff))+
#   geom_point()


df[, Zero_id := rleid(Zero)]

Zero.all <- df %>% 
  filter(Zero == 1)


Zero <- df %>% 
  filter(Zero == 1) %>% 
  group_by(Zero_id) %>%
  slice(50:n())



Fig <- 
ggplot(Zero.all, aes(date, pCO2_corr))+
  geom_point(alpha=0.2)+
  ylim(0,40)

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("HydroC_pCO2_zeroing_BloomSail_alpha.tiff", width = 500, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <- 
ggplot()+
  geom_point(data=Zero.all, aes(date, pCO2_corr, col="All"))+
  geom_point(data=Zero, aes(date, pCO2_corr, col="-50 first rows"))+
  ylim(0,40)+
  scale_color_brewer(palette = "Set1", name="Zeroing subset")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("HydroC_pCO2_zeroing_BloomSail_removed.tiff", width = 500, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <- 
ggplot()+
  geom_point(data=Zero.all, aes(date, pCO2_corr, col="All"))+
  geom_point(data=Zero, aes(date, pCO2_corr, col="-50 first rows"))+
  ylim(0,40)+
  scale_color_brewer(palette = "Set1", name="Zeroing subset")+
  facet_wrap(~Zero_id, scales = "free_x")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("HydroC_pCO2_zeroing_BloomSail_individual_zeroing.tiff", width = 800, height = 800, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


