library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Remote_sensing/SMHI")


df <- read.csv("181129_SMHI_remote_sensing.csv")

df$datetime <- ymd_hms(paste(df$datetime))


Fig <- 
ggplot(df, aes(datetime, CT, col=Station))+
  geom_point()

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("Surface_CT_timeseries_by_station.tiff", width = 500, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <- 
ggplot(df, aes(datetime, chl_nn, col=Station))+
  geom_point()

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("Surface_Chl_nn_timeseries_by_station.tiff", width = 500, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <- 
ggplot(df, aes(CT, chl_nn, col=Station))+
  geom_point()

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("Surface_Chl_nn_vs_CT.tiff", width = 130, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <- 
ggplot(df, aes(CT, chl_oc4me, col=Station))+
  geom_point()

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("Surface_Chl_oc4me_vs_CT.tiff", width = 130, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <- 
ggplot(df, aes(datetime, chl_oc4me, col=Station))+
  geom_point()

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("Surface_Chl_oc4me_timeseries_by_station.tiff", width = 500, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


