library(data.table)
library(ggplot2)
library(rgdal)
library(plotKML)
library(birk)
library(lubridate)
library(zoo)
library(viridis)
library(RColorBrewer)

#### merge track and sensor data


df <- merge(track, Sensor, all=TRUE)
df$lon<-na.approx(df$lon, na.rm = F)
df$lat<-na.approx(df$lat, na.rm = F)

df <-
df %>% 
  filter(!is.na(Tem.S))

#df <- na.omit(df)

rm(Sensor, track)

ggplot(df, aes(date, Tem.S))+
  geom_path()


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Merged_data_files")
write.csv(df, "BloomSail_Sensor_Track_data.csv", row.names = FALSE)




setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/NMEA")

NMEA<-data.table(read.csv("NMEA_AIS-dec_180705_1911.csv", sep = ";")[,seq(1,7,1)])
names(NMEA) <- c("date", "MMSI", "name", "lon", "lat", "SOG", "COG")

NMEA$date<- dmy_hms(NMEA$date, tz="UTC")

NMEA <- NMEA[MMSI != ""]
NMEA <- na.omit(NMEA)


NMEA$SOG <- as.numeric(as.character(NMEA$SOG))
NMEA$COG <- as.numeric(as.character(NMEA$COG))

NMEA$lon <- as.numeric(as.character(NMEA$lon))
NMEA$lat <- as.numeric(as.character(NMEA$lat))

NMEA$direction <- "North"
NMEA[COG <  270 & COG >90]$direction <- "South"

#unique(NMEA$name)
setorder(NMEA, name)


plot.NMEA <- NMEA[lon>18.8 & lon<19.6 & lat>57.25 & lat < 57.5]



rm(NMEA, Sensor, track)



df <- df[type %in% c("P", "T")]



Fig <-
  ggplot()+
  geom_path(data=plot.NMEA, aes(lon, lat, group=as.factor(MMSI)), col="grey")+
  geom_path(data = df, aes(lon, lat, group = transect.ID))+
  geom_point(data = df[type == "P"], aes(lon, lat, col=label))+
  #scale_color_brewer(palette = "Spectral")+
  labs(x="Lon [?E]", y="Lat [?N]")+
  coord_quickmap()


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Profiles_Map.tiff", width = 180, height = 150, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
  ggplot()+
  geom_path(data=plot.NMEA, aes(lon, lat, group=as.factor(MMSI)), col="grey")+
  geom_path(data = df[type=="T" & Dep.S < 4], aes(lon, lat, col=Sal.S), size=1.5)+
  scale_color_viridis(limits = c(6.5,7.5))+
  labs(x="Lon [?E]", y="Lat [?N]")+
  coord_quickmap()+
  facet_wrap(~transect.ID, ncol=2)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Surface_Map_Salinity.tiff", width = 180, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
  ggplot()+
  geom_path(data=plot.NMEA, aes(lon, lat, group=as.factor(MMSI)), col="grey")+
  geom_path(data = df[type=="T" & Dep.S < 4], aes(lon, lat, col=pCO2), size=1.5)+
  scale_color_viridis(limits = c(70, 170))+
  labs(x="Lon [?E]", y="Lat [?N]")+
  coord_quickmap()+
  facet_wrap(~transect.ID, ncol=2)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Surface_Map_pCO2.tiff", width = 180, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
  ggplot()+
  geom_path(data=plot.NMEA, aes(lon, lat, group=as.factor(MMSI)), col="grey")+
  geom_path(data = df[type=="T" & Dep.S < 4], aes(lon, lat, col=Tem.S), size=1.5)+
  scale_color_viridis()+#limits = c(70, 120))+
  labs(x="Lon [?E]", y="Lat [?N]")+
  coord_quickmap()+
  facet_wrap(~transect.ID, ncol = 2)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Surface_Map_Tem.tiff", width = 180, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
  ggplot()+
  geom_path(data=plot.NMEA, aes(lon, lat, group=as.factor(MMSI)), col="grey")+
  geom_path(data = df[type=="T"], aes(lon, lat, col=pH), size=1.5)+
  scale_color_viridis()+
  labs(x="Lon [?E]", y="Lat [?N]")+
  coord_quickmap()+
  facet_wrap(~transect.ID, ncol=2)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Surface_Map_pH.tiff", width = 180, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
  ggplot()+
  geom_path(data=plot.NMEA, aes(lon, lat, group=as.factor(MMSI)), col="grey")+
  geom_path(data = df[type=="T"], aes(lon, lat, col=O2), size=1.5)+
  scale_color_viridis()+
  labs(x="Lon [?E]", y="Lat [?N]")+
  coord_quickmap()+
  facet_wrap(~transect.ID, ncol=2)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Surface_Map_O2.tiff", width = 180, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
  ggplot()+
  geom_path(data=plot.NMEA, aes(lon, lat, group=as.factor(MMSI)), col="grey")+
  geom_path(data = df[type=="T"], aes(lon, lat, col=CT.calc), size=1.5)+
  scale_color_viridis(limits = c(1420, 1560))+
  labs(x="Lon [?E]", y="Lat [?N]")+
  coord_quickmap()+
  facet_wrap(~transect.ID, ncol=2)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Surface_Map_CT.tiff", width = 180, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


write.csv(df, "180818_BloomSail_transects_profiles.csv", row.names = FALSE)








