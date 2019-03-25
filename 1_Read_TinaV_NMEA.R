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
