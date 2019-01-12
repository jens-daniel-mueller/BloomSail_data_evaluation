library(data.table)
library(ggplot2)
library(rgdal)
library(plotKML)
library(birk)
library(lubridate)
library(zoo)
library(viridis)
library(RColorBrewer)
library(seacarb)


rm(list = ls())

### read GPX track from GPS Logger file


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/TRack/GPS_Logger_Track")
files <- list.files(pattern = "[.]txt$")


for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    
    tempo<-data.table ( read.delim(file, sep=",")[,c(2,3,4)])
    names(tempo) <- c("date", "lat", "lon")
    tempo$date<- ymd_hms(tempo$date, tz="UTC")
    
    dataset<-rbind(dataset, tempo)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset<-data.table ( read.delim(file, sep=",")[,c(2,3,4)])
    names(dataset) <- c("date", "lat", "lon")
    dataset$date<- ymd_hms(dataset$date, tz="UTC")
    
  }
}


track <- dataset
rm(dataset, file, files)

# ggplot(track, aes(lon, lat, col=date))+
#   geom_path()


#################################
### read data from sensor package

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/180616")
files <- list.files(pattern = "[.]cnv$")

for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    tempo <- read.delim(file, sep="", skip = 160, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,4,5,7,9,11,13)])
    names(tempo) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    tempo$start.date <- start.date
    tempo$date <- tempo$date + tempo$start.date
    
    tempo$type <- substr(file, 8,8)
    tempo$label <- substr(file, 8,10)
    # tempo$start.date <- NULL
    # tempo$date.mean <- mean(tempo$date)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep.S == max(tempo$Dep.S)]$date)]$cast <- "down"
    
    dataset<-rbind(dataset, tempo)
    
    rm(start.date)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    dataset <- read.delim(file, sep="", skip = 160, header = FALSE)
    dataset <- data.table(dataset[,c(2,3,4,5,7,9,11,13)])
    names(dataset) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    dataset$start.date <- start.date
    dataset$date <- dataset$date + dataset$start.date
    dataset$type <- substr(file, 8,8)
    dataset$label <- substr(file, 8,10)
    # dataset$start.date <- NULL
    # dataset$date.mean <- mean(dataset$date)
    dataset$cast <- "up"
    dataset[date < mean(dataset[Dep.S == max(dataset$Dep.S)]$date)]$cast <- "down"
    
    rm(start.date)
  }
}



Sensor <- dataset
Sensor$transect.ID <- "180616"

rm(dataset, file, files)



####

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/180705")
files <- list.files(pattern = "[.]cnv$")
#file <- files[1]

for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    tempo <- read.delim(file, sep="", skip = 160, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,4,5,7,9,11,13)])
    names(tempo) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    tempo$start.date <- start.date
    tempo$date <- tempo$date + tempo$start.date
    
    tempo$type <- substr(file, 8,8)
    tempo$label <- substr(file, 8,10)
    # tempo$start.date <- NULL
    # tempo$date.mean <- mean(tempo$date)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep.S == max(tempo$Dep.S)]$date)]$cast <- "down"
    
    dataset<-rbind(dataset, tempo)
    
    rm(start.date)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    dataset <- read.delim(file, sep="", skip = 160, header = FALSE)
    dataset <- data.table(dataset[,c(2,3,4,5,7,9,11,13)])
    names(dataset) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    dataset$start.date <- start.date
    dataset$date <- dataset$date + dataset$start.date
    dataset$type <- substr(file, 8,8)
    dataset$label <- substr(file, 8,10)
    # dataset$start.date <- NULL
    # dataset$date.mean <- mean(dataset$date)
    dataset$cast <- "up"
    dataset[date < mean(dataset[Dep.S == max(dataset$Dep.S)]$date)]$cast <- "down"
    
    rm(start.date)
  }
}


dataset$transect.ID <- "180705"
Sensor <- rbind(Sensor, dataset)

rm(dataset, file, files)


#####



setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/180709")
files <- list.files(pattern = "[.]cnv$")
#file <- files[1]

for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    tempo <- read.delim(file, sep="", skip = 160, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,4,5,7,9,11,13)])
    names(tempo) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    tempo$start.date <- start.date
    tempo$date <- tempo$date + tempo$start.date
    
    tempo$type <- substr(file, 8,8)
    tempo$label <- substr(file, 8,10)
    # tempo$start.date <- NULL
    # tempo$date.mean <- mean(tempo$date)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep.S == max(tempo$Dep.S)]$date)]$cast <- "down"
    
    dataset<-rbind(dataset, tempo)
    
    rm(start.date)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    dataset <- read.delim(file, sep="", skip = 160, header = FALSE)
    dataset <- data.table(dataset[,c(2,3,4,5,7,9,11,13)])
    names(dataset) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    dataset$start.date <- start.date
    dataset$date <- dataset$date + dataset$start.date
    dataset$type <- substr(file, 8,8)
    dataset$label <- substr(file, 8,10)
    # dataset$start.date <- NULL
    # dataset$date.mean <- mean(dataset$date)
    dataset$cast <- "up"
    dataset[date < mean(dataset[Dep.S == max(dataset$Dep.S)]$date)]$cast <- "down"
    
    rm(start.date)
  }
}


dataset$transect.ID <- "180709"
Sensor <- rbind(Sensor, dataset)


rm(dataset, file, files)

####



setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/180718")
files <- list.files(pattern = "[.]cnv$")
#file <- files[1]

for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    tempo <- read.delim(file, sep="", skip = 160, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,4,5,7,9,11,13)])
    names(tempo) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    tempo$start.date <- start.date
    tempo$date <- tempo$date + tempo$start.date
    
    tempo$type <- substr(file, 8,8)
    tempo$label <- substr(file, 8,10)
    # tempo$start.date <- NULL
    # tempo$date.mean <- mean(tempo$date)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep.S == max(tempo$Dep.S)]$date)]$cast <- "down"
    
    dataset<-rbind(dataset, tempo)
    
    rm(start.date)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    dataset <- read.delim(file, sep="", skip = 160, header = FALSE)
    dataset <- data.table(dataset[,c(2,3,4,5,7,9,11,13)])
    names(dataset) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    dataset$start.date <- start.date
    dataset$date <- dataset$date + dataset$start.date
    dataset$type <- substr(file, 8,8)
    dataset$label <- substr(file, 8,10)
    # dataset$start.date <- NULL
    # dataset$date.mean <- mean(dataset$date)
    dataset$cast <- "up"
    dataset[date < mean(dataset[Dep.S == max(dataset$Dep.S)]$date)]$cast <- "down"
    
    rm(start.date)
  }
}


dataset$transect.ID <- "180718"
Sensor <- rbind(Sensor, dataset)


rm(dataset, file, files)






####



setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/180723")
files <- list.files(pattern = "[.]cnv$")
#file <- files[1]

for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    tempo <- read.delim(file, sep="", skip = 160, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,4,5,7,9,11,13)])
    names(tempo) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    tempo$start.date <- start.date
    tempo$date <- tempo$date + tempo$start.date
    
    tempo$type <- substr(file, 8,8)
    tempo$label <- substr(file, 8,10)
    # tempo$start.date <- NULL
    # tempo$date.mean <- mean(tempo$date)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep.S == max(tempo$Dep.S)]$date)]$cast <- "down"
    
    dataset<-rbind(dataset, tempo)
    
    rm(start.date)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    dataset <- read.delim(file, sep="", skip = 160, header = FALSE)
    dataset <- data.table(dataset[,c(2,3,4,5,7,9,11,13)])
    names(dataset) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    dataset$start.date <- start.date
    dataset$date <- dataset$date + dataset$start.date
    dataset$type <- substr(file, 8,8)
    dataset$label <- substr(file, 8,10)
    # dataset$start.date <- NULL
    # dataset$date.mean <- mean(dataset$date)
    dataset$cast <- "up"
    dataset[date < mean(dataset[Dep.S == max(dataset$Dep.S)]$date)]$cast <- "down"
    
    rm(start.date)
  }
}


dataset$transect.ID <- "180723"
Sensor <- rbind(Sensor, dataset)


rm(dataset, file, files)






####



setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/180730")
files <- list.files(pattern = "[.]cnv$")
#file <- files[1]

for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    tempo <- read.delim(file, sep="", skip = 160, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,4,5,7,9,11,13)])
    names(tempo) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    tempo$start.date <- start.date
    tempo$date <- tempo$date + tempo$start.date
    
    tempo$type <- substr(file, 8,8)
    tempo$label <- substr(file, 8,10)
    # tempo$start.date <- NULL
    # tempo$date.mean <- mean(tempo$date)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep.S == max(tempo$Dep.S)]$date)]$cast <- "down"
    
    dataset<-rbind(dataset, tempo)
    
    rm(start.date)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    dataset <- read.delim(file, sep="", skip = 160, header = FALSE)
    dataset <- data.table(dataset[,c(2,3,4,5,7,9,11,13)])
    names(dataset) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    dataset$start.date <- start.date
    dataset$date <- dataset$date + dataset$start.date
    dataset$type <- substr(file, 8,8)
    dataset$label <- substr(file, 8,10)
    # dataset$start.date <- NULL
    # dataset$date.mean <- mean(dataset$date)
    dataset$cast <- "up"
    dataset[date < mean(dataset[Dep.S == max(dataset$Dep.S)]$date)]$cast <- "down"
    
    rm(start.date)
  }
}


dataset$transect.ID <- "180730"
Sensor <- rbind(Sensor, dataset)


rm(dataset, file, files)





####



setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/180802")
files <- list.files(pattern = "[.]cnv$")
#file <- files[1]

for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    tempo <- read.delim(file, sep="", skip = 160, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,4,5,7,9,11,13)])
    names(tempo) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    tempo$start.date <- start.date
    tempo$date <- tempo$date + tempo$start.date
    
    tempo$type <- substr(file, 8,8)
    tempo$label <- substr(file, 8,10)
    # tempo$start.date <- NULL
    # tempo$date.mean <- mean(tempo$date)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep.S == max(tempo$Dep.S)]$date)]$cast <- "down"
    
    dataset<-rbind(dataset, tempo)
    
    rm(start.date)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    dataset <- read.delim(file, sep="", skip = 160, header = FALSE)
    dataset <- data.table(dataset[,c(2,3,4,5,7,9,11,13)])
    names(dataset) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    dataset$start.date <- start.date
    dataset$date <- dataset$date + dataset$start.date
    dataset$type <- substr(file, 8,8)
    dataset$label <- substr(file, 8,10)
    # dataset$start.date <- NULL
    # dataset$date.mean <- mean(dataset$date)
    dataset$cast <- "up"
    dataset[date < mean(dataset[Dep.S == max(dataset$Dep.S)]$date)]$cast <- "down"
    
    rm(start.date)
  }
}


dataset$transect.ID <- "180802"
Sensor <- rbind(Sensor, dataset)


rm(dataset, file, files)




####



setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/180806")
files <- list.files(pattern = "[.]cnv$")
#file <- files[1]

for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    tempo <- read.delim(file, sep="", skip = 160, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,4,5,7,9,11,13)])
    names(tempo) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    tempo$start.date <- start.date
    tempo$date <- tempo$date + tempo$start.date
    
    tempo$type <- substr(file, 8,8)
    tempo$label <- substr(file, 8,10)
    # tempo$start.date <- NULL
    # tempo$date.mean <- mean(tempo$date)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep.S == max(tempo$Dep.S)]$date)]$cast <- "down"
    
    dataset<-rbind(dataset, tempo)
    
    rm(start.date)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    dataset <- read.delim(file, sep="", skip = 160, header = FALSE)
    dataset <- data.table(dataset[,c(2,3,4,5,7,9,11,13)])
    names(dataset) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    dataset$start.date <- start.date
    dataset$date <- dataset$date + dataset$start.date
    dataset$type <- substr(file, 8,8)
    dataset$label <- substr(file, 8,10)
    # dataset$start.date <- NULL
    # dataset$date.mean <- mean(dataset$date)
    dataset$cast <- "up"
    dataset[date < mean(dataset[Dep.S == max(dataset$Dep.S)]$date)]$cast <- "down"
    
    rm(start.date)
  }
}


dataset$transect.ID <- "180806"
Sensor <- rbind(Sensor, dataset)


rm(dataset, file, files)




####

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/180815")
files <- list.files(pattern = "[.]cnv$")
#file <- files[1]

for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    tempo <- read.delim(file, sep="", skip = 160, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,4,5,7,9,11,13)])
    names(tempo) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    tempo$start.date <- start.date
    tempo$date <- tempo$date + tempo$start.date
    
    tempo$type <- substr(file, 8,8)
    tempo$label <- substr(file, 8,10)
    # tempo$start.date <- NULL
    # tempo$date.mean <- mean(tempo$date)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep.S == max(tempo$Dep.S)]$date)]$cast <- "down"
    
    dataset<-rbind(dataset, tempo)
    
    rm(start.date)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    dataset <- read.delim(file, sep="", skip = 160, header = FALSE)
    dataset <- data.table(dataset[,c(2,3,4,5,7,9,11,13)])
    names(dataset) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    dataset$start.date <- start.date
    dataset$date <- dataset$date + dataset$start.date
    dataset$type <- substr(file, 8,8)
    dataset$label <- substr(file, 8,10)
    # dataset$start.date <- NULL
    # dataset$date.mean <- mean(dataset$date)
    dataset$cast <- "up"
    dataset[date < mean(dataset[Dep.S == max(dataset$Dep.S)]$date)]$cast <- "down"
    
    rm(start.date)
  }
}


dataset$transect.ID <- "180815"
Sensor <- rbind(Sensor, dataset)


rm(dataset, file, files)




####calcualte CT* based on measured pCO2, S, and T, as well as the Alkalinity
#### * in CT indicates that absolute values are somewhat vague, but changes in CT are represented well
#### underlying pCO2 data still require Response time (RT) correction



Sensor[transect.ID == "180616"]$pCO2 <- NA

Sensor$CT.calc <- 0
Sensor$CT.calc <- NA

Sensor[transect.ID != "180616"]$CT.calc <- 
  carb(24, var1=Sensor[transect.ID != "180616"]$pCO2, var2=1670e-6,
       S=Sensor[transect.ID != "180616"]$Sal.S, T=Sensor[transect.ID != "180616"]$Tem.S,
       Patm=1, P=0, Pt=0, Sit=0,
       k1k2="m10", kf="x", ks="d", pHscale="T", b="u74", gas="insitu", 
       warn="y", eos="eos80")[16]*1e6


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







####

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/HydroFIA_pH")

HF <- data.table(read.delim("180812_data.txt", sep=",", skip = 2))[,c(1,3,6,7,8,9)]
names(HF) <- c("date", "sample", "Sal", "pH.Mosley.25", "pH.fitpoints", "pH.error")

HF$date <- ymd_hms(HF$date)
HF <- HF[date > ymd("18-07-01")]

HF$transect.ID <- substr(HF$sample, 1,6)
HF$type <- substr(HF$sample, 8,8)
HF$label <- substr(HF$sample, 8,10)
HF$Dep.HF <- as.numeric(substr(HF$sample, 12,13))

HF <- HF[label %in% c("P07", "P10")]
HF <- HF[, .SD[seq(.N-2,.N)], by=.(transect.ID, label, Dep.HF)]



Fig <-
  ggplot()+
  geom_path(data = HF, 
            aes(pH.Mosley.25, Dep.HF, 
                col=transect.ID))+
  scale_y_reverse()+
  facet_wrap(~label)+
  #xlim(0,27)+
  #scale_color_viridis(discrete = TRUE, direction = -1)+
  scale_color_brewer(palette = "Spectral")+
  labs(x="pH [spec, Mosley, 25C]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180819_Profiles_pH_HydroFIA.tiff", width = 250, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)








