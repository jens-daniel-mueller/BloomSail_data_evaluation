#### Load required packages ####

library(tidyverse)
library(data.table)
library(lubridate)
library(here)
library(zoo)



#### read data from sensor package ####

setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/TinaV/Sensor/OGB")
files <- list.files(pattern = "[.]cnv$")
#file <- files[1]

for (file in files){
  
    start.date <- data.table(read.delim(file, sep="#", nrows = 160))[[78,1]]
    start.date <- substr(start.date, 15, 34)
    start.date <- mdy_hms(start.date, tz="UTC")
    
    tempo <- read.delim(file, sep="", skip = 160, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,4,5,7,9,11,13)])
    names(tempo) <- c("date", "Dep.S", "Tem.S", "Sal.S", "pH", "Chl", "O2", "pCO2")
    tempo$start.date <- start.date
    tempo$date <- tempo$date + tempo$start.date

    tempo$transect.ID <- substr(file, 1, 6)
    
    tempo$label <- substr(file, 11,12)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep.S == max(tempo$Dep.S)]$date)]$cast <- "down"
    
    if (exists("dataset")){
      dataset <- rbind(dataset, tempo)
    }
    
        if (!exists("dataset")){
      dataset <- tempo
    }

    rm(start.date)
    rm(tempo)
  }


df <- dataset
rm(dataset, file, files)


### read GPX track from GPS Logger file ####

setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/TinaV/Track/GPS_Logger_Track")
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


#### Merge track and sensor data ###

all <- merge(track, df, all=TRUE)
all$lon<-na.approx(all$lon, na.rm = F)
all$lat<-na.approx(all$lat, na.rm = F)

all <- na.omit(all)


#### safe data files ####

all <-
  all %>%
  select(ID = transect.ID, date_time = date, lat, lon, station=label, cast, 
         dep = Dep.S, tem = Tem.S, sal = Sal.S, pCO2, O2)


write_csv(all, here::here("Data/_merged_data_files",
                          "BloomSail_Ostergarnsholm_Sensor_track.csv"))


