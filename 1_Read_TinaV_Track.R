library(tidyverse)
library(data.table)
library(lubridate)


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


#### plots to check succesful read-in and data-quality ####

track %>% 
  ggplot(aes(lon, lat, col=date))+
  geom_path()


setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/_summarized_data_files")
write.csv(track, "TinaV_Track.csv", row.names = FALSE)
