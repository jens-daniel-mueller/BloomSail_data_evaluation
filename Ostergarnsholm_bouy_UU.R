library(ggplot2)
library(tidyverse)
library(data.table)
library(lubridate)
library(paletteer)
library(zoo)

#### read data from sensor package ####

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/OGB")
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


#Sensor$transect.ID <- "180616"
df <- dataset
rm(dataset, file, files)



surface.ts <-
  df %>% 
  filter(label=="bo", cast=="down", Dep.S >2, Dep.S <6) %>%
  group_by(start.date) %>% 
  summarise(date = mean(date),
            pCO2.max = max(pCO2),
            pCO2.min = min(pCO2),
            pCO2.mean = mean(pCO2),
            Tem.S.max = max(Tem.S),
            Tem.S.min = min(Tem.S),
            Tem.S.mean = mean(Tem.S),
            Sal.S.max = max(Sal.S),
            Sal.S.min = min(Sal.S),
            Sal.S.mean = mean(Sal.S)) %>% 
  ungroup()


### read GPX track from GPS Logger file ####

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


#### Merge track and sensor data ###

all <- merge(track, df, all=TRUE)
all$lon<-na.approx(all$lon, na.rm = F)
all$lat<-na.approx(all$lat, na.rm = F)

all <- na.omit(all)


#### plot data from sensor package ####


setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")

#### plot profiles ####


df %>% 
  filter(label=="bo", cast=="down") %>%
ggplot(aes(pCO2, Dep.S, col=as.factor(start.date)))+
  geom_path()+
  scale_color_paletteer_d("ggthemes", tableau_cyclic, name="Date")+
  scale_y_reverse()+
  labs(x="pCO2 [µatm]", y="Depth [m]", title = "Raw pCO2 profiles at Ostergarnsholm bouy")+
  theme_bw()

ggsave("OGB_pCO2_profiles.jpg")  

df %>% 
  filter(label=="bo", cast=="down") %>%
ggplot(aes(Tem.S, Dep.S, col=as.factor(start.date)))+
  geom_path()+
  scale_color_paletteer_d("ggthemes", tableau_cyclic, name="Date")+
  scale_y_reverse()+
  labs(x="Temp [°C]", y="Depth [m]", title = "Temperature profiles at Ostergarnsholm bouy")+
  theme_bw()

ggsave("OGB_Tem_profiles.jpg")  


df %>% 
  filter(label=="bo", cast=="down", Dep.S > 2) %>%
ggplot(aes(Sal.S, Dep.S, col=as.factor(start.date)))+
  geom_path()+
  scale_color_paletteer_d("ggthemes", tableau_cyclic, name="Date")+
  scale_y_reverse()+
  labs(x="Salinity", y="Depth [m]", title = "Salinity profiles at Ostergarnsholm bouy")+
  theme_bw()

ggsave("OGB_Sal_profiles.jpg")  


#### plot surface timeseries data at bouy ####

surface.ts %>%   
ggplot()+
  geom_errorbar(aes(date, ymax=pCO2.max, ymin=pCO2.min))+
  geom_path(aes(date, pCO2.mean))+
  geom_point(aes(date, pCO2.mean))+
  theme_bw()+
  labs(x="Date", y="pCO2 [µatm]", title = "Min, mean, and max pCO2 at bouy in 2-6 m water depth")

ggsave("OGB_surface_pCO2_timeseries.jpg")  

surface.ts %>%   
ggplot()+
  geom_errorbar(aes(date, ymax=Tem.S.max, ymin=Tem.S.min))+
  geom_path(aes(date, Tem.S.mean))+
  geom_point(aes(date, Tem.S.mean))+
  theme_bw()+
  labs(x="Date", y="Temp [°C]", title = "Min, mean, and max Temperature at bouy in 2-6 m water depth")

ggsave("OGB_surface_Tem_timeseries.jpg")  

surface.ts %>%   
ggplot()+
  geom_errorbar(aes(date, ymax=Sal.S.max, ymin=Sal.S.min))+
  geom_path(aes(date, Sal.S.mean))+
  geom_point(aes(date, Sal.S.mean))+
  theme_bw()+
  labs(x="Date", y="Salinity", title = "Min, mean, and max Salinity at bouy in 2-6 m water depth")

ggsave("OGB_surface_Sal_timeseries.jpg")  


#### plot surface maps around Ostergarnsholm ####

all %>%
  filter(label %in% c("in", "ou")) %>% 
ggplot(aes(lon, lat, col=pCO2))+
  geom_point()+
  scale_color_paletteer_c("oompaBase", "jetColors", name="pCO2 [µatm]")+
  facet_wrap(~transect.ID)+
  theme_bw()
  
ggsave("OGB_surface_pCO2_map.jpg")  


  



