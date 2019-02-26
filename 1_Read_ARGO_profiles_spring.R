library(ggplot2)
library(data.table)
library(viridis)
library(lubridate)
library(ggmap)
library(tidyverse)


setwd("C:/Mueller_Jens_Data/180501_N-Fix_Study_TinaV/data/ARGO/data")
files <- list.files(pattern = "[.]csv$")
#file <- files[19]


for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
 
    
    tempo <-read.delim(file, sep=",", header = TRUE)
    a<-dim(tempo)
    tempo <- data.table(tempo[,c(3,4,5,6,7,8,12,14,a[2]-1)])
    names(tempo) <- c("mean_date", "lat", "lon" ,"pres" , "Tem", "Sal", "DOX2", "CPHL", "Cast")
    tempo <- tempo[Cast == "Secondary sampling: discrete [low resolution profile]"]
    tempo$mean_date <- as.character(tempo$mean_date)
    tempo$mean_date <- ymd_hms(tempo$mean_date)
    length_cast[file]<- length(tempo$mean_date)

    
    
    
    dataset<-rbind(dataset, tempo)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.delim(file, sep=",", header = TRUE)
    a<-dim(dataset)
    dataset <- data.table(dataset[,c(3,4,5,6,7,8,12,14,a[2]-1)])
    names(dataset) <- c("mean_date", "lat", "lon" ,"pres" , "Tem", "Sal", "DOX2", "CPHL", "Cast")
    dataset <- dataset[Cast == "Secondary sampling: discrete [low resolution profile]"]
    dataset$mean_date <- as.character(dataset$mean_date)
    dataset$mean_date <- ymd_hms(dataset$mean_date)
    length_cast<- length(dataset$mean_date)


}
}

for (file in files){
  if (exists("cast")){
    tempo <- read.delim(file, sep=",", header = TRUE,nrows=1)
    a<-dim(tempo)
    tempo <- data.table(tempo[,c(3,4,5,a[2]-1)])
    names(tempo) <- c("date", "lat", "lon", "type")
    tempo <- tempo[type ==  "Secondary sampling: discrete [low resolution profile]"]
    tempo$date <- as.character(tempo$date)
    tempo$date <- ymd_hms(tempo$date)
    
    cast<-rbind(cast, tempo)
    rm(tempo)
    
  }
  
  if (!exists("cast")){
    
    cast <- read.delim(file, sep=",", header = TRUE,nrows=1)
    a<-dim(cast)
    cast <- data.table(cast[,c(3,4,5,a[2]-1)])
    names(cast) <- c("date", "lat", "lon", "type")
    cast <- cast[type ==  "Secondary sampling: discrete [low resolution profile]"]
    cast$date <- as.character(cast$date)
    cast$date <- ymd_hms(cast$date)
    
  }
}

rm(a) 
rm(length_cast)
rm(file)
rm(files)
  
df <- dataset
rm(dataset)


df <- df[mean_date > ymd("18/07/01")]

setwd("C:/Mueller_Jens_Data/180501_N-Fix_Study_TinaV/data/ARGO/plots")

# overview plot of casts
Fig <-
ggplot()+
    geom_point(data=cast,aes(lon,lat, col=factor(as.Date(cast$date))))+
    geom_path(data=cast,aes(lon,lat))+
    scale_y_continuous(name="Latitude",limits = c(56.75,58))+
    scale_x_continuous(name="Longitude",limits = c(17.5,20))+
    scale_color_viridis(discrete = "TRUE", name="Time", direction = -1)+
    borders("world",fill="black",color="black")

tiff("./180814_ARGO_track.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)


#Plots of Tem

Fig <-
ggplot()+
  geom_path(data = df, aes(Tem, pres, col=factor(as.Date(df$mean_date))))+
  scale_y_reverse(name="Depth (m)", limits=c(150,0), breaks = seq(0,300,10))+
  scale_x_continuous(name="Temperature (?C)", breaks = seq(0,30,2))+
  scale_color_viridis(discrete = "TRUE", name="Time", direction = -1)

tiff("./180814_Temp_ARGO.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
ggplot()+
  geom_path(data = df, aes(Tem, pres, col=factor(as.Date(df$mean_date))))+
  scale_y_reverse(name="Depth (m)", limits=c(35,0), breaks = seq(0,300,10))+
  scale_x_continuous(name="Temperature (?C)", breaks = seq(0,30,2))+
  scale_color_viridis(discrete = "TRUE", name="Time", direction = -1)

tiff("./180814_Temp_ARGO_upper30.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

# Plots of Sal

Fig <-
  ggplot()+
  geom_path(data = df, aes(Sal, pres, col=factor(as.Date(df$mean_date))))+
  scale_y_reverse(name="Depth (m)", limits=c(150,0), breaks = seq(0,300,10))+
  scale_x_continuous(name="Salinity (PSU)",breaks = seq(0,20,1))+
  scale_color_viridis(discrete = "TRUE", name="Time", direction = -1)

tiff("./180814_Sal_ARGO.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
  ggplot()+
  geom_path(data = df, aes(Sal, pres, col=factor(as.Date(df$mean_date))))+
  scale_y_reverse(name="Depth (m)", limits=c(40,0), breaks = seq(0,300,10))+
  scale_x_continuous(name="Salinity (PSU)", limits=c(6.5,7.5), breaks = seq(0,20,.5))+
  scale_color_viridis(discrete = "TRUE", name="Time", direction = -1)

tiff("./180814_Sal_ARGO_upper30.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

# Plots of DOX2

Fig <-
ggplot()+
  geom_path(data = df, aes(DOX2, pres, col=factor(as.Date(df$mean_date))))+
  scale_y_reverse(name="Depth (m)", limits=c(150,0), breaks = seq(0,300,10))+
  scale_x_continuous(name="Desolved Oxygen", breaks = seq(0,500,50))+
  scale_color_viridis(discrete = "True", name="Time", direction = -1)

tiff("./180814_DOX2_ARGO.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
ggplot()+
  geom_path(data = df, aes(DOX2, pres, col=factor(as.Date(df$mean_date))))+
  scale_y_reverse(name="Depth (m)", limits=c(35,0), breaks = seq(0,300,10))+
  scale_x_continuous(name="Desolved Oxygen", limits=c(200,500), breaks = seq(0,500,50))+
  scale_color_viridis(discrete = "TRUE", name="Time", direction = -1)

tiff("./180814_DOX2_ARGO_upper30.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

# Plots of CPHL

Fig <-
ggplot()+
  geom_path(data = df, aes(CPHL, pres, col=factor(as.Date(df$mean_date))))+
  scale_y_reverse(name="Depth (m)", limits=c(150,0), breaks = seq(0,300,10))+
  scale_x_continuous(name="Chlorophyll", breaks = seq(0,20,2))+
  scale_color_viridis(discrete = "TRUE", name="Time", direction = -1)

tiff("./180814_CPHL_ARGO.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
  ggplot()+
  geom_path(data = df, aes(CPHL, pres, col=factor(as.Date(df$mean_date))))+
  scale_y_reverse(name="Depth (m)", limits=c(35,0), breaks = seq(0,300,10))+
  scale_x_continuous(name="Chlorophyll", breaks = seq(0,20,2))+
  scale_color_viridis(discrete = "TRUE", name="Time", direction = -1)

tiff("./180814_CPHL_ARGO_upper30.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)
