library(ggplot2)
library(data.table)
library(viridis)


mytheme <-
  theme(
    text = element_text(size=8, colour = "black"),
    axis.text = element_text(size=8, colour = "black"),
    legend.text = element_text(size=8, colour = "black"),
    legend.title = element_text(size=8, colour = "black"),
    panel.border = element_rect(colour = "black", size=1, fill=NA),
    panel.background = element_rect(fill="white"),# colour = "black", size=2),
    line = element_line(size = 0.5),
    axis.ticks = element_line(colour = "black", size = 0.5),
    axis.ticks.length = unit(1, "mm"),
    legend.spacing = unit(2, "mm"),
    legend.key.size = unit(5, "mm"),
    legend.key = element_rect(colour=NA, fill=NA, size=0.5),
    legend.background = element_rect(colour = "black"),
    plot.margin=unit(c(3,3,3,3), "mm"),
    panel.grid.major.y = element_line(colour = "grey", size = 0.5),
    panel.grid.major.x = element_line(colour = "grey", size = 0.5)
  )


setwd("C:/Mueller_Jens_Data/180501_N-Fix_Study_TinaV/data/ARGO/data")
files <- list.files(pattern = "[.]TOB$")


for (file in files){
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    tempo <-read.delim(file, sep="", skip = 28, header = FALSE)
    tempo <- data.table(tempo[,c(2,3,5,7,8)])
    names(tempo) <- c("Dep", "Tem", "Sal", "date", "time")
    tempo$date <- paste(tempo$date, tempo$time)
    tempo$time <- NULL
    tempo$date <- as.POSIXct(strptime(tempo$date, format="%m/%d/%Y %H:%M:%S", tz="GMT"))
    tempo$date.mean <- mean(tempo$date)
    tempo$cast <- "up"
    tempo[date < mean(tempo[Dep == max(tempo$Dep)]$date)]$cast <- "down"
    dataset<-rbind(dataset, tempo)
    rm(tempo)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.delim(file, sep="", skip = 28, header = FALSE)
    dataset <- data.table(dataset[,c(2,3,5,7,8)])
    names(dataset) <- c("Dep", "Tem", "Sal", "date", "time")
    dataset$date <- paste(dataset$date, dataset$time)
    dataset$time <- NULL
    dataset$date <- as.POSIXct(strptime(dataset$date, format="%m/%d/%Y %H:%M:%S", tz="GMT"))
    dataset$date.mean <- mean(dataset$date)
    dataset$cast <- "up"
    dataset[date < mean(dataset[Dep == max(dataset$Dep)]$date)]$cast <- "down"
}
}
  
rm(file)
rm(files)
  
df <- dataset
rm(dataset)

write.csv(df, "180609_CTD_BloomSail.csv", row.names = FALSE)

setwd("C:/Mueller_Jens_Data/180501_N-Fix_Study_TinaV/data/CTD/plots")

Fig <-
ggplot()+
  geom_path(data = df[Dep>0.5 & cast=="up"], aes(Tem, Dep, col=as.factor(date.mean)))+
  geom_point(data = df[Dep>0.5 & cast=="up"], aes(Tem, Dep, col=as.factor(date.mean)))+
  scale_y_reverse(name="Depth (m)", limits=c(30,0), breaks = seq(0,30,5))+
  scale_x_continuous(name="Temperature (°C)", breaks = seq(0,30,2))+
  scale_color_brewer(palette = "Set1", name="Time")+
  #scale_color_viridis(name="Time", discrete = TRUE)+
  mytheme

tiff("./180609_Temp_Upcast.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
ggplot()+
  geom_path(data = df[Dep>0.5 & cast=="down"], aes(Tem, Dep, col=as.factor(date.mean)))+
  geom_point(data = df[Dep>0.5 & cast=="down"], aes(Tem, Dep, col=as.factor(date.mean)))+
  scale_y_reverse(name="Depth (m)", limits=c(30,0), breaks = seq(0,30,5))+
  scale_x_continuous(name="Temperature (°C)", breaks = seq(0,30,2))+
  scale_color_brewer(palette = "Set1", name="Time")+
  #scale_color_viridis(name="Time", discrete = TRUE)+
  mytheme

tiff("./180609_Temp_Downcast.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
ggplot()+
  geom_path(data = df[Dep<7 & Dep>0.5 & cast=="up"], aes(Tem, Dep, col=as.factor(date.mean)))+
  geom_point(data = df[Dep<7 & Dep>0.5 & cast=="up"], aes(Tem, Dep, col=as.factor(date.mean)))+
  scale_y_reverse(name="Depth (m)", limits=c(7,0), breaks = seq(0,30,1))+
  scale_x_continuous(name="Temperature (°C)", breaks = seq(0,30,0.05))+
  scale_color_brewer(palette = "Set1", name="Time")+
  #scale_color_viridis(name="Time", discrete = TRUE)+
  mytheme

tiff("./180609_Temp_Upcast_5m.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
ggplot()+
  geom_path(data = df[Dep<7 & Dep>0.5 & cast=="down"], aes(Tem, Dep, col=as.factor(date.mean)))+
  geom_point(data = df[Dep<7 & Dep>0.5 & cast=="down"], aes(Tem, Dep, col=as.factor(date.mean)))+
  scale_y_reverse(name="Depth (m)", limits=c(7,0), breaks = seq(0,30,1))+
  scale_x_continuous(name="Temperature (°C)", breaks = seq(0,30,0.05))+
  scale_color_brewer(palette = "Set1", name="Time")+
  #scale_color_viridis(name="Time", discrete = TRUE)+
  mytheme

tiff("./180609_Temp_Downcast_5m.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
ggplot()+
  geom_path(data = df[Dep>0.5], aes(Tem, Dep))+
  geom_point(data = df[Dep>0.5], aes(Tem, Dep, fill=cast), shape=21)+
  scale_y_reverse(name="Depth (m)", limits=c(30,0), breaks = seq(0,30,5))+
  scale_x_continuous(name="Temperature (°C)", breaks = seq(0,30,2))+
  scale_fill_manual(values = c("black", "white"), name="cast")+
  facet_wrap(~date.mean)+
  mytheme


tiff("./180609_Temp_all.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
ggplot()+
  geom_point(data = df[Dep>0.6 & cast == "up"], aes(Sal, Dep, col=as.factor(date.mean)))+
  geom_path(data = df[Dep>0.6 & cast == "up"], aes(Sal, Dep, col=as.factor(date.mean)))+
  scale_y_reverse(name="Depth (m)", limits=c(30,0), breaks = seq(0,30,5))+
  scale_x_continuous(name="Salinity", breaks = seq(0,30,0.02))+
  scale_color_brewer(palette = "Set1", name="Time")+
  #scale_color_viridis(name="Time", discrete = TRUE)+
  mytheme
  

tiff("./180609_Sal_Upcast.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
ggplot()+
  geom_point(data = df[Dep>0.6 & cast == "down"], aes(Sal, Dep, col=as.factor(date.mean)))+
  geom_path(data = df[Dep>0.6 & cast == "down"], aes(Sal, Dep, col=as.factor(date.mean)))+
  scale_y_reverse(name="Depth (m)", limits=c(30,0), breaks = seq(0,30,5))+
  scale_x_continuous(name="Salinity", breaks = seq(0,30,0.02))+
  scale_color_brewer(palette = "Set1", name="Time")+
  #scale_color_viridis(name="Time", discrete = TRUE)+
  mytheme
  

tiff("./180609_Sal_Downcast.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
ggplot()+
  geom_point(data = df[Dep<7 & Dep>0.7 & cast == "up"], aes(Sal, Dep, col=as.factor(date.mean)))+
  geom_path(data = df[Dep<7 & Dep>0.7 & cast == "up"], aes(Sal, Dep, col=as.factor(date.mean)))+
  scale_y_reverse(name="Depth (m)", limits=c(7,0), breaks = seq(0,30,1))+
  scale_x_continuous(name="Salinity", breaks = seq(0,30,0.01))+
  scale_color_brewer(palette = "Set1", name="Time")+
  #scale_color_viridis(name="Time", discrete = TRUE)+
  mytheme
  

tiff("./180609_Sal_Upcast_5m.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
ggplot()+
  geom_point(data = df[Dep<7 & Dep>0.7 & cast == "down"], aes(Sal, Dep, col=as.factor(date.mean)))+
  geom_path(data = df[Dep<7 & Dep>0.7 & cast == "down"], aes(Sal, Dep, col=as.factor(date.mean)))+
  scale_y_reverse(name="Depth (m)", limits=c(7,0), breaks = seq(0,30,1))+
  scale_x_continuous(name="Salinity", breaks = seq(0,30,0.01))+
  scale_color_brewer(palette = "Set1", name="Time")+
  #scale_color_viridis(name="Time", discrete = TRUE)+
  mytheme
  

tiff("./180609_Sal_Downcast_5m.tiff", width = 150, height = 150, units = 'mm', res = 300, compression = 'lzw')
Fig
dev.off()
rm(Fig)