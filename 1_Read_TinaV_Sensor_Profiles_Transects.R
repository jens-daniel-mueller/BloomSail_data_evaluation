library(tidyverse)
library(data.table)
library(lubridate)
library(here)

#### read data from sensor package ####

setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/TinaV/Sensor/Profiles_Transects")
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
  tempo$type <- substr(file, 8,8)
  tempo$label <- substr(file, 8,10)
  
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


#### plots to check succesful read-in and data-quality ####

# Profiling data

df %>% 
  filter(type == "P") %>% 
  ggplot(aes(Tem.S, Dep.S, col=label, linetype = cast))+
  geom_line()+
  scale_y_reverse()+
  geom_vline(xintercept = c(10, 20))+
  facet_wrap(~transect.ID)

df[transect.ID == "180723" & label == "P07" & Dep.S < 2 & cast == "up"]$Tem.S <- NA


df %>% 
  filter(type == "P") %>% 
  ggplot(aes(Sal.S, Dep.S, col=label, linetype = cast))+
  geom_path()+
  scale_y_reverse()+
  facet_wrap(~transect.ID)

df[Sal.S < 6]$Sal.S <- NA


df %>% 
  filter(type == "P") %>% 
  ggplot(aes(pH, Dep.S, col=label, linetype=cast))+
  geom_path()+
  scale_y_reverse()+
  facet_wrap(~transect.ID)

df[pH < 7.5]$pH <- NA

df[transect.ID == "180709" & label == "P03" & Dep.S < 5 & cast == "down"]$pH <- NA
df[transect.ID == "180709" & label == "P05" & Dep.S < 10 & cast == "down"]$pH <- NA
df[transect.ID == "180718" & label == "P10" & Dep.S < 3 & cast == "down"]$pH <- NA
df[transect.ID == "180815" & label == "P03" & Dep.S < 2 & cast == "down"]$pH <- NA
df[transect.ID == "180820" & label == "P11" & Dep.S < 15 & cast == "down"]$pH <- NA


df %>% 
  filter(type == "P") %>% 
  ggplot(aes(pCO2, Dep.S, col=label, linetype = cast))+
  geom_path()+
  scale_y_reverse()+
  facet_wrap(~transect.ID)

df[transect.ID == "180616"]$pCO2 <- NA


df %>% 
  filter(type == "P") %>% 
  ggplot(aes(O2, Dep.S, col=label, linetype = cast))+
  geom_path()+
  scale_y_reverse()+
  facet_wrap(~transect.ID)

df %>% 
  filter(type == "P") %>% 
  ggplot(aes(Chl, Dep.S, col=label, linetype = cast))+
  geom_path()+
  scale_y_reverse()+
  facet_wrap(~transect.ID)

df[Chl > 100]$Chl <- NA


# Surface transect data

df %>% 
  filter(type == "T") %>% 
  ggplot(aes(date, Dep.S, col=label))+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(~transect.ID, scales = "free_x")

df %>% 
  filter(type == "T") %>% 
  ggplot(aes(date, Tem.S, col=label))+
  geom_point()+
  facet_wrap(~transect.ID, scales = "free_x")

df %>% 
  filter(type == "T") %>% 
  ggplot(aes(date, Sal.S, col=label))+
  geom_point()+
  facet_wrap(~transect.ID, scales = "free_x")

df %>% 
  filter(type == "T") %>% 
  ggplot(aes(date, pCO2, col=label))+
  geom_point()+
  facet_wrap(~transect.ID, scales = "free_x")

df %>% 
  filter(type == "T") %>% 
  ggplot(aes(date, pH, col=label))+
  geom_point()+
  facet_wrap(~transect.ID, scales = "free_x")

df %>% 
  filter(type == "T") %>% 
  ggplot(aes(date, Chl, col=label))+
  geom_point()+
  facet_wrap(~transect.ID, scales = "free_x")

df[type == "T" & Chl > 10]$Chl <- NA


df %>% 
  filter(type == "T") %>% 
  ggplot(aes(date, O2, col=label))+
  geom_point()+
  facet_wrap(~transect.ID, scales = "free_x")



write_csv(df, here("Data/_summarized_data_files", "Tina_V_Sensor_Profiles_Transects.csv"))




