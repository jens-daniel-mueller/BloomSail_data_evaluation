library(tidyverse)
library(data.table)
library(readxl)
library(gsubfn)
library(lubridate)
library(here)

library(seacarb)



### June - August 2018

setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/Finnmaid/pCO2-final-2018/June-August")
files <- list.files(pattern = "[.]xls$")
#file <-files[1]

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


rm(df, files, file)
temp <- temp[pCO2 != 0]


#### Los Gatos data

setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/Finnmaid/pCO2-final-2018/June-August/LGR")
files <- list.files(pattern = "[.]xls$")
#file <-files[1]

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(2,3,4,8,6,5,14,7,15,9)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- dmy_hms(df$date)
  df <- data.table(df)
  
  df$route <- substr(as.character(file), 12, 12)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp.LGR")){
    temp.LGR <- rbind (temp.LGR, df)
  } else{temp.LGR <- df}
  
}


#Convert O2 sat to O2 concentration #

source(here::here("O2stoO2c.R"))

temp.LGR <- temp.LGR %>%
  filter() %>% 
  mutate(cO2 = O2stoO2c(O2sat = cO2, T=Tem, S=Sal, P=3/10, p_atm = 1013.5))



#### Merge Los Gator and LICOR data files ####

temp$sensor <- "LICOR"
temp.LGR$sensor <- "LosGatos"

temp <- rbind(temp, temp.LGR)

rm(temp.LGR, df, file, files)


#### Assign subareas according to Schneider and Mueller (2018) ####

temp$Area <- with(temp,
                  ifelse(Lon>12 & Lon<12.6, "1.MEB",
                  ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
                  ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
                  ifelse(Lat>57.33 & Lat<57.5 & route %in% c("E"), "BS",
                  ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
                  ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
                  ifelse(Lon>22 & Lon<24, "6.WGF",
                  ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN")))))))))




ggplot(temp[Area == "BS"], aes(Lat, Tem, col=ID, shape=sensor))+
  geom_point()+
  scale_color_viridis_d()


ggplot(temp[Area == "BS"], aes(date, Tem, shape=sensor))+
  geom_point()+
  scale_color_viridis_d()

ggplot(temp[Area == "BS"], aes(date, Sal, shape=sensor))+
  geom_point()

ggplot(temp[Area == "BS"], aes(date, cO2, shape=sensor))+
  geom_point()


ggplot(temp, aes(Lat, Sal, col=ID, shape=sensor))+
  geom_point()+
  scale_color_viridis_d()



temp <-temp[complete.cases(temp[,pCO2]),]

temp.mean <- temp[,.(
  date = mean(date),
  mean.Sal = mean(Sal, na.rm = TRUE),
  SD.Sal = sd(Sal, na.rm = TRUE),
  mean.Tem = mean(Tem, na.rm = TRUE),
  SD.Tem = sd(Tem, na.rm = TRUE),
  mean.pCO2 = mean(pCO2, na.rm = TRUE),
  SD.pCO2 = sd(pCO2, na.rm = TRUE),
  max.pCO2 = max(pCO2),
  min.pCO2 = min(pCO2),
  mean.cO2 = mean(cO2, na.rm = TRUE),
  SD.cO2 = sd(cO2, na.rm = TRUE),
  mean.patm = mean(patm, na.rm = TRUE),
  nr=.N),
  by=.(Area, ID)] [Area != "NaN"]


setorder(temp.mean, date)

ggplot()+
  geom_point(data = temp[Area == "BS"], aes(date, pCO2, col=ID, shape=sensor))+
  geom_path(data = temp.mean[Area == "BS"], aes(date, mean.pCO2))+
  scale_color_viridis(discrete = TRUE)






