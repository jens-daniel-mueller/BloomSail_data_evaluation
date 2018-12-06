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
library(tidyr)

### read PP count data

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Bottle/Phytoplankton")

df <- data.table(read.csv("181205_BloomSail_Plankton_counts.csv"))
df <- df[,-seq(4, 21,1)]


names(df) <-
  c("date", "station", "dep",
    "Aphanizomenon.1",	"Aphanizomenon.2", "Aphanizomenon.3",
    "Aphanizomenon.t",	"Aphanizomenon.HV",	"Aphanizomenon.Hl",
    "Dolichospermum.1", "Dolichospermum.2", "Dolichospermum.3", "Dolichospermum.4",
    "Dolichospermum.t", "Dolichospermum.HV", "Dolichospermum.Hl",
    "Nodularia.1", "Nodularia.2", "Nodularia.3",
    "Nodularia.t", "Nodularia.HV", "Nodularia.Hl",
    "Nodulariadead.1", "Nodulariadead.2", "Nodulariadead.3", "Nodulariadead.t", 
    "total.t"
)


df <- gather(df, para, value, Aphanizomenon.1:total.t, factor_key=TRUE)
df <- separate(df, col = para, into = c("Species", "class"))


df$date <- ymd(df$date)
df$dep <- as.numeric(df$dep)

df <- data.table(df)




Fig <-
ggplot(df[dep %in% c(0,5,20) &
                   station %in% c("P07", "P10") &
                   class == "t"],
       aes(date, value, col=as.factor(dep)))+
  geom_point()+
  geom_line()+
  facet_grid(station~Species)+
  coord_cartesian(ylim=c(0, 500))+
  scale_color_viridis(discrete = TRUE, name= "Depth (m)")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(y="Biomass (mg/m3)")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("./Phytoplankton_P0710_Dep0-5-20_total_biomass.tiff", width = 350, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
ggplot(df[dep %in% c(0,5,20) &
                   station %in% c("P07") &
                   class %in% c("1", "2", "3", "4", "t")],
       aes(date, value, col=as.factor(class)))+
  geom_point()+
  geom_line()+
  facet_grid(dep~Species)+
  coord_cartesian(ylim=c(0, 500))+
  scale_color_viridis(discrete = TRUE, name= "Size class")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(y="Biomass (mg/m3)")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./181009_Phytoplankton_sizeclasses_P07.tiff", width = 350, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
ggplot(df[dep %in% c(0,5,20) &
                   station %in% c("P10") &
                   class %in% c("1", "2", "3", "4", "t")],
       aes(date, value, col=as.factor(class)))+
  geom_point()+
  geom_line()+
  facet_grid(dep~Species)+
  coord_cartesian(ylim=c(0, 500))+
  scale_color_viridis(discrete = TRUE, name= "Size class")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(y="Biomass (mg/m3)")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./181009_Phytoplankton_sizeclasses_P10.tiff", width = 350, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
ggplot(df[dep %in% c(0,5,20) &
                   station %in% c("P07", "P10") &
                   class == "HV"],
       aes(date, value, col=as.factor(dep)))+
  geom_point()+
  geom_line()+
  facet_grid(station~Species)+
  coord_cartesian(ylim=c(0, 1e8))+
  scale_color_viridis(discrete = TRUE, name= "Depth (m)")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(y="Heterocysten (1/m3)")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./181009_Phytoplankton_Heterocysten_Volumen.tiff", width = 350, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
ggplot(df[dep %in% c(0,5,20) &
                   station %in% c("P07", "P10") &
                   class == "Hl"],
       aes(date, value, col=as.factor(dep)))+
  geom_point()+
  geom_line()+
  facet_grid(station~Species)+
  coord_cartesian(ylim=c(0, 50))+
  scale_color_viridis(discrete = TRUE, name= "Depth (m)")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(y="Heterocysten (1/mm)")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./181009_Phytoplankton_Heterocysten_length.tiff", width = 350, height = 250, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



df$dep.int <- cut(df$dep, c(-1,10,40))
df <- df[class == "t"]

df <- na.omit(df)
sum <- df[,.(value = mean(value)), by=.(date, dep.int, Species)]

Fig <-
  ggplot(sum,
         aes(date, value, col=as.factor(dep.int)))+
  geom_point()+
  geom_line()+
  facet_grid(~Species)+
  coord_cartesian(ylim=c(0, 500))+
  scale_color_viridis(discrete = TRUE, name= "Depth (m)")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  labs(y="Biomass (mg/m3)")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./181009_Phytoplankton_mean.tiff", width = 350, height = 150, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)
