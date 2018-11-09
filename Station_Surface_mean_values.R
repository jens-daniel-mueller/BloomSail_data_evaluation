library(data.table)
library(tidyverse)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Merged_data_files")

df <- read.csv("BloomSail_Sensor_Track_data.csv") %>% 
  setDT() %>% 
  mutate(date = ymd_hms(date))


surface.mean <- df %>%
  filter(type == "P" & cast == "down" & Dep.S <5 & Dep.S > 2 &
           label %in% c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09",
                        "P10", "P11", "P12", "P13")) %>% 
  group_by(date = ymd_hms(start.date),
           Station =label) %>% 
  summarise(lat = mean(lat),
            lon = mean(lon),
            pCO2 = mean(pCO2),
            Sal = mean(Sal.S),
            CT = mean(CT.calc),
            Tem = mean(Tem.S)) %>% 
  ungroup()


setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")

ggplot(surface.mean, aes(date, Tem, col=Station))+
  geom_point()+
  geom_path()+
  scale_color_viridis_d()+
  labs(y="Temperature (°C)", x="Date")

ggsave("Temperature_vs_Date_by_Station.jpg", plot = last_plot(), device = "jpeg", path = NULL,
       scale = 1, width = 15, height = 10, units = c("cm"),
       dpi = 600, limitsize = TRUE)

ggplot(surface.mean, aes(date, CT, col=Station))+
  geom_point()+
  geom_path()+
  scale_color_viridis_d()+
  labs(y="CT* (µmol / kg)", x="Date")+
  ylim(1390, 1550)

ggsave("CT_vs_Date_by_Station.jpg", plot = last_plot(), device = "jpeg", path = NULL,
       scale = 1, width = 15, height = 10, units = c("cm"),
       dpi = 600, limitsize = TRUE)

ggplot(surface.mean, aes(date, Sal, col=Station))+
  geom_point()+
  geom_path()+
  scale_color_viridis_d()+
  labs(y="Salinity", x="Date")

ggsave("Salinity_vs_Date_by_Station.jpg", plot = last_plot(), device = "jpeg", path = NULL,
       scale = 1, width = 15, height = 10, units = c("cm"),
       dpi = 600, limitsize = TRUE)



ggplot()+
  geom_polygon(data=map_data("world"), aes(x=long, y = lat, group = group))+
  geom_point(data=surface.mean, aes(lon, lat, col=Station))+
  scale_color_viridis_d()+
  labs(y="Lat (°N)", x="Lon (°E)")+
  coord_quickmap(xlim = c(18,20), ylim = c(57,58))

ggsave("Map_Stations.jpg", plot = last_plot(), device = "jpeg", path = NULL,
       scale = 1, width = 15, height = 15, units = c("cm"),
       dpi = 600, limitsize = TRUE)



setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Merged_data_files")
write.csv(surface.mean, "BloomSail_Stations_data_Surface_parameters.csv", row.names = FALSE)



