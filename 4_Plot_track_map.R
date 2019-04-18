# Packages ----------------------------------------------------------------
library(tidyverse)
library(geosphere)

####load track coordinates and bathymetry data file
####format date column to POSIXct

df.map <- read_csv(here::here("Data/Maps","Bathymetry_Gotland_east.csv"))
track <- read_csv(here::here("Data/_summarized_data_files", "TinaV_Track.csv"))

####Produce first plots of Sensor data and safe to folder "plots"

track %>% 
  ggplot(aes(lon, lat))+
  geom_path()+
  geom_point(data=track_ref, aes(lon, lat), col="red")

track_ref <-
bind_rows(
bind_cols(lon=19,    lat=57.425),
bind_cols(lon=19.4,  lat=57.3),
bind_cols(lon=19.54, lat=57.4),
bind_cols(lon=19.18, lat=57.48)
)


track_ref <- track_ref %>% 
  mutate(bearing)


# # Plot Map with track data
# 
# track %>% 
#   ggplot()+
#   geom_path(aes(lon, lat, group=transect.ID), col="grey")+
#   geom_path(data = track[label %in% c("P07", "P10")], aes(lon, lat, col=label))+
#   scale_color_brewer(palette = "Set1", name="Station")+
#   labs(x="Longitude (°E)", y="Latitude (°N)")+
#   coord_quickmap()
# 
# ggsave("Track.png", width = 200, height = 200, units = "mm")
# 
# # Plot Map with track data and all Stations
# 
# stations <-
#   track %>% 
#   filter(label %in% c("P01","P02","P03","P04","P05","P06","P07",
#                       "P08","P09","P10","P11","P12")) %>% 
#   group_by(label) %>% 
#   summarise(lon=mean(lon),
#             lat=mean(lat)) %>% 
#   ungroup()
# 
# track %>% 
# ggplot()+
#   geom_raster(data=df.map, aes(lon, lat, fill=elev))+
#   scale_fill_continuous(na.value = "black", name="Tiefe [m]")+
#   geom_path(aes(lon, lat), col="grey80")+
#   geom_point(data=stations, aes(lon, lat, col=label),size=3)+
#   #scale_color_discrete(name="Station")+
#   scale_color_viridis_d(name="Station", option = "C")+
#   labs(x="Längengrad (°E)", y="Breitengrad (°N)")+
#   coord_quickmap(expand = 0, ylim = c(57.25,57.6), xlim = c(18.6, 19.8))+
#   theme_bw()+
#   guides(col = guide_legend(nrow = 5))
# 
# ggsave("Track_all_stations.png", width = 250, height = 200, units = "mm")
#  