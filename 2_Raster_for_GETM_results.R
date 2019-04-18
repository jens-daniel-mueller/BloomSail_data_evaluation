# Packages ----------------------------------------------------------------
library(tidyverse)
library(geosphere)
library(plotKML)
library(scico)

####load track coordinates and bathymetry data file
####format date column to POSIXct

df.map <- read_csv(here::here("Data/Maps","Bathymetry_Gotland_east.csv"))
track <- read_csv(here::here("Data/_summarized_data_files", "TinaV_Track.csv"))

Grid_start <- readGPX(here::here("Data/GETM/Standard_track",
                         "BloomSail_GETM_grid_startpoints.gpx"))
Grid_start <- Grid_start$waypoints[,1:2]

####Produce first plots of Sensor data and safe to folder "plots"
# 
# track %>% 
#   ggplot(aes(lon, lat))+
#   geom_path()


# East Gotland EW sections ---------------------------------------------------

bearing = 118
steps = 59

#transect <- 1
for (transect in seq(1,nrow(Grid_start),1)) {

  transect_temp <- bind_cols(Grid_start[transect,], transect = transect)

for (i in seq(1,steps,1)) {
  
  lon = destPoint(p = slice(transect_temp, n()), b = bearing, d = 2000)[,1]
  lat = destPoint(p = slice(transect_temp, n()), b = bearing, d = 2000)[,2]

  transect_temp <-
    bind_rows(transect_temp,
      bind_cols(lon=lon, lat=lat, transect=transect)
    )
}
  
  if (exists("transect_GE")){
    transect_GE <- bind_rows(transect_GE, transect_temp)
  } else{transect_GE <- transect_temp}
  
}
  
rm(end, start, bearing, i, lat, lon, steps)

transect_GE %>% 
  ggplot(aes(lon, lat, col=transect))+
  geom_point()


# East Gotland NS sections ---------------------------------------------------

bearing = 28
steps = 65

Grid_start <- transect_GE[transect_GE$transect==1,][seq(8,60,6),1:2]

#transect <- 1
for (transect in seq(1,nrow(Grid_start),1)) {

  transect_temp <- bind_cols(Grid_start[transect,], transect = transect)

for (i in seq(1,steps,1)) {
  
  lon = destPoint(p = slice(transect_temp, n()), b = bearing, d = 2000)[,1]
  lat = destPoint(p = slice(transect_temp, n()), b = bearing, d = 2000)[,2]

  transect_temp <-
    bind_rows(transect_temp,
      bind_cols(lon=lon, lat=lat, transect=transect)
    )
}
  
  if (exists("transect_NS")){
    transect_NS <- bind_rows(transect_NS, transect_temp)
  } else{transect_NS <- transect_temp}
  
}
  
rm(end, start, bearing, i, lat, lon, steps)


transect_GE$direction <- "EW"
transect_NS$direction <- "NS"

transect_GETM <- bind_rows(
  transect_GE, transect_NS)


track %>% 
  ggplot(aes(lon, lat, col="BloomSail Track"))+
  geom_raster(data=df.map, aes(lon, lat, fill=elev))+
  scale_fill_scico(palette = "oslo", na.value = "black", name="Depth [m]")+
  geom_path()+
  geom_point(data = transect_GETM, aes(lon, lat, col="GETM raster", shape=direction))+
  scale_color_manual(values = c("orange", "red"), name="")+
  coord_quickmap()+
  theme_bw()

ggsave(here::here("Plots/GETM/Raster_Area", "GETM_raster_BloomSail.jpg"), width = 8, height = 6)
write_csv(transect_GETM, here::here("Data/_summarized_data_files", "GETM_raster_BloomSail.csv"))


# 
# # Reference track south ---------------------------------------------------
# 
# track_ref_S <-
#     bind_cols(lon=18.975, lat=57.425)
# end   <- cbind(lon=19.4,  lat=57.3)
# 
# steps = 50
# bearing = bearing(track_ref_S, end)
# 
# for (i in seq(1,steps,1)) {
#   
#   lon = destPoint(p = slice( track_ref_S, n()), b = bearing, d = 2000)[,1]
#   lat = destPoint(p = slice( track_ref_S, n()), b = bearing, d = 2000)[,2]
# 
#   track_ref_S <-
#     bind_rows(track_ref_S,
#       bind_cols(lon=lon, lat=lat)
#     )
#   
#   start <- cbind(lon, lat)
#   
# }
# 
# rm(end, start, bearing, i, lat, lon, steps)
# 
# 
# # Reference track east ---------------------------------------------------
# 
# track_ref_E <- slice( track_ref_S, 16)
# 
# end   <- cbind(lon=19.54, lat=57.4)
# 
# steps = (round(distGeo(track_ref_E, end), -3)+6000) /2000
# bearing = bearing(track_ref_E, end)
# 
# for (i in seq(1,steps,1)) {
#   
#   lon = destPoint(p = slice( track_ref_E, n()), b = bearing, d = 2000)[,1]
#   lat = destPoint(p = slice( track_ref_E, n()), b = bearing, d = 2000)[,2]
#   
#   track_ref_E <-
#     bind_rows(track_ref_E,
#               bind_cols(lon=lon, lat=lat)
#     )
#   
#   start <- cbind(lon, lat)
#   
# }
# 
# # Reference track north ---------------------------------------------------
# 
# track_ref_N <- bind_cols(lon=19.18, lat=57.48)
# end   <- slice( track_ref_E, 8)
# 
# steps = 50
# bearing = bearing(track_ref_N, end)
# 
# for (i in seq(1,steps,1)) {
#   
#   lon = destPoint(p = slice( track_ref_N, n()), b = bearing, d = 2000)[,1]
#   lat = destPoint(p = slice( track_ref_N, n()), b = bearing, d = 2000)[,2]
# 
#   track_ref_N <-
#     bind_rows(track_ref_N,
#       bind_cols(lon=lon, lat=lat)
#     )
#   
#   start <- cbind(lon, lat)
#   
# }
# 
# rm(end, start, bearing, i, lat, lon, steps)
# 
# 
# rm(end, start, bearing, i, lat, lon, steps)
# 
# track %>% 
#   ggplot(aes(lon, lat))+
#   geom_raster(data=df.map, aes(lon, lat, fill=elev))+
#   scale_fill_continuous(na.value = "black", name="Depth [m]")+
#   geom_path(col="white")+
#   geom_point(data = track_ref_S, aes(lon, lat), col="red")+
#   geom_point(data = track_ref_N, aes(lon, lat), col="red")+
#   geom_point(data = track_ref_E, aes(lon, lat), col="red")
# 

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