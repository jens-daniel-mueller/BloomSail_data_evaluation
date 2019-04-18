# Packages ----------------------------------------------------------------
library(tidyverse)
library(geosphere)
library(plotKML)
library(scico)

####load track coordinates and bathymetry data file
####format date column to POSIXct

df.map <- read_csv(here::here("Data/Maps","Bathymetry_Gotland_east.csv"))
track <- read_csv(here::here("Data/_summarized_data_files", "TinaV_Track.csv"))


# Reference track south ---------------------------------------------------

track_ref_S <-
  bind_cols(lon=18.975, lat=57.425)
end   <- cbind(lon=19.4,  lat=57.3)

steps = 15
bearing = bearing(track_ref_S, end)

for (i in seq(1,steps,1)) {
  
  lon = destPoint(p = slice( track_ref_S, n()), b = bearing, d = 2000)[,1]
  lat = destPoint(p = slice( track_ref_S, n()), b = bearing, d = 2000)[,2]
  
  track_ref_S <-
    bind_rows(track_ref_S,
              bind_cols(lon=lon, lat=lat)
    )
  
  start <- cbind(lon, lat)
  
}

rm(end, start, bearing, i, lat, lon, steps)

track %>%
  ggplot(aes(lon, lat))+
  geom_path()+
  geom_point(data = track_ref_S, aes(lon, lat), col="red")

# Reference track east ---------------------------------------------------

track_ref_E <- slice( track_ref_S, n())
end   <- cbind(lon=19.54, lat=57.4)

steps = 7
bearing = bearing(track_ref_E, end)

for (i in seq(1,steps,1)) {
  
  lon = destPoint(p = slice( track_ref_E, n()), b = bearing, d = 2000)[,1]
  lat = destPoint(p = slice( track_ref_E, n()), b = bearing, d = 2000)[,2]
  
  track_ref_E <-
    bind_rows(track_ref_E,
              bind_cols(lon=lon, lat=lat)
    )
  
  start <- cbind(lon, lat)
  
}

track %>%
  ggplot(aes(lon, lat))+
  geom_path()+
  geom_point(data = track_ref_S, aes(lon, lat), col="red")+
  geom_point(data = track_ref_E, aes(lon, lat), col="red")


# Reference track north ---------------------------------------------------

track_ref_N <- slice( track_ref_E, n())
end   <- cbind(lon=19.1, lat=57.5)

steps = 12
bearing = bearing(track_ref_N, end)

for (i in seq(1,steps,1)) {
  
  lon = destPoint(p = slice( track_ref_N, n()), b = bearing, d = 2000)[,1]
  lat = destPoint(p = slice( track_ref_N, n()), b = bearing, d = 2000)[,2]
  
  track_ref_N <-
    bind_rows(track_ref_N,
              bind_cols(lon=lon, lat=lat)
    )
  
  start <- cbind(lon, lat)
  
}

track %>%
  ggplot(aes(lon, lat))+
  geom_path()+
  geom_point(data = track_ref_S, aes(lon, lat), col="red")+
  geom_point(data = track_ref_N, aes(lon, lat), col="red")+
  geom_point(data = track_ref_E, aes(lon, lat), col="red")


# Reference track West ---------------------------------------------------

track_ref_W <- slice( track_ref_N, n())
end   <- slice( track_ref_S, 2)

steps = 6
bearing = bearing(track_ref_W, end)

for (i in seq(1,steps,1)) {
  
  lon = destPoint(p = slice( track_ref_N, n()), b = bearing, d = 2000)[,1]
  lat = destPoint(p = slice( track_ref_N, n()), b = bearing, d = 2000)[,2]
  
  track_ref_N <-
    bind_rows(track_ref_N,
              bind_cols(lon=lon, lat=lat)
    )
  
  start <- cbind(lon, lat)
  
}

track %>%
  ggplot(aes(lon, lat))+
  geom_path()+
  geom_point(data = track_ref_S, aes(lon, lat), col="red")+
  geom_point(data = track_ref_N, aes(lon, lat), col="red")+
  geom_point(data = track_ref_W, aes(lon, lat), col="red")+
  geom_point(data = track_ref_E, aes(lon, lat), col="red")

transect_GETM <- bind_rows(
  track_ref_E, track_ref_N, track_ref_S, track_ref_W
)

transect_GETM <- distinct(transect_GETM)

track %>% 
  ggplot(aes(lon, lat, col="BloomSail Track"))+
  geom_raster(data=df.map, aes(lon, lat, fill=elev))+
  scale_fill_scico(palette = "oslo", na.value = "black", name="Depth [m]")+
  geom_path()+
  geom_point(data = transect_GETM, aes(lon, lat, col="GETM track | 2km"))+
  scale_color_manual(values = c("orange", "red"), name="")+
  coord_quickmap(expand = 0)+
  theme_bw()

ggsave(here::here("Plots/GETM/Raster_Area", "GETM_raster_BloomSail.jpg"), width = 8, height = 6)
write_csv(transect_GETM, here::here("Data/_summarized_data_files", "GETM_raster_BloomSail.csv"))



# # Gridded raster for entire eastern Gotland area --------------------------
# 
# 
# Grid_start <- readGPX(here::here("Data/GETM/Standard_track",
#                          "BloomSail_GETM_grid_startpoints.gpx"))
# Grid_start <- Grid_start$waypoints[,1:2]
# 
# ####Produce first plots of Sensor data and safe to folder "plots"
# # 
# # track %>% 
# #   ggplot(aes(lon, lat))+
# #   geom_path()
# 
# 
# # East Gotland EW sections ---------------------------------------------------
# 
# bearing = 118
# steps = 59
# 
# #transect <- 1
# for (transect in seq(1,nrow(Grid_start),1)) {
# 
#   transect_temp <- bind_cols(Grid_start[transect,], transect = transect)
# 
# for (i in seq(1,steps,1)) {
#   
#   lon = destPoint(p = slice(transect_temp, n()), b = bearing, d = 2000)[,1]
#   lat = destPoint(p = slice(transect_temp, n()), b = bearing, d = 2000)[,2]
# 
#   transect_temp <-
#     bind_rows(transect_temp,
#       bind_cols(lon=lon, lat=lat, transect=transect)
#     )
# }
#   
#   if (exists("transect_GE")){
#     transect_GE <- bind_rows(transect_GE, transect_temp)
#   } else{transect_GE <- transect_temp}
#   
# }
#   
# rm(end, start, bearing, i, lat, lon, steps)
# 
# transect_GE %>% 
#   ggplot(aes(lon, lat, col=transect))+
#   geom_point()
# 
# 
# # East Gotland NS sections ---------------------------------------------------
# 
# bearing = 28
# steps = 65
# 
# Grid_start <- transect_GE[transect_GE$transect==1,][seq(8,60,6),1:2]
# 
# #transect <- 1
# for (transect in seq(1,nrow(Grid_start),1)) {
# 
#   transect_temp <- bind_cols(Grid_start[transect,], transect = transect)
# 
# for (i in seq(1,steps,1)) {
#   
#   lon = destPoint(p = slice(transect_temp, n()), b = bearing, d = 2000)[,1]
#   lat = destPoint(p = slice(transect_temp, n()), b = bearing, d = 2000)[,2]
# 
#   transect_temp <-
#     bind_rows(transect_temp,
#       bind_cols(lon=lon, lat=lat, transect=transect)
#     )
# }
#   
#   if (exists("transect_NS")){
#     transect_NS <- bind_rows(transect_NS, transect_temp)
#   } else{transect_NS <- transect_temp}
#   
# }
#   
# rm(end, start, bearing, i, lat, lon, steps)
# 
# 
# transect_GE$direction <- "EW"
# transect_NS$direction <- "NS"
# 
# transect_GETM <- bind_rows(
#   transect_GE, transect_NS)
# 
# 
# track %>% 
#   ggplot(aes(lon, lat, col="BloomSail Track"))+
#   geom_raster(data=df.map, aes(lon, lat, fill=elev))+
#   scale_fill_scico(palette = "oslo", na.value = "black", name="Depth [m]")+
#   geom_path()+
#   geom_point(data = transect_GETM, aes(lon, lat, col="GETM raster", shape=direction))+
#   scale_color_manual(values = c("orange", "red"), name="")+
#   coord_quickmap()+
#   theme_bw()
# 
# ggsave(here::here("Plots/GETM/Raster_Area", "GETM_raster_BloomSail.jpg"), width = 8, height = 6)
# write_csv(transect_GETM, here::here("Data/_summarized_data_files", "GETM_raster_BloomSail.csv"))
  