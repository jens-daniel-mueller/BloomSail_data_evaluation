# Packages ----------------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(raster)
library(here)
library(geosphere)
library(lubridate)

# Section below Finnmaid --------------------------------------------------

# # read lon from old ncf file
# 
# E_sec <- nc_open(here::here("Data/GETM", "Finnmaid.W.TSage.zax.nc"))
# lat <- ncvar_get(E_sec, "latc")
# lon <- ncvar_get(E_sec, "lonc")
# # 
# # track <- bind_cols(lat=lat, lon=lon, route=rep("W", length(lat)))
# # 
# # nc_close(E_sec) 
# # rm(E_sec) 
# # 
# # E_sec <- nc_open(here::here("Data/GETM", "Finnmaid.E.TSage.zax.nc"))
# # lat <- ncvar_get(E_sec, "latc")
# # lon <- ncvar_get(E_sec, "lonc")
# # 
# # track <- bind_rows(track,
# #                    bind_cols(lat=lat, lon=lon, route=rep("E", length(lat))))
# # 
# # nc_close(E_sec) 
# # rm(E_sec) 
# # 
# # track %>% 
# #   ggplot(aes(lon, lat, col=route))+
# #   geom_path()
# 
# # read ncf file
# 
# E_sec <- nc_open(here::here("Data/GETM", "2018_Finnmaid.E.TSage.zax.nc"))
# print(E_sec)
# 
# lon <- ncvar_get(E_sec, "lonc")
# lat <- ncvar_get(E_sec, "latc", verbose = F)
# t <- ncvar_get(E_sec, "time")
# zax <- ncvar_get(E_sec, "zax")
# 
# 
# E_sec_array <- ncvar_get(E_sec, "temp") # store the data in a 3-dimensional array
# dim(E_sec_array) # should be 3d with dimensions: 1575 coordinate, 51 depth levels, 31 time steps
# 
# fillvalue <- ncatt_get(E_sec, "temp", "_FillValue")
# nc_close(E_sec) 
# 
# # Working with the data
# 
# E_sec_array[E_sec_array == fillvalue$value] <- NA
# 
# max_col <- max(E_sec_array, na.rm = TRUE)
# min_col <- min(E_sec_array, na.rm = TRUE)
# 
# # i <- 3
# for (i in seq(1,length(t),1)){
# 
# E_sec_slice <- E_sec_array[, , i] 
# E_sec_r <- raster(t(E_sec_slice), xmn=min(lon), xmx=max(lon), ymn=min(zax), ymx=max(zax))#,
# E_sec_r <- flip(E_sec_r, direction='y')
# 
# date <- ymd_hms("2018-4-1 00:00:00")+t[i]
# 
# # Plotting
# E_sec_r_df <- as.data.frame(E_sec_r, xy=TRUE)
# 
# E_sec_r_df %>% 
# ggplot() +
#   geom_raster(aes(x = x, y = -y, 
#                   fill = layer)) + 
#   scale_fill_viridis_c(name="SST (°C)")+#,
#                        #limits = c(min_col,max_col))+
#   scale_y_reverse()+
#   labs(x="Lon (°E)", y="Depth (m)", title = paste(date ,"| Route: E"))+
#   coord_cartesian(expand = 0)+
#   theme_bw()
# 
# ggsave(here::here("Plots/GETM/Sections", paste(as.character(date),"_Temp_Route_E.jpg",sep = "")),
#        width = 7, height = 4, dpi = 150)
# 
# rm(E_sec_slice, E_sec_r, E_sec_r_df)
# }




# Animation Section below Finnmaid --------------------------------------------------
# 
# # read lon from old ncf file
# 
# E_sec <- nc_open(here::here("Data/GETM", "Finnmaid.E.TSage.zax.nc"))
# lon <- ncvar_get(E_sec, "lonc")
# nc_close(E_sec) 
# rm(E_sec) 
# 
# # read ncf file
# 
# E_sec <- nc_open(here::here("Data/GETM", "2018_Finnmaid.E.TSage.zax.nc"))
# t <- ncvar_get(E_sec, "time")
# zax <- ncvar_get(E_sec, "zax")
# 
# E_sec_array <- ncvar_get(E_sec, "temp") # store the data in a 3-dimensional array
# dim(E_sec_array) # should be 3d with dimensions: 1575 coordinate, 51 depth levels, 31 time steps
# 
# fillvalue <- ncatt_get(E_sec, "temp", "_FillValue")
# nc_close(E_sec) 
# E_sec_array[E_sec_array == fillvalue$value] <- NA
# 
# max_col <- max(E_sec_array, na.rm = TRUE)
# min_col <- min(E_sec_array, na.rm = TRUE)
# 
# #i <- 3
# for (i in seq(1,length(t),1)){
# 
# E_sec_slice <- E_sec_array[, , i] 
# E_sec_r <- raster(t(E_sec_slice), xmn=min(lon), xmx=max(lon), ymn=min(zax), ymx=max(zax))#,
# E_sec_r <- flip(E_sec_r, direction='y')
# 
# date <- ymd_hms("2018-4-1 00:00:00")+t[i]
# head(date)
# 
# E_sec_r_df <- as.data.frame(E_sec_r, xy=TRUE)
# 
# E_sec_r_df <- E_sec_r_df %>% 
#   mutate(date = date)
# 
# if (exists("ts", inherits = FALSE)){
#   ts <- bind_rows(ts, E_sec_r_df)
# } else{ts <- E_sec_r_df}
# 
# rm(E_sec_slice, E_sec_r, E_sec_r_df)
# }
# 
# rm(date)
# 
# library(gganimate)
# length(unique(ts$date))
# 
# animate(
# ts %>% 
#   #filter(date <= ymd("2018-04-04")) %>% 
#   ggplot() +
#   geom_raster(aes(x = x, y = -y, 
#                   fill = layer)) + 
#   scale_fill_viridis_c(name="SST (°C)",
#                        limits = c(min_col,max_col))+
#   scale_y_reverse()+
#   labs(x="Lon (°E)", y="Depth (m)", title = 'Date: {frame_time}')+
#   coord_cartesian(expand = 0)+
#   theme_bw()+
#   transition_time(date), nframes = length(unique(ts$date)))
# 
# 
# anim_save(here::here("Plots/GETM/Sections", "Temp_Route_E_2018.gif"),
#           animation = last_animation())




# Vertical profiles in BloomSail Area -------------------------------------

# read lon from old ncf file

nc <- nc_open(here::here("Data/GETM", "Finnmaid.W.TSage.zax.nc"))
print(nc)
lat <- ncvar_get(nc, "latc")
lon <- ncvar_get(nc, "lonc")
nc_close(nc) 
rm(nc) 

# read relevant ncf file and subset temperature sections

nc <- nc_open(here::here("Data/GETM", "2018_Finnmaid.W.TSage.zax.nc"))
print(nc)
t <- ncvar_get(nc, "time")
zax <- ncvar_get(nc, "zax")

#var <- "temp"
attributes(nc$var)

for (var in c("SurfaceAge", "salt", "temp" )) {
  
array <- ncvar_get(nc, var) # store the data in a 3-dimensional array
dim(array) # should have 3 dimensions: 1575 coordinate, 51 depth levels, 153 time steps

fillvalue <- ncatt_get(nc, var, "_FillValue")
array[array == fillvalue$value] <- NA
rm(fillvalue)

#nc_close(nc)


# slice individual sections
# subset profiles in BloomSail Area
# bind profiles to one data frame

#i <- 3
for (i in seq(1,length(t),1)){

slice <- array[, , i]
slice <- as.data.frame(t(slice))
names(slice) <- lat
slice$dep <- zax

slice_long <- slice %>% 
  gather("lat", "value", 1:length(lat)) %>% 
  mutate(lat = as.numeric(lat),
         dep = -dep)

# slice_long %>%
#   ggplot(aes(lat, dep, col=value))+
#   geom_point()+
#   scale_y_reverse()+
#   scale_color_viridis_c(option = "A")

date <- ymd_hms("2018-4-1 00:00:00")+t[i]

temp <- slice_long %>% 
  filter(lat > 57.33, lat<57.5) %>% 
  group_by(dep) %>% 
  summarise_all("mean") %>% 
  ungroup() %>% 
  mutate(date = date)
  
if (exists("profiles", inherits = FALSE)){
  profiles <- bind_rows(profiles, temp)
} else{profiles <- temp}

rm(slice, slice_long, temp, date)

}

profiles <- profiles %>% 
  mutate(var = var)

if (exists("profiles_all", inherits = FALSE)){
  profiles_all <- bind_rows(profiles_all, profiles)
} else{profiles_all <- profiles}

rm(profiles, array)

}

# Plot profiles for spring and summer period

profiles_all %>% 
  filter(dep <= 35) %>% 
  filter(date >= ymd("2018-06-01")) %>% 
  ggplot(aes(value, dep, group=as.factor(date),  col=date)) +
  geom_path()+
  scale_color_viridis_c(trans = "time", name="")+
  scale_y_reverse()+
  labs(x="Value", y="Depth (m)", 
       title = "2018 | Finnmaid | Route E | 57.33-57.5 °N")+
  theme_bw()+
  facet_grid(~var, scales = "free_x")

ggsave(here::here("Plots/GETM/Profiles",
                  "Profiles_E_BloomSail_2018_summer.jpg"),
       width = 10, height = 5, dpi = 300)

profiles_all %>% 
  filter(dep <= 35) %>% 
  filter(date <= ymd("2018-05-31")) %>% 
  ggplot(aes(value, dep, group=as.factor(date),  col=date)) +
  geom_path()+
  scale_color_viridis_c(trans = "time", name="")+
  scale_y_reverse()+
  labs(x="Value", y="Depth (m)", 
       title = "2018 | Finnmaid | Route E | 57.33-57.5 °N")+
  theme_bw()+
  facet_grid(~var, scales = "free_x")

ggsave(here::here("Plots/GETM/Profiles",
                  "Profiles_E_BloomSail_2018_spring.jpg"),
       width = 10, height = 5, dpi = 300)


 

# Timeseries Hovmoeller (location vs time) --------------------------------
#of water column parameters (mixing depth, SST ...)
# 
# # Read and plot timeseries data
# 
# nc <- nc_open(here::here("Data/GETM", "2018_Finnmaid.W.2d.nc"))
# lat <- ncvar_get(nc, "latc", verbose = F)
# t <- ymd_hms("2018-4-1 00:00:00")+ ncvar_get(nc, "time")
# head(t) # look at the first few entries in the time vector
# 
# var <- "mld_age_1"
# for (var in c("SSS","SST","mld_age_1","mld_age_3","mld_age_5", "mld_rho","mld_tke")){
# 
# array <- ncvar_get(nc, var) # store the data in a 3-dimensional array
# #dim(E_ts_array) # should be 2d with dimensions: 1575 coordinate, 31d*(24h/d/3h)=248 time steps
# 
# fillvalue <- ncatt_get(nc, var, "_FillValue")
# array[array == fillvalue$value] <- NA
# 
# E_ts_array_r <- raster(t(E_ts_array), xmn=min(lat), xmx=max(lat), ymn=min(t), ymx=max(t))#,
# E_ts_array_r <- flip(E_ts_array_r, direction='y')
# 
# E_ts_array_r_df <- as.data.frame(E_ts_array_r, xy=TRUE)
# E_ts_array_r_df <- E_ts_array_r_df %>% 
#   arrange(x,y) %>% 
#   mutate(y = rep(t, length(lat)))
# 
# E_ts_array_r_df %>% 
#   filter(layer <= 50,
#         layer >= 0) %>% 
#   ggplot(aes(y, x, fill = layer, z = layer)) +
#   geom_raster()+ 
#   #geom_contour(breaks = seq(0,100,10), col="white")+ 
#   scale_fill_viridis_c(name=var, direction = -1)+
#   labs(x="Date", y="Lat (°N)", title = paste("2018 | Finnmaid Route: E"))+
#   theme_bw()+
#   coord_cartesian(expand = 0)
# 
# ggsave(here::here("Plots/GETM/timeseries", paste(var,"_E_2018.jpg",sep = "")),
#        width = 7, height = 5, dpi = 300)
# 
# rm(E_ts_array, E_ts_array_r, E_ts_array_r_df, fillvalue)
# 
# }
# 
# 


# Timeseries one location -------------------------------------------------

#### Read and plot timeseries data

nc <- nc_open(here::here("Data/GETM", "2018_Finnmaid.W.2d.nc"))
print(nc)

lon <- ncvar_get(nc, "lonc")
lat <- ncvar_get(nc, "latc", verbose = F)
t <- ymd_hms("2018-4-1 00:00:00")+ncvar_get(nc, "time")

head(t) # look at the first few entries in the time vector

var <- "SST"

for (var in c("SST","mld_age_1","mld_age_3","mld_age_5", "mld_rho","mld_tke")){

array <- ncvar_get(nc, var) # store the data in a 3-dimensional array
dim(array) # should be 2d with dimensions: 1575 coordinate, 31d*(24h/d/3h)=248 time steps

array <- as.data.frame(t(array), xy=TRUE)
array <- as_tibble(array)

array_long <- array %>%
  set_names(as.character(lat)) %>% 
  mutate(date_time = t) %>% 
  gather("lat", "value", 1:length(lat)) %>% 
  mutate(lat = as.numeric(lat)) %>% 
  filter(lat > 57.33, lat<57.5) %>%  
  group_by(date_time) %>% 
  summarise_all("mean") %>% 
  ungroup() %>% 
  mutate(var = var)


if (exists("ts", inherits = FALSE)){
  ts <- bind_rows(ts, array_long)
} else{ts <- array_long}

rm(array, array_long)
}

ts %>% 
  filter(date_time >= ymd("2018-06-01")) %>% 
  ggplot(aes(date_time, value, col=var)) +
  geom_line()+ 
  labs(x="Date", y="Value", title = paste("2018 | Finnmaid | Route E | 57.33-57.5 °N"))+
  theme_bw()

ggsave(here::here("Plots/GETM/timeseries", paste("ts_E_BloomSail_2018_summer.jpg",sep = "")),
       width = 13, height = 3, dpi = 300)

ts %>% 
  filter(date_time <= ymd("2018-05-31")) %>% 
  ggplot(aes(date_time, value, col=var)) +
  geom_line()+ 
  labs(x="Date", y="Value", title = paste("2018 | Finnmaid | Route E | 57.33-57.5 °N"))+
  theme_bw()

ggsave(here::here("Plots/GETM/timeseries", paste("ts_E_BloomSail_2018_spring.jpg",sep = "")),
       width = 13, height = 3, dpi = 300)

