# Packages ----------------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(raster)
library(here)
library(geosphere)
library(lubridate)

# Section below Finnmaid --------------------------------------------------

# read lon from old ncf file

E_sec <- nc_open(here::here("Data/GETM", "Finnmaid.E.TSage.zax.nc"))
lon <- ncvar_get(E_sec, "lonc")
nc_close(E_sec) 
rm(E_sec) 

# read ncf file

E_sec <- nc_open(here::here("Data/GETM", "2018_Finnmaid.E.TSage.zax.nc"))
print(E_sec)

lon <- ncvar_get(E_sec, "lonc")
lat <- ncvar_get(E_sec, "latc", verbose = F)
t <- ncvar_get(E_sec, "time")
zax <- ncvar_get(E_sec, "zax")


E_sec_array <- ncvar_get(E_sec, "temp") # store the data in a 3-dimensional array
dim(E_sec_array) # should be 3d with dimensions: 1575 coordinate, 51 depth levels, 31 time steps

fillvalue <- ncatt_get(E_sec, "temp", "_FillValue")
nc_close(E_sec) 

# Working with the data

E_sec_array[E_sec_array == fillvalue$value] <- NA

max_col <- max(E_sec_array, na.rm = TRUE)
min_col <- min(E_sec_array, na.rm = TRUE)

# i <- 3
for (i in seq(1,length(t),1)){

E_sec_slice <- E_sec_array[, , i] 
E_sec_r <- raster(t(E_sec_slice), xmn=min(lon), xmx=max(lon), ymn=min(zax), ymx=max(zax))#,
E_sec_r <- flip(E_sec_r, direction='y')

date <- ymd_hms("2018-4-1 00:00:00")+t[i]

# Plotting
E_sec_r_df <- as.data.frame(E_sec_r, xy=TRUE)

E_sec_r_df %>% 
ggplot() +
  geom_raster(aes(x = x, y = -y, 
                  fill = layer)) + 
  scale_fill_viridis_c(name="SST (°C)")+#,
                       #limits = c(min_col,max_col))+
  scale_y_reverse()+
  labs(x="Lon (°E)", y="Depth (m)", title = paste(date ,"| Route: E"))+
  coord_cartesian(expand = 0)+
  theme_bw()

ggsave(here::here("Plots/GETM/Sections", paste(as.character(date),"_Temp_Route_E.jpg",sep = "")),
       width = 7, height = 4, dpi = 150)

rm(E_sec_slice, E_sec_r, E_sec_r_df)
}


# Vertical profiles in BloomSail Area -------------------------------------

# read lon from old ncf file

E_sec <- nc_open(here::here("Data/GETM", "Finnmaid.E.TSage.zax.nc"))
lat <- ncvar_get(E_sec, "latc")
nc_close(E_sec) 
rm(E_sec) 

# read relevant ncf file and subset temperature sections

E_sec <- nc_open(here::here("Data/GETM", "2018_Finnmaid.E.TSage.zax.nc"))
print(E_sec)
t <- ncvar_get(E_sec, "time")
zax <- ncvar_get(E_sec, "zax")

E_sec_array <- ncvar_get(E_sec, "temp") # store the data in a 3-dimensional array
dim(E_sec_array) # should have 3 dimensions: 1575 coordinate, 51 depth levels, 153 time steps

fillvalue <- ncatt_get(E_sec, "temp", "_FillValue")

nc_close(E_sec) 

E_sec_array[E_sec_array == fillvalue$value] <- NA


# slice individual sections
# subset profiles in BloomSail Area
# bind profiles to one data frame

#i <- 3
for (i in seq(1,length(t),1)){

E_sec_slice <- E_sec_array[, , i] 
E_sec_r <- raster(t(E_sec_slice), xmn=min(lat), xmx=max(lat), ymn=min(zax), ymx=max(zax))#,
E_sec_r <- flip(E_sec_r, direction='y')
E_sec_r_df <- as.data.frame(E_sec_r, xy=TRUE)

date <- ymd_hms("2018-4-1 00:00:00")+t[i]

temp <- E_sec_r_df %>% 
  filter(x > 57.33, x<57.5) %>% 
  group_by(y) %>% 
  summarise_all("mean") %>% 
  ungroup() %>% 
  mutate(date = date)
  
if (exists("profiles", inherits = FALSE)){
  profiles <- bind_rows(profiles, temp)
} else{profiles <- temp}

rm(E_sec_slice, E_sec_r, E_sec_r_df, temp)
}

# Plot profiles for spring and summer period

profiles %>% 
  filter(date >= ymd("2018-06-01"),
         layer >= 10) %>% 
  ggplot(aes(layer, -y, group=as.factor(date),  col=date)) +
  geom_path()+
  scale_color_viridis_c(trans = "time")+
  scale_y_reverse()+
  labs(x="Temperature (degC)", y="Depth (m)", 
       title = "2018 | Finnmaid | Route E | 57.33-57.5 °N")+
  theme_bw()

ggsave(here::here("Plots/GETM/Profiles",
                  "Profiles_E_BloomSail_2018_summer.jpg"),
       width = 6, height = 5, dpi = 300)


profiles %>% 
  filter(date < ymd("2018-06-01"),
         y >= -60) %>% 
  ggplot(aes(layer, -y, group=as.factor(date),  col=date)) +
  geom_path()+
  scale_y_reverse()+
  scale_color_viridis_c(trans = "time")+
  labs(x="Temperature (degC)", y="Depth (m)", 
       title = "2018 | Finnmaid | Route E | 57.33-57.5 °N")+
  theme_bw()

ggsave(here::here("Plots/GETM/Profiles",
                  "Profiles_E_BloomSail_2018_spring.jpg"),
       width = 6, height = 5, dpi = 300)




# Timeseries Hovmoeller (location vs time) --------------------------------
#of water column parameters (mixing depth, SST ...)

# Read and plot timeseries data

E_ts <- nc_open(here::here("Data/GETM", "2018_Finnmaid.E.2d.nc"))
print(E_ts)

lon <- ncvar_get(E_ts, "lonc")
lat <- ncvar_get(E_ts, "latc", verbose = F)
t <- ymd_hms("2018-4-1 00:00:00")+ncvar_get(E_ts, "time")
SST <- ncvar_get(E_ts, "SST")

head(t) # look at the first few entries in the time vector

#var <- "mld_age_1"
for (var in c("SSS","SST","mld_age_1","mld_age_3","mld_age_5", "mld_rho","mld_tke")){

E_ts_array <- ncvar_get(E_ts, var) # store the data in a 3-dimensional array
#dim(E_ts_array) # should be 2d with dimensions: 1575 coordinate, 31d*(24h/d/3h)=248 time steps

fillvalue <- ncatt_get(E_ts, var, "_FillValue")
fillvalue
#nc_close(E_ts)

# Working with the data

E_ts_array[E_ts_array == fillvalue$value] <- NA
E_ts_array_r <- raster(t(E_ts_array), xmn=min(lat), xmx=max(lat), ymn=min(t), ymx=max(t))#,
E_ts_array_r <- flip(E_ts_array_r, direction='y')

# Plotting
E_ts_array_r_df <- as.data.frame(E_ts_array_r, xy=TRUE)
E_ts_array_r_df <- E_ts_array_r_df %>% 
  arrange(x,y) %>% 
  mutate(y = rep(t, length(lon)))

E_ts_array_r_df %>% 
  filter(layer <= 30,
         layer >= 0) %>% 
  ggplot() +
  geom_raster(aes(y, x, fill = layer)) + 
  scale_fill_viridis_c(name=var, direction = -1)+
  labs(x="Date", y="Lat (°N)", title = paste("2018 | Finnmaid Route: E"))+
  theme_bw()+
  coord_cartesian(expand = 0)

ggsave(here::here("Plots/GETM/timeseries", paste(var,"_E_2018.jpg",sep = "")),
       width = 7, height = 5, dpi = 300)

rm(E_ts_array, E_ts_array_r, E_ts_array_r_df, fillvalue)

}




# Timeseries one location -------------------------------------------------

#### Read and plot timeseries data

E_ts <- nc_open(here::here("Data/GETM", "2018_Finnmaid.E.2d.nc"))
print(E_ts)

lon <- ncvar_get(E_ts, "lonc")
lat <- ncvar_get(E_ts, "latc", verbose = F)
t <- ymd_hms("2018-4-1 00:00:00")+ncvar_get(E_ts, "time")
#t <- ncvar_get(E_ts, "time")/(60*60*24)
SST <- ncvar_get(E_ts, "SST")

head(t) # look at the first few entries in the time vector

#var <- "SSS"

for (var in c("SST","mld_age_1","mld_age_3","mld_age_5", "mld_rho","mld_tke")){

E_ts_array <- ncvar_get(E_ts, var) # store the data in a 3-dimensional array
#dim(E_ts_array) # should be 2d with dimensions: 1575 coordinate, 31d*(24h/d/3h)=248 time steps

fillvalue <- ncatt_get(E_ts, var, "_FillValue")
fillvalue
#nc_close(E_ts)

#### Working with the data ####

E_ts_array[E_ts_array == fillvalue$value] <- NA
E_ts_array_r <- raster(t(E_ts_array), xmn=min(lat), xmx=max(lat), ymn=min(t), ymx=max(t))#,
E_ts_array_r <- flip(E_ts_array_r, direction='y')

#### Plotting #####
E_ts_array_r_df <- as.data.frame(E_ts_array_r, xy=TRUE)

temp <- E_ts_array_r_df %>% 
  arrange(x,y) %>% 
  mutate(y = rep(t, length(lon))) %>% 
  filter(x > 57.33, x<57.5) %>% 
  group_by(y) %>% 
  summarise_all("mean") %>% 
  ungroup() %>% 
  mutate(var = var)


if (exists("ts", inherits = FALSE)){
  ts <- bind_rows(ts, temp)
} else{ts <- temp}


rm(E_ts_array, E_ts_array_r, E_ts_array_r_df, fillvalue, temp)


}

ts %>% 
  filter(y >= ymd("2018-06-01")) %>% 
  ggplot(aes(y, layer, col=var)) +
  geom_line()+ 
  labs(x="Date", y="Value", title = paste("2018 | Finnmaid | Route E | 57.33-57.5 °N"))+
  theme_bw()+
  ylim(0,30)

ggsave(here::here("Plots/GETM/timeseries", paste("ts_E_BloomSail_2018_summer.jpg",sep = "")),
       width = 13, height = 3, dpi = 300)

ts %>% 
  filter(y < ymd("2018-06-01")) %>% 
  ggplot(aes(y, layer, col=var)) +
  geom_line()+ 
  labs(x="Date", y="Value", title = paste("2018 | Finnmaid | Route E | 57.33-57.5 °N"))+
  theme_bw()

ggsave(here::here("Plots/GETM/timeseries", paste("ts_E_BloomSail_2018_spring.jpg",sep = "")),
       width = 13, height = 3, dpi = 300)
