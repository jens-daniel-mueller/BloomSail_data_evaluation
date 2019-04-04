#### load required packages ####

library(tidyverse)
library(ncdf4)
library(raster)
library(here)
library(geosphere)


#### Plots section data below Finnmaid track ####

#### read ncf file ####

E_sec <- nc_open(here::here("Data/GETM", "Finnmaid.E.TSage.zax.nc"))
print(E_sec)

lon <- ncvar_get(E_sec, "lonc")
lat <- ncvar_get(E_sec, "latc", verbose = F)
t <- ncvar_get(E_sec, "time")
zax <- ncvar_get(E_sec, "zax")

head(lon) # look at the first few entries in the longitude vector

E_sec_array <- ncvar_get(E_sec, "temp") # store the data in a 3-dimensional array
dim(E_sec_array) # should be 3d with dimensions: 1575 coordinate, 51 depth levels, 31 time steps

fillvalue <- ncatt_get(E_sec, "temp", "_FillValue")
fillvalue

#All done reading in the data. We can close the netCDF file.
nc_close(E_sec) 


#### Working with the data ####

E_sec_array[E_sec_array == fillvalue$value] <- NA

max_col <- max(E_sec_array, na.rm = TRUE)
min_col <- min(E_sec_array, na.rm = TRUE)
# i <- 3
for (i in seq(1,length(t),1)){

E_sec_slice <- E_sec_array[, , i] 
E_sec_r <- raster(t(E_sec_slice), xmn=min(lon), xmx=max(lon), ymn=min(zax), ymx=max(zax))#,
E_sec_r <- flip(E_sec_r, direction='y')

#### Plotting #####
E_sec_r_df <- as.data.frame(E_sec_r, xy=TRUE)

E_sec_r_df %>% 
ggplot() +
  geom_raster(aes(x = x, y = -y, 
                  fill = layer)) + 
  scale_fill_viridis_c(name="SST (°C)",
                       limits = c(min_col,max_col))+
  scale_y_reverse()+
  labs(x="Lon (°E)", y="Depth (m)", title = paste("August",i ,"| Route: E"))+
  coord_cartesian(expand = 0)+
  theme_bw()

ggsave(here::here("Plots/GETM/Sections", paste(i,"_August_2017_Temp_Route_E.jpg",sep = "")),
       width = 7, height = 4, dpi = 150)

rm(E_sec_slice, E_sec_r, E_sec_r_df)
}




#### Plots timeseries of water column parameters (mixing depth, SST ...) vs location ####

#### Read and plot timeseries data ####

E_ts <- nc_open(here::here("Data/GETM", "Finnmaid.E.2d.nc"))
print(E_ts)

#lon <- ncvar_get(E_ts, "lonc")
lat <- ncvar_get(E_ts, "latc", verbose = F)
t <- ncvar_get(E_ts, "time")/(60*60*24)
#SST <- ncvar_get(E_ts, "SST")

head(t) # look at the first few entries in the time vector

#var <- "mld_age_1"

for (var in names(E_ts$var)){

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

E_ts_array_r_df %>% 
  ggplot() +
  geom_raster(aes(y, x, fill = layer)) + 
  scale_fill_viridis_c(name=var)+
  labs(x="Days", y="Lon (°E)", title = paste("August 2017","| Finnmaid Route: E"))+
  theme_bw()+
  coord_cartesian(expand = 0)

ggsave(here::here("Plots/GETM/timeseries", paste(var,"_E_Aug_2017.jpg",sep = "")),
       width = 7, height = 5, dpi = 300)

rm(E_ts_array, E_ts_array_r, E_ts_array_r_df, fillvalue)

}
