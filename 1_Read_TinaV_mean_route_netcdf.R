# Packages ----------------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(here)
library(geosphere)


# Read mean route netcdf from Henry Bittig --------------------------------

nc <- nc_open(here::here("Data/_summarized_data_files", "GETM_raster_BloomSail.nc"))
print(nc)

ncatt_get(nc, 0)
ncatt_get(nc, "lat")
ncatt_get(nc, "lon")

attributes(nc)
attributes(nc$var)
attributes(nc$dim)
nc$dim

#var = unlist(attributes(nc$var))[2]

for (var in  unlist(attributes(nc$var))) {
  
  col <- ncvar_get(nc, var)
  
  if (exists("temp")){
  temp <- bind_cols(temp, as_tibble(col))
  } else{temp <- as_tibble(col)}

}

df <- temp %>% 
  set_names(unlist(attributes(nc$var)))

rm(temp)


df <- df %>% 
  mutate(dist_geo = distGeo( cbind(lon, lat), cbind( lag(lon), lag(lat) ) )/1e3)


df %>% 
  filter(!is.na(dist_geo)) %>% 
  ggplot()+
  geom_point(aes(lon,lat, col=dist_geo))+
  scale_color_viridis_c()

