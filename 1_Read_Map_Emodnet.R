library(data.table)
library(tidyverse)

setwd("C:/Mueller_Jens_Data/Sea_charts/EMODnet/D6_2018.xyz")
map <- read.delim("D6_2018.xyz", sep = ";", header = FALSE)
map <- map[,-4]
map <- data.table(map)
names(map) <- c("lon", "lat", "elev")

df.map <-
  map %>%
  filter(lat > 56.8 & lat < 58 & lon > 18 & lon < 20.4 & elev <= 0) %>% 
  complete(lon, lat)

ggplot(df.map, aes(lon, lat, fill=elev))+
  geom_raster()+
  scale_fill_gradient(low = "grey20", high = "grey80", na.value = "black", name="Depth [m]")+
  coord_quickmap(expand = 0)+
  labs(x="Longitude [°E]", y="Latitude [°N]")+
  theme_bw()


write.csv(df.map, "Bathymetry_Gotland_east.csv", row.names = FALSE)
