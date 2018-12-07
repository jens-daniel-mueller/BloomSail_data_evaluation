library(ggplot2)
library(tidyverse)
library(data.table)
library(lubridate)


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Merged_data_files")
Sensor <- data.table(read.csv("BloomSail_Sensor_Track_data.csv"))

Sensor$date <- ymd_hms(Sensor$date)
Sensor$start.date <- ymd_hms(Sensor$start.date)

# # safe data subset for Jugend forscht project of Ben Ole Grabler
# df.tem <-
#   Sensor %>% 
#   filter(label %in% c("P07", "P10")) %>% 
#   select(date, lat, lon, Dep.S, Tem.S, Sal.S, start.date, label, cast, transect.ID) %>% 
#   rename(sal = Sal.S, dep = Dep.S, tem = Tem.S)
# 
# write.csv(df.tem, "BloomSail_Sensor_P07P10_TemSalDep.csv", row.names = FALSE)
# rm(df.tem)
# 
# df.route <-
#   Sensor %>% 
#   select(date, lat, lon, type, label, cast, transect.ID)
# 
# write.csv(df.route, "BloomSail_route.csv", row.names = FALSE)
# rm(df.route)


Fig <-
  ggplot()+
  geom_point(data = temp[Area == "BS"], aes(date, pCO2, col="Finnmaid"))+
  geom_point(data = df[Dep.S < 4 & Dep.S > 2 & 
                         cast == "down" &
                         label %in% c("P10", "P04", "P02", "P12")], aes(date, pCO2, col="Tina V"))+
  scale_color_brewer(palette = "Set1", name = "platform")+
  ylim(60, 200)



setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180821_Surface_pCO2_Finnmaid-TinaV.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
  ggplot()+
  geom_point(data = temp[Area == "BS"], aes(date, Tem))+
  geom_point(data = df[Dep.S < 6 & 
                         cast == "down" &
                         label %in% c("P10", "P04", "P02", "P12")], aes(date, Tem.S, col=Dep.S))+
  scale_color_viridis()


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180821_Surface_Tem2_Finnmaid-TinaV.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
  ggplot()+
  geom_point(data = temp[Area == "BS"], aes(date, Tem, col="Finnmaid"))+
  geom_point(data = df[Dep.S < 4 & Dep.S > 2 & 
                         cast == "down" &
                         label %in% c("P10", "P04", "P02", "P12")], aes(date, Tem.S, col="Tina V"))+
  scale_color_brewer(palette = "Set1", name = "platform")



setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180821_Surface_Tem_Finnmaid-TinaV.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
  ggplot()+
  geom_point(data = temp[Area == "BS"], aes(date, Sal, col="Finnmaid"))+
  geom_point(data = df[Dep.S < 4 & Dep.S > 2 & 
                         cast == "down" &
                         label %in% c("P10", "P04", "P02", "P12")], aes(date, Sal.S, col="Tina V"))+
  scale_color_brewer(palette = "Set1", name = "platform")



setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180821_Surface_Sal_Finnmaid-TinaV.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
  ggplot()+
  geom_path(data = Sensor[type=="P" & cast == "down"], aes(Sal.S, Dep.S, col=transect.ID))+
  scale_y_reverse()+
  facet_wrap(~label)+
  scale_x_continuous(limits = c(6.5,7.5), breaks = seq(0,35,0.5))+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  labs(x="Salinity", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Profiles_Salinity.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
  ggplot()+
  geom_line(data = Sensor[type=="P" & cast == "down"], aes(Tem.S, Dep.S, col=transect.ID))+
  scale_y_reverse()+
  facet_wrap(~label)+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  xlim(0,27)+
  labs(x="Tem [?C]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Profiles_Temperature.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
  ggplot()+
  geom_path(data = Sensor[type=="P" & cast == "down"], aes(O2, Dep.S, linetype=cast, col=transect.ID))+
  #geom_path(data = Sensor[type=="P"], aes(Sal.S, Dep.S, linetype=cast, col="Salinity"))+
  scale_y_reverse()+
  facet_wrap(~label)+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  #xlim(8,22)+
  labs(x="O2 [% sat]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Profiles_O2.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
  ggplot()+
  geom_path(data = Sensor[type=="P" & cast == "down"], aes(pH, Dep.S, col=transect.ID))+
  #geom_path(data = Sensor[type=="P"], aes(Sal.S, Dep.S, linetype=cast, col="Salinity"))+
  scale_y_reverse()+
  facet_wrap(~label)+
  xlim(8.5,9.3)+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  labs(x="pH [not yet calibrated]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Profiles_pH.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
  ggplot()+
  geom_path(data = Sensor[type=="P" & cast == "down"], aes(Chl, Dep.S, linetype=cast, col=transect.ID))+
  #geom_path(data = Sensor[type=="P"], aes(Sal.S, Dep.S, linetype=cast, col="Salinity"))+
  scale_y_reverse()+
  facet_wrap(~label)+
  xlim(0,1.6)+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  labs(x="Chl [not yet calibrated]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Profiles_Chl.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
  ggplot()+
  geom_vline(xintercept = c(100, 200, 400))+
  geom_hline(yintercept = c(20))+
  geom_path(data = Sensor[type=="P" & cast == "down"], aes(pCO2, Dep.S, linetype=cast, col=as.factor(transect.ID)))+
  #geom_path(data = Sensor[type=="P"], aes(Sal.S, Dep.S, linetype=cast, col="Salinity"))+
  scale_y_reverse()+
  facet_wrap(~label)+
  #xlim(0,1.6)+
  scale_color_viridis_d(direction = -1)+
  labs(x="pCO2 [µatm, not yet RT corrected]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./Profiles_pCO2.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)





Fig <-
  ggplot()+
  geom_path(data = Sensor[type=="P" & cast =="down"], aes(CT.calc, Dep.S, linetype=cast, col=transect.ID))+
  #geom_path(data = Sensor[type=="P"], aes(Sal.S, Dep.S, linetype=cast, col="Salinity"))+
  scale_y_reverse()+
  facet_wrap(~label)+
  xlim(1400,1600)+
  scale_color_viridis(discrete = TRUE, direction = -1)+
  labs(x="CT* [?mol/kg, from pCO2 downcast and AT=1670?M]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Profiles_CT_downcast.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
  ggplot()+
  geom_path(data = Sensor[type=="P" & cast =="down" & transect.ID != "180616" & Dep.S > 1.5], 
            aes(CT.calc, Dep.S, linetype=cast, group=interaction(label,transect.ID), col=transect.ID))+
  #geom_path(data = Sensor[type=="P"], aes(Sal.S, Dep.S, linetype=cast, col="Salinity"))+
  scale_y_reverse()+
  #facet_wrap(~label)+
  xlim(1400,1700)+
  #scale_color_viridis(discrete = TRUE, direction = -1)+
  scale_color_brewer(palette = "Spectral")+
  labs(x="CT* [?mol/kg, from pCO2 downcast and AT=1670?M]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Profiles_CT_downcast_date.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
  ggplot()+
  geom_vline(xintercept = c(100,400))+
  geom_path(data = Sensor[type=="P" & cast =="down" & transect.ID != "180616" & Dep.S > 1.5], 
            aes(pCO2, Dep.S, linetype=cast, group=interaction(label,transect.ID), col=as.factor(transect.ID)))+
  scale_y_reverse()+
  scale_color_brewer(palette = "Spectral", name="Date")+
  xlim(50, 500)+
  labs(x="pCO2 [µatm]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./181128_Profiles_pCO2_downcast_date.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
  ggplot()+
  geom_path(data = Sensor[type=="P" & cast =="down" & transect.ID != "180616" & Dep.S > 1.5], 
            aes(CT.calc, Dep.S, linetype=cast, group=interaction(label,transect.ID), col=as.factor(transect.ID)))+
  scale_y_reverse()+
  scale_color_brewer(palette = "Spectral", name="Date")+
  xlim(1400, 1700)+
  labs(x="CT* [µmol/kg, from pCO2 downcast and AT=1670 µM]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./181128_Profiles_CT_downcast_date.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)




Fig <-
  ggplot()+
  geom_path(data = Sensor[type=="P" & cast =="down" & transect.ID != "180616" & Dep.S > 1.5], 
            aes(Tem.S, Dep.S, linetype=cast, group=interaction(label,transect.ID), col=transect.ID))+
  #geom_path(data = Sensor[type=="P"], aes(Sal.S, Dep.S, linetype=cast, col="Salinity"))+
  scale_y_reverse()+
  #facet_wrap(~label)+
  xlim(0,27)+
  #scale_color_viridis(discrete = TRUE, direction = -1)+
  scale_color_brewer(palette = "Spectral")+
  labs(x="Tem [?C]", y="Depth [m]")


setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/plots")
tiff("./180818_Profiles_Tem_downcast_date.tiff", width = 180, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

