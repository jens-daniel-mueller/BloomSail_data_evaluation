library(data.table)
library(lubridate)
library(tidyverse)


####

setwd("C:/Mueller_Jens_Data/180530_BloomSail/BloomSail_data_evaluation/Data/Tina_V/Bottle/HydroFIA_pH")

HF <- data.table(read.delim("180824_data.txt", sep=",", skip = 2))[,c(1,3,6,7,8,9)]
names(HF) <- c("date", "sample", "Sal", "pH.Mosley.25", "pH.fitpoints", "pH.error")

HF$date <- ymd_hms(HF$date)
HF <- HF[date > ymd("18-07-01")]

HF$transect.ID <- substr(HF$sample, 1,6)
HF$type <- substr(HF$sample, 8,8)
HF$label <- substr(HF$sample, 8,10)
HF$Dep.HF <- as.numeric(substr(HF$sample, 12,13))

HF <- HF[label %in% c("P07", "P10")]
HF <- HF[, .SD[seq(.N-2,.N)], by=.(transect.ID, label, Dep.HF)]

ggplot(data = HF, 
       aes(pH.Mosley.25, Dep.HF, 
           col=transect.ID))+
  geom_path()+
  geom_point()+
  scale_y_reverse()+
  facet_wrap(~label)+
  #xlim(0,27)+
  #scale_color_viridis(discrete = TRUE, direction = -1)+
  scale_color_brewer(palette = "Spectral")+
  labs(x="pH [spec, Mosley, 25C]", y="Depth [m]")




