library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)

setwd("C:/Mueller_Jens_Data/180530_BloomSail/Data/Sensor/HydroC-pCO2")
files <- list.files(pattern = "*.txt")

df <- files %>%
  map(read.delim, sep=";", dec=",", skip=4, col.names = seq(1,23,1),
      colClasses = c(rep("character",2), rep("numeric", 21))) %>%
  reduce(rbind) %>% 
  select(seq(1,22,1)) %>% 
  setDT()

rm(files)
  
names(df) <- c("Date",	"Time",	"Weekday",	"P_pump",
           "p_NDIR",	"p_in",	"I_total",	"U_supply",
          "Zero",	"Flush", "external_pump",	"Runtime",
          "Signal_raw",	"Signal_ref",	"T_sensor",
          "Signal_proc",	"Conc_estimate",	"pCO2_corr",
          "xCO2_corr",	"T_control",	"T_gas",	"%rH_gas")

df$date.time <- ymd_hms(paste(df$Date, df$Time))


#### Subset and plot Zeroing data

df[, Zero_id := rleid(Zero)]

Zero.all <- df %>% 
  filter(Zero == 1)

Zero <- df %>% 
  filter(Zero == 1) %>% 
  group_by(Zero_id) %>%
  slice(50:n())


Fig <- 
ggplot(Zero.all, aes(date.time, pCO2_corr))+
  geom_point(alpha=0.2)+
  ylim(0,40)

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("HydroC_pCO2_zeroing_BloomSail_alpha.tiff", width = 500, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <- 
ggplot()+
  geom_point(data=Zero.all, aes(date.time, pCO2_corr, col="All"))+
  geom_point(data=Zero, aes(date.time, pCO2_corr, col="-50 first rows"))+
  ylim(0,40)+
  scale_color_brewer(palette = "Set1", name="Zeroing subset")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("HydroC_pCO2_zeroing_BloomSail_removed.tiff", width = 500, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <- 
ggplot()+
  geom_point(data=Zero.all, aes(date.time, pCO2_corr, col="All"))+
  geom_point(data=Zero, aes(date.time, pCO2_corr, col="-50 first rows"))+
  ylim(0,40)+
  scale_color_brewer(palette = "Set1", name="Zeroing subset")+
  facet_wrap(~Zero_id, scales = "free_x")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("HydroC_pCO2_zeroing_BloomSail_individual_zeroing.tiff", width = 800, height = 800, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



#### Subset and plot Flush data


df[, Flush_id := rleid(Flush)]

Flush.all <- df %>% 
  filter(Flush == 1) %>% 
  mutate(date.time.sec = as.numeric(date.time))
 
Flush <- Flush.all %>%
  filter(Flush == 1 & !Flush_id %in% c(2,4,10,12,18,20,44,64,74,90,102,206)) %>%
  group_by(Flush_id) %>%
  slice(20:200) %>%
  mutate(sec = date.time.sec - first(date.time.sec)) %>% 
  ungroup()

# library(tidyr)
# 
# Flush.stats <- Flush %>%
#   group_by(Flush_id) %>%
#   do(fit = nls(pCO2_corr ~ SSasymp(Date, yf, y0, log_alpha), data = .)) %>% 
#   tidy(fit) %>% 
#   ungroup()


Flush.one <- Flush %>% 
  filter(Flush_id == 98)



ggplot(Flush.one, aes(sec, pCO2_corr))+
  geom_point()


library(minpack.lm)
curve.nlslrc = nlsLM(pCO2_corr ~ Am*(1-((1-(Rd/Am))^(1-(date.time.sec/LCP)))),
                     start=list(Am=(max(photolrc)-min(photolrc)),
                                Rd=-min(photolrc),
                                LCP= (max(photolrc)-1)),
                     data = Flush.one)
coef(curve.nlslrc)
#      Am         Rd        LCP 
#8.011311   1.087484 -20.752957

plot(photolrc ~ PARlrc, data = curvelrc)
lines(0:1300, 
      predict(curve.nlslrc, 
              newdata = data.frame(PARlrc = 0:1300)))



nls(pCO2_corr ~ yf + (y0 - yf) * exp(-alpha * sec), 
    data = Flush.one)

fit <- nls(pCO2_corr ~ SSasymp(sec, yf, y0, log_alpha), data = Flush.one)
fit


qplot(sec, pCO2_corr, data = augment(fit)) + geom_line(aes(pCO2_corr = .fitted))


df %>% 
  group_by(sensor) %>% 
  do(fit = nls(y ~ SSasymp(t, yf, y0, log_alpha), data = .)) %>% 
  tidy(fit) %>% 
  select(sensor, term, estimate) %>% 
  spread(term, estimate) %>% 
  mutate(alpha = exp(log_alpha))






Flush.one %>% 
do(tidy(lm(pCO2_corr ~ date.time)))

lm(Flush.one$pCO2_corr ~ Flush.one$date.time)

as.numeric(Flush.one$date.time)

nls(pCO2_corr ~ yf + (y0 - yf) * exp(-alpha * Date), 
    data = Flush.one,
    start = list(y0 = 70, yf = 110, alpha = 1))



Fig <- 
  ggplot()+
  geom_point(data = Flush.all, aes(date, pCO2_corr, col="All"), alpha=0.2)+
  geom_point(data = Flush, aes(date, pCO2_corr, col="20-200"), alpha=0.2)+
  #geom_smooth(data = Flush, aes(date, pCO2_corr, col="20-200"), method = "lm", formula = y ~ log(x))+
  scale_color_brewer(palette = "Set1", name="Flush subset")+
  facet_wrap(~Flush_id, scales = "free")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("HydroC_pCO2_zeroing_BloomSail_individual_Flush.tiff", width = 800, height = 800, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <- 
  ggplot()+
  geom_point(data = Flush, aes(sec, pCO2_corr, col=as.factor(Flush_id)), alpha=0.2)+
  scale_color_viridis_d(name="Flush ID")

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("HydroC_pCO2_zeroing_BloomSail_individual_Flush_overleigth.tiff", width = 300, height = 300, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

