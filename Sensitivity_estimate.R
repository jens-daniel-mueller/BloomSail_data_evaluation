library(seacarb)
library(data.table)
library(tidyverse)


df <- data.frame(cbind(
  (c(1720)),
  (c(7))))

Tem <- seq(5,25,5)
pCO2<-seq(50,500,5)

df<-merge(df, Tem)
names(df) <- c("AT", "S", "Tem")  

df<-merge(df, pCO2)
names(df) <- c("AT", "S", "Tem", "pCO2")  

df<-data.table(df)
df$AT<-df$AT*1e-6

df$DIC<-carb(flag=24, var1=df$pCO2, var2=df$AT, S=df$S, T=df$Tem, k1k2="m10", kf="dg", pHscale="T")[,16]
df$pCO2.corr<-carb(flag=15, var1=df$AT, var2=df$DIC, S=df$S, T=df$Tem, k1k2="m10", kf="dg", pHscale="T")[,9]

df$pCO2.2<-df$pCO2.corr + 25
df$DIC.2<-carb(flag=24, var1=df$pCO2.2, var2=df$AT, S=df$S, T=df$Tem, k1k2="m10", kf="dg", pHscale="T")[,16]


df$ratio<-(df$pCO2.2-df$pCO2.corr)/(df$DIC.2*1e6-df$DIC*1e6)

Fig <-
  ggplot(df, aes(pCO2, ratio, col=as.factor(Tem)))+
  geom_line()+
  scale_color_viridis_d(option = "C",name="Tem [°C]")+
  labs(x=expression(pCO[2]~(µatm)), y=expression(Delta~pCO[2]~"/"~Delta~DIC~(µatm~µmol^{-1}~kg)))+
  scale_y_continuous(limits = c(0,8), breaks = seq(0,10,1))

setwd("C:/Mueller_Jens_Data/180530_BloomSail/plots")
tiff("pCO2_DIC_sensitivity.tiff", width = 130, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

