library(seacarb)



####calcualte CT* based on measured pCO2, S, and T, as well as the Alkalinity
#### * in CT indicates that absolute values are somewhat vague, but changes in CT are represented well
#### underlying pCO2 data still require Response time (RT) correction



Sensor[transect.ID == "180616"]$pCO2 <- NA

Sensor$CT.calc <- 0
Sensor$CT.calc <- NA

Sensor[transect.ID != "180616"]$CT.calc <- 
  carb(24, var1=Sensor[transect.ID != "180616"]$pCO2, var2=1670e-6,
       S=Sensor[transect.ID != "180616"]$Sal.S, T=Sensor[transect.ID != "180616"]$Tem.S,
       Patm=1, P=0, Pt=0, Sit=0,
       k1k2="m10", kf="x", ks="d", pHscale="T", b="u74", gas="insitu", 
       warn="y", eos="eos80")[16]*1e6

