# saturated water vapor in mbar

pH2Osat <- function(T,S){
  1013.25*(exp(24.4543-(67.4509*(100./(T+273.15)))-
                 (4.8489*log(((273.15+T)/100)))-0.000544*S))
  }

#scaled temperature for use in TCorr and SCorr

sca_T   <- function(T){
  log((298.15-T)/(273.15+T)) 
  }

#temperature correction part from Garcia and Gordon (1992), 
#Benson and Krause (1984) refit mL(STP) L-1; and conversion from mL(STP) L-1 to umol L-1

TCorr   <- function(sca_T){
  44.6596*exp(2.00907+3.22014*sca_T+4.05010*sca_T^2+
                4.94457*sca_T^3-2.56847e-1*sca_T^4+3.88767*sca_T^5)
  }

#salinity correction part from Garcia and Gordon (1992), Benson and Krause (1984) refit ml(STP) L-1

Scorr   <- function(S, sca_T){
  exp(S*(-6.24523e-3-7.37614e-3*sca_T-1.03410e-2*sca_T^2-8.17083e-3*sca_T^3)
      -4.88682e-7*S^2)
}

#molar volume of O2 in m3 mol-1 Pa dbar-1 (Enns et al. 1965)
Vm     <- 0.317

#universal gas constant in J mol-1 K-1
R     <- 8.314 


O2stoO2c <-
function(O2sat,T,S,P,p_atm){
  O2sat/100*(TCorr(sca_T(T))*Scorr(S, sca_T(T)))*
    (p_atm-pH2Osat(T, S))/(1013.25-pH2Osat(T, S))/exp(Vm*P/(R*(T+273.15)))
}


O2stoO2c(100, 5, 0, 0,  1013.25)



#function O2conc=O2stoO2c(O2sat,T,S,P,p_atm)
#
# convert oxygen saturation to molar oxygen concentration
#
# inputs:
#   O2sat  - oxygen saturation in #
#   T      - temperature in °C
#   S      - salinity (PSS-78)
#   P      - hydrostatic pressure in dbar (default: 0 dbar)
#   p_atm  - atmospheric (air) pressure in mbar (default: 1013.25 mbar)
#
# output:
#   O2conc - oxygen concentration in umol L-1
#
# according to recommendations by SCOR WG 142 "Quality Control Procedures
# for Oxygen and Other Biogeochemical Sensors on Floats and Gliders"
#
# Henry Bittig
# Laboratoire d'Océanographie de Villefranche-sur-Mer, France
# bittig@obs-vlfr.fr
# 28.10.2015
# 19.04.2018, v1.1, fixed typo in B2 exponent
# 25.03.2019, translated from Matlab to R by Jens Daniel Müller, IOW (mail: jens.mueller@io-warnemuende.de)