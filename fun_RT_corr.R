



RT_corr <- function(c1, c0, dt, tau) {
  ( 1 / ( 2* (( 1+(2*tau/dt) )^(-1) ))) * (c1 - (1-(2* (( 1+(2*tau/dt) )^(-1) ))) * c0)
}



window <- 15
rolling_mean   <- rollify(~mean(.x, na.rm = TRUE), window = window)
rolling_median <- rollify(~median(.x, na.rm = TRUE), window = window)


pCO2_RT = RT_corr(pCO2, lag(pCO2), dt, tau),
pCO2_RT = if_else(pCO2_RT %in% c(Inf, -Inf), NaN, pCO2_RT)
pCO2_RT_mean = rolling_mean(pCO2_RT)
pCO2_RT_median = rolling_median(pCO2_RT)

# time shift RT corrected data
shift <- as.integer(as.character(window/2))

pCO2_RT_mean = lead(pCO2_RT_mean, shift),
         pCO2_RT_median = lead(pCO2_RT_median, shift))

rm(rolling_median, rolling_mean, shift, tau_high, tau_low, window, RT_corr)

df <- full_join(df_full, df)
rm(df_full)
