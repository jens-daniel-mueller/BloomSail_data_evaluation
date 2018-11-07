library(lubridate)

df <- data.frame(
    date = seq(ymd("2018-01-01"), ymd("2018-06-29"), by = "days"),
    flag = rep( c(rep(1,10), rep(0, 20)), 6),
    value = seq(1,180,1)
  )

df