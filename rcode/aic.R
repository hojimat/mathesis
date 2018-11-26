# ARIMA FORECAST
for(i in 1:6){
  for(j in 1:6){
    erer <- arima(drealstock[-1], order = c(i,0,j))
    cat("ARMA(",i,j,") = ", erer$aic, "\n")
  }
}
