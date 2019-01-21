# Params:

adf.test(drealstock[-1])
wewe <- arima(drealstock[-1],c(2,0,2))
param_mu_ss <- as.data.frame(forecast(wewe, 200))[200,1]
param_sig_ss <- as.data.frame(forecast(wewe, 200))[200,3] - param_mu_ss


adf.test(drealwage[-1])
dfdf <- arima(drealwage[-1], c(5,0,2) )
param_sig_yy <- as.data.frame(forecast(dfdf,200))[200,1] - as.data.frame(forecast(dfdf,200))[200,2]


adf.test(drealhouse[-1], k = 3)
asas <- arima(drealhouse[-1],c(1,0,1))
param_mu_hh <- as.data.frame(forecast(asas, 200))[200,1]
param_sig_hh <- as.data.frame(forecast(asas, 200))[200,3] - param_mu_hh

#common params
param_mu_s <- (1 + param_mu_ss)^12 - 1
param_mu_h <- (1 + param_mu_hh)^12 - 1

param_sig_s <- param_sig_ss*sqrt(12)
param_sig_h <- param_sig_hh*sqrt(12)
param_sig_w <- param_sig_yy*sqrt(12)

param_rho_hs <- 0.27
param_rho_hw <- 0.35

param_beta  <- 0.89
param_r_f <- 0.12 - 0.09
param_inf <- 0.084
param_gamma <- 1.5
param_R <- 65 #retirement age
param_Y <- 25 #life cycle period
param_T <- param_R-param_Y
param_kappa <- 1

#heter params
param_mu_w_flat
param_mu_w_moderate
param_mu_w_steep

param_rho_ws_low <- 0
param_rho_ws_medium <- 0.2
param_rho_ws_high <- 0.4