# house price
set.seed(1)
HH <- rep(0,param_T); HH[1] <- 200000
HH[2:30] <- 1+rnorm(29,param_mu_h,param_sig_h)
HH <- cumprod(HH)
plot(c(28:57),HH)


# Munk solution with housing
hc <- data.frame("init" = rep(0,param_T))


hc$munkhouse_steep_low <- rep(0,param_T)
hc$munkhouse_steep_mod <- rep(0,param_T)
hc$munkhouse_steep_hi <- rep(0,param_T)

hc$munkhouse_moderate_low <- rep(0,param_T)
hc$munkhouse_moderate_mod <- rep(0,param_T)
hc$munkhouse_moderate_hi <- rep(0,param_T)

hc$munkhouse_flat_low <- rep(0,param_T)
hc$munkhouse_flat_mod <- rep(0,param_T)
hc$munkhouse_flat_hi <- rep(0,param_T)


fc$munkhouse_steep_low <- rep(0,param_T)
fc$munkhouse_steep_low[1] <- LL_steep[1]*0.03
fc$munkhouse_steep_mod <- rep(0,param_T)
fc$munkhouse_steep_mod[1] <- LL_steep[1]*0.03
fc$munkhouse_steep_hi <- rep(0,param_T)
fc$munkhouse_steep_hi[1] <- LL_steep[1]*0.03

fc$munkhouse_moderate_low <- rep(0,param_T)
fc$munkhouse_moderate_low[1] <- LL_moderate[1]*0.03
fc$munkhouse_moderate_mod <- rep(0,param_T)
fc$munkhouse_moderate_mod[1] <- LL_moderate[1]*0.03
fc$munkhouse_moderate_hi <- rep(0,param_T)
fc$munkhouse_moderate_hi[1] <- LL_moderate[1]*0.03

fc$munkhouse_flat_low <- rep(0,param_T)
fc$munkhouse_flat_low[1] <- LL_flat[1]*0.03
fc$munkhouse_flat_mod <- rep(0,param_T)
fc$munkhouse_flat_mod[1] <- LL_flat[1]*0.03
fc$munkhouse_flat_hi <- rep(0,param_T)
fc$munkhouse_flat_hi[1] <- LL_flat[1]*0.03

#individualized interest rates

munkh_f_h <- rep(0,param_T-1)
munkh_f_l <- rep(0,param_T-1)
munkh_f_m <- rep(0,param_T-1)

munkh_m_h <- rep(0,param_T-1)
munkh_m_l <- rep(0,param_T-1)
munkh_m_m <- rep(0,param_T-1)

munkh_s_h <- rep(0,param_T-1)
munkh_s_l <- rep(0,param_T-1)
munkh_s_m <- rep(0,param_T-1)




#dynamic portfolios
for(i in 2:param_T){
  #calculate optimal risky asset share
  munkh_f_h[i-1] <- min(munk(t = i-1, dwi =  L_flat, dfi =  fc$munkhouse_flat_hi, rhowsi =  param_rho_ws_high,rhowhi =  param_rho_hw,house = TRUE)$pis,1)
  munkh_f_l[i-1] <- min(munk(t = i-1, dwi =  L_flat, dfi = fc$munkhouse_flat_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house = TRUE)$pis,1)
  munkh_f_m[i-1] <- min(munk(t = i-1, dwi =  L_flat, dfi = fc$munkhouse_flat_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house = TRUE)$pis,1)
  
  munkh_m_h[i-1] <- min(munk(t = i-1, dwi =  L_moderate, dfi = fc$munkhouse_moderate_hi, rhowsi = param_rho_ws_high, rhowhi = param_rho_hw,house = TRUE)$pis,1)
  munkh_m_l[i-1] <- min(munk(t = i-1, dwi = L_moderate, dfi = fc$munkhouse_moderate_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,TRUE)$pis,1)
  munkh_m_m[i-1] <- min(munk(t = i-1, dwi = L_moderate, dfi = fc$munkhouse_moderate_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,TRUE)$pis,1)
  
  munkh_s_h[i-1] <- min(munk(t = i-1, dwi = L_steep, dfi = fc$munkhouse_steep_hi, rhowsi = param_rho_ws_high, rhowhi = param_rho_hw,house = TRUE)$pis,1)
  munkh_s_l[i-1] <- min(munk(t = i-1, dwi = L_steep, dfi = fc$munkhouse_steep_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house = TRUE)$pis,1)
  munkh_s_m[i-1] <- min(munk(t = i-1, dwi = L_steep, dfi = fc$munkhouse_steep_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house = TRUE)$pis,1)
  
  
  
  #law of motion
  
  if(hc$munkhouse_flat_hi[i-1] < 200000){
    hc$munkhouse_flat_hi[i] <- (hc$munkhouse_flat_hi[i-1] + LL_flat[i-1]*0.03)*(1+param_mu_h)
  }else{
    fc$munkhouse_flat_hi[i] <- (fc$munkhouse_flat_hi[i-1] + LL_flat[i-1]*0.03)*(1 + param_mu_s*munkh_f_h[i-1] + param_r_f*(1-munkh_f_h[i-1]))
  }
  
  if(hc$munkhouse_flat_mod[i-1] < 200000){
    hc$munkhouse_flat_mod[i] <- (hc$munkhouse_flat_mod[i-1] + LL_flat[i-1]*0.03)*(1+param_mu_h)
  }else{
    fc$munkhouse_flat_mod[i] <- (fc$munkhouse_flat_mod[i-1] + LL_flat[i-1]*0.03)*(1 + param_mu_s*munkh_f_m[i-1] + param_r_f*(1-munkh_f_m[i-1]))
  }
  
  if(hc$munkhouse_flat_low[i-1] < 200000){
    hc$munkhouse_flat_low[i] <- (hc$munkhouse_flat_low[i-1] + LL_flat[i-1]*0.03)*(1+param_mu_h)
  }else{
    fc$munkhouse_flat_low[i] <- (fc$munkhouse_flat_low[i-1] + LL_flat[i-1]*0.03)*(1 + param_mu_s*munkh_f_l[i-1] + param_r_f*(1-munkh_f_l[i-1]))
  }
  
  if(hc$munkhouse_moderate_hi[i-1] < 200000){
    hc$munkhouse_moderate_hi[i] <- (hc$munkhouse_moderate_hi[i-1] + LL_moderate[i-1]*0.03)*(1+param_mu_h)
  }else{
    fc$munkhouse_moderate_hi[i] <- (fc$munkhouse_moderate_hi[i-1] + LL_moderate[i-1]*0.03)*(1 + param_mu_s*munkh_m_h[i-1] + param_r_f*(1-munkh_m_h[i-1]))
  }
  
  if(hc$munkhouse_moderate_mod[i-1] < 200000){
    hc$munkhouse_moderate_mod[i] <- (hc$munkhouse_moderate_mod[i-1] + LL_moderate[i-1]*0.03)*(1+param_mu_h)
  }else{
    fc$munkhouse_moderate_mod[i] <- (fc$munkhouse_moderate_mod[i-1] + LL_moderate[i-1]*0.03)*(1 + param_mu_s*munkh_m_m[i-1] + param_r_f*(1-munkh_m_m[i-1]))
  }
  
  if(hc$munkhouse_moderate_low[i-1] < 200000){
    hc$munkhouse_moderate_low[i] <- (hc$munkhouse_moderate_low[i-1] + LL_moderate[i-1]*0.03)*(1+param_mu_h)
  }else{
    fc$munkhouse_moderate_low[i] <- (fc$munkhouse_moderate_low[i-1] + LL_moderate[i-1]*0.03)*(1 + param_mu_s*munkh_m_l[i-1] + param_r_f*(1-munkh_m_l[i-1]))
  }
  
  if(hc$munkhouse_steep_hi[i-1] < 200000){
    hc$munkhouse_steep_hi[i] <- (hc$munkhouse_steep_hi[i-1] + LL_steep[i-1]*0.03)*(1+param_mu_h)
  }else{
    fc$munkhouse_steep_hi[i] <- (fc$munkhouse_steep_hi[i-1] + LL_steep[i-1]*0.03)*(1 + param_mu_s*munkh_s_h[i-1] + param_r_f*(1-munkh_s_h[i-1]))
  }
  
  if(hc$munkhouse_steep_mod[i-1] < 200000){
    hc$munkhouse_steep_mod[i] <- (hc$munkhouse_steep_mod[i-1] + LL_steep[i-1]*0.03)*(1+param_mu_h)
  }else{
    fc$munkhouse_steep_mod[i] <- (fc$munkhouse_steep_mod[i-1] + LL_steep[i-1]*0.03)*(1 + param_mu_s*munkh_s_m[i-1] + param_r_f*(1-munkh_s_m[i-1]))
  }
  
  if(hc$munkhouse_steep_low[i-1] < 200000){
    hc$munkhouse_steep_low[i] <- (hc$munkhouse_steep_low[i-1] + LL_steep[i-1]*0.03)*(1+param_mu_h)
  }else{
    fc$munkhouse_steep_low[i] <- (fc$munkhouse_steep_low[i-1] + LL_steep[i-1]*0.03)*(1 + param_mu_s*munkh_s_l[i-1] + param_r_f*(1-munkh_s_l[i-1]))
  }
  
}