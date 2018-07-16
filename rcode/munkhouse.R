# house price
#set.seed(1)
#HH <- rep(0,param_T); HH[1] <- 200000
#HH[2:30] <- 1+rnorm(29,param_house[i-1],param_sig_h)
#HH <- cumprod(HH)
#plot(c(28:57),HH)


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

smunkh_f_h <- rep(0,param_T-1)
smunkh_f_l <- rep(0,param_T-1)
smunkh_f_m <- rep(0,param_T-1)

smunkh_m_h <- rep(0,param_T-1)
smunkh_m_l <- rep(0,param_T-1)
smunkh_m_m <- rep(0,param_T-1)

smunkh_s_h <- rep(0,param_T-1)
smunkh_s_l <- rep(0,param_T-1)
smunkh_s_m <- rep(0,param_T-1)

hmunkh_f_h <- rep(0,param_T-1)
hmunkh_f_l <- rep(0,param_T-1)
hmunkh_f_m <- rep(0,param_T-1)

hmunkh_m_h <- rep(0,param_T-1)
hmunkh_m_l <- rep(0,param_T-1)
hmunkh_m_m <- rep(0,param_T-1)

hmunkh_s_h <- rep(0,param_T-1)
hmunkh_s_l <- rep(0,param_T-1)
hmunkh_s_m <- rep(0,param_T-1)

gammas <- data.frame(init=rep(0,29))
#dynamic portfolios
for(i in 2:param_T){
  #calculate optimal risky asset share
  
  smunkh_f_h[i-1] <- munk(t = i-1, dwi = L_flat, dfi = fc$munkhouse_flat_hi, rhowsi =  param_rho_ws_high,rhowhi =  param_rho_hw,house = TRUE)$pis
  smunkh_f_l[i-1] <- munk(t = i-1, dwi = L_flat, dfi = fc$munkhouse_flat_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house = TRUE)$pis
  smunkh_f_m[i-1] <- munk(t = i-1, dwi = L_flat, dfi = fc$munkhouse_flat_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house = TRUE)$pis
  smunkh_m_h[i-1] <- munk(t = i-1, dwi = L_moderate, dfi = fc$munkhouse_moderate_hi, rhowsi = param_rho_ws_high, rhowhi = param_rho_hw,house = TRUE)$pis  
  smunkh_m_l[i-1] <- munk(t = i-1, dwi = L_moderate, dfi = fc$munkhouse_moderate_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house=TRUE)$pis
  smunkh_m_m[i-1] <- munk(t = i-1, dwi = L_moderate, dfi = fc$munkhouse_moderate_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house=TRUE)$pis
  smunkh_s_h[i-1] <- munk(t = i-1, dwi = L_steep, dfi = fc$munkhouse_steep_hi, rhowsi = param_rho_ws_high, rhowhi = param_rho_hw,house = TRUE)$pis
  smunkh_s_l[i-1] <- munk(t = i-1, dwi = L_steep, dfi = fc$munkhouse_steep_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house = TRUE)$pis
  smunkh_s_m[i-1] <- munk(t = i-1, dwi = L_steep, dfi = fc$munkhouse_steep_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house = TRUE)$pis
  
  hmunkh_f_h[i-1] <- munk(t = i-1, dwi = L_flat, dfi = fc$munkhouse_flat_hi, rhowsi =  param_rho_ws_high,rhowhi =  param_rho_hw,house = TRUE)$pih
  hmunkh_f_l[i-1] <- munk(t = i-1, dwi = L_flat, dfi = fc$munkhouse_flat_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house = TRUE)$pih
  hmunkh_f_m[i-1] <- munk(t = i-1, dwi = L_flat, dfi = fc$munkhouse_flat_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house = TRUE)$pih
  hmunkh_m_h[i-1] <- munk(t = i-1, dwi = L_moderate, dfi = fc$munkhouse_moderate_hi, rhowsi = param_rho_ws_high, rhowhi = param_rho_hw,house = TRUE)$pih
  hmunkh_m_l[i-1] <- munk(t = i-1, dwi = L_moderate, dfi = fc$munkhouse_moderate_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house=TRUE)$pih
  hmunkh_m_m[i-1] <- munk(t = i-1, dwi = L_moderate, dfi = fc$munkhouse_moderate_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house=TRUE)$pih
  hmunkh_s_h[i-1] <- munk(t = i-1, dwi = L_steep, dfi = fc$munkhouse_steep_hi, rhowsi = param_rho_ws_high, rhowhi = param_rho_hw,house = TRUE)$pih
  hmunkh_s_l[i-1] <- munk(t = i-1, dwi = L_steep, dfi = fc$munkhouse_steep_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house = TRUE)$pih
  hmunkh_s_m[i-1] <- munk(t = i-1, dwi = L_steep, dfi = fc$munkhouse_steep_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house = TRUE)$pih
  
    
  if(smunkh_f_h[i-1] + hmunkh_f_h[i-1] > 1){
    gammas$smunkh_f_h[i-1] <- smunkh_f_h[i-1] / (smunkh_f_h[i-1] + hmunkh_f_h[i-1])
    gammas$hmunkh_f_h[i-1] <- hmunkh_f_h[i-1] / (smunkh_f_h[i-1] + hmunkh_f_h[i-1])
    fc$munkhouse_flat_hi[i] <- (fc$munkhouse_flat_hi[i-1] + LL_flat[i-1]*0.03)*(1 + param_stock[i-1]*gammas$smunkh_f_h[i-1] + param_house[i-1]*gammas$hmunkh_f_h[i-1])
  }else{
    fc$munkhouse_flat_hi[i] <- (fc$munkhouse_flat_hi[i-1] + LL_flat[i-1]*0.03)*(1 + param_stock[i-1]*smunkh_f_h[i-1] + param_house[i-1]*hmunkh_f_h[i-1] + param_r_f*(1-smunkh_f_h[i-1] - hmunkh_f_h[i-1]))    
    gammas$smunkh_f_h[i-1] <- smunkh_f_h[i-1]
    gammas$hmunkh_f_h[i-1] <- hmunkh_f_h[i-1]
  }
  

  if(smunkh_f_l[i-1] + hmunkh_f_l[i-1] > 1){
    gammas$smunkh_f_l[i-1] <- smunkh_f_l[i-1] / (smunkh_f_l[i-1] + hmunkh_f_l[i-1])
    gammas$hmunkh_f_l[i-1] <- hmunkh_f_l[i-1] / (smunkh_f_l[i-1] + hmunkh_f_l[i-1])
    fc$munkhouse_flat_low[i] <- (fc$munkhouse_flat_low[i-1] + LL_flat[i-1]*0.03)*(1 + param_stock[i-1]*gammas$smunkh_f_l[i-1] + param_house[i-1]*gammas$hmunkh_f_l[i-1])
  }else{
    fc$munkhouse_flat_low[i] <- (fc$munkhouse_flat_low[i-1] + LL_flat[i-1]*0.03)*(1 + param_stock[i-1]*smunkh_f_l[i-1] + param_house[i-1]*hmunkh_f_l[i-1] + param_r_f*(1-smunkh_f_l[i-1] - hmunkh_f_l[i-1]))    
    gammas$smunkh_f_l[i-1] <- smunkh_f_l[i-1]
    gammas$hmunkh_f_l[i-1] <- hmunkh_f_l[i-1]
  }
  
  

  if(smunkh_f_m[i-1] + hmunkh_f_m[i-1] > 1){
    gammas$smunkh_f_m[i-1] <- smunkh_f_m[i-1] / (smunkh_f_m[i-1] + hmunkh_f_m[i-1])
    gammas$hmunkh_f_m[i-1] <- hmunkh_f_h[i-1] / (smunkh_f_m[i-1] + hmunkh_f_m[i-1])
    fc$munkhouse_flat_mod[i] <- (fc$munkhouse_flat_mod[i-1] + LL_flat[i-1]*0.03)*(1 + param_stock[i-1]*gammas$smunkh_f_m[i-1] + param_house[i-1]*gammas$hmunkh_f_m[i-1])
  }else{
    fc$munkhouse_flat_mod[i] <- (fc$munkhouse_flat_mod[i-1] + LL_flat[i-1]*0.03)*(1 + param_stock[i-1]*smunkh_f_m[i-1] + param_house[i-1]*hmunkh_f_m[i-1] + param_r_f*(1-smunkh_f_m[i-1] - hmunkh_f_m[i-1]))    
    gammas$smunkh_f_m[i-1] <- smunkh_f_m[i-1]
    gammas$hmunkh_f_m[i-1] <- hmunkh_f_m[i-1]
  }
  

  if(smunkh_m_h[i-1] + hmunkh_m_h[i-1] > 1){
    gammas$smunkh_m_h[i-1] <- smunkh_m_h[i-1] / (smunkh_m_h[i-1] + hmunkh_m_h[i-1])
    gammas$hmunkh_m_h[i-1] <- hmunkh_m_h[i-1] / (smunkh_m_h[i-1] + hmunkh_m_h[i-1])
    fc$munkhouse_moderate_hi[i] <- (fc$munkhouse_moderate_hi[i-1] + LL_moderate[i-1]*0.03)*(1 + param_stock[i-1]*gammas$smunkh_m_h[i-1] + param_house[i-1]*gammas$hmunkh_m_h[i-1])
  }else{
    fc$munkhouse_moderate_hi[i] <- (fc$munkhouse_moderate_hi[i-1] + LL_moderate[i-1]*0.03)*(1 + param_stock[i-1]*smunkh_m_h[i-1] + param_house[i-1]*hmunkh_m_h[i-1] + param_r_f*(1-smunkh_m_h[i-1] - hmunkh_m_h[i-1]))    
    gammas$smunkh_m_h[i-1] <- smunkh_m_h[i-1]
    gammas$hmunkh_m_h[i-1] <- hmunkh_m_h[i-1]
  }
  
  if(smunkh_m_l[i-1] + hmunkh_m_l[i-1] > 1){
    gammas$smunkh_m_l[i-1] <- smunkh_m_l[i-1] / (smunkh_m_l[i-1] + hmunkh_m_l[i-1])
    gammas$hmunkh_m_l[i-1] <- hmunkh_m_l[i-1] / (smunkh_m_l[i-1] + hmunkh_m_l[i-1])
    fc$munkhouse_moderate_low[i] <- (fc$munkhouse_moderate_low[i-1] + LL_moderate[i-1]*0.03)*(1 + param_stock[i-1]*gammas$smunkh_m_l[i-1] + param_house[i-1]*gammas$hmunkh_m_l[i-1])
  }else{
    fc$munkhouse_moderate_low[i] <- (fc$munkhouse_moderate_low[i-1] + LL_moderate[i-1]*0.03)*(1 + param_stock[i-1]*smunkh_m_l[i-1] + param_house[i-1]*hmunkh_m_l[i-1] + param_r_f*(1-smunkh_m_l[i-1] - hmunkh_m_l[i-1]))    
    gammas$smunkh_m_l[i-1] <- smunkh_m_l[i-1]
    gammas$hmunkh_m_l[i-1] <- hmunkh_m_l[i-1]
  }
  
  if(smunkh_m_m[i-1] + hmunkh_m_m[i-1] > 1){
    gammas$smunkh_m_m[i-1] <- smunkh_m_m[i-1] / (smunkh_m_m[i-1] + hmunkh_m_m[i-1])
    gammas$hmunkh_m_m[i-1] <- hmunkh_m_m[i-1] / (smunkh_m_m[i-1] + hmunkh_m_m[i-1])
    fc$munkhouse_moderate_mod[i] <- (fc$munkhouse_moderate_mod[i-1] + LL_moderate[i-1]*0.03)*(1 + param_stock[i-1]*gammas$smunkh_m_m[i-1] + param_house[i-1]*gammas$hmunkh_m_m[i-1])
  }else{
    fc$munkhouse_moderate_mod[i] <- (fc$munkhouse_moderate_mod[i-1] + LL_moderate[i-1]*0.03)*(1 + param_stock[i-1]*smunkh_m_m[i-1] + param_house[i-1]*hmunkh_m_m[i-1] + param_r_f*(1-smunkh_m_m[i-1] - hmunkh_m_m[i-1]))    
    gammas$smunkh_m_m[i-1] <- smunkh_m_m[i-1]
    gammas$hmunkh_m_m[i-1] <- hmunkh_m_m[i-1]
  }
  
  if(smunkh_s_h[i-1] + hmunkh_s_h[i-1] > 1){
    gammas$smunkh_s_h[i-1] <- smunkh_s_h[i-1] / (smunkh_s_h[i-1] + hmunkh_s_h[i-1])
    gammas$hmunkh_s_h[i-1] <- hmunkh_s_h[i-1] / (smunkh_s_h[i-1] + hmunkh_s_h[i-1])
    fc$munkhouse_steep_hi[i] <- (fc$munkhouse_steep_hi[i-1] + LL_steep[i-1]*0.03)*(1 + param_stock[i-1]*gammas$smunkh_s_h[i-1] + param_house[i-1]*gammas$hmunkh_s_h[i-1])
  }else{
    fc$munkhouse_steep_hi[i] <- (fc$munkhouse_steep_hi[i-1] + LL_steep[i-1]*0.03)*(1 + param_stock[i-1]*smunkh_s_h[i-1] + param_house[i-1]*hmunkh_s_h[i-1] + param_r_f*(1-smunkh_s_h[i-1] - hmunkh_s_h[i-1]))    
    gammas$smunkh_s_h[i-1] <- smunkh_s_h[i-1]
    gammas$hmunkh_s_h[i-1] <- hmunkh_s_h[i-1]
  }
  
  if(smunkh_s_l[i-1] + hmunkh_s_l[i-1] > 1){
    gammas$smunkh_s_l[i-1] <- smunkh_s_l[i-1] / (smunkh_s_l[i-1] + hmunkh_s_l[i-1])
    gammas$hmunkh_s_l[i-1] <- hmunkh_s_l[i-1] / (smunkh_s_l[i-1] + hmunkh_s_l[i-1])
    fc$munkhouse_steep_low[i] <- (fc$munkhouse_steep_low[i-1] + LL_steep[i-1]*0.03)*(1 + param_stock[i-1]*gammas$smunkh_s_l[i-1] + param_house[i-1]*gammas$hmunkh_s_l[i-1])
  }else{
    fc$munkhouse_steep_low[i] <- (fc$munkhouse_steep_low[i-1] + LL_steep[i-1]*0.03)*(1 + param_stock[i-1]*smunkh_s_l[i-1] + param_house[i-1]*hmunkh_s_l[i-1] + param_r_f*(1-smunkh_s_l[i-1] - hmunkh_s_l[i-1]))    
    gammas$smunkh_s_l[i-1] <- smunkh_s_l[i-1]
    gammas$hmunkh_s_l[i-1] <- hmunkh_s_l[i-1]
  }
  
  if(smunkh_s_m[i-1] + hmunkh_s_m[i-1] > 1){
    gammas$smunkh_s_m[i-1] <- smunkh_s_m[i-1] / (smunkh_s_m[i-1] + hmunkh_s_m[i-1])
    gammas$hmunkh_s_m[i-1] <- hmunkh_s_m[i-1] / (smunkh_s_m[i-1] + hmunkh_s_m[i-1])
    fc$munkhouse_steep_mod[i] <- (fc$munkhouse_steep_mod[i-1] + LL_steep[i-1]*0.03)*(1 + param_stock[i-1]*gammas$smunkh_s_m[i-1] + param_house[i-1]*gammas$hmunkh_s_m[i-1])
  }else{
    fc$munkhouse_steep_mod[i] <- (fc$munkhouse_steep_mod[i-1] + LL_steep[i-1]*0.03)*(1 + param_stock[i-1]*smunkh_s_m[i-1] + param_house[i-1]*hmunkh_s_m[i-1] + param_r_f*(1-smunkh_s_m[i-1] - hmunkh_s_m[i-1]))    
    gammas$smunkh_s_m[i-1] <- smunkh_s_m[i-1]
    gammas$hmunkh_s_m[i-1] <- hmunkh_s_m[i-1]
  }

}

# 
# pdf(file = "Dropbox/research/tex/figs/smunkhouse.pdf")
# plot(c(28:56),gammas$smunkh_s_h, type="l", ylim=c(0,1), col="red", xlab="age", ylab="stock share")
# lines(c(28:56),gammas$smunkh_s_m, col="orange")
# lines(c(28:56),gammas$smunkh_s_l, col="green")
# lines(c(28:56),gammas$smunkh_m_h, col="blue")
# lines(c(28:56),gammas$smunkh_m_m, col="violet")
# lines(c(28:56),gammas$smunkh_m_l, col="purple")
# lines(c(28:56),gammas$smunkh_f_h, col="grey")
# lines(c(28:56),gammas$smunkh_f_m, col="grey3")
# lines(c(28:56),gammas$smunkh_f_l, col="red3")
# legend(x = "bottomleft", y=0, legend = c("steep-high", "steep-low", "steep-moderate", "moderate-high", "moderate-low", "moderate-moderate", "flat-high", "flat-low", "flat-moderate"), fill=c("red","orange","green", "blue", "violet", "purple", "grey", "grey3", "red3"))
# dev.off()
# 
# pdf(file = "Dropbox/research/tex/figs/hmunkhouse.pdf")
# plot(c(28:56),gammas$hmunkh_s_h, type="l", ylim=c(0.05,0.5), col="red", xlab="age", ylab="house share")
# lines(c(28:56),gammas$hmunkh_s_m, col="orange")
# lines(c(28:56),gammas$hmunkh_s_l, col="green")
# lines(c(28:56),gammas$hmunkh_m_h, col="blue")
# lines(c(28:56),gammas$hmunkh_m_m, col="violet")
# lines(c(28:56),gammas$hmunkh_m_l, col="purple")
# lines(c(28:56),gammas$hmunkh_f_h, col="grey")
# lines(c(28:56),gammas$hmunkh_f_m, col="grey3")
# lines(c(28:56),gammas$hmunkh_f_l, col="red3")
# legend(x = "topleft", y=0, legend = c("steep-high", "steep-low", "steep-moderate", "moderate-high", "moderate-low", "moderate-moderate", "flat-high", "flat-low", "flat-moderate"), fill=c("red","orange","green", "blue", "violet", "purple", "grey", "grey3", "red3"))
# dev.off()