# init dataframe
fc <- data.frame("init" = rep(0,param_T))




# MARKOWITZ
#######################################################################################
markowitz <- rep(0.17, param_T)

fc$markowitz <- rep(0,param_T)
fc$markowitzup <- rep(0,param_T)
fc$markowitzdown <- rep(0,param_T)

fc$markowitz[1] <- LL_steep[1]*0.03
fc$markowitzup[1] <- LL_steep[1]*0.03
fc$markowitzdown[1] <- LL_steep[1]*0.03

for(i in 2:param_T){
  fc$markowitz[i] <- (fc$markowitz[i-1])*(1+param_mu_s*markowitz[i-1] + param_r_f*(1 - markowitz[i-1])) + LL_steep[i]*0.03
  fc$markowitzup[i] <- (fc$markowitz[i-1])*(1+(param_mu_s + param_sig_s)*0.5 + param_r_f*0.5) + LL_steep[i]*0.03
  fc$markowitzdown[i] <- (fc$markowitz[i-1])*(1+(param_mu_s - param_sig_s)*0.5 + param_r_f*0.5) + LL_steep[i]*0.03
}
#pdf("~/Dropbox/research/mathesis/tex/figs/fincapital.pdf")
plot(c(25:64), fc$markowitz, type="l", ylab="financial capital", xlab="age", col="red", ylim=c(0, 540))
lines(c(25:64), fc$markowitzup, lty=2)
lines(c(25:64), fc$markowitzdown, lty=2)
#dev.off()
############################################################################################################


#param_stock <- rnorm(39, mean = param_mu_s, sd = param_sig_s)
#plot(param_stock, type="l")
#param_stock <- rep(param_mu_s, param_T)
#dynamic portfolios
for(i in 2:param_T){
  #calculate optimal risky asset share
  merton_s[i-1] <- min(merton(i-1,L_steep,fc$merton_steep),1)
  merton_m[i-1] <- min(merton(i-1,L_moderate,fc$merton_moderate),1)
  merton_f[i-1] <- min(merton(i-1,L_flat,fc$merton_flat),1)
  
  munk_f_h[i-1] <- min(munk(t = i-1, dwi =  L_flat, dfi =  fc$munk_flat_hi, rhowsi =  param_rho_ws_high,rhowhi =  param_rho_hw,house = FALSE)$pis,1)
  munk_f_l[i-1] <- min(munk(t = i-1, dwi =  L_flat, dfi = fc$munk_flat_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house = FALSE)$pis,1)
  munk_f_m[i-1] <- min(munk(t = i-1, dwi =  L_flat, dfi = fc$munk_flat_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house = FALSE)$pis,1)
  
  munk_m_h[i-1] <- min(munk(t = i-1, dwi =  L_moderate, dfi = fc$munk_moderate_hi, rhowsi = param_rho_ws_high, rhowhi = param_rho_hw,house = FALSE)$pis,1)
  munk_m_l[i-1] <- min(munk(t = i-1, dwi = L_moderate, dfi = fc$munk_moderate_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house=FALSE)$pis,1)
  munk_m_m[i-1] <- min(munk(t = i-1, dwi = L_moderate, dfi = fc$munk_moderate_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house=FALSE)$pis,1)
  
  munk_s_h[i-1] <- min(munk(t = i-1, dwi = L_steep, dfi = fc$munk_steep_hi, rhowsi = param_rho_ws_high, rhowhi = param_rho_hw,house = FALSE)$pis,1)
  munk_s_l[i-1] <- min(munk(t = i-1, dwi = L_steep, dfi = fc$munk_steep_low, rhowsi = param_rho_ws_low, rhowhi = param_rho_hw,house = FALSE)$pis,1)
  munk_s_m[i-1] <- min(munk(t = i-1, dwi = L_steep, dfi = fc$munk_steep_mod, rhowsi = param_rho_ws_medium, rhowhi = param_rho_hw,house = FALSE)$pis,1)
  

  
  #law of motion of financial capital
  fc$merton_steep[i] <- (fc$merton_steep[i-1] + LL_steep[i-1]*0.03)*(1 + param_stock[i-1]*merton_s[i-1] + param_r_f*(1-merton_s[i-1]))
  fc$merton_moderate[i] <- (fc$merton_moderate[i-1] + LL_moderate[i-1]*0.03)*(1 + param_stock[i-1]*merton_m[i-1] + param_r_f*(1-merton_m[i-1]))
  fc$merton_flat[i] <- (fc$merton_flat[i-1] + LL_flat[i-1]*0.03)*(1 + param_stock[i-1]*merton_f[i-1] + param_r_f*(1-merton_f[i-1]))
  
  fc$munk_flat_hi[i] <- (fc$munk_flat_hi[i-1] + LL_flat[i-1]*0.03)*(1 + param_stock[i-1]*munk_f_h[i-1] + param_r_f*(1-munk_f_h[i-1]))
  fc$munk_flat_mod[i] <- (fc$munk_flat_hi[i-1] + LL_flat[i-1]*0.03)*(1 + param_stock[i-1]*munk_f_m[i-1] + param_r_f*(1-munk_f_m[i-1]))
  fc$munk_flat_low[i] <- (fc$munk_flat_hi[i-1] + LL_flat[i-1]*0.03)*(1 + param_stock[i-1]*munk_f_l[i-1] + param_r_f*(1-munk_f_l[i-1]))
  
  fc$munk_moderate_hi[i] <- (fc$munk_moderate_hi[i-1] + LL_moderate[i-1]*0.03)*(1 + param_stock[i-1]*munk_m_h[i-1] + param_r_f*(1-munk_m_h[i-1]))
  fc$munk_moderate_mod[i] <- (fc$munk_moderate_hi[i-1] + LL_moderate[i-1]*0.03)*(1 + param_stock[i-1]*munk_m_m[i-1] + param_r_f*(1-munk_m_m[i-1]))
  fc$munk_moderate_low[i] <- (fc$munk_moderate_hi[i-1] + LL_moderate[i-1]*0.03)*(1 + param_stock[i-1]*munk_m_l[i-1] + param_r_f*(1-munk_m_l[i-1]))
  
  fc$munk_steep_hi[i] <- (fc$munk_steep_hi[i-1] + LL_steep[i-1]*0.03)*(1 + param_stock[i-1]*munk_s_h[i-1] + param_r_f*(1-munk_s_h[i-1]))
  fc$munk_steep_mod[i] <- (fc$munk_steep_hi[i-1] + LL_steep[i-1]*0.03)*(1 + param_stock[i-1]*munk_s_m[i-1] + param_r_f*(1-munk_s_m[i-1]))
  fc$munk_steep_low[i] <- (fc$munk_steep_hi[i-1] + LL_steep[i-1]*0.03)*(1 + param_stock[i-1]*munk_s_l[i-1] + param_r_f*(1-munk_s_l[i-1]))
  
  fc$stoage[i]   <- (fc$stoage[i-1] + LL_moderate[i-1]*0.03)*(1+param_stock[i-1]*stoage[i-1] + param_r_f*(1-stoage[i-1]))
  fc$bodie[i]   <- (fc$bodie[i-1] + LL_moderate[i-1]*0.03)*(1+param_stock[i-1]*bodie[i-1] + param_r_f*(1-bodie[i-1]))
  fc$markowitz[i]   <- (fc$markowitz[i-1] + LL_moderate[i-1]*0.03)*(1+param_stock[i-1]*0.17 + param_r_f*0.83)
  fc$ah0[i]   <- (fc$ah0[i-1] + LL_moderate[i-1]*0.03)*(1+param_stock[i-1]*0.3 + param_r_f*0.7)
}




#pdf(file = "~/Dropbox/research/mathesis/tex/figs/individuals10.pdf")
# plot(c(26:64),merton_f, type="l", col="green", xlab="age", ylab="stock share",ylim = c(0,1))
# lines(c(26:64),merton_s, col="red")
# lines(c(26:64),munk_m_m, col="blue")
# lines(c(26:64),munk_f_h, col="orange")
# legend(x = "topright", y =0, legend = c("Bodie(steep wages)","Bodie(flat wages)", "Munk(flat wages)", "Munk(moderate wages)"), fill=c("red","green","orange", "blue"), cex = 1.5)
#dev.off()
 
# 
# 
#pdf(file="~/Dropbox/research/mathesis/tex/figs/fincapital.pdf")
# plot(c(26:65), fc$ah0, ftype="l", xlab="age", ylab="financial capital",col="red3", type="l")
# lines(c(26:65), fc$markowitz, col="orange2")
# legend(x = "topleft", y =0, legend = c("Anadolu Hayat Riskless", "Markowitz Solution"), fill=c("red3","orange2"), cex=1.5)
#dev.off()










#################################################################################################################

# Anadolu Hayat riskless
fc$ah0 <- rep(0,param_T)
fc$ah0[1] <- LL_moderate[1]*0.03

# Hundred minus age
fc$stoage <- rep(0,param_T)
fc$stoage[1] <- LL_moderate[1]*0.03

# Bodie et al (two hundred minus 2.5 age)
fc$bodie <- rep(0,param_T)
fc$bodie[1] <- LL_moderate[1]*0.03

# Markowitz constant solution
# fc$markowitz <- rep(0,param_T)
# fc$markowitz[1] <- LL_moderate[1]*0.03

# Merton basic solution
fc$merton_steep <- rep(0,param_T)
fc$merton_steep[1] <- LL_steep[1]*0.03

fc$merton_moderate <- rep(0,param_T)
fc$merton_moderate[1] <- LL_moderate[1]*0.03

fc$merton_flat <- rep(0,param_T)
fc$merton_flat[1] <- LL_flat[1]*0.03

# Munk solution without housing (dwdt_rhows)

fc$munk_steep_low <- rep(0,param_T)
fc$munk_steep_low[1] <- LL_steep[1]*0.03
fc$munk_steep_mod <- rep(0,param_T)
fc$munk_steep_mod[1] <- LL_steep[1]*0.03
fc$munk_steep_hi <- rep(0,param_T)
fc$munk_steep_hi[1] <- LL_steep[1]*0.03

fc$munk_moderate_low <- rep(0,param_T)
fc$munk_moderate_low[1] <- LL_moderate[1]*0.03
fc$munk_moderate_mod <- rep(0,param_T)
fc$munk_moderate_mod[1] <- LL_moderate[1]*0.03
fc$munk_moderate_hi <- rep(0,param_T)
fc$munk_moderate_hi[1] <- LL_moderate[1]*0.03

fc$munk_flat_low <- rep(0,param_T)
fc$munk_flat_low[1] <- LL_flat[1]*0.03
fc$munk_flat_mod <- rep(0,param_T)
fc$munk_flat_mod[1] <- LL_flat[1]*0.03
fc$munk_flat_hi <- rep(0,param_T)
fc$munk_flat_hi[1] <- LL_flat[1]*0.03


#individualized interest rates
merton_f <- rep(0,param_T-1)
merton_m <- rep(0,param_T-1)
merton_s <- rep(0,param_T-1)

munk_f_h <- rep(0,param_T-1)
munk_f_l <- rep(0,param_T-1)
munk_f_m <- rep(0,param_T-1)

munk_m_h <- rep(0,param_T-1)
munk_m_l <- rep(0,param_T-1)
munk_m_m <- rep(0,param_T-1)

munk_s_h <- rep(0,param_T-1)
munk_s_l <- rep(0,param_T-1)
munk_s_m <- rep(0,param_T-1)


#######################################################################################

#default portfolios
stoage <- c(100:1)
bodie  <- rep(100,100)
bodie[40:60] <- 200-2.5*(40:60)
bodie[61:100] <- 50
stoage <- stoage[param_Y:param_R]
bodie <- bodie[param_Y:param_R]
stoage <- stoage/100
bodie <- bodie/100
age <- c(param_Y:param_R)
ah0 <- rep(0.6, param_T)
#markowitz <- rep(0.63, param_T)


pdf(file = "~/Dropbox/research/mathesis/tex/figs/defaults.pdf")
plot(age, stoage, ylim = c(0,1), xlim = c(param_Y,param_R), type="l", col="red", ylab = "stock share")
lines(age, bodie, type="l", col="orange")
lines(age[-1], markowitz, type="l", col="green")
lines(age[-1], ah0, type="l", col="purple")
legend(x = "topright", y=0, legend = c("(100-t)%", "Cocco et al.", "Markowitz", "Turkish banks"), fill = c("red", "orange", "green", "purple"))
dev.off()