L_steep <- rep(0,T)
L_moderate <- rep(0,T)
L_flat <- rep(0,T)
for(i in 1:param_T){
  ans <- 0
  anm <- 0
  anf <- 0
  for(j in i:param_T){
    tt <- j-i
    ans <- ans + LL_steep[j]/((1+param_r_f)^tt)
    anm <- anm + LL_moderate[j]/((1+param_r_f)^tt)
    anf <- anf + LL_flat[j]/((1+param_r_f)^tt)
  }
  L_steep[i] <- ans
  L_moderate[i] <- anm
  L_flat[i] <- anf
}








Fs_merton <- rep(0,param_T); Fs_merton[1] <- LL_steep[1]*0.03
Fm_merton <- rep(0,param_T); Fm_merton[1] <- LL_moderate[1]*0.03
Ff_merton <- rep(0,param_T); Ff_merton[1] <- LL_flat[1]*0.03

Fstoage <- rep(0,param_T); Fstoage[1] <- LL_moderate[1]*0.03
Fbodie <- rep(0,param_T); Fstoage[1] <- LL_moderate[1]*0.03
Fmarkowitz <- rep(0,param_T); Fstoage[1] <- LL_moderate[1]*0.03
Fah0 <- rep(0,param_T); Fstoage[1] <- LL_moderate[1]*0.03


for(i in 2:param_T){
  #calculate optimal risky asset share
  riskys <- min(merton(i-1,L_steep),1)
  riskym <- min(merton(i-1,L_moderate),1)
  riskyf <- min(merton(i-1,L_flat),1)
  
  #law of motion of financial capital
  Fs_merton[i] <- (Fs_merton[i-1] + LL_steep[i-1]*0.03)*(1 + param_mu_s*riskys + param_r_f*(1-riskys))
  Fm_merton[i] <- (Fm_merton[i-1] + LL_moderate[i-1]*0.03)*(1 + param_mu_s*riskym + param_r_f*(1-riskym))
  Ff_merton[i] <- (Ff_merton[i-1] + LL_flat[i-1]*0.03)*(1 + param_mu_s*riskyf + param_r_f*(1-riskyf))
  
  Fstoage[i]   <- (Fstoage[i-1] + LL_moderate[i-1]*0.03)*(1+param_mu_s*stoage[i-1] + param_r_f*(1-stoage[i-1]))
  Fbodie[i]   <- (Fbodie[i-1] + LL_moderate[i-1]*0.03)*(1+param_mu_s*bodie[i-1] + param_r_f*(1-bodie[i-1]))
  Fmarkowitz[i]   <- (Fmarkowitz[i-1] + LL_moderate[i-1]*0.03)*(1+param_mu_s*0.63 + param_r_f*0.37)
  Fah0[i]   <- (Fah0[i-1] + LL_moderate[i-1]*0.03)*(1+param_mu_s*0.3 + param_r_f*0.7)
}

s_merton <- rep(0,param_T)
m_merton <- rep(0,param_T)
f_merton <- rep(0,param_T)
for(i in 1:30){
  s_merton[i]<-min(merton(i,L_steep),1)
  m_merton[i]<-min(merton(i,L_moderate),1)
  f_merton[i]<-min(merton(i,L_flat),1)
}

pdf(file = "Dropbox/research/tex/figs/individuals.pdf")
plot(c(28:57),f_merton, type="l", col="green", xlab="age", ylab="stock share")
lines(c(28:57),m_merton, col="orange")
lines(c(28:57),s_merton, col="red")
legend(x = "bottomleft", y =0, legend = c("steep","moderate","flat"), fill=c("red","orange","green"))
dev.off()








pdf(file="Dropbox/research/tex/figs/humancapital.pdf")
plot(c(28:57), L_steep, type="l", xlab="age", ylab="human capital", ylim=c(30000,1200000),col="red3")
lines(c(28:57), L_moderate, col="orange2")
lines(c(28:57), L_flat, col="green3")
legend(x = "topright", y =0, legend = c("steep","moderate","flat"), fill=c("red3","orange2","green3"), cex=1.5)
dev.off()

pdf(file="Dropbox/research/tex/figs/fincapital.pdf")
plot(c(28:57), Fah0, type="l", xlab="age", ylab="financial capital",col="red3")
lines(c(28:57), Fmarkowitz, col="orange2")
legend(x = "topleft", y =0, legend = c("30% stocks", "63% stocks"), fill=c("red3","orange2"), cex=1.5)
dev.off()