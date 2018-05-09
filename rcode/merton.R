merton <- function(t, dwi){
  ans <- 0
  ans <- param_mu_s - param_r_f
  ans <- ans / param_gamma
  ans <- ans / param_sig_s^2
  ans <- ans * (1+dwi[t]/FF[t])
  return(ans)
}




Fs <- rep(0,param_T); Fs[1] <- 25000
Fm <- rep(0,param_T); Fm[1] <- 25000
Ff <- rep(0,param_T); Ff[1] <- 25000

for(i in 2:param_T){
  riskys <- min(merton(i-1,L_steep),1)
  riskym <- min(merton(i-1,L_moderate),1)
  riskyf <- min(merton(i-1,L_flat),1)
  
  Fs[i] <- Fs[i-1]*(1 + param_r_f*riskys + param_mu_s*(1-riskys))
  Fm[i] <- Fm[i-1]*(1 + param_r_f*riskym + param_mu_s*(1-riskym))
  Ff[i] <- Ff[i-1]*(1 + param_r_f*riskyf + param_mu_s*(1-riskyf))
}