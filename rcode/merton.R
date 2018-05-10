merton <- function(t, dwi){
  ans <- 0
  ans <- param_mu_s - param_r_f
  ans <- ans / param_gamma
  ans <- ans / param_sig_s^2
  ans <- ans * (1+dwi[t]/FF[t])
  return(ans)
}


